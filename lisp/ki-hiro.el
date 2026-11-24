;;; ki-hiro.el --- HIRO REST client for KI work  -*- lexical-binding: t; -*-

;; Author: Moamen Hraden
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: comm

;;; Commentary:

;; The HIRO calls KI work needs, in Elisp: login with token refresh,
;; graph reads, KI check, variables.  The same calls the Python tools in
;; dlr_core/bin make, and the hard-won rules from their comments:
;;
;; - the endpoint answers 503 now and then; retry, and never turn an
;;   error into an empty result (_kat2_review/_graph.py)
;; - ids read from files on Windows carry CR; strip them
;;
;; An environment is a connector identity JSON file (server_url,
;; default-scope or scope, username, password, client_id,
;; client_secret).  The credentials are read from that file at login and
;; not kept; the token lives in `ki-hiro--tokens', apart from the
;; environment, so a backtrace that prints the environment shows neither.
;; Everything here is synchronous.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-http)

(defgroup ki-hiro nil
  "HIRO REST client for KI work."
  :group 'ki
  :prefix "ki-hiro-")

(defcustom ki-hiro-retries 8
  "Attempts for a request answered with a 5xx status."
  :type 'natnum)

(defcustom ki-hiro-timeout 120
  "Seconds to wait for one answer."
  :type 'natnum)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defconst ki-hiro-graph "/api/graph/7.4"
  "Graph API path; the version the Python tools are proven against.")

(define-error 'ki-hiro-error "HIRO request failed")

(cl-defstruct (ki-hiro-env (:constructor ki-hiro-env--create) (:copier nil))
  "A HIRO environment: its identity FILE, SERVER, SCOPE and API VERSION."
  file server scope version)

(cl-defstruct (ki-hiro--token (:copier nil))
  value refresh expires-at)

(defvar ki-hiro--envs (make-hash-table :test #'equal)
  "Environment file -> `ki-hiro-env'.")

(defvar ki-hiro--tokens (make-hash-table :test #'equal)
  "Environment file -> `ki-hiro--token' for this session.")

(defun ki-hiro--read-identity (file)
  "The connector identity JSON in FILE, as an alist."
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer :object-type 'alist)))

(defun ki-hiro-env (file)
  "The environment from connector identity FILE."
  (let ((file (expand-file-name file)))
    (or (gethash file ki-hiro--envs)
        (let* ((json (ki-hiro--read-identity file))
               (scopes (delete-dups
                        (delq nil (list (alist-get 'scope json)
                                        (alist-get 'default-scope json))))))
          ;; two spellings in the wild; same rule as hiro_env.py
          (when (cdr scopes)
            (error "%s names different scopes: %S" file scopes))
          (unless scopes
            (error "%s names no scope" file))
          (puthash file
                   (ki-hiro-env--create
                    :file file
                    :server (string-trim-right (alist-get 'server_url json) "/+")
                    :scope (car scopes))
                   ki-hiro--envs)))))

(defun ki-hiro-env-name (env)
  "Short name of ENV, the file name without extension."
  (file-name-base (ki-hiro-env-file env)))

(defun ki-hiro-env-prod-p (env)
  "Non-nil if ENV looks like a production environment."
  (string-match-p "prod" (ki-hiro-env-name env)))


;;; HTTP

(defun ki-hiro--bytes (string)
  "STRING as UTF-8 bytes.
url.el refuses a request that mixes multibyte text with the bytes of a
body holding umlauts, and strings from JSON are multibyte even when
they are ASCII, so every part of the request goes in as bytes."
  (encode-coding-string string 'utf-8))

(defun ki-hiro--http (method url &optional body token)
  "Send METHOD to URL with JSON BODY.  Return (STATUS . JSON-OR-TEXT)."
  (let* ((url-request-method (ki-hiro--bytes method))
         (url-request-extra-headers
          (append '(("Content-Type" . "application/json")
                    ("Accept" . "application/json"))
                  (and token `(("Authorization"
                                . ,(ki-hiro--bytes (concat "Bearer " token)))))))
         (url-request-data (and body (ki-hiro--bytes
                                      (json-serialize body :null-object nil
                                                      :false-object :false))))
         (url-show-status nil)
         (buffer (url-retrieve-synchronously (ki-hiro--bytes url) t t
                                             ki-hiro-timeout)))
    (unless buffer
      (signal 'ki-hiro-error (list method url "no answer")))
    (with-current-buffer buffer
      (unwind-protect
          (let ((status url-http-response-status)
                (text (decode-coding-string
                       (buffer-substring-no-properties
                        (or url-http-end-of-headers (point-min)) (point-max))
                       'utf-8)))
            (cons status
                  (condition-case nil
                      (json-parse-string text :object-type 'alist :array-type 'list
                                         :null-object nil :false-object nil)
                    (error (string-trim text)))))
        (kill-buffer buffer)))))

(defun ki-hiro--server-error-p (response)
  "Non-nil if RESPONSE is a 5xx, by status or by the error in its body."
  (let ((status (car response))
        (body (cdr response)))
    (or (null status)
        (>= status 500)
        ;; the error is an object with a code, or just a message (check)
        (and (consp body)
             (let* ((err (alist-get 'error body))
                    (code (and (consp err) (alist-get 'code err))))
               (and (numberp code) (>= code 500)))))))

(defun ki-hiro--with-retries (send what)
  "Call SEND until it answers without a server error.  WHAT names it."
  (let ((tries 0) response)
    (while (progn (setq response (funcall send))
                  (and (ki-hiro--server-error-p response)
                       (< (cl-incf tries) ki-hiro-retries)))
      (sleep-for (min 4 (+ 0.5 (* 0.3 tries)))))
    (when (ki-hiro--server-error-p response)
      (signal 'ki-hiro-error (list what (car response) (cdr response))))
    response))


;;; Login

(defun ki-hiro--version (env)
  "The /api/version answer of ENV's server, cached."
  (or (ki-hiro-env-version env)
      (let ((response (ki-hiro--with-retries
                       (lambda () (ki-hiro--http "GET" (concat (ki-hiro-env-server env)
                                                               "/api/version")))
                       "GET /api/version")))
        (unless (eql (car response) 200)
          (signal 'ki-hiro-error (list "GET /api/version" (car response))))
        (setf (ki-hiro-env-version env) (cdr response)))))

(defun ki-hiro--auth-url (env action)
  "URL of the auth ACTION (\"app\" or \"refresh\") for ENV."
  (concat (ki-hiro-env-server env)
          (alist-get 'endpoint (alist-get 'auth (ki-hiro--version env)))
          "/" action))

(defun ki-hiro--take-token (env response)
  "Store the token in RESPONSE for ENV; non-nil when there was one."
  (let* ((body (cdr response))
         (value (and (consp body)
                     (or (alist-get '_TOKEN body) (alist-get 'access_token body))))
         (old (gethash (ki-hiro-env-file env) ki-hiro--tokens)))
    (when (and (eql (car response) 200) value)
      (puthash (ki-hiro-env-file env)
               (make-ki-hiro--token
                :value value
                :refresh (or (alist-get 'refresh_token body)
                             (and old (ki-hiro--token-refresh old)))
                :expires-at (let ((at (alist-get 'expires-at body))
                                  (in (alist-get 'expires_in body)))
                              (cond (at (/ (if (stringp at) (string-to-number at) at)
                                           1000.0))
                                    (in (+ (float-time) in))
                                    (t (+ (float-time) 3600)))))
               ki-hiro--tokens)
      t)))

(defun ki-hiro--login (env)
  "Get a fresh token for ENV, with the credentials from its file."
  (let ((response
         (ki-hiro--with-retries
          (lambda ()
            (let ((identity (ki-hiro--read-identity (ki-hiro-env-file env))))
              (ki-hiro--http "POST" (ki-hiro--auth-url env "app")
                             `((client_id . ,(alist-get 'client_id identity))
                               (client_secret . ,(alist-get 'client_secret identity))
                               (username . ,(alist-get 'username identity))
                               (password . ,(alist-get 'password identity))))))
          "login")))
    (unless (ki-hiro--take-token env response)
      ;; the body may echo the request; report the status only
      (signal 'ki-hiro-error (list "login" (car response))))))

(defun ki-hiro--refresh (env)
  "Refresh ENV's token; non-nil on success."
  (when-let* ((old (gethash (ki-hiro-env-file env) ki-hiro--tokens))
              (refresh (ki-hiro--token-refresh old))
              (identity (ki-hiro--read-identity (ki-hiro-env-file env))))
    (ki-hiro--take-token
     env (ignore-errors
           (ki-hiro--http "POST" (ki-hiro--auth-url env "refresh")
                          `((client_id . ,(alist-get 'client_id identity))
                            (client_secret . ,(alist-get 'client_secret identity))
                            (refresh_token . ,refresh)))))))

(defun ki-hiro--current-token (env)
  "A valid token for ENV, refreshed or renewed when it runs out."
  (let ((token (gethash (ki-hiro-env-file env) ki-hiro--tokens)))
    (unless (and token
                 (> (- (ki-hiro--token-expires-at token) (float-time)) 60))
      (or (ki-hiro--refresh env)
          (ki-hiro--login env))
      (setq token (gethash (ki-hiro-env-file env) ki-hiro--tokens)))
    (ki-hiro--token-value token)))

(defun ki-hiro-forget-token (env)
  "Drop ENV's token; the next request logs in again."
  (remhash (ki-hiro-env-file env) ki-hiro--tokens))


;;; Requests

(defun ki-hiro-request (env method path &optional body)
  "Send METHOD to PATH on ENV's server.  Return (STATUS . JSON-OR-TEXT).
Retries server errors; a 401 renews the token once.  Other statuses are
returned, not signalled: the caller knows which ones are an answer."
  (let* ((url (concat (ki-hiro-env-server env) path))
         (what (concat method " " path))
         (send (lambda ()
                 (ki-hiro--http method url body (ki-hiro--current-token env))))
         (response (ki-hiro--with-retries send what)))
    (if (not (eql (car response) 401))
        response
      (ki-hiro-forget-token env)
      (ki-hiro--with-retries send what))))

(defun ki-hiro--ok (response what)
  "The body of RESPONSE, or signal unless its status is 200."
  (unless (eql (car response) 200)
    (signal 'ki-hiro-error (list what (car response) (cdr response))))
  (cdr response))

(defun ki-hiro--id (id)
  "ID without surrounding whitespace, URL-encoded for a path."
  (url-hexify-string (string-trim id)))

(defun ki-hiro-get (env id)
  "The vertex ID."
  (ki-hiro--ok (ki-hiro-request env "GET" (format "%s/%s" ki-hiro-graph (ki-hiro--id id)))
               (concat "GET " id)))

(defun ki-hiro-gremlin (env root query)
  "Items of the gremlin QUERY from vertex ROOT."
  (alist-get 'items
             (ki-hiro--ok (ki-hiro-request env "POST" (concat ki-hiro-graph "/query/gremlin")
                                           `((query . ,query) (root . ,(string-trim root))
                                             (limit . -1) (offset . 0) (listMeta . :false)))
                          (concat "gremlin " query))))

(defun ki-hiro-out-edges (env id)
  "The outgoing edges of vertex ID."
  (ki-hiro-gremlin env id "outE()"))

(defun ki-hiro-vertices (env query fields &optional limit)
  "Vertices matching the lucene QUERY, with FIELDS (comma separated)."
  (alist-get 'items
             (ki-hiro--ok (ki-hiro-request env "POST" (concat ki-hiro-graph "/query/vertices")
                                           `((query . ,query) (fields . ,fields)
                                             (limit . ,(or limit "-1"))))
                          (concat "vertices " query))))

(defun ki-hiro-type-query (env type)
  "Lucene query for vertices of TYPE in ENV's scope."
  (format "ogit\\/_type:\"%s\" AND ogit\\/_scope:\"%s\""
          (string-replace "/" "\\/" type) (ki-hiro-env-scope env)))

(defun ki-hiro-values (env id from to)
  "Timeseries values of ID between FROM and TO, epoch milliseconds.
The endpoint reads only the last hour without FROM, and answers 504 for
a long series: keep the window narrow."
  (alist-get 'items
             (ki-hiro--ok (ki-hiro-request
                           env "GET" (format "%s/%s/values?from=%d&to=%d" ki-hiro-graph
                                             (ki-hiro--id id) from to))
                          (concat "values " id))))

(defun ki-hiro-check (env text)
  "Validate the KI TEXT.  Return (STATUS . ANSWER); 200 means valid."
  (ki-hiro-request env "POST" "/api/ki/6/check" `((ki . ,text))))

(defun ki-hiro-deployed-pools (env)
  "The knowledge pools ENV's scope deploys."
  (ki-hiro--ok (ki-hiro-request env "GET" (format "/api/knowledge/1/instance/%s/deployed/pools"
                                                  (ki-hiro-env-scope env)))
               "deployed pools"))

(defun ki-hiro-variable (env name)
  "The registered variable NAME, or nil when the registry does not know it.
The registry is eventually consistent: a 404 right after registering
proves nothing."
  (let ((response (ki-hiro-request env "GET" (concat "/api/variables/6/define?name="
                                                     (url-hexify-string name)))))
    (pcase (car response)
      (200 (cdr response))
      (404 nil)
      (_ (signal 'ki-hiro-error (list "variable" name (car response)))))))

(provide 'ki-hiro)
;;; ki-hiro.el ends here

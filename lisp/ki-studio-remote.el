;;; ki-studio-remote.el --- KI Studio commands that read HIRO  -*- lexical-binding: t; -*-

;; Author: Moamen Hraden
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages

;;; Commentary:

;; The read-only half of KI Studio: check the buffer, compare it with
;; the deployed KI, look at the av* attributes of a job description,
;; test the `on' part against it, and read an issue.  Nothing here
;; writes to HIRO.
;;
;; Ports of dlr_core/bin/ki_tool.py check, _kat2_review/_ki_diff.py and
;; the dlr-teaching scripts selbsttest.py, av_attribute.py and
;; gefeuert.py, with the cautions from their comments.

;;; Code:

(require 'ki-studio)
(require 'ki-hiro)
(require 'ediff)

(defcustom ki-studio-pool-name "default"
  "Name of the knowledge pool whose deployed KIs count."
  :type 'string
  :group 'ki-studio)


;;; Environment, per checkout and session

(defvar-local ki-studio-env-file nil
  "Connector identity file for this buffer; asked for when nil.")
(put 'ki-studio-env-file 'safe-local-variable #'stringp)

(defvar ki-studio--env-by-root (make-hash-table :test #'equal)
  "Checkout root -> chosen identity file, for this session.")

(defun ki-studio-select-env (file)
  "Use the connector identity FILE for this checkout.
There is no default on purpose: test and prod files sit side by side,
and a default would one day point at the wrong tenant."
  (interactive
   (let ((root (or (ki-studio--root) (user-error "Not inside a dlr_core checkout"))))
     (list (expand-file-name
            (completing-read "Environment: "
                             (directory-files root nil (rx ".env" eos))
                             nil t)
            root))))
  (puthash (ki-studio--root) file ki-studio--env-by-root)
  (force-mode-line-update t)
  (message "KI Studio uses %s" (file-name-base file)))

(defun ki-studio--env ()
  "The environment of this checkout, asking for it the first time."
  (let ((file (or ki-studio-env-file
                  (gethash (ki-studio--root) ki-studio--env-by-root)
                  (progn (call-interactively #'ki-studio-select-env)
                         (gethash (ki-studio--root) ki-studio--env-by-root)))))
    (ki-hiro-env file)))

(defun ki-studio--env-label ()
  "Mode line text for the chosen environment, nil when none is chosen."
  (when-let* ((root (ki-studio--root))
              (file (or ki-studio-env-file (gethash root ki-studio--env-by-root))))
    (let ((name (file-name-base file)))
      (propertize name 'face (if (string-match-p "prod" name) 'error 'success)))))

(defmacro ki-studio--reading (what &rest body)
  "Run BODY with a message that WHAT is being read."
  (declare (indent 1))
  `(let ((inhibit-message nil))
     (message "%s from %s..." ,what (ki-hiro-env-name (ki-studio--env)))
     (prog1 (progn ,@body)
       (message nil))))


;;; Small readers

(defun ki-studio--header (field)
  "The header FIELD (name, version, description) of the buffer's KI."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (rx bol (+ blank) (literal field) ":" (* blank) "\"" (group (* (not (any "\"")))) "\"")
           nil t)
      (match-string-no-properties 1))))

(defun ki-studio--number (value)
  "VALUE, a number or a numeric string, as a number."
  (if (numberp value) value (string-to-number (or value "0"))))

(defun ki-studio--date (millis)
  "MILLIS since the epoch as a short local time."
  (format-time-string "%Y-%m-%d %H:%M" (/ (ki-studio--number millis) 1000)))

(defun ki-studio--json-list (value)
  "Issue variable VALUE as a list of strings.
Multi-valued variables come as a JSON array inside a string."
  (cond ((null value) nil)
        ((and (stringp value) (string-prefix-p "[" value))
         (condition-case nil
             (json-parse-string value :array-type 'list)
           (error (list value))))
        (t (list (format "%s" value)))))


;;; Check

(defvar ki-studio--registered (make-hash-table :test #'equal)
  "(ENV-FILE . VARIABLE) -> t for variables known to be registered.")

(defun ki-studio--unregistered (env names)
  "The NAMES the variable registry of ENV does not know.
The registry is eventually consistent, so a name is asked twice before
it counts as unregistered."
  (let (missing)
    (dolist (name names)
      (let ((key (cons (ki-hiro-env-file env) name)))
        (unless (or (gethash key ki-studio--registered)
                    (and (or (ki-hiro-variable env name)
                             (progn (sleep-for 0.5) (ki-hiro-variable env name)))
                         (puthash key t ki-studio--registered)))
          (push name missing))))
    (nreverse missing)))

(defun ki-studio-check ()
  "Validate the buffer with the engine, and look for unregistered variables.
An unregistered issue variable passes check and deploy, and the rule then
runs without effect; that is what the second half catches."
  (interactive)
  (let* ((env (ki-studio--env))
         (response (ki-studio--reading "Checking"
                     (ki-hiro-check env (buffer-substring-no-properties
                                         (point-min) (point-max)))))
         (body (cdr response)))
    (setq ki-studio--check-diags nil)
    (if (not (eql (car response) 200))
        (let ((errors (and (consp body) (alist-get 'errors body))))
          (dolist (err errors)
            (push (list (alist-get 'line err) :error
                        (concat "check: " (alist-get 'message err)))
                  ki-studio--check-diags))
          (unless errors
            ;; an uncaught compiler error carries no line, only a dump
            (let ((text (format "%s" (if (consp body) (alist-get 'error body) body))))
              (push (list 1 :error
                          (concat "check: " (truncate-string-to-width text 300 nil nil "…")
                                  (and (equal text "unsupported format")
                                       " (the text has to start with `ki'; comments go under it)")))
                    ki-studio--check-diags)))
          (add-hook 'after-change-functions #'ki-studio--forget-check-lines nil t)
          (message "%s" (nth 2 (car ki-studio--check-diags))))
      (let* ((variables (alist-get 'variables body))
             (issue (seq-remove (lambda (v) (string-prefix-p "ogit/" v))
                                (alist-get 'ISSUE variables)))
             (names (mapcar (lambda (v) (string-remove-prefix "/" v)) issue))
             (missing (ki-studio--reading "Asking the variable registry"
                        (ki-studio--unregistered env names))))
        (dolist (name missing)
          (push (list name :error
                      (format "check: %s is not registered; the KI would run without effect (variable_tool.py add, before create)"
                              name))
                ki-studio--check-diags))
        (message "check ok: %d issue variables%s" (length names)
                 (if missing (format ", %d NOT registered: %s" (length missing)
                                     (string-join missing ", "))
                   ", all registered"))))
    (when (bound-and-true-p flymake-mode)
      (flymake-start))))

(defun ki-studio--forget-check-lines (&rest _)
  "Drop the line diagnostics of the last check; the lines moved."
  (setq ki-studio--check-diags
        (seq-remove (lambda (d) (integerp (car d))) ki-studio--check-diags))
  (remove-hook 'after-change-functions #'ki-studio--forget-check-lines t))


;;; The deployed KI

(defvar ki-studio--pool-cache (make-hash-table :test #'equal)
  "Env file -> (TIME . KI-IDS) of the deployed pool.")

(defun ki-studio--pool-kis (env &optional refresh)
  "Ids of the KIs deployed in the pool `ki-studio-pool-name' of ENV.
Kept for 5 minutes unless REFRESH."
  (let ((cached (gethash (ki-hiro-env-file env) ki-studio--pool-cache)))
    (if (and cached (not refresh) (< (float-time (time-since (car cached))) 300))
        (cdr cached)
      (let* ((pool (or (seq-find (lambda (p) (equal (alist-get 'ogit/name p) ki-studio-pool-name))
                                 (ki-hiro-deployed-pools env))
                       (user-error "No deployed pool named %s" ki-studio-pool-name)))
             (ids (cl-loop for edge in (ki-hiro-out-edges env (alist-get 'ogit/_id pool))
                           when (equal (alist-get 'ogit/_in-type edge)
                                       "ogit/Automation/KnowledgeItem")
                           collect (alist-get 'ogit/_in-id edge))))
        ;; a short or empty pool is a failed read, not an answer (METHODE B8)
        (when (< (length ids) 100)
          (error "Only %d KIs in pool %s: token, 503 or wrong pool?"
                 (length ids) ki-studio-pool-name))
        (puthash (ki-hiro-env-file env) (cons (current-time) ids) ki-studio--pool-cache)
        ids))))

(defun ki-studio--deployed (env name &optional refresh)
  "The deployed KI vertex named NAME in ENV, or nil.
Several vertices can carry one name (a fresh vertex is the way to fix a
broken one); the deployed one is the one in the pool."
  (let* ((pool (ki-studio--pool-kis env refresh))
         (candidates (ki-hiro-vertices
                      env (format "%s AND ogit\\/name:\"%s\""
                                  (ki-hiro-type-query env "ogit/Automation/KnowledgeItem") name)
                      "ogit/_id,ogit/name"))
         (deployed (seq-filter (lambda (v) (and (equal (alist-get 'ogit/name v) name)
                                                (member (alist-get 'ogit/_id v) pool)))
                               candidates)))
    (when (cdr deployed)
      (error "%d deployed vertices named %s" (length deployed) name))
    (and deployed (ki-hiro-get env (alist-get 'ogit/_id (car deployed))))))

(defun ki-studio--deployed-or-error (refresh)
  "The deployed vertex of the buffer's KI; REFRESH re-reads the pool."
  (let* ((name (or (ki-studio--header "name") (user-error "No name: in the header")))
         (env (ki-studio--env)))
    (or (ki-studio--reading (format "Looking up %s" name)
          (ki-studio--deployed env name refresh))
        (user-error "%s is not deployed in pool %s" name ki-studio-pool-name))))

(defun ki-studio--text (vertex)
  "The rule text of the KI VERTEX."
  (or (alist-get 'ogit/Automation/knowledgeItemFormalRepresentation vertex) ""))

(defun ki-studio-diff (&optional refresh)
  "Compare the buffer with the deployed KI of the same name.
Reports a differing ogit/description too: KAT shows that one, and the
rule text alone does not carry it.  With REFRESH (prefix argument),
re-read the pool."
  (interactive "P")
  (let* ((vertex (ki-studio--deployed-or-error refresh))
         (graph (ki-studio--text vertex))
         (mine (buffer-substring-no-properties (point-min) (point-max)))
         (desc-ok (equal (alist-get 'ogit/description vertex)
                         (ki-studio--header "description")))
         (label (format "v%s, %s" (alist-get 'ogit/_v vertex)
                        (ki-studio--date (alist-get 'ogit/_modified-on vertex)))))
    (if (equal (string-trim-right mine "\n+") (string-trim-right graph "\n+"))
        (message "Same as deployed %s; ogit/description %s" label
                 (if desc-ok "the same" "DIFFERS from the header"))
      (let ((buffer (get-buffer-create (format "*ki deployed: %s*" (ki-studio--header "name")))))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert graph)
            (ki-mode)
            (setq buffer-read-only t)))
        (unless desc-ok
          (message "ogit/description of the vertex differs from the header"))
        (ediff-buffers (current-buffer) buffer)))))

(defun ki-studio-pull (&optional refresh)
  "Replace the buffer with the deployed KI of the same name.
The graph is the source of truth and the file its copy; this is the one
direction the review copies move.  The buffer is not saved."
  (interactive "P")
  (let* ((vertex (ki-studio--deployed-or-error refresh))
         (graph (ki-studio--text vertex)))
    (if (equal (buffer-substring-no-properties (point-min) (point-max)) graph)
        (message "Already the deployed text (v%s)" (alist-get 'ogit/_v vertex))
      (when (yes-or-no-p (format "Replace the buffer with deployed v%s? "
                                 (alist-get 'ogit/_v vertex)))
        (let ((point (point)))
          (erase-buffer)
          (insert graph)
          (goto-char (min point (point-max))))
        (message "Pulled v%s; not saved" (alist-get 'ogit/_v vertex))))))


;;; Job description and its WorkUnits

(defvar ki-studio--jd-by-root (make-hash-table :test #'equal)
  "Checkout root -> (ID . NAME) of the job description in focus.")

(defvar ki-studio--jd-cache (make-hash-table :test #'equal)
  "JD id -> (JD . WORKUNITS) read this session.")

(defun ki-studio--job-descriptions (env)
  "All job descriptions in ENV's scope: alists with id, name, created-on."
  (ki-hiro-vertices env (ki-hiro-type-query env "ogit/HR/JobDescription")
                    "ogit/_id,ogit/name,ogit/_created-on"))

(defun ki-studio--xid-hash (vertex)
  "The import hash in VERTEX's xid, \"dlr-hr-jd:HASH\" or \"dlr-hr-wu:HASH:N\"."
  (when-let* ((xid (alist-get 'ogit/_xid vertex)))
    (nth 1 (split-string (car (last (split-string xid "|"))) ":"))))

(defun ki-studio--import-counts (env)
  "Hash: job description import hash -> (WORKUNITS WITH-VALUES WITH-AV).
One query for all WorkUnits of the scope, assigned to their job
description by the hash in their xid.  That matched the ogit/contains
edges for 140 of 142 job descriptions (2026-11-24): a WorkUnit can be
connected to a job description whose hash it does not carry."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (wu (ki-hiro-vertices env (ki-hiro-type-query env "ogit/HR/WorkUnit")
                                  "ogit/_id,ogit/_xid,ogit/values,/avKern,/avFakt"))
      (when-let* ((hash (ki-studio--xid-hash wu)))
        (let ((entry (gethash hash counts (list 0 0 0))))
          (puthash hash
                   (list (1+ (nth 0 entry))
                         (+ (nth 1 entry) (if (alist-get 'ogit/values wu) 1 0))
                         (+ (nth 2 entry) (if (or (alist-get '/avKern wu) (alist-get '/avFakt wu)) 1 0)))
                   counts))))
    counts))

(defun ki-studio--import-flag (counts)
  "Short import status for COUNTS, a list (WORKUNITS WITH-VALUES WITH-AV)."
  (pcase-let ((`(,n ,values ,av) counts))
    (cond ((zerop n) (propertize "✗ keine WorkUnits" 'face 'error))
          ((and (= values n) (= av n)) (propertize (format "✓ %d WU" n) 'face 'success))
          (t (propertize
              (concat (format "⚠ %d WU" n)
                      (and (< values n) (format ", %d ohne values" (- n values)))
                      (and (< av n) (format ", %d ohne av*" (- n av))))
              'face 'warning)))))

(defun ki-studio--wu-counts (workunits)
  "(WORKUNITS WITH-VALUES WITH-AV) of the WorkUnit vertices WORKUNITS."
  (list (length workunits)
        (seq-count (lambda (w) (alist-get 'ogit/values w)) workunits)
        (seq-count #'ki-studio--av-attributes workunits)))

(defun ki-studio-select-jd (id name)
  "Focus the job description ID (named NAME) for its view and on-part tests.
There are duplicates and versions without WorkUnits, so the choice shows
the creation date, the end of the id, and whether the import is complete:
WorkUnits there, each with its activity text (values) and av* attributes."
  (interactive
   (let* ((env (ki-studio--env))
          (jds (ki-studio--reading "Listing job descriptions"
                 (ki-hiro-vertices env (ki-hiro-type-query env "ogit/HR/JobDescription")
                                   "ogit/_id,ogit/_xid,ogit/name,ogit/_created-on")))
          (counts (ki-studio--reading "Counting WorkUnits" (ki-studio--import-counts env)))
          (table (mapcar (lambda (jd)
                           (cons (format "%s  [%s  …%s]" (alist-get 'ogit/name jd)
                                         (ki-studio--date (alist-get 'ogit/_created-on jd))
                                         (substring (alist-get 'ogit/_id jd) -8))
                                 jd))
                         jds))
          (focused (car (gethash (ki-studio--root) ki-studio--jd-by-root)))
          (current (car (seq-find (lambda (entry)
                                    (equal (alist-get 'ogit/_id (cdr entry)) focused))
                                  table)))
          (completion-extra-properties
           (list :annotation-function
                 (lambda (candidate)
                   (let ((hash (ki-studio--xid-hash (cdr (assoc candidate table)))))
                     (concat "  " (if hash
                                      (ki-studio--import-flag (gethash hash counts (list 0 0 0)))
                                    (propertize "? ohne xid" 'face 'shadow))
                             (and (equal candidate current)
                                  (propertize "  ◀ im Fokus" 'face 'bold)))))))
          ;; the focused one is the default: listed first, kept with RET
          (jd (cdr (assoc (completing-read
                           (if current
                               (format "Job description (im Fokus: %s): "
                                       (alist-get 'ogit/name (cdr (assoc current table))))
                             "Job description: ")
                           table nil t nil nil current)
                          table))))
     (list (alist-get 'ogit/_id jd) (alist-get 'ogit/name jd))))
  (puthash (ki-studio--root) (cons id name) ki-studio--jd-by-root)
  ;; the edges, not the xids: this is the count the rules see
  (message "Focus: %s, %s" name
           (ki-studio--import-flag (ki-studio--wu-counts (cdr (ki-studio--jd id))))))

(defun ki-studio--jd-or-ask ()
  "The (ID . NAME) in focus, asking for one the first time."
  (or (gethash (ki-studio--root) ki-studio--jd-by-root)
      (progn (call-interactively #'ki-studio-select-jd)
             (gethash (ki-studio--root) ki-studio--jd-by-root))))

(defvar ki-studio--jd-related (make-hash-table :test #'equal)
  "JD id -> alist (TYPE . VERTICES) of its other neighbours, read with it.")

(defun ki-studio--jd (id &optional refresh)
  "(JD . WORKUNITS) of the job description ID, read once per session.
Its qualification requirements and authorities are read in the same
pass, see `ki-studio--jd-related'.  REFRESH reads it again: an
enrichment can change the WorkUnits mid-round."
  (or (and (not refresh) (gethash id ki-studio--jd-cache))
      (let* ((env (ki-studio--env))
             (by-name (lambda (a b) (string< (alist-get 'ogit/name a "")
                                             (alist-get 'ogit/name b ""))))
             (jd (ki-studio--reading "Reading the job description" (ki-hiro-get env id)))
             (edges (ki-hiro-out-edges env id))
             (neighbours
              (lambda (type)
                (ki-studio--reading (format "Reading %s" (file-name-nondirectory type))
                  (mapcar (lambda (edge) (ki-hiro-get env (alist-get 'ogit/_in-id edge)))
                          (seq-filter (lambda (edge) (equal (alist-get 'ogit/_in-type edge) type))
                                      edges)))))
             (pair (cons jd (sort (funcall neighbours "ogit/HR/WorkUnit") by-name))))
        (puthash id `(("ogit/HR/QualificationRequirement"
                       . ,(funcall neighbours "ogit/HR/QualificationRequirement"))
                      ("ogit/HR/Authority" . ,(funcall neighbours "ogit/HR/Authority")))
                 ki-studio--jd-related)
        (puthash id pair ki-studio--jd-cache))))

(defun ki-studio--av-attributes (vertex)
  "The non-empty av* attributes of VERTEX, as (NAME . VALUE), sorted."
  (sort (cl-loop for (key . value) in vertex
                 for name = (symbol-name key)
                 when (and (string-prefix-p "/av" name) value (not (equal value "")))
                 collect (cons (substring name 1) value))
        (lambda (a b) (string< (car a) (car b)))))

(defvar ki-studio-av-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'ki-studio-av-refresh)
    map))

(define-derived-mode ki-studio-av-mode special-mode "KI-Stelle"
  "A job description: its text, qualifications, authorities and WorkUnits."
  (setq truncate-lines nil)
  (visual-line-mode 1))

(defvar-local ki-studio--av-jd nil "JD id this buffer shows.")

(defun ki-studio--insert-field (label value)
  "Insert LABEL and VALUE, the value wrapped under its own indentation."
  (when (and value (not (equal value "")))
    ;; continuation lines, wrapped or real, start under the value
    (let ((prefix (make-string (+ 4 (max 13 (length label))) ?\s)))
      (insert (propertize (format "   %-13s " label) 'face 'font-lock-variable-name-face)
              (propertize (format "%s" value) 'wrap-prefix prefix 'line-prefix prefix)
              "\n"))))

(defun ki-studio--free-attributes (vertex)
  "The free attributes of VERTEX (stored with a leading slash), sorted.
The av* ones are left out; they have their own lines."
  (sort (cl-loop for (key . value) in vertex
                 for name = (symbol-name key)
                 when (and (string-prefix-p "/" name) (not (string-prefix-p "/av" name)))
                 collect (cons (substring name 1) value))
        (lambda (a b) (string< (car a) (car b)))))

(defun ki-studio-av (&optional refresh jd-id)
  "Show the focused job description: everything a rule can read on it.
Its text and free attributes, its qualification requirements and
authorities, and each WorkUnit with its activity text and av* values.
With REFRESH (prefix argument), read it again.  JD-ID overrides the
focused one."
  (interactive "P")
  (let* ((jd-id (or jd-id (car (ki-studio--jd-or-ask))))
         (root (ki-studio--root))
         (pair (ki-studio--jd jd-id refresh))
         (jd (car pair))
         (related (gethash jd-id ki-studio--jd-related))
         (qualifications (cdr (assoc "ogit/HR/QualificationRequirement" related)))
         (authorities (cdr (assoc "ogit/HR/Authority" related)))
         (buffer (get-buffer-create (format "*ki stelle: %s*" (alist-get 'ogit/name jd)))))
    (with-current-buffer buffer
      (ki-studio-av-mode)
      (setq ki-studio--av-jd jd-id
            default-directory root)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "%s  (%s)\n" (alist-get 'ogit/name jd) jd-id) 'face 'bold)
                (format "%d WorkUnits, %d Qualifikationen, %d Befugnisse, angelegt %s, v%s\n"
                        (length (cdr pair)) (length qualifications) (length authorities)
                        (ki-studio--date (alist-get 'ogit/_created-on jd))
                        (alist-get 'ogit/_v jd)))
        (ki-studio--section "Stelle")
        (ki-studio--insert-field "description" (alist-get 'ogit/description jd))
        (ki-studio--insert-field "content" (alist-get 'ogit/content jd))
        (pcase-dolist (`(,name . ,value) (ki-studio--free-attributes jd))
          (ki-studio--insert-field name value))
        (ki-studio--section "Qualifikationen (ogit/requires)")
        (dolist (q qualifications)
          (ki-studio--insert-field (or (alist-get 'ogit/HR/qualificationKind q) "-")
                                   (alist-get 'ogit/description q)))
        (when authorities
          (ki-studio--section "Befugnisse (ogit/defines)")
          (dolist (a authorities)
            (ki-studio--insert-field "Authority" (or (alist-get 'ogit/description a)
                                                     (alist-get 'ogit/name a)))))
        (dolist (wu (cdr pair))
          (insert "\n"
                  (propertize (format "== %s" (alist-get 'ogit/name wu)) 'face 'font-lock-function-name-face)
                  (format "  [%s]  Anteil %s  v%s\n"
                          (alist-get 'ogit/_id wu) (alist-get 'ogit/HR/timeShare wu)
                          (alist-get 'ogit/_v wu)))
          (ki-studio--insert-field "values" (alist-get 'ogit/values wu))
          (let ((av (ki-studio--av-attributes wu)))
            (if (not av)
                (insert "   (keine av*-Attribute)\n")
              (pcase-dolist (`(,name . ,value) av)
                (ki-studio--insert-field name value)))))
        (goto-char (point-min))))
    (display-buffer buffer)))

(defun ki-studio-av-refresh ()
  "Read the job description of this av buffer again."
  (interactive)
  (ki-studio-av t ki-studio--av-jd))


;;; PCRE to Emacs regexp

(defun ki-studio--class (pattern start)
  "Translate the [...] class at START of PATTERN.  Return (REGEXP . END)."
  (let ((i (1+ start)) (n (length pattern)) negated members bracket dash)
    (when (and (< i n) (eq (aref pattern i) ?^))
      (setq negated t i (1+ i)))
    (when (and (< i n) (eq (aref pattern i) ?\]))
      (setq bracket t i (1+ i)))
    (while (and (< i n) (not (eq (aref pattern i) ?\])))
      (let ((c (aref pattern i)))
        (if (and (eq c ?\\) (< (1+ i) n))
            (let ((d (aref pattern (1+ i))))
              (pcase d
                (?d (push "[:digit:]" members))
                (?s (push "[:space:]" members))
                (?w (push "[:alnum:]_" members))
                (?\] (setq bracket t))
                (?- (setq dash t))
                (_ (push (string d) members)))
              (setq i (+ i 2)))
          (push (string c) members)
          (setq i (1+ i)))))
    (when (>= i n)
      (error "Unterminated character class"))
    (cons (concat "[" (and negated "^") (and bracket "]")
                  (apply #'concat (nreverse members)) (and dash "-") "]")
          (1+ i))))

(defun ki-studio--pcre-to-emacs (pattern)
  "Translate the KI regex PATTERN to an Emacs regexp.
Return (REGEXP CASE-FOLD . GROUPS), GROUPS an alist (NAME . NUMBER).
Covers what the corpus uses; lookaround signals an error.  The engine
matches bytes and Emacs characters: an umlaut counts 2 there, 1 here."
  (let ((i 0) (n (length pattern)) (group 0) out groups fold)
    (while (< i n)
      (let ((c (aref pattern i)))
        (cond
         ((eq c ?\\)
          (let ((d (and (< (1+ i) n) (aref pattern (1+ i)))))
            (push (pcase d
                    ((or ?| ?\( ?\) ?{ ?} ?/) (string d))
                    (?d "[0-9]") (?D "[^0-9]")
                    (?s "[[:space:]]") (?S "[^[:space:]]")
                    (?w "[[:alnum:]_]") (?W "[^[:alnum:]_]")
                    (?b "\\b") (?B "\\B")
                    ((or ?. ?* ?+ ?? ?\[ ?\] ?^ ?$ ?\\) (string ?\\ d))
                    ('nil (error "Trailing backslash"))
                    (_ (string d)))
                  out)
            (setq i (+ i 2))))
         ((eq c ?\[)
          (let ((class (ki-studio--class pattern i)))
            (push (car class) out)
            (setq i (cdr class))))
         ((eq c ?\()
          (cond
           ((string-prefix-p "(?:" (substring pattern i))
            (push "\\(?:" out) (setq i (+ i 3)))
           ((string-match (rx bos "(?<" (group (any "A-Za-z_") (* (any "A-Za-z0-9_"))) ">")
                          (substring pattern i))
            (setq group (1+ group))
            (push (cons (match-string 1 (substring pattern i)) group) groups)
            (push (format "\\(?%d:" group) out)
            (setq i (+ i (match-end 0))))
           ((string-prefix-p "(?i)" (substring pattern i))
            (setq fold t i (+ i 4)))
           ((string-prefix-p "(?" (substring pattern i))
            (error "Not translated: %s" (substring pattern i (min n (+ i 4)))))
           (t
            (setq group (1+ group))
            (push (format "\\(?%d:" group) out)
            (setq i (1+ i)))))
         ((eq c ?\)) (push "\\)" out) (setq i (1+ i)))
         ((eq c ?|) (push "\\|" out) (setq i (1+ i)))
         ((eq c ?{) (push "\\{" out) (setq i (1+ i)))
         ((eq c ?}) (push "\\}" out) (setq i (1+ i)))
         (t (push (string c) out) (setq i (1+ i))))))
    (cons (apply #'concat (nreverse out)) (cons fold groups))))

(defun ki-studio--group-end (pattern start)
  "Index after the group opening at START of PATTERN."
  (let ((i (1+ start)) (depth 1) (n (length pattern)))
    (while (and (< i n) (> depth 0))
      (pcase (aref pattern i)
        (?\\ (setq i (1+ i)))
        (?\[ (setq i (1- (cdr (ki-studio--class pattern i)))))
        (?\( (setq depth (1+ depth)))
        (?\) (setq depth (1- depth))))
      (setq i (1+ i)))
    (when (> depth 0) (error "Unbalanced group"))
    i))

(defun ki-studio--compile-regex (pattern)
  "Compile the KI regex PATTERN.  Return (MAIN . AHEAD).
MAIN is the translation (see `ki-studio--pcre-to-emacs'); AHEAD is a
list of (POSITIVE . TRANSLATION) for a leading run of lookaheads,
`^(?=A)(?!B)...REST': each A must match from the start, no B may, and
then ^REST must match.  That is the shape the corpus uses lookaround
in; any other lookaround signals an error."
  (let ((i 0) ahead)
    (when (string-prefix-p "^(?" pattern)
      (setq i 1)
      (while (string-match-p (rx bos "(?" (any "=!")) (substring pattern i))
        (let ((end (ki-studio--group-end pattern i)))
          (push (cons (eq (aref pattern (+ i 2)) ?=)
                      (ki-studio--pcre-to-emacs
                       (concat "^" (substring pattern (+ i 3) (1- end)))))
                ahead)
          (setq i end))))
    (cons (ki-studio--pcre-to-emacs (if (> i 1) (concat "^" (substring pattern i)) pattern))
          (nreverse ahead))))

(defun ki-studio--regex-match (compiled text)
  "Match COMPILED (see `ki-studio--compile-regex') against TEXT.
Return (HIT . BELEG)."
  (pcase-let* ((`((,regexp ,fold . ,groups) . ,ahead) compiled))
    (if (and (cl-every (pcase-lambda (`(,positive ,re ,fold-a . ,_))
                         (let ((case-fold-search fold-a))
                           (eq positive (and (string-match re text) t))))
                       ahead)
             (let ((case-fold-search fold))
               (string-match regexp text)))
        (cons t (when-let* ((n (cdr (assoc "BELEG" groups))))
                  (match-string n text)))
      (cons nil nil))))


;;; Testing the on part

(defun ki-studio--on-conditions ()
  "The conditions of the buffer's `on' part, as plists."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward (rx bol "on" (* blank) eol) nil t)
      (user-error "No `on' part"))
    (let ((end (save-excursion (and (re-search-forward (rx bol (or "when" "do") (* blank) eol) nil t)
                                    (match-beginning 0))))
          conditions)
      (forward-line 1)
      (while (< (point) (or end (point-max)))
        (let ((line (string-trim (buffer-substring-no-properties (point) (line-end-position)))))
          (unless (or (string-empty-p line) (string-prefix-p "#" line))
            (push (cond
                   ((string-match (rx bos (group (+ (not space))) (* blank) "=~" (* blank)
                                      "/" (group (* nonl)) "/" eos)
                                  line)
                    (let ((attr (match-string 1 line)) (pattern (match-string 2 line)))
                      (condition-case err
                          (list :op 'regex :attr attr :regex (ki-studio--compile-regex pattern)
                                :source line)
                        (error (list :op 'skip :source line
                                     :why (error-message-string err))))))
                   ((string-match (rx bos (group (+ (not space))) (* blank) "==" (* blank)
                                      "\"" (group (* (not (any "\"")))) "\"" eos)
                                  line)
                    (list :op 'equal :attr (match-string 1 line) :value (match-string 2 line)
                          :source line))
                   ((string-match (rx bos (group (+ (not space))) (? (+ blank) "as" (+ blank) (+ word)) eos)
                                  line)
                    (list :op 'present :attr (match-string 1 line) :source line))
                   (t (list :op 'skip :source line :why "not evaluated")))
                  conditions)))
        (forward-line 1))
      (nreverse conditions))))

(defun ki-studio--attribute (vertex attr)
  "Value of ATTR on VERTEX; a free attribute is stored with a slash."
  (alist-get (intern (if (string-prefix-p "ogit/" attr) attr (concat "/" attr))) vertex))

(defun ki-studio--evaluate (condition vertex)
  "Evaluate CONDITION on VERTEX.  Return (HIT . BELEG)."
  (let ((value (ki-studio--attribute vertex (plist-get condition :attr))))
    (pcase (plist-get condition :op)
      ('present (cons (and value (not (equal value ""))) nil))
      ('equal (cons (equal (format "%s" (or value "")) (plist-get condition :value)) nil))
      ('regex (ki-studio--regex-match (plist-get condition :regex)
                                      (format "%s" (or value "")))))))

(defun ki-studio-test-on ()
  "Test the `on' part of the buffer against the focused job description.
Evaluates `A =~ /re/', `A == \"v\"' and `A [as X]' on the job description
and each WorkUnit, as selbsttest.py does; other lines are listed as not
evaluated.  Emacs, not the engine, matches here: check with the first run
that the KI fires where this hits."
  (interactive)
  (let* ((jd-id (car (ki-studio--jd-or-ask)))
         (pair (ki-studio--jd jd-id))
         (conditions (ki-studio--on-conditions))
         (file (buffer-name))
         (hits 0)
         (buffer (get-buffer-create "*ki test on*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "%s  against  %s\n" file (alist-get 'ogit/name (car pair)))
                            'face 'bold))
        (dolist (c conditions)
          (when (eq (plist-get c :op) 'skip)
            (insert (format "  not evaluated (%s): %s\n" (plist-get c :why) (plist-get c :source)))))
        (insert "\n")
        (dolist (node (cons (car pair) (cdr pair)))
          (let (failed beleg)
            (dolist (c conditions)
              (unless (eq (plist-get c :op) 'skip)
                (pcase-let ((`(,hit . ,captured) (ki-studio--evaluate c node)))
                  (if hit
                      (unless beleg (setq beleg captured))
                    (push (plist-get c :source) failed)))))
            (unless failed (setq hits (1+ hits)))
            (insert (if failed "-        " (propertize "TREFFER  " 'face 'success))
                    (format "%-12s %s\n"
                            (car (last (split-string (alist-get 'ogit/_type node "") "/")))
                            (alist-get 'ogit/name node)))
            (when (and (not failed) beleg)
              (insert "         BELEG: " (propertize beleg 'face 'font-lock-string-face) "\n"))
            (dolist (f (nreverse failed))
              (insert (propertize (format "         not met: %s\n" (truncate-string-to-width f 90 nil nil "…"))
                                  'face 'shadow)))))
        (insert (format "\n%d of %d nodes meet the on part\n" hits (1+ (length (cdr pair)))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buffer)
    (message "%d node(s) meet the on part" hits)))


;;; Issues

(defvar ki-studio--ki-names (make-hash-table :test #'equal)
  "Env file -> (TIME . HASH id -> name) of all KIs in scope.")

(defun ki-studio--ki-name (env id)
  "Name of the KI ID in ENV, from one listing of all KIs (kept an hour)."
  (let ((cached (gethash (ki-hiro-env-file env) ki-studio--ki-names)))
    (unless (and cached (< (float-time (time-since (car cached))) 3600)
                 (gethash id (cdr cached)))
      (let ((table (make-hash-table :test #'equal)))
        (dolist (ki (ki-hiro-vertices env (ki-hiro-type-query env "ogit/Automation/KnowledgeItem")
                                      "ogit/_id,ogit/name"))
          (puthash (alist-get 'ogit/_id ki) (alist-get 'ogit/name ki) table))
        (setq cached (cons (current-time) table))
        (puthash (ki-hiro-env-file env) cached ki-studio--ki-names)))
    (gethash id (cdr cached) "?")))

(defun ki-studio--files-by-name ()
  "KI name -> file under ki/ of this checkout."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (file (directory-files-recursively (ki-studio--ki-dir) (rx ".ki" eos)))
      (with-temp-buffer
        (insert-file-contents file nil 0 300)
        (when (re-search-forward (rx "name:" (* blank) "\"" (group (* (not (any "\"")))) "\"") nil t)
          (puthash (match-string 1) file table))))
    table))

(defun ki-studio--fired (env issue)
  "The KIs fired on ISSUE, in order: list of (COUNT MILLIS KI-ID NODE-ID).
Read from the \"Minimal History\" timeseries in a narrow window around
the run: without one the endpoint reads the last hour only.  Falls back
to /usedKIList, which is NOT an order; then COUNT and MILLIS are nil."
  (let* ((id (alist-get 'ogit/_id issue))
         (history (seq-find (lambda (v) (equal (alist-get 'ogit/name v) "Minimal History"))
                            (ki-hiro-gremlin env id "out('ogit/generates')"))))
    (when history
      (let* ((from (- (ki-studio--number (alist-get 'ogit/_created-on issue)) 60000))
             (to (+ (ki-studio--number (alist-get 'ogit/_modified-on issue)) 60000))
             (values (ignore-errors (ki-hiro-values env (alist-get 'ogit/_id history) from to))))
        (if values
            (sort (mapcar (lambda (item)
                            (let ((v (json-parse-string (alist-get 'value item) :object-type 'alist)))
                              (list (alist-get 'Count v) (alist-get 'timestamp item)
                                    (alist-get 'KIID v) (alist-get 'NodeID v))))
                          values)
                  (lambda (a b) (< (car a) (car b))))
          (mapcar (lambda (ki) (list nil nil ki nil))
                  (split-string (or (alist-get '/usedKIList (ki-hiro-get env (alist-get 'ogit/_id history)))
                                    "")
                                "," t)))))))

(defvar ki-studio-issue-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'ki-studio-issue-refresh)
    map))

(define-derived-mode ki-studio-issue-mode special-mode "KI-issue"
  "One issue: status, result, justification lines and the KIs that fired."
  (setq truncate-lines t))

(defvar-local ki-studio--issue-id nil "Issue this buffer shows.")

(defvar ki-studio--issue-history nil "Issue ids read before.")

(defun ki-studio--section (title)
  "Insert a section TITLE."
  (insert "\n" (propertize title 'face 'font-lock-keyword-face) "\n"))

(defun ki-studio--insert-ki (name files root)
  "Insert the KI NAME, as a button to its file when FILES has one.
FILES maps names to files; ROOT is the checkout, for the help text."
  (if-let* ((file (gethash name files)))
      (insert-text-button name 'action (lambda (_) (find-file file))
                          'help-echo (file-relative-name file root))
    (insert (propertize name 'face 'shadow))))

(defun ki-studio--waiting-on (todo)
  "Files of the KIs under ki/ whose `when' waits on TODO."
  (let ((ki-project-directory (ki-studio--ki-dir)))
    (cl-loop for (file . occs) in (ki--project-occurrences)
             when (cl-some (lambda (occ) (and (equal (ki-occ-name occ) todo)
                                              (eq (ki-occ-section occ) 'when)
                                              (eq (ki-occ-kind occ) 'read)))
                           occs)
             collect file)))

(defun ki-studio--insert-halt (env fired lines open files root)
  "Insert where a run that has not resolved stands.
FIRED and LINES are the fired KIs and justification lines of the issue,
OPEN its open ToDos; the KIs waiting on each are where to look for the
rule that did not fire.  FILES and ROOT as in `ki-studio--insert-ki'."
  (ki-studio--section "Halt")
  (let* ((names (mapcar (lambda (f) (ki-studio--ki-name env (nth 2 f))) fired))
         ;; the idle stop only says that nothing else fired any more
         (idle (equal (car (last names)) "Auto Idle"))
         (last-rule (car (last (if idle (butlast names) names)))))
    (cond ((null fired))
          ((null (caar fired))
           (insert "  zuletzt gefeuert  (unbekannt: keine Reihenfolge gelesen)\n"))
          (t (when idle
               (insert "  gestoppt durch    Auto Idle: danach feuerte nichts mehr\n"))
             (when last-rule
               (insert "  zuletzt gefeuert  ")
               (ki-studio--insert-ki last-rule files root)
               (insert "\n")))))
  (when lines
    (insert "  letzte Zeile      "
            (string-trim (car (split-string (car (last lines)) "|"))) "\n"))
  ;; ProcessIssue is the global trigger every rule waits on, not a step
  (dolist (todo (remove "ProcessIssue" open))
    (let ((waiting (ki-studio--waiting-on todo)))
      (insert (format "  %s offen, %d KIs warten darauf\n" todo (length waiting)))
      (dolist (file (seq-take waiting 15))
        (insert "    ")
        (insert-text-button (file-name-base file) 'action (lambda (_) (find-file file))
                            'help-echo (file-relative-name file root))
        (insert "\n"))
      (when (> (length waiting) 15)
        (insert (format "    … %d weitere (M-? auf %s)\n" (- (length waiting) 15) todo))))))

(defun ki-studio-issue (id)
  "Show the issue ID: status, result, open ToDos, lines and fired KIs."
  (interactive (list (string-trim (read-string "Issue id: " nil 'ki-studio--issue-history
                                               (car ki-studio--issue-history)))))
  (let* ((env (ki-studio--env))
         (root (ki-studio--root))
         (issue (ki-studio--reading "Reading the issue" (ki-hiro-get env id)))
         (fired (ki-studio--reading "Reading the fired KIs" (ki-studio--fired env issue)))
         (files (ki-studio--files-by-name))
         (todos (cons "ProcessIssue" (mapcar #'cdr (ki-studio--todo-map))))
         (buffer (get-buffer-create (format "*ki issue: …%s*" (substring id -8)))))
    (with-current-buffer buffer
      (ki-studio-issue-mode)
      (setq ki-studio--issue-id id
            default-directory root)
      (let ((inhibit-read-only t)
            (status (alist-get 'ogit/status issue)))
        (erase-buffer)
        (insert (propertize (format "Issue %s\n" id) 'face 'bold)
                (format "Status   %s\n" (propertize (format "%s" status) 'face
                                                    (pcase status
                                                      ("RESOLVED" 'success)
                                                      ((or "STOPPED" "WAITING") 'warning)
                                                      (_ 'default))))
                "Stelle   ")
        (let ((jd-id (alist-get 'ogit/Automation/originNode issue))
              (jd-name (or (alist-get '/stelle issue) "?")))
          (insert-text-button jd-name
                              'action (lambda (_)
                                        (ki-studio-select-jd jd-id jd-name)
                                        (ki-studio-av))
                              'help-echo "Focus this job description and show it")
          (insert (format "  (%s)\n" jd-id)))
        (insert (format "Lauf     %s -> %s, v%s\n"
                        (ki-studio--date (alist-get 'ogit/_created-on issue))
                        (ki-studio--date (alist-get 'ogit/_modified-on issue))
                        (alist-get 'ogit/_v issue))
                (format "Ergebnis %s / %s\n"
                        (or (alist-get '/zielEntgeltgruppe issue) "-")
                        (or (alist-get '/zielFallgruppe issue) "-")))
        (let ((open (seq-filter (lambda (todo) (alist-get (intern (concat "/" todo)) issue)) todos))
              (question (alist-get '/KlaereRueckfrage issue))
              (lines (ki-studio--json-list (alist-get '/begruendung issue))))
          (insert (format "Offen    %s\n" (if open (string-join open ", ") "-")))
          (when question
            (insert (propertize (format "Rueckfrage %s\n" question) 'face 'warning)))
          (unless (member status '("RESOLVED" "RESOLVED_EXTERNAL"))
            (ki-studio--insert-halt env fired lines open files root)))
        (ki-studio--section "Begruendung")
        (dolist (line (ki-studio--json-list (alist-get '/begruendung issue)))
          (let ((parts (mapcar #'string-trim (split-string line "|"))))
            (insert (format "  %-14s %-22s %s\n"
                            (propertize (or (nth 3 parts) "?") 'face
                                        (pcase (nth 3 parts)
                                          ("erfuellt" 'success)
                                          ("nicht_erfuellt" 'error)
                                          (_ 'shadow)))
                            (truncate-string-to-width (or (nth 2 parts) "") 22)
                            (or (car parts) line)))))
        (when-let* ((hints (ki-studio--json-list (alist-get '/hinweise issue))))
          (ki-studio--section "Hinweise")
          (dolist (line hints)
            (insert "  " (car (split-string line "|")) "\n")))
        (ki-studio--section
         (format "Gefeuert (%d%s)" (length fired)
                 (if (and fired (null (caar fired))) ", OHNE Reihenfolge" "")))
        (let ((t0 (cadr (car fired))))
          (pcase-dolist (`(,count ,millis ,ki ,_node) fired)
            (insert (format "  %4s %7s  " (or count "-")
                            (if (and millis t0) (format "%dms" (- millis t0)) "")))
            (ki-studio--insert-ki (ki-studio--ki-name env ki) files root)
            (insert "\n")))
        (ki-studio--section "Variablen")
        (dolist (kv (sort (seq-filter (lambda (kv) (string-prefix-p "/" (symbol-name (car kv)))) issue)
                          (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))
          (insert (format "  %-36s %s\n" (substring (symbol-name (car kv)) 1)
                          (truncate-string-to-width (format "%s" (cdr kv)) 120 nil nil "…"))))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun ki-studio-issue-refresh ()
  "Read this issue again."
  (interactive)
  (ki-studio-issue ki-studio--issue-id))

(defvar-keymap ki-studio-issues-mode-map
  :doc "Keymap for `ki-studio-issues-mode'."
  "RET" #'ki-studio-issues-open
  "g" #'ki-studio-issues-refresh)

(define-derived-mode ki-studio-issues-mode tabulated-list-mode "KI-issues"
  "The issues on one job description.  RET reads one, g lists again.")

(defvar-local ki-studio--issues-jd nil "(ID . NAME) this issue list shows.")

(defun ki-studio-issues-open ()
  "Read the issue on this line."
  (interactive)
  (ki-studio-issue (or (tabulated-list-get-id) (user-error "No issue on this line"))))

(defun ki-studio-issues-refresh ()
  "List the issues of this buffer's job description again."
  (interactive)
  (ki-studio-issues ki-studio--issues-jd))

(defun ki-studio-issues (&optional jd)
  "List the issues on the focused job description, newest first.
JD, an (ID . NAME), overrides the focused one.  RET reads one."
  (interactive)
  (let* ((env (ki-studio--env))
         (jd (or jd (ki-studio--jd-or-ask)))
         (root (ki-studio--root))
         (issues (ki-studio--reading "Listing issues"
                   (ki-hiro-vertices
                    env (format "%s AND ogit\\/Automation\\/originNode:\"%s\""
                                (ki-hiro-type-query env "ogit/Automation/AutomationIssue")
                                (car jd))
                    "ogit/_id,ogit/_created-on,ogit/_modified-on,ogit/status,ogit/subject")))
         (buffer (get-buffer-create (format "*ki issues: %s*" (cdr jd)))))
    (with-current-buffer buffer
      (ki-studio-issues-mode)
      (setq default-directory root
            ki-studio--issues-jd jd
            tabulated-list-format [("Angelegt" 17 t) ("Geaendert" 17 t) ("Status" 11 t)
                                   ("Subject" 14 t) ("Id" 0 nil)]
            tabulated-list-sort-key '("Angelegt" . t)
            tabulated-list-entries
            (mapcar (lambda (i)
                      (let ((id (alist-get 'ogit/_id i)))
                        (list id (vector (ki-studio--date (alist-get 'ogit/_created-on i))
                                         (ki-studio--date (alist-get 'ogit/_modified-on i))
                                         (format "%s" (alist-get 'ogit/status i))
                                         (format "%s" (alist-get 'ogit/subject i))
                                         id))))
                    issues))
      (tabulated-list-init-header)
      (tabulated-list-print))
    (pop-to-buffer buffer)
    (message "%d issues on %s" (length issues) (cdr jd))))

;;; Evil

;; evil's normal state shadows the keys of these modes (RET moves a
;; line), so bind them there too, as evil-collection does: RET, gr, q
(declare-function evil-define-key* "evil-core")
(with-eval-after-load 'evil
  (evil-define-key* 'normal ki-studio-issues-mode-map
    (kbd "RET") #'ki-studio-issues-open
    (kbd "gr") #'ki-studio-issues-refresh
    (kbd "q") #'quit-window)
  (evil-define-key* 'normal ki-studio-issue-mode-map
    (kbd "gr") #'ki-studio-issue-refresh
    (kbd "q") #'quit-window)
  (evil-define-key* 'normal ki-studio-av-mode-map
    (kbd "gr") #'ki-studio-av-refresh
    (kbd "q") #'quit-window))

(provide 'ki-studio-remote)
;;; ki-studio-remote.el ends here

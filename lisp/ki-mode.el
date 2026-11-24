;;; ki-mode.el --- Major mode for HIRO Knowledge Items  -*- lexical-binding: t; -*-

;; Author: Moamen Hraden
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages

;;; Commentary:

;; Major mode for .ki files, the rule language of the HIRO automation
;; engine.  A Knowledge Item has four sections, each keyword alone on a
;; line at column 0:
;;
;;   ki     header: name, version, description and comments
;;   on     the graph node the rule binds to
;;   when   conditions on the issue
;;   do     actions
;;
;; Provides highlighting (including /regex/ literals after =~ and ${...}
;; interpolation in strings), comments, indentation, imenu and outline.

;;; Code:

(defgroup ki nil
  "Major mode for HIRO Knowledge Items."
  :group 'languages
  :prefix "ki-")

(defcustom ki-indent-offset 2
  "Indentation of section bodies and nested blocks."
  :type 'natnum
  :safe #'natnump)

(defface ki-section-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the section keywords ki, on, when and do.")

(defconst ki-section-keywords '("ki" "on" "when" "do"))

(defconst ki-keywords
  '("if" "then" "else" "end" "begin" "with" "count" "list" "first"
    "not" "and" "or" "as"))

(defconst ki-builtins
  '("add" "delete" "query" "count" "join" "javascript" "regex"
    "log" "log.error" "log.warn" "log.info"
    "connect" "disconnect" "create_vertex" "stop_issue"))

(defconst ki--section-re
  (rx (or "ki" "on" "when" "do") (* blank) eol)
  "Section keyword, matched from the first non-blank character of a line.")


;;; Syntax

(defvar ki-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?' "\"" st)
    (modify-syntax-entry ?\\ "\\" st)
    ;; `#' only starts a comment at the beginning of a line, see
    ;; `ki-syntax-propertize'; inside regexes it is an ordinary char.
    (modify-syntax-entry ?# "." st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?/ "_" st)
    (modify-syntax-entry ?@ "_" st)
    (modify-syntax-entry ?_ "_" st)
    (dolist (c '(?= ?< ?> ?! ?~ ?& ?| ?* ?+ ?- ?% ?$ ?:))
      (modify-syntax-entry c "." st))
    st)
  "Syntax table for `ki-mode'.")

(defun ki--propertize-regexp ()
  "Mark the /regex/ literal whose opening slash was just matched.
The slashes become string fences, so parens, quotes and `#' inside
the regex do not confuse indentation or comments.  Regexes inside
strings or comments, and unterminated ones, are left alone."
  (let ((open (match-beginning 1)))
    (unless (nth 8 (save-excursion (save-match-data
                                     (syntax-ppss (match-beginning 0)))))
      (goto-char (1+ open))
      (while (progn (skip-chars-forward "^/\\\\\n")
                    (eq (char-after) ?\\))
        (forward-char 1)
        (unless (eolp) (forward-char 1)))
      (when (eq (char-after) ?/)
        (let ((fence (string-to-syntax "|")))
          (put-text-property open (1+ open) 'syntax-table fence)
          (put-text-property (point) (1+ (point)) 'syntax-table fence))
        (forward-char 1))))
  nil)

(defconst ki-syntax-propertize
  (syntax-propertize-rules
   ((rx bol (* blank) (group "#")) (1 "<"))
   ((rx "=~" (* blank) (group "/")) (1 (ki--propertize-regexp))))
  "Function marking line comments and /regex/ literals.")


;;; Font lock

(defvar ki-font-lock-keywords
  `((,(rx bol (or "ki" "on" "when" "do") (* blank) eol) . 'ki-section-face)
    ;; header fields and keyword arguments
    (,(rx bol (+ blank) (group (or "name" "version" "description")) ":")
     1 'font-lock-property-name-face)
    (,(rx (group (any "a-z_") (* (any "a-zA-Z0-9_"))) ":" (or blank eol))
     1 'font-lock-property-use-face)
    ;; calls
    (,(concat "\\_<" (regexp-opt ki-builtins t) "(")
     1 'font-lock-builtin-face)
    (,(rx symbol-start (group (any "a-z_") (* (any "a-zA-Z0-9_."))) "(")
     1 'font-lock-function-call-face)
    (,(regexp-opt ki-keywords 'symbols) . 'font-lock-keyword-face)
    ;; LOCAL::X, QUERY::ogit/name
    (,(rx symbol-start (group (or "LOCAL" "QUERY")) "::")
     1 'font-lock-builtin-face)
    ;; bindings: `... as NAME'
    (,(rx symbol-start "as" (+ blank) (group (+ (any "A-Za-z0-9_"))))
     1 'font-lock-variable-name-face)
    (,(rx symbol-start "ogit/" (* (any "A-Za-z0-9_/"))) . 'font-lock-type-face)
    (,(rx "@" (+ (any "A-Za-z0-9_"))) . 'font-lock-preprocessor-face)
    (,(rx symbol-start (any "A-Z") (+ (any "A-Z0-9_")) symbol-end)
     . 'font-lock-variable-use-face)
    (,(rx symbol-start (? "-") (+ digit) (? "." (+ digit)) symbol-end)
     . 'font-lock-number-face)
    (,(rx (or "==" "!=" ">=" "<=" "=~" "!~" "=" ">" "<"))
     . 'font-lock-operator-face)
    ;; these live inside strings and regexes, so they override
    (,(rx "${" (+ (not (any "}\n"))) "}") 0 'font-lock-variable-use-face t)
    (,(rx "(?<" (group (any "A-Za-z_") (* (any "A-Za-z0-9_"))) ">")
     1 'font-lock-variable-name-face t))
  "Highlighting for `ki-mode'.")

(defun ki--syntactic-face (state)
  "Face for the string or comment described by syntax STATE."
  (cond ((eq (nth 3 state) t) 'font-lock-regexp-face)
        ((nth 3 state) 'font-lock-string-face)
        (t 'font-lock-comment-face)))


;;; Indentation

(defun ki--code-line-p ()
  "Non-nil if the current line holds code, not just blanks or a comment."
  (save-excursion
    (back-to-indentation)
    (not (or (eolp) (eq (char-after) ?#)))))

(defun ki--previous-code-line ()
  "Move to the previous code line.  Return nil if there is none."
  (let (found)
    (while (and (not found) (zerop (forward-line -1)))
      (setq found (ki--code-line-p)))
    found))

(defun ki--ppss (pos)
  "Like `syntax-ppss' at POS, but without moving point."
  (save-excursion (syntax-ppss pos)))

(defun ki--depth-at (pos)
  "Paren depth at POS."
  (car (ki--ppss pos)))

(defun ki--line-ends-with (regexp)
  "Non-nil if the code on the current line ends with REGEXP."
  (save-excursion
    (end-of-line)
    (skip-chars-backward " \t")
    (and (looking-back regexp (line-beginning-position))
         (not (nth 8 (syntax-ppss (match-beginning 0)))))))

(defun ki--continues-previous-p ()
  "Non-nil if the previous code line ends with a comma."
  (save-excursion
    (and (ki--previous-code-line)
         (ki--line-ends-with ","))))

(defun ki--statement-start ()
  "Indentation of the statement that the current line ends.
Follows closing parens back to their opener and comma-continued lines
back to their first line."
  (let (done)
    (while (not done)
      (let* ((start (ki--ppss (line-beginning-position)))
             (end-depth (ki--depth-at (line-end-position))))
        (cond
         ((< end-depth (car start))
          (goto-char (nth end-depth (nth 9 start))))
         ((ki--continues-previous-p)
          (ki--previous-code-line))
         (t (setq done t))))))
  (current-indentation))

(defun ki--indentation-after-line ()
  "Indentation for a line following the current code line."
  (back-to-indentation)
  (let ((bol-depth (ki--depth-at (line-beginning-position)))
        (eol-depth (ki--depth-at (line-end-position))))
    (cond
     ((looking-at ki--section-re) ki-indent-offset)
     ((ki--line-ends-with (rx symbol-start (or "then" "else" "begin")))
      (+ (current-indentation) ki-indent-offset))
     ((> eol-depth bol-depth)
      ;; an argument list stays open: indent past the callee, or line up
      ;; with the first element of a [ list
      (let ((opener (nth 1 (ki--ppss (line-end-position)))))
        (save-excursion
          (goto-char opener)
          (if (eq (char-after) ?\[)
              (progn (forward-char 1)
                     (skip-chars-forward " \t")
                     (if (eolp)
                         (+ (current-indentation) ki-indent-offset)
                       (current-column)))
            (skip-chars-backward "A-Za-z0-9_.")
            (+ (current-column) ki-indent-offset)))))
     ((and (looking-at (rx (? "not" (+ blank)) (or "with" "count") blank))
           (not (ki--line-ends-with (rx symbol-start "end"))))
      (+ (current-indentation) ki-indent-offset))
     ((ki--line-ends-with ",")
      (if (ki--continues-previous-p)
          (current-indentation)
        (+ (current-indentation) ki-indent-offset)))
     (t (ki--statement-start)))))

(defun ki--calculate-indentation ()
  "Indentation for the current line, or nil to leave it alone."
  (save-excursion
    (back-to-indentation)
    (let ((ppss (ki--ppss (line-beginning-position))))
      (cond
       ((nth 3 ppss) nil)
       ((looking-at ki--section-re) 0)
       ((looking-at (rx (any ")]")))
        (goto-char (nth 1 ppss))
        (ki--statement-start))
       (t
        (let ((closer (looking-at (rx (or "end" "else") symbol-end))))
          (if (not (ki--previous-code-line))
              0
            (let ((indent (ki--indentation-after-line)))
              (if closer
                  (max 0 (- indent ki-indent-offset))
                indent)))))))))

(defun ki-indent-line ()
  "Indent the current line as KI code."
  (interactive)
  (let ((indent (ki--calculate-indentation)))
    (if (not indent)
        'noindent
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion (indent-line-to indent))))))


;;; Mode

;;;###autoload
(define-derived-mode ki-mode prog-mode "KI"
  "Major mode for HIRO Knowledge Items."
  (setq-local syntax-propertize-function ki-syntax-propertize)
  (setq-local font-lock-defaults
              '(ki-font-lock-keywords
                nil nil nil nil
                (font-lock-syntactic-face-function . ki--syntactic-face)))
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx "#" (* "#") (* blank)))
  (setq-local indent-line-function #'ki-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local outline-regexp (rx (or "ki" "on" "when" "do") (* blank) eol))
  (setq-local outline-level (lambda () 1))
  (setq-local imenu-generic-expression
              `(("Bindings"
                 ,(rx symbol-start "as" (+ blank)
                      (group (any "A-Z") (+ (any "A-Za-z0-9_"))))
                 1)
                ("Bindings"
                 ,(rx "(?<" (group (any "A-Za-z_") (* (any "A-Za-z0-9_"))) ">")
                 1)
                (nil ,(rx bol (group (or "ki" "on" "when" "do")) (* blank) eol)
                     1))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ki\\'" . ki-mode))

(provide 'ki-mode)
;;; ki-mode.el ends here

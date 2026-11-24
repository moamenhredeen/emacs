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
;;
;; Across the KI files of a project it also provides xref (M-. lists the
;; KIs that write a variable, M-? every use), completion of variable and
;; attribute names, and a flymake backend for the traps of the language
;; that pass the validator and then never fire.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'xref)
(require 'flymake)

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

(defconst ki--section-line-re
  (rx bol (group (or "ki" "on" "when" "do")) (* blank) eol)
  "Section line, the keyword in group 1.")


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


;;; Variable occurrences

(cl-defstruct (ki-occ (:constructor ki-occ-create) (:copier nil))
  "One use of a variable or attribute name in a KI.
KIND is `node' (read in `on'), `read' or `guard' (read in `when',
`guard' after `not'), `assign' (bare `=' or `set'), `add', `delete' or
`read' in `do'."
  name ctx kind section beg end line col text)

(defconst ki--symbol-re
  (rx symbol-start (any "A-Za-z_") (* (or (syntax word) (syntax symbol))))
  "A name, including an `@context' suffix and `/' path separators.")

(defconst ki--non-variables
  (append ki-section-keywords ki-keywords ki-builtins '("true" "false")))

(defun ki--setup-syntax ()
  "Give the current buffer the syntax of KI code, without the mode."
  (set-syntax-table ki-mode-syntax-table)
  (setq-local syntax-propertize-function ki-syntax-propertize))

(defun ki--variable-token-p (beg end name)
  "Non-nil if NAME between BEG and END is a variable or attribute.
Excludes text in strings, comments and regexes, calls, keyword
arguments, LOCAL::/QUERY:: names and UPPERCASE aliases."
  (and (not (nth 8 (ki--ppss beg)))
       (not (memq (char-before beg) '(?: ?.)))
       (not (memq (char-after end) '(?\( ?. ?:)))
       (not (member name ki--non-variables))
       (let ((case-fold-search nil))
         (not (string-match-p (rx bos (any "A-Z") (* (any "A-Z0-9_")) eos)
                              name)))))

(defun ki--occurrence-kind (section beg end)
  "Classify the name between BEG and END, which sits in SECTION."
  (save-excursion
    (pcase section
      ('on 'node)
      ('when
       (goto-char beg)
       (if (looking-back (rx "not" (+ blank)) (line-beginning-position))
           'guard
         'read))
      (_
       (goto-char beg)
       (cond
        ((looking-back (rx "add(" (* blank)) (line-beginning-position)) 'add)
        ((looking-back (rx "set(" (* blank)) (line-beginning-position)) 'assign)
        ((looking-back (rx "delete(" (* blank)) (line-beginning-position))
         'delete)
        ((progn (goto-char end)
                (looking-at (rx (? "[" (* (not (any "]\n"))) "]")
                                (* blank) "=" (not (any "=~")))))
         'assign)
        (t 'read))))))

(defun ki--scan ()
  "Return the variable and attribute occurrences of the buffer, in order."
  (save-excursion
    (syntax-propertize (point-max))
    (goto-char (point-min))
    (let ((line 1) section occs)
      (while (not (eobp))
        (if (looking-at ki--section-line-re)
            (setq section (intern (match-string-no-properties 1)))
          (when (memq section '(on when do))
            (let ((bol (point))
                  (eol (line-end-position)))
              (while (re-search-forward ki--symbol-re eol t)
                (let* ((beg (match-beginning 0))
                       (end (match-end 0))
                       (parts (split-string (match-string-no-properties 0) "@"))
                       (name (car parts)))
                  (when (ki--variable-token-p beg end name)
                    (push (ki-occ-create
                           :name name :ctx (cadr parts)
                           :kind (ki--occurrence-kind section beg end)
                           :section section :beg beg :end end
                           :line line :col (- beg bol)
                           :text (buffer-substring-no-properties bol eol))
                          occs)))))))
        (forward-line 1)
        (setq line (1+ line)))
      (nreverse occs))))


;;; Project index

(defcustom ki-root-markers '("README.md" ".git")
  "Files marking the directory whose KI files belong together.
The first marker found above a KI file wins, not the nearest."
  :type '(repeat string))

(defvar-local ki-project-directory nil
  "Directory holding the KI files of this project.
When nil, found from `ki-root-markers'.")
(put 'ki-project-directory 'safe-local-variable #'stringp)

(defun ki--root ()
  "Directory whose KI files belong to the current one, or nil."
  (or ki-project-directory
      (let ((dir (if buffer-file-name
                     (file-name-directory buffer-file-name)
                   default-directory)))
        (seq-some (lambda (marker) (locate-dominating-file dir marker))
                  ki-root-markers))))

(defvar ki--file-cache (make-hash-table :test #'equal)
  "File name -> (MODIFICATION-TIME . OCCURRENCES).")

(defvar ki--from-buffers t
  "Non-nil to read files with unsaved changes from their buffer.")

(defun ki--file-occurrences (file)
  "Occurrences in FILE, from its buffer when that has unsaved changes.
See `ki--from-buffers'."
  (let ((buffer (get-file-buffer file)))
    (if (and ki--from-buffers buffer (buffer-modified-p buffer))
        (with-current-buffer buffer (ki--scan))
      (let ((mtime (file-attribute-modification-time (file-attributes file)))
            (cached (gethash file ki--file-cache)))
        (if (and cached (equal (car cached) mtime))
            (cdr cached)
          (let ((occs (with-temp-buffer
                        (insert-file-contents file)
                        (ki--setup-syntax)
                        (ki--scan))))
            (puthash file (cons mtime occs) ki--file-cache)
            occs))))))

(defun ki--project-occurrences ()
  "Alist (FILE . OCCURRENCES) over all KI files of the project."
  (when-let* ((root (ki--root)))
    (mapcar (lambda (file) (cons file (ki--file-occurrences file)))
            (directory-files-recursively root (rx ".ki" eos)))))

(defvar ki--names-cache nil
  "(ROOT TIME . NAMES), see `ki--names'.")

(defun ki--names ()
  "Alist (NAME . ISSUE-P) of the names used in the project.
ISSUE-P is non-nil for issue variables, nil for node attributes.
Completion asks on every key, and checking 500 files takes a noticeable
moment, so this is kept until a KI file is saved or 5 minutes pass."
  (let ((root (ki--root)))
    (if (and ki--names-cache
             (equal (car ki--names-cache) root)
             (< (float-time (time-since (cadr ki--names-cache))) 300))
        (cddr ki--names-cache)
      (let ((table (make-hash-table :test #'equal))
            ;; saved files only, or the word being typed becomes a name
            (ki--from-buffers nil))
        (pcase-dolist (`(,_ . ,occs) (ki--project-occurrences))
          (dolist (occ occs)
            (when (or (not (eq (ki-occ-kind occ) 'node))
                      (not (gethash (ki-occ-name occ) table)))
              (puthash (ki-occ-name occ) (not (eq (ki-occ-kind occ) 'node))
                       table))))
        (let (names)
          (maphash (lambda (k v) (push (cons k v) names)) table)
          (setq ki--names-cache (cons root (cons (current-time) names)))
          names)))))

(defun ki--forget-names ()
  "Drop the cached names; a saved KI may use new ones."
  (setq ki--names-cache nil))


;;; Xref

(defun ki-xref-backend ()
  "Xref backend for KI variables."
  'ki)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql ki)))
  (when-let* ((symbol (thing-at-point 'symbol t)))
    (car (split-string symbol "@"))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql ki)))
  (mapcar #'car (ki--names)))

(defun ki--xrefs (name predicate)
  "Xrefs to the occurrences of NAME that satisfy PREDICATE."
  (let (xrefs)
    (pcase-dolist (`(,file . ,occs) (ki--project-occurrences))
      (dolist (occ occs)
        (when (and (equal (ki-occ-name occ) name) (funcall predicate occ))
          (push (xref-make (format "%-6s %s" (ki-occ-kind occ)
                                   (string-trim (ki-occ-text occ)))
                           (xref-make-file-location file (ki-occ-line occ)
                                                    (ki-occ-col occ)))
                xrefs))))
    (nreverse xrefs)))

(cl-defmethod xref-backend-definitions ((_backend (eql ki)) name)
  ;; the KIs that write it; a node attribute or an engine variable like
  ;; ProcessIssue has no writer, so fall back to every use
  (or (ki--xrefs name (lambda (occ) (memq (ki-occ-kind occ) '(assign add))))
      (ki--xrefs name #'always)))

(cl-defmethod xref-backend-references ((_backend (eql ki)) name)
  (ki--xrefs name #'always))


;;; Completion

(defun ki-completion-at-point ()
  "Complete variable and attribute names, keywords and builtins."
  (unless (nth 8 (ki--ppss (point)))
    (let* ((pos (point))
           (bounds (bounds-of-thing-at-point 'symbol))
           (start (if bounds (car bounds) pos))
           (end (if bounds (cdr bounds) pos))
           (at (save-excursion
                 (goto-char start)
                 (search-forward "@" pos t))))
      (if at
          (list at end '("any" "self" "scope") :exclusive 'no)
        (let ((names (ki--names)))
          (list start end
                (append (mapcar #'car names) ki-builtins ki-keywords)
                :exclusive 'no
                :annotation-function
                (lambda (candidate)
                  (let ((entry (assoc candidate names)))
                    (cond (entry (if (cdr entry) " issue" " node"))
                          ((member candidate ki-builtins) " builtin"))))))))))


;;; Lints

(defvar ki-lint-functions
  '(ki-lint-scope-in-when
    ki-lint-leading-slash
    ki-lint-regex-literals
    ki-lint-write-once
    ki-lint-read-and-write
    ki-lint-execidle
    ki-lint-count-in-with)
  "Functions run by `ki-flymake', a hook.
Each is called with the occurrences of the buffer (see `ki--scan') and
returns a list of (BEG END TYPE MESSAGE), TYPE being :error, :warning
or :note.")

(defun ki--map-section-lines (sections fn)
  "Call FN at the start of each line in SECTIONS, a list of symbols.
FN gets the section; collect and return its non-nil results."
  (save-excursion
    (goto-char (point-min))
    (let (section results)
      (while (not (eobp))
        (if (looking-at ki--section-line-re)
            (setq section (intern (match-string-no-properties 1)))
          (when (memq section sections)
            (when-let* ((result (save-excursion (funcall fn section))))
              (push result results))))
        (forward-line 1))
      (nreverse results))))

(defun ki--regex-literals ()
  "List of (OPEN . END) for each /regex/ literal after `=~'.
END is after the closing slash, nil for an unterminated literal."
  (save-excursion
    (syntax-propertize (point-max))
    (goto-char (point-min))
    (let (literals)
      (while (re-search-forward (rx "=~" (* blank) "/") nil t)
        (let ((open (1- (point))))
          (unless (nth 8 (ki--ppss (match-beginning 0)))
            (push (cons open
                        (and (equal (get-text-property open 'syntax-table)
                                    (string-to-syntax "|"))
                             (ignore-errors (scan-sexps open 1))))
                  literals))))
      (nreverse literals))))

(defun ki-lint-scope-in-when (occs)
  "A bare X@scope in `when' passes the validator and crashes the compiler."
  (cl-loop for occ in occs
           when (and (eq (ki-occ-section occ) 'when)
                     (equal (ki-occ-ctx occ) "scope"))
           collect (list (ki-occ-beg occ) (ki-occ-end occ) :error
                         "X@scope in `when' crashes the compiler; use @self or @any")))

(defun ki-lint-leading-slash (_occs)
  "A free attribute is written without its leading slash."
  (ki--map-section-lines
   '(on when)
   (lambda (_section)
     (when (looking-at (rx (* blank) (? "not" (+ blank))
                           (group "/" (any "A-Za-z_"))))
       (list (match-beginning 1) (line-end-position) :error
             "Free attribute with a leading `/': write `/x' as `x', or the rest is read as a regex")))))

(defun ki-lint-regex-literals (_occs)
  "Unterminated literals, literals cut short by a `/', umlauts in classes."
  (let (diags)
    (pcase-dolist (`(,open . ,end) (ki--regex-literals))
      (if (not end)
          (push (list open (save-excursion (goto-char open) (line-end-position))
                      :error "Unterminated regex literal")
                diags)
        (let ((rest (save-excursion
                      (goto-char end)
                      (skip-chars-forward " \t")
                      (unless (or (eolp) (looking-at (rx (or "or" "and") symbol-end)))
                        (point)))))
          (when rest
            (push (list (1- end) (save-excursion (goto-char rest) (line-end-position))
                        :error "A `/' ends the regex literal here; escape it as `\\/'")
                  diags)))
        (setq diags (nconc (ki--class-umlauts (1+ open) (1- end)) diags))))
    diags))

(defun ki--class-umlauts (beg end)
  "Diagnostics for [...] classes with non-ASCII between BEG and END.
The regex works on bytes, and an umlaut is two bytes in UTF-8, so such a
class matches half an umlaut.  Followed by a quantifier it still
matches the whole word, so only a class without one is reported."
  (let ((pos beg) class-start non-ascii diags)
    (while (< pos end)
      (let ((c (char-after pos)))
        (cond
         ((eq c ?\\) (setq pos (1+ pos)))
         ((and (not class-start) (eq c ?\[))
          (setq class-start pos non-ascii nil)
          ;; a ] right after [ or [^ is a literal
          (when (eq (char-after (1+ pos)) ?^) (setq pos (1+ pos)))
          (when (eq (char-after (1+ pos)) ?\]) (setq pos (1+ pos))))
         ((and class-start (eq c ?\]))
          (when (and non-ascii (not (memq (char-after (1+ pos)) '(?* ?+ ?{))))
            (push (list class-start (1+ pos) :warning
                        "Non-ASCII in a character class matches one byte of it; write it out: (a|ä|ae)")
                  diags))
          (setq class-start nil))
         ((and class-start (> c 127)) (setq non-ascii t))))
      (setq pos (1+ pos)))
    diags))

(defun ki--issue-name-p (occ)
  "Non-nil if OCC names an issue variable rather than a node attribute."
  (not (string-prefix-p "ogit/" (ki-occ-name occ))))

(defun ki-lint-write-once (occs)
  "A registered variable is assigned at most once per KI."
  (let ((seen (make-hash-table :test #'equal)) diags)
    (dolist (occ occs)
      (when (and (eq (ki-occ-kind occ) 'assign) (ki--issue-name-p occ))
        (if (gethash (ki-occ-name occ) seen)
            (push (list (ki-occ-beg occ) (ki-occ-end occ) :warning
                        (format "%s is assigned twice: a registered variable is write-once per KI, even across branches"
                                (ki-occ-name occ)))
                  diags)
          (puthash (ki-occ-name occ) t seen))))
    diags))

(defun ki-lint-read-and-write (occs)
  "A registered variable may not be read and assigned by the same KI."
  (let ((read (cl-loop for occ in occs
                       when (and (eq (ki-occ-kind occ) 'read)
                                 (eq (ki-occ-section occ) 'when))
                       collect (ki-occ-name occ))))
    (cl-loop for occ in occs
             when (and (eq (ki-occ-kind occ) 'assign)
                       (ki--issue-name-p occ)
                       (member (ki-occ-name occ) read))
             collect (list (ki-occ-beg occ) (ki-occ-end occ) :warning
                           (format "%s is read in `when' and assigned here; one KI cannot read and write a registered variable"
                                   (ki-occ-name occ))))))

(defun ki-lint-execidle (occs)
  "A KI testing ExecIdle has to switch itself off."
  (when-let* ((idle (cl-find-if (lambda (occ)
                                 (and (equal (ki-occ-name occ) "ExecIdle")
                                      (eq (ki-occ-section occ) 'when)))
                               occs)))
    (let ((waits (cl-loop for occ in occs
                          when (eq (ki-occ-kind occ) 'read)
                          collect (ki-occ-name occ)))
          (guards (cl-loop for occ in occs
                           when (eq (ki-occ-kind occ) 'guard)
                           collect (ki-occ-name occ))))
      (unless (or (cl-some (lambda (occ)
                             (pcase (ki-occ-kind occ)
                               ('delete (member (ki-occ-name occ) waits))
                               ((or 'assign 'add) (member (ki-occ-name occ) guards))))
                           occs)
                  (save-excursion
                    (goto-char (point-min))
                    (re-search-forward (rx symbol-start "stop_issue(") nil t)))
        (list (list (ki-occ-beg idle) (ki-occ-end idle) :warning
                    "Tests ExecIdle but never switches itself off: delete the ToDo it waits on, or set a marker it guards with `not'"))))))

(defun ki-lint-count-in-with (_occs)
  "Comparing a `count' alias inside the enclosing `with' never fires."
  (let (stack aliases)
    (ki--map-section-lines
     '(on when)
     (lambda (_section)
       (back-to-indentation)
       (let ((closed (ki--line-ends-with (rx symbol-start "end"))))
         (cond
          ((looking-at (rx "end" symbol-end)) (pop stack) nil)
          ((looking-at (rx (? "not" (+ blank)) "with" blank))
           (unless closed (push 'with stack))
           nil)
          ((looking-at (rx "count" blank (* nonl) blank "as" (+ blank)
                           (group (+ (any "A-Z0-9_")))))
           (when (memq 'with stack)
             (push (match-string-no-properties 1) aliases))
           (unless closed (push 'count stack))
           nil)
          ((and (memq 'with stack)
                (looking-at (rx (group (+ (any "A-Z0-9_"))) (* blank)
                                (or "<" ">" "==" "!=")))
                (member (match-string-no-properties 1) aliases))
           (list (match-beginning 1) (match-end 1) :warning
                 "Inside `with' this is read as an attribute of the alias and never fires; compare after the `end' of `with'"))))))))

(defun ki-flymake (report-fn &rest _)
  "Flymake backend running `ki-lint-functions'."
  (let ((occs (ki--scan)) diags)
    (run-hook-wrapped
     'ki-lint-functions
     (lambda (fn)
       (condition-case err
           (setq diags (nconc (funcall fn occs) diags))
         (error (message "ki-mode: lint %s failed: %S" fn err)))
       nil))
    (funcall report-fn
             (mapcar (pcase-lambda (`(,beg ,end ,type ,message))
                       (flymake-make-diagnostic (current-buffer) beg end
                                                type message))
                     diags))))


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
                     1)))
  (add-hook 'xref-backend-functions #'ki-xref-backend nil t)
  (add-hook 'completion-at-point-functions #'ki-completion-at-point nil t)
  (add-hook 'flymake-diagnostic-functions #'ki-flymake nil t)
  (add-hook 'after-save-hook #'ki--forget-names nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ki\\'" . ki-mode))

(provide 'ki-mode)
;;; ki-mode.el ends here

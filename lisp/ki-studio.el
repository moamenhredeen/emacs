;;; ki-studio.el --- DLR conventions on top of ki-mode  -*- lexical-binding: t; -*-

;; Author: Moamen Hraden
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages

;;; Commentary:

;; `ki-studio-mode' is a minor mode for KI files inside a dlr_core
;; checkout, recognised by bin/ki_tool.py.  ki-mode knows the language;
;; this knows the conventions of the DLR rule set, from ki/README.md and
;; the ki-bauen skill of the dlr-teaching plugin:
;;
;; - lints: the justification line in 8 parts, its key and result, the
;;   header, the folder's agenda ToDo, and what an own KI must not carry
;; - eldoc: which part of a begruendung/hinweise line point is in
;; - `ki-studio-new-ki': a new KI in a section folder, from the skeleton

;;; Code:

(require 'ki-mode)

(defgroup ki-studio nil
  "DLR conventions for HIRO Knowledge Items."
  :group 'ki
  :prefix "ki-studio-")

(defun ki-studio--root ()
  "Root of the dlr_core checkout holding the current file, or nil."
  (when-let* ((dir (if buffer-file-name
                      (file-name-directory buffer-file-name)
                    default-directory))
             (root (locate-dominating-file dir "bin/ki_tool.py")))
    (expand-file-name root)))

(defun ki-studio--ki-dir ()
  "The ki/ directory of the checkout."
  (if-let* ((root (ki-studio--root)))
      (expand-file-name "ki" root)
    (user-error "Not inside a dlr_core checkout")))


;;; Folders and their agenda ToDo

(defvar ki-studio--todo-cache nil
  "(README MODIFICATION-TIME . ALIST), see `ki-studio--todo-map'.")

(defun ki-studio--todo-map ()
  "Alist (FOLDER . TODO) from the table under \"Why subfolders\".
Folders without an agenda ToDo (00-lifecycle, 08-hinweise) are absent."
  (let* ((readme (expand-file-name "README.md" (ki-studio--ki-dir)))
         (mtime (file-attribute-modification-time (file-attributes readme))))
    (if (and ki-studio--todo-cache
             (equal (car ki-studio--todo-cache) readme)
             (equal (cadr ki-studio--todo-cache) mtime))
        (cddr ki-studio--todo-cache)
      (let (alist)
        (with-temp-buffer
          (insert-file-contents readme)
          (while (re-search-forward
                  (rx bol "| `" (group (+ (not (any "/`")))) "/` | `"
                      (group (+ (any "A-Za-z"))) "`")
                  nil t)
            (push (cons (match-string 1) (match-string 2)) alist)))
        (setq ki-studio--todo-cache (cons readme (cons mtime alist)))
        alist))))


;;; The justification line

(defconst ki-studio--line-parts
  '(("begruendung"
     ("Satz" . "der Schluss des Schritts als lesbarer deutscher Satz")
     ("Stufe" . "categoryIdentifier (I.EG6), nur bei der Regel, die eine Stufe feststellt")
     ("Merkmal" . "Variablenname der Anforderung, genau eines je Zeile")
     ("Ergebnis" . "erfuellt | nicht_erfuellt | nicht_bewertet | info")
     ("WorkUnit" . "${AVID}, leer wenn die Zeile die ganze Stelle betrifft")
     ("Anteil" . "${ZEITANTEIL}, oder der Anteil, mit dem die Stufe entschieden wurde")
     ("Beleg" . "${BELEG}: das getroffene Segment, per Named Capture")
     ("Quelle" . "Normstelle, Merkmalsknoten-Id, Suchparameter; nie eine Sitzung"))
    ("hinweise"
     ("Satz" . "was die Stelle zeigt, ohne Merkmal")
     ("Beleg" . "worauf die Regel trifft")
     ("Quelle" . "Normstelle, Suchparameter; nie eine Sitzung")))
  "Parts of a begruendung and a hinweise line (ki/README.md).")

(defconst ki-studio--results '("erfuellt" "nicht_erfuellt" "nicht_bewertet" "info"))

(defun ki-studio--lines ()
  "Writes to begruendung and hinweise in the buffer.
A list of plists (:kind :form :beg :end :key :key-beg :string-beg
:string-end): FORM is `add', `set' or `assign', KEY the literal key
string or nil, STRING-BEG/END the value string including its quotes."
  (save-excursion
    (goto-char (point-min))
    (let (lines)
      (while (re-search-forward
              (rx symbol-start (group (or "begruendung" "hinweise")) "[") nil t)
        (let ((beg (match-beginning 1)) (end (match-end 1))
              (kind (match-string-no-properties 1)))
          (unless (nth 8 (ki--ppss beg))
            (let ((form (save-excursion
                          (goto-char beg)
                          (cond ((looking-back (rx "add(" (* blank)) (line-beginning-position)) 'add)
                                ((looking-back (rx "set(" (* blank)) (line-beginning-position)) 'set)
                                (t 'assign))))
                  key key-beg string-beg string-end)
              (when (looking-at (rx "\"" (group (* (not (any "\"\n")))) "\""))
                (setq key (match-string-no-properties 1)
                      key-beg (match-beginning 0)))
              (when (re-search-forward (rx point (* (not (any "\n="))) "]" (* blank)
                                           "=" (* blank) "\"")
                                       (line-end-position) t)
                (setq string-beg (1- (point))
                      string-end (ignore-errors (scan-sexps string-beg 1))))
              (push (list :kind kind :form form :beg beg :end end
                          :key key :key-beg key-beg
                          :string-beg string-beg :string-end string-end)
                    lines)))))
      (nreverse lines))))

(defun ki-studio-lint-lines (_occs)
  "The begruendung and hinweise lines: form, key, parts and result."
  (let ((todos (mapcar #'cdr (ki-studio--todo-map))) diags)
    (dolist (line (ki-studio--lines))
      (let* ((kind (plist-get line :kind))
             (beg (plist-get line :beg))
             (end (plist-get line :end))
             (key (plist-get line :key))
             (sbeg (plist-get line :string-beg))
             (send (plist-get line :string-end))
             (parts (length (cdr (assoc kind ki-studio--line-parts)))))
        (pcase (plist-get line :form)
          ('set (push (list beg end :error
                            (format "%s is written with add(), never set: set replaces earlier lines" kind))
                      diags))
          ('assign (push (list beg end :error
                               (format "%s is written with add(); a bare assignment replaces earlier lines" kind))
                         diags)))
        ;; a rule may write the line of another section, but the key is
        ;; always a section ToDo
        (when (and key (not (member key todos)))
          (push (list (plist-get line :key-beg) (+ (plist-get line :key-beg) (length key) 2)
                      :warning (format "Key \"%s\" is not a section ToDo; use the ToDo of the section the line belongs to"
                                       key))
                diags))
        (when (and sbeg send)
          (let* ((text (buffer-substring-no-properties (1+ sbeg) (1- send)))
                 (fields (split-string text "|")))
            (if (/= (length fields) parts)
                (push (list sbeg send :error
                            (format "%s line has %d separators, needs %d (%d parts)"
                                    kind (1- (length fields)) (1- parts) parts))
                      diags)
              (when (equal kind "begruendung")
                (let ((result (string-trim (nth 3 fields))))
                  (unless (or (member result ki-studio--results)
                              (string-search "${" result))
                    (push (list sbeg send :error
                                (format "Part 4 (Ergebnis) is \"%s\", must be one of: %s"
                                        result (string-join ki-studio--results ", ")))
                          diags)))))))))
    diags))

(defun ki-studio-eldoc (callback &rest _)
  "Name the part of the begruendung or hinweise line point is in."
  (let* ((pos (point))
         (ppss (ki--ppss pos)))
    (when (eq (nth 3 ppss) ?\")
      (let ((start (nth 8 ppss)))
        (save-excursion
          (goto-char start)
          (when (looking-back (rx (group (or "begruendung" "hinweise"))
                                  "[" (* (not (any "]\n"))) "]" (* blank) "=" (* blank))
                              (line-beginning-position))
            (let* ((kind (match-string-no-properties 1))
                   (parts (cdr (assoc kind ki-studio--line-parts)))
                   (index (cl-count ?| (buffer-substring-no-properties start pos))))
              (when-let* ((part (nth index parts)))
                (funcall callback
                         (format "%s %d/%d %s: %s" kind (1+ index) (length parts)
                                 (propertize (car part) 'face 'eldoc-highlight-function-argument)
                                 (cdr part)))))))))))


;;; Header, folder and own-KI conventions

(defun ki-studio-lint-header (_occs)
  "name matches the file name, description is present, version N@almato."
  (save-excursion
    (goto-char (point-min))
    (let ((header-end (or (save-excursion (re-search-forward (rx bol (or "on" "when" "do") eol) nil t))
                          (point-max)))
          diags)
      (if (re-search-forward (rx bol (+ blank) "name:" (* blank) "\"" (group (* (not (any "\"")))) "\"")
                             header-end t)
          (when (and buffer-file-name
                     (not (equal (match-string-no-properties 1)
                                 (file-name-base buffer-file-name))))
            (push (list (match-beginning 1) (match-end 1) :warning
                        "name differs from the file name; the review tools match files to the pool by name")
                  diags))
        (push (list (point-min) (line-end-position) :error "No name: in the header") diags))
      (goto-char (point-min))
      (unless (re-search-forward (rx bol (+ blank) "description:" (* blank) "\"" (not (any "\"")))
                                 header-end t)
        (push (list (point-min) (save-excursion (goto-char (point-min)) (line-end-position))
                    :warning "No description: KAT shows it to the expert, and update needs it")
              diags))
      (goto-char (point-min))
      (when (re-search-forward (rx bol (+ blank) "version:" (* blank) "\"" (group (* (not (any "\"")))) "\"")
                               header-end t)
        (unless (string-match-p (rx bos (+ digit) "@almato" eos) (match-string-no-properties 1))
          (push (list (match-beginning 1) (match-end 1) :warning "version is N@almato")
                diags)))
      diags)))

(defun ki-studio-lint-own-ki (_occs)
  "What a new or changed KI must not carry (clean-up 1)."
  (save-excursion
    (goto-char (point-min))
    (let (diags section)
      (while (not (eobp))
        (cond
         ((looking-at ki--section-line-re)
          (setq section (match-string-no-properties 1)))
         ((looking-at (rx (* blank) "#" (* nonl)
                          (group (or (seq (or "Sitzung" "Session" "session")
                                          (+ blank) (+ digit))
                                     (seq (+ digit) "_" (* blank) "[")))))
          (push (list (match-beginning 1) (match-end 1) :note
                      "Comment refers to a session; the edge carries that (clean-up 1)")
                diags))
         ((and (equal section "do")
               (looking-at (rx (* blank) (group "log" (? "." (+ alpha))) "(")))
          (push (list (match-beginning 1) (match-end 1) :note
                      "No log() in a rule (clean-up 1)")
                diags))
         ((and (equal section "do")
               (looking-at (rx (* blank) (group "if") symbol-end)))
          (push (list (match-beginning 1) (match-end 1) :note
                      "One decision per KI, no `if' in the body (clean-up 1)")
                diags)))
        (forward-line 1))
      diags)))


;;; New KI

(defun ki-studio--folders ()
  "Folders under ki/, relative, sorted."
  (let ((ki-dir (ki-studio--ki-dir)))
    (sort (mapcar (lambda (dir) (file-relative-name dir ki-dir))
                  (seq-filter #'file-directory-p
                              (directory-files-recursively ki-dir "" t)))
          #'string<)))

(defun ki-studio-new-ki (folder name)
  "Create the KI NAME in FOLDER under ki/, from the skeleton.
The `when' waits on the folder's agenda ToDo, which is also the key of
the justification line."
  (interactive
   (list (completing-read "Folder: " (ki-studio--folders) nil t)
         (read-string "Name: " "DLR-Grade-")))
  (let* ((ki-dir (ki-studio--ki-dir))
         (file (expand-file-name (concat name ".ki") (expand-file-name folder ki-dir)))
         (todo (cdr (assoc (car (split-string folder "/")) (ki-studio--todo-map)))))
    (when (file-exists-p file)
      (user-error "%s exists" (file-relative-name file ki-dir)))
    (find-file file)
    (insert "ki\n"
            "  name: \"" name "\"\n"
            "  version: \"1@almato\"\n"
            "  description: \"\"\n"
            "  # Was die Regel prueft und warum dieser Zuschnitt.\n"
            "on\n"
            "  ogit/_id as AVID\n"
            "  ogit/_type == \"ogit/HR/WorkUnit\"\n"
            "  ogit/HR/timeShare as ZEITANTEIL\n"
            "  avFakt =~ /\\|(?<BELEG>)\\|/\n"
            "when\n"
            "  ogit/subject == \"GradePosition\"\n"
            "  ProcessIssue\n"
            (if todo (format "  %s@any\n" todo) "")
            "do\n"
            "  add(begruendung[\"" (or todo "ToDo") "\"] = \"Satz |  | merkmal | erfuellt | ${AVID} | ${ZEITANTEIL} | ${BELEG} | \")\n")
    (goto-char (point-min))
    (search-forward "description: \"")))


;;; Mode

(defvar-keymap ki-studio-mode-map
  :doc "Keymap for `ki-studio-mode'."
  "C-c C-n" #'ki-studio-new-ki)

;;;###autoload
(define-minor-mode ki-studio-mode
  "DLR conventions for KI files in a dlr_core checkout."
  :lighter " Studio"
  (if ki-studio-mode
      (progn
        (dolist (fn '(ki-studio-lint-lines ki-studio-lint-header
                      ki-studio-lint-own-ki))
          (add-hook 'ki-lint-functions fn t t))
        (add-hook 'eldoc-documentation-functions #'ki-studio-eldoc nil t))
    (dolist (fn '(ki-studio-lint-lines ki-studio-lint-header
                  ki-studio-lint-own-ki))
      (remove-hook 'ki-lint-functions fn t))
    (remove-hook 'eldoc-documentation-functions #'ki-studio-eldoc t)))

;;;###autoload
(defun ki-studio-maybe-enable ()
  "Turn on `ki-studio-mode' when the file is in a dlr_core checkout."
  (when (ki-studio--root)
    (ki-studio-mode 1)))

(provide 'ki-studio)
;;; ki-studio.el ends here

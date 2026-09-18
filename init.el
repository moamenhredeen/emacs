;; ***********************************************************************
;; ***
;; *** My Personal Emacs Configuration
;; ***
;;
;;  ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;;  ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;  █████╗  ██╔████╔██║███████║██║     ███████╗
;;  ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;  ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;  ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝


;; ***********************************************************************
;; ***
;; *** Utility Functions
;; ***


(defun my/smart-find-file ()
        (interactive)
        (if (projectile-project-p)
            (consult-projectile-find-file)
        (ido-find-file)))

(defun my/smart-find-buffer ()
    (interactive)
    (if (projectile-project-p)
        (consult-project-buffer)
        (consult-buffer)))


(defun my/home-directory ()
    "os independent home directory.
this function return home environment variable on linux
and USERPROFILE environment variable on windows."
  (interactive)
  (if (or (eq system-type 'windows-nt) (eq system-type 'ms-dos))
      (getenv "USERPROFILE")
    (getenv "HOME")))


(defun my/disable-tabs ()
  (setq indent-tabs-mode nil))

(defun my/enable-tabs  ()
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

; Source - https://stackoverflow.com/a/2417617
; Posted by scottfrazer
; Retrieved 2026-09-14, License - CC BY-SA 2.5

(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))



;; ***********************************************************************
;; ***
;; *** better defaults
;; ***


;; variables
(setq
	my/notes-directory (file-name-concat (expand-file-name (my/home-directory))  "notes")
	default-directory "C:/Users/mhraden/"
	explicit-shell-file-name "pwsh.exe")

(setq-default evil-shift-width custom-tab-width)
(setq compilation-window-height 15
      compilation-scroll-output t
      dired-dwim-target t
	  tab-bar-show nil
      ;; do not show documentation in the minibuffer
      eldoc-echo-area-use-multiline-p nil
      backup-directory-alist '(("." . "~/.emacsbackups"))
      auto-save-file-name-transforms '((".*" "~/.emacsbackups/" t))
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" "~/.emacsbackups/")
      create-lockfiles nil)


;; modes
(pixel-scroll-precision-mode)
(which-key-mode)
(setq global-hl-line-mode nil)

;; functions
(prefer-coding-system 'utf-8)

;; alists
(add-to-list 'default-frame-alist '(font . "Iosevka NF"))

;; (add-to-list 'default-frame-alist '(undecorated . t))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))


;; hooks
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
;; (add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; tabs hooks
(add-hook 'lisp-mode-hook 'my/disable-tabs)
(add-hook 'prog-mode-hook 'my/enable-tabs)
(add-hook 'emacs-lisp-mode-hook 'my/enable-tabs)

;; disable line nubmer hooks
;; (global-display-line-numbers-mode 0)
;; (defun my/disable-line-numbers ()
;;   (display-line-numbers-mode 0))
;; (add-hook 'shell-mode-hook 'my/disable-line-numbers)
;; (add-hook 'org-mode-hook 'my/disable-line-numbers)
;; (add-hook 'treemacs-mode-hook 'my/disable-line-numbers)
;; (add-hook 'eshell-mode-hook 'my/disable-line-numbers)



;; packagess
(use-package emacs
  :init
  (setq completion-cycle-threshold 3
        tab-always-indent 'complete
        enable-recursive-minibuffers t
        minibuffer-prompt-properties
		'(read-only t cursor-intangible t face minibuffer-prompt)))


;; ***********************************************************************
;; ***
;; *** package repositories
;; ***

;; melpa (elpa is the default and is initialized automatically before init.el)
(use-package package
  :config
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t))

;; Windows ships no gpg, and the one on PATH from git-for-windows is an MSYS
;; build that reads "c:/..." as a relative path -- it looks for the keyring
;; under <cwd>/c:/... and signature verification fails with "No public key".
;; Point epg at the native GnuPG build instead, which understands drive letters.
(when (eq system-type 'windows-nt)
  (let ((gpg "C:/Program Files/GnuPG/bin/gpg.exe"))
    (when (file-executable-p gpg)
      (setq epg-gpg-program gpg))))

;; Keeps the GNU ELPA signing keys current when they are rotated.
(use-package gnu-elpa-keyring-update
  :ensure t)


;; ***********************************************************************
;; ***
;; *** Org mode
;; ***

(use-package org
  :config
  (setq org-src-fontify-natively t
        org-hide-emphasis-markers t
        org-src-window-setup 'current-window
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        ;; org-hide-leading-stars t
        org-hide-block-startup t
        org-startup-folded t
		org-default-notes-file (file-name-concat my/notes-directory "inbox.org")
		org-agenda-files (list (file-name-concat my/notes-directory  "20260630T103000--inbox__plan.org"))
        ;; org-ellipsis " ─╮"
        org-todo-keywords '((sequence "TODO(t)" "DOING(o)" "WAITING(w)" "BLOCKED(b)"
                                      "|"
                                      "DONE(d)" "CANCELED(c)"))
        org-todo-keyword-faces '(("TODO"     . (:foreground "#ff6e6e" :weight bold))
                                 ("DOING"    . (:foreground "#ffea73" :weight bold))
                                 ("WAITING"  . (:foreground "#fabd2f" :weight bold))
                                 ("BLOCKED"  . (:foreground "#fb4934" :weight bold))
                                 ("DONE"     . (:foreground "#98971a" :weight bold))
                                 ("CANCELED" . (:foreground "#ebdbb2" :weight bold))))
  ;; variable-sized headlines
  ;; (dolist (face '((org-level-1 . 1.4)
  ;;                 (org-level-2 . 1.25)
  ;;                 (org-level-3 . 1.15)
  ;;                 (org-level-4 . 1.1)
  ;;                 (org-level-5 . 1.05)
  ;;                 (org-level-6 . 1.0)
  ;;                 (org-level-7 . 1.0)
  ;;                 (org-level-8 . 1.0)))
  ;; (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))
  (set-face-attribute 'org-document-title nil :height 1.6 :weight 'bold))

(use-package ox-latex
  :custom
  (org-latex-listings t))

;; markdown export backend (built into org), enables C-c C-e m
(use-package ox-md
  :after org)

;; expose git-bash unix tools (xargs, etc.) to emacs subprocesses on windows
(when (eq system-type 'windows-nt)
  (let ((git-usr-bin "C:/Program Files/Git/usr/bin"))
    (when (file-directory-p git-usr-bin)
      (add-to-list 'exec-path git-usr-bin t)
      (setenv "PATH" (concat (getenv "PATH") path-separator
                             (replace-regexp-in-string "/" "\\\\" git-usr-bin))))))

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :custom
  (denote-directory my/notes-directory)
  ;; The vocabulary already in the vault, so completion offers it.  Everything
  ;; else is still picked up because `denote-infer-keywords' is on.
  (denote-known-keywords
   '("fsp" "bycs" "ticket" "todo" "doing" "done" "review" "draft"
     "reference" "bug" "backend" "java" "springsecurity" "flutter"
     "riverpod" "webcomponents" "css" "aria" "technicalmigration"
     "capture" "journal"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  ;; NOTE: `md' is not a valid value -- it silently fell back to Org.  The
  ;; vault is Markdown with YAML front matter, which is also what Obsidian
  ;; reads, so notes stay readable from both sides.
  (denote-file-type 'markdown-yaml)
  ;; `signature' holds the ticket number for the notes migrated from the
  ;; Obsidian vault (e.g. 20260909T141121==254--bestandskontrolle__...).
  ;; It is not prompted for by default; use `denote-signature' to set one,
  ;; or `C-u C-u M-x denote' for the full prompt set.
  (denote-prompts '(title keywords))
  (denote-date-prompt-use-org-read-date t)
  (denote-backlinks-show-context t)
  ;; Obsidian leftovers that are not notes.
  (denote-excluded-directories-regexp "\\`\\(\\.obsidian\\|attachments\\|Excalidraw\\|pdfs\\)\\'")
  :config
  (unless (file-directory-p denote-directory)
    (make-directory denote-directory t))
  ;; Denote adds itself to `markdown-follow-link-functions', so C-c C-o on a
  ;; [Title](denote:IDENTIFIER) link jumps to the note.
  (require 'markdown-mode nil t)
  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :ensure t
  :after (consult denote)
  :config
  (consult-denote-mode 1))

(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :custom
    (org-journal-dir (file-name-concat my/notes-directory "journal"))
    (org-journal-agenda-integration t)
    (org-journal-file-format "%Y-%m.org")
    (org-journal-file-type 'monthly)
    :config
    ;; TODO: do i need this ??
    (setq org-agenda-file-regexp "\\`\\\([^.].*\\.org\\\|[0-9]\\\{8\\\}\\\(\\.gpg\\\)?\\\)\\'")
    (add-to-list 'org-agenda-files org-journal-dir))


;; ***********************************************************************
;; ***
;; *** Utility Packages
;; ***

;; built-in: auto-pair brackets/quotes and highlight matching parens
(electric-pair-mode 1)
(show-paren-mode 1)

;; TODO: is there better alternatives
(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode +1))


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))


(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))


(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode))


;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode))


(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  :init
  (global-corfu-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-projectile
  :ensure t)

(use-package restclient
  :ensure t)

;; ***********************************************************************
;; ***
;; *** Git configuration
;; ***


(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (custom-set-variables
   '(git-gutter:modified-sign " ")
   '(git-gutter:added-sign " ")
   '(git-gutter:deleted-sign " "))
  (set-face-background 'git-gutter:modified "#83a598") ;; background color
  (set-face-background 'git-gutter:added "#b8bb26")
  (set-face-background 'git-gutter:deleted "#fb4934")
  (global-git-gutter-mode 1))



;; ***********************************************************************
;; ***
;; *** Programming
;; ***

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :hook ((markdown-mode . visual-line-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-hide-urls t)
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do))
  )

;; TypeScript / TSX (Lit web components)
(use-package typescript-ts-mode
  :mode (("\\.ts\\'"  . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :custom
  (typescript-ts-mode-indent-offset 2))

;; lit-html template literal highlighting (optional but nice for Lit)
(use-package mhtml-mode
  :defer t)

;; project.el drives eglot's notion of "the workspace root". By default it
;; only recognises VCS markers, which sometimes picks the wrong ancestor for
;; Java sources buried deep under src/main/java. Teach it that pom.xml marks
;; a project root too, so eglot hands bromo a Maven project (not ~/).
(defun my/maven-project-finder (dir)
  "Detect a Maven project root by walking up DIR for pom.xml."
  (when-let ((root (locate-dominating-file dir "pom.xml")))
    (cons 'transient (expand-file-name root))))

(with-eval-after-load 'project
  (add-hook 'project-find-functions #'my/maven-project-finder))

(use-package eglot
  :ensure t
  :hook ((go-ts-mode
          rust-mode
          typescript-ts-mode
          tsx-ts-mode
          powershell-mode
          nxml-mode
          js-ts-mode
          go-ts-mode
          dart-mode
          latex-mode
          java-mode
          java-ts-mode)
		 . eglot-ensure)
  :config
  ;; bromo — the in-development Java LSP.
  ;; Build:  cd ~/git-repos/bromo && ./mvnw -DskipTests package
  ;; Reload after rebuild: M-x eglot-reconnect
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode)
                 . ("C:/Users/mhraden/.jdks/temurin-25.0.3/bin/java.exe"
                    "--enable-preview" "-XX:+UseZGC"
                    "-jar" "C:/Users/mhraden/git-repos/bromo/target/bromo.jar"
                    "--stdio"))))


;; ***********************************************************************
;; ***
;; *** Key Mapping
;; ***

(use-package evil
  :ensure t
  :init
  (setq
   evil-undo-system 'undo-redo
   evil-want-integration t
   evil-want-keybinding nil
   ;; evil-insert-state-cursor '(box "#d600b6")
   ;; evil-normal-state-cursor '(box "black")
   )
  :config
  (evil-mode 1))


(use-package evil-collection
  :ensure t
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-multiedit
  :ensure t
  :after evil
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-nerd-commenter
  :after evil
  :ensure t)

(use-package evil-org
  :ensure t
  :hook (evil-org . org-mode)
  :after evil
  :after org
  :config
  ;; (add-hook 'org-mode-hook 'evil-org-mode)
  ;; (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


;; cleaner way for defining keymap
(use-package general
  :ensure t
  :after evil
  :config
  (general-define-key
   ;; navigation
   "M-k"    'evil-window-up
   "M-j"    'evil-window-down
   "M-h"    'evil-window-left
   "M-l"    'evil-window-right

   "M-s"    'split-window-below
   "M-v"    'split-window-right

   "M-q"    'evil-quit
   "M-o"    'tab-switch
   "M-n"    'evil-multiedit-match-and-next
   "M-N"    'evil-multiedit-match-all)


  (general-define-key
   :states		'(normal visual)
    "g c"		'evilnc-comment-or-uncomment-lines
    "g i"		'eglot-find-implementation)

  (general-define-key
   :states		'normal
   :keymaps		'emacs-lisp-mode-map
   "K"			'describe-function)

  (general-create-definer my-leader-def
    :prefix		"SPC")

  ;; Global Keybindings
  (my-leader-def
    :states   '(normal visual)
    :keymaps  'override

    ;; most used commands
    "w"       'save-buffer
    "f"       'find-file
    "b"       'consult-buffer
    "q"       'delete-window
    "k"       'magit-status
    "x"       'execute-extended-command
    "h"       'consult-apropos
    "d"       'docker
    "t"       'eshell
    "c"       'my/open-init-file
    "e"       'dired-jump

    ;; eglot
    "a"     'eglot-code-actions
    "rr"     'eglot-rename
    "rf"     'eglot-format


    ;; search
    "sb"    'consult-line
    "sg"    'consult-ripgrep
    "so"    'consult-outline
    "si"    'consult-imenu
    "sr"    'consult-register
    "sm"    'consult-mode-command
    "sl"    'consult-goto-line
    "sw"    'occur

    ;; project key biding
    "ps"      'project-switch-project
    "pf"      'project-find-file
    "pb"      'project-buffer
    "pe"      'projec-dired

    ;; org key binding
    "oa"        'org-agenda
    "oc"        'org-capture
	"oi"		(lambda () (interactive) (find-file (file-name-concat my/notes-directory "inbox.org")))

    ;; denote key binding
    "nf"        'denote-open-or-create
    "nn"        'denote
    "nN"        'denote-type
    "nd"        'denote-date
    "nt"        'denote-template
    "nz"        'denote-signature
    "ns"        'consult-denote-grep
    "ni"        'denote-link
    "nI"        'denote-add-links
    "nb"        'denote-backlinks
    "nr"        'denote-rename-file
    "nk"        'denote-rename-file-keywords
    "np"        'my/pdf-section-link-insert

    ;; org journal key binding
    "jn"      'org-journal-next-entry
    "jp"      'org-journal-previous-entry
    "ji"      'org-journal-new-entry
    "js"      'org-journal-search)

  ;; Org Mode Keybindings
  (general-define-key
   :keymaps  '(org-tree-slide-mode-map override)
   :states   '(normal insert)
   "<right>"     'org-tree-slide-move-next-tree
   "<left>"     'org-tree-slide-move-previous-tree)

  ;; Org Mode evil Keybindings
  ;; org mode local leader
  (general-create-definer my-local-leader-def
    :prefix   "SPC .")

  (my-local-leader-def
    :states   'normal
    :keymaps  'org-mode-map
    "h"      'consult-org-heading
    "l"       'org-insert-link
    "t"       'org-set-tags-command
    "p"       'org-set-property-and-value))


(global-set-key (kbd "<escape>") 'keyboard-escape-quit)



;; ***********************************************************************
;; ***
;; *** Theme and UI Customizations
;; ***
(use-package modus-themes
  :ensure t
  :init
  ;; rainbow = distinct accent color per heading level (markdown + org).
  ;; Must be set before load-theme. Numbers scale heading height.
  (setq modus-themes-headings
        '((1 . (rainbow bold 1.4))
          (2 . (rainbow bold 1.25))
          (3 . (rainbow bold 1.15))
          (4 . (rainbow bold 1.1))
          (5 . (rainbow bold 1.05))
          (6 . (rainbow bold 1.0))))
  :config
  (load-theme 'modus-vivendi-tritanopia t))

;; ***********************************************************************
;; ***
;; *** PDF viewing -- pdf-tools
;; ***
;; The epdfinfo server comes from MSYS2:
;;   pacman -S mingw-w64-x86_64-emacs-pdf-tools-server
;; C:\msys64\mingw64\bin is appended to the user PATH so Windows can resolve
;; both epdfinfo.exe and the poppler/cairo/glib DLLs it links against.
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :custom
  ;; Render at the frame's real pixel density; without this pages look blurry
  ;; on HiDPI displays.
  (pdf-view-use-scaling t)
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations t)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  ;; Do not rebuild the server: the MSYS2 package supplies it.
  (setq pdf-info-epdfinfo-program
        (or (executable-find "epdfinfo")
            "C:/msys64/mingw64/bin/epdfinfo.exe"))
  (pdf-tools-install :no-query)
  ;; `pixel-scroll-precision-mode' fights with pdf-view's own scrolling.
  (add-hook 'pdf-view-mode-hook
            (lambda () (setq-local pixel-scroll-precision-mode nil))))

;; Annotations.  `pdf-annot' ships with pdf-tools; it is autoloaded by
;; `pdf-tools-install', so this block only carries settings and bindings.
;;
;;   C-c C-a h   highlight the active region
;;   C-c C-a u   underline          C-c C-a s   squiggly
;;   C-c C-a o   strike out         C-c C-a t   free-form text box
;;   C-c C-a m   markup menu        C-c C-a l   list all annotations
;;   C-c C-a D   delete annotation under point
;;   C-c C-a a   dired of the file attachments
;;
;; In the annotation list (`pdf-annot-list-annotations'): RET jumps to the
;; annotation, d marks for deletion, x executes.  Edits are written back into
;; the PDF itself on save (C-x C-s).
(use-package pdf-annot
  :after pdf-tools
  :custom
  ;; Open the edit popup right after creating an annotation.
  (pdf-annot-activate-created-annotations t)
  ;; Show the annotation list in a side window instead of splitting.
  (pdf-annot-list-display-buffer-action
   '((display-buffer-in-side-window)
     (side . left)
     (window-width . 0.35)
     (inhibit-same-window . t)))
  :config
  ;; Stamp annotations with a real name; `user-full-name' is empty on this
  ;; machine, so fall back to the login name.
  (setq pdf-annot-default-annotation-properties
        `((t (label . ,(if (string-empty-p (or user-full-name ""))
                           user-login-name
                         user-full-name)))
          (text  (icon . "Note") (color . "#ff9500"))
          (highlight (color . "#ffe066"))
          (underline (color . "#4ea1ff"))
          (squiggly  (color . "#c586c0"))
          (strike-out (color . "#ff6e6e")))))

;; org links to PDF pages, regions and annotations.
;; `org-store-link' (C-c l) in a pdf-view buffer yields a link like
;;   [[pdf:/path/to/file.pdf::12++0.53;;annot-12-3][Some text]]
;; which reopens the exact page, scroll position and annotation.
(use-package org-pdftools
  :ensure t
  :after (org pdf-tools)
  :hook (org-mode . org-pdftools-setup-link)
  :custom
  ;; Put the highlight text in the link description automatically.
  (org-pdftools-use-isearch-link t)
  (org-pdftools-use-freepointer-annot t)
  ;; Where exported/extracted annotation images land.
  (org-pdftools-export-style 'pdftools))

;; ***********************************************************************
;; ***
;; *** Markdown -> PDF section links
;; ***
;; The vault is Markdown, so the Org `pdf:' links above are unreachable from
;; a note.  These commands produce plain Markdown links carrying both a page
;; and a section:
;;
;;   [Authentication](pdfs/spring-security.pdf#page=42&section=Authentication)
;;
;; The section is resolved against the PDF's embedded outline (the same tree
;; the `o' key shows in pdf-view), so a new edition of the document with
;; different pagination still lands on the right heading.  `page' is the
;; fallback for documents without an outline, or headings that were reworded.
;;
;; A link may also name an annotation, which is the precise form: select
;; text in the PDF, press C-c h, and the region is highlighted and linked in
;; one step, described by the text it covers:
;;
;;   [tokens are opaque to the client](pdfs/oauth.pdf#page=42&section=Tokens&annot=annot-42-3&text=tokens%20are%20opaque...)
;;
;; Each target is a fallback for the one before it -- annotation, then
;; section, then page -- so a link degrades instead of breaking.
;;
;;   C-c d       (pdf-view)  copy a link to the section around the current page
;;   C-c h       (pdf-view)  highlight the region and copy a link to it,
;;                           or pick an existing annotation on this page
;;                           (C-u C-c h to click the one you want)
;;   SPC n p     (anywhere)  pick a section from a PDF's outline and insert it
(defun my/pdf-info-release (file)
  "Have epdfinfo drop its open handle on FILE, unless a buffer visits it.
The server keeps one handle per document it has been asked about, and on
Windows that handle blocks renaming or deleting the file from Dired or
Explorer.  A pdf-view buffer closes its own document from
`kill-buffer-hook'; a document opened only to answer a query has nobody
to close it, so close it here."
  (when (and file (not (find-buffer-visiting file)))
    (ignore-errors (pdf-info-close file))))

(defun my/pdf-outline-entries (&optional file)
  "Outline entries of FILE that point somewhere inside the document.
Discards the `uri' and `goto-remote' entries, and the `goto-dest' ones
whose page is 0, which Poppler uses for an unspecified destination."
  (unwind-protect
      (seq-filter (lambda (entry)
                    (and (eq (alist-get 'type entry) 'goto-dest)
                         (> (alist-get 'page entry) 0)))
                  (pdf-info-outline file))
    (my/pdf-info-release file)))

(defun my/pdf-enclosing-section (&optional file page)
  "Return the outline entry of FILE that PAGE falls under.
The outline is in document order, so the last entry starting at or
before PAGE is the most specific heading preceding it."
  (let ((page (or page (pdf-view-current-page)))
        best)
    (dolist (entry (my/pdf-outline-entries file) best)
      (when (<= (alist-get 'page entry) page)
        (setq best entry)))))

(defconst my/pdf-annot-fingerprint-length 60
  "How much annotation text to embed in a link.
Used to recover the annotation when its id has drifted; see
`my/pdf-annot-resolve'.")

(defun my/pdf-annot-markup-p (annot)
  "Return non-nil if ANNOT marks up a region of text."
  (memq (pdf-annot-get-type annot) '(highlight underline squiggly strike-out)))

(defun my/pdf-annot-text (annot)
  "Return the document text that ANNOT marks up, as a single line."
  (let ((page (pdf-annot-get annot 'page)))
    (string-trim
     (replace-regexp-in-string
      "[ \t\n\r]+" " "
      (mapconcat (lambda (edges)
                   (pdf-info-gettext page edges pdf-view-selection-style))
                 (pdf-annot-get-display-edges annot)
                 " ")))))

(defun my/pdf-annot-label (annot)
  "Return a human label for ANNOT.
For a markup annotation that is the text it covers; for a sticky note
it is the note's own contents."
  (let ((marked (if (my/pdf-annot-markup-p annot) (my/pdf-annot-text annot) ""))
        (contents (string-trim (or (pdf-annot-get annot 'contents) ""))))
    (cond ((not (string-empty-p marked)) marked)
          ((not (string-empty-p contents)) contents)
          (t (format "annotation on p.%d" (pdf-annot-get annot 'page))))))

(defun my/pdf-annot-fingerprint (annot)
  "Return ANNOT's label, truncated to a length worth putting in a link."
  (truncate-string-to-width (my/pdf-annot-label annot)
                            my/pdf-annot-fingerprint-length nil nil t))

(defun my/pdf-link-description (string)
  "Make STRING safe to use as a Markdown link description."
  (replace-regexp-in-string "[][]" "" string))

(defun my/pdf-section-link (file page &optional title annot)
  "Format a Markdown link to PAGE of FILE.
TITLE, when given, names the enclosing outline section.  ANNOT, when
given, is an annotation object: the link then points at it, is described
by the text it covers, and keeps TITLE and PAGE as fallbacks."
  (let ((fingerprint (and annot (my/pdf-annot-fingerprint annot))))
    (format "[%s](%s#page=%d%s%s)"
            (my/pdf-link-description
             (or fingerprint title (format "p.%d" page)))
            (file-relative-name file (denote-directory))
            page
            ;; Titles and annotation text routinely contain spaces, and
            ;; occasionally the `&' and `#' that would truncate the fragment.
            (if title (concat "&section=" (url-hexify-string title)) "")
            (if annot
                (format "&annot=%s&text=%s"
                        (url-hexify-string
                         (symbol-name (pdf-annot-get-id annot)))
                        (url-hexify-string fingerprint))
              ""))))

(defun my/pdf-section-link-store ()
  "Copy a Markdown link to the section enclosing the current page."
  (interactive)
  (pdf-util-assert-pdf-buffer)
  (let* ((file (buffer-file-name))
         (page (pdf-view-current-page))
         (link (my/pdf-section-link
                file page
                (alist-get 'title (my/pdf-enclosing-section file page)))))
    (kill-new link)
    (message "%s" link)))

(defun my/pdf-annot-read-on-page ()
  "Choose one of the annotations on the current page."
  (let* ((annots (pdf-annot-getannots (pdf-view-current-page)))
         (table (mapcar (lambda (a)
                          (cons (format "%s: %s"
                                        (pdf-annot-get-type a)
                                        (my/pdf-annot-label a))
                                a))
                        annots)))
    (unless annots
      (user-error "No annotations on page %d" (pdf-view-current-page)))
    (cdr (assoc (completing-read "Annotation: " table nil t) table))))

(defun my/pdf-annot-link-store (&optional click)
  "Copy a Markdown link to a highlight in the current PDF.

With an active region, highlight it first and link the new annotation.
Otherwise pick an existing annotation from the current page, or, with a
prefix argument CLICK, by clicking it."
  (interactive "P")
  (pdf-util-assert-pdf-buffer)
  (let* ((annot (cond
                 ;; `pdf-view-active-region' returns the (PAGE . EDGES) cons
                 ;; that the markup constructors expect; t deactivates it.
                 ((pdf-view-active-region-p)
                  (pdf-annot-add-highlight-markup-annotation
                   (pdf-view-active-region t)))
                 (click (pdf-annot-read-annotation
                         "Click the annotation to link: "))
                 (t (my/pdf-annot-read-on-page))))
         (file (buffer-file-name))
         (page (pdf-annot-get annot 'page))
         (link (my/pdf-section-link
                file page
                (alist-get 'title (my/pdf-enclosing-section file page))
                annot)))
    (kill-new link)
    (message "%s" link)))

(defun my/pdf-annot-resolve (id page text)
  "Return the annotation ID denotes in the current buffer.

Annotation ids are positional -- epdfinfo derives them from the
annotation's index on its page -- so an id can come to mean a different
annotation once one is added or removed before it.  Accept the id only
if it still resolves to something on PAGE, and otherwise fall back to
the annotation on PAGE whose text matches TEXT."
  (let ((by-id (ignore-errors (pdf-annot-getannot id))))
    (or (and by-id
             (or (null page) (= (pdf-annot-get by-id 'page) page))
             by-id)
        (and text page
             (seq-find (lambda (a)
                         (string= (my/pdf-annot-fingerprint a) text))
                       (pdf-annot-getannots page))))))

(defun my/pdf-section-link-insert (file)
  "Insert a Markdown link to a section of FILE, chosen from its outline.
FILE need not be open: `pdf-info-outline' queries epdfinfo by name."
  (interactive (list (read-file-name "PDF: " (denote-directory) nil t
                                     nil (lambda (f)
                                           (or (file-directory-p f)
                                               (string-suffix-p ".pdf" f t))))))
  (let* ((entries (my/pdf-outline-entries file))
         (_ (unless entries
              (user-error "%s has no outline" (file-name-nondirectory file))))
         (table (mapcar
                 (lambda (entry)
                   (cons (format "%s%s (p.%d)"
                                 (make-string (* 2 (1- (alist-get 'depth entry))) ?\s)
                                 (alist-get 'title entry)
                                 (alist-get 'page entry))
                         entry))
                 entries))
         (collection
          (lambda (string predicate action)
            (if (eq action 'metadata)
                ;; Document order, not alphabetical: the indentation above is
                ;; meaningless once the entries are sorted.
                '(metadata (display-sort-function . identity))
              (complete-with-action action table string predicate))))
         (entry (cdr (assoc (completing-read "Section: " collection nil t) table))))
    (insert (my/pdf-section-link file
                                 (alist-get 'page entry)
                                 (alist-get 'title entry)))))

(defun my/markdown-follow-pdf (url)
  "Follow FILE.pdf#page=N&section=TITLE links.
Returns non-nil when it handled URL, which stops
`markdown-follow-link-functions' from trying the remaining handlers."
  (when (and (not (string-match-p "\\`[a-z][a-z0-9+.-]*:" url)) ; leave http:, file: etc. alone
             (string-match "\\`\\([^#]+\\.pdf\\)#\\(.*\\)\\'" url))
    (let* ((name (match-string 1 url))
           (fragment (match-string 2 url))
           (page (and (string-match "page=\\([0-9]+\\)" fragment)
                      (string-to-number (match-string 1 fragment))))
           (section (and (string-match "section=\\([^&]+\\)" fragment)
                         (url-unhex-string (match-string 1 fragment))))
           (annot-id (and (string-match "annot=\\([^&]+\\)" fragment)
                          (intern (url-unhex-string (match-string 1 fragment)))))
           (annot-text (and (string-match "text=\\([^&]+\\)" fragment)
                            (url-unhex-string (match-string 1 fragment))))
           ;; Links are written relative to the notes directory, but resolve
           ;; against the note's own directory first so they also work in a
           ;; Markdown file living outside the vault.
           (file (seq-find #'file-exists-p
                           (list (expand-file-name name default-directory)
                                 (expand-file-name name (denote-directory))))))
      (unless file
        (user-error "No such PDF: %s" name))
      (pop-to-buffer (find-file-noselect file))
      (when (derived-mode-p 'pdf-view-mode)
        (let ((annot (when annot-id
                       (my/pdf-annot-resolve annot-id page annot-text)))
              (entry (when section
                       (seq-find (lambda (e)
                                   (string= (alist-get 'title e) section))
                                 (my/pdf-outline-entries)))))
          (cond
           ;; Most specific target first.  `pdf-annot-show-annotation' turns
           ;; to the annotation's page, scrolls it into view and, with a
           ;; non-nil second argument, flashes it apart from its neighbours.
           (annot (pdf-annot-show-annotation annot t (selected-window)))
           ;; `pdf-links-action-perform' takes an outline entry as-is: it
           ;; turns the page, scrolls to the heading's vertical position and
           ;; flashes an arrow at it.  Same code path as RET in the outline.
           (entry (pdf-links-action-perform entry))
           (page (pdf-view-goto-page page)))
          (when (and annot-id (not annot) page)
            (message "Annotation gone; fell back to %s"
                     (if entry (format "section %S" section)
                       (format "page %d" page))))
          (when (and section (not entry) (not annot) page)
            (message "Section %S not in outline; used page %d"
                     section page))))
      t)))

(with-eval-after-load 'markdown-mode
  ;; Denote puts `denote-link-markdown-follow' on this hook.  That one matches
  ;; an identifier anywhere in the URL, so it must not see these links first;
  ;; a negative depth keeps this handler in front of it.
  (add-hook 'markdown-follow-link-functions #'my/markdown-follow-pdf -100))

(with-eval-after-load 'pdf-view
  ;; `C-c C-a' is taken by pdf-annot.
  (define-key pdf-view-mode-map (kbd "C-c d") #'my/pdf-section-link-store)
  (define-key pdf-view-mode-map (kbd "C-c h") #'my/pdf-annot-link-store))


;; ***********************************************************************
;; ***
;; *** Auto Generated
;; ***
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("488b82a8d9ace0aea8a6825db144e3c65c4f1ef3e090b618bf311d9cdb513322"
	 default))
 '(git-gutter:added-sign " ")
 '(git-gutter:deleted-sign " ")
 '(git-gutter:modified-sign " ")
 '(package-selected-packages
   '(acp consult-denote consult-projectile corfu dap-mode dart-mode
		 dired-subtree emacsql embark-consult evil-collection
		 evil-multiedit evil-nerd-commenter evil-org evil-surround
		 general git-gutter gnu-elpa-keyring-update hackernews
		 kdl-mode kotlin-mode log4j-mode lua-mode magit marginalia
		 markdown-mode modus-themes olivetti orderless org-journal
		 org-modern org-pdftools plantuml-mode powershell restclient
		 shell-maker spinner treemacs vertico web-mode yaml)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

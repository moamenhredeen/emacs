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
(global-display-line-numbers-mode 1)
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
        org-hide-block-startup t
        org-startup-folded t
		org-default-notes-file (file-name-concat my/notes-directory "inbox.org")
		org-agenda-files (list (file-name-concat my/notes-directory  "20260630T103000--inbox__plan.org"))
        org-todo-keywords '((sequence "TODO(t)" "DOING(o)" "WAITING(w)" "BLOCKED(b)"
                                      "|"
                                      "DONE(d)" "CANCELED(c)"))
        org-todo-keyword-faces '(("TODO"     . (:foreground "#ff6e6e" :weight bold))
                                 ("DOING"    . (:foreground "#ffea73" :weight bold))
                                 ("WAITING"  . (:foreground "#fabd2f" :weight bold))
                                 ("BLOCKED"  . (:foreground "#fb4934" :weight bold))
                                 ("DONE"     . (:foreground "#98971a" :weight bold))
                                 ("CANCELED" . (:foreground "#ebdbb2" :weight bold))))
  ;; (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))
  (set-face-attribute 'org-document-title nil :height 1.6 :weight 'bold)
  ;; render more, show less markup
  (setq org-pretty-entities t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-startup-with-inline-images t
        org-image-actual-width '(600)))

;; bullets, block borders, tag/date pills, checkbox glyphs
;; (use-package org-modern
;;   :ensure t
;;   :after org
;;   :custom
;;   ;; (org-modern-table nil)                ; keep tables plain, they stay monospace
;;   :config
;;   (global-org-modern-mode))

;; ***********************************************************************
;; ***
;; *** Document view (org + markdown)
;; ***
;; Org and markdown buffers read like a rendered page: proportional body
;; text, centered column, no line numbers. The theme keeps code, tables
;; and metadata in fixed-pitch so they stay aligned.

;; (set-face-attribute 'variable-pitch nil :family "Segoe UI" :height 1.1)
;; (set-face-attribute 'fixed-pitch nil :family "Iosevka NF" :height 1.0)

(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 100))

(defun my/document-view ()
  "Make the current org/markdown buffer look like a rendered page."
  ;; (variable-pitch-mode 1)
  (visual-line-mode 1)
  (olivetti-mode 1)
  (display-line-numbers-mode 0)
  (setq-local line-spacing 0.25))

(add-hook 'org-mode-hook #'my/document-view)
(add-hook 'markdown-mode-hook #'my/document-view)

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
  :hook ((markdown-mode . markdown-display-inline-images))
  :custom
  (markdown-fontify-code-blocks-natively t)
  ;; header sizes come from the theme
  (markdown-header-scaling nil)
  (markdown-hide-urls t)
  (markdown-hide-markup t)
  (markdown-fontify-whole-heading-line t)
  (markdown-list-item-bullets '("•" "◦" "▪" "▫"))
  (markdown-max-image-size '(600 . 600))
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do))
  )

;; TypeScript / TSX (Lit web components)
(use-package typescript-ts-mode
  :mode (("\\.ts\\'"  . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :custom
  (typescript-ts-mode-indent-offset 2))

;; HIRO Knowledge Items (.ki); the mode lives in lisp/ki-mode.el, the DLR
;; conventions in lisp/ki-studio.el (on inside a dlr_core checkout).
;; my/enable-tabs (prog-mode-hook) turns on tabs and rebinds TAB to
;; tab-to-tab-stop; KI files use spaces and need TAB to indent.
(defun my/ki-mode-setup ()
  (setq indent-tabs-mode nil)
  (local-set-key (kbd "TAB") #'indent-for-tab-command)
  (flymake-mode 1))

(use-package ki-mode
  :load-path "lisp"
  :mode "\\.ki\\'"
  :hook (ki-mode . my/ki-mode-setup))

(use-package ki-studio
  :load-path "lisp"
  :hook (ki-mode . ki-studio-maybe-enable))

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

;; (use-package eglot
;;   :ensure t
;;   :hook ((go-ts-mode
;;           rust-mode
;;           typescript-ts-mode
;;           tsx-ts-mode
;;           powershell-mode
;;           nxml-mode
;;           js-ts-mode
;;           go-ts-mode
;;           dart-mode
;;           latex-mode
;;           java-mode
;;           java-ts-mode)
;; 		 . eglot-ensure)
;;   :config
;;   ;; bromo — the in-development Java LSP.
;;   ;; Build:  cd ~/git-repos/bromo && ./mvnw -DskipTests package
;;   ;; Reload after rebuild: M-x eglot-reconnect
;;   (add-to-list 'eglot-server-programs
;;                '((java-mode java-ts-mode)
;;                  . ("C:/Users/mhraden/.jdks/temurin-25.0.3/bin/java.exe"
;;                     "--enable-preview" "-XX:+UseZGC"
;;                     "-jar" "C:/Users/mhraden/git-repos/bromo/target/bromo.jar"
;;                     "--stdio"))))

;; these grammars came from a bundle without the libtree-sitter- prefix that
;; emacs looks for, so map the language to the actual dll name
(when (treesit-available-p)
  (dolist (lang '(elixir heex))
    (add-to-list 'treesit-load-name-override-list
                 (list lang (symbol-name lang) (format "tree_sitter_%s" lang)))))

;; language servers send large JSON payloads; the default read chunk is too small
(setq read-process-output-max (* 1024 1024))

;; fringes are disabled, so show flymake diagnostics in the margin instead
(setq flymake-indicator-type 'margins)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :mode (("\\.exs?\\'" . elixir-ts-mode)
         ("\\.heex\\'" . heex-ts-mode))
  :hook (((elixir-ts-mode
           heex-ts-mode
           go-ts-mode
           rust-mode
           typescript-ts-mode
           tsx-ts-mode
           js-ts-mode
           java-mode
           java-ts-mode
           powershell-mode
           nxml-mode
           latex-mode)
          . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . my/lsp-mode-setup-completion))
  :init
  ;; must be set before lsp-mode loads; the default s-l is Win+L on windows
  (setq lsp-keymap-prefix "C-c l")
  ;; let corfu + orderless filter lsp candidates
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :custom
  (lsp-completion-provider :none)        ; corfu is the completion UI
  (lsp-enable-snippet nil)               ; yasnippet is not installed
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil)
  (lsp-log-io nil)
  (lsp-elixir-local-server-command
   (expand-file-name "~/tools/elixir-ls-v0.31.1/language_server.bat"))
  :config
  ;; evil-collection has no lsp-mode module; mirror what its eglot module
  ;; bound, otherwise K falls back to man pages and gd to a text search
  (add-hook 'lsp-mode-hook #'evil-normalize-keymaps)
  (general-define-key
   :states		'normal
   :keymaps		'lsp-mode-map
   "gd"			'xref-find-definitions
   "gD"			'xref-find-definitions-other-window
   "g5"			'xref-find-definitions-other-frame
   "gr"			'xref-find-references
   "C-t"		'xref-go-back
   "K"			'lsp-describe-thing-at-point)

  ;; bromo — the in-development Java LSP.
  ;; Build:  cd ~/git-repos/bromo && ./mvnw -DskipTests package
  ;; Reload after rebuild: M-x lsp-workspace-restart
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     '("C:/Users/mhraden/.jdks/temurin-25.0.3/bin/java.exe"
                       "--enable-preview" "-XX:+UseZGC"
                       "-jar" "C:/Users/mhraden/git-repos/bromo/target/bromo.jar"
                       "--stdio"))
    :activation-fn (lsp-activate-on "java")
    :server-id 'bromo)))

;; ***********************************************************************
;; ***
;; *** AI agent (agent-shell + Claude Code)
;; ***
;; Talks to Claude Code over ACP through the claude-agent-acp adapter
;; (npm i -g @agentclientprotocol/claude-agent-acp). Auth is the Claude
;; subscription login of the local `claude' CLI, no API key needed.
;; If it ever asks for auth, run `claude' once in a terminal and /login.
(use-package agent-shell
  :ensure t
  :commands (agent-shell agent-shell-anthropic-start-claude-code)
  :config
  ;; always Claude, skip the agent picker
  (setq agent-shell-preferred-agent-config 'claude-code
        agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t)
        agent-shell-anthropic-claude-acp-command '("claude-agent-acp")))

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
   ;; state is shown in the mode line, no "-- INSERT --" echo
   evil-echo-state nil
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
    "g i"		'lsp-find-implementation)

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
    "i"       'agent-shell              ; Claude Code

    ;; lsp
    "a"     'lsp-execute-code-action
    "rr"     'lsp-rename
    "rf"     'lsp-format-buffer


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
;; JetBrains Islands Dark port, see themes/islands-dark-theme.el
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'islands-dark t)
;; gaps between windows in the darker main-window color, like the islands
;; right-only: a bottom divider would sit under the mode line like padding
(setq window-divider-default-right-width 6
      window-divider-default-places 'right-only)
(window-divider-mode 1)

;; *** Mode line
;; Built-in only. Layout:
;;  NORMAL  ● init.el   project   main  ⚠2 ✖1          Elisp …  CRLF  42:17 Top
;; Colors for these faces come from the theme.
(defface my/mode-line-evil-normal '((t :inherit mode-line-emphasis))
  "Evil normal state tag.")
(defface my/mode-line-evil-insert '((t :inherit success))
  "Evil insert state tag.")
(defface my/mode-line-evil-visual '((t :inherit warning))
  "Evil visual state tag.")
(defface my/mode-line-evil-replace '((t :inherit error))
  "Evil replace state tag.")
(defface my/mode-line-evil-other '((t :inherit shadow))
  "Evil emacs/motion/operator state tag.")
(defface my/mode-line-modified '((t :inherit warning))
  "Modified-buffer dot.")
(defface my/mode-line-dim '((t :inherit shadow))
  "Secondary mode line text (encoding, position).")

(setq mode-line-collapse-minor-modes t  ; minor modes behind a clickable "…"
      mode-line-modes-delimiters '("" . "")
      mode-line-right-align-edge 'window
      project-mode-line t
      evil-mode-line-format nil)        ; the tag is placed below instead

(with-eval-after-load 'evil
  (setq evil-normal-state-tag   (propertize " NORMAL "   'face 'my/mode-line-evil-normal)
        evil-insert-state-tag   (propertize " INSERT "   'face 'my/mode-line-evil-insert)
        evil-visual-state-tag   (propertize " VISUAL "   'face 'my/mode-line-evil-visual)
        evil-replace-state-tag  (propertize " REPLACE "  'face 'my/mode-line-evil-replace)
        evil-emacs-state-tag    (propertize " EMACS "    'face 'my/mode-line-evil-other)
        evil-motion-state-tag   (propertize " MOTION "   'face 'my/mode-line-evil-other)
        evil-operator-state-tag (propertize " OPERATOR " 'face 'my/mode-line-evil-other)))

(defun my/mode-line-modified ()
  "Lock for read-only, dot for modified, blank otherwise."
  (cond (buffer-read-only (propertize " " 'face 'my/mode-line-dim))
        ((and (buffer-modified-p) (not (derived-mode-p 'special-mode)))
         (propertize "● " 'face 'my/mode-line-modified))
        (t "  ")))

(defun my/mode-line-vc ()
  "Branch name without the backend prefix, e.g. \" main\"."
  (when (stringp vc-mode)
    (concat "  " (replace-regexp-in-string "\\` *[A-Za-z]+[-:@!?]" " " vc-mode))))

(defun my/mode-line-encoding ()
  "Only show coding/EOL when it is not plain utf-8 with unix line ends."
  (let* ((cs buffer-file-coding-system)
         (base (coding-system-base cs))
         (eol (coding-system-eol-type cs))
         (parts (delq nil
                      (list (unless (memq base '(utf-8 undecided prefer-utf-8 no-conversion))
                              (upcase (symbol-name base)))
                            (pcase eol (1 "CRLF") (2 "CR"))))))
    (when parts
      (propertize (concat (string-join parts " ") "  ") 'face 'my/mode-line-dim))))

;; plain "%b" instead of the default 12-column padded name
(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   (:eval (bound-and-true-p evil-mode-line-tag))
   " "
   (:eval (my/mode-line-modified))
   (:eval (when (file-remote-p default-directory)
            (propertize "@ " 'face 'my/mode-line-dim)))
   mode-line-buffer-identification
   (project-mode-line ("  " project-mode-line-format))
   (:eval (my/mode-line-vc))
   (flymake-mode ("  " flymake-mode-line-counters))
   "%n"
   mode-line-format-right-align
   mode-line-misc-info
   mode-line-modes
   "  "
   (:eval (my/mode-line-encoding))
   (:propertize "%l:%c  %p" face my/mode-line-dim)
   "  "))

(use-package org-todoist
  :vc (:url "https://github.com/Lillenne/org-todoist"
       :rev :newest        ; latest commit; omit to use the last tagged release
       :branch "main")
  :config
  (setq org-todoist-api-token "0187338b068672d9eba165ac6791f10eaf362554"
		org-todoist-storage-dir my/notes-directory))

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
   '("de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0"
	 "1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5"
	 "488b82a8d9ace0aea8a6825db144e3c65c4f1ef3e090b618bf311d9cdb513322"
	 default))
 '(git-gutter:added-sign " ")
 '(git-gutter:deleted-sign " ")
 '(git-gutter:modified-sign " ")
 '(package-selected-packages
   '(acp agent-shell consult-denote consult-projectile corfu csv-mode
		 dap-mode dart-mode dired-subtree emacsql embark-consult
		 evil-collection evil-multiedit evil-nerd-commenter evil-org
		 evil-surround general git-gutter gnu-elpa-keyring-update
		 hackernews jupyter kdl-mode kotlin-mode log4j-mode lsp-mode
		 lua-mode magit marginalia markdown-mode modus-themes
		 nano-theme olivetti orderless org-journal org-modern
		 org-todoist plantuml-mode powershell restclient rustic
		 shell-maker spinner treemacs vertico web-mode yaml))
 '(package-vc-selected-packages
   '((org-todoist :url "https://github.com/Lillenne/org-todoist" :branch
				  "main"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

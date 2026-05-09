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


;; ***********************************************************************
;; ***
;; *** better defaults
;; ***


;; variables
(setq
	my/notes-directory (file-name-concat (expand-file-name (my/home-directory)) "notes")
	default-directory (expand-file-name "~/")
	explicit-shell-file-name (if (eq system-type 'windows-nt)
				     "pwsh.exe"
				   (or (getenv "SHELL") "/bin/bash")))

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


;; hooks
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; tabs hooks
(add-hook 'lisp-mode-hook 'my/disable-tabs)
(add-hook 'prog-mode-hook 'my/enable-tabs)
(add-hook 'emacs-lisp-mode-hook 'my/enable-tabs)

;; disable line nubmer hooks
(defun my/disable-line-numbers ()
  (display-line-numbers-mode 0))
(add-hook 'shell-mode-hook 'my/disable-line-numbers)
(add-hook 'org-mode-hook 'my/disable-line-numbers)
(add-hook 'treemacs-mode-hook 'my/disable-line-numbers)
(add-hook 'eshell-mode-hook 'my/disable-line-numbers)
(add-hook 'pdf-view-mode-hook 'my/disable-line-numbers)



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
        org-hide-leading-stars t
        org-hide-block-startup t
		org-default-notes-file (file-name-concat my/notes-directory "inbox.org")
		org-agenda-files (list my/notes-directory)
        org-ellipsis " ─╮"
        org-todo-keywords '("TODO" "WIP"
							"|"
							"DONE" "BLOCKED" "CANCELED")
        org-todo-keyword-faces '(("TODO" . (:foreground "#ff6e6e" :weight bold))
                                 ("WIP" . (:foreground "#ffea73" :weight bold))
                                 ("DONE" . (:foreground "#98971a" :weight bold))
                                 ("CANCELED" . (:foreground "#ebdbb2" :weight bold))
                                 ("BLOCKED" . (:foreground "#ebdbb2" :weight bold))))
  ;; variable-sized headlines
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.25)
                  (org-level-3 . 1.15)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.05)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))
  (set-face-attribute 'org-document-title nil :height 1.6 :weight 'bold))

(use-package ox-latex
  :custom
  (org-latex-listings t))


(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-name-concat my/notes-directory "org-roam"))
  (org-roam-capture-templates '(
    ("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
"#+title: ${title}
#+STARTUP: fold
#+date: %U
")
      :unnarrowed t)
    ("b" "Book" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
"#+title: ${title}
#+STARTUP: fold
#+date: %U
#+filetags: book
")
      :unnarrowed t)
    ("p" "Project" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
"#+title: ${title}
#+STARTUP: fold
#+date: %U
#+filetags: project
")
      :unnarrowed t)
    )))

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


(use-package org-modern
  :ensure t
  :custom
  (org-modern-star 'replace)
  (org-modern-table nil)
  (org-modern-block-fringe nil)
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

(use-package olivetti
  :ensure t
  :hook (org-mode . olivetti-mode)
  :custom (olivetti-body-width 100))


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
  :hook ((markdown-mode . my/disable-line-numbers)
         (markdown-mode . olivetti-mode)
         (markdown-mode . visual-line-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-hide-urls t)
  :config
  (dolist (face '((markdown-header-face-1 . 1.4)
                  (markdown-header-face-2 . 1.25)
                  (markdown-header-face-3 . 1.15)
                  (markdown-header-face-4 . 1.1)
                  (markdown-header-face-5 . 1.05)
                  (markdown-header-face-6 . 1.0)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(use-package eglot
  :ensure t
  :after (yasnippet)
  :hook ((go-ts-mode
          rust-mode
          typescript-ts-mode
          powershell-mode
          nxml-mode
          js-ts-mode
          go-ts-mode
          dart-mode
          latex-mode)
		 . eglot-ensure))


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
    "ps"      'consult-projectile-switch-project
    "pf"      'consult-projectile-find-file
    "pb"      'consult-project-buffer
    "pe"      'projectile-dired

    ;; org roam key binding
    "oa"        'org-agenda
    "oc"        'org-capture
    "of"        'org-roam-node-find
    "ob"        'org-roam-buffer
    "ot"        'org-roam-buffer-toggle
	"oi"		(lambda () (interactive) (find-file (file-name-concat my/notes-directory "inbox.org")))

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
;; *** RSS (Elfeed)
;; ***

(use-package elfeed
  :ensure t
  :defer t
  :config
  (setq elfeed-use-curl t
        elfeed-curl-max-connections 10
        elfeed-search-filter "@6-months-ago +unread"
        elfeed-feeds
        '(;; Architecture & System Design
          ("https://martinfowler.com/feed.atom"            arch)
          ("https://blog.pragmaticengineer.com/rss/"       arch)
          ("http://highscalability.com/rss.xml"            arch)

          ;; Web
          ("https://web.dev/feed.xml"                      web)
          ("https://developer.mozilla.org/en-US/blog/rss.xml" web)
          ("https://www.smashingmagazine.com/feed/"        web)

          ;; Java & Spring
          ("https://spring.io/blog.atom"                   java spring)
          ("https://www.baeldung.com/feed/"                java)
          ("https://inside.java/feed.xml"                  java)

          ;; AI
          ("https://ai.googleblog.com/feeds/posts/default" ai)
          ("https://www.deeplearning.ai/the-batch/feed/"   ai)
          ("https://research.facebook.com/feed/"           ai)

          ;; Security
          ("https://krebsonsecurity.com/feed/"             security)
          ("https://www.schneier.com/feed/atom/"           security)
          ("https://googleprojectzero.blogspot.com/feeds/posts/default" security)
          ("https://feeds.feedburner.com/TheHackersNews"   security)
          ("https://isc.sans.edu/rssfeed_full.xml"         security)

          ;; Deep Dives
          ("https://jvns.ca/atom.xml"                      deep)
          ("https://danluu.com/atom.xml"                   deep)
          ("https://matklad.github.io/feed.xml"            deep)
          ("https://fasterthanli.me/index.xml"             deep))))

;; ***********************************************************************
;; ***
;; *** Theme and UI Customizations
;; ***
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-operandi-tinted t))
;; (load-theme 'tango)


;; ***********************************************************************
;; ***
;; *** Email (mu4e)
;; ***

(when (eq system-type 'gnu/linux)
  (load (expand-file-name "mu4e-config.el" user-emacs-directory)))


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
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

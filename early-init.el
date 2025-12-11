;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings for quick startup and convenience
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Startup speed, annoyance suppression
(setq gc-cons-threshold 10000000)

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)


(setq large-file-warning-thresold 100000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)


;; Make the backspace properly erase the tab instead of
;; removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;; Speed up startup
(setq auto-mode-case-fold nil)

(setq ring-bell-function 'ignore)

;; always select the newly opend buffer
(setq help-window-select t)

; START TABS CONFIG
;; Create a variable for our preferred tab width
(setq custom-tab-width 4)

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode 1)
(column-number-mode)
(global-display-line-numbers-mode t)
(toggle-truncate-lines 0)

;; disable warning
(setq warning-minimum-level :emergency)

;; Use RET to open org-mode links, including those in quick-help.org
(setq org-return-follows-link t)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; y/n for  answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)
(defalias 'yes-or-no-p 'y-or-n-p)


; (setq default-frame-alist '(;; Setting the face in here prevents flashes of
;                             ;; color as the theme gets activated
;                             (background-color . "#000000")
;                             (ns-appearance . dark)
;                             (ns-transparent-titlebar . t)))


;; remove window fringes
(fringe-mode 0)

(setq initial-scratch-message "
;;  ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;;  ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;  █████╗  ██╔████╔██║███████║██║     ███████╗
;;  ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;  ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;  ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
")

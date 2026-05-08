;; ***********************************************************************
;; ***
;; *** mu4e Email Configuration
;; ***

(use-package mu4e
  :ensure t
  :config

  ;; ---------------------------------------------------------------------------
  ;; Core settings

  (setq mu4e-maildir "~/.mail"
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 300
        mu4e-index-update-in-background t
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-compose-dont-reply-to-self t
        mu4e-hide-index-messages t)

  ;; ---------------------------------------------------------------------------
  ;; Accounts

  (defvar my/mu4e-account-alist
    '(("gmail"
       (mu4e-sent-folder   "/gmail/[Gmail]/Sent Mail")
       (mu4e-drafts-folder "/gmail/Drafts")
       (mu4e-trash-folder  "/gmail/[Gmail]/Trash")
       (mu4e-refile-folder "/gmail/[Gmail]/All Mail")
       (user-mail-address  "moamenhredeen@gmail.com")
       (user-full-name     "Moamen Hraden")
       (smtpmail-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-service 587)
       (smtpmail-stream-type starttls))
      ("proton"
       (mu4e-sent-folder   "/proton/Sent")
       (mu4e-drafts-folder "/proton/Drafts")
       (mu4e-trash-folder  "/proton/Trash")
       (mu4e-refile-folder "/proton/Archive")
       (user-mail-address  "moamen@hredeen.com")
       (user-full-name     "Moamen Hraden")
       (smtpmail-smtp-server "127.0.0.1")
       (smtpmail-smtp-service 1025)
       (smtpmail-stream-type starttls))))

  (defun my/mu4e-set-account ()
    "Set account vars based on selected folder or prompt."
    (let* ((account (if mu4e-compose-parent-message
                        (let ((from (mu4e-message-field mu4e-compose-parent-message :from)))
                          (if (seq-find (lambda (addr)
                                          (string-match-p "hredeen\\.com"
                                                          (plist-get addr :email)))
                                        from)
                              "proton" "gmail"))
                      (completing-read "Account: " (mapcar #'car my/mu4e-account-alist))))
           (vars (cdr (assoc account my/mu4e-account-alist))))
      (dolist (var vars)
        (set (car var) (cadr var)))))

  (add-hook 'mu4e-compose-pre-hook #'my/mu4e-set-account)

  ;; Default to Gmail
  (setq mu4e-sent-folder   "/gmail/[Gmail]/Sent Mail"
        mu4e-drafts-folder "/gmail/Drafts"
        mu4e-trash-folder  "/gmail/[Gmail]/Trash"
        mu4e-refile-folder "/gmail/[Gmail]/All Mail")

  ;; ---------------------------------------------------------------------------
  ;; SMTP

  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls)

  ;; ---------------------------------------------------------------------------
  ;; Bookmarks (quick search views)

  (setq mu4e-bookmarks
        '((:name "All Inboxes"
           :query "maildir:/gmail/Inbox OR maildir:/proton/Inbox"
           :key ?i)
          (:name "Gmail Inbox"
           :query "maildir:/gmail/Inbox"
           :key ?g)
          (:name "Proton Inbox"
           :query "maildir:/proton/Inbox"
           :key ?p)
          (:name "Unread"
           :query "flag:unread AND NOT flag:trashed"
           :key ?u)
          (:name "Today"
           :query "date:today..now"
           :key ?t)
          (:name "Last 7 days"
           :query "date:7d..now"
           :key ?w)))

  ;; ---------------------------------------------------------------------------
  ;; UI

  (setq mu4e-headers-date-format "%Y-%m-%d %H:%M"
        mu4e-headers-fields '((:date          . 18)
                               (:flags         .  6)
                               (:from          . 22)
                               (:subject       . nil))))

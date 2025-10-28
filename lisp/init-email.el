;;; init-email.el --- Email Configuration -*- lexical-binding: t; -*-
;;; Code: 

(use-package gnus
  :init
  (setq gnus-home-directory "~/.local/share/gnus"
	gnus-startup-file "~/.local/share/gnus/newsrc")
  :config
  (setq user-full-name "Sebsatian Hempel Linde"
	user-mail-address "sebastian@hempellinde.com")
  (setq gnus-select-method
        '(nnimap "imap.fastmail.com"
                 (nnimap-user "sebastian@hempellinde.com")
                 (nnimap-inbox "INBOX")
                 (nnimap-server-port "imaps")
                 (nnimap-stream ssl)))
  (setq gnus-save-newsrc-file nil  ;; Not needed 
	))



;; (use-package gnus
;;   :init
;;   ;; let's do our best to keep Gnus files/dir outside of ~
;;   ;; some of these are not really Gnus vars, but declared in
;;   ;; nndraft, message, etc.
;;   (setq gnus-home-directory "~/.gnus.d/"
;;         gnus-directory "~/.gnus.d/News/"
;;         gnus-default-directory "~/.gnus.d"
;;         nndraft-directory "~/.gnus.d/News/drafts/"
;;         nnmh-directory "~/.gnus.d/News/drafts/"
;;         nnfolder-directory "~/.gnus.d/Mail/archive"
;;         nnml-directory "~/.gnus.d/Mail/"
;;         message-directory "~/.gnus.d/Mail/"
;;         gnus-article-save-directory "~/.gnus.d/News/"
;;         gnus-read-newsrc-file nil
;;         gnus-save-newsrc-file nil
;;         ;; the two values above mean I don't need this.
;;         ;; But, just in case:
;;         gnus-startup-file "~/.gnus.d/.newsrc"
;;         gnus-dribble-directory "~/.gnus.d/"
;;         gnus-use-dribble-file nil
;;         gnus-always-read-dribble-file nil)
;;   :custom
;;   ;; the first four are not really Gnus values, but this is a sensible place
;;   ;; to set them
;;   (user-full-name "Sebsatian Hempel Linde")
;;   (user-mail-address "sebastian@hempellinde.com")
;;   ;; -----
;;   (gnus-select-method '(nnnil ""))
;;   (gnus-secondary-select-methods '((nnimap "fastmail"
;;                                            (nnimap-address "imap.fastmail.com")
;;                                            (nnimap-server-port 993)
;;                                            (nnimap-stream ssl)
;;                                            (nnir-search-engine imap))))
;;   ;; Archive outgoing email in Sent folder, mark it as read
;;   (gnus-message-archive-method '(nnimap "imap.fastmail.com"))
;;   (gnus-message-archive-group "nnimap+fastmail:Sent")
;;   (gnus-gcc-mark-as-read t)
;;   (gnus-search-use-parsed-queries t)
;;   (gnus-auto-select-next nil)
;;   (gnus-paging-select-next nil)
;;   (gnus-summary-stop-at-end-of-message t)
;;   (gnus-mime-display-multipart-related-as-mixed t)
;;   (gnus-user-date-format-alist '((t . "%Y-%m-%d %I:%M%p")))
;;   (gnus-thread-sort-functions '(gnus-thread-sort-by-date))
;;   (gnus-show-threads t)
;;   (gnus-sum-thread-tree-false-root nil) ;; use subject
;;   (gnus-sum-thread-tree-root nil)
;;   (gnus-sum-thread-tree-indent " ")
;;   (gnus-sum-thread-tree-vertical        "│")
;;   (gnus-sum-thread-tree-leaf-with-other "├─>")
;;   (gnus-sum-thread-tree-single-leaf     "└─>")
;;   ;; search all directories (groups) when looking for more messages
;;   ;; in the same thread (defaul A T, v s for me)
;;   (gnus-refer-thread-use-search t))

;; (use-package message
;;   :bind
;;   (:map message-mode-map
;;         ("C-c e" . hoagie-confirm-encrypt)
;;         ;; used to set this in a hook, but truth is, 99% of my email
;;         ;; is sent from the main address
;;         ("C-c f" . hoagie-change-from))
;;   :custom
;;   ;; actually part of simple.el, but putting it here because
;;   ;; it is relevant to message.el behaviour for C-x m
;;   (mail-user-agent 'gnus-user-agent)
;;   (read-mail-command 'gnus)
;;   ;; integrate with ecomplete. Press TAB to get email completion
;;   (message-mail-alias-type 'ecomplete)
;;   (message-self-insert-commands nil)
;;   (message-expand-name-standard-ui t)
;;   ;; This causes problems when Cc: is already present.
;;   ;; Need to either add a func to add a header, or internalize the
;;   ;; existing commands to "go to header" which add them
;;   ;; (message-default-mail-headers "Cc: \nBcc: \n")
;;   :config
;;   ;; From https://www.emacswiki.org/emacs/GnusTutorial#h5o-40
;;   (defvar hoagie-email-addresses '("sebastian@sebasmonia.com"
;;                                    "capsule@sebasmonia.com"
;;                                    "code@sebasmonia.com"
;;                                    "mailing@sebasmonia.com"
;;                                    "subscriptions@sebasmonia.com"
;;                                    "thingstopay@sebasmonia.com"
;;                                    "work@sebasmonia.com")
;;     "The list of aliases in my email setup.")
;;   (defun hoagie-message-change-from ()
;;     "Select the \"From:\" address when composing a new email."
;;     (interactive)
;;     (let* ((selected-address (completing-read "From: " hoagie-email-addresses))
;;            (address (concat user-full-name " <" selected-address ">"))
;;            (from-text (concat "From: " address)))
;;       (setq gnus-article-current-point (point))
;;       (goto-char (point-min))
;;       (while (re-search-forward "^From:.*$" nil t)
;;         (replace-match from-text))
;;       (goto-char gnus-article-current-point)))
;;   (defun hoagie-confirm-encrypt ()
;;     "Answer y/n to whether to send the message encrypted."
;;     (interactive)
;;     (when (y-or-n-p "Encrypt message?")
;;       (mml-secure-message-encrypt))))

;; ;; Send email via Fastmail's SMTP:
;; (use-package smtpmail
;;   :custom
;;   (send-mail-function 'smtpmail-send-it)
;;   (smtpmail-smtp-server "smtp.fastmail.com")
;;   (smtpmail-stream-type 'starttls)
;;   (smtpmail-smtp-service 587))

;;   (setq send-mail-function 'smtpmail-send-it
;;         message-send-mail-function 'smtpmail-send-it
;;         smtpmail-smtp-server "smtp.fastmail.com"
;;         smtpmail-smtp-service 587
;;         smtpmail-stream-type 'starttls
;;         smtpmail-debug-info t
;;         smtpmail-debug-verb t)


(provide 'init-email)
;;; init-email.el ends here

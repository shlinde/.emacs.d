;;; init-editor.el --- Editor Configuration -*- lexical-binding: t; -*-
;;; Code:

;;; User Experience
;;;; Terminal
;; Enable mouse-based scrolling in terminal Emacs
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  ;; These are often helpful for finer control if xterm-mouse-mode alone isn't enough
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(setq select-enable-clipboard 't)
(setq select-enable-primary nil)
(setq interprogram-cut-function #'gui-select-text)

;;;; Editor
;; Ensure to setup WSL

;; Ensure keyboard repeat rate
(shell-command "xset r rate 250 60")

;; Send custom file to oblivion
(setq custom-file (make-temp-file "emacs-custom-"))

;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
;; as a buffer is unsaved, backups create copies once, when the file is first
;; written, and never again until it is killed and reopened. This is better
;; suited to version control, and I don't want world-readable copies of
;; potentially sensitive material floating around our filesystem.
(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat shl--cache-dir "backup/")))
      tramp-backup-directory-alist backup-directory-alist)

;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
;; Use `recover-file' or `recover-session' to recover them.
(setq auto-save-default t
      ;; Don't auto-disable auto-save after deleting big chunks. This defeats
      ;; the purpose of a failsafe. This adds the risk of losing the data we
      ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
      auto-save-include-big-deletions t
      ;; Keep it out of `user-emacs-dir' or the local directory.
      auto-save-list-file-prefix (concat shl--cache-dir "autosave/")
      tramp-auto-save-directory  (concat shl--cache-dir "tramp-autosave/")
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

;;; Built-in Plugins
(use-package autorevert
  ;; revert buffers when their files/state have changed
  :config
  (auto-revert-mode)
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        ;; Only prompts for confirmation when buffer is unsaved.
        revert-without-query (list ".")))

(use-package savehist
  ;; persist variables across sessions
  :hook (emacs-startup . savehist-mode)
  ;; :custom (savehist-file (concat shl--cache-dir "savehist"))
  :config
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil     ; save on kill only
        savehist-additional-variables
        '(kill-ring                        ; persist clipboard
          register-alist                   ; persist macros
          mark-ring global-mark-ring       ; persist marks
          search-ring regexp-search-ring)) ; persist searches
  (defun shl--savehist-remove-unprintable-registers-h ()
    "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
    ;; Save new value in the temp buffer savehist is running
    ;; `savehist-save-hook' in. We don't want to actually remove the
    ;; unserializable registers in the current session!
    (setq-local register-alist
                (cl-remove-if-not #'savehist-printable register-alist)))
  (defun shl--savehist-unpropertize-variables-h ()
    "Remove text properties from `kill-ring' to reduce savehist cache size."
    (setq kill-ring
          (mapcar #'substring-no-properties
                  (cl-remove-if-not #'stringp kill-ring))
          register-alist
          (cl-loop for (reg . item) in register-alist
                   if (stringp item)
                   collect (cons reg (substring-no-properties item))
                   else collect (cons reg item))))
  (add-hook 'savehist-save-hook #'shl--savehist-remove-unprintable-registers-h)
  (add-hook 'savehist-save-hook #'shl--savehist-unpropertize-variables-h))

(use-package recentf
  :hook (elpaca-after-init . recentf-mode))

(use-package saveplace
  :hook (elpaca-after-init . save-place-mode))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package subword
  :ensure nil
  :hook (after-init . global-subword-mode))

(use-package window
  :ensure nil
  :bind (("M-o" . other-window)))

;; Expand Region
(use-package expand-region
  :ensure t
  :bind ("M-h" . er/expand-region))

;; Multiple Cursors Emacs
(use-package multiple-cursors
  :ensure t)

;;; Bookmarks
(use-package bookmark
  :ensure nil
  :defer 2
  :config
  (setq bookmark-default-file (concat shl--cache-dir "bookmarks")))

(defun shl/load-config ()
  "Reload Emacs config."
  (interactive)
  (load-file user-init-file)
  (message "Emacs configuration reloaded."))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))


(use-package avy
  :ensure t
  :commands (avy-goto-word-1 avy-goto-char-2 avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 0.27)
  ;; (setq avy-keys '( ?f ?d ?s ?a ?g ?q ?e ?r
  ;;                   ?c ?v ?p ?. ?, 
  ;;                   ?u ?/ ?b ?n ?i ?o ?' ?l ?j))
  ;; (setq avy-single-candidate-jump nil)
  (setq avy-dispatch-alist '((?m . avy-action-mark)
                             (?$ . avy-action-ispell)
                             (?z . avy-action-zap-to-char)
                             (?  . avy-action-embark)
                             (?= . avy-action-define)
                             (23 . avy-action-zap-to-char)
                             ;; (67108896 . avy-action-mark-to-char)
                             (?h . avy-action-helpful)
                             (?x . avy-action-exchange)
                             
                             (11 . avy-action-kill-line)
                             (25 . avy-action-yank-line)
                             
                             (?w . avy-action-easy-kill)
                             (?k . avy-action-kill-stay)
                             (?y . avy-action-yank)
                             (?t . avy-action-teleport)
                             
                             (?W . avy-action-copy-whole-line)
                             (?K . avy-action-kill-whole-line)
                             (?Y . avy-action-yank-whole-line)
                             (?T . avy-action-teleport-whole-line)
                             ;; (67108923 . avy-action-add-cursor)
                             (?\; . avy-action-add-cursor)))

  :init
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  
  
  :bind (("C-M-'"   . avy-resume)
         ("M-j"     . avy-goto-char-timer)
         ("M-s y"   . avy-copy-line)
         ("M-s M-y" . avy-copy-region)
         ("M-s M-k" . avy-kill-whole-line)
         ("M-s j"   . avy-goto-char-2)
         ("M-s M-p" . avy-goto-line-above)
         ("M-s M-n" . avy-goto-line-below)
         ("M-s C-w" . avy-kill-region)
         ("M-s M-w" . avy-kill-ring-save-region)
         ("M-s t"   . avy-move-line)
         ("M-s M-t" . avy-move-region)
         ("M-g l"   . avy-goto-end-of-line)))


(provide 'init-editor)
;;; init-editor.el ends here

;;; init-dired.el --- Dired Configuration -*- lexical-binding: t; -*-
;;; Code:

(require 'init-general)

(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-jump-other-window)
  :general
  (:keymaps 'dired-mode-map
	    "h" '(shl-dired-updirectory :which-key "up directory"))
	    ;; "s" '(:ignore t :which-key "sort")
	    ;; "se" '(dired-sort-by-extension :which-key "by extension"))
  :ghook 'hl-line-mode
  :config
  (defun shl-dired-updirectory ()
    (interactive)
    (find-alternate-file ".."))

  (defun dired-sort-by-extension-and-name ()
    "Sort by extension, then name (groups file types together)."
    (interactive)
    (dired-sort-other "-laFh1vX --group-directories-first"))

  (defun dired-sort-by-time ()
    "Sort by modification time, newest first (directories first)."
    (interactive)
    (dired-sort-other "-laFh1vt --group-directories-first"))

  (defun dired-sort-by-name-only ()
    "Sort alphabetically only (directories first)."
    (interactive)
    (dired-sort-other "-laFh1v --group-directories-first"))

  (defun dired-sort-by-size ()
    "Sort by file size, largest first (directories first)."
    (interactive)
    (dired-sort-other "-laFh1vS --group-directories-first"))

  ;; Like with ls, append "@" to file names if they're symlinks
  (setq dired-ls-F-marks-symlinks t)
  ;; don't ask about killing buffer visiting file
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  ;; always delete and copy recursively
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-deletion-confirmer 'y-or-n-p)
  (setq dired-dwim-target t)
  ;; allow editing file permissions
  (setq wdired-allow-to-change-permissions t)
  ;; open PDF files in external viewer
  (setq dired-guess-shell-alist-user '(("\.pdf$" . default)))

  ;; Instantly revert Dired buffers
  (setq dired-auto-revert-buffer t)

  ;; Showing free space is a sigificant performance hit.
  (setq dired-free-space nil)
  ;; Allow dired, gnus, & mu4e to work together
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))

(use-package dired-narrow
  :ensure t
  :general (:keymaps 'dired-mode-map
		     "/" '(dired-narrow :which-key "dired narrow")))

;;;; Dired Colors
(use-package diredfl
  :ensure t
  :ghook 'diredfl-global-mode)

;;;; Cycle Dired Buffer
;;https://www.reddit.com/r/emacs/comments/qnthhw/comment/hjiv2uc/?utm_source=share&utm_medium=web2x&context=3
;; Allow for cycling from bottom to top of dired buffer and vice versa
(add-hook 'dired-mode-hook
          (defun shl-dired-wrap ()
            "Cycle from bottom to top of buffer"
            (make-local-variable 'post-command-hook)
            (add-hook 'post-command-hook
                      (defun shl-dired-wrap-1 ()
                        ""
                        (if (= 1 (save-excursion
                                   (forward-line)))
                            (goto-line 3))
                        (if (= -1 (save-excursion
                                    (forward-line -1)))
                            (goto-line (count-lines
                                        (point-min)
                                        (point-max))))))))


(provide 'init-dired)
;;; init-dired.el ends here

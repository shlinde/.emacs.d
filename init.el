;;; init.el --- Emacs Config of Sebastian Hempel Linde -*- lexical-binding: t -*-


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
(load-theme 'modus-vivendi-deuteranopia :no-confirm)

(display-time-mode)
(setq default-frame-alist '((undecorated . t)))


;; Disable unwanted UI elements
(dolist (mode '(tool-bar-mode scroll-bar-mode 
			      horizontal-scroll-bar-mode menu-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defaults

;; Don't clutter directories with backups
(let ((backup-dir "~/.cache/tmp/emacs/backups")
      (auto-saves-dir "~/.cache/tmp/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))


(setopt backup-by-copying t    ; Don't delink hardlinks
	delete-old-versions t  ; Clean up the backups
	version-control t      ; Use version numbers on backups,
	kept-new-versions 5    ; keep some new versions
	kept-old-versions 2)   ; and some old ones, too


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core
(defvar shl/missing-packages
  "Packages not able to install.")

(defvar shl/package-contents-refreshed nil)

(defun shl/package-refresh-contents-once ()
  (when (not shl/package-contents-refreshed)
    (setq shl/package-contents-refreshed t)
    (package-refresh-contents)))

(defmacro shl/pkg-config (name &rest config)
  (declare (indent 1))
  `(if (not (package-installed-p ,name))
       (condition-case err
	   (progn (shl/package-refresh-contents-once)
		  (package-install ,name))
	 (error
	  (message "Couldn't install optional package `%s': %S" ,name err)
	  (push ,name shl/missing-packages)))
     ,@config))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Experience
(shl/pkg-config 'vertico
  (vertico-mode))

(shl/pkg-config 'embark
  (global-set-key (kbd "C-.") 'embark-act)
  (global-set-key (kbd "M-.") 'embark-dwim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Support

(when (not (package-installed-p 'zig-mode))
  (package-install 'zig-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(embark "vertico" "vertico" vertico "vertico" "vertico" "vertico"
	    zig-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

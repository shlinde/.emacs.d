(defvar shl-is-wsl
  (when (string-match "-[Mm]icrosoft" operating-system-release)
    t)
  "Is Emacs running in WSL?")

(defvar shl-is-tiling-wm
  (when (and (getenv "XDG_SESSION_DESKTOP") (string-match "river" (getenv "XDG_SESSION_DESKTOP")))
    t)
  "Is Emacs running in a tiling window manager")

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      ring-bell-function 'ignore
      use-dialog-box nil
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t)

;; Disable unwanted UI elements
(dolist (mode '(tool-bar-mode scroll-bar-mode 
			      horizontal-scroll-bar-mode menu-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; When running in a tiling wm I don't want decorations
(when shl-is-tiling-wm
  (setq default-frame-alist '((undecorated . t))))

;; Increase garbage collection threshold while loading Emacs.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)


;; Speed up startup time - but save the values for after Emacs
;; is started.
(defvar shl--file-name-handler-alist file-name-handler-alist)
(defvar shl--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1000 1000 8)
                  gc-cons-percentage 0.1
                  file-name-handler-alist shl--file-name-handler-alist
                  vc-handled-backends shl--vc-handled-backends)))

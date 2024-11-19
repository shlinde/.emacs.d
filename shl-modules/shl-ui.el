;;; User Interface Configuration -*- lexical-binding: t -*-
;;; Commentary
;;;    This module is responsible for ensuring that the user interface is configured
;;;    This includes the following:
;;;    - Modeline
;;;    - Line numbers
;;;    - Fill column
;;;
;;; Code:

;; Setup line numbers
(use-package display-line-numbers
  :ensure nil
  :hook ((org-mode . display-line-numbers-mode)
         (prog-mode . display-line-numbers-mode))
  :config
  (setopt display-line-numbers-width 3
          display-line-numbers-type 'relative))

;; Display fill-column
(use-package display-fill-column-indicator
  :ensure nil
  :if (boundp 'display-fill-column-indicator)
  :hook ((prog-mode . display-fill-column-indicator-mode))
  :config
  (setq-default indicate-buffer-boundaries 'left
		fill-column 100
                display-fill-column-indicator-character ?┊))

;; Modeline setup
;; Time
(use-package time
  :ensure nil
  :hook ((after-init . display-time-mode))
  :config
  (setopt display-time-24hr-format t))

;; Battery if applicable
(use-package battery
  :if (not shl-is-wsl)
  :ensure nil
  :hook ((after-init . display-battery-mode))
  :config
  (setq battery-mode-line-format
          (cond
           ((eq battery-status-function #'battery-linux-proc-acpi)
	        "⏻%b%p%%,%d°C ")
	       (battery-status-function
	        "⏻%b%p%% "))))

;; Emacs User Interface configuration
(use-package emacs
  :ensure nil
  :config
  (setopt ring-bell-function 'ignore
          use-short-answers t  ;; Use y and n instead of yes and no.
          inhibit-splash-screen t
	  column-number-mode t  ;; Show column number in modeline
	  indent-tabs-mode nil  ;; Ensure that all indentation is with spaces
	  scroll-preserve-screen-position 'always  ;; Ensure that scrolling does not move point
          truncate-lines nil ;; Truncate lines when wider than buffer-width
          truncate-partial-width-windows nil
          require-final-newline t))

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(provide 'shl-ui)
;;; shl-ui.el ends here

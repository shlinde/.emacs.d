;; Line numbers
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


;; Speed up font rendering for special characters
;; @see https://www.reddit.com/r/emacs/comments/988paa/emacs_on_windows_seems_lagging/
(setq inhibit-compacting-font-caches t)

;; Modeline: Time
(use-package time
  :ensure nil
  :hook ((after-init . display-time-mode))
  :config
  (setopt display-time-24hr-format t))

;; Modeline: Battery
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

(provide 'shl-appearance)

(use-package fontaine
  :ensure t
  :hook ((after-init . fontaine-mode)
         (after-init . (lambda ()
			 (fontaine-set-preset 'regular-dark))))
  :config
  (setopt x-underline-at-descent-line nil)
  (setq fontaine-presets
        '((small
           :default-family "Iosevka Comfy Motion"
           :default-height 80
           :variable-pitch-family "Iosevka Comfy Duo")
          (regular-dark
           :default-family "Iosevka Comfy"
           :variable-pitch-family "Iosevka Comfy Duo"
           :default-weight medium) ; like this it uses all the fallback values and is named `regular'
          (regular-light 
           :default-weight semilight) ; like this it uses all the fallback values and is named `regular'
          (medium-light
           :default-weight semilight
           :default-height 115)
          (medium-dark
           :default-weight medium
           :default-height 115
           :bold-weight extrabold)
          (large-dark
           :default-weight medium
           :default-height 125
           :bold-weight extrabold))))

(use-package modus-themes
  :ensure t
  :demand t
  :bind (("<f5>" . modus-themes-toggle))
  :config
  (setopt modus-themes-custom-auto-relload nil
	  modus-themes-to-toggle '(modus-vivendi modus-operandi)
	  modus-themes-mixed-fonts t
	  modus-themes-italic-constructs t
	  modus-themes-bold-constructs t
	  modus-themes-completions '((t . (extrabold)))
	  modus-themes-prompts '(extrabold)
	  modus-themes-common-palette-overrides nil))
  ;; (modus-themes-load-theme (car modus-themes-to-toggle)))

(use-package zenburn-theme
  :ensure t
  :demand t
  :config (load-theme 'zenburn :no-confirm))

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

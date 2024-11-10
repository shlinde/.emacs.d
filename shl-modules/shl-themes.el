;;; Configuration of Themes -'- lexical-binding: t -*-
;;; Commentary:
;;;     Sets up the configuration of the various theme modules used.
;;;     Only load and use the theme that is specified in `init.el'
;;; Code:

(use-package modus-themes
  :if (string= shl-theme "modus")
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
	  modus-themes-common-palette-overrides nil)
  (modus-themes-load-theme (car modus-themes-to-toggle)))

(use-package zenburn-theme
  :if (string= shl-theme "zenburn")
  :ensure t
  :demand t
  :config (load-theme 'zenburn :no-confirm))

(provide 'shl-themes)
;;; shl-theme.el ends here

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

(use-package ef-themes
  :if (string= shl-theme "ef")
  :ensure t
  :demand t
  :config
  (setopt ef-themes-to-toggle '(ef-cyprus ef-autumn)
   ef-themes-variable-pitch-ui t
          ef-themes-mixed-fonts t
          ef-themes-rotate ef-themes-items)
  (defun my-ef-themes-mode-line ()
    "Tweak the style of the mode lines."
    (ef-themes-with-colors
      (custom-set-faces
       `(mode-line ((,c :background ,bg-active :foreground ,fg-main :box (:line-width 1 :color ,fg-dim))))
       `(mode-line-inactive ((,c :box (:line-width 1 :color ,bg-active)))))))

  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-mode-line)
  (ef-themes-select (car ef-themes-to-toggle)))


(use-package zenburn-theme
  :if (string= shl-theme "zenburn")
  :ensure t
  :demand t
  :config (load-theme 'zenburn :no-confirm))

(provide 'shl-themes)
;;; shl-theme.el ends here

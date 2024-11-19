;;; Configuration of Themes -'- lexical-binding: t -*-
;;; Commentary:
;;;     Sets up the configuration of the various theme modules used.
;;;     Only load and use the theme that is specified in `init.el'
;;; Code:

(use-package doom-themes
  :ensure t
  :if (string= shl-theme "doom")
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (if shl-dark-p
      (load-theme 'doom-tokyo-night :no-confirm)
    (load-theme 'doom-one-light :no-confirm))

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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
  :bind (("<f5>" . ef-themes-toggle))
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

(use-package gruber-darker-theme
  :if (string= shl-theme "gruber")
  :ensure t
  :demand t
  :config (load-theme 'gruber-darker :no-confirm))

(provide 'shl-themes)
;;; shl-theme.el ends here

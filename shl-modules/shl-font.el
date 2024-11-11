;;; Font Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;;    Set up fonts using Fontaine.
;;;    Ensure that when `shl-dark-p' is true we use a thicker font weight
;;; Code:

(use-package fontaine
  :ensure t
  :hook ((after-init . fontaine-mode)
         (after-init . (lambda ()
                         (if shl-dark-p
                             (fontaine-set-preset 'regular-dark)
                           (fontaine-set-preset 'regular-light)))))
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
           :default-family "Iosevka Comfy"
           :variable-pitch-family "Iosevka Comfy Duo"
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

;; Speed up font rendering for special characters
;; @see https://www.reddit.com/r/emacs/comments/988paa/emacs_on_windows_seems_lagging/
(setq inhibit-compacting-font-caches t)

(provide 'shl-font)
;;; shl-font.el ends here

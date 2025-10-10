;;; init-ui.el --- User Interface -*- lexical-binding: t; -*-
;;; Code: 


;;;;; Fontaine (font configurations)
;; Read the manual: <https://protesilaos.com/emacs/fontaine>
(use-package fontaine
  :ensure t
  :hook
  ;; Persist the latest font preset when closing/starting Emacs.
  ((elpaca-after-init . fontaine-mode)
   (elpaca-after-init . (lambda ()
			  (fontaine-set-preset 'regular-light))))

  :general
  (shl/leader
    "t f" '(shl/fontaine-toggle :which-key "fontaine toggle"))
  :init
  (defun shl/fontaine-toggle ()
    "Toggle between =regular-light' and =regular-dark' fontaine presets."
    (interactive)
    (let* ((current (and (boundp 'fontaine-current-preset) fontaine-current-preset))
           (next (if (eq current 'regular-light) 'regular-dark 'regular-light)))
      (fontaine-set-preset next)
      (message "Fontaine preset set to: %s" next)))

  :config
  ;; And this is for Emacs28.
  (setq-default text-scale-remap-header-line t)

  ;; This is the default value.  Just including it here for
  ;; completeness.
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; The font family is my design: <https://github.com/protesilaos/aporetic>.
  (setq fontaine-presets
        '((small
           :default-height 80)
          (regular-dark
           :default-family "RobotoMono Nerd Font"
           :default-height 110
	   :default-weight semibold
           :fixed-pitch-family "RobotoMono Nerd Font"
           :variable-pitch-family "Aporetic Sans")
          (regular-light
           :default-family "RobotoMono Nerd Font"
           :default-height 110
	   :default-weight normal
           :fixed-pitch-family "RobotoMono Nerd Font"
           :variable-pitch-family "Aporetic Sans"))))

;;;; Font Lock
(use-package font-lock
  :ensure nil
  :defer 1
  :custom
  ;; Max font lock decoration (set nil for less)
  (font-lock-maximum-decoration t)
  ;; No limit on font lock
  (font-lock-maximum-size nil))

;; Set default line spacing. If the value is an integer, it indicates
;; the number of pixels below each line. A decimal number is a scaling factor
;; relative to the current window's default line height. The setq-default
;; function sets this for all buffers. Otherwise, it only applies to the current
;; open buffer
(setq-default line-spacing 0.05)


(use-package doom-themes
  :ensure t
  :demand t
  :custom
  (doom-gruvbox-dark-variant "hard")
  :config
  (add-hook 'enable-theme-functions #'shl/doom-theme-settings)
  (defun shl/doom-theme-settings (theme &rest args)
    "Additional face settings for doom themes"
    (if (eq theme 'doom-rouge)
        (progn
          (setq window-divider-default-right-width 2
                window-divider-default-bottom-width 2
                window-divider-default-places t)
          (message "Turned on window dividers")
          (window-divider-mode 1))
      (window-divider-mode -1)
      (message "Turned off window dividers"))
    (when (string-match-p "^doom-" (symbol-name theme))
      ;; (when (eq theme 'doom-rouge)
      ;;   (custom-set-faces `(hl-line ((,class :background "#1f2a3f")))))
      ;; Window dividers
      (let ((class '((class color) (min-colors 256))))
        (dolist (face-spec
                 '((aw-leading-char-face (:height 2.0 :foreground unspecified :inherit mode-line-emphasis)
                    ace-window)
                   (aw-background-face (:inherit default :weight normal) ace-window)
                   (outline-1        (:height 1.25) outline)
                   (outline-2        (:height 1.20) outline)
                   (outline-3        (:height 1.16) outline)
                   (outline-4        (:height 1.12) outline)
                   ;; (tab-bar            (:background "black" :height 1.0 :foreground "white")
                   ;;  tab-bar)
                   ;; (tab-bar-tab
                   ;;  (:bold t :height 1.10 :foreground nil :inherit mode-line-emphasis)
                   ;;  tab-bar)
                   ;; (tab-bar-tab-inactive
                   ;;  (:inherit 'mode-line-inactive :height 1.10 :background "black")
                   ;;  tab-bar)
                   ))
          (cl-destructuring-bind (face spec library) face-spec
            (if (featurep library)
                (custom-set-faces `(,face ((,class ,@spec))))
              (with-eval-after-load library
                (when (string-match-p "^doom-" (symbol-name theme))
                  (custom-set-faces `(,face ((,class ,@spec))))))))))))
  (doom-themes-org-config)
  (use-package doom-rouge-theme
    :config
    (setq doom-rouge-padded-modeline nil
          doom-rouge-brighter-comments t
          doom-rouge-brighter-tabs t))

  (use-package doom-iosvkem-theme
    :disabled
    ;; :custom-face
    ;; (default ((t (:background "#061229"))))
    :config
    (setq doom-Iosvkem-brighter-comments nil
          doom-Iosvkem-comment-bg nil
          doom-Iosvkem-brighter-modeline nil))
  (load-theme 'doom-Iosvkem))




(use-package doom-modeline
  :ensure t
  :hook (elpaca-after-init . doom-modeline-mode))


(use-package diminish :ensure t)

(use-package spacious-padding
  :ensure t
  :hook (elpaca-after-init . spacious-padding-mode))

(provide 'init-ui)
;;; init-ui.el ends here

;;; init-ui.el --- Setup UI -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

;;; Theme
(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-mode-line '(accented borderless)
	modus-themes-bold-constructs t
	modus-themes-italic-constructs t
	modus-themes-fringes 'subtle
	modus-themes-tabs-accented t
	modus-themes-paren-match '(bold intense)
	modus-themes-prompts '(bold intense)
	modus-themes-completions
        (quote ((matches . (extrabold underline))
                (selection . (semibold italic))))
	modus-themes-org-blocks 'tinted-background
	modus-themes-scale-headings t
	modus-themes-region '(bg-only)
	modus-themes-headings
	'((1 . (rainbow overline background 1.4))
          (2 . (rainbow background 1.3))
          (3 . (rainbow bold 1.2))
          (t . (semilight 1.1))))
  (modus-themes-select 'modus-vivendi-tinted))

;;; Font
;;;; Fontaine (font configurations)
;; Read the manual: <https://protesilaos.com/emacs/fontaine>
(use-package fontaine
  :ensure t
  :if (display-graphic-p)
  :hook ((emacs-startup . (lambda ()
                        (fontaine-set-preset 'regular-dark))))
  :bind (("C-c f" . fontaine-set-preset)
         ("C-c F" . fontaine-toggle-preset))
  :config
  ;; This is defined in Emacs C code: it belongs to font settings.
  (setq x-underline-at-descent-line nil)

  (setq fontaine-presets
        '((regular-dark
           :default-weight medium
           :default-height 110
           :bold-weight extrabold
           :default-family "Iosevka Comfy"
           :variable-pitch-family "Iosevka Comfy Motion")
          (regular-light 
           :default-weight semilight
           :default-height 110
           :bold-weight extrabold
           :default-family "Iosevka Comfy"
           :variable-pitch-family "Iosevka Comfy Motion")
          (medium-dark
           :default-weight medium
           :default-height 120
           :bold-weight extrabold
           :default-family "Iosevka Comfy"
           :variable-pitch-family "Iosevka Comfy Motion")
          (medium-light
           :default-weight semilight
           :default-height 120
           :bold-weight extrabold
           :default-family "Iosevka Comfy"
           :variable-pitch-family "Iosevka Comfy Motion")))
  (with-eval-after-load 'pulsar
    (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)))

;;; Modeline
(use-package time
  :ensure nil
  :hook (after-init . display-time-mode))


;;; General User Experience
;; Get rid of the splash screen, and make the *scratch* buffer suitable for
;; writing.
(setq inhibit-startup-message t
	  inhibit-splash-screen t
	  inhibit-startup-echo-area-message user-login-name
	  inhibit-default-init t
	  initial-major-mode 'fundamental-mode
	  initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

;; Get rid of the annoying system beep.
(setq ring-bell-function 'ignore)

;; Scrolling

(setq hscroll-margin 2 
      hscroll-step 1
	  next-screen-context-lines 3
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered. The default (0) triggers this too
      ;; aggressively, so I've set it to 10 to recenter if scrolling too far
      ;; off-screen.
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

(setq uniquify-buffer-name-style 'forward)

;; Don't resize emacs in steps.
(setq window-resize-pixelwise t
	  frame-resize-pixelwise t)

;; Show tooltips when hovering over buttons
(tooltip-mode 1)

;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like diff-hl and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;;; Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; UX: GUIs are inconsistent across systems, desktop environments, and themes,
;;   and don't match the look of Emacs. They also impose inconsistent shortcut
;;   key paradigms. I'd rather Emacs be responsible for prompting.
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; FIX: The native border "consumes" a pixel of the fringe on righter-most
;;   splits, `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'after-init #'window-divider-mode)

;; UX: Favor vertical splits over horizontal ones. Monitors are trending toward
;;   wide, rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)

;;; Built-in packages

;;;###package ansi-color
(setq ansi-color-for-comint-mode t)

(with-eval-after-load 'comint-mode
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 2048)) ; double the default

(with-eval-after-load 'compilation-mode
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output 'first-error)
  (add-hook 'compilation-filter-hook
            #'ansi-color-compilation-filter)
  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of Emacs.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))

(with-eval-after-load 'ediff-mode
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))


(use-package paren
  ;; highlight matching delimiters
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;;; Line Numbers
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode . display-line-numbers-mode)
         (text-mode . display-line-numbers-mode)
         (conf-mode . display-line-numbers-mode))
  :config
  ;; Explicitly define a width to reduce the cost of on-the-fly computation
  (setq-default display-line-numbers-width 3)

  ;; Show absolute line numbers for narrowed regions to make it easier to tell the
  ;; buffer is narrowed, and where you are, exactly.
  (add-hook 'display-line-numbers-mode-hook
            (lambda ()
              (setq display-line-numbers 'relative)))
  (setq-default display-line-numbers-widen t))


(provide 'init-ui)
;;; init-ui.el ends here

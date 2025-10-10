;;; init-prog.el --- Programming Configuration -*- lexical-binding: t; -*-
;;; Code:

(require 'init-general)
(require 'init-completion)

;;;; Show Pretty Symbols
(use-package prog-mode
  :ensure nil
  :defer t
  :custom
  ;; Show markup at point
  (prettify-symbols-unprettify-at-point t)
  :config
  ;; Pretty symbols
  (global-prettify-symbols-mode +1))

;;;; Delimiters & Identifiers
(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode
  :ghook 'prog-mode-hook)

(use-package rainbow-identifiers
  :ensure t
  :commands rainbow-identifiers-mode)

;;;;; Pair Delimiters
(use-package elec-pair
  :ensure nil
  :defer 1
  :config (electric-pair-mode 1))

;; Show matching parens
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0))

(use-package embrace
  :ensure t
  :bind (("C-M-s-#" . embrace-commander))
  :config
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (defun embrace-markdown-mode-hook ()
    (dolist (lst '((?* "*" . "*")
                   (?\ "\\" . "\\")
                   (?$ "$" . "$")
                   (?/ "/" . "/")))
      (embrace-add-pair (car lst) (cadr lst) (cddr lst))))
  (add-hook 'markdown-mode-hook 'embrace-markdown-mode-hook))


;;;;; Structural Editing: Edit & Traverse Delimiters
;; TODO: Write a transient for puni bindings
(use-package puni
  :ensure t
  :bind (:map puni-mode-map
         ;; Add slurp and bark bindings
         ("C-(" . #'puni-slurp-backward)
         ("C-)" . #'puni-slurp-forward)
         ("C-{" . #'puni-barf-backward)
         ("C-}" . #'puni-barf-forward))
  :ghook '(prog-mode
          tex-mode
          org-mode markdown-mode
          eval-expression-minibuffer-setup))

(use-package flymake
  :ensure nil
  :ghook '(prog-mode)
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-start-on-flymake-mode t)
  (flymake-no-changes-timeout nil)
  (flymake-start-on-save-buffer t)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-wrap-around nil)
  ;; Customize mode-line
  (flymake-mode-line-counter-format '("" flymake-mode-line-error-counter flymake-mode-line-warning-counter flymake-mode-line-note-counter ""))
  (flymake-mode-line-format '(" " flymake-mode-line-exception flymake-mode-line-counters)))

;; A collection of flymake backends
(use-package flymake-collection
  :ensure t
  :hook (elpaca-after-init . flymake-collection-hook-setup))

;;;;; Multi-Compile
(use-package multi-compile
  :ensure t
  :commands (compile multi-compile-run)
  :custom
  (multi-compile-history-file (concat lem-cache-dir "multi-compile.cache"))
  (multi-compile-completion-system 'default)
  :config
  ;; Use for book compiling
  (defun string/starts-with (string prefix)
    "Return t if STRING starts with prefix."
    (and (stringp string) (string-match (rx-to-string `(: bos ,prefix) t) string))))

(use-package compile
  :ensure nil
  :config
  (setq compilation-always-kill t
	compilation-ask-about-save nil
	compilation-scroll-output 'first-error)

  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of emacs.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer)

  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))

  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(use-package consult-flymake
  :ensure nil
  :after consult
  :general-config
  (shl/leader
    "dd" '(consult-flymake :which-key "consult flymake")))

(provide 'init-prog)
;;; init-prog.el ends here

;;; init-term.el --- Terminal Configuration -*- lexical-binding: t; -*-
;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :hook (emacs-startup . exec-path-from-shell-initialize)
  :init
  (setq exec-path-from-shell-shell-name (executable-find "zsh")
        exec-path-from-shell-arguments '("-l")) ; login so config.fish runs
  :config
  (exec-path-from-shell-copy-envs '("PATH" "MANPATH" "UV_INDEX_YGGDRASIL_USERNAME" "UV_INDEX_YGGDRASIL_PASSWORD" "GEMINI_API_KEY"))
  (exec-path-from-shell-initialize))

;;;;; EAT (Emulate a terminal)
(use-package eat
  :ensure (eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el")))
  :general (shl/leader "tt" '(eat :which-key "terminal"))
  :config
  (setq eat-kill-buffer-on-exit t
        eat-enable-yank-to-terminal t
        eat-enable-directory-tracking t
        eat-enable-shell-command-history t
        eat-enable-shell-prompt-annotation t
        eat-term-scrollback-size nil)
  ;; For `eat-eshell-mode' -- integration with eshell.
  (add-hook 'eshell-load-hook #'eat-eshell-mode))

(provide 'init-term)
;;; init-term.el ends here

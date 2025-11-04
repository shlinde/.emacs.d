;;; init-term.el --- Terminal Configuration -*- lexical-binding: t; -*-
;;; Code:

(require 'init-general)

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
  :config
  (setq eat-kill-buffer-on-exit t
        eat-enable-yank-to-terminal t
        eat-enable-directory-tracking t
        eat-enable-shell-command-history t
        eat-enable-shell-prompt-annotation t
        eat-term-scrollback-size nil)
  ;; For `eat-eshell-mode' -- integration with eshell.
  (add-hook 'eshell-load-hook #'eat-eshell-mode))

(use-package eshell
  :ensure nil  ; Built-in to Emacs
  :commands (eshell)
  :general (shl/leader "e" '(eshell :which-key "terminal"))
  :hook (eshell-mode . (lambda () (setenv "TERM" "xterm-256color")))
  :config
  ;; Basic Eshell settings can go here
  (setq eshell-buffer-shorthand t) ; Show shortened buffer names
  ;; Define aliases directly in your init file.
  (defun eshell/ll (&rest args)
    "Long listing, with colors."
    (eshell/ls "-AFGhl" "--color=always" args))

  (defun eshell/ff (file)
    "Open FILE with `find-file'."
    (find-file file))

  (defun eshell/e (file)
    "Open FILE with `find-file-other-window'."
    (find-file-other-window file))

  (defun eshell/gd ()
    "Run `magit-diff-unstaged'."
    (magit-diff-unstaged))

  (defun eshell/clear ()
    "Clear the Eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer))))

(provide 'init-term)
;;; init-term.el ends here

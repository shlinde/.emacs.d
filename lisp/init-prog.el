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

;;; Languages
;;; YAML
(use-package yaml-mode
  :ensure t)

;;;; Python
(use-package python
  :ensure nil
  :hook ((python-base-mode . shl/python-auto-uv)
	 (python-base-mode . shl/ruff-enable))
  :general
  (shl/local-leader
    :keymaps 'python-base-mode-map
    "t" '((lambda () (interactive)
              (let ((default-directory (shl/python--project-root)))
		(compile "uv run ty check")))
            :which-key "ty check")
    "r" '((lambda () (interactive)
              (let ((default-directory (shl/python--project-root)))
		(compile "uv run ruff check --fix .")))
            :which-key "ruff full"))
  :config
  (defun shl/python--project-root ()
    (when-let* ((proj (project-current)))
      (expand-file-name (project-root proj))))
  (defun shl/python--venv-path ()
    (when-let* ((root (shl/python--project-root)))
      (let ((p (expand-file-name ".venv" root)))
        (when (file-exists-p p) p))))

  (defun shl/python--ensure-exec-path (bin)
    (setq exec-path (cons bin (remove bin exec-path)))
    (setenv "PATH" (concat bin path-separator (getenv "PATH"))))

  (defun shl/python-auto-uv ()
    "Activate project .venv if present; configure PYTHONPATH heuristically."
    (when-let* ((venv (shl/python--venv-path)))
      (let* ((bin (expand-file-name (if (eq system-type 'windows-nt) "Scripts" "bin") venv))
             (python (expand-file-name (if (eq system-type 'windows-nt) "python.exe" "python") bin)))
        (when (file-exists-p python)
          (setq python-shell-interpreter python)
          (shl/python--ensure-exec-path bin)
          (setenv "VIRTUAL_ENV" venv)
          ;; Add src to PYTHONPATH if present
          (let* ((root (shl/python--project-root))
                 (src  (and root (cl-find-if #'file-exists-p
                                             (mapcar (lambda (d) (expand-file-name d root))
                                                     '("src" "lib"))))))
            (when src
              (setenv "PYTHONPATH"
                      (concat src path-separator (or (getenv "PYTHONPATH") "")))))
          (message "uv: activated %s" venv)))))

  (defun shl/ruff--project-has-config ()
    (when-let* ((root (shl/python--project-root)))
      (file-exists-p (expand-file-name "pyproject.toml" root))))
  (defun shl/ruff-format-buffer ()
    (when (and (eq major-mode 'python-mode) (shl/ruff--project-has-config))
      (let* ((root (shl/python--project-root))
             (default-directory root))
        (call-process "uv" nil nil nil "run" "ruff" "format" buffer-file-name)
        (call-process "uv" nil nil nil "run" "ruff" "check" "--fix" buffer-file-name)
        (revert-buffer t t t))))

  (defun shl/ruff-enable ()
    (add-hook 'before-save-hook #'shl/ruff-format-buffer nil t))

  (with-eval-after-load 'compile
    (add-to-list 'compilation-error-regexp-alist-alist
		 '(ty "^ *--> \\([^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))
    (add-to-list 'compilation-error-regexp-alist 'ty))

  ;; Put this in your init.el
  (with-eval-after-load 'compile
    ;; Regex: matches lines like
    ;; │ /full/path/to/file.py:52 │
    (add-to-list 'compilation-error-regexp-alist-alist
		 '(structlog-traceback
		   "^[[:space:]]*[│|]? ?\\(/[^: \n]+\\.py\\):\\([0-9]+\\)\\b"
		   1 2))  ;; 1=file 2=line
    (add-to-list 'compilation-error-regexp-alist
		 'structlog-traceback)))

(use-package uv
  :ensure (uv :type git :host github :repo "johannes-mueller/uv.el")
  :general
  (shl/local-leader
    :keymaps 'python-base-mode-map
    "u" '(uv :which-key "uv")
    "a" '(uv-add :which-key "uv add")
    "r" '(uv-run :which-key "uv run"))
  :init
  (add-to-list 'treesit-language-source-alist '(toml "https://github.com/tree-sitter-grammars/tree-sitter-toml"))
  (unless (treesit-language-available-p 'toml)
    (treesit-install-language-grammar 'toml)))

(provide 'init-prog)
;;; init-prog.el ends here

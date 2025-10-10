;;; init.el --- SHLinde Emacs Configuration -*- lexical-binding: t; -*-
;;; Code:

;;; Core
(defconst shl--cache-dir (concat user-emacs-directory ".cache/"))

(defconst shl--code-reference-dir
  (expand-file-name "~/data/resources/code-reference/")
  "Directory for cloned reference code repositories.")

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

(require 'init-optim)
(require 'init-elpaca)


;;; Org mode
(use-package org
  :ensure (:wait t)
  :bind (("C-c n" . (lambda ()
		      (interactive)
		      (find-file "~/data/org/inbox.org")))))

(require 'init-general)  ; TODO Remove this when the init file is refactored. Should just be required from the modules.
(require 'init-ui)
(require 'init-vc)

;;; User Experience
;;;; Terminal
;; Enable mouse-based scrolling in terminal Emacs
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  ;; These are often helpful for finer control if xterm-mouse-mode alone isn't enough
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(setq select-enable-clipboard 't)
(setq select-enable-primary nil)
(setq interprogram-cut-function #'gui-select-text)

;;;; UI
;;;;; Theme
(setq custom-safe-themes t)

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-custom-auto-reload nil
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-completions '((t . (bold)))
        modus-themes-prompts '(bold)
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15)))))

(use-package kaolin-themes
  :ensure t)

(use-package ef-themes
  :ensure t)





;;;;; Modeline
;; (use-package doom-modeline
;;   :ensure t
;;   :demand t
;;   :hook (elpaca-after-init . doom-modeline-mode))

(use-package time
  :ensure nil
  :demand t
  :hook (elpaca-after-init . display-time-mode))

(use-package battery
  :ensure nil
  :demand t
  :hook (elpaca-after-init . display-battery-mode))

;;;; Editor
;; Ensure to setup WSL

;; Ensure keyboard repeat rate
(shell-command "xset r rate 250 60")

;; Send custom file to oblivion
(setq custom-file (make-temp-file "emacs-custom-"))

;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
;; as a buffer is unsaved, backups create copies once, when the file is first
;; written, and never again until it is killed and reopened. This is better
;; suited to version control, and I don't want world-readable copies of
;; potentially sensitive material floating around our filesystem.
(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat shl--cache-dir "backup/")))
      tramp-backup-directory-alist backup-directory-alist)

;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
;; Use `recover-file' or `recover-session' to recover them.
(setq auto-save-default t
      ;; Don't auto-disable auto-save after deleting big chunks. This defeats
      ;; the purpose of a failsafe. This adds the risk of losing the data we
      ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
      auto-save-include-big-deletions t
      ;; Keep it out of `user-emacs-dir' or the local directory.
      auto-save-list-file-prefix (concat shl--cache-dir "autosave/")
      tramp-auto-save-directory  (concat shl--cache-dir "tramp-autosave/")
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

;;; Built-in Plugins
(use-package autorevert
  ;; revert buffers when their files/state have changed
  :config
  (auto-revert-mode)
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        ;; Only prompts for confirmation when buffer is unsaved.
        revert-without-query (list ".")))

(use-package savehist
  ;; persist variables across sessions
  :hook (emacs-startup . savehist-mode)
  ;; :custom (savehist-file (concat shl--cache-dir "savehist"))
  :config
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil     ; save on kill only
        savehist-additional-variables
        '(kill-ring                        ; persist clipboard
          register-alist                   ; persist macros
          mark-ring global-mark-ring       ; persist marks
          search-ring regexp-search-ring)) ; persist searches
  (defun shl--savehist-remove-unprintable-registers-h ()
    "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
    ;; Save new value in the temp buffer savehist is running
    ;; `savehist-save-hook' in. We don't want to actually remove the
    ;; unserializable registers in the current session!
    (setq-local register-alist
                (cl-remove-if-not #'savehist-printable register-alist)))
  (defun shl--savehist-unpropertize-variables-h ()
    "Remove text properties from `kill-ring' to reduce savehist cache size."
    (setq kill-ring
          (mapcar #'substring-no-properties
                  (cl-remove-if-not #'stringp kill-ring))
          register-alist
          (cl-loop for (reg . item) in register-alist
                   if (stringp item)
                   collect (cons reg (substring-no-properties item))
                   else collect (cons reg item))))
  (add-hook 'savehist-save-hook #'shl--savehist-remove-unprintable-registers-h)
  (add-hook 'savehist-save-hook #'shl--savehist-unpropertize-variables-h))

(use-package recentf
  :hook (elpaca-after-init . recentf-mode))

(use-package saveplace
  :hook (elpaca-after-init . save-place-mode))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package subword
  :ensure nil
  :hook (after-init . global-subword-mode))

(use-package window
  :ensure nil
  :bind (("M-o" . other-window)))

;; Expand Region
(use-package expand-region
  :ensure t
  :bind ("M-h" . er/expand-region))

;; Multiple Cursors Emacs
(use-package multiple-cursors
  :ensure t)


;;; Bookmarks
(use-package bookmark
  :ensure nil
  :defer 2
  :config
  (setq bookmark-default-file (concat shl--cache-dir "bookmarks")))

;;; New Git Project
(defun shl-git-new-project ()
  "Initialize a new git repo and add it to project.el's known projects."
  (interactive)
  (let ((project-dir (expand-file-name
                      (read-directory-name "New project root:"))))
    (magit-init project-dir)
    (setq default-directory project-dir)
    ;; make sure project.el remembers new project
    (let ((pr (project--find-in-directory default-directory)))
      (project-remember-project pr))))

;;; Clone a Git Repo from Clipboard
;; http://xenodium.com/emacs-clone-git-repo-from-clipboard/
(defun shl-git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in Dired when finishe.
Git repo is cloned to directory set by `shl-user-elisp-dir'."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (download-dir shl-user-elisp-dir)
         (project-dir (concat (file-name-as-directory download-dir)
                              (file-name-base url)))
         (default-directory download-dir)
         (command (format "git clone %s" url))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
    (when (file-exists-p project-dir)
      (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
          (delete-directory project-dir t)
        (user-error "Bailed")))
    (switch-to-buffer buffer)
    (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
    (with-current-buffer buffer
      (setq default-directory download-dir)
      (shell-command-save-pos-or-erase)
      (require 'shell)
      (shell-mode)
      (view-mode +1))
    (set-process-sentinel proc (lambda (process state)
                                 (let ((output (with-current-buffer (process-buffer process)
                                                 (buffer-string))))
                                   (kill-buffer (process-buffer process))
                                   (if (= (process-exit-status process) 0)
                                       (progn
                                         (message "finished: %s" command)
                                         (dired project-dir))
                                     (user-error (format "%s\n%s" command output))))))
    (set-process-filter proc #'comint-output-filter)))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  )

;;; Dired
(require 'init-dired)
(require 'init-term)
(require 'init-workspaces)
(require 'init-completion)
(require 'init-lsp)
(require 'init-prog)


;;; Treesitter
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


;;;; Environment Variables


;;; Version Control

   

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

(defun shl/load-config ()
  "Reload Emacs config."
  (interactive)
  (load-file user-init-file)
  (message "Emacs configuration reloaded."))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

;; (require 'init-evil)
(require 'init-org)
(require 'init-ai)

(provide 'init)
;;; init.el ends here


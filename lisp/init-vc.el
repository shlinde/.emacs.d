;;; init-vc.el --- Version Control Setup -*- lexical-binding: t; -*-
;;; Code:

(require 'shl-core)
(require 'init-general)

(use-package transient
  :ensure t)

(use-package vc
  :ensure nil
  :hook (emacs-startup . vc-mode)
  :custom
  (vc-follow-symlinks t)
  (vc-log-short-style '(file)))

(use-package vc-git
  :ensure nil
  :after vc
  :config
  (setq vc-git-diff-switches "--patch-with-stat")
  (setq vc-git-print-log-follow t))

(use-package vc-annotate
  :ensure nil
  :after vc
  :config
  (setq vc-annotate-display-mode 'scale))

(use-package magit
  :ensure t
  :commands
  (magit-blame-mode
   magit-commit
   magit-diff
   magit-log
   magit-status)
  :hook (git-commit-mode . turn-on-flyspell)
  :bind ((:map magit-log-mode-map
          ;; Keybindings for use with updating packages interactively
          ("Q" . #'exit-recursive-edit)))
  :general
  (shl/leader
    "g g" '(magit-dispatch :which-key "magit dispatch")
    "g s" '(magit-status :which-key "magit s"))
  :preface
  (defun shl-display-magit-in-other-window (buffer)
  (if (one-window-p)
      (progn
        (split-window-right)
        (other-window 1)
        (display-buffer buffer
                        '((display-buffer-reuse-window))))
    (magit-display-buffer-traditional buffer)))
  :init
  ;; Suppress the message we get about "Turning on
  ;; magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  :config
  (setq magit-log-margin '(t "%Y-%m-%d.%H:%M:%S "  magit-log-margin-width nil 18))
  (setq magit-refresh-status-buffer t)
  ;; Fine grained diffs
  (setq magit-diff-refine-hunk t)
  ;; control magit initial visibility
  (setq magit-section-initial-visibility-alist
        '((stashes . hide) (untracked . hide) (unpushed . hide) ([unpulled status] . show)))
  (global-git-commit-mode t) ; use emacs as editor for git commits

  ;; refresh status buffer
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  ;; no magit header line as it conflicts w/bespoke-modeline
  (advice-add 'magit-set-header-line-format :override #'ignore)
  ;; display magit setting
  (setq magit-display-buffer-function #'shl-display-magit-in-other-window))

(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))

(defun shl/clone-and-switch (repo-identifier)
  "Clone a Git repository if needed, then switch to it as a project.

The repository can be a full URL or a GitHub \"owner/repo\" shorthand.
It is cloned into `shl--code-reference-dir' if it doesn't
already exist.

Finally, it runs `project-switch-project` on the repository's
local directory."
  (interactive "sSwitch to repo (URL or owner/repo): ")
  (let* ((full-url (if (string-match-p "://" repo-identifier)
                       repo-identifier
                     (format "https://github.com/%s.git" repo-identifier)))
         (repo-name (file-name-sans-extension (file-name-nondirectory full-url)))
         (destination (expand-file-name repo-name shl--code-reference-dir)))

    (if (file-exists-p destination)
        ;; If repo exists, just switch to it.
        (progn
          (message "Repository '%s' already exists. Switching." repo-name)
          (project-switch-project destination))

      ;; If repo does not exist, clone it first.
      (make-directory (file-name-directory destination) t)
      (let ((process-connection-type nil)) ; Use a pipe
        (message "Cloning %s into %s..." repo-name destination)
        (let* ((buffer (generate-new-buffer (format "*cloning %s*" repo-name)))
               (proc (start-process "git-clone" buffer "git" "clone" "--depth=1" full-url destination)))
          (set-process-sentinel
           proc
           (lambda (p e)
             (with-current-buffer (process-buffer p)
               (let ((exit-status (process-exit-status p)))
                 (if (zerop exit-status)
                     (progn
                       (message "Successfully cloned %s. Switching project." repo-name)
                       ;; Switch project on success
                       (project-switch-project destination)
                       (run-with-timer 1 nil #'kill-buffer (current-buffer)))
                   (progn
                     (pop-to-buffer (current-buffer))
                     (message "Failed to clone %s. See buffer %s for details."
                              repo-name (buffer-name)))))))))))))

(defun shl/switch-to-code-reference-project ()
  "Select a project from `shl--code-reference-dir' and switch to it."
  (interactive)
  (let* ((base-dir shl--code-reference-dir)
         (directories (when (file-directory-p base-dir)
                        ;; Get full paths, excluding '.' and '..'
                        (directory-files base-dir t "^[^.]" t))))
    (unless directories
      (user-error "No repositories found in %s" base-dir))

    ;; Create an alist: (("repo-name" . "/full/path/to/repo") ...)
    (let* ((candidates (mapcar (lambda (dir)
                                 (cons (file-name-nondirectory dir) dir))
                               (seq-filter #'file-directory-p directories)))
           (selection (consult--read candidates
                                     :prompt "Switch to project: "
                                     :require-match t)))
      (when selection
        ;; =selection= is the full path (the cdr of the selected pair)
        (project-switch-project selection)))))

(shl/leader
  "g r" '(shl/clone-and-switch :which-key "clone ref repo"))


(use-package diff-hl
  :ensure t
  :hook
  ((prog-mode . diff-hl-mode)
   (text-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)
   (magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)))


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


(provide 'init-vc)
;;; init-vc.el ends here


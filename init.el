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

;;; General
(use-package general
  :ensure (:wait t)
  :demand t
  :config

  ;; Leader (global override map)
  (general-create-definer shl/leader
    :keymaps 'override
    :prefix "C-c"
    :global-prefix "C-c")

  ;; Local leader (per major mode if you want later)
  (general-create-definer shl/local-leader
    :prefix "C-c m")) 

(require 'init-ui)
(require 'init-vc)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c s" "search"
    "C-c b" "buffers"
    "C-c g" "git"
    "C-c c" "code"
    "C-c o" "org"
    "C-c h" "AI"
    "C-c x" "xref"
    "C-c t" "toggle"))

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

;; Which-key
(use-package which-key
  :defer 1
  :ensure t
  :init
  (setq which-key-idle-delay 0.3)
  :config
  (setq which-key-sort-order 'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-max-display-columns 5)
  (which-key-mode))

;;; Project
(use-package project
  :ensure nil
  :commands (project-find-file
             project-switch-to-buffer
             project-switch-project
             project-switch-project-open-file)
  :bind (:map project-prefix-map
         ("P" .  project-switch-project)
         ("t" .  shl-goto-projects)
         ("R" .  project-remember-projects-under))
  :custom
  (project-list-file (concat shl--cache-dir "projects"))
  (project-switch-commands '((project-find-file "Find file")
                             (project-find-regexp "Find regexp")
                             (project-find-dir "Find directory")
                             (project-vc-dir "VC-Dir")
                             (project-magit-dir "Magit status")))
  (project-vc-extra-root-markers '(".dir-locals.el" ".project.el" "package.json" "requirements.txt" "autogen.sh"))
  :config
  ;; Use Ripgrep if installed
  (when (shell-command-to-string "command rg --version")
    (setq xref-search-program 'ripgrep))
  (setq shl-project-dir "~/data/source/")
  ;; remove deleted projects from list
  (project-forget-zombie-projects))

(defun shl--project-name ()
  "Return name of project without path."
  (file-name-nondirectory (directory-file-name (if (vc-root-dir) (vc-root-dir) "-"))))

;; magit function for project
(defun project-magit-dir ()
  "Run magit in the current project's root."
  (interactive)
  (magit-status))
;; Add to keymap
(define-key (current-global-map) (kbd "C-x p G") #'project-magit-dir)

;;;; Open project & file
(with-eval-after-load 'project
  (defun project-switch-project-open-file (dir)
    "Switch to another project by running an Emacs command.
Open file using project-find-file

When called in a program, it will use the project corresponding
to directory DIR."
    (interactive (list (project-prompt-project-dir)))
    (let ((default-directory dir)
          (project-current-inhibit-prompt t))
      (call-interactively 'project-find-file))))

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
(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-jump-other-window)
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-recursive-deletes 'always)
  (dired-recursive-copies  'always)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (add-hook 'dired-mode-hook #'hl-line-mode))

;;;; Minibuffer Completion
(use-package vertico
  :ensure t
  :hook (elpaca-after-init . vertico-mode)
  :custom
  (vertico-resize t)
  (vertico-count 14)
  (vertico-cycle t)
  :config
  (use-package vertico-directory :after vertico)
  (use-package vertico-multiform  :after vertico
    :config
    ;; Example: different style for file prompts
    (setq vertico-multiform-categories
          '((file (vertico-grid))))
    (vertico-multiform-mode 1))
  ;; M-DEL go up, C-l accept directory
  (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
  (define-key vertico-map (kbd "C-l")   #'vertico-directory-enter)
  ;; Repeat last minibuffer session
  (use-package vertico-repeat :after vertico
    :config (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)))

(use-package orderless
  :ensure t
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file    (styles orderless basic partial-completion))
     (eglot   (styles orderless basic))
     (project (styles orderless basic))
     (symbol  (styles orderless basic))
     (capf    (styles orderless basic))))
  :config
  ;; Dispatch helpers:
  ;;  !literal  -> exclude literal
  ;;  word=     -> literal match
  ;;  ~word     -> flex
  (defun shl/orderless-dispatch (pattern _index _total)
    (cond
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1)))
     ((string-prefix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 1)))))
  (setq orderless-style-dispatchers '(shl/orderless-dispatch)
        orderless-component-separator #'orderless-escapable-split-on-space))


(use-package marginalia
  :ensure t
  :hook (elpaca-after-init . marginalia-mode)
  :general
  (:keymaps 'minibuffer-local-map
            "M-A" 'marginalia-cycle))

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)        ;; Jump to a specific character
         ("C-'" . avy-goto-char-2)      ;; Jump to 2-character sequence
	 ("M-j" . avy-goto-char-timer)
         ("M-g g" . avy-goto-line))    ;; Jump to a line
  :config
  (setq avy-background t)              ;; Dim background while jumping
  (setq avy-style 'pre))               ;; Use pre-jump overlay

(use-package consult
  :ensure t
  :after (vertico orderless)
  :general
  (shl/leader
    ;; Search / grep / navigation
    "s r" '(consult-ripgrep        :which-key "ripgrep")
    "s g" '(consult-git-grep       :which-key "git-grep")
    "s f" '(consult-find           :which-key "find")
    "s l" '(consult-line           :which-key "line")
    "s L" '(consult-line-multi     :which-key "multi-line")
    "s m" '(consult-mark           :which-key "marks")
    "s k" '(consult-global-mark    :which-key "global marks")
    "s i" '(consult-imenu          :which-key "imenu")
    "s I" '(consult-imenu-multi    :which-key "imenu multi")
    "s o" '(consult-outline        :which-key "outline")
    ;; Buffers / files
    "b b" '(consult-buffer         :which-key "buffer switch")
    "b B" '(consult-buffer-other-window :which-key "buffer other win")
    "b r" '(consult-recent-file    :which-key "recent file")
    ;; Registers
    "r r" '(consult-register       :which-key "registers")
    "r y" '(consult-yank-pop       :which-key "yank ring")
    ;; Misc
    "x e" '(consult-compile-error  :which-key "compilation errs")
    "x f" '(consult-flymake        :which-key "flymake"))
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.3
        xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref
        consult-narrow-key "<")
  :config
  ;; Optionally highlight current line in minibuffer (visual aid)
  (add-hook 'minibuffer-setup-hook
            (lambda () (when (eq this-command 'consult-line)
			 (hl-line-mode 1))))



  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.25 any)
   consult-ripgrep consult-git-grep consult-grep consult-bookmark
   consult-recent-file consult-xref
   :preview-key '(:debounce 0.4 any)))

;; Optional: quick dir switching inside file prompts
(use-package consult-dir
  :ensure t
  :general
  (shl/leader "f d" '(consult-dir :which-key "dir switch"))
  (:keymaps 'minibuffer-local-completion-map
            "C-x C-d" 'consult-dir
            "C-x C-j" 'consult-dir-jump-file)) 


(use-package embark
  :ensure t
  :demand t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\=\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package corfu
  :ensure t
  :hook (elpaca-after-init . global-corfu-mode)
  :general
  (:keymaps 'corfu-map
	    "C-n"       #'corfu-next
	    "C-p"       #'corfu-previous
	    "C-y"       #'corfu-insert
	    "RET"       nil
	    "<return>"  nil
	    "C-m"       nil
	    "M-n"       #'corfu-popupinfo-scroll-up
	    "M-p"       #'corfu-popupinfo-scroll-down
	    "M-."       #'embark-dwim
	    "C-."       #'embark-act
	    "C-g"       #'corfu-quit
	    "TAB"       #'corfu-insert
	    [tab]       #'corfu-insert)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-scroll-margin 2)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary 'separator)
  (corfu-popupinfo-delay '(0.4 . 0.1))
  :config
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :after corfu
  :config (corfu-terminal-mode 1))

(use-package cape
  :ensure t
  :preface
  (defun shl/maybe-eglot-completion ()
    "Call =eglot-completion-at-point= only when Eglot manages this buffer."
    (when (and (featurep 'eglot)
	       (eglot-managed-p))
      (eglot-completion-at-point)))
  :init
  (setq cape-dabbrev-min-length 3
        cape-dabbrev-check-other-buffers t
        cape-dabbrev-after-symbol nil)
  (defun shl/setup-prog-capfs ()
    (setq-local completion-at-point-functions
                (list
                 (cape-capf-super
		  #'shl/maybe-eglot-completion-at-point
                  #'cape-file
                  #'cape-keyword
                  #'cape-symbol
                  #'cape-abbrev
                  #'cape-dabbrev))))
  (defun shl/setup-text-capfs ()
    (setq-local completion-at-point-functions
                (list
                 (cape-capf-super
                  #'cape-dabbrev
                  #'cape-abbrev
                  #'cape-file
                  #'cape-keyword))))
  :hook
  (prog-mode . shl/setup-prog-capfs)
  (text-mode . shl/setup-text-capfs)
  :general
  (shl/leader
    "c d" '(cape-dabbrev :which-key "dabbrev capf")
    "c f" '(cape-file    :which-key "file capf")
    "c k" '(cape-keyword :which-key "keyword capf")
    "c a" '(cape-abbrev  :which-key "abbrev capf")
    "c s" '(cape-symbol  :which-key "symbol capf")))

(use-package emacs
  :ensure nil
  :config
  (setq read-buffer-completion-ignore-case t
	read-file-name-completion-ignore-case t))

(use-package eglot
  :ensure nil                            ; built-in
  :hook ((python-base-mode . shl/eglot-python-auto)
         (eglot-managed-mode . shl/eglot-managed-setup))
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect 0)
  (eglot-connect-timeout 20)
  (eglot-events-buffer-size 0)           ; don’t keep noisy event buffers
  (eglot-report-progress nil)
  (eglot-send-changes-idle-time 0.2)
  :config
  ;; (Optional) stricter diagnostics ordering (pyright/basedpyright honors some of these)
  (setq eglot-workspace-configuration
        '(:basedpyright
          (:typeCheckingMode "strict"
			     :useLibraryCodeForTypes t
			     :reportMissingTypeStubs "warning"
			     :reportUnknownParameterType "warning"
			     :reportUnknownArgumentType "warning"
			     :reportUnknownVariableType "warning"
			     :reportUnknownMemberType "warning"
			     :reportImplicitStringConcatenation "warning"
			     :disableOrganizeImports nil)))

  ;; Inlay hints ON by default for managed buffers
  (defun shl/eglot-managed-setup ()
    (when (boundp 'eglot-inlay-hints-mode)
      (eglot-inlay-hints-mode 1)))

  ;; --- Dynamic Python server resolution (prefers project .venv + basedpyright) ---
  (defun shl/eglot--python-server-command ()
    "Return a list suitable as Eglot server command for Python.
Priority:
1. .venv/bin/basedpyright-langserver
2. .venv/bin/basedpyright
3. global basedpyright-langserver / basedpyright
4. uv run basedpyright-langserver (fallback if uv exists)
5. pyright-langserver (last resort)
Returns nil if nothing is found."
    (let* ((root (when-let* ((proj (project-current nil))) (project-root proj)))
           (venv (and root (let ((p (expand-file-name ".venv" root)))
                             (when (file-directory-p p) p))))
           (bin  (when venv (expand-file-name (if (eq system-type 'windows-nt) "Scripts" "bin") venv)))
           (cands (append
                   (when bin
                     (mapcar (lambda (n) (expand-file-name n bin))
                             '("basedpyright-langserver" "basedpyright" "pyright-langserver")))
                   '("basedpyright-langserver" "basedpyright" "pyright-langserver")))
           found)
      (setq found (seq-find #'file-executable-p cands))
      (cond
       (found (list found))
       ((and (executable-find "uv") (executable-find "basedpyright-langserver"))
        '("uv" "run" "basedpyright-langserver" "--stdio"))
       ((executable-find "pyright-langserver")
        '("pyright-langserver" "--stdio"))
       ((executable-find "basedpyright")
        '("basedpyright" "--stdio"))
       (t nil))))

  (defun shl/eglot-python-auto ()
    "Ensure Eglot for Python with dynamic server resolution."
    (when (derived-mode-p 'python-base-mode)
      (let ((cmd (shl/eglot--python-server-command)))
        (unless cmd
          (message "Eglot: no Python LSP server found (install basedpyright or pyright)"))
        ;; Update eglot-server-programs entry for Python on the fly (buffer-local override style).
        ;; (let ((entry `((python-mode python-ts-mode) . ,(or cmd '("pyright-langserver" "--stdio")))))
	(let ((entry (cons '(python-mode python-ts-mode)
			   (or cmd '("pyright-langserver" "--stdio")))))
          ;; Avoid duplicate inserts:
          (setq eglot-server-programs
                (cl-delete-if (lambda (pair)
                                (let ((modes (car pair)))
                                  (and (listp modes)
                                       (memq 'python-mode modes))))
                              eglot-server-programs))
          (add-to-list 'eglot-server-programs entry)))
      (eglot-ensure)))

  ;; Convenient helpers
  (defun shl/eglot-restart ()
    "Restart the Eglot server for this buffer."
    (interactive)
    (eglot-reconnect (eglot--current-server-or-lose)))

  (defun shl/eglot-format-buffer ()
    "Format buffer via Eglot (or fallback to indent-region)."
    (interactive)
    (if (eglot-managed-p)
        (eglot-format-buffer)
      (indent-region (point-min) (point-max))))

  (defun shl/eglot-toggle-inlay-hints ()
    (interactive)
    (if (bound-and-true-p eglot-inlay-hints-mode)
        (eglot-inlay-hints-mode 0)
      (eglot-inlay-hints-mode 1))))


(shl/leader
  "l l" '(eglot-ensure              :which-key "start/connect")
  "l R" '(shl/eglot-restart         :which-key "restart")
  "l q" '(eglot-shutdown            :which-key "shutdown")
  "l a" '(eglot-code-actions        :which-key "code actions")
  "l r" '(eglot-rename              :which-key "rename")
  "l f" '(shl/eglot-format-buffer   :which-key "format buf")
  "l h" '(shl/eglot-toggle-inlay-hints :which-key "inlay hints")
  "l d" '(eglot-find-declaration    :which-key "declaration")
  "l D" '(eglot-find-definition     :which-key "definition")
  "l i" '(eglot-find-implementation :which-key "implementation")
  "l t" '(eglot-find-typeDefinition :which-key "type def")
  "l s" '(eglot-shutdown-all        :which-key "shutdown all")
  "l e" '(eglot-events-buffer       :which-key "events buffer")
  "l o" '(eglot-stats               :which-key "stats")
  "l ." '(eglot-code-action-quickfix :which-key "quick fix"))

;;; Workspaces
(use-package tab-bar
  :ensure nil
  :after (project)
  :commands (tab-bar-new-tab
             tab-bar-switch-to-tab
             tab-bar-switch-to-next-tab
             tab-bar-switch-to-prev-tab)
  :custom
  (tab-bar-show 1)
  (tab-bar-tab-hints t) ;; show numbers in tabs
  ;; Unless another file/buffer is designated, start from workspace scratch buffer
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-close-tab-select 'recent)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (tab-bar-tab-name-format-function #'shl--tab-bar-tab-name-format)
  (tab-bar-new-button nil)
  (tab-bar-close-button nil)
  (tab-bar-auto-width nil)
  (tab-bar-format '(tab-bar-format-history
                    tab-bar-format-tabs
                    shl--tab-bar-suffix
                    tab-bar-format-add-tab))
  :config

  ;; https://christiantietze.de/posts/2022/02/emacs-tab-bar-numbered-tabs/
  (defvar shl-tab-bar--circle-numbers-alist
    '((0 . "⓪")
      (1 . "①")
      (2 . "②")
      (3 . "③")
      (4 . "④")
      (5 . "⑤")
      (6 . "⑥")
      (7 . "⑦")
      (8 . "⑧")
      (9 . "⑨")
      (10 . "⑩")
      (11 . "⑪")
      (12 . "⑫")
      (13 . "⑬")
      (14 . "⑭")
      (15 . "⑮"))

    "Alist of integers to strings of circled unicode numbers.")
  (defun shl--tab-bar-tab-name-format (tab i)
    (let ((current-p (eq (car tab) 'current-tab))
          (tab-num (if (and tab-bar-tab-hints (< i 16))
                       (alist-get i shl-tab-bar--circle-numbers-alist) "")))
      (propertize
       (concat
        " "
        tab-num
        (propertize " " 'display '(space :width (4)))
        (alist-get 'name tab)
        (or (and tab-bar-close-button-show
                 (not (eq tab-bar-close-button-show
                          (if current-p 'non-selected 'selected)))
                 tab-bar-close-button)
            "")
        (propertize " " 'display '(space :width (4))))
       'face (funcall tab-bar-tab-face-function tab))))


  ;; See https://github.com/rougier/nano-modeline/issues/33
  (defun shl--tab-bar-suffix ()
    "Add empty space.
This ensures that the last tab's face does not extend to the end
of the tab bar."
    " ")


  ;; https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/
  (defun shl-tab-bar-select-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar-mode' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Otherwise use completion to select the tab."
    (interactive)
    (let ((tabs (mapcar (lambda (tab)
                          (alist-get 'name tab))
                        (tab-bar--tabs-recent))))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (tab-bar-switch-to-tab
              (completing-read "Select tab: " tabs nil t)))))))

;;;; Tab Workspaces
(use-package tabspaces
  :ensure (tabspaces :type git :host github :repo "mclear-tools/tabspaces")
  ;; Add some functions to the project map
  :bind (:map project-prefix-map
         ("p" . tabspaces-open-or-create-project-and-workspace))
  :hook (emacs-startup . tabspaces-mode)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Home")
  :config
  (defun shl--consult-tabspaces ()
    "Deactivate isolated buffers when not using tabspaces."
    (require 'consult)
    (cond (tabspaces-mode
           ;; hide full buffer list (still available with "b")
           (consult-customize consult--source-buffer :hidden t :default nil)
           (add-to-list 'consult-buffer-sources 'consult--source-workspace))
          (t
           (consult-customize consult--source-buffer :hidden nil :default t)
           (setq consult-buffer-sources (remove #'consult--source-workspace consult-buffer-sources)))))
  (add-hook 'tabspaces-mode-hook #'shl--consult-tabspaces))

;;;;; Consult Isolated Workspace Buffers
;; Filter Buffers for Consult-Buffer
(defun shl-buff-filter (buffer)
  (let ((blst (cl-remove (buffer-name) (frame-parameter nil 'buffer-list))))
    (memq buffer blst)))

(with-eval-after-load 'consult
  ;; hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                           :predicate #'tabspaces--local-buffer-p
                           :sort 'visibility
                           :as #'buffer-name)))

    "Set workspace buffer list for consult-buffer."))


;;; Compilation
;; Colorful Compilation
(use-package compile
  :ensure nil
  :config
  (setq compilation-ask-about-save nil
	compilation-scroll-output 'first-error)

  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of emacs.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer)

  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))

  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

  )

;;; Treesitter
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


;;;; Environment Variables
(use-package exec-path-from-shell
  :ensure t
  :hook (emacs-startup . exec-path-from-shell-initialize)
  :init
  (setq exec-path-from-shell-shell-name (executable-find "zsh")
        exec-path-from-shell-arguments '("-l")) ; login so config.fish runs
  :config
  (exec-path-from-shell-copy-envs '("PATH" "MANPATH" "UV_INDEX_YGGDRASIL_USERNAME" "UV_INDEX_YGGDRASIL_PASSWORD"))
  (exec-path-from-shell-initialize))

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
  :bind (("C-c r" . (lambda ()
		      (interactive)
		      (ido-find-file-in-dir "~/data/resources/books/"))))
  :config
  (pdf-tools-install))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

(use-package eat
  :bind ("C-c t t" . eat)
  :ensure (eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

;; (require 'init-evil)
(require 'init-org)
(require 'init-ai)

(provide 'init)
;;; init.el ends here


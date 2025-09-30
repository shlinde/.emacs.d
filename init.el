;;; init.el --- SHLinde Emacs Configuration -*- lexical-binding: t; -*-
;;; Code:

;;; Core
(defconst shl--cache-dir (concat user-emacs-directory ".cache/"))

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;;----------------------------------------------------------------------
;; TIMEOUT: GENERIC DEBOUNCE & THROTTLE
;;----------------------------------------------------------------------
(defun timeout--throttle-advice (&optional timeout)
  "Return a function that throttles its argument function.

THROTTLE defaults to 1.0 seconds. This is intended for use as
function advice."
  (let ((throttle-timer)
        (timeout (or timeout 1.0))
        (result))
    (lambda (orig-fn &rest args)
      "Throttle calls to this function."
      (if (timerp throttle-timer)
          result
        (prog1
            (setq result (apply orig-fn args))
          (setq throttle-timer
                (run-with-timer
                 timeout nil
                 (lambda ()
                   (cancel-timer throttle-timer)
                   (setq throttle-timer nil)))))))))

(defun timeout--debounce-advice (&optional delay default)
  "Return a function that debounces its argument function.

Delay defaults to 0.50 seconds.  DEFAULT is the immediate return
value of the function when called.

This is intended for use as function advice."
  (let ((debounce-timer nil)
        (delay (or delay 0.50)))
    (lambda (orig-fn &rest args)
      "Debounce calls to this function."
      (if (timerp debounce-timer)
          (timer-set-idle-time debounce-timer delay)
        (prog1 default
          (setq debounce-timer
                (run-with-idle-timer
                 delay nil
                 (lambda (buf)
                   (cancel-timer debounce-timer)
                   (setq debounce-timer nil)
                   (with-current-buffer buf
                     (apply orig-fn args)))
                 (current-buffer))))))))

;;;###autoload
(defun timeout-debounce! (func &optional delay default)
  "Debounce FUNC by DELAY seconds.

This advises FUNC, when called (interactively or from code), to
run after DELAY seconds. If FUNC is called again within this time,
the timer is reset.

DELAY defaults to 0.5 seconds. Using a delay of 0 resets the
function.

DEFAULT is the immediate return value of the function when called."
  (if (and delay (= delay 0))
      (advice-remove func 'debounce)
    (advice-add func :around (timeout--debounce-advice delay default)
                '((name . debounce)
                  (depth . -99)))))

;;;###autoload
(defun timeout-throttle! (func &optional throttle)
  "Throttle FUNC by THROTTLE seconds.

This advises FUNC so that it can run no more than once every
THROTTLE seconds.

THROTTLE defaults to 1.0 seconds. Using a throttle of 0 resets the
function."
  (if (= throttle 0)
      (advice-remove func 'throttle)
    (advice-add func :around (timeout--throttle-advice throttle)
                '((name . throttle)
                  (depth . -98)))))

;;; Package Manager
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

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
        modus-themes-to-toggle '(modus-operandi modus-vivendi)
        ;; modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
        ;; modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia)
        ;; modus-themes-to-toggle '(modus-operandi-tritanopia modus-vivendi-tritanopia)
        modus-themes-to-rotate modus-themes-items
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

(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '(("5:00" . modus-operandi)
                           ("19:30" . modus-vivendi)))
  (circadian-setup))

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
           :default-family "Aporetic Serif Mono"
           :default-height 110
	   :default-weight semibold
           :fixed-pitch-family "Aporetic Serif Mono"
           :variable-pitch-family "Aporetic Sans")
          (regular-light
           :default-family "Aporetic Serif Mono"
           :default-height 110
	   :default-weight semilight
           :fixed-pitch-family "Aporetic Serif Mono"
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
  :general
  ("C-x p p" '(project-switch-project :which-key "switch")
   "C-x p f" '(project-find-file      :which-key "find file")
   "C-x p r" '(shl/project-ripgrep    :which-key "ripgrep")
   "C-x p b" '(shl/project-buffers    :which-key "buffers")
   "C-x p B" '(shl/project-switch-to-buffer :which-key "consult buffer")
   "C-x p k" '(shl/project-kill-buffers :which-key "kill bufs")
   "C-x p d" '(shl/project-open-dirvish :which-key "dirvish root")
   "C-x p s" '(shl/project-scratch    :which-key "scratch")
   "C-x p n" '(shl/project-org-note   :which-key "note")
   "C-x p c" '(shl/project-compilation :which-key "compile")
   "C-x p S" '(shl/project-shell      :which-key "shell")
   "C-x p E" '(shl/project-eshell     :which-key "eshell")
   "C-x p j" '(shl/project-just-target :which-key "just target")
   "C-x p t" '(shl/project-pytest-file :which-key "pytest")
   "C-x p l" '(shl/project-pytest-last-failed :which-key "pytest last")
   "C-x p R" '(shl/project-ruff-check :which-key "ruff")
   "C-x p N" '(shl/project-drop-note :which-key "quick note")
   "C-x p o" '(shl/project-chat-browse :which-key "chats"))

  :init
  (defun shl/project-root (&optional proj)
    (expand-file-name (project-root (or proj (project-current t)))))

  (defun shl/project-open-dirvish ()
    (interactive)
    (dirvish (shl/project-root)))

  (defun shl/project-ripgrep (&optional initial)
    (interactive)
    (let ((dir (shl/project-root)))
      (let ((default-directory dir))
	(consult-ripgrep dir initial))))

  (defun shl/project-buffers ()
    (interactive)
    (consult-buffer (project-buffers (project-current t))))

  (defun shl/project-kill-buffers ()
    (interactive)
    (let* ((proj (project-current t))
           (bufs (project-buffers proj)))
	  (when (yes-or-no-p (format "Kill %d project buffers? " (length bufs)))
	    (mapc #'kill-buffer bufs)
	    (message "Killed project buffers."))))

  (defun shl/project-scratch ()
    "Create (or switch to) a project-local scratch buffer."
    (interactive)
    (let* ((root (shl/project-root))
           (name (format "*scratch:%s/" (file-name-nondirectory (directory-file-name root)))))
	  (with-current-buffer (get-buffer-create name)
	    (unless (derived-mode-p 'python-base-mode 'prog-mode 'text-mode 'org-mode)
              (text-mode))
	    (setq default-directory root)
	    (pop-to-buffer (current-buffer)))))

  (defun shl/project-org-note ()
    "Capture a project note into a central Org file with a link."
    (interactive)
    (let* ((root (shl/project-root))
           (title (file-name-nondirectory (directory-file-name root)))
           (file (expand-file-name "project-notes.org" org-directory)))
	  (with-current-buffer (find-file-noselect file)
	    (goto-char (point-max))
	    (unless (bolp) (insert "\n"))
	    (insert (format "/ %s (%s)\n:CREATED: %s\n\n"
			    title root (format-time-string "%F %T")))
	    (save-buffer))
	  (find-file file)
	  (recenter)))

  (defun shl/project-compilation ()
    (interactive)
    (let ((default-directory (shl/project-root)))
      (call-interactively #'compile)))

  (defun shl/project-shell ()
    (interactive)
    (let ((default-directory (shl/project-root)))
      (shell (generate-new-buffer-name (format "*shell:%s/" (file-name-nondirectory (directory-file-name default-directory)))))))

  (defun shl/project-eshell ()
    (interactive)
    (let ((default-directory (shl/project-root)))
      (eshell t)))

  ;; Python oriented helpers
  (defun shl/project-pytest-file ()
    (interactive)
    (let ((default-directory (shl/project-root)))
      (compile "uv run pytest -q")))

  (defun shl/project-pytest-last-failed ()
    (interactive)
    (let ((default-directory (shl/project-root)))
      (compile "uv run pytest -q --last-failed")))

  (defun shl/project-ruff-check ()
    (interactive)
    (let ((default-directory (shl/project-root)))
      (compile "uv run ruff check --fix .")))

  ;; just integration (assumes shl/just-run already defined; fallback)
  (defun shl/project-just-target ()
    (interactive)
    (let ((default-directory (shl/project-root)))
      (if (fboundp 'shl/just-run) (call-interactively #'shl/just-run)
	(message "No shl/just-run function defined."))))

  ;; GPTel chat browsing scoped to project (open chat dir of project if you ever isolate them)
  ;; For now reuse global chat open:
  (defun shl/project-chat-browse ()
    (interactive)
    (shl/gptel-chat-open))

  (defun shl/project-drop-note (text)
    "Append TEXT to a project-local NOTES.org file."
    (interactive "sNote: ")
    (let* ((root (shl/project-root))
           (file (expand-file-name "NOTES.org" root)))
	  (with-temp-buffer
	    (when (file-exists-p file)
	      (insert-file-contents file))
	    (goto-char (point-max))
	    (unless (bolp) (insert "\n"))
	    (insert (format "/ %s\n%s\n" (format-time-string "%F %T") text))
	    (write-region (point-min) (point-max) file))
	  (message "Note added.")))

  (defun shl/project-switch-to-buffer ()
    (interactive)
    (let ((project-current-inhibit-prompt t))
      (consult-buffer (project-buffers (project-current t)))))
  :custom
  ;; Persist list of known projects
  (project-list-file (expand-file-name "projects.eld" user-emacs-directory))
  ;; When switching, offer a command menu
  (project-switch-use-entire-map t)
  ;; Extend auto-discovery (add pyproject + justfile)
  (project-vc-extra-root-markers '("pyproject.toml" "justfile" "Justfile" "requirements.txt"))
  ;; Don’t treat large vendored dirs as part of file indexing
  (project-vc-ignores '("dist" "build" ".mypy_cache" ".ruff_cache" ".pytest_cache" ".direnv" ".venv" "__pycache__"))
  :config
  ;; Optional: prune dead project entries on startup
  ;; (defun shl/project--prune-dead ()
  ;;   (setq project--list
  ;;         (cl-remove-if-not
  ;;          (lambda (entry) (file-directory-p (car entry)))
  ;;          project--list))
  ;;   (project--write-project-list))
  ;; (add-hook 'emacs-startup-hook #'shl/project--prune-dead)

  (setq project-switch-commands
	'((?f "Find file" project-find-file)
          (?d "Dirvish"   shl/project-open-dirvish)
          (?b "Buffers"   shl/project-buffers)
          (?g "Ripgrep"   shl/project-ripgrep)
          (?s "Shell"     shl/project-shell)
          (?e "Eshell"    shl/project-eshell)
          (?c "Compile"   shl/project-compilation)
          (?t "Pytest file" shl/project-pytest-file)
          (?l "Pytest last failed" shl/project-pytest-last-failed)
          (?r "Ruff check" shl/project-ruff-check)
          (?j "just target" shl/project-just-target)
          (?k "Kill buffers" shl/project-kill-buffers)
          (?n "Scratch"   shl/project-scratch)
          (?o "Org note"  shl/project-org-note))))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  )

;;; Dired
(use-package dired
  :ensure nil
  :commands (dired)
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-recursive-deletes 'always)
  (dired-recursive-copies  'always)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (add-hook 'dired-mode-hook #'hl-line-mode))

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)          ; replace dired everywhere

  (defun shl/dirvish-preview-active-p ()
    "Return non-nil if a Dirvish preview window is currently shown."
    (when (fboundp 'dirvish--get-preview-window)
      (ignore-errors
	(let* ((dv (and (fboundp 'dirvish-curr) (dirvish-curr)))
               (win (and dv (dirvish--get-preview-window dv))))
              (and win (window-live-p win))))))
  (defun shl/dirvish-ensure-preview ()
    (when (and (derived-mode-p 'dired-mode) (not (shl/dirvish-preview-active-p)))
      (dirvish-toggle-preview)))
  :general
  (:keymaps 'dirvish-mode-map
	    ;; Navigation / structure
	    "TAB"   #'dirvish-subtree-toggle
	    "M-RET" #'dirvish-subtree-toggle
	    "h"     #'dirvish-history-jump
	    "H"     #'dirvish-history-go-back
	    "L"     #'dirvish-history-go-forward
	    ;; Preview / info
	    "SPC"   #'dirvish-toggle-preview
	    "f"     #'dirvish-file-info-menu
	    "y"     #'dirvish-yank-menu
	    "s"     #'dirvish-quicksort
	    "n"     #'dirvish-narrow
	    ;; Search (Consult)
	    "/"     (lambda () (interactive)
		      (consult-ripgrep (dired-current-directory)))
	    ;; Utilities
	    "?"     #'dirvish-dispatch
	    "q"     #'dirvish-quit
	    "!"     #'async-shell-command
	    "C-c C-e" #'wdired-change-to-wdired-mode)

  :custom
  (dirvish-use-header-line t)
  (dirvish-reuse-session t)
  (dirvish-hide-details t)
  (dirvish-subtree-always-show-state t)
  (dirvish-cache-dir (expand-file-name "dirvish-cache/" user-emacs-directory))
  ;; Attributes shown in listing (adjust to taste)
  (dirvish-attributes '(subtree-state all-the-icons file-time file-size git-msg))
  ;; What preview handlers to try (order matters)
  (dirvish-preview-dispatchers '(image gif pdf epub archive code text))
  (dirvish-mode-line-format '(:left (path) :right (omit yank index)))
  :config
  (add-hook 'dirvish-directory-view-mode-hook #'hl-line-mode)
  ;; Optional: faster navigation feel
  (setq mouse-1-click-follows-link nil)
  :config

  (defun shl/dirvish-project-root ()
    "Open current project root in Dirvish."
    (interactive)
    (if-let ((proj (project-current)))
	(dirvish (project-root proj))
      (user-error "No project found")))
  (shl/leader "f d" '(shl/dirvish-project-root :which-key "project dir"))

  :config
  (when (not (require 'all-the-icons nil t))
    (setq dirvish-attributes (remove 'all-the-icons dirvish-attributes))))


;; Optional: all-the-icons (guard if not installed)


;; Helpful extra packages (optional):
(use-package dirvish-extras :after dirvish :ensure nil)

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
  :defer
  :bind
  :custom-face
  (tab-bar-tab ((t (:inherit font-lock-function-name-face))))
  :config
  (tab-bar-history-mode 1)
  (defun tab-bar-format-menu-bar ()
    "Produce the Menu button for the tab bar that shows the menu bar."
    `((menu-bar menu-item (propertize " 𝝺 " 'face 'tab-bar-tab-inactive)
                tab-bar-menu-bar :help "Menu Bar")))
  (defun my/tab-bar-tab-name-format-comfortable (tab i)
    (propertize (concat " " (tab-bar-tab-name-format-default tab i) " ")
                'face (funcall tab-bar-tab-face-function tab)))
  (setq tab-bar-tab-name-format-function #'my/tab-bar-tab-name-format-comfortable)
  
  ;; Make items in tab-bar-format-global clickable
  (define-advice tab-bar-format-global (:override () with-buttons)
    (let ((strings (split-string (format-mode-line global-mode-string))))
      (mapcan (lambda (s)
                (list
                 `(sep menu-item ,(tab-bar-separator) ignore)
                 `(global menu-item ,s tab-bar-format-global-action)))
              strings)))
  
  (defun tab-bar-format-global-action (event)
    (interactive "e")
    (let ((posn (event-start event)))
      (when-let* ((str (posn-string posn))
                  (str-button (get-text-property (cdr str) 'button (car str))))
	(button-activate str t))))
  
  (setq tab-bar-format '(tab-bar-format-menu-bar
                         ;; tab-bar-format-history
                         tab-bar-format-tabs
                         tab-bar-separator
                         tab-bar-format-add-tab
                         tab-bar-format-align-right
                         tab-bar-format-global)
        tab-bar-close-button-show nil)

  (add-variable-watcher
   'tab-bar-show
   (defun my/tab-bar-show--handle-global-mode-string
       (sym newval op _buf)
     (when (eq op 'set)
       (if newval
           (progn (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
                  (tab-bar--define-keys))
         (set 'tab-bar-format (delq 'tab-bar-format-global tab-bar-format))))))

  (defun my/tab-bar-name ()
    "Use project as tab name."
    (let ((dir (expand-file-name
                ;; (or (if (fboundp 'project-root)
                ;;         (project-root (project-current)))
                ;;     default-directory)
                default-directory
                )))
      (or
       (and dir
            (let ((name (file-name-nondirectory (substring dir 0 -1))))
              ;; (substring dir (1+ (string-match "/[^/]+/$" dir)) -1)
              (truncate-string-to-width name tab-bar-tab-name-truncated-max nil ? )))
       (buffer-name))))
  (timeout-throttle! #'my/tab-bar-name 0.6)
  
  (setq  tab-bar-close-last-tab-choice 'tab-bar-mode-disable
         tab-bar-show                   '0 ;; (when (version< "28.0" emacs-version) 1)
         tab-bar-tab-name-truncated-max 24
         tab-bar-new-tab-choice        "*scratch*"
         tab-bar-tab-name-function #'tab-bar-tab-name-current-with-count)

  (setq tab-bar-select-tab-modifiers '(meta hyper))

  (defun my/tab-bar-show-hide-tabs ()
    "Show or hide tabs."
    (interactive)
    (setq tab-bar-show (if tab-bar-show nil 1))))

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
(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)         ;; Open Magit status buffer
         ("C-x M-g" . magit-dispatch))   ;; Open Magit's dispatcher
  :config
  ;; Ensure Magit doesn't open too many windows by default
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))

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

(require 'init-evil)
(require 'init-org)
(require 'init-ai)

(provide 'init)
;;; init.el ends here


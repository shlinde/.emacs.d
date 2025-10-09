;;; init.el --- SHLinde Emacs Configuration -*- lexical-binding: t; -*-
;;; Code:

;;; Core
(defconst shl--cache-dir (concat user-emacs-directory ".cache/"))

(defconst shl--code-reference-dir
  (expand-file-name "~/data/resources/code-reference/")
  "Directory for cloned reference code repositories.")

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

(use-package zenburn-theme
  :ensure t)

(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '(("5:00" . kaolin-aurora)
                           ("19:30" . kaolin-aurora)))
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
  :init
  (defun shl/setup-project-exclusions ()
    "Exclude the code reference directory from known projects."
    (let ((excluded-path (expand-file-name shl--code-reference-dir)))
      ;; Ensure the path ends with a slash for precise matching
      (unless (string-suffix-p "/" excluded-path)
	(setq excluded-path (concat excluded-path "/")))
      ;; Add the path to the exclusion list, avoiding duplicates.
      (add-to-list 'project-list-exclude
                   (regexp-quote (concat excluded-path "*")))))

;; Call the function after project.el is loaded.
  :custom
  ;; Persist list of known projects
  (project-list-file (expand-file-name "projects.eld" user-emacs-directory))
  ;; When switching, offer a command menu
  (project-switch-use-entire-map t)
  ;; Extend auto-discovery (add pyproject + justfile)
  (project-vc-extra-root-markers '("pyproject.toml" "justfile" "Justfile" "requirements.txt"))
  ;; Don’t treat large vendored dirs as part of file indexing
  (project-vc-ignores '("dist" "build" ".mypy_cache" ".ruff_cache" ".pytest_cache" ".direnv" ".venv" "__pycache__")))


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
(use-package transient
  :ensure t)

(use-package magit
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
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-side 'left)
  (diff-hl-fringe-bmp-function 'shl--diff-hl-fringe-bmp-from-type)
  (diff-hl-fringe-face-function 'shl--diff-hl-fringe-face-from-type)
  (diff-hl-margin-symbols-alist
   '((insert . "┃")
     (delete . "┃")
     (change . "┃")
     (unknown . "?")
     (ignored . "i")))
  :init
  (defun shl--diff-hl-fringe-face-from-type (type _pos)
    (intern (format "shl--diff-hl-%s" type)))

  (defun shl--diff-hl-fringe-bmp-from-type(type _pos)
    (intern (format "shl--diff-hl-%s" type)))

  (defun shl--diff-hl-set-render-mode ()
    (diff-hl-margin-mode (if window-system -1 1)))
  :config
  (diff-hl-margin-mode 1)
  (define-fringe-bitmap 'diff-hl-insert
    [#b00000011] nil nil '(center repeated))
  (define-fringe-bitmap 'diff-hl-change
    [#b00000011] nil nil '(center repeated))
  (define-fringe-bitmap 'diff-hl-delete
    [#b00000011] nil nil '(center repeated)))
  

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


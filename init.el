;;; init.el --- SHLinde Emacs Configuration -*- lexical-binding: t; -*-
;;; Code:

;;; Core
(defconst shl--cache-dir (concat user-emacs-directory ".cache/"))

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

DELAY defaults to 0.50 seconds.  DEFAULT is the immediate return
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
  :bind (("C-c f" . fontaine-set-preset)
         ("C-c F" . fontaine-toggle-preset))
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
           :variable-pitch-family "Aporetic Sans")
          (medium-dark
           :default-family "Aporetic Serif Mono"
           :default-height 115
	   :default-weight semibold
           :fixed-pitch-family "Aporetic Serif Mono"
           :variable-pitch-family "Aporetic Sans")
          (medium-light
           :default-family "Aporetic Serif Mono"
           :default-height 115
	   :default-weight semilight
           :fixed-pitch-family "Aporetic Serif Mono"
           :variable-pitch-family "Aporetic Sans")
          (large
           :default-height 150)
          (presentation
           :default-height 180)
          (jumbo
           :inherit medium
           :default-height 260))))


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

;;;; Minibuffer Completion
(use-package vertico
  :ensure t
  :hook (elpaca-after-init . vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)) ;; Cycle annotation styles
  :init
  (marginalia-mode +1))                   ;; Enable marginalia globally  

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)        ;; Jump to a specific character
         ("C-'" . avy-goto-char-2)      ;; Jump to 2-character sequence
	 ("M-j" . avy-goto-char-timer)
         ("M-g g" . avy-goto-line))    ;; Jump to a line
  :config
  (setq avy-background t)              ;; Dim background while jumping
  (setq avy-style 'pre))               ;; Use pre-jump overlay

;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
	 ("C-x C-r" . consult-recent-file)  ;; Removes open read only 
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

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
  :hook (elpaca-after-init . exec-path-from-shell-initialize)
  :config (exec-path-from-shell-copy-env "ANTROPIC_API_KEY"))

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

;;; Languages
;;;; Python
(defun uv-activate ()
  "Activate Python environment managed by uv based on current project directory.
Looks for .venv directory in project root and activates the Python interpreter."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (venv-path (expand-file-name ".venv" project-root))
         (python-path (expand-file-name
                       (if (eq system-type 'windows-nt)
                           "Scripts/python.exe"
                         "bin/python")
                       venv-path)))
    (if (file-exists-p python-path)
        (progn
          ;; Set Python interpreter path
          (setq python-shell-interpreter python-path)

          ;; Update exec-path to include the venv's bin directory
          (let ((venv-bin-dir (file-name-directory python-path)))
            (setq exec-path (cons venv-bin-dir
                                  (remove venv-bin-dir exec-path))))

          ;; Update PATH environment variable
          (setenv "PATH" (concat (file-name-directory python-path)
                                 path-separator
                                 (getenv "PATH")))

          ;; Update VIRTUAL_ENV environment variable
          (setenv "VIRTUAL_ENV" venv-path)

          ;; Remove PYTHONHOME if it exists
          (setenv "PYTHONHOME" nil)

          (message "Activated UV Python environment at %s" venv-path))
      (error "No UV Python environment found in %s" project-root))))

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

(use-package eat
  :bind ("C-c t t" . eat)
  :ensure t)


(defun shl/org-open-file-from-property ()
  "Find and open the file specified in the \"file\" property of the current Org heading.

The function searches for a property like:
:PROPERTIES:
:file: /path/to/your/file.txt
:END:

and opens the specified file."
  (interactive)
  ;; We need to be in org-mode to use this function
  (unless (derived-mode-p 'org-mode)
    (error "This command can only be run from an Org mode buffer"))

  (let* (;; org-entry-get will get a property from the current headline.
         ;; The "t" argument tells it to search inherited properties as well.
         (file-path (org-entry-get (point) "file-path" t)))

    ;; Check if the file-path was found and is not an empty string
    (if (and file-path (not (string-empty-p file-path)))
        ;; If a valid path is found, expand it (to handle ~/) and open it
        (find-file (expand-file-name file-path))
      ;; If no "file" property is found, inform the user.
      (message "No \":file:\" property found at the current heading."))))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c r") #'shl/org-open-file-from-property))

;;; Org-mode (personal information manager)
(use-package org
  :ensure nil
  :init
  (setq org-directory (expand-file-name "~/data/org/"))
  (setq org-imenu-depth 7)

  (add-to-list 'safe-local-variable-values '(org-hide-leading-stars . t))
  (add-to-list 'safe-local-variable-values '(org-hide-macro-markers . t))
  :bind
  (:map global-map
	("C-c l" . org-store-link)
	("C-c o" . org-open-at-point-global)
	:map org-mode-map
	("C-c M-l" . org-insert-last-stored-link)
	("C-c C-M-l" . org-toggle-link-display)
	("M-." . org-edit-special) ; alias for C-c ' (mnenomic is global M-. that goes to source)
	:map org-src-mode-map
	("M-," . org-edit-src-exit) ; see M-. above
	:map narrow-map
	("b" . org-narrow-to-block)
	("e" . org-narrow-to-element)
	("s" . org-narrow-to-subtree))
  :config
  (setq org-special-ctrl-a/e nil)
  (setq org-special-ctrl-k nil)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-hide-emphasis-markers nil)
  (setq org-hide-macro-markers nil)
  (setq org-hide-leading-stars nil)
  (setq org-cycle-separator-lines 0)
  (setq org-structure-template-alist
        '(("s" . "src")
          ("e" . "src emacs-lisp")
          ("E" . "src emacs-lisp :results value code :lexical t")
          ("t" . "src emacs-lisp :tangle FILENAME")
          ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
          ("x" . "example")
          ("X" . "export")
          ("q" . "quote")))
  (setq org-fold-catch-invisible-edits 'show)
  (setq org-return-follows-link nil)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-modules '(ol-info ol-eww))
  (setq org-use-sub-superscripts '{})
  (setq org-insert-heading-respect-content t)
  (setq org-read-date-prefer-future 'time)
  (setq org-highlight-latex-and-related nil) ; other options affect elisp regexp in src blocks
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-block-delimiter-line t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)
  (setq org-priority-faces nil))


;;;; `org-indent-mode' and related
(use-package org
  :ensure nil
  :hook (org . org-indent-mode)
  :config
  (setq org-adapt-indentation nil) ; No, non, nein, όχι to literal indentation!
  (setq org-indent-mode-turns-on-hiding-stars nil)
  (setq org-indent-indentation-per-level 2))

;;;; refile, todo
(use-package org
  :ensure nil
  :config
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 2))
          (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t)
  (setq org-reverse-note-order nil)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "MAYBE(m)" "|" "CANCELED(c@)" "DONE(d!)")
	  (sequence "NOTE(n)")
	  (sequence "READ(r) | COMPLETED(C!)")
	  (sequence "APPOINTMENT(a)")))

  (defface shl/org-todo-alternative
    '((t :inherit (italic org-todo)))
    "Face for alternative TODO-type Org keywords.")

  (defface shl/org-done-alternative
    '((t :inherit (italic org-done)))
    "Face for alternative DONE-type Org keywords.")

  (setq org-todo-keyword-faces
        '(("MAYBE" . shl/org-todo-alternative)
          ("CANCELED" . shl/org-done-alternative)))

  (defface shl/org-tag-coaching
    '((default :inherit unspecified :weight regular :slant normal)
      (((class color) (min-colors 88) (background light))
       :foreground "#004476")
      (((class color) (min-colors 88) (background dark))
       :foreground "#c0d0ef")
      (t :foreground "cyan"))
    "Face for coaching Org tag.")

  (defface shl/org-tag-shlasks
    '((default :inherit unspecified :weight regular :slant normal)
      (((class color) (min-colors 88) (background light))
       :foreground "#603f00")
      (((class color) (min-colors 88) (background dark))
       :foreground "#deba66")
      (t :foreground "yellow"))
    "Face for shlasks Org tag.")

  (setq org-tag-faces
        '(("coaching" . shl/org-tag-coaching)
          ("shlasks" . shl/org-tag-shlasks)))

  (setq org-use-fast-todo-selection 'expert)

  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-fontify-whole-heading-line nil)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t))

(use-package org
  :ensure nil
  :bind (("C-c c" . org-capture)
	 ("C-c j" . open-journal)
	 ("C-c a" . org-agenda))
  :init
  (defun open-journal ()
    "Open the journal.org file in ~/data/org/ directory."
    (interactive)
    (find-file "~/data/org/journal.org"))
  :config
  (setq org-capture-templates
	(quote (("j" "Journal" entry (file "~/data/org/journal.org")
		 "* %^{Title} :JOURNAL:\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
		 :prepend t
		 :empty-lines 1)
		("a" "Appointment" entry (file "~/data/org/journal.org")
		 "* APPOINTMENT %^{Title} :APPOINTMENT:\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
		 :prepend t
		 :empty-lines 1)
		("t" "Todo" entry (file "~/data/org/journal.org")
		 "* TODO %^{Title} :TODO:\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
		 :prepend t
		 :empty-lines 1))))

  (setq org-agenda-files '("~/data/org/journal.org")
	org-log-into-drawer t))


(use-package org-modern
  :ensure t
  :hook (elpaca-after-init . global-org-modern-mode))

;;; AI
(use-package gptel
  :ensure (:url "https://github.com/karthink/gptel") ; For Emacs>=30
  :config 
  (setq gptel-backend (gptel-make-gh-copilot "Copilot")
	gptel-default-mode 'org-mode)
  (gptel-make-tool
   :name "read_buffer"                    ; javascript-style snake_case name
   :function (lambda (buffer)                  ; the function that will run
               (unless (buffer-live-p (get-buffer buffer))
		 (error "error: buffer %s is not live." buffer))
               (with-current-buffer  buffer
		 (buffer-substring-no-properties (point-min) (point-max))))
   :description "return the contents of an emacs buffer"
   :args (list '(:name "buffer"
		       :type string            ; :type value must be a symbol
		       :description "the name of the buffer whose contents are to be retrieved"
   :category "emacs"))))                     ; An arbitrary label for grouping


(use-package mcp
  :ensure t
  :after gptel
  :custom (mcp-hub-servers
           `(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/home/lizqwer/MyProject/")))
             ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
	     ("ddg-search" . (:command "uvx" :args ("duckduckgo-mcp-server")))
             ("context7" . (:command "npx" :args ("-y" "@upstash/context7-mcp@latest")))
             ("sequential-thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
             ("qdrant" . (:url "http://localhost:8000/sse"))))
  :config
  (require 'mcp-hub)
  (require 'gptel-integrations))

;; (require 'init-evil)

(provide 'init)
;;; init.el ends here

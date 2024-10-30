;;; init.el --- Emacs Config of Sebastian Hempel Linde -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core
(defvar shl/missing-packages
  "Packages not able to install.")

(defvar shl/is-wsl
  (when (string-match "-[Mm]icrosoft" operating-system-release)
  "Is emacs running in WSL?"))

(defvar shl/package-contents-refreshed nil)

(defun shl/package-refresh-contents-once ()
  (when (not shl/package-contents-refreshed)
    (setq shl/package-contents-refreshed t)
    (package-refresh-contents)))

(defmacro shl/pkg-config (name &rest config)
  (declare (indent 1))
  `(if (not (package-installed-p ,name))
       (condition-case err
	   (progn (shl/package-refresh-contents-once)
		  (package-install ,name))
	 (error
	  (message "Couldn't install optional package `%s': %S" ,name err)
	  (push ,name shl/missing-packages)))
     ,@config))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance

;; Disable unwanted UI elements
(dolist (mode '(tool-bar-mode scroll-bar-mode 
			      horizontal-scroll-bar-mode menu-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; Font setup
(shl/pkg-config 'fontaine
  (add-hook 'after-init-hook #'fontaine-mode)
  (add-hook 'after-init-hook (lambda ()
			      (fontaine-set-preset 'regular-dark)))
   (setopt x-underline-at-descent-line nil)
   (setq fontaine-presets
         '((small
            :default-family "Iosevka Comfy Motion"
            :default-height 80
            :variable-pitch-family "Iosevka Comfy Duo")
           (regular-dark
            :default-family "Iosevka Comfy"
            :variable-pitch-family "Iosevka Comfy Duo"
            :default-weight medium) ; like this it uses all the fallback values and is named `regular'
           (regular-light 
            :default-weight semilight) ; like this it uses all the fallback values and is named `regular'
           (medium-light
            :default-weight semilight
            :default-height 115)
           (medium-dark
            :default-weight medium
            :default-height 115
            :bold-weight extrabold))))

;; Theme setup
(shl/pkg-config 'modus-themes
  (require 'modus-themes)

  (setopt modus-themes-custom-auto-relload nil
	  modus-themes-to-toggle '(modus-vivendi modus-operandi)
	  modus-themes-mixed-fonts t
	  modus-themes-italic-constructs t
	  modus-themes-bold-constructs t
	  modus-themes-completions '((t . (extrabold)))
	  modus-themes-prompts '(extrabold)
	  modus-themes-common-palette-overrides nil)
  (modus-themes-load-theme (car modus-themes-to-toggle)))

;; Line numbers
(setopt display-line-numbers-width 3
        display-line-numbers-type 'relative)
(add-hook 'org-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Display fill-column
(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left
		fill-column 100
                display-fill-column-indicator-character ?┊)
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode))

;; Speed up font rendering for special characters
;; @see https://www.reddit.com/r/emacs/comments/988paa/emacs_on_windows_seems_lagging/
(setq inhibit-compacting-font-caches t)

;; Modeline
(add-hook 'after-init-hook #'display-time-mode)
(add-hook 'after-init-hook #'display-battery-mode)

(unless shl/is-wsl
  (setq default-frame-alist '((undecorated . t))))

;; GUI Frames
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defaults

;; Setup PATH to follow system
(shl/pkg-config 'exec-path-from-shell
  (when (or (memq window-system '(mac ns x pgtk))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
    (exec-path-from-shell-initialize)))

;; Don't clutter directories with backups
(let ((backup-dir "~/.cache/tmp/emacs/backups")
      (auto-saves-dir "~/.cache/tmp/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))


(setopt backup-by-copying t    ; Don't delink hardlinks
	delete-old-versions t  ; Clean up the backups
	version-control t      ; Use version numbers on backups,
	kept-new-versions 5    ; keep some new versions
	kept-old-versions 2)   ; and some old ones, too

;; Turn on savehist mode
(savehist-mode 1)

;; Settings
(setopt ring-bell-function 'ignore
	custom-file (concat user-emacs-directory "custom.el")
        bookmark-default-file (locate-user-emacs-file ".bookmarks.el")  ;; Hide bookmarks.el, to not clutter user-emacs-dir
        use-short-answers t  ;; Use y and n instead of yes and no.
	buffer-menu-max-size 30
        inhibit-splash-screen t
	case-fold-search t  ;; Ignore case while searching
	column-number-mode t  ;; Show column number in modeline
	indent-tabs-mode nil  ;; Ensure that all indentation is with spaces
	create-lockfiles nil  ;; Don't clutter directories with lock files
	auto-save-default nil ;; Don't autosave buffers
	make-backup-files nil  ;; Don't make backups
	vc-make-backup-files nil  ;; Don't make backups of version controlled files
	save-interprogram-paste-before-kill t  ;; Save existing clipboard text into kill ring before replacing.
	scroll-preserve-screen-position 'always  ;; Ensure that scrolling does not move point
        truncate-lines nil ;; Truncate lines when wider than buffer-width
        truncate-partial-width-windows nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Experience
(shl/pkg-config 'vertico
  (setopt vertico-scroll-margin 0
	  vertico-count 5
	  vertico-resize t
	  vertico-cycle t)
  (vertico-mode))

(shl/pkg-config 'embark
  (global-set-key (kbd "C-.") 'embark-act)
  (global-set-key (kbd "M-.") 'embark-dwim))

(shl/pkg-config 'consult
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  (global-set-key [remap goto-line] 'consult-goto-line))

(shl/pkg-config 'embark-consult
  (with-eval-after-load 'embark
    (require 'embark-consult)
    (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)))

(shl/pkg-config 'marginalia
  (setopt marginalia-max-relative-age 0) ;; Use absolute time
  (marginalia-mode))

(shl/pkg-config 'which-key
  (add-hook 'after-init-hook 'which-key-mode)
  (setq-default which-key-idle-delay 0.3))

(shl/pkg-config 'orderless
  (setq completion-category-defaults nil
        completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless)))
        completion-cycle-threshold 4)

  (with-eval-after-load 'vertico
    (require 'orderless)
    (setq completion-styles '(orderless basic))))

;; Corfu
(shl/pkg-config 'corfu
  (add-hook 'after-init-hook #'global-corfu-mode)

  (setq global-corfu-modes '((not erc-mode
                                  circe-mode
                                  help-mode
                                  gud-mode
                                  eat-mode
                                  inferior-python-mode)
                             t))

  (setopt corfu-auto t
	  corfu-auto-delay 0.1
	  corfu-auto-prefix 1
	  corfu-cycle t
	  corfu-separator ?\s
	  corfu-preselect 'first
	  corfu-count 16
	  corfu-max-width 120
	  corfu-preview-current nil
	  corfu-on-exact-match nil
	  corfu-quit-at-boundary 'separator
	  corfu-quit-no-match 'separator)
  (setq text-mode-ispell-word-completion nil)

  (with-eval-after-load 'corfu
    (require 'corfu-history)
    (add-hook 'corfu-mode-hook 'corfu-history-mode)

    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'corfu-history)))

  ;; Rebinding keys
  (with-eval-after-load 'corfu
    (define-key corfu-map (kbd "C-y") 'corfu-insert)
    (define-key corfu-map (kbd "RET") nil))

  ;; Popopinfo
  (with-eval-after-load 'corfu
    (require 'corfu-popupinfo)
    (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
    (setq corfu-popupinfo-delay '(0.5 . 1.0))))

;; Ensure that opening parentheses are paired with closing
(add-hook 'prog-mode-hook #'electric-pair-mode)
(add-hook 'prog-mode-hook #'electric-indent-mode)

;; Subword-mode enables moving in CamelCase and snake_case
(global-subword-mode)

;; Delete selection
(add-hook 'after-init-hook #'delete-selection-mode)

;; Expand Region makes for a nicer way to mark stuff
(shl/pkg-config 'expand-region
  (global-set-key (kbd "M-h") #'er/expand-region))

;; Hippie Expand instead of dabbrev
(global-set-key [remap dabbre-expand] 'hippie-expand)

;; Compilation
(defun shl/colorize-compilation-buffer ()
  (read-only-mode 'toggle)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode 'toggle))
(add-hook 'compilation-filter-hook 'shl/colorize-compilation-buffer)

;; Magit
(shl/pkg-config 'magit
  (global-set-key (kbd "C-x g") #'magit-status))

(require 'treesit)
(when (treesit-available-p)
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (mapc #'treesit-install-language-grammar
        (mapcar #'car treesit-language-source-alist))

  (setq major-mode-ramap-alist
        '((yaml-mode . yaml-ts-mode)
          (toml-mode . toml-ts-mode)
          (bash-mode . bash-ts-mode)
          (python-mode . python-ts-mode))))

;; Function to clone a git repository from a URL into the code-reference folder
(require 'cl-lib)
(defun shl/git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in dired when finished."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (download-dir (expand-file-name "~/data/resources/code-reference/"))
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

;; Eshell
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          #'(lambda()
              (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(shl/pkg-config 'esh-autosuggest
  (add-hook 'eshell-mode-hook #'esh-autosuggest-mode))

(defun shl/create-project (project-name)
  (interactive "sProject name: ")
  (make-directory (concat "~/data/source/" project-name))
  (magit-init (concat "~/data/source/" project-name))
  (dired (concat "~/data/source/" project-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org and Friends
(require 'calendar)
(setopt calendar-mark-diary-entries-flag nil
        calendar-mark-holidays-flag t
        calendar-week-start-day 1
        calendar-date-style 'iso
        calendar-time-zone-style 'numeric)


;; (shl/pkg-config 'org
;;   (global-set-key (kbd "C-c l") #'org-store-link)
;;   (global-set-key (kbd "C-c a") #'org-agenda)
;;   (global-set-key (kbd "C-c c") #'org-capture)

;;   (defun shl/open-default-notes-file)

;;   (defalias 'shl/org-mode-map
;;     (let ((map (make-sparse-keymap)))
;;       (define-key map (kbd "n") #'shl/open-default-notes-file)
;;       map)
;;     "SHL Org")

;;   (keymap-set global-map "C-c n" shl/org-mode-map)

;;   (setopt org-directory (expand-file-name "~/data/org/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Support

(shl/pkg-config 'zig-mode)

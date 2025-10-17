;;; init-evil.el --- Vim Movements -*- lexical-binding: t; -*-
;;; Code:

(require 'init-general)

(defvar shl/evil-leader-key "SPC"
  "The leader prefix for Evil.")

(defvar shl/evil-local-leader-key "SPC m"
  "The leader prefix for Evil.")

(use-package general
  :ensure nil
  :config
  (general-evil-setup t)

  (general-create-definer shl/evil-leader
    :prefix shl/evil-leader-key)

  (general-create-definer shl/evil-local-leader
    :prefix shl/evil-local-leader-key))


(use-package evil
  :ensure t
  :hook ((text-mode prog-mode) . evil-mode)
  :preface

  (defun shl/save-and-kill-buffer ()
    "Save the current buffer to file, then kill it."
    (interactive)
    (save-buffer)
    (kill-current-buffer))

  (general-setq evil-want-keybinding nil
		evil-want-integration t
		evil-ex-search-vim-style-regexp t
		evil-ex-visual-char-range t  ; column range for ex commands
		evil-mode-line-format 'nil
		;; more vim-like behavior
		evil-symbol-word-search t
		;; if the current state is obvious from the cursor's color/shape, then
		;; we won't need superfluous indicators to do it instead.
		evil-default-cursor '+evil-default-cursor-fn
		evil-normal-state-cursor 'box
		evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn)
		evil-insert-state-cursor 'bar
		evil-visual-state-cursor 'hollow
		;; Only do highlighting in selected window so that Emacs has less work
		;; to do highlighting them all.
		evil-ex-interactive-search-highlight 'selected-window
		;; It's infuriating that innocuous "beginning of line" or "end of line"
		;; errors will abort macros, so suppress them:
		evil-kbd-macro-suppress-motion-error t)
  ;; evil-undo-system
  ;; (cond ((modulep! :emacs undo +tree) 'undo-tree)
  ;;       ((modulep! :emacs undo) 'undo-fu)
  ;;       ((> emacs-major-version 27) 'undo-redo)))

  :config
  (setq evil-visual-update-x-selection-p nil)

  (general-define-key
   :states 'insert
   "C-g" 'evil-normal-state
   "C-h" 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (general-define-key
   :states 'motion
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line)

  ;; Ensure that :q works as I expect
  (evil-ex-define-cmd "q" 'kill-current-buffer)
  (evil-ex-define-cmd "wq" 'shl/save-and-kill-buffer)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :ensure t
  :general
  (general-t 'normal 'override "c" #'evilnc-comment-or-uncomment-lines)
  ("C-/" #'evilnc-comment-or-uncomment-lines)
  ('normal "gc" #'evilnc-comment-operator))

(shl/evil-leader
  :states 'normal
  "p p" '(project-switch-project :which-key "switch")
  "p f" '(project-find-file      :which-key "find file")
  "p r" '(shl/project-ripgrep    :which-key "ripgrep")
  "p b" '(shl/project-buffers    :which-key "buffers")
  "p B" '(shl/project-switch-to-buffer :which-key "consult buffer")
  "p k" '(shl/project-kill-buffers :which-key "kill bufs")
  "p d" '(shl/project-open-dirvish :which-key "dirvish root")
  "p s" '(shl/project-scratch    :which-key "scratch")
  "p n" '(shl/project-org-note   :which-key "note")
  "p c" '(shl/project-compilation :which-key "compile")
  "p S" '(shl/project-shell      :which-key "shell")
  "p E" '(shl/project-eshell     :which-key "eshell")
  "p j" '(shl/project-just-target :which-key "just target")
  "p t" '(shl/project-pytest-file :which-key "pytest")
  "p l" '(shl/project-pytest-last-failed :which-key "pytest last")
  "p R" '(shl/project-ruff-check :which-key "ruff")
  "p N" '(shl/project-drop-note :which-key "quick note")
  "p o" '(shl/project-chat-browse :which-key "chats"))

(general-with 'fontaine
  (shl/evil-leader
    :states 'normal
    "t f" '(shl/fontaine-toggle :which-key "fontaine toggle")))

(general-with 'consult
  (shl/evil-leader
    :states 'normal
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
    "x f" '(consult-flymake        :which-key "flymake")))

(general-with 'consult-dir
  (shl/evil-leader
    :states 'normal
    "f d" '(consult-dir :which-key "dir switch")))

(general-with 'eglot
  (shl/evil-leader
    :states 'normal
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
    "l ." '(eglot-code-action-quickfix :which-key "quick fix")))

(shl/evil-leader
  :states 'normal
  "n c" '(org-capture :which-key "capture")
  "n a" '(org-agenda :which-key "agenda")
  "n j" '(shl/open-journal :which-key "open journal"))

(use-package gptel
  :ensure nil
  :general
  (shl/evil-leader
    :states 'normal
    "h f" '(shl/gptel-chat-open     :which-key "chats browse")
    "h s" '(shl/gptel-chat-ripgrep  :which-key "chats search")
    "h l" '(shl/gptel-chat-latest :which-key "latest chat")
    "h r" '(gptel-add :which-key "add region")
    "h s" '((lambda () (interactive)
	      (gptel-request
		  (format "Explain and improve types in:\n%s"
			  (if (use-region-p)
			      (buffer-substring-no-properties (region-beginning) (region-end))
			    (buffer-substring-no-properties (point-min) (point-max))))))
	    :which-key "analyze code")
    "h h" #'shl/gptel-minibuffer
    "h m" #'gptel-menu
    "h n" #'gptel
    "h t" #'gptel-org-set-topic
    "h A" '(lambda () (interactive)
	     (setq shl/gptel-autosave-enabled (not shl/gptel-autosave-enabled))
	     (message "Gptel autosave: %s"
		      (if shl/gptel-autosave-enabled "ON" "OFF")))))

(shl/evil-local-leader
  :states 'normal
  :keymaps 'python-base-mode-map
  "t" '((lambda () (interactive)
	  (let ((default-directory (shl/python--project-root)))
	    (compile "uv run ty check")))
	:which-key "ty check")
  "r" '((lambda () (interactive)
	  (let ((default-directory (shl/python--project-root)))
	    (compile "uv run ruff check --fix .")))
	:which-key "ruff full"))

(use-package uv
  :ensure nil
  :general
  (shl/evil-local-leader
    :keymaps 'python-base-mode-map
    :states 'normal
    "u" '(uv :which-key "uv")
    "a" '(uv-add :which-key "uv add")
    "r" '(uv-run :which-key "uv run")))

(shl/evil-leader
  :states 'normal
  "b b" '(consult-buffer :which-key "switch buffer")
  "b k" '(kill-buffer :which-key "kill buffer")
  "f f" '(find-file :which-key "find files")
  "f r" '(consult-recent-file :which-key "find recent"))

(use-package magit
  :ensure nil
  :general
  (shl/evil-leader
    :states 'normal
    "g d" '(magit-diff :which-key "diff")
    "g g" '(magit-status :which-key "magit")
    "g s" '(magit-stage :which-key "stage")))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'init-evil)
;;; init-evil.el ends here

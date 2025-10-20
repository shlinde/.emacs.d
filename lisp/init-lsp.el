;;; init-lsp.el --- LSP Configuration -*- lexical-binding: t; -*-
;;; Code:

(require 'init-general)

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :hook ((;; Existing tree-sitter modes
          ;; c-ts-mode
          ;; c++-ts-mode
          cmake-ts-mode
          csharp-ts-mode
          css-ts-mode
          dockerfile-ts-mode
          go-ts-mode
          html-ts-mode
          java-ts-mode
          js-ts-mode
          json-ts-mode
          python-ts-mode
          ruby-ts-mode
          rust-ts-mode
          toml-ts-mode
          typescript-ts-mode
          yaml-ts-mode
          ;; Traditional modes as fallback
          ;; c-mode
          ;; c++-mode
          python-mode
          ruby-mode
          rust-mode
          js-mode
          typescript-mode) . eglot-ensure)

  :general-config
  (shl/leader
    :keymaps 'eglot-mode-map
    "l l" '(eglot-ensure              :which-key "start/connect")
    "l R" '(shl/eglot-restart         :which-key "restart")
    "l q" '(eglot-shutdown            :which-key "shutdown")
    "l a" '(eglot-code-actions        :which-key "code actions")
    "l r" '(eglot-rename              :which-key "rename")
    "l h" '(shl/eglot-toggle-inlay-hints :which-key "inlay hints")
    "l d" '(eglot-find-declaration    :which-key "declaration")
    "l D" '(eglot-find-definition     :which-key "definition")
    "l i" '(eglot-find-implementation :which-key "implementation")
    "l t" '(eglot-find-typeDefinition :which-key "type def")
    "l s" '(eglot-shutdown-all        :which-key "shutdown all")
    "l e" '(eglot-events-buffer       :which-key "events buffer")
    "l o" '(eglot-stats               :which-key "stats")
    "l ." '(eglot-code-action-quickfix :which-key "quick fix"))
  :init
  (defun shl/eglot-toggle-inlay-hints ()
    (interactive)
    (if (bound-and-true-p eglot-inlay-hints-mode)
        (eglot-inlay-hints-mode 0)
      (eglot-inlay-hints-mode 1)))

  (defun shl/eglot-restart ()
    "Restart the Eglot server for this buffer."
    (interactive)
    (eglot-reconnect (eglot--current-server-or-lose)))

  :config
  (fset #'jsonrpc--log-event #'ignore)
  (general-setq-default eglot-events-buffer-size 0)

  (general-setq eglot-autoshutdown t
		eglot-confirm-server-initiated-edits nil
		eglot-events-buffer-size 0

		eglot-report-progress nil))

;;;; Eldoc Integration
(use-package eldoc
  :ensure nil
  :custom
  ;; Configure eldoc for better LSP integration
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-idle-delay 0.1))

;;;; Flymake Configuration for LSP
(use-package flymake
  :ensure nil
  :hook (eglot-managed-mode . flymake-mode)
  :general-config
  (shl/leader
    :keymaps 'flymake-mode-map
    "d" '(:ignore t :which-key "diagnostics")
    "dl" '(flymake-show-buffer-diagnostics :which-key "buffer diagnostics")
    "dp" '(flymake-show-project-diagnostics :which-key "project diagnostics")
    "dn" '(flymake-goto-next-error :which-key "next error")
    "dp" '(flymake-goto-previous-error :which-key "previous error"))
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer t))

;;;; Flyover - Enhanced Error Display
(use-package flyover
  :ensure t
  :hook (flymake-mode . flyover-mode)
  :custom
  ;; Customize error display
  (flyover-icon-enable t)
  (flyover-error-icon-char "●")
  (flyover-warning-icon-char "●")
  (flyover-info-icon-char "●")
  ;; Auto-update overlays
  (flyover-update-frequency 0.3))


;;;; Completion Integration with LSP
(with-eval-after-load 'corfu
  ;; Enable LSP completion features
  (setq-default corfu-auto t
                corfu-auto-delay 0.1
                corfu-auto-prefix 2))

(with-eval-after-load 'cape
  ;; Add Cape completion functions for LSP
  (defun shl-setup-lsp-capf ()
    "Configure completion-at-point for LSP."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'cape-dabbrev
                       #'cape-file
                       #'cape-keyword))))
  
  (add-hook 'eglot-managed-mode-hook #'shl-setup-lsp-capf))

;;;; Project Integration
(use-package project
  :ensure nil
  :config
  ;; Better project detection for LSP
  (defun shl-project-try-lsp (dir)
    "Find project root by looking for LSP config files."
    (when-let ((root (locate-dominating-file 
                      dir 
                      (lambda (d)
                        (let ((lsp-files '(".lsp" ".ccls" "compile_commands.json" 
                                           "compile_flags.txt" ".clangd" "tsconfig.json"
                                           "package.json" "Cargo.toml" "go.mod" "pom.xml"
                                           "build.gradle" "CMakeLists.txt" "Makefile")))
                          (cl-some (lambda (f) 
                                     (file-exists-p (expand-file-name f d)))
                                   lsp-files))))))
      (cons 'lsp-project root)))
  
  (add-to-list 'project-find-functions #'shl-project-try-lsp)
  
  ;; Define project-root method for lsp-project backend
  (cl-defmethod project-root ((project (head lsp-project)))
    "Return root directory of LSP project."
    (cdr project)))

(provide 'init-lsp)
;;; init-lsp.el ends here

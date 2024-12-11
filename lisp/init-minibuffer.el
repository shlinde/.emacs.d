;;; init-minibuffer.el --- Configuration of Minibuffer Behavior -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

;;; Vertico
(use-package vertico
  :ensure (:files (:defaults "extensions/*.el"))
  :commands vertico-mode
  :init (vertico-mode 1)
  :bind (:map vertico-map
         ("M-RET"   . nil)
         ("M-s"     . nil)
         ("M-i"     . vertico-insert)
         ("C-M-n"   . vertico-next-group)
         ("C-M-p"   . vertico-previous-group)
         ("C-j"     . (lambda () (interactive)
	        	(if minibuffer--require-match
	        	    (minibuffer-complete-and-exit)
	        	  (exit-minibuffer))))
         ("C->"     . embark-become)
         (">"       . embark-become)
         ("C-<tab>"   . embark-act-with-completing-read)
         ("M-*"      . embark-act-all)
         ("M-s o"   . embark-collect)
         ("C-c C-o" . embark-collect)
         ("C-M-l"     . embark-export))
  :config
  (setq vertico-count 10
        vertico-cycle t
        vertico-resize t)
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  (advice-add #'ffap-menu-ask :around
              (lambda (&rest args)
                (cl-letf (((symbol-function #'minibuffer-completion-help)
                           #'ignore))
                  (apply args)))))

;;; Consult
(use-package consult
  :ensure t
  ;; :hook (minibuffer-setup . consult-completion-enable-in-minibuffer)
  ;; :hook ((shell-mode eshell-mode) . (lambda () (setq completion-in-region-function
  ;;                                               #'consult-completion-in-region)))
  :init
  ;; (defun consult-completion-enable-in-minibuffer ()
  ;;     "Enable consult-completion-in-region in the minibuffer if
  ;; `completion-at-point' is bound."
  ;;     (when (where-is-internal #'completion-at-point (list (current-local-map)))
  ;;       ;; (setq-local corfu-auto nil) Enable/disable auto completion
  ;;       (setq completion-in-region-function #'consult-completion-in-region)))
  :bind (("C-x b"   . consult-buffer)
         ("C-x H-r" . consult-recent-file)
         ("C-x M-:" . consult-complex-command)
         ("M-s M-o" . consult-multi-occur)
         ("M-X" . consult-mode-command)
         ("C-h C-m" . consult-minor-mode-menu)
         ("C-c C-j" . consult-outline)
         ("M-s M-j" . consult-outline)
         ("M-s M-l" . consult-locate)
         ("M-s g"   . consult-ripgrep)
         ("M-s G"   . consult-git-grep)
         ("C-x C-r" . consult-recent-file)
         ("s-b" . consult-buffer)
         ("M-g j" . consult-compile-error)
         ("M-g g" . consult-goto-line)
         ;; ("H-b" . consult-buffer)
         ("M-m" . consult-register-store)
         ("M-s k l" . consult-focus-lines)
         ("M-'" . consult-register-load)
         ("M-y" . consult-yank-pop)
         ("C-x `" . consult-compile-error)
         :map help-map
         ("TAB" . consult-info)
         :map ctl-x-r-map
         ("b" . consult-bookmark)
         ("x" . consult-register)
         :map ctl-x-4-map
         ("b" . consult-buffer-other-window)
         :map ctl-x-5-map
         ("b" . consult-buffer-other-frame)
         ;; :map space-menu-file-map
         ;; ("l" . consult-locate)
         :map minibuffer-local-map
         ("M-r" . consult-history)
         :map project-prefix-map
         ("b" . consult-project-buffer))
  :config
  (setq consult-narrow-key "<")
  (setq consult-line-numbers-widen t)
  (setq consult-preview-key 'any)
  (setq consult-async-split-style 'semicolon)
  (consult-customize
   ;; consult-ripgrep consult-git-grep consult-grep consult-xref
   consult-bookmark consult--source-buffer consult-recent-file
   consult--source-recent-file consult--source-project-recent-file
   consult--source-bookmark consult--source-project-buffer
   consult-info
   :preview-key "C-M-m"
   consult-theme :preview-key (list :debounce 0.3 "C-M-m"))

  (when (executable-find "plocate")
    (setq consult-locate-args "plocate --ignore-case --existing --regexp"))
  
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  
  (setq register-preview-delay 1.0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  
  (use-package consult-flymake
    :bind ("M-g f" . consult-flymake)))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here

;;; init-ai.el --- AI Setup -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

;;
;; (require 'init-evil)
;; (require 'init-general)
(require 'init-org)

;; gptel
(use-package gptel
  :ensure (:host github :protocol ssh
	             :repo "karthink/gptel")
  :commands (gptel gptel-send)
  :bind (("C-c i" . gptel-menu)
	     ("C-c I" . gptel-send))
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model 'claude-3-7-sonnet-20250219
        gptel-backend (gptel-make-anthropic "Claude"          ;Any name you want
                        :stream t                             ;Streaming responses
                        :key (getenv "ANTHROPIC_API_KEY"))))


(use-package aidermacs
  :ensure (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :config
  (setq aidermacs-default-model "sonnet")
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu)
  ; Enable minor mode for Aider files
  (aidermacs-setup-minor-mode)
  ; See the Configuration section below
  (setq aidermacs-auto-commits t)
  (setq aidermacs-use-architect-mode t)
  ;; Use vterm backend (default is comint)
  (setq aidermacs-backend 'vterm)
  ; Ensure emacs can access *_API_KEY through .bashrc or setenv
  (setenv "ANTHROPIC_API_KEY" (getenv "ANTHROPIC_API_KEY")))

(provide 'init-ai)
;;; init-ai.el ends here

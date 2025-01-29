;;; init-ai.el --- AI Setup -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

;;
;; (require 'init-evil)
;; (require 'init-general)

;; gptel
(use-package gptel
  :ensure (:host github :protocol ssh
	             :repo "karthink/gptel")
  :commands (gptel gptel-send)
  :bind (("C-c i" . gptel-menu)
	     ("C-c I" . gptel-send))
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model 'claude-3-haiku-20240307
        gptel-backend (gptel-make-anthropic "Claude"          ;Any name you want
                        :stream t                             ;Streaming responses
                        :key (getenv "ANTHROPIC_API_KEY"))))



(use-package aider
  :ensure (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022")))


(provide 'init-ai)
;;; init-ai.el ends here

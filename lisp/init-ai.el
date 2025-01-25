;;; init-ai.el --- AI Setup -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

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

	

(provide 'init-ai)
;;; init-ai.el ends here

;;; init-ai.el --- AI Setup -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

(use-package corfu
  :ensure (corfu :files (:defaults "extensions/*.el"))
  :config
  (setopt corfu-auto t
          corfu-auto-delay 0.15
          corfu-auto-prefix 1)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :ensure t
  :init
  ;; company adapter examples
  (setq-local completion-at-point-functions
              (mapcar #'cape-company-to-capf
                      (list #'company-files
                            #'company-ispell
                            #'company-dabbrev)))
  (setq completion-at-point-functions
        (list
         (cape-company-to-capf
          (apply-partially #'company--multi-backend-adapter
                           '(company-dabbrev company-elisp))))))


(provide 'init-completion)
;;; init-completion.el ends herec

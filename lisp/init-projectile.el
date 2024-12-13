;;; init-projectile.el --- Projectile setup -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :init
  (setopt projectile-mode-line nil)
  (projectile-global-mode)
  (setq projectile-project-root-files-bottom-up
        '(".git" ".projectile"))
  (setq projectile-completion-system 'auto)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching nil)
  (setq projectile-verbose nil)
  (setq projectile-do-log nil)
  (setq projectile-switch-project-action
        (lambda ()
          (dired (projectile-project-root))))

  (general-spc "p" #'projectile-command-map))


(provide 'init-projectile)
;;; init-projectile.el ends here

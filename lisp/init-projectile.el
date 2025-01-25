;;; init-projectile.el --- Projectile setup -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :init
  (setq projectile-mode-line nil)
  (setq projectile-enable-caching t
        projectile-completion-system 'auto
        projectile-project-search-path '("~/data/source" "~/data/resources/code-reference")
        projectile-project-root-files-bottom-up
        '(".git" ".projectile")
        projectile-completion-system 'auto
        projectile-verbose nil
        projectile-do-log nil
        projectile-switch-project-action
        (lambda ()
          (dired (projectile-project-root))))

  (projectile-global-mode)
  (global-set-key (kbd "C-x p") #'projectile-command-map))


(provide 'init-projectile)
;;; init-projectile.el ends here

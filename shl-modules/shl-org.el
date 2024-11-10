(use-package org
  :ensure t
  :bind (:map global-map
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (setopt org-directory (expand-file-name "~/data/org/")
          org-agenda-files `(,org-directory)
          org-default-notes-file (concat org-directory "/tasks.org")
          org-id-link-to-org-use-id t))



(use-package org
  :ensure nil
  :config 
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-persistent-message nil)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0))

(provide 'shl-org)

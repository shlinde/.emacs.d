;;; init.el --- SHLinde Emacs Configuration -*- lexical-binding: t; -*-
;;; Code:

;;; Core
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))
(require 'init-optim)
(require 'init-elpaca)

;;; Org mode
(use-package org
  :ensure (:wait t)
  :bind (("C-c n" . (lambda ()
		      (interactive)
		      (find-file "~/data/org/inbox.org")))))

(require 'init-general)  ; TODO Remove this when the init file is refactored. Should just be required from the modules.
(require 'init-ui)
(require 'init-editor)
(require 'init-vc)
(require 'init-dired)
(require 'init-term)
(require 'init-workspaces)
(require 'init-completion)
(require 'init-treesitter)
(require 'init-lsp)
(require 'init-prog)
;; (require 'init-evil)
(require 'init-org)
(require 'init-ai)


;; (require 'init-android)  ; only load if on android

(provide 'init)
;;; init.el ends here


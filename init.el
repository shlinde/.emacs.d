;;; init.el --- SHLinde Emacs Configuration -*- lexical-binding: t; -*-
;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;;; Core
(require 'init-optim)
(require 'init-elpaca)
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


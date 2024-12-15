;;; init.el --- Custom configurations -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

;;; Core Setup
;; Add `lisp' directory to load-path
(push (file-name-concat user-emacs-directory "lisp/") load-path)

;; Setup core paths
(require 'init-core)

;;; Package Setup - Elpaca
(require 'init-elpaca)

;;; Optimizations
(require 'init-optim)

;;; UI Setup
(require 'init-ui)

;; ;;; General
;; (require 'init-general)
;; (elpaca-wait)

;; ;; ;;; Evil
;; (require 'init-evil)

;;; Editor Setup
(require 'init-editor)

;;; Dired Setup
(require 'init-dired)

;;; Projectile
(require 'init-projectile)


;;; Version Control Setup
(require 'init-vcs)

;;; Setup Minibuffer
(require 'init-minibuffer)

;;; Completion
(require 'init-lsp)

;;; Completion
(require 'init-completion)

;;; Setup Org
(require 'init-org)

;;; Setup AI Companion
(require 'init-ai)

;;; Python
(require 'init-python)

;;; Zig
(require 'init-zig)

;;; Rust
(require 'init-rust)

;;; Setup Hydra
(require 'init-hydra)

(provide 'init)
;;; init.el ends here

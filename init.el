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

;;; Version Control Setup
(require 'init-vcs)

;;; Setup Minibuffer
(require 'init-minibuffer)

(provide 'init)
;;; init.el ends here
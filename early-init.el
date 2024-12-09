;;; early-init.el --- Early customization -*- lexical-binding: t; -*-
;;
;;
;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;
;;; Commentary:
;;
;; See Emacs Help for more information on The Early Init File.
;; Basically, this file contains frame customizations.
;;
;;; Code:

;; Do garbage collection after we load and setup Emacs
(setq gc-cons-threshold most-positive-fixnum)

;; Enable using PLISTS
(setenv "LSP_USE_PLISTS" "true")

;; Package initialization happens before loading `user-init-file',
;; but after `early-init-file'
(setq package-enable-at-startup nil
      package-quickstart nil
      load-prefer-newer t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)

(provide 'early-init)
;;; early-init.el ends here

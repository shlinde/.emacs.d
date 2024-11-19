;;; Emacs Configuration of Sebastian Hempel Linde -*- lexical-binding: t -*-
;;; Commentary:
;;;     This part of the configuration ensures that every custom lisp modules and every
;;;     configuration module is loaded at the correct time.
;;; Code:

(defconst shl-theme "doom"
  "Selected theme for the Emacs configuration")

(defconst shl-dark-p t
  "True for dark theme, nil for light.")

(defconst shl--module-dir (concat user-emacs-directory "shl-modules")
  "Modules of my emacs configuration.")

(defconst shl--site-lisp-dir (concat user-emacs-directory "shl-lisp")
  "Site lisp of my emacs configuration.")

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defun shl--load-custom-lisp (pkg &optional maybe-disabled)
  "Load PKG if MAYBE-DISABLED is nil or it's nil but start up in normal slowly."
  (when (not maybe-disabled)
    (load (file-truename (format "%s/%s" shl--site-lisp-dir pkg)) t t)))

(defun shl--load-module (pkg &optional maybe-disabled)
  "Load PKG if MAYBE-DISABLED is nil or it's nil but start up in normal slowly."
  (when (not maybe-disabled)
    (load (file-truename (format "%s/%s" shl--module-dir pkg)) t t)))

(let* ((file-name-handler-alist nil))
  ;; Load custom Lisp
  (shl--load-custom-lisp 'shl-core)

  ;; Load modules
  (shl--load-module 'shl-themes)
  (shl--load-module 'shl-font)
  (shl--load-module 'shl-ui)
  (shl--load-module 'shl-essentials)
  (shl--load-module 'shl-motion)
  (shl--load-module 'shl-tools)
  (shl--load-module 'shl-langs)
  (shl--load-module 'shl-org))

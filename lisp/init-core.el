 ;;; init-core.el --- Core Settings and Helpers -*- lexical-binding: t; -*-
;;;
;;; Author: Sebastian Hempel Linde <sebastian@hempellinde.com>
;;;
;;; Commentary:
;;; Code:

;;; Environment
;;  Helpers for knowing which environment we are in
(defconst env-graphic-p (display-graphic-p))
(defconst env-rootp (string-equal "root" (getenv "USER")))
(defconst env-sys-linux-p (eq system-type 'gnu/linux))
(defconst env-sys-wsl-p (when (string-match "-[Mm]icrosoft" operating-system-release) t))
(defconst env-sys-name (system-name))

;; Paths to common directories
(defconst shl--home-dir (file-name-as-directory (getenv "HOME"))
  "Path to user home directory.")

(defconst shl--data-dir (concat shl--home-dir "data/")
  "Path to user data directory.")

(defconst shl--emacs-dir  (file-name-as-directory user-emacs-directory)
  "Path to Emacs configuration.")

(defconst shl--local-dir
  (file-name-as-directory (concat shl--home-dir ".cache"))
  "Path to cache directory.")

(defconst shl--etc-dir (concat shl--local-dir "etc/")
  "Directory for non-volatile storage.
Use this for files that don't change much, like servers binaries,
external dependencies or long-term shared data")

(defconst shl--cache-dir (concat shl--local-dir "cache/")
  "Directory for volatile storage.")

(defconst shl--packages-dir
  (expand-file-name (format "packages/%s.%s"
                            emacs-major-version
                            emacs-minor-version)
                    shl--local-dir)
  "Path to directory containing packages.")

(defconst shl--projects-dir
  (file-name-as-directory
   (or (getenv "PROJECTS_HOME")
       (concat shl--data-dir "source/")))
  "Root directory for projects.")

(defconst shl--org-dir
  (file-name-as-directory
   (or (getenv "ORG_HOME")
       (concat shl--data-dir "org/")))
  "Root directory for notes.")

(provide 'init-core)
;;; init-core.el ends here

;;; shl-core.el --- Core variables and functions -*- lexical-binding: t; -*-
;;; Code:

;;; Core
(defconst shl--cache-dir (concat user-emacs-directory ".cache/"))

(defconst shl--code-reference-dir
  (expand-file-name "~/data/resources/code-reference/")
  "Directory for cloned reference code repositories.")

;;;; Directory Variables
;;  We're going to define a number of directories that are used throughout this
;;  configuration to store different types of files. This is a bit like the
;;  `no-littering' package, and allows us to keep `user-emacs-directory' tidy.

(defconst shl-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory.")

(defconst shl-library-dir (concat shl-emacs-dir "shl-library/")
  "The directory for 𝛌-Emacs Lisp libraries.
This will house all setup libraries and external libraries or packages.")

(defconst shl-user-dir (concat shl-library-dir "shl-user/")
  "Storage for personal elisp, scripts, and any other private files.")

(defconst shl-setup-dir (concat shl-library-dir "shl-setup/")
  "The storage location of the setup-init files.")

(defconst shl-var-dir (concat shl-emacs-dir "var/")
  "The directory for non-essential file storage.
Contents are subject to change. Used for package storage (elpa or
straight) and by `shl-etc-dir' and `shl-cache-dir'.")

(defconst shl-etc-dir (concat shl-var-dir "etc/")
  "The directory for non-volatile storage.
  These are not deleted or tampered with by emacs functions. Use
  this for dependencies like servers or config files that are
  stable (i.e. it should be unlikely that you need to delete them
               if something goes wrong).")

(defconst shl-cache-dir (concat shl-var-dir "cache/")
  "The directory for volatile storage.
  Use this for transient files that are generated on the fly like
  caches and ephemeral/temporary files. Anything that may need to
  be cleared if there are probshls.")

(defconst shl-default-config-file (concat shl-library-dir "shl-default-config.el")
  "A sample default configuration of the personal config file to get the user started.")

;;;; User Configuration Variables

;; Define customization group for Shl Emacs.
(defgroup shl-emacs '()
  "An Emacs distribution with sane defaults, pre-configured packages, and useful functions, aimed at writing and academic work in the humanities."
  :tag "Shl-Emacs"
  :link '(url-link "https://github.com/Shl-Emacs/shl-emacs")
  :group 'emacs)

;; Find the user configuration file
(defconst shl-config-file (expand-file-name "config.el" shl-user-dir)
  "The user's configuration file.")

;; These next two variables are both optional, but may be convenient.
;; They are used with the functions `shl-goto-projects' and `shl-goto-elisp-library'.

;; Set user project directory
(defcustom shl-project-dir nil "Set the directory for user projects."
  :group 'shl-emacs
  :type 'string)

;; Set user project directory
(defcustom shl-mono-font "Aporetic Sans Mono" "Set the main font for Emacs."
  :group 'shl-emacs
  :type 'string)

;; Set user project directory
(defcustom shl-variable-pitch-font "Aporetic Serif" "Set the main font for Emacs."
  :group 'shl-emacs
  :type 'string)

;; Set user elisp project dir
(defcustom shl-user-elisp-dir nil
  "Directory for personal elisp projects.e
Any customized libraries not available via standard package repos like elpa or melpa should go here."
  :group 'shl-emacs
  :type 'string)

;; Ensure packages?
(defcustom shl-package-ensure-packages t
  "Whether to ensure packages with use-package, or install manually using the list in `package-selected-packages'."
  :group 'shl-emacs
  :type 'boolean)

;;;; Make System Directories
;; Directory paths
(dolist (dir (list shl-library-dir shl-var-dir shl-etc-dir shl-cache-dir shl-user-dir shl-setup-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))




(provide 'shl-core)
;;; shl-core.el ends here

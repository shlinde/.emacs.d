;;; shl-core.el --- Core variables and functions -*- lexical-binding: t; -*-
;;; Code:

;;; Core
(defconst shl--cache-dir (concat user-emacs-directory ".cache/"))

(defconst shl--code-reference-dir
  (expand-file-name "~/data/resources/code-reference/")
  "Directory for cloned reference code repositories.")


(provide 'shl-core)
;;; shl-core.el ends here

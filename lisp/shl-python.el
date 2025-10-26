;;; Python Helpers -*- lexical-binding: t -*-
;;; Code


;; (defun consult-python-symbol ()
;;   "Search for python symbols in the current project."
;;   (interactive)
;;   (consult--read (shl/python-symbols)
;; 		   :prompt "Search Python Symbol: "
;;                    :require-match t
;;                    :category 'python-symbol
;;                    :lookup #'consult--lookup-member
;;                    :history 'symbol-history))

(defun shl/python-symbols ()
  "Query Python symbols for current project."
  (when (executable-find "uv")
    (split-string (shell-command-to-string "uv run --with pydoc_utils python -m pydoc_utils") "\n" t)))

(defun shl/python-doc (symbol)
  "Get the pydoc documentation for SYMBOL in a compilation buffer."
  (interactive (list (completing-read "Symbol: " (shl/python-symbols))))
  (compilation-start
   (format "uv run python -m pydoc %s | sed 's/^ *| \\{0,3\\}//'" symbol)
   'pydoc-mode
   (lambda (_mode) "*pydoc*")))

(define-derived-mode pydoc-mode compilation-mode "Pydoc"
  "Major mode for viewing pydoc output."
  (setq-local compilation-read-only t
	      compilation-scroll-output nil))

(provide 'shl-python)
;;; shl-python.el ends here

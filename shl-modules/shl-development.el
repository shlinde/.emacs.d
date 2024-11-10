(defun shl/colorize-compilation-buffer ()
  (read-only-mode 'toggle)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode 'toggle))
(add-hook 'compilation-filter-hook 'shl/colorize-compilation-buffer)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setopt magit-repository-directories
          '(("~/data/source" . 1))))

(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          #'(lambda()
              (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(shl-pkg-config 'esh-autosuggest
  (add-hook 'eshell-mode-hook #'esh-autosuggest-mode))

(use-package eat
  :ensure t
  :bind (("C-c t t" . eat)))

(use-package zig-mode
  :ensure t)

(use-package odin-mode
  :vc (:url "https://git.sr.ht/~mgmarlow/odin-mode")
  :ensure t
  :mode ("\\.odin\\'" . odin-mode))


(provide 'shl-development)

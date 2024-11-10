;;; init.el --- Emacs Configuration of Sebastian Hempel Linde -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar shl--package-contents-refreshed nil)

(defun shl--package-refresh-contents-once ()
  "Refresh the contents of the package cache.
Should only be done once when starting Emacs."
  (when (not shl--package-contents-refreshed)
    (setq shl--package-contents-refreshed t)
    (package-refresh-contents)))

(defvar shl--missing-packages
  "Packages not able to install")

(defmacro shl-pkg-config (name &rest config)
  "Install NAME Package and evaluate the CONFIG if
package install is successful."
  (declare (indent 1))
  `(if (not (package-installed-p ,name))
       (condition-case err
	   (progn (shl--package-refresh-contents-once)
		  (package-install ,name))
	 (error
	  (message "Couldn't install optional package `%s': %S" ,name err)
	  (push ,name shl--missing-packages)))
     ,@config))

(defmacro shl-bind-key (keymap &rest definitions)
  "Expand key binding DEFINITIONS for the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs."))
  (let ((keys (seq-filter #'stringp definitions))
        (commands (seq-remove #'stringp definitions)))
    `(when-let* (((keymapp ,keymap))
                 (map ,keymap))
       ,@(mapcar
          (lambda (pair)
            (let* ((key (car pair))
                   (command (cdr pair)))
              (unless (and (null key) (null command))
                `(define-key map (kbd ,key) ,command))))
          (cl-mapcar #'cons keys commands)))))

(require 'cl-lib)
(defun shl-git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in dired when finished."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (download-dir (expand-file-name "~/data/resources/code-reference/"))
         (project-dir (concat (file-name-as-directory download-dir)
                              (file-name-base url)))
         (default-directory download-dir)
         (command (format "git clone %s" url))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
    (when (file-exists-p project-dir)
      (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
          (delete-directory project-dir t)
        (user-error "Bailed")))
    (switch-to-buffer buffer)
    (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
    (with-current-buffer buffer
      (setq default-directory download-dir)
      (shell-command-save-pos-or-erase)
      (require 'shell)
      (shell-mode)
      (view-mode +1))
    (set-process-sentinel proc (lambda (process state)
                                 (let ((output (with-current-buffer (process-buffer process)
                                                 (buffer-string))))
                                   (kill-buffer (process-buffer process))
                                   (if (= (process-exit-status process) 0)
                                       (progn
                                         (message "finished: %s" command)
                                         (dired project-dir))
                                     (user-error (format "%s\n%s" command output))))))
    (set-process-filter proc #'comint-output-filter)))

(provide 'shl-core)

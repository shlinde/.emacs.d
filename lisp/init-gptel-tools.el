;;; init-ai.el --- AI Configuration -*- lexical-binding: t; -*-
;;; Code:

(require 'init-ai)

(defun gptel--normalize-max-depth (max-depth)
  "Convert MAX-DEPTH to a number, handling strings, numbers, or nil.
Returns 3 as default if MAX-DEPTH is nil or invalid."
  (cond
   ;; Already a number
   ((numberp max-depth) max-depth)
   ;; String that can be converted to number
   ((and (stringp max-depth)
         (not (string-empty-p max-depth))
         (string-match-p "^[0-9]+$" max-depth))
    (string-to-number max-depth))
   ;; Default case (nil, empty string, or invalid input)
   (t 3)))

(defun gptel--parse-gitignore (gitignore-file)
  "Parse a .gitignore file and return a list of patterns."
  (when (file-exists-p gitignore-file)
    (with-temp-buffer
      (insert-file-contents gitignore-file)
      (let ((patterns '()))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (string-trim (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))))
            (unless (or (string-empty-p line) (string-prefix-p "#" line))
              (push line patterns)))
          (forward-line 1))
        (nreverse patterns)))))

(defun gptel--should-ignore-p (file-path gitignore-patterns)
  "Check if FILE-PATH should be ignored based on GITIGNORE-PATTERNS."
  (let ((relative-path (file-name-nondirectory file-path)))
    (cl-some (lambda (pattern)
               (cond
                ;; Directory pattern (ends with /)
                ((string-suffix-p "/" pattern)
                 (and (file-directory-p file-path)
                      (string-match-p (concat "^" (regexp-quote (string-remove-suffix "/" pattern)) "$")
                                     relative-path)))
                ;; Exact match
                ((not (string-match-p "[*?]" pattern))
                 (string= relative-path pattern))
                ;; Wildcard pattern
                (t
                 (string-match-p (concat "^" (replace-regexp-in-string "\\*" ".*" (regexp-quote pattern)) "$")
                                relative-path))))
             gitignore-patterns)))

(defun gptel--collect-gitignore-patterns (directory)
  "Collect all .gitignore patterns from DIRECTORY and parent directories."
  (let ((patterns '())
        (current-dir (expand-file-name directory)))
    (while (and current-dir (not (string= current-dir "/")))
      (let ((gitignore-file (expand-file-name ".gitignore" current-dir)))
        (when (file-exists-p gitignore-file)
          (setq patterns (append (gptel--parse-gitignore gitignore-file) patterns))))
      (let ((parent (file-name-directory (directory-file-name current-dir))))
        (setq current-dir (if (string= parent current-dir) nil parent))))
    ;; Add common ignore patterns
    (append patterns '(".git" ".DS_Store" "node_modules" "__pycache__" "*.pyc"))))

(defun gptel--directory-tree (directory max-depth show-hidden)
  "Generate a tree representation of DIRECTORY."
  (let ((expanded-dir (expand-file-name directory)))
    (concat (abbreviate-file-name expanded-dir) "\n"
            (gptel--directory-tree-recursive expanded-dir max-depth 0 show-hidden ""))))

(defun gptel--directory-tree-recursive (directory max-depth current-depth show-hidden prefix)
  "Internal recursive function for generating directory tree."
  (if (>= current-depth max-depth)
      ""
    (let* ((expanded-dir (expand-file-name directory))
           (gitignore-patterns (gptel--collect-gitignore-patterns expanded-dir))
           (entries (condition-case nil
                        (directory-files expanded-dir t "^[^.]" t)
                      (error nil)))
           (filtered-entries '())
           (result ""))

      ;; Add hidden files if requested
      (when show-hidden
        (setq entries (append entries
                             (directory-files expanded-dir t "^\\.[^.]" t))))

      ;; Filter out ignored files
      (dolist (entry entries)
        (unless (gptel--should-ignore-p entry gitignore-patterns)
          (push entry filtered-entries)))

      (setq filtered-entries (sort filtered-entries #'string<))

      ;; Generate tree output
      (let ((total (length filtered-entries)))
        (dotimes (i total)
          (let* ((entry (nth i filtered-entries))
                 (basename (file-name-nondirectory entry))
                 (is-last (= i (1- total)))
                 (is-dir (file-directory-p entry))
                 (connector (if is-last "└── " "├── "))
                 (new-prefix (concat prefix (if is-last "    " "│   "))))

            (setq result (concat result prefix connector basename
                                (if is-dir "/" "") "\n"))

            ;; Recurse into directories
            (when (and is-dir (< (1+ current-depth) max-depth))
              (setq result (concat result
                                  (gptel--directory-tree-recursive entry max-depth
                                                                  (1+ current-depth) show-hidden
                                                                  new-prefix)))))))
      result)))

(gptel-make-tool
 :function (lambda (directory &optional max-depth show-hidden)
             (with-temp-message (format "Listing directory tree: %s" directory)
               (condition-case err
                   (let ((max-depth (gptel--normalize-max-depth max-depth))
                         (show-hidden (and show-hidden (not (string= show-hidden "")))))
                     (gptel--directory-tree directory max-depth show-hidden))
                 (error (format "Error listing directory: %s - %s" directory (error-message-string err))))))
 :name "list_directory"
 :description "List the contents of a directory in a tree format, respecting .gitignore files"
 :args (list '(:name "directory"
                     :type string
                     :description "The path to the directory to list")
             '(:name "max-depth"
                     :type string
                     :description "Optional: Maximum depth to traverse (default: 3)")
             '(:name "show-hidden"
                     :type string
                     :description "Optional: Show hidden files/directories (default: false)"))
 :category "filesystem"
 :include t)

(gptel-make-tool
   :function (lambda (filepath)
               (let ((ignore-patterns (when (boundp 'gptel-read-file-ignore-patterns)
                                       gptel-read-file-ignore-patterns))
                     (expanded-path (expand-file-name filepath)))
                 (if (and ignore-patterns
                          (cl-some (lambda (pattern) 
                                    (string-match-p pattern expanded-path)) 
                                  ignore-patterns))
                     (format "Access denied: File %s matches ignore patterns" filepath)
                   (with-temp-message (format "Reading file: %s" filepath)
                     (condition-case err
                         (with-temp-buffer
                           (insert-file-contents expanded-path)
                           (buffer-string))
                       (error (format "Error reading file: %s - %s" filepath (error-message-string err))))))))
   :name "read_file"
   :description "Read and display the contents of a file. Note: If a file is already included in the current gptel context (conversation), there is no need to read it again as the context is always current."
   :args (list '(:name "filepath"
                       :type string
                       :description "Path to the file to read. Supports relative paths and ~. Only use this tool for files not already in the conversation context."))
   :category "filesystem"
   :include t)

(setq gptel-read-file-ignore-patterns
      '("*.secret"
        "config/*"))

(gptel-make-tool
 :name "file_lint_with_flycheck"
 :description (concat
               "Lints the specified file using Flycheck in Emacs, returning any errors or warnings "
               "(or a 'no errors found' message).\n\n"
               "**LLM Workflow for Code Modification:**\n"
               "1.  **Baseline Check:** Run this tool on the file *before* generating a patch or "
               "other code modifications. This helps understand the existing lint status and "
               "avoid re-introducing pre-existing issues or being blamed for them.\n"
               "2.  **Verification Check:** After your changes have been applied to the file "
               "(e.g., via a patch), run this tool again.\n"
               "3.  **Self-Correction:** Compare the lint output from step 2 (after your changes) "
               "with the baseline from step 1 (before your changes). If your modifications "
               "introduced *new* lint errors, you are expected to refactor your code to fix "
               "these new errors. Re-run this lint tool to confirm your fixes before "
               "considering the task complete. Focus on fixing errors introduced by your changes.")
 :args (list '(:name "filename"
                     :type "string"
                     :description "The path (relative or absolute) to the file to be checked."))
 :category "emacs"
 :include t
 :function
 (lambda (filename)
   (unless (require 'flycheck nil t)
     (error "Flycheck package is not available."))
   (unless (stringp filename)
     (error "Filename argument must be a string."))
   (let ((original-filename filename))
     (condition-case err
         (let* ((absolute-filename (expand-file-name filename))
                (buffer-object (get-file-buffer absolute-filename))
                (temp-buffer-created (not buffer-object))
                (buffer (or buffer-object
                            (progn
                              (unless (file-exists-p absolute-filename)
                                (error "File not found: %s (expanded from %s)" absolute-filename original-filename))
                              (find-file-noselect absolute-filename))))
                (flycheck-was-on-in-buffer nil)
                (errors-string "Error: Failed to collect Flycheck results.")) ; Default error

           (unless (buffer-live-p buffer)
             (error "Could not open or find buffer for file: %s" absolute-filename))

           (with-temp-message (format "Linting %s with Flycheck..." absolute-filename)
             (unwind-protect
                 (progn ;; Main work block
                   (with-current-buffer buffer
                     (setq flycheck-was-on-in-buffer flycheck-mode)
                     (unless flycheck-mode
                       (flycheck-mode 1)
                       (unless flycheck-mode
                         (error "Failed to enable Flycheck mode in buffer %s." (buffer-name))))

                     (flycheck-buffer) ;; Request a syntax check

                     (let ((timeout 15.0)
                           (start-time (float-time)))
                       (while (and (flycheck-running-p)
                                   (< (- (float-time) start-time) timeout))
                         (sit-for 0.1 t))) ;; Wait, process events

                     ;; Check reason for loop termination & collect errors
                     (if (flycheck-running-p)
                         (error "Flycheck timed out after %.0f seconds for %s" timeout absolute-filename)
                       ;; Flycheck is no longer running, collect errors
                       (progn
                         ;; flycheck-current-errors is a VARIABLE
                         (let ((current-errors flycheck-current-errors))
                           (if current-errors
                               (setq errors-string
                                     (format "Flycheck results for %s:\n%s"
                                             absolute-filename
                                             (mapconcat
                                              (lambda (err-obj) ;; err-obj is a flycheck-error struct
                                                (format "- %S at L%s%s (%s): %s"
                                                        (flycheck-error-level err-obj)
                                                        (flycheck-error-line err-obj)
                                                        (if-let ((col (flycheck-error-column err-obj))) ; CORRECTED
                                                            (format ":C%s" col) "")
                                                        (flycheck-error-checker err-obj)
                                                        (flycheck-error-message err-obj)))
                                              current-errors "\n")))
                             (setq errors-string (format "No Flycheck errors found in %s." absolute-filename)))))))
                   ;; errors-string is now set based on Flycheck output
                   ) ;; End of with-current-buffer
                 ;; Cleanup block for unwind-protect
                 (progn
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (when (and flycheck-mode (not flycheck-was-on-in-buffer))
                         (flycheck-mode 0))))
                   (when (and temp-buffer-created (buffer-live-p buffer))
                     (kill-buffer buffer)))))
           errors-string)
       (error (format "Error linting file %s: %s"
                      original-filename (error-message-string err)))))))

(gptel-make-tool
 :function (lambda (buffer)
             (with-temp-message (format "Reading buffer: %s" buffer)
               (condition-case err
                   (if (buffer-live-p (get-buffer buffer))
                       (with-current-buffer buffer
                         (buffer-substring-no-properties (point-min) (point-max)))
                     (format "Error: buffer %s is not live." buffer))
                 (error (format "Error reading buffer %s: %s" 
                                buffer (error-message-string err))))))
 :name "read_buffer"
 :description "Return the contents of an Emacs buffer"
 :args (list '(:name "buffer"
                     :type string
                     :description "The name of the buffer whose contents are to be retrieved"))
 :category "emacs"
 :include t)

(defun gptel-read-documentation (symbol)
  "Read the documentation for SYMBOL, which can be a function or variable."
  (with-temp-message (format "Reading documentation for: %s" symbol)
    (condition-case err
        (let ((sym (intern symbol)))
          (cond
           ((fboundp sym)
            (documentation sym))
           ((boundp sym)
            (documentation-property sym 'variable-documentation))
           (t
            (format "No documentation found for %s" symbol))))
      (error (format "Error reading documentation for %s: %s" 
                     symbol (error-message-string err))))))

(gptel-make-tool
 :name "read_documentation"
 :function #'gptel-read-documentation
 :description "Read the documentation for a given function or variable"
 :args (list '(:name "name"
                     :type string
                     :description "The name of the function or variable whose documentation is to be retrieved"))
 :category "emacs"
 :include t)

(gptel-make-tool
 :name "TrafilaturaFetch"
 :function #'munen-gptel--trafilatura-fetch-url
 :description "Fetch content from a URL using trafilatura, which extracts main content and metadata while removing boilerplate, navigation and ads."
 :args '((:name "url"
                :type string
                :description "URL to fetch content from"))
 :category "web"
 :include t)

(provide 'init-gptel-tools)
;;; init-gptel-tools.el ends here

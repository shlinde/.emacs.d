;;; init-ai.el --- AI Configuration -*- lexical-binding: t; -*-
;;; Code:

(require 'init-general)

;;; AI
(use-package gptel
  :ensure (:url "https://github.com/karthink/gptel") ; For Emacs>=30
  :defer 2
  :preface
  (defvar shl/gptel-chat-directory
    (expand-file-name "chats/" org-directory)
    "Directory for Gptel chat transcripts.")

  (defun shl/gptel-minibuffer (prompt)
    (interactive (list (read-string "GPT> ")))
    (message "GPT... Thinking")
    (let ((gptel-model "gemini-2.0-flash-001")
          (gptel-max-tokens 256)
	  (gptel-tools '()))
      (gptel-request
       prompt
       :stream nil ;; ensure single final callback
       :callback
       (lambda (response info)
	 (if-let* ((err (plist-get info :error)))
	     (message "GPT error: %s" err)
	   (when (stringp response)
	     (kill-new response)
	     (let ((one-line (replace-regexp-in-string "[\n\r]+" " "
						       (string-trim response))))
	       (message "GPT: %s" one-line))))))))

  (defun shl/gptel--chat-dir ()
    (file-name-as-directory (expand-file-name shl/gptel-chat-directory)))

  (defun shl/gptel-chat-open ()
    "Open Gptel chats directory in Dirvish with preview auto-enabled."
    (interactive)
    (let ((dir (shl/gptel--chat-dir)))
      (make-directory dir t)
      (dirvish dir)
      ;; Slight delay so window/layout exists before toggling preview.
      (run-at-time 0.05 nil #'shl/dirvish-ensure-preview)))

  (defun shl/gptel-chat-ripgrep (&optional initial)
    "Run consult-ripgrep limited to the Gptel chats directory."
    (interactive)
    (let ((dir (shl/gptel--chat-dir)))
      (make-directory dir t)
      (let ((default-directory dir))
	(consult-ripgrep dir initial))))
  :config 
  (if (string-equal (system-name) "archlinux")
      (setq gptel-model 'gemini-2.5-pro-exp-03-25
	    gptel-backend (gptel-make-gemini "Gemini"
			    :key (getenv "GEMINI_API_KEY")
			    :stream t))
    (setq gptel-backend (gptel-make-gh-copilot "Copilot")
	  gptel-model 'gemini-2.5-pro))

  (setq gptel-default-mode 'org-mode)

  (gptel-make-tool
   :name "read_buffer"                    ; javascript-style snake_case name
   :function (lambda (buffer)                  ; the function that will run
               (unless (buffer-live-p (get-buffer buffer))
		 (error "error: buffer %s is not live." buffer))
               (with-current-buffer  buffer
		 (buffer-substring-no-properties (point-min) (point-max))))
   :description "return the contents of an emacs buffer"
   :args (list '(:name "buffer"
		       :type string            ; :type value must be a symbol
		       :description "the name of the buffer whose contents are to be retrieved"
		       :category "emacs")))
  ;; --- Gptel autosave (robust) -----------------------------------


  (defvar shl/gptel-autosave-enabled t)
  (defvar-local shl/gptel--autosave-initialized nil)

  (defun shl/gptel--timestamp ()
    (format-time-string "%Y%m%dT%H%M%S"))

  (defun shl/gptel--ensure-chat-file ()
    (unless (or buffer-file-name
		shl/gptel--autosave-initialized
		(not (derived-mode-p 'org-mode)))
      (let* ((dir (file-name-as-directory shl/gptel-chat-directory))
             (fname (concat (shl/gptel--timestamp) ".org"))
             (path (expand-file-name fname dir)))
	(make-directory dir t)
	(when (= (point-min) (point-max))
          (insert (format "#+title: Chat %s\n#+created: %s\n\n"
                          (format-time-string "%F %T")
                          (format-time-string "%F %T"))))
	(write-region (point-min) (point-max) path nil 'silent)
	(set-visited-file-name path t)
	(set-buffer-modified-p nil)
	(setq shl/gptel--autosave-initialized t)
	(message "Gptel transcript -> %s"
		 (file-relative-name path org-directory)))))

  (defun shl/gptel--maybe-annotate-model ()
    (when (and (derived-mode-p 'org-mode)
               (boundp 'gptel-model) gptel-model
               (save-excursion
		 (goto-char (point-min))
		 (not (re-search-forward "^#\\+model:" (line-end-position 5) t))))
      (save-excursion
	(goto-char (point-min))
	(forward-line 1)
	(insert (format "#+model: %s\n" gptel-model)))))

  (defun shl/gptel--autosave-after-response (&rest _)
    (when (and shl/gptel-autosave-enabled
               (derived-mode-p 'org-mode))
      (shl/gptel--ensure-chat-file)
      (shl/gptel--maybe-annotate-model)
      (when (buffer-modified-p)
	(save-buffer))))

  (with-eval-after-load 'gptel
    ;; Preferred hook (if defined in your version):
    (when (boundp 'gptel-post-response-hook)
      (add-hook 'gptel-post-response-hook #'shl/gptel--autosave-after-response))
    ;; Fallback advice for older internal insertion function:
    (when (and (not (boundp 'gptel-post-response-hook))
               (fboundp 'gptel--insert-response))
      (advice-add 'gptel--insert-response :after #'shl/gptel--autosave-after-response))
    (add-hook 'gptel-mode-hook #'shl/gptel--ensure-chat-file))

  (defun shl/gptel--maybe-enable ()
    "Enable gptel-mode for Org files under shl/gptel-chat-directory."
    (when (and (derived-mode-p 'org-mode)
               buffer-file-name
               (string-prefix-p (shl/gptel--chat-dir)
				(file-name-directory (expand-file-name buffer-file-name)))
               (fboundp 'gptel-mode))
      (gptel-mode 1)))

  (add-hook 'find-file-hook #'shl/gptel--maybe-enable)

  (defun shl/gptel-chat-latest ()
    "Open the most recently modified chat file."
    (interactive)
    (let* ((dir (shl/gptel--chat-dir))
           (files (directory-files dir t "\\.org$" t))
           (latest (car (sort files
                              (lambda (a b)
				(time-less-p (nth 5 (file-attributes b))
                                             (nth 5 (file-attributes a))))))))
	  (if latest
              (find-file latest)
	    (message "No chat files yet.")))))


(shl/leader
  "h f" '(shl/gptel-chat-open     :which-key "chats browse")
  "h s" '(shl/gptel-chat-ripgrep  :which-key "chats search")
  "h l" '(shl/gptel-chat-latest :which-key "latest chat")
  "h r" '(gptel-add :which-key "add region")
  "h s" '((lambda () (interactive)
            (gptel-request
             (format "Explain and improve types in:\n%s"
                     (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (buffer-substring-no-properties (point-min) (point-max))))))
          :which-key "analyze code")
  "h h" #'shl/gptel-minibuffer
  "h m" #'gptel-menu
  "h n" #'gptel
  "h t" #'gptel-org-set-topic
  "h A" '(lambda () (interactive)
           (setq shl/gptel-autosave-enabled (not shl/gptel-autosave-enabled))
           (message "Gptel autosave: %s"
                    (if shl/gptel-autosave-enabled "ON" "OFF"))))


(use-package mcp
  :ensure t
  :after gptel
  :hook (elpaca-after-init . (lambda ()
			       (mcp-hub-connect)
			       (message "MCP: connected: %s" (mch-hub-list-connections))))
  :custom (mcp-hub-servers
           `(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/home/slinde/data")))
             ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
	     ;; ("serena" . (:command "uvx" :args ("--from" "git+https://github.com/oraios/serena" "serena" "start-mcp-server")))
	     ("deepwiki" :url "https://mcp.deepwiki.com/sse")
	     ("ddg-search" . (:command "uvx" :args ("duckduckgo-mcp-server")))
             ;; ("context7" . (:command "npx" :args ("-y" "@upstash/context7-mcp@latest")))  ;; deep-wiki should be better
             ("sequential-thinking" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-sequential-thinking")))))
  :config
  (require 'mcp-hub)
  (require 'gptel-integrations))

(use-package ragmacs
   :ensure (:host github :repo "positron-solutions/ragmacs")
   :after gptel
   :defer
   :init
   (gptel-make-preset 'introspect
     :pre (lambda () (require 'ragmacs))
     :system
     "You are pair programming with the user in Emacs and on Emacs.
 
 Your job is to dive into Elisp code and understand the APIs and
 structure of elisp libraries and Emacs.  Use the provided tools to do
 so, but do not make duplicate tool calls for information already
 available in the chat.
 
 <tone>
 1. Be terse and to the point.  Speak directly.
 2. Explain your reasoning.
 3. Do NOT hedge or qualify.
 4. If you don't know, say you don't know.
 5. Do not offer unprompted advice or clarifications.
 6. Never apologize.
 7. Do NOT summarize your answers.
 </tone>
 
 <code_generation>
 When generating code:
 1. Always check that functions or variables you use in your code exist.
 2. Also check their calling convention and function-arity before you use them.
 3. Write code that can be tested by evaluation, and offer to evaluate
 code using the `elisp_eval` tool.
 </code_generation>
 
 <formatting>
 1. When referring to code symbols (variables, functions, tags etc) enclose them in markdown quotes.
    Examples: `read_file`, `getResponse(url, callback)`
    Example: `<details>...</details>`
 2. If you use LaTeX notation, enclose math in \( and \), or \[ and \] delimiters.
 </formatting>"
     :tools '("introspection")))

(provide 'init-ai)
;;; init-ai.el ends here

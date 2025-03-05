;; Author: Syl Montalvo  <psicotherapy@gmail.com>
;; Version: 0.1
;; Keywords: ai, tools, grok

;;; Commentary:
;; This package provides a simple interface to send prompts to the Grok AI
;; (built by xAI) and display the response in an Emacs buffer.

;;; Code:

(require 'url)
(require 'json)

(defcustom grok-api-endpoint "https://api.x.ai/v1"
  "The API endpoint for Grok AI."
  :type 'string
  :group 'grok-prompt)

(defcustom grok-api-key nil
  "Your API key for authenticating with the Grok AI service."
  :type 'string
  :group 'grok-prompt)

(defun grok-send-prompt (prompt)
  "Send PROMPT to Grok AI and display the response in a buffer."
  (interactive "sEnter your prompt for Grok: ")
  (unless grok-api-key
    (error "Please set `grok-api-key` to use this feature"))
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Authorization" . ,(concat "Bearer " grok-api-key))))
        (url-request-data
         (encode-coding-string
          (json-encode `(("prompt" . ,prompt)))
          'utf-8)))
    (url-retrieve
     grok-api-endpoint
     #'grok-handle-response
     nil t t)))

(defun grok-handle-response (status)
  "Handle the response from Grok AI and display it in a buffer.
STATUS is the HTTP status plist returned by `url-retrieve'."
  (let ((buffer (get-buffer-create "*Grok Response*")))
    (with-current-buffer buffer
      (erase-buffer)
      (if (plist-get status :error)
          (insert (format "Error: %s" (plist-get status :error)))
        (goto-char (point-min))
        (search-forward "\n\n" nil t) ;; Skip HTTP headers
        (let* ((json-data (json-read))
               (response (alist-get 'response json-data)))
          (insert (or response "No response received from Grok")))))
    (pop-to-buffer buffer)
    (goto-char (point-min))))

(defun grok-prompt-region ()
  "Send the current region as a prompt to Grok AI."
  (interactive)
  (if (use-region-p)
      (grok-send-prompt (buffer-substring-no-properties
                         (region-beginning)
                         (region-end)))
    (message "No active region to send as a prompt")))

;; Keybindings (optional)
(global-set-key (kbd "C-c g p") 'grok-send-prompt)
(global-set-key (kbd "C-c g r") 'grok-prompt-region)

(provide 'grok-prompt)

;;; grok-prompt.el ends here

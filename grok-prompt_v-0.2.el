;;; grok-prompt.el --- Send prompts to Grok AI from Emacs -*- lexical-binding: t -*-
;; Author | Curator: Syl Montalvo <psicotherapy@gmail.com>
;; Version: 0.2
;; Keywords: ai, tools, grok
;;; Commentary:
;; This package provides an interface to send prompts to the Grok AI
;; with support for system messages, model selection,
;; streaming, and temperature control.
;;; Code:
(require 'url)
(require 'json)
(defcustom grok-api-endpoint "https://api.xai.com/grok"
  "The API endpoint for Grok AI." :type 'string :group 'grok-prompt)
(defcustom grok-api-key nil
  "Your API key for authenticating with the Grok AI service."
  :type 'string
  :group 'grok-prompt)

(defcustom grok-default-model "grok-3"
  "The default model to use for Grok API requests."
  :type 'string
  :group 'grok-prompt)

(defcustom grok-default-temperature 0.7
  "The default temperature for Grok API requests."
  :type 'float
  :group 'grok-prompt)

(defcustom grok-system-message "You are Grok, created by xAI. Provide concise, helpful answers."
  "The default system message to set the AI's behavior."
  :type 'string
  :group 'grok-prompt)

(defun grok-send-prompt (prompt &optional model temperature stream)
  "Send PROMPT to Grok AI and display the response in a buffer. 
MODEL, TEMPERATURE, and STREAM are optional parameters."
  (interactive "sEnter your prompt for Grok: ")  
  (unless grok-api-key
    (error "Please set `grok-api-key` to use this feature"))
  (let* ((model (or model grok-default-model))
         (temperature (or temperature grok-default-temperature))
         (stream (or stream nil)) ;; Default to non-streaming
         (messages `((("role" . "system") ("content" . ,grok-system-message))
                     (("role" . "user") ("content" . ,prompt))))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " grok-api-key))))
         (url-request-data
          (encode-coding-string
           (json-encode `(("messages" . ,messages)
                          ("model" . ,model)
                          ("temperature" . ,temperature)
                          ("stream" . ,stream)))
           'utf-8)))
    (if stream
        (message "Streaming not yet fully supported in this version")
      (url-retrieve
       grok-api-endpoint
       #'grok-handle-response
       nil t t))))

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
                (response (alist-get 'content
                                     (aref (alist-get 'messages json-data) 0))))
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

# shiny-garbanzo
### Documentation for Grok AI Emacs Interface

#### Overview
This package provides a simple interface to send prompts to the Grok AI (built by xAI) and display the responses in an Emacs buffer.

#### Custom Variables
- **`grok-api-endpoint`**: 
  - Type: String
  - Description: The API endpoint for Grok AI. Default is set to `"https://api.xai.com/grok"`.

- **`grok-api-key`**: 
  - Type: String
  - Description: Your API key for authenticating with the Grok AI service. This must be set for the interface to function.

#### Functions

- **`grok-send-prompt`**:
  - **Usage**: `(grok-send-prompt PROMPT)`
  - **Description**: Sends a user-defined prompt to Grok AI and displays the response in a new buffer. If the API key is not set, it raises an error.
  - **Interactive**: Yes, prompts the user to enter a string.

- **`grok-handle-response`**:
  - **Usage**: `(grok-handle-response STATUS)`
  - **Description**: Handles the HTTP response from Grok AI. It processes the response, extracts the relevant data, and displays it in a buffer named "*Grok Response*". If an error occurs, it displays the error message.
  - **Parameters**: 
    - `STATUS`: A plist containing the HTTP status returned by `url-retrieve`.

- **`grok-prompt-region`**:
  - **Usage**: `(grok-prompt-region)`
  - **Description**: Sends the currently selected region of text as a prompt to Grok AI. If no region is selected, it notifies the user.
  - **Interactive**: Yes.

#### Keybindings (Optional)
- **`C-c g p`**: Binds the `grok-send-prompt` function to this key combination for quick access.
- **`C-c g r`**: Binds the `grok-prompt-region` function to this key combination for sending selected text.

### Conclusion
This documentation provides a concise overview of the Grok AI Emacs interface, detailing its custom variables, functions, and keybindings. Contributors can use this information to understand the functionality and contribute to the development of the project. Feel free to join.

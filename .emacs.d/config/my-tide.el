;;; Code:

(defun setup-tide-mode ()
  "Setup TIDE."
  (interactive)
  (when (or
         (string= (file-name-extension buffer-file-name) "ts")
         (string= (file-name-extension buffer-file-name) "tsx"))

    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))

    ;;(setq tide-tsserver-executable "/usr/bin/tsserver") ;; use global tsserver

    (set-face-attribute 'flycheck-error nil :foreground "black" :background "red")
    (global-prettify-symbols-mode +1)
    (eldoc-mode +1)
    ;;  (auto-complete-mode t)
    ;;  (turn-on-auto-fill) ;; buggy!
    
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1)
    )
  )

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'web-mode-hook #'setup-tide-mode) ;; Won't do: will use tide for css, js, etc
;;(add-hook 'find-file-hook 'setup-tide-mode)

;; format options
(setq tide-format-options '(
        :indentSize 2
        :tabSize 2
        :convertTabsToSpaces t
        :indentStyle 2 ;; smart indent style
        :insertSpaceAfterCommaDelimiter t
        :insertSpaceAfterSemicolonInForStatements t
        :insertSpaceBeforeAndAfterBinaryOperators t
        :insertSpaceAfterKeywordsInControlFlowStatements t
        :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
        :insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis nil
        :insertSpaceAfterOpeningAndBeforeClosingNonemptyBrackets nil
        :insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces nil
        :insertSpaceAfterOpeningAndBeforeClosingJsxExpressionBraces nil
        :placeOpenBraceOnNewLineForFunctions nil
        :placeOpenBraceOnNewLineForControlBlocks nil
))

(setq typescript-expr-indent-offset 0)
(setq typescript-indent-level 2)

;;; dart-mode.el --- simple highlight major mode for editing Dart. -*- coding: utf-8; lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Code:

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq dart-font-lock-keywords
      (let* (
            ;; define several category of keywords
            (x-keywords '("break" "default" "do" "else" "for" "if" "return" "while" "abstract" "const" "factory" "final" "operator" "static" "typedef" "var" "switch" "case" "try" "catch" "throw" "continue" "true" "false" "null"))
            (x-types '("double" "num" "int" "List" "Map" "String" "void" "bool" "dynamic"))
            (x-builtins '("class" "interface" "is" "is!" "new" "const" "extends" "implements" "await" "async" "get" "set" "this"))

            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            (x-types-regexp (regexp-opt x-types 'words))
            (x-decorator-regexp "@[A-Za-z0-9_]+")
            (x-constants-regexp "[A-Z_][A-Z_][A-Z_][A-Z_]+")
            (x-func-regexp "[A-Za-z0-9_]+\(\\|\)")
            (x-private-var-regexp "_[a-zA-Z0-9]+")
;;            (x-custom-type-regexp "\s[A-Z][a-zA-Z0-9]+(<\\|>)?\s")
            (x-builtins-regexp (regexp-opt x-builtins 'words)))

        `(
          (,x-types-regexp . font-lock-type-face)
          (,x-builtins-regexp . font-lock-builtin-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          (,x-decorator-regexp . font-lock-keyword-face)
          (,x-constants-regexp . font-lock-constant-face)
;;          (,x-custom-type-regexp . font-lock-type-face)
          (,x-func-regexp . font-lock-function-name-face)
          (,x-private-var-regexp . font-lock-variable-name-face)

          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

;;;###autoload
(define-derived-mode dart-mode c-mode "Dart"
  "Major mode for editing Dart"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((dart-font-lock-keywords))))

;; add the mode to the `features' list
(provide 'dart-mode)

;;; dart-mode.el ends here

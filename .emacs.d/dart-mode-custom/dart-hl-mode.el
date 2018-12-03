;;; dart-hl-mode.el --- sample major mode for editing LSL. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2017, by you

;; Author: your name ( your email )
;; Version: 2.0.13
;; Created: 26 Jun 2015
;; Keywords: languages
;; Homepage: http://ergoemacs.org/emacs/elisp_syntax_coloring.html

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; short description here

;; full doc on how to use here

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
            (x-func-regexp "[A-Za-z0-9_]+\(\\|\)")
            (x-private-var-regexp "_[a-zA-Z0-9]+")
;;            (x-custom-type-regexp "\s[A-Z][a-zA-Z0-9]+(<\\|>)?\s")
            (x-builtins-regexp (regexp-opt x-builtins 'words)))

        `(
          (,x-types-regexp . font-lock-type-face)
          (,x-builtins-regexp . font-lock-builtin-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          (,x-decorator-regexp . font-lock-keyword-face)
;;          (,x-custom-type-regexp . font-lock-type-face)
          (,x-func-regexp . font-lock-function-name-face)
          (,x-private-var-regexp . font-lock-variable-name-face)

          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

;;;###autoload
(define-derived-mode dart-hl-mode c-mode "darthl mode"
  "Major mode for editing Dart"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((dart-font-lock-keywords))))

;; add the mode to the `features' list
(provide 'dart-hl-mode)

;;; dart-hl-mode.el ends here

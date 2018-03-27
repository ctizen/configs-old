Dart Mode
=========
Dart Mode is a major mode for editing Dart files in Emacs.

## Installation

1.  Install dart-mode by cloning this repository.
    
	Add  dart-mode.el to your load path.
	
	Add helm-dart.el to your load path to get helm completions for the quick fix
    feature. 
	
1.	The analysis server is turned on by default. To turn it off, add the
      following to your `.emacs` init file
	  ```
     (setq dart-enable-analysis-server nil)
	```
	
1.  OPTIONAL: To enable on-the-fly syntax checking, add the
    following to your `.emacs` file:
    ```
    (add-hook 'dart-mode-hook 'flycheck-mode)
    ```

1.  OPTIONAL: To enable imenu support, add the
    following to your `.emacs` file:
    ```
	(add-hook 'dart-mode-hook
          (lambda ()
            (setq imenu-create-index-function #'dart-imenu-index)))
    ```
1.  OPTIONAL: To view the class hierarchy install tree-mode

1.  Features & functions: Provides the following functions
    1. dartfmt: can be called on a save file hook
	   To vary the line length that dartfmt uses  
	   ```
	    (setq dartfmt-args (quote ("-l 90")))	
        will set it to 90 characters. Default is 80 characters.
	   ```
    2. dart-jump-to-defn: Navigate source code (bound to M-.)
    3. dart-type-hierarchy: bind to a key and show the type hierachy
    4. dart-hover-information: bind to key and show dartdocs for the element
    5. dart-imports: Organizes all of the directives - removes unused imports 
	   and sorts directives
    6. dart-sort-members: Sort all of the directives, unit and class members
	   of the given Dart file.
    7. helm-dart-quick-fix: apply quick fix (requires helm-dart.el)

1. Use [company-dart](https://github.com/sid-kurias/company-dart) to get intellisense
   like support.

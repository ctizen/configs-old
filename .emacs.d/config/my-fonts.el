;; Fonts
(defvar *selected-font* (if (eq system-type 'darwin)
                            "Roboto Mono Light for Powerline-12"
                          "Terminus-11"))
(defvar *selected-font-family* (if (eq system-type 'darwin)
                                   "Roboto Mono Light for Powerline"
                                 "Terminus"))
(set-frame-font *selected-font*)
(set-face-attribute 'default t :font *selected-font* )
(set-frame-font *selected-font* nil t)
(if (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(font . "Roboto Mono Light for Powerline-12"))
    (add-to-list 'default-frame-alist '(font . "Terminus-11")))

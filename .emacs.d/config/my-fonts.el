;; Fonts
(defvar *selected-font* (if (eq system-type 'darwin)
                            "PragmataPro-14"
                          "PragmataPro-12"))
(defvar *selected-font-family* "PragmataPro")
(set-frame-font *selected-font*)
(set-face-attribute 'default t :font *selected-font* )
(set-frame-font *selected-font* nil t)
(if (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(font . "PragmataPro-14"))
    (add-to-list 'default-frame-alist '(font . "PragmataPro-12")))

;;; ../dotfiles/emacs/.doom.d/clomacs.el -*- lexical-binding: t; -*-

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

(require 'clomacs)
(require 'cl-lib)
(require 'dash)

(clomacs-defun get-property System/getProperty)

(message (get-property "java.version"))


(comment
 (clomacs-httpd-start)

 )

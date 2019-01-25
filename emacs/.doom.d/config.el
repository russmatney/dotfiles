;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Author     : russmatney
;; CreatedAt  : 15 April 2018
;; ModifiedAt : 15 April 2018
;; Status     : Usable
;;
;; Configuration for doom and emacs at large.
;;
;; Misc and otherwise uncategorized details are located here.
;; Note the use of `(load! +filename)` to load sibling files.
;;

;;; Private keys'n'such
;;(load! "+private")

;; (defvar +russ-dir (file-name-directory load-file-name))

;; Basic Config
(setq backup-directory-alist `(("." . "~/.emacs-tmp/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-tmp/" t)))

;; Spaces over tabs
(setq c-basic-indent 2)
(setq c-default-style "linux")
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

;; Auto revert-mode
(global-auto-revert-mode t)

;; Turn off line wrapping
(setq-default truncate-lines 1)

;; 80 chars! be diligent!
(setq whitespace-line-column 80
      whitespace-style
      '(face trailing lines-tail))
(setq-default fill-column 80)
(auto-fill-mode 1)

;; turn on whitespace mode
(global-whitespace-mode t)

;; turn on whitespace cleanup
(add-hook! 'before-save-hook 'whitespace-cleanup)

(setq +ivy-buffer-icons t)

;; zen-mode for focused writing
(def-package! zen-mode)

(set-face-attribute 'default nil :height 100)

;; PATH
(add-to-list 'exec-path "/home/russ/.nix-profile/bin")
(add-to-list 'exec-path "/home/russ/.pyenv/shims")

;; Add '--hidden' to rg command to include hidden files in search
;; Note that `echo ".git/" >> ~/.ignore` will exclude .git from these searches
(setq counsel-rg-base-command
    "rg -zS --hidden --no-heading --line-number --color never %s .")

;; load bindings
(load! "+bindings")

;; load commands
(load! "+commands")

;; load programming language configs
(load! "+langs")

;; load prose tools (includes markdown)
(load! "+org")

;; TODO get working with toc.org
(setq org-publish-project-alist
      '(("todo"
         :base-directory "~/Dropbox/todo/"
         :base-extension "org"
         :publishing-directory "~/todo-html/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 2
         :auto-preamble t)))
; (org-publish-current-project)

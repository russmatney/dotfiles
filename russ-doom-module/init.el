;;; private/russ/init.el -*- lexical-binding: t; -*-

(setq user-mail-address "russell.matney@gmail.com"
      user-full-name    "Russell matney")

;; host-specific settings
(pcase (system-name)
  ("proteus"
   (setq +doom-modeline-height 25
         doom-font (font-spec :family "Fira Mono" :size 10)
         doom-variable-pitch-font (font-spec :family "Fira Sans" :size 10)
         doom-unicode-font (font-spec :family "DejaVu Sans Mono" :size 10)
         doom-line-number-lpad 3))
  ("halimede"
   (setq +doom-modeline-height 27))
  ;; ("nereid")
  ;; ("io")
  ;; ("sao")
  )

;; Basic Config
(setq backup-directory-alist `(("." . "~/.emacs-tmp/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-tmp/" t)))

;; Spaces over tabs
(setq c-basic-indent 2)
(setq c-default-style "linux")
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

;; Auto revert-mode. Look ma, no hands...
(global-auto-revert-mode t)

;; Turn off line wrapping
(setq-default truncate-lines 1)

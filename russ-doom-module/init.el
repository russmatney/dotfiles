;;; private/hlissner/init.el -*- lexical-binding: t; -*-

;; I've swapped these keys on my keyboard
;(setq x-super-keysym 'meta
;      x-alt-keysym   'alt)

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

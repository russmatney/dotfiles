;;; ../dotfiles/emacs/.doom.d/+ivy.el -*- lexical-binding: t; -*-

;;;;;;;;;;
;; ivy

(setq +ivy-buffer-icons t)

;; Add '--hidden' to rg command to include hidden files in search
;; Note that `echo ".git/" >> ~/.ignore` will exclude .git from these searches
(setq counsel-rg-base-command
      "rg -zS -T jupyter -T svg -T lock -T license --no-heading --line-number --color never %s .")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy/Counsel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 :after ivy
 :map ivy-minibuffer-map
 [escape] #'keyboard-escape-quit
 "A-v" #'yank
 "A-z" #'undo
 "M-v" #'yank
 "M-z" #'undo
 "C-r" #'evil-paste-from-register
 "C-k" #'ivy-previous-line
 "C-j" #'ivy-next-line
 "C-l" #'ivy-alt-done
 "C-h" #'ivy-backward-kill-word
 "C-w" #'ivy-backward-kill-word
 "C-u" #'ivy-kill-line
 "C-b" #'backward-word
 "C-f" #'forward-word

 :map ivy-switch-buffer-map
 "C-k" #'ivy-previous-line
 "C-j" #'ivy-next-line
 "C-l" #'ivy-alt-done
 "C-h" #'ivy-backward-kill-word)

(map!
 :desc "swiper"                :nv "/"   #'evil-ex-search-forward
 (:leader
  :desc "Imenu"                 :nv "i"   #'imenu
  :desc "Imenu across buffers"  :nv "I"   #'imenu-anywhere
  :desc "swiper"                :nv "/"   #'swiper))

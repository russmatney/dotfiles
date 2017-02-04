;;;; A bunch of stuff that belongs ahead of external dependencies
;;;; This prevents them from failing when there's an external problem
;;;; (and not expanding the screen enough for you to read it, for example)


;;; zoom-frm fixes a `text-scale-adjust` bug in 'linum line numbers. (does it?)
(require 'zoom-frm)
;;; Cmd-+ to make text larger
(global-set-key (kbd "s-+") 'zoom-frm-in)
(global-set-key (kbd "s-=") 'zoom-frm-in)
;;; Cmd-- to make text smaller
(global-set-key (kbd "s--") 'zoom-frm-out)
(global-set-key (kbd "s-_") 'zoom-frm-out)

;; auto-save-files not in same dir as original
(setq backup-directory-alist `(("." . "~/.emacs/auto-save-list")))

;; Hide the menu-bar
(setq ns-auto-hide-menu-bar t)

;; Native App Settings
(tool-bar-mode -1)

;; Disable GUI scrollbars
(scroll-bar-mode -1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Change font settings
(set-frame-font "Operator Mono 12")

;; Add transparency
(set-frame-parameter (selected-frame) 'alpha '(99 . 99))
(add-to-list 'default-frame-alist '(alpha . (99 . 99)))

;; Fullscreen settings (@wpcarro)
(setq ns-use-native-fullscreen nil)
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)

;; default full screen
(setq default-frame-alist
    '((fullscreen . fullboth) (fullscreen-restore . fullheight)))

;; Window movement
;; (global-set-key (kbd "C-l C-l") 'windmove-right)
;; (global-set-key (kbd "C-l l") 'windmove-right)
;; (global-set-key (kbd "C-h C-h") 'windmove-left)
;; (global-set-key (kbd "C-h h") 'windmove-left)
;; (global-set-key (kbd "C-k C-k") 'windmove-up)
;; (global-set-key (kbd "C-k k") 'windmove-up)
;; (global-set-key (kbd "C-j C-j") 'windmove-down)
;; (global-set-key (kbd "C-j j") 'windmove-down)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-j") 'windmove-down)

;; Scrolling Settings (@wpcarro)
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; line numbers
(global-linum-mode t)

;; line wrap
(setq-default word-wrap t)
(toggle-truncate-lines -1)





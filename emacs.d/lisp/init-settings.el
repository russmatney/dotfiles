;;; init-settings.el --- Settings and general configuration
;;; Commentary:
;;; Code:

(fset 'yes-or-no-p 'y-or-n-p)


(setq inhibit-startup-screen t)
(split-window-right)
(find-file-other-window "~/Dropbox/todo/todo.org")

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(global-auto-revert-mode t)

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
)

(setq-default show-trailing-whitespace t)

(global-hl-line-mode 1)

(setq visible-bell nil)
;; (setq visible-bell 1)

(use-package zoom-frm
  :config
  (global-set-key (kbd "s-=") 'zoom-frm-in)
  (global-set-key (kbd "s--") 'zoom-frm-out)
  (global-set-key (kbd "s-0") 'zoom-frm-unzoom)
)

;; auto-save-files not in same dir as original
(setq backup-directory-alist `(("." . "~/.emacs/auto-save-list")))

(setq ns-auto-hide-menu-bar t)

(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq-default indent-tabs-mode nil)

(set-frame-font "Operator Mono 12")

(use-package seethru
  :config
  (global-set-key (kbd "s-+") (lambda () (interactive) (seethru-relative 5)))
  (global-set-key (kbd "s-_") (lambda () (interactive) (seethru-relative -5)))
  (global-set-key (kbd "s-)") (lambda () (interactive) (seethru 100)))
  (global-set-key (kbd "s-(") (lambda () (interactive) (seethru 0)))
)

(setq ns-use-native-fullscreen nil)
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)

;; default full screen
(setq default-frame-alist
    '((fullscreen . fullboth) (fullscreen-restore . fullheight)))

;; Scrolling Settings
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; line numbers
(global-linum-mode t)

;; line wrap
(setq-default word-wrap t)
;; (toggle-truncate-lines 1)

(column-number-mode)
(show-paren-mode)
(eldoc-mode)

(setq recentf-max-saved-items 50)
(run-at-time (current-time) 300 'recentf-save-list)

(provide 'init-settings)
;;; init-settings.el ends here

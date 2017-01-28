;;; init.el --- Begin Emacs config.

;;; Commentary:

;; used packages broken into individual init-*.el files

;;; Code:

;; Update package-archive lists
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

;; Always download missing use-package packages
(setq use-package-always-ensure t)

;; set $PATH vars on shell commands
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (atom-one-dark)))
 '(custom-safe-themes
   (quote
    ("08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" default)))
 '(package-selected-packages
   (quote
    (use-package ack xpm flycheck helm-company discover helm-projectile magit evil-tutor helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-file "~/dotfiles/emacs.d/init-evil.el")
(load-file "~/dotfiles/emacs.d/init-helm.el")
(load-file "~/dotfiles/emacs.d/init-alchemist.el")
(load-file "~/dotfiles/emacs.d/init-company.el")
(load-file "~/dotfiles/emacs.d/init-projectile.el")


;; Hide the menu-bar
(setq ns-auto-hide-menu-bar t)

;; Native App Settings
(tool-bar-mode -1)

;; Disable GUI scrollbars
(scroll-bar-mode -1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Change font settings
(set-frame-font "Operator Mono 14")

;; Add transparency
(set-frame-parameter (selected-frame) 'alpha '(99 . 99))
(add-to-list 'default-frame-alist '(alpha . (99 . 99)))

;; Fullscreen settings (@wpcarro)
(setq ns-use-native-fullscreen nil)
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)

;; Window movement
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-j") 'windmove-down)

;; Scrolling Settings (@wpcarro)
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;;; init-use-package.el ends here


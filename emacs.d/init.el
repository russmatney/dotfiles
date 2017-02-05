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
    ("98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" default)))
 '(initial-buffer-choice "~/Dropbox/todo/2017-january.org")
 '(package-selected-packages
   (quote
    (highlight-parentheses jade-mode zoom-frm material-theme popwin ace-window evil-magit highlight-indent-guides atom-one-dark-theme flycheck-credo flycheck-mix evil-surround evil-matchit helm-swoop ag helm-ag neotree use-package ack xpm flycheck helm-company discover helm-projectile magit evil-tutor helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; nothing else before starter kit
(load-file "~/dotfiles/emacs.d/starter-kit.el")
 
(global-auto-revert-mode t)

;; org mode
(setq org-todo-keywords
       '((sequence "TODO" "IDEA" "WORKFLOW" "STORYX" "TRIAGE" "JACK" "BLOG" "POST" "OPENSOURCE" "|" "TODOLOL" "INACTIVE" "REMINDER" "DONE" "BLOCKED" "SCHEDULED")))



(load-file "~/dotfiles/emacs.d/evil.el")
(load-file "~/dotfiles/emacs.d/projectile.el")
(load-file "~/dotfiles/emacs.d/helm.el")
(load-file "~/dotfiles/emacs.d/alchemist.el")
(load-file "~/dotfiles/emacs.d/company.el")
(load-file "~/dotfiles/emacs.d/neotree.el")
(load-file "~/dotfiles/emacs.d/swoop.el")
(load-file "~/dotfiles/emacs.d/flycheck.el")

(add-to-list 'display-buffer-alist
             `(,(rx bos (or "*alchemist test report*"
                            "*alchemist mix*"
                            "*alchemist help*"))
                    (display-buffer-reuse-window)
                    (inhibit-switch-frame t)
                    (reusable-frames . visible)))

(add-hook 'elixir-mode
          (lambda () (modify-syntax-entry ?_ "w")))

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
)

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
)
(use-package avy)

(use-package ag)
(use-package helm-ag)


(provide 'init)

;;; init.el ends here

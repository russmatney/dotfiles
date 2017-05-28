;;; Package --- Summary
;;; Commentary:
;;;   Pulled and refactored from: https://github.com/rranelli/emacs-dotfiles
;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" "~/dotfiles/emacs.d"))

(setq rm/init-errors nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#181e26" "#ff665c" "#7bc275" "#ECBE7B" "#51afef" "#C57BDB" "#46D9FF" "#DFDFDF"])
 '(custom-enabled-themes (quote (smart-mode-line-dark)))
 '(custom-safe-themes
   (quote
    ("63b822ccd7a1928a7cbc88037dddf7b74b2f8a507e1bccd7281f20646f72cd0a" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "7f4b67cb8aff9eb76ef818b3e41ed5f03581799f8e31899c93ec85b0ef049ceb" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "227e2c160b0df776257e1411de60a9a181f890cfdf9c1f45535fc83c9b34406b" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" default)))
 '(fci-rule-color "#3E4451")
 '(helm-ag-ignore-patterns (quote (".*//doc//.*'")))
 '(jdee-db-active-breakpoint-face-colors (cons "#181e26" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#181e26" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#181e26" "#3D3D48"))
 '(org-ellipsis "  ")
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (markdown-mode helm-company f dockerfile-mode clojure-mode bookmark+ avy multi-term with-editor s projectile magit helm-core helm git-commit frame-cmds flycheck evil diminish dash company async all-the-icons yasnippet zoom-frm which-key use-package smart-mode-line slime seethru popwin popup-imenu neotree less-css-mode imenu-anywhere iedit highlight-indent-guides helm-swoop helm-projectile helm-ls-git helm-ag flycheck-mix flycheck-credo exec-path-from-shell evil-surround evil-nerd-commenter evil-matchit evil-magit evil-leader elm-mode doom-themes atom-one-dark-theme alchemist ag ace-window)))
 '(popwin:special-display-config
   (quote
    (help-mode
     ("^\\*helm.*\\*$" :regexp t :height 30)
     ("^\\*helm.*\\*$" :regexp t)
     ("*Miniedit Help*" :noselect t)
     (completion-list-mode :noselect t)
     (compilation-mode :noselect t)
     (grep-mode :noselect t)
     (occur-mode :noselect t)
     ("*Pp Macroexpand Output*" :noselect t)
     "*Shell Command Output*" "*vc-diff*" "*vc-change-log*"
     (" *undo-tree*" :width 60 :position right)
     ("^\\*anything.*\\*$" :regexp t)
     "*slime-apropos*" "*slime-macroexpansion*" "*slime-description*"
     ("*slime-compilation*" :noselect t)
     "*slime-xref*"
     (sldb-mode :stick t)
     slime-repl-mode slime-connection-list-mode)))
 '(vc-annotate-background "#000000")
 '(vc-annotate-color-map
   (quote
    ((20 . "#B6E63E")
     (40 . "#c4db4e")
     (60 . "#d3d15f")
     (80 . "#E2C770")
     (100 . "#ebb755")
     (120 . "#f3a73a")
     (140 . "#FD971F")
     (160 . "#fb713a")
     (180 . "#fa4b56")
     (200 . "#F92672")
     (220 . "#f33260")
     (240 . "#ed3f4e")
     (260 . "#E74C3C")
     (280 . "#dd6a60")
     (300 . "#d38885")
     (320 . "#c9a6aa")
     (340 . "#C0C5CF")
     (360 . "#C0C5CF"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'init-bootstrap)
(rr/safe-load-init-files)

(message "======================================")
(message (if rm/init-errors
             (mapconcat #'identity rm/init-errors "\n")
           "Init.el finished with no errors."))
(message "======================================")
;;; init.el ends here

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
   [("#0d0d0d" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#c9b4cf" "#8abeb7" "#ffffff")])
 '(custom-enabled-themes (quote (smart-mode-line-dark)))
 '(custom-safe-themes
   (quote
    ("611e38c2deae6dcda8c5ac9dd903a356c5de5b62477469133c89b2785eb7a14d" "0f0022c8091326c9894b707df2ae58dd51527b0cf7abcb0a310fb1e7bda78cd2" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "ed763cdf0b7c523cdc094ba137080e7f2c4e7a28303e0cbeb0eda159f964f1b6" "16fd69242d5383a431bc49ed3b567dbce148a4991242baa11ee6367ca93705e2" "c9321e2db48a21fc656a907e97ee85d8cd86967855bf0bed3998bcf9195c758b" "0f97285f9e0c7d9cad04f2130859d20d6c9b3142877b2bca52d958f4f1cf346f" "63b822ccd7a1928a7cbc88037dddf7b74b2f8a507e1bccd7281f20646f72cd0a" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "7f4b67cb8aff9eb76ef818b3e41ed5f03581799f8e31899c93ec85b0ef049ceb" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "227e2c160b0df776257e1411de60a9a181f890cfdf9c1f45535fc83c9b34406b" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" default)))
 '(fci-rule-color "#5c5e5e")
 '(helm-ag-ignore-patterns (quote (".*//doc//.*'")))
 '(jdee-db-active-breakpoint-face-colors (cons "#0d0d0d" "#41728e"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#0d0d0d" "#b5bd68"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#0d0d0d" "#5a5b5a"))
 '(org-ellipsis "  ")
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (ess markdown-mode stickyfunc-enhance multi-line keyfreq rc-mode doom-nlinum doom-neotree flycheck-credo flycheck-mix exec-path-from-shell rainbow-delimiters smart-mode-line yasnippet all-the-icons neotree flycheck alchemist helm-ag helm-projectile helm-ls-git helm evil-nerd-commenter evil-surround evil-leader evil discover-my-major imenu-anywhere popup-imenu which-key ag iedit shackle ace-window seethru zoom-frm highlight-indent-guides use-package)))
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
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(vc-annotate-background "#0d0d0d")
 '(vc-annotate-color-map
   (list
    (cons 20 "#b5bd68")
    (cons 40 "#c8c06c")
    (cons 60 "#dcc370")
    (cons 80 "#f0c674")
    (cons 100 "#eab56d")
    (cons 120 "#e3a366")
    (cons 140 "#de935f")
    (cons 160 "#d79e84")
    (cons 180 "#d0a9a9")
    (cons 200 "#c9b4cf")
    (cons 220 "#ca9aac")
    (cons 240 "#cb8089")
    (cons 260 "#cc6666")
    (cons 280 "#af6363")
    (cons 300 "#936060")
    (cons 320 "#765d5d")
    (cons 340 "#5c5e5e")
    (cons 360 "#5c5e5e")))
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

;;; packages.el -*- lexical-binding: t; -*-

;; lisp
(package! dash)

;; javascript
(package! prettier-js)
(package! rjsx-mode)
(package! add-node-modules-path)

;; lsp
(package! lsp-mode)
(package! lsp-ui :recipe (:host github :repo "emacs-lsp/lsp-ui"))
(package! company-lsp)

;; rust
;;(package! lsp-rust)

;; fountain
(package! fountain-mode :recipe (:host github :repo "rnkn/fountain-mode"))

;; fennel
(package! fennel-mode :recipe (:host gitlab :repo "technomancy/fennel-mode"))
(package! friar :recipe (:host github
                         :repo "russmatney/friar"
                         :branch "develop"
                         :files (:defaults "*.lua" "*.fnl")))

;; python
(package! py-yapf)

;; clojure
(package! aggressive-indent)
(package! lispy)
(package! lispyville)

(package! flycheck-clj-kondo)
(package! ivy-cider :recipe (:host github :repo "rschmukler/ivy-cider"))

(package! company-css-classes :recipe
  (:host github :repo "russmatney/company-css-classes" :files ("*")))

(package! monroe :recipe (:host github :repo "sanel/monroe"))

(package! clojure-essential-ref)
(package! clojure-essential-ref-nov)

(package! clomacs)

;; wakatime
(package! wakatime-mode)

;; exwm
(package! exwm)

;; org
(package! doct)
(package! org-rich-yank)
(package! org-projectile)

;; org-roam
;; (package! org-roam-server)

;; ink
(package! ink-mode :recipe
  (:host github :repo "russmatney/ink-mode"))

;; nov.el
(package! nov)


;; magit
(package! magit-org-todos)

(package! browse-at-remote)

;; logs
(package! logview)

(package! maple-preview :recipe
  (:host github :repo "honmaple/emacs-maple-preview" :files ("*.el" "index.html" "static")))

(package! map :pin "bb50dbaafc0f71743bd9ffd5784258a9fd682c20")

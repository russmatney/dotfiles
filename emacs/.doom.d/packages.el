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

;; clj-refactor 3.4.2 d7384a4edddc6c875f8562e2acd4e7346a68553f
(package! clj-refactor :pin "d7384a4edddc6c875f8562e2acd4e7346a68553f")
;; cider 1.4 b2cee7fc301735b403920583cc2c23dcf70990a3
(package! cider :pin "b2cee7fc301735b403920583cc2c23dcf70990a3")
;; clojure mode 5.14 b7d08b87f6a116ff47b33ee857926b60c66c3ab7

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

;; ink
(package! ink-mode :recipe
  (:host github :repo "russmatney/ink-mode"))

;; nov.el
(package! nov)


;; magit
(package! magit-org-todos)

(package! browse-at-remote)
(package! git-link :recipe (:host github :repo "sshaw/git-link"))

;; logs
(package! logview)

(package! maple-preview :recipe
  (:host github :repo "honmaple/emacs-maple-preview" :files ("*.el" "index.html" "static")))

(package! map :pin "bb50dbaafc0f71743bd9ffd5784258a9fd682c20")
(package! xref :pin "a82f459b37b31546bf274388baf8aca79e9c30d9")

;; dirvish
(package! dirvish)

;; graphql
(package! graphql-mode)

;; task runners
(package! run-command)

(package! fabb :recipe (:local-repo "fabb" :build (:not compile)))

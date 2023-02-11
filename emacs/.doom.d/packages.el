;;; packages.el -*- lexical-binding: t; -*-


(package! compat :pin "e59e311d1ffc422f91e4e9c494598e978f5c2125" 
          :recipe (:host github :repo "emacs-compat/compat"))

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
(package! rustic :pin "53cacf5039f1d88cdd5cad98d1e9e0ad92da2615")

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

(package! clj-refactor)
(package! cider)

(package! neil :recipe (:host github :repo "babashka/neil" :files ("*.el")))

(package! aggressive-indent)

(package! evil)
(package! lispy)
(package! lispyville)

(package! flycheck-clj-kondo)
;; (package! ivy-cider :recipe (:host github :repo "rschmukler/ivy-cider"))

;; TODO point to the local version
;; TODO rename this
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

;; TODO maybe this has shipped the perf fix by now?
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam")
  :pin "e435581215a9eb8e95dde49ce1fb3c25929b80f4")

;; nov.el
(package! nov)


;; magit
(package! magit-org-todos)

(package! browse-at-remote)
(package! git-link :recipe (:host github :repo "sshaw/git-link"))

;; logs
(package! logview)

(package! map :pin "bb50dbaafc0f71743bd9ffd5784258a9fd682c20")
(package! xref :pin "a82f459b37b31546bf274388baf8aca79e9c30d9")

;; dirvish
(package! dirvish)

;; graphql
(package! graphql-mode)

;; task runners
(package! run-command)

;; local

(package! fabb :recipe (:local-repo "fabb" :build (:not compile)))

;; ink
(package! ink-mode :recipe (:local-repo "ink-mode" :build (:not compile)))

;; godot
(package! gdscript-mode :recipe (:local-repo "gdscript-mode" :build (:not compile)))

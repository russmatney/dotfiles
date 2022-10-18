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

;; clj-refactor 3.4.2 d7384a4edddc6c875f8562e2acd4e7346a68553f
(package! clj-refactor :pin "d7384a4edddc6c875f8562e2acd4e7346a68553f")
;; cider 1.4 b2cee7fc301735b403920583cc2c23dcf70990a3
(package! cider :pin "b2cee7fc301735b403920583cc2c23dcf70990a3")
;; clojure mode 5.14 b7d08b87f6a116ff47b33ee857926b60c66c3ab7

(package! aggressive-indent :pin "70b3f0add29faff41e480e82930a231d88ee9ca7")

(package! evil :pin "5826a8877736fc734ea9da7d2bba11ef2b05032c")
(package! lispy :pin "dbab5899f26fa2ee27f5c2e8b32c20f2f69242fb")
(when (featurep! :editor evil)
  (package! lispyville :pin "14ee8711d58b649aeac03581d22b10ab077f06bd"))

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

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
(package! rustic)

;; fennel
(package! fennel-mode :recipe (:host gitlab :repo "technomancy/fennel-mode"))
;; (package! friar :recipe (:host github
;;                          :repo "russmatney/friar"
;;                          :branch "develop"
;;                          :files (:defaults "*.lua" "*.fnl")))

;; python
;; (package! py-yapf)

;; clojure

;; (package! clojure-mode :pin "481ca480e8b7b6c90881f8bd8434addab1d33778")
;; (package! clj-refactor :pin "b390a17a2b0db2bb7d2b0d1cf014513cc1346e4c")
;; (package! cider :pin "9c605cd4938c0055c2766c55606f19ecbf664e8e")

;; (package! neil :recipe (:host github :repo "babashka/neil" :files ("*.el")))

(package! aggressive-indent)

(package! evil)
(package! evil-collection)
(package! lispy)
(package! lispyville)

(package! flycheck-clj-kondo)
;; (package! ivy-cider :recipe (:host github :repo "rschmukler/ivy-cider"))

;; TODO point to the local version
;; TODO rename this
;; (package! company-css-classes :recipe
;;   (:host github :repo "russmatney/company-css-classes" :files ("*")))

;; (package! monroe :recipe (:host github :repo "sanel/monroe"))

(package! clojure-essential-ref)
(package! clojure-essential-ref-nov)

;; (package! clomacs)

;; wakatime
(package! wakatime-mode)

;; exwm
;; (package! exwm)

;; org
(package! doct)
(package! org-rich-yank)
(package! org-projectile)

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam")
  :pin "74422df546a515bc984c2f3d3a681c09d6f43916")

;; nov.el
;; (package! nov)


;; magit
(package! magit-org-todos)

(package! browse-at-remote)
(package! git-link :recipe (:host github :repo "sshaw/git-link"))

;; logs
(package! logview)

;; dirvish
(package! dirvish)

;; graphql
;; (package! graphql-mode)

;; task runners
;; (package! run-command)

;; kubernetes
(package! kubernetes)
(package! kubernetes-evil)
(package! kele)

(package! elcord)

(package! gdscript-mode :pin "32086df83335ce0e5120b21b80cf7996edb2232e")

;; local packages/forks

(package! fabb :recipe (:local-repo "fabb" :build (:not compile)))
(package! ink-mode :recipe (:local-repo "ink-mode" :build (:not compile)))

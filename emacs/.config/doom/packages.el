;;; packages.el -*- lexical-binding: t; -*-


;; ignore doom snippets
;; (package! doom-snippets :ignore t)

;; formerly doom core
(package! rainbow-delimiters)

;; lisp
(package! dash)

;; javascript
(package! prettier-js)
(package! rjsx-mode)
(package! add-node-modules-path)

;; lsp
(package! lsp-mode)
(package! lsp-ui :recipe (:host github :repo "emacs-lsp/lsp-ui"))

;; rust
(package! rustic)

;; fennel
(package! fennel-mode :recipe (:host gitlab :repo "technomancy/fennel-mode"))

;; python
;; (package! py-yapf)

;; clojure

;; (package! neil :recipe (:host github :repo "babashka/neil" :files ("*.el")))

(package! aggressive-indent)

(package! lispy)
(package! lispyville)

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
(package! org-project-capture)
(package! org-roam-ui)

;; nov.el
;; (package! nov)

(package! browse-at-remote)
(package! git-link :recipe (:host github :repo "sshaw/git-link"))

;; logs
(package! logview)

;; dirvish
(package! dirvish)

;; graphql
;; (package! graphql-mode)

;; kubernetes
;; (package! kubernetes)
;; (package! kubernetes-evil)
;; (package! kele)

(package! elcord)

(package! gdscript-mode)

;; local packages/forks

(package! fabb :recipe (:local-repo "fabb" :build (:not compile)))
(package! ink-mode :recipe (:local-repo "ink-mode" :build (:not compile)))

(package! ultra-scroll :recipe (:host github :repo "jdtsmith/ultra-scroll"))


(package! aidermacs)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

(package! journalctl-mode)

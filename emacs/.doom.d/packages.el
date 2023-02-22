;;; packages.el -*- lexical-binding: t; -*-


;; https://github.com/minad/corfu/issues/290#issuecomment-1424842189
;; (package! compat :pin "e59e311d1ffc422f91e4e9c494598e978f5c2125"
;;           :recipe (:host github :repo "emacs-compat/compat"))

;; https://github.com/doomemacs/doomemacs/pull/7075/files
;; (package! vertico
;;   :recipe (:host github :repo "minad/vertico"
;;            :files ("*.el" "extensions/*.el"))
;;   :pin "f303790546edecc67aa3bd5e23c68f982f1345dd")

;; (package! orderless :pin "ae849b3d9f8c8a777e05816321ed2b00e8304447")
;; (package! consult :pin "b22a7de62ee4adf766be2f867dee8b6980902bba")
;; (package! compat :pin "2bedcb5ea91914e75d4905bc53e537b33f8f51e9")
;; (package! consult-dir :pin "ed8f0874d26f10f5c5b181ab9f2cf4107df8a0eb")
;; (when (modulep! :checkers syntax)
;;   (package! consult-flycheck :pin "51b1b48e8dad314f9c9d963376f2ea8de94b97f2"))
;; (package! embark :pin "4882b395cef98a517d530ffe483aa0dc7201158c")
;; (package! embark-consult :pin "4882b395cef98a517d530ffe483aa0dc7201158c")

;; (package! marginalia :pin "6d48ed54be87969e3ce53a24dbc63ec72ec6a91a")
;; (package! wgrep :pin "edf768732a56840db6879706b64c5773c316d619")

;; (when (modulep! +childframe)
;;   (package! vertico-posframe
;;     :recipe (:host github :repo "tumashu/vertico-posframe")
;;     :pin "790f74b49d5309dc2f0e6a438e2e89007d591d07"))

;; (package! git-commit :pin "6d325d90ba1374d48c4c7088f96864b678155f48")
;; (package! consult-lsp :pin "f8db3252c0daa41225ba4ed1c0d178b281cd3e90")
;; (package! magit :pin "6d325d90ba1374d48c4c7088f96864b678155f48")



;; lisp
(package! dash)

;; javascript
(package! prettier-js)
(package! rjsx-mode)
(package! add-node-modules-path)

;; lsp
(package! lsp-mode :pin "03e1818acae6de9a6ed89c54573f8050e4d4e463"
  ;; :recipe (:host github :repo "emacs-lsp/lsp-mode")
  )
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
  :pin "74422df546a515bc984c2f3d3a681c09d6f43916")

;; nov.el
(package! nov)


;; magit
(package! magit-org-todos)

(package! browse-at-remote)
(package! git-link :recipe (:host github :repo "sshaw/git-link"))

;; logs
(package! logview)

;; (package! map :pin "bb50dbaafc0f71743bd9ffd5784258a9fd682c20")
;; (package! xref :pin "a82f459b37b31546bf274388baf8aca79e9c30d9")

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

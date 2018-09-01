;;; packages.el -*- lexical-binding: t; -*-
;;
;; Author     : russmatney
;; CreatedAt  : 15 April 2018
;; ModifiedAt : 15 April 2018
;; Status     : Usable
;;

;; elixir
(package! flycheck-mix)
(package! flycheck-credo)

;; haskell
(package! intero)
(package! ghc)
(package! lsp-haskell)

;; lisp
(package! paredit)
(package! dash)

;; org
(package! org-clubhouse
  :recipe (:fetcher github
           :repo "urbint/org-clubhouse"
           :files ("*")))

;; writing
(package! zen-mode
  :recipe (:fetcher github
           :repo "aki237/zen-mode"
           :files ("*")))

;; javascript
(package! flow-minor-mode)
(package! company-flow)
(package! flycheck-flow)
(package! prettier-js)
(package! rjsx-mode)
(package! add-node-modules-path)
(package! graphql-mode)
(package! lsp-javascript-flow :recipe (:fetcher github :repo "emacs-lsp/lsp-javascript"))

;; lsp
(package! lsp-mode)
(package! lsp-ui :recipe (:fetcher github :repo "emacs-lsp/lsp-ui"))
(package! company-lsp)

;; rust
(package! lsp-rust)

;; helm
(package! helm)

;; fountain
(package! fountain-mode :recipe (:fetcher github :repo "rnkn/fountain-mode"))

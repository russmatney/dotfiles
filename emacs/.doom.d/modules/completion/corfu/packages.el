;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el


(package! corfu :pin "e77f89ca457295519891e1c5f365b0da25b3134e")
(when (featurep! +orderless)
  (package! orderless :pin "87ab7e47e343285f7afd42779c78551332b8fd84"))
(package! kind-icon :pin "f10bf6170769a1b0ef7d06347d1631fb6e71446b")

(package! cape :recipe (:host github :repo "minad/cape" :branch "main"))
(package! corfu-doc :recipe (:host github :repo "galeo/corfu-doc" :branch "main"))

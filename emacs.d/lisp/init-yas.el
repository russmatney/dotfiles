;;; init-yas.el --- YaSnippet config
;;; Commentary:
;;; Code:
(use-package yasnippet
  :config
  (add-to-list 'load-path
                "~/dotfiles/emacs.d/snippets/"
                )
  (yas-global-mode 1)
)
(provide 'init-yas)
;;; init-yas.el ends here

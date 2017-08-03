;;; init-yas.el --- YaSnippet config
;;; Commentary:
;;; Code:
(use-package yasnippet
  :config
  (yas-global-mode 1)

  (defcustom rm/yas-snippet-dirs
    (expand-file-name "snippets" "~/dotfiles/emacs.d")
    "Directory to load yasnippet's snippet files."
    :group 'init-yas)

  (setq yas-snippet-dirs (append yas-snippet-dirs rm/yas-snippet-dirs))
  (yas-reload-all)

  ;; -- hooks --
  (add-hook 'prog-mode-hook
          '(lambda () (yas-minor-mode)))

)
(provide 'init-yas)
;;; init-yas.el ends here

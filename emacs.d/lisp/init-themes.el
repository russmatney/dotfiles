;;; init-themes.el --- Theme configuration
;;; Commentary:
;;; Code:

;; (load-theme 'atom-one-dark)
(use-package doom-themes
  :init
  ;;; Settings (defaults)
  (setq doom-enable-bold t
      doom-enable-italic t

      ;; doom-one specific settings
      doom-one-brighter-comments t
  )

  (setq org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

  ;; brighter source buffers (that represent files)
  (add-hook 'find-file-hook 'doom-buffer-mode-maybe)
  ;; if you use auto-revert-mode
  (add-hook 'after-revert-hook 'doom-buffer-mode-maybe)
  ;; you can brighten other buffers (unconditionally) with:
  (add-hook 'ediff-prepare-buffer-hook 'doom-buffer-mode)

  ;; brighter minibuffer when active
  (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)

  :config
  ;; (load-theme 'doom-one t)

  ;; Enable custom neotree theme
  (require 'doom-neotree)
  ;; Enable nlinum line highlighting
  (require 'doom-nlinum)
)

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
)


(provide 'init-themes)
;;; init-themes.el ends here

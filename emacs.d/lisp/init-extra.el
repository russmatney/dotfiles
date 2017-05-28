;;; init-extra.el --- Misc config without a home
;;; Commentary:
;;; Code:

;;; buffer-wide find/replace toggle
(use-package iedit)

(use-package ag)

(use-package which-key
  :ensure t
  :defer t
  :diminish which-key-mode
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :bind* (("M-m ?" . which-key-show-top-level))
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
    "M-m ?" "top level bindings"))


(use-package popup-imenu
  :config (define-key popup-isearch-keymap (kbd "<escape>") 'popup-isearch-cancel)
)

(use-package imenu-anywhere)


;;; focus mode
(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 100 (frame-char-width)))
        2))))

;; Now activate this global minor mode
;; (bzg-big-fringe-mode 1)

(require 'multi-term)

(add-hook 'shell-mode-hook (lambda () (linum-mode -1)))


(provide 'init-extra)
;;; init-extra.el ends here

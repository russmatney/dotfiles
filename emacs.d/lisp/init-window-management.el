;;; init-window-management.el --- Window movement and management
;;; Commentary:
;;; Code:

;; Window movement
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-j") 'windmove-down)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; (defadvice split-window-below (after restore-balanace-below activate)
;;   (balance-windows))

;; (defadvice split-window-right (after restore-balance-right activate)
;;   (balance-windows))

;; (defadvice delete-window (after restore-balance activate)
;;   (balance-windows))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
)

(use-package popwin
  :config

  (defun helm-popwin-help-mode-off ()
    "Turn `popwin-mode' off for *Help* buffers."
    (when (boundp 'popwin:special-display-config)
      (popwin:display-buffer helm-buffer t)
      (customize-set-variable 'popwin:special-display-config
                              (delq 'help-mode popwin:special-display-config))))

  (defun helm-popwin-help-mode-on ()
    "Turn `popwin-mode' on for *Help* buffers."
    (when (boundp 'popwin:special-display-config)
      (customize-set-variable 'popwin:special-display-config
                              (add-to-list 'popwin:special-display-config 'help-mode nil #'eq))))

  (add-hook 'helm-after-initialize-hook #'helm-popwin-help-mode-off)
  (add-hook 'helm-cleanup-hook #'helm-popwin-help-mode-on)

  (add-to-list 'popwin:special-display-config '("^\\*helm.*\\*$" :regexp t))
  (push '("^\\*helm.*\\*$" :regexp t :height 30) popwin:special-display-config)

)

(require 'window-numbering)

(use-package shackle
  :config
  (setq shackle-rules '(("\\`\\*term.*?\\*\\'" :regexp t :align right :size 0.3)))
  (shackle-mode))


(provide 'init-window-management)
;;; init-window-management.el ends here

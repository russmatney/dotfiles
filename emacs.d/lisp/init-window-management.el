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
;;   (balance-windows-area))

;; (defadvice split-window-right (after restore-balance-right activate)
;;   (balance-windows-area))

;; (defadvice delete-window (after restore-balance activate)
;;   (balance-windows-area))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(require 'window-numbering)

(use-package shackle
  :config

  (setq helm-display-function 'pop-to-buffer) ; make helm play nice

  ;; TODO append to shackle rules, add to emux
  (setq shackle-rules
        '(
          ("\\`\\*term.*?\\*\\'" :regexp t :align emux-get-term-alignment :size 0.3)
          ("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.3)
          ("\\`\\*emux helm.*?\\*\\'" :regexp t :align t :size 0.3)
          ))

  (shackle-mode))

;; TODO alignment per project?
(setq-default emux-term-alignment 'right)

(defun emux-get-term-alignment ()
  emux-term-alignment)

(provide 'init-window-management)
;;; init-window-management.el ends here

;;; init-company.el --- Company config
;;; Commentary:
;;; Code:

(use-package company
  :bind ("<escape>" . company-abort)

  :config
  (setq company-show-numbers t)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)

  (dotimes (i 10)
    (define-key company-active-map (kbd (format "C-%d" i)) 'company-complete-number))

  (define-key company-active-map (kbd "C-h") 'evil-indent-line)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "C-l") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)

  (add-hook 'after-init-hook 'global-company-mode)
)


(provide 'init-company)
;;; init-company.el ends here

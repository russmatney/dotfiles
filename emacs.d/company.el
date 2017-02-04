;;; company.el --- Company mode configuration

;;; Commentary:

;; Complete-anything mode

;;; Code:

(use-package company
  :bind ("<tab>" . company-complete-common)
        )

  :config
  (setq company-show-numbers t)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)

  (dotimes (i 10)
    (define-key company-active-map (kbd (format "C-%d" i)) 'company-complete-number))

  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)

  (global-company-mode)
)

(provide 'setup-company)

;;; company.el ends here

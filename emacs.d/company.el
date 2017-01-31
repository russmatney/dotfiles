;;; company.el --- Company mode configuration

;;; Commentary:

;; Complete-anything mode

;;; Code:

(use-package company
  :config
  (setq company-show-numbers t)
  (setq company-idle-delay 0)

  (dotimes (i 10)
    (define-key company-active-map (kbd (format "C-%d" i)) 'company-complete-number))

  (global-company-mode)
)

(provide 'setup-company)

;;; company.el ends here

;;; company.el --- Company mode configuration

;;; Commentary:

;; Complete-anything mode

;;; Code:

(use-package company
  :config
  (setq company-idle-delay 0)
  (global-company-mode)
)

(provide 'setup-company)

;;; company.el ends here

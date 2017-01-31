;;; company.el --- Company mode configuration

;;; Commentary:

;; Complete-anything mode

;;; Code:

(use-package company
  :config
  (setq company-show-numbers t)
  (setq company-idle-delay 0)

  ;; use numbers 0-9 to select company completion candidates
  ;; TODO doesn't work for 0?
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                  `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9)))

  (global-company-mode)
)

(provide 'setup-company)

;;; company.el ends here

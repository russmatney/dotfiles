;;; init-scratch.el --- Scratch config
;;; Commentary:
;;; Code:


(defun emux-window-config-change-hook ()
  "Run after window configuration change."
  ;; (emux-handle-term-window-dedicate)
  )
(add-hook 'window-configuration-change-hook 'emux-window-config-change-hook)


;; TODO fix this - it's dedicating the wrong window/wrong time
(defun emux-handle-term-window-dedicate ()
  "If the term window is the only one, undedicate it.
Otherwise, dedicate it."
  (set-window-dedicated-p (emux-get-term-window) (emux-term-only-window-p)))

(defun emux-term-only-window-p ()
  "If the term window is the only one, undedicate it.
Otherwise, dedicate it."
  (and (emux-term-window-open-p)
           (eq 1 (length (emux-filter-non-file-windows (window-list))))))

(defun emux-filter-non-file-windows (windows)
  "Filter the passed WINDOWS that match the buffer prefixes."
  (remove t
          (mapcar
           #'(lambda (window)
              (let ((buffer (window-buffer window)))
                (if (or
                     ;; NOTE THE GODDAMN SPACE IN " *NeoTree*"
                     (string-prefix-p " *NeoTree*" (buffer-name buffer))
                     (string-prefix-p "*NeoTree*" (buffer-name buffer))
                     )
                    t
                  window)
                    ))
           windows)))



(provide 'init-scratch)
;;; init-scratch.el ends here

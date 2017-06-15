;;; init-bootstrap.el --- Configures the bootstrapping of the Emacs configuration.
;;; Commentary:
;;;   Pulled and refactored from: https://github.com/rranelli/emacs-dotfiles
;;; Code:

(let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
       ;; (vendor-dir (expand-file-name "vendor" user-emacs-directory))
       )
  (add-to-list 'load-path lisp-dir)
  ;; (add-to-list 'load-path vendor-dir)
  )

(defvar init-files
  '(
    ;; local packages
    window-numbering

    ;; configs
    init-org
    init-packages
    init-settings
    init-window-management
    init-extra
    init-evil
    init-helm
    init-elixir
    init-company
    init-flycheck
    init-neotree
    init-projectile
    init-web
    init-yas
    init-term
    init-pager
    init-home
    init-themes
    init-todo
    init-scratch
  )
)


(defun safe-require (feature)
  "Safely requires FEATURE."
  (condition-case ex
      (progn
        (message (format "loading %s"
                         (symbol-name feature)))
        (require feature))
    ('error (add-to-list 'rm/init-errors
			 (format "[ERROR LOADING \"%s\"]: %s"
                                 (symbol-name feature) ex)))))

(defun rr/safe-load-init-files ()
  (dolist (file init-files)
    (safe-require file)))

(defun rr/unsafe-load-init-files ()
  (dolist (file init-files)
    (require file)))

;; add shell env
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))


(provide 'init-bootstrap)
;;; init-bootstrap.el ends here

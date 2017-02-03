;;; swoop.el --- Swoop configuration

;;; Commentary:

;; Swoop for navigating/hopping around a file or project with a search phrase.

;;; Code:

(use-package helm-swoop
  :bind (
    ([tab] . helm-next-line)
    ([backtab] . helm-previous-line)
  )

  :config
    ;; If this value is t, split window inside the current window
    (setq helm-swoop-split-with-multiple-windows nil)

    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
    (setq helm-swoop-split-direction 'split-window-vertically)
)

(provide 'setup-swoop)

;;; swoop.el ends here

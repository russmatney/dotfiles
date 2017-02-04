;;; swoop.el --- Swoop configuration

;;; Commentary:

;; Swoop for navigating/hopping around a file or project with a search phrase.

;;; Code:

(use-package helm-swoop
  :bind (
    :map helm-swoop-map
    ([tab] . helm-next-line)
    ([backtab] . helm-previous-line)
  )

  :config
    ;; If this value is t, split window inside the current window
    (setq helm-swoop-split-with-multiple-windows nil)

    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
    (setq helm-swoop-split-direction 'split-window-vertically)

    ;; If there is no symbol at the cursor, use the last used words instead.
    (setq helm-swoop-pre-input-function
      (lambda ()
        (let (($pre-input (thing-at-point 'symbol)))
          (if (eq (length $pre-input) 0)
              helm-swoop-pattern ;; this variable keeps the last used words
            $pre-input))))
)

(provide 'setup-swoop)

;;; swoop.el ends here

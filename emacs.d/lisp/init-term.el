;;; init-term.el --- Configuration for
;;; Commentary:
;;;   much thanks to:
;;;     - http://echosa.github.io/blog/2012/06/06/improving-ansi-term/
;;;     - https://emacs.stackexchange.com/questions/328/how-to-override-keybindings-for-term
;;;     - http://oremacs.com/2015/01/01/three-ansi-term-tips/
;;; Code:

;;; git diff and glp are too tall
;;; fzf -> use helm for c-r, c-t, c-y


(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer-and-window)
        )
    ad-do-it))
(ad-activate 'term-sentinel)

(defadvice ansi-term (before force-zsh)
  (interactive (list "/bin/zsh")))
(ad-activate 'ansi-term)

(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))


;; force term-mode to expose the passed global binding
(defun expose-global-binding-in-term (binding)
   (define-key term-raw-map binding
     (lookup-key (current-global-map) binding)))


(defun my-term-hook ()
  (goto-address-mode)
  (linum-mode -1)

  ;; expose for Ctrl-{h,j,k,l} window movement
  (expose-global-binding-in-term (kbd "C-l"))
  (expose-global-binding-in-term (kbd "C-h"))
  (expose-global-binding-in-term (kbd "C-j"))
  (expose-global-binding-in-term (kbd "C-k"))

  ;; keep M-x
  (expose-global-binding-in-term (kbd "M-x"))

  ;; ensure these are unset in term
  (define-key evil-insert-state-map (kbd "C-k") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-r") nil)
  (define-key evil-insert-state-map (kbd "C-t") nil)
  (define-key evil-insert-state-map (kbd "C-e") nil)
  (define-key evil-insert-state-map (kbd "C-a") nil)
  (define-key evil-insert-state-map (kbd "C-c") 'term-interrupt-subjob)
  (define-key evil-normal-state-map (kbd "C-c") 'term-interrupt-subjob)

  (define-key term-raw-map (kbd "s-v") 'term-paste)
)

(add-hook 'term-exec-hook 'my-term-use-utf8)
(add-hook 'term-mode-hook 'my-term-hook)
(add-hook 'shell-mode-hook (lambda () (linum-mode -1)))

(provide 'init-term)
;;; init-term.el ends here

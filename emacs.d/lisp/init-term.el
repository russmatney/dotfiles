;;; init-term.el --- Configuration for term (emux)
;;; Commentary:
;;;   much thanks to:
;;;     - http://echosa.github.io/blog/2012/06/06/improving-ansi-term/
;;;     - https://emacs.stackexchange.com/questions/328/how-to-override-keybindings-for-term
;;;     - http://oremacs.com/2015/01/01/three-ansi-term-tips/
;;;     - https://emacs.stackexchange.com/questions/18672/how-to-define-a-function-that-calls-a-console-process-using-ansi-term
;;;     - https://github.com/wpcarro/pc_settings/commit/aab701e10e52afec790b069a5e14c961c6f32307
;;; Code:

(defun expose-global-binding-in-term (binding)
  "Expose BINDING that is otherwise blocked/used by term mode."
   (define-key term-raw-map binding
     (lookup-key (current-global-map) binding)))

(defun rm/evil-open-at-bottom ()
  "Move the cursor to the bottom of the terminal when entering insert mode."
  (interactive)
  (end-of-buffer)
  (evil-insert-state 1)
  (term-send-raw-string "\b"))

(defun expose-global-binding-in-term (binding)
  "Expose BINDING that is otherwise blocked/used by term mode."
   (define-key term-raw-map binding
     (lookup-key (current-global-map) binding)))

(defun rm/term-mode-hook ()
  "Config term mode bindings."

  (setq window-max-chars-per-line 1000)

  ;; expose for Ctrl-{h,j,k,l} window movement
  (expose-global-binding-in-term (kbd "C-l"))
  (expose-global-binding-in-term (kbd "C-h"))
  (expose-global-binding-in-term (kbd "C-j"))
  (expose-global-binding-in-term (kbd "C-k"))

  ;; keep M-x
  (expose-global-binding-in-term (kbd "M-x"))
  (expose-global-binding-in-term (kbd "M-:"))

  ;; ensure these are unset in term
  (evil-define-key 'insert term-raw-map
    (kbd "C-k") nil
    (kbd "C-p") nil
    (kbd "C-n") nil
    (kbd "C-r") nil
    (kbd "C-t") nil
    (kbd "C-e") nil
    (kbd "C-a") nil
    (kbd "C-c") 'term-interrupt-subjob
  )
  (evil-define-key 'normal term-raw-map
    (kbd "C-r") nil
    (kbd "C-c") 'term-interrupt-subjob
    (kbd "C-r") 'wc/helm-shell-history
    (kbd "i") 'rm/evil-open-at-bottom
    (kbd ".") 'rm/repeat-last-shell-command
  )

  (define-key term-raw-map (kbd "C-r") 'wc/helm-shell-history)
  (define-key term-raw-map (kbd "M-j") 'wc/helm-autojump)
  (define-key term-raw-map (kbd "M-g") 'wc/helm-git-branches)
  (define-key term-raw-map (kbd "s-v") 'term-paste)
)
(add-hook 'term-mode-hook 'rm/term-mode-hook)

(provide 'init-term)
;;; init-term.el ends here

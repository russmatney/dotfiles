;;; init-extra.el --- Misc config without a home
;;; Commentary:
;;; much thanks to: http://echosa.github.io/blog/2012/06/06/improving-ansi-term/
;;; Code:

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defadvice ansi-term (before force-zsh)
  (interactive (list "/bin/zsh")))
(ad-activate 'ansi-term)

(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))


(defun my-term-paste (_)
 (interactive)
 (process-send-string
  (get-buffer-process (current-buffer))
  (if string string (current-kill 0))))

(defun my-term-hook ()
  (goto-address-mode)
  (define-key term-raw-map "\C-y" 'my-term-paste))

(add-hook 'term-exec-hook 'my-term-use-utf8)
(add-hook 'term-mode-hook 'my-term-hook)




(provide 'init-extra)
;;; init-extra.el ends here

;;; init-pager.el --- mode for term-like paging
;; derived from Matt Briggs here: https://github.com/mbriggs/emacs-pager
;;; Commentary:
;;; Code:

(defun rm/open-in-pager (file)
  "Opens the passed FILE in the current buffer."
  (find-file file)
  (emacs-pager-mode)
)

(defvar emacs-pager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'kill-this-buffer)

    map)
  "Keymap for Emacs pager mode.")

(defcustom emacs-pager-max-line-coloring 500
  "Maximum number of lines to ansi-color.
If performance is bad when loading data, reduce this number."
  :group 'emacs-pager)

;;;###autoload
(define-derived-mode emacs-pager-mode fundamental-mode "Pager"
  "Mode for viewing data paged by emacs-pager"
  (setq-local make-backup-files nil)
  (ansi-color-apply-on-region (goto-char (point-min))
                              (save-excursion
                                (forward-line emacs-pager-max-line-coloring)
                                (point)))

  (let* ((buffer-name "*pager*"))
    (set-buffer-modified-p nil)
    (read-only-mode)
    (evil-define-key 'normal emacs-pager-mode-map
      (kbd "q") 'kill-this-buffer
      (kbd "ESC") 'kill-this-buffer
      ;; (kbd "SPC") 'evil-scroll-page-down))
      ))
)

(provide 'init-pager)
;;; init-pager.el ends here

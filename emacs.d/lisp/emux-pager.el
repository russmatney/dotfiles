;;; emux-pager.el --- Mode for term-like paging
;; derived from Matt Briggs here: https://github.com/mbriggs/emacs-pager
;;; Commentary:
;;; Code:

(defun emux-open-in-pager (file)
  "Opens the passed FILE in the current buffer.

First un-dedicates the window to allow it to be reused by the pager.

Currently the window dedication is reset via
kill-pager-buffer-and-rededicate-term-window.  Yuk."
  (set-window-dedicated-p (get-buffer-window (current-buffer)) nil)
  (find-file file)
  (emacs-pager-mode))

(defvar emacs-pager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'kill-pager-buffer-and-rededicate-term-window)

    map)
  "Keymap for Emacs pager mode.")

(defcustom emacs-pager-max-line-coloring 500
  "Maximum number of lines to ansi-color.
If performance is bad when loading data, reduce this number."
  :group 'emacs-pager)

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
      (kbd "q") 'kill-pager-buffer-and-rededicate-term-window
      (kbd "ESC") 'kill-pager-buffer-and-rededicate-term-window
      ;; (kbd "SPC") 'evil-scroll-page-down))
      )))

(defun kill-pager-buffer-and-rededicate-term-window ()
  "Kill the paging buffer and rededicate the term window."
  (interactive)
  (kill-this-buffer)
  (set-window-dedicated-p (emux-get-term-window) t))

(provide 'emux-pager)
;;; emux-pager.el ends here

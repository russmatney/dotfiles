;;; private/russ/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+russ:multi-next-line "private/russ/autoload/evil" nil t)
(evil-define-motion +russ:multi-next-line (count)
  "Move down 6 lines."
  :type line
  (let ((line-move-visual (or visual-line-mode (derived-mode-p 'text-mode))))
    (evil-line-move (* 6 (or count 1)))))

;;;###autoload (autoload '+russ:multi-previous-line "private/russ/autoload/evil" nil t)
(evil-define-motion +russ:multi-previous-line (count)
  "Move up 6 lines."
  :type line
  (let ((line-move-visual (or visual-line-mode (derived-mode-p 'text-mode))))
    (evil-line-move (- (* 6 (or count 1))))))

;;;###autoload (autoload '+russ:cd "private/russ/autoload/evil" nil t)
(evil-define-command +russ:cd ()
  "Change `default-directory' with `cd'."
  (interactive "<f>")
  (cd input))

;;;###autoload (autoload '+russ:kill-all-buffers "private/russ/autoload/evil" nil t)
(evil-define-command +russ:kill-all-buffers (&optional bang)
  "Kill all buffers. If BANG, kill current session too."
  (interactive "<!>")
  (if bang
      (+workspace/kill-session)
    (doom/kill-all-buffers)))

;;;###autoload (autoload '+russ:kill-matching-buffers "private/russ/autoload/evil" nil t)
(evil-define-command +russ:kill-matching-buffers (&optional bang pattern)
  "Kill all buffers matching PATTERN regexp. If BANG, only match project
buffers."
  (interactive "<a>")
  (doom/kill-matching-buffers pattern bang))

;;; from https://www.emacswiki.org/emacs/IncrementNumber
;;;###autoload
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

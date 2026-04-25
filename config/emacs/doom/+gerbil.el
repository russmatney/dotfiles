;;; -*- lexical-binding: t; -*-


;;; gerbil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *gerbil-path*
  "/opt/gerbil/"
  ;; "/opt/homebrew/Cellar/0.17_3/"
  )

(defvar *gerbil-elisp-path*
  "/opt/gerbil/share/emacs/site-lisp/"
  ;; "/opt/homebrew/share/emacs/site-lisp/gerbil-scheme/"
  )

(defvar *gambit-elisp-path*
  "/opt/gerbil/share/emacs/site-lisp/"
  ;; "/opt/homebrew/share/emacs/site-lisp/gambit-scheme/"
  )

;; /opt/homebrew/share/emacs/site-lisp/gerbil-scheme

(use-package! gerbil-mode
  :when (file-directory-p *gerbil-path*)
  :mode (("\\.ss\\'"  . gerbil-mode)
         ("\\.pkg\\'" . gerbil-mode))
  :bind (:map comint-mode-map
	 (("C-S-n" . comint-next-input)
	  ("C-S-p" . comint-previous-input)
	  ("C-S-l" . clear-comint-buffer)
          ("C-j" . nil)
          ("C-k" . nil))
	 :map gerbil-mode-map
	 (("C-S-l" . clear-comint-buffer)))
  :init
  (autoload 'gerbil-mode
    (expand-file-name "gerbil-mode.el" *gerbil-elisp-path*)
    "Gerbil editing mode." t)
  :hook
  (gerbil-mode . rainbow-delimiters-mode)
  (gerbil-mode . linum-mode)
  (inferior-scheme-mode . gambit-inferior-mode)
  :config
  (require 'gambit
           (expand-file-name "gambit.el" *gambit-elisp-path*))
  (setf scheme-program-name (expand-file-name "bin/gxi" *gerbil-path*))

  (let ((tags (locate-dominating-file default-directory "TAGS")))
    (when tags (visit-tags-table tags)))
  (let ((tags (expand-file-name "src/TAGS" *gerbil-path*)))
    (when (file-exists-p tags) (visit-tags-table tags)))

  (defun clear-comint-buffer ()
    (interactive)
    (with-current-buffer "*scheme*"
      (let ((comint-buffer-maximum-size 0))
        (comint-truncate-buffer))))

  (defun gerbil-setup-buffers ()
    "Change current buffer mode to gerbil-mode and start a REPL"
    (interactive)
    (gerbil-mode)
    (split-window-right)
    (shrink-window-horizontally 2)
    (let ((buf (buffer-name)))
      (other-window 1)
      (run-scheme "gxi")
      (switch-to-buffer-other-window "*scheme*" nil)
      (switch-to-buffer buf)))

  ;; pulled from cider-util
  (defun list-at-point (&optional bounds)
    "Return the list (compound form) at point as a string, otherwise nil.
If BOUNDS is non-nil, return a list of its starting and ending position
instead."
    (when-let* ((b (or (and (equal (char-after) ?\()
                            (member (char-before) '(?\' ?\, ?\@))
                            ;; hide stuff before ( to avoid quirks with '( etc.
                            (save-restriction
                              (narrow-to-region (point) (point-max))
                              (bounds-of-thing-at-point 'list)))
                       (bounds-of-thing-at-point 'list))))
      (funcall (if bounds #'list #'buffer-substring-no-properties)
               (car b) (cdr b))))

  (defun scheme-send-list-at-point (&optional something)
    (interactive "P")
    (save-excursion
      (goto-char (cadr (list-at-point 'bounds)))
      (scheme-send-last-sexp)))

  (map! :localleader
        :map gerbil-mode-map

        (:desc "gerbil-setup-buffers" "g" #'gerbil-setup-buffers)

        (:prefix ("e" . "eval/send")
         :desc "send-last-sexp" "e" #'scheme-send-last-sexp
         :desc "send-list-at-point" "l" #'scheme-send-list-at-point
         :desc "reload-buffer" "b" #'gerbil-reload-current-buffer
         :desc "send-definition" "d" #'scheme-send-definition
         :desc "send-region" "r" #'scheme-send-region)))

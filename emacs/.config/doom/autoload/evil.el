;;;  -*- lexical-binding: t; -*-

;; disable :unless predicates with (sp-pair "'" nil :unless nil)
;; disable :post-handlers with (sp-pair "{" nil :post-handlers nil)
;; ...or specific :post-handlers with (sp-pair "{" nil :post-handlers '(:rem ("| " "SPC")))
;; (after! smartparens
;;   ;; Autopair quotes more conservatively; if I'm next to a word/before another
;;   ;; quote, I likely don't want another pair.
;;   (let ((unless-list '(sp-point-before-word-p
;;                        sp-point-after-word-p
;;                        sp-point-before-same-p)))
;;     (sp-pair "'"  nil :unless unless-list)
;;     (sp-pair "\"" nil :unless unless-list))

;;   ;; Expand {|} => { | }
;;   ;; Expand {|} => {
;;   ;;   |
;;   ;; }
;;   (dolist (brace '("(" "{" "["))
;;     (sp-pair brace nil
;;              :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
;;              ;; I likely don't want a new pair if adjacent to a word or opening brace
;;              :unless '(sp-point-before-word-p sp-point-before-same-p)))

;;   ;; Don't do square-bracket space-expansion where it doesn't make sense to
;;   (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
;;                  "[" nil :post-handlers '(:rem ("| " "SPC")))

;;   ;; Highjacks backspace to:
;;   ;;  a) balance spaces inside brackets/parentheses ( | ) -> (|)
;;   ;;  b) delete space-indented `tab-width' steps at a time
;;   ;;  c) close empty multiline brace blocks in one step:
;;   ;;     {
;;   ;;     |
;;   ;;     }
;;   ;;     becomes {|}
;;   ;;  d) refresh smartparens' :post-handlers, so SPC and RET expansions work
;;   ;;     even after a backspace.
;;   ;;  e) properly delete smartparen pairs when they are encountered, without the
;;   ;;     need for strict mode.
;;   ;;  f) do none of this when inside a string
;;   (advice-add #'delete-backward-char :override #'doom/delete-backward-char)

;;   ;; Makes `newline-and-indent' smarter when dealing with comments
;;   (advice-add #'newline-and-indent :around #'doom*newline-and-indent))


;; ;;;###autoload
;; (evil-define-motion +russ:multi-next-line (count)
;;   "Move down 6 lines."
;;   :type line
;;   (let ((line-move-visual (or visual-line-mode (derived-mode-p 'text-mode))))
;;     (evil-line-move (* 6 (or count 1)))))

;; ;;;###autoload
;; (evil-define-motion +russ:multi-previous-line (count)
;;   "Move up 6 lines."
;;   :type line
;;   (let ((line-move-visual (or visual-line-mode (derived-mode-p 'text-mode))))
;;     (evil-line-move (- (* 6 (or count 1))))))

;; ;;; from https://www.emacswiki.org/emacs/IncrementNumber
;; ;;;###autoload
;; (defun increment-number-at-point ()
;;   (interactive)
;;   (skip-chars-backward "0-9")
;;   (or (looking-at "[0-9]+")
;;       (error "No number at point"))
;;   (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

;; config/default/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :feature evil)

;;;###autoload (autoload '+default:multi-next-line "config/default/autoload/evil" nil t)
(evil-define-motion +default:multi-next-line (count)
  "Move down 6 lines."
  :type line
  (let ((line-move-visual (or visual-line-mode (derived-mode-p 'text-mode))))
    (evil-line-move (* 6 (or count 1)))))

;;;###autoload (autoload '+default:multi-previous-line "config/default/autoload/evil" nil t)
(evil-define-motion +default:multi-previous-line (count)
  "Move up 6 lines."
  :type line
  (let ((line-move-visual (or visual-line-mode (derived-mode-p 'text-mode))))
    (evil-line-move (- (* 6 (or count 1))))))

;;;###autoload (autoload '+default:cd "config/default/autoload/evil" nil t)
(evil-define-command +default:cd ()
  "Change `default-directory' with `cd'."
  (interactive "<f>")
  (cd input))

;;;###autoload (autoload '+default:kill-all-buffers "config/default/autoload/evil" nil t)
(evil-define-command +default:kill-all-buffers (&optional bang)
  "Kill all buffers. If BANG, kill current session too."
  (interactive "<!>")
  (if bang
      (+workspace/kill-session)
    (doom/kill-all-buffers)))

;;;###autoload (autoload '+default:kill-matching-buffers "config/default/autoload/evil" nil t)
(evil-define-command +default:kill-matching-buffers (&optional bang pattern)
  "Kill all buffers matching PATTERN regexp. If BANG, only match project
buffers."
  (interactive "<a>")
  (doom/kill-matching-buffers pattern bang))

;;;###autoload
(defun +default/easymotion ()
  "Invoke and lazy-load `evil-easymotion' without compromising which-key
integration."
  (interactive)
  (let ((prefix (this-command-keys)))
    (map! :m prefix nil)
    (evilem-default-keybindings prefix)
    (set-transient-map evilem-map)
    (which-key-reload-key-sequence prefix)))

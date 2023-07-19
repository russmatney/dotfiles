;;; ../dotfiles/emacs/.doom.d/+company.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;
;; completion

;; company
(use-package! company
  :config
  (setq
   company-idle-delay 1.5
   company-minimum-prefix-length 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:after company
  (:map company-active-map
   ;; Don't interfere with `evil-delete-backward-word' in insert mode
   "C-w"        nil

   ;; Navigate candidates
   "C-n"        #'company-other-backend
   "C-p"        #'company-other-backend
   "C-j"        #'company-select-next
   "C-k"        #'company-select-previous
   "C-l"        #'company-complete-selection
   "<down>"       #'company-select-next
   "<up>"         #'company-select-previous
   "<right>"       #'company-complete-selection
   "C-SPC"      #'company-complete-common
   "TAB"     #'company-complete-common-or-cycle
   [backtab]    #'company-select-previous
   [escape]     (λ! (company-abort) (evil-normal-state 1))

   ;; filter or show docs for candidate
   "C-h"        #'company-show-doc-buffer
   "<right>"        #'company-show-doc-buffer
   "C-s"        #'company-filter-candidates)))

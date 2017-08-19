;;; private/russ/+company.el -*- lexical-binding: t; -*-

 ;; company-mode (vim-like omnicompletion)
(map!
 :i "C-SPC"  #'+company/complete

 (:after company
   (:map company-active-map
     ;; Don't interfere with `evil-delete-backward-word' in insert mode
     "C-w"        nil

     ;; Navigate candidates
     "C-n"        #'company-select-next
     "C-p"        #'company-select-previous
     "C-j"        #'company-select-next
     "C-k"        #'company-select-previous
     "C-l"        #'company-complete-selection
     "C-SPC"      #'company-complete-common
     [tab]        #'company-complete-common-or-cycle
     [backtab]    #'company-select-previous
     [escape]     (λ! (company-abort) (evil-normal-state 1))

     ;; filter or show docs for candidate
     "C-h"        #'company-show-doc-buffer
     "C-s"        #'company-filter-candidates)))


(after! company
  (setq company-idle-delay 0))

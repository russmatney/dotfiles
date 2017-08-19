;;; private/russ/+helm.el -*- lexical-binding: t; -*-

(after! helm
  (setq ;; favors findability over speed
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-M-x-fuzzy-match t
        helm-ff-newfile-prompt-p nil

        ;; next-line moves across sources
        helm-move-to-line-cycle-in-source nil)

  ;; overwritten to make this larger
  (set! :popup "\\` ?\\*[hH]elm.*?\\*\\'" :size 28 :regexp t))


(map!
 (:after helm
   (:map helm-map
     "ESC"        nil
     [tab]        #'helm-next-source
     [backtab]    #'helm-previous-source
     "C-u"        #'helm-delete-minibuffer-contents
     "C-w"        #'backward-kill-word
     "C-r"        #'evil-paste-from-register
     "C-b"        #'backward-word
     [left]       #'backward-char
     [right]      #'forward-char
     [escape]     #'helm-keyboard-quit
     "C-j"        #'helm-next-line
     "C-k"        #'helm-previous-line
     "C-n"        #'helm-next-line
     "C-p"        #'helm-previous-line

     "C-SPC"      #'helm-execute-persistent-action
     )

   (:after helm-files
     (:map helm-generic-files-map
       :e "ESC"     #'helm-keyboard-quit)
     (:map helm-find-files-map
       "C-h"   #'helm-find-files-up-one-level
       "C-l"   #'helm-execute-persistent-action
       "TAB"   #'helm-execute-persistent-action
       "C-SPC" #'helm-execute-persistent-action
       ))

   (:after helm-ag
     (:map helm-ag-map
       "<backtab>"  #'helm-ag-edit))))

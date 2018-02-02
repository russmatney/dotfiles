;;; private/russ/+leader.el -*- lexical-binding: t; -*-

(defmacro find-file-in! (path &optional project-p)
  "Returns an interactive function for searching files."
  `(lambda () (interactive)
     (let ((default-directory ,path))
       (call-interactively
        ',(command-remapping
           (if project-p
               #'projectile-find-file
             #'find-file))))))

(map!
 [remap evil-jump-to-tag] #'projectile-find-tag
 [remap find-tag]         #'projectile-find-tag
 ;; ensure there are no conflicts
 :nmvo doom-leader-key nil
 :nmvo doom-localleader-key nil)


(map!
 (:leader
   :desc "evil-ex"            :nv ";"   #'evil-ex
   :desc "eval-expression"    :nv "="   #'balance-windows-area
   :desc "eval-buffer"        :nv "B"   #'eval-buffer
   :desc "org-capture"        :nv "x"   #'org-capture
   :desc "+doom/blink-cursor" :n  "DEL" #'+doom/blink-cursor

   ;; Quick find file/buffer
   :desc "Helm mini"              :n  "SPC" #'helm-mini
   :desc "Workspace buffers"      :n  "b"   #'persp-switch-to-buffer
   :desc "Workspace buffers"      :n  ","   #'persp-switch-to-buffer
   :desc "All buffers"            :n  "<"   #'switch-to-buffer
   :desc "Find file"              :n  "."   #'find-file
   :desc "Previous Buffer"        :nv "["   #'doom/previous-buffer
   :desc "Next Buffer"            :nv "]"   #'doom/next-buffer
   :desc "projectile-find-file"   :n  "p"   #'projectile-find-file
   :desc "projectile-test-toggle" :n  "t"   #'projectile-toggle-between-implementation-and-test

   ;; Other search/nav
   :desc "Save buffer (write file)" :n  "RET" #'save-buffer
   :desc "Imenu"                 :nv "i"   #'imenu
   :desc "Imenu across buffers"  :nv "I"   #'imenu-anywhere
   :desc "Swiper"                :nv "/"   #'swiper
   :desc "Jump to register"      :nv "j"   #'jump-to-register

   ;; Windows management
   :desc "neotree-find-current-file"   :nv "n"  #'+russ/neotree-find-current-file
   :desc "neotree-reveal-current-file" :nv "N"  #'+russ/neotree-reveal-current-file
   :desc "Toggle last popup"           :n  "`"  #'doom/popup-toggle
   :desc "Open vertical split"         :n  "v"  #'evil-window-vsplit
   :desc "Open vertical split"         :n  "s"  #'evil-window-split

   ;; Editing
   :desc "Comment Selection" :nvm "c"   #'comment-or-uncomment-region
   :desc "Fix indent"        :nvm "TAB" #'evil-indent-line

   (:desc "kill" :prefix "k"
     :desc "delete-window"           :n "k" #'delete-window
     :desc "doom/kill-other-buffers" :n "B" #'doom/kill-other-buffers
     :desc "kill-buffer-from-list"   :n "b" #'kill-buffer
     :desc "ace-delete-window"       :n "a" #'ace-delete-window
     :desc "+workspace/delete"       :n "s" #'+workspace/delete
     :desc "hide-neotree"            :n "n" #'neotree-hide)

   (:desc "git" :prefix "g"
     :desc "Git status"        :n  "s" #'magit-status
     :desc "Git blame"         :n  "b" #'magit-blame
     :desc "Git time machine"  :n  "t" #'git-timemachine-toggle
     :desc "Git revert hunk"   :n  "r" #'git-gutter:revert-hunk)

   (:desc "help" :prefix "h"
     :desc "Help map"              :n "h" help-map
     :desc "Apropos"               :n "a" #'apropos
     :desc "Reload theme"          :n "R" #'doom/reload-theme
     :desc "Find library"          :n "l" #'find-library
     :desc "Toggle Emacs log"      :n "m" #'doom/popup-toggle-messages
     :desc "Command log"           :n "L" #'global-command-log-mode
     :desc "Describe function"     :n "f" #'describe-function
     :desc "Describe key"          :n "k" #'describe-key
     :desc "Describe char"         :n "c" #'describe-char
     :desc "Describe mode"         :n "M" #'describe-mode
     :desc "Describe variable"     :n "v" #'describe-variable
     :desc "Describe face"         :n "F" #'describe-face
     :desc "Describe DOOM setting" :n "s" #'doom/describe-setting
     :desc "Describe DOOM module"  :n "d" #'doom/describe-module
     :desc "Find definition"       :n "." #'+jump/definition
     :desc "Find references"       :n "/" #'+jump/references
     :desc "What face"             :n "'" #'doom/what-face
     :desc "What minor modes"      :n ";" #'doom/what-minor-mode
     :desc "Info"                  :n "i" #'info
     :desc "Toggle profiler"       :n "p" #'doom/toggle-profiler)

   (:desc "open" :prefix "o"
     :desc "Neotree"             :n  "n" #'+neotree/toggle
     :desc "Terminal"            :n  "t" #'+term/popup
     :desc "Terminal in project" :n  "T" #'+term/popup-in-project

     ;; applications
     :desc "APP: elfeed"  :n "E" #'=rss
     :desc "APP: email"   :n "M" #'=email
     :desc "APP: twitter" :n "T" #'=twitter
     :desc "APP: regex"   :n "X" #'=regex

     ;; macos
     (:when IS-MAC
       :desc "Reveal in Finder"          :n "o" #'+macos/reveal-in-finder
       :desc "Reveal project in Finder"  :n "O" #'+macos/reveal-project-in-finder))

   (:desc "window" :prefix "w"
     ;; Navigation
     :desc "evil-window-left"  :n "C-h" #'evil-window-left
     :desc "evil-window-down"  :n "C-j" #'evil-window-down
     :desc "evil-window-up"    :n "C-k" #'evil-window-up
     :desc "evil-window-right" :n "C-l" #'evil-window-right
     :desc "ace-window"        :n "C-w" #'ace-window)

   (:desc "tmux" :prefix "m"
     ;; howdy tmux!
     :desc "rerun last command"  :n "." #'+russ/tmux-repeat
     :desc "fire shell command"  :n "m" #'+russ/shell-command
     :desc "fire tmux command"   :n "t" #'+russ/tmux-command
     :desc "send cancel command" :n "c" #'+russ/tmux-cancel
     :desc "send q key"          :n "q" #'+russ/tmux-send-q
     :desc "switch client"       :n "s" #'+russ/tmux-switch-client
     )

   (:desc "eval" :prefix "e"
     :desc "current buffer"      :n "b" #'+eval/buffer
     :desc "region"              :n "r" #'+eval/region
     :desc "region and replace"  :n "R" #'+eval/region-and-replace
     :desc "last sexp"           :n "l" #'cider-eval-last-sexp
     )
   )

   ;; Haskell Mode - recced by @rschmukler
   (:after haskell-mode
     (:leader
       :desc "Jump to definition at point"   :n "l" #'intero-goto-definition
       :desc "Describe symbol at point"      :n "d" #'intero-info
       :desc "Describe type at point"        :n "t" #'intero-type-at

       ;; (:desc "insert" :prefix "w"
       ;;   ;; :desc "Insert type signature"        :n "s" #'intero-
       ;; )
   ))
 )

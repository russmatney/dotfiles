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
   :desc "Exec Command"    :nv ";"  #'evil-ex
   :desc "Org Capture"     :nv "x"  #'org-capture
   :desc "Neotree Toggle"  :nv "n"  #'+russ/neotree-project-root-dir-or-current-dir

   ;; Quick find file/buffer
   :desc "Helm mini"             :n "b"   #'helm-mini
   :desc "Find file in project"  :n "SPC" #'projectile-find-file
   :desc "Workspace buffers"     :n ","   #'persp-switch-to-buffer
   :desc "All buffers"           :n "<"   #'switch-to-buffer
   :desc "Find file"             :n "."   #'find-file
   :desc "Toggle last popup"     :n "`"   #'doom/popup-toggle
   :desc "Eval expression"       :n "="   #'eval-expression
   :desc "Blink cursor line"     :n "DEL" #'+doom/blink-cursor
   :desc "Jump to bookmark"      :n "RET" #'bookmark-jump

   :desc "Open vertical split"  :n "v" #'evil-window-vsplit ;; TODO consider split+helm-mini
   :desc "Open vertical split"  :n "s" #'evil-window-split

   ;; C-u is used by evil
   :desc "Universal argument"    :n "u"  #'universal-argument
   :desc "window"                :n "w"  evil-window-map

   :desc "smart-back" :nv "[" #'smart-backward
   :desc "smart-back" :nv "]" #'smart-forward

   :desc "Imenu"                 :nv "i" #'imenu
   :desc "Imenu across buffers"  :nv "I" #'imenu-anywhere
   :desc "Swiper"                :nv "/" #'swiper

   :desc "File file"   :n "." #'find-file

   (:desc "workspace" :prefix "TAB"
     :desc "Display tab bar"          :n "TAB" #'+workspace/display
     :desc "New workspace"            :n "n"   #'+workspace/new
     :desc "Load workspace from file" :n "f"   #'+workspace/load
     :desc "Save workspace to file"   :n "s"   #'+workspace/save
     :desc "Autosave current session" :n "S"   #'+workspace/save-session
     :desc "Switch workspace"         :n "."   #'+workspace/switch-to
     :desc "Kill all buffers"         :n "x"   #'doom/kill-all-buffers
     :desc "Delete session"           :n "X"   #'+workspace/kill-session
     :desc "Delete this workspace"    :n "d"   #'+workspace/delete
     :desc "Load session"             :n "L"   #'+workspace/load-session
     :desc "Next workspace"           :n "]"   #'+workspace/switch-right
     :desc "Previous workspace"       :n "["   #'+workspace/switch-left
     :desc "Switch to last workspace" :n "l"   #'+workspace/switch-to-last)

   (:desc "kill" :prefix "k"
     :desc "Delete this window"     :n "k" #'delete-window
     :desc "Kill other buffers"     :n "b" #'doom/kill-other-buffers
     :desc "Ace delete window"      :n "a" #'ace-delete-window
     :desc "Delete this workspace"  :n "s" #'+workspace/delete)

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

   (:desc "project" :prefix "p"
     :desc "Browse project"          :n  "." (find-file-in! (doom-project-root))
     :desc "Find file in project"    :n  "/" #'projectile-find-file
     :desc "Run cmd in project root" :nv "!" #'projectile-run-shell-command-in-root
     :desc "Switch project"          :n  "p" #'projectile-switch-project
     :desc "Recent project files"    :n  "r" #'projectile-recentf
     :desc "List project tasks"      :n  "t" #'+ivy/tasks
     :desc "Pop term in project"     :n  "o" #'+term/popup-in-project
     :desc "Invalidate cache"        :n  "x" #'projectile-invalidate-cache)

   (:desc "toggle" :prefix "t"
     :desc "Flyspell"               :n "s" #'flyspell-mode
     :desc "Flycheck"               :n "f" #'flycheck-mode
     :desc "Line numbers"           :n "l" #'doom/toggle-line-numbers
     :desc "Big mode"               :n "b" #'doom-big-font-mode))

 ;; TODO might be able to delete this
 (:map evil-window-map ; prefix "C-w"
   ;; Navigation
   "C-h"     #'evil-window-left
   "C-j"     #'evil-window-down
   "C-k"     #'evil-window-up
   "C-l"     #'evil-window-right
   "C-w"     #'ace-window
   ;; Swapping windows
   "H"       #'+evil/window-move-left
   "J"       #'+evil/window-move-down
   "K"       #'+evil/window-move-up
   "L"       #'+evil/window-move-right
   "C-S-w"   #'ace-swap-window
   ;; Window undo/redo
   "u"       #'winner-undo
   "C-u"     #'winner-undo
   "C-r"     #'winner-redo
   "o"       #'doom/window-enlargen
   ;; Delete window
   "c"       #'+workspace/close-window-or-workspace
   "C-C"     #'ace-delete-window))

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
 ;; --- <leader> -------------------------------------
 (:leader
   :desc "Ex command"      :nv ";"  #'evil-ex
   :desc "M-x"             :nv ":"  #'execute-extended-command
   :desc "Helm Mini"       :nv "b"  #'helm-mini
   :desc "Neotree Toggle"  :nv "n"  #'+russ/neotree-project-root-dir-or-current-dir

   :desc "Pop up scratch buffer"   :nv "x"  #'doom/scratch-buffer
   :desc "Org Capture"             :nv "X"  #'+org/capture ; TODO doesn't work?

   ;; Most commonly used
   :desc "Find file in project"    :n "SPC" #'projectile-find-file
   ;; :desc "Switch workspace buffer" :n ","   #'persp-switch-to-buffer
   :desc "Switch buffer"           :n "<"   #'switch-to-buffer
   :desc "Browse files"            :n "."   #'find-file
   :desc "Toggle last popup"       :n "~"   #'doom/popup-toggle
   :desc "Eval expression"         :n "`"   #'eval-expression
   :desc "Blink cursor line"       :n "DEL" #'+doom/blink-cursor
   :desc "Jump to bookmark"        :n "RET" #'bookmark-jump
   :desc "Helm-Mini"               :n "ESC" #'helm-mini

   :desc "Open vertical split"     :n "v" #'evil-window-vsplit

   ;; C-u is used by evil
   :desc "Universal argument"    :n "u"  #'universal-argument
   :desc "window"                :n "w"  evil-window-map


   (:desc "previous..." :prefix "["
     :desc "Text size"           :nv "[" #'text-scale-decrease
     :desc "Buffer"              :nv "b" #'doom/previous-buffer
     :desc "Diff Hunk"           :nv "d" #'git-gutter:previous-hunk
     :desc "Todo"                :nv "t" #'hl-todo-previous
     :desc "Error"               :nv "e" #'previous-error
     :desc "Workspace"           :nv "w" #'+workspace/switch-left
     :desc "Smart jump"          :nv "h" #'smart-backward
     :desc "Spelling error"      :nv "s" #'evil-prev-flyspell-error
     :desc "Spelling correction" :n  "S" #'flyspell-correct-previous-word-generic)

   (:desc "next..." :prefix "]"
     :desc "Text size"           :nv "]" #'text-scale-increase
     :desc "Buffer"              :nv "b" #'doom/next-buffer
     :desc "Diff Hunk"           :nv "d" #'git-gutter:next-hunk
     :desc "Todo"                :nv "t" #'hl-todo-next
     :desc "Error"               :nv "e" #'next-error
     :desc "Workspace"           :nv "w" #'+workspace/switch-right
     :desc "Smart jump"          :nv "l" #'smart-forward
     :desc "Spelling error"      :nv "s" #'evil-next-flyspell-error
     :desc "Spelling correction" :n  "S" #'flyspell-correct-word-generic)

   (:desc "search" :prefix "/"
     :desc "Swiper"                :nv "/" #'swiper
     :desc "Imenu"                 :nv "i" #'imenu
     :desc "Imenu across buffers"  :nv "I" #'imenu-anywhere)

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
     :desc "Delete this workspace"  :n "s" #'+workspace/delete
     )

   (:desc "code" :prefix "c"
     :desc "List errors"               :n  "x" #'flycheck-list-errors
     :desc "Evaluate buffer/region"    :n  "e" #'+eval/buffer
                                       :v  "e" #'+eval/region
     :desc "Evaluate & replace region" :nv "E" #'+eval:replace-region
     :desc "Build tasks"               :nv "b" #'+eval/build
     :desc "Jump to definition"        :n  "d" #'+jump/definition
     :desc "Jump to references"        :n  "D" #'+jump/references
     :desc "Open REPL"                 :n  "r" #'+eval/repl
                                       :v  "r" #'+eval:repl)

   (:desc "file" :prefix "f"
     :desc "File file"                 :n "." #'find-file
     :desc "Sudo find file"            :n ">" #'doom/sudo-find-file
     :desc "Find file in project"      :n "/" #'projectile-find-file
     :desc "Find file from here"       :n "?" #'counsel-file-jump
     :desc "Find other file"           :n "a" #'projectile-find-other-file
     :desc "Open project editorconfig" :n "c" #'editorconfig-find-current-editorconfig
     :desc "Find file in dotfiles"     :n "d" #'+russ/find-in-dotfiles
     :desc "Browse dotfiles"           :n "D" #'+russ/browse-dotfiles
     :desc "Find file in emacs.d"      :n "e" #'+russ/find-in-emacsd
     :desc "Browse emacs.d"            :n "E" #'+russ/browse-emacsd
     :desc "Recent files"              :n "r" #'recentf
     :desc "Recent project files"      :n "R" #'projectile-recentf
     :desc "Yank filename"             :n "y" #'+russ/yank-buffer-filename)

   (:desc "git" :prefix "g"
     :desc "Git status"        :n  "s" #'magit-status
     :desc "Git blame"         :n  "b" #'magit-blame
     :desc "Git time machine"  :n  "t" #'git-timemachine-toggle
     :desc "Git revert hunk"   :n  "r" #'git-gutter:revert-hunk
     :desc "List gists"        :n  "g" #'+gist:list
     :desc "Next hunk"         :nv "]" #'git-gutter:next-hunk
     :desc "Previous hunk"     :nv "[" #'git-gutter:previous-hunk)

   (:desc "help" :prefix "h"
     :n "h" help-map
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
     :desc "Find documentation"    :n "h" #'+jump/documentation
     :desc "What face"             :n "'" #'doom/what-face
     :desc "What minor modes"      :n ";" #'doom/what-minor-mode
     :desc "Info"                  :n "i" #'info
     :desc "Toggle profiler"       :n "p" #'doom/toggle-profiler)

   (:desc "insert" :prefix "i"
     :desc "From kill-ring" :nv "y" #'counsel-yank-pop
     :desc "From snippet"   :nv "s" #'yas-insert-snippet)

   (:desc "open" :prefix "o"
     :desc "Default browser"     :n  "b" #'browse-url-of-file
     :desc "Debugger"            :n  "d" #'+debug/open
     :desc "REPL"                :n  "r" #'+eval/repl
                                 :v  "r" #'+eval:repl
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
       :desc "Reveal project in Finder"  :n "O" #'+macos/reveal-project-in-finder
       :desc "Send to Transmit"          :n "u" #'+macos/send-to-transmit
       :desc "Send project to Transmit"  :n "U" #'+macos/send-project-to-transmit
       :desc "Send to Launchbar"         :n "l" #'+macos/send-to-launchbar
       :desc "Send project to Launchbar" :n "L" #'+macos/send-project-to-launchbar))

   (:desc "project" :prefix "p"
     :desc "Browse project"          :n  "." (find-file-in! (doom-project-root))
     :desc "Find file in project"    :n  "/" #'projectile-find-file
     :desc "Run cmd in project root" :nv "!" #'projectile-run-shell-command-in-root
     :desc "Switch project"          :n  "p" #'projectile-switch-project
     :desc "Recent project files"    :n  "r" #'projectile-recentf
     :desc "List project tasks"      :n  "t" #'+ivy/tasks
     :desc "Pop term in project"     :n  "o" #'+term/popup-in-project
     :desc "Invalidate cache"        :n  "x" #'projectile-invalidate-cache)

   (:desc "quit" :prefix "q"
     :desc "Quit"                    :n "q" #'evil-save-and-quit
     :desc "Quit (forget session)"   :n "Q" #'+workspace/kill-session-and-quit)

   (:desc "remote" :prefix "r"
     :desc "Upload local"           :n "u" #'+upload/local
     :desc "Upload local (force)"   :n "U" (λ! (+upload/local t))
     :desc "Download remote"        :n "d" #'+upload/remote-download
     :desc "Diff local & remote"    :n "D" #'+upload/diff
     :desc "Browse remote files"    :n "." #'+upload/browse
     :desc "Detect remote changes"  :n ">" #'+upload/check-remote)

   (:desc "snippets" :prefix "s"
     :desc "New snippet"           :n  "n" #'yas-new-snippet
     :desc "Insert snippet"        :nv "i" #'yas-insert-snippet
     :desc "Find snippet for mode" :n  "s" #'yas-visit-snippet-file
     :desc "Find snippet"          :n  "S" #'+russ/find-in-snippets)

   (:desc "toggle" :prefix "t"
     :desc "Flyspell"               :n "s" #'flyspell-mode
     :desc "Flycheck"               :n "f" #'flycheck-mode
     :desc "Line numbers"           :n "l" #'doom/toggle-line-numbers
     :desc "Fullscreen"             :n "f" #'doom/toggle-fullscreen
     :desc "Indent guides"          :n "i" #'highlight-indentation-mode
     :desc "Indent guides (column)" :n "I" #'highlight-indentation-current-column-mode
     :desc "Impatient mode"         :n "h" #'+impatient-mode/toggle
     :desc "Big mode"               :n "b" #'doom-big-font-mode
     :desc "Evil goggles"           :n "g" #'+evil-goggles/toggle))

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

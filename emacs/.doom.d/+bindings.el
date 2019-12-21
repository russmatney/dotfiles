;;;  -*- lexical-binding: t; -*-

;; Author     : russmatney
;; CreatedAt  : 15 April 2018
;; ModifiedAt : 15 April 2018
;; Status     : Usable
;;
;; 'Bindings' is intended to include:
;;   - emacs key bindings (including evil modes)
;;   - vim-style leader-key bindings
;;   - vim 'commands' (i.e. `:wq`)
;;
;; The goal is one file for mapping input to functions fired.
;;
;; Those functions could be built-in, from packages, or from autoloads.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 :nmvo doom-leader-key nil
 :nmvo doom-localleader-key nil)


;; helper for defining evil commands
(defalias 'ex! 'evil-ex-define-cmd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rm/search ()
  (interactive)
  (evil-ex "pg "))

(map! :leader
      "a" #'rm/search)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ex! "x" #'evil-save-modified-and-close)

(map!
 :v  "@"  #'+evil:macro-on-all-lines
 :n  "g@" #'+evil:macro-on-all-lines
 ;; repeat in visual mode
 :v  "."  #'evil-repeat
 ;; don't leave visual mode after shifting
 :v  "<"  #'+evil/visual-dedent  ; vnoremap < <gv
 :v  ">"  #'+evil/visual-indent  ; vnoremap > >gv

 ;; evil-commentary
 :n  "gc"  #'evil-commentary

 ;; evil-exchange
 :n  "gx"  #'evil-exchange

 ;; evil-matchit
 :nv [tab] #'+evil/matchit-or-toggle-fold

 ;; evil-surround
 :v  "S"  #'evil-surround-region
 :o  "s"  #'evil-surround-edit
 :o  "S"  #'evil-Surround-edit

 ;; --- Custom evil text-objects ---------------------
 :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
 :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
 :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
 :textobj "I" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
 :textobj "J" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:leader
   (:desc "Editor" :prefix "e"
     :desc "Open ~/.zshrc"                 :n "v" (lambda! (find-file "~/.zshrc"))
     :desc "Open file in ~/.doom.d/"       :n "c" #'russ/open-emacs-config-file
     :desc "Open ~/todo/bookmarks.org"     :n "b" (lambda! (find-file "~/todo/bookmarks.org"))
     :desc "Open file in ~/.emacs.d/"      :n "d" #'russ/open-doom-file
     :desc "Open ~/urbint/grid"            :n "g" (lambda! (find-file "~/urbint/grid/README.md"))
     :desc "Open file in ~/dotfiles/"      :n "z" #'russ/open-dotfile
     :desc "Open ~/.config/i3/config"      :n "i" (lambda! (find-file "~/.config/i3/config.base"))
     :desc "Open ~/.config/polybar/config" :n "p" (lambda! (find-file "~/.config/polybar/config"))
     :desc "Open ~/.tmux.conf"             :n "T" (lambda! (find-file "~/.tmux.conf"))
     :desc "Open file in ~/Dropbox/todo/"  :n "t" #'russ/open-org-file
     :desc "Open ~/todo/urbint.org"        :n "u" (lambda! (find-file "~/todo/urbint.org"))
     :desc "Open ~/.config/alacritty/alacritty.yml" :n "a" (lambda! (find-file "~/.config/alacritty/alacritty.yml"))
     :desc "Create new snippet"            :n "s" #'yas-new-snippet
     :desc "Edit snippet"                  :n "e" #'yas-visit-snippet-file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:after yasnippet
   (:map yas-keymap
     "C-e"           #'+snippets/goto-end-of-field
     "C-a"           #'+snippets/goto-start-of-field
     "<M-right>"     #'+snippets/goto-end-of-field
     "<M-left>"      #'+snippets/goto-start-of-field
     "<M-backspace>" #'+snippets/delete-to-start-of-field
     [backspace]     #'+snippets/delete-backward-char
     [delete]        #'+snippets/delete-forward-char-or-field)
   (:map yas-minor-mode-map
     :ig "<tab>" yas-maybe-expand
     :v  "<tab>" #'yas-insert-snippet)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 :nvime "A-x" #'execute-extended-command
 :nvime "M-x" #'execute-extended-command

 (:leader :desc "evil-ex" :nv ";" #'evil-ex)

 "A-<backspace>"     #'backward-kill-word
 "M-<backspace>"     #'backward-kill-word

 ;; save file (buffer)
 (:leader
   :desc "Save buffer (write file)" :n  "RET" #'save-buffer)

 ;; Text-scaling
 "M-+"    (λ! (text-scale-set 0))
 "M-="    #'doom/increase-font-size
 "M--"    #'doom/decrease-font-size

 ;; eval exp and buffer
 :nvime "A-;" #'eval-last-sexp
 :nvime "A-:" #'eval-expression
 ;; eval exp and buffer
 :nvime "M-;" #'eval-last-sexp
 :nvime "M-:" #'eval-expression

 ;; org
 (:leader :desc "org-capture"     :nv "x"   #'org-capture)
 (:leader :desc "org-agenda"     :nv "A"   #'org-agenda)

 (:leader :desc "helm-mini"     :nv "m"   #'helm-mini)

 ;;"M-RET" #'toggle-frame-fullscreen

 "M-v" #'evil-paste-after
 "M-c" #'evil-copy)

;;"M-h" #'ns-do-hide-emacs


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kill the things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:leader
   (:desc "kill" :prefix "k"
     :desc "delete-window"           :n "k" #'delete-window
     :desc "doom/kill-other-buffers" :n "B" #'doom/kill-other-buffers
     :desc "kill-buffer-from-list"   :n "b" #'kill-buffer
     :desc "ace-delete-window"       :n "a" #'ace-delete-window
     :desc "+workspace/delete"       :n "s" #'+workspace/delete
     :desc "hide-neotree"            :n "n" #'neotree-hide)))

(ex! "k" #'kill-this-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Splits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:leader
   :desc "Open vertical split"         :n  "v"  #'evil-window-vsplit
   :desc "Open vertical split"         :n  "s"  #'evil-window-split)
 (:leader
   :desc "Open vertical split"         :n  "\\" #'evil-window-vsplit
   :desc "Open vertical split"         :n  "-"  #'evil-window-split))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigating Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 ;; movement
 "A-w"    #'ace-window
 "A-h"    #'evil-window-left
 "A-l"    #'evil-window-right
 "A-j"    #'evil-window-down
 "A-k"    #'evil-window-up

 "M-w"    #'ace-window
 "M-h"    #'evil-window-left
 "M-l"    #'evil-window-right
 "M-j"    #'evil-window-down
 "M-k"    #'evil-window-up

 ;; size Adjustments
 "S-<left>"  #'evil-window-increase-width
 "S-<right>" #'evil-window-decrease-width

 ;; quicker j/k hops
 :m "C-j" #'+default:multi-next-line
 :m "C-k" #'+default:multi-previous-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigating Files/Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 ;; next, previous buffer
 :n  "]b" #'next-buffer
 :n  "[b" #'previous-buffer

 ;; find file in project
 :n "C-p"   #'projectile-find-file
 (:leader
   :desc "projectile-find-file"   :n  "p"   #'projectile-find-file
   :desc "(invalidate cache, then) projectile-find-file"   :n  "P"   (λ! (projectile-find-file t))
   :desc "Find file"              :n  "."   #'find-file))


(map!
 ;; recentf
 (:leader :desc "recentf"                :n "r"    #'counsel-recentf)

 ;; toggle last two files
 (:leader :desc "last buffer"            :n "SPC"  #'evil-switch-to-windows-last-buffer)

 ;; find in open buffers
 (:leader :desc "Workspace buffers"      :n  "b"   #'switch-to-buffer)
 :n "M-b"   #'persp-switch-to-buffer

 ;; test toggle
 (:leader :desc "projectile-test-toggle" :n  "t" #'projectile-toggle-between-implementation-and-test))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 ;; Workspaces
 "M-c"    #'+workspace/new
 "M-,"    #'+workspace/rename
 "M-P"    #'rs/projectile-switch-project-workspace
 "M-O"    #'russ/projectile-open-file-from-project

 ;; switch to
 :n "M-s"  #'+workspace/switch-to
 :n "[w"    #'+workspace/switch-left
 :n "]w"    #'+workspace/switch-right
 :n "[s"    #'+workspace/switch-left
 :n "]s"    #'+workspace/switch-right

 "M-p"    #'+workspace/switch-right
 "M-n"    #'+workspace/switch-left)

;; TODO add "M-1" to go to workspace "1", etc.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:leader (:desc "git" :prefix "g"
            :desc "Git status"        :n  "s" #'magit-status
            :desc "Git blame"         :n  "b" #'magit-blame
            :desc "Git status"        :n  "l" #'magit-log
            :desc "Git time machine"  :n  "t" #'git-timemachine-toggle
            :desc "Git revert hunk"   :n  "r" #'git-gutter:revert-hunk
            :desc "Git revert hunk"   :n  "a" #'git-gutter:stage-hunk)))

(map!
 ;; git-gutter
 :n  "]d" #'git-gutter:next-hunk
 :n  "[d" #'git-gutter:previous-hunk

 ;; evil-magit
 (:after evil-magit
   :map (magit-status-mode-map magit-revision-mode-map)
   :n "C-j" nil
   :n "C-k" nil)

 ;; magit
 :n  "gm" #'magit
 :n  "gl" #'magit-log-head
 :n  "gt" #'git-timemachine

 ;; git-timemachine
 (:after git-timemachine
   (:map git-timemachine-mode-map
     :nv "p" #'git-timemachine-show-previous-revision
     :nv "C-k" #'git-timemachine-show-previous-revision
     :nv "n" #'git-timemachine-show-next-revision
     :nv "C-j" #'git-timemachine-show-previous-revision
     :nv "g" #'git-timemachine-show-nth-revision
     :nv "c" #'git-timemachine-show-commit
     :nv "q" #'git-timemachine-quit
     :nv "w" #'git-timemachine-kill-abbreviated-revision
     :nv "W" #'git-timemachine-kill-revision
     :nv "b" #'git-timemachine-blame)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! dired
  :init
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  :config
  (map!
   :n "-" #'dired-jump
   :map dired-mode-map
   :n "-"        #'dired-up-directory
   :n "<return>" #'dired-find-alternate-file
   :n "/"        #'dired
   :n "q"        (lambda! (quit-window t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ranger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (map! :n "-" #'ranger)
(setq ranger-show-literal nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 :n  "]e" #'next-error
 :n  "[e" #'previous-error)

(ex! "er[rors]"    #'flycheck-list-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy/Counsel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:after counsel
   (:map counsel-ag-map
     [backtab]  #'+ivy/wgrep-occur  ; search/replace on results
     "C-SPC"    #'counsel-git-grep-recenter)))   ; preview


(map!
 :after ivy
 :map ivy-minibuffer-map
 [escape] #'keyboard-escape-quit
 "A-v" #'yank
 "A-z" #'undo
 "M-v" #'yank
 "M-z" #'undo
 "C-r" #'evil-paste-from-register
 "C-k" #'ivy-previous-line
 "C-j" #'ivy-next-line
 "C-l" #'ivy-alt-done
 "C-h" #'ivy-backward-kill-word
 "C-w" #'ivy-backward-kill-word
 "C-u" #'ivy-kill-line
 "C-b" #'backward-word
 "C-f" #'forward-word

 :map ivy-switch-buffer-map
 "C-k" #'ivy-previous-line
 "C-j" #'ivy-next-line
 "C-l" #'ivy-alt-done
 "C-h" #'ivy-backward-kill-word)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imenu/Swiper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:leader
   :desc "Imenu"                 :nv "i"   #'imenu
   :desc "Imenu across buffers"  :nv "I"   #'imenu-anywhere
   :desc "Swiper"                :nv "f"   #'swiper
   :desc "swiper"                :nv "/"   #'swiper))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popup menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:leader
   :desc "RAISE" :nv "r"   #'+popup/raise))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:map help-mode-map
   :n "q"   #'quit-window))

(map!
 (:leader
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
     :desc "Toggle profiler"       :n "p" #'doom/toggle-profiler)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; TODO perhaps not a binding...
(after! company
  (setq company-idle-delay 0.4))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Term buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:after comint
   ;; TAB auto-completion in term buffers
   :map comint-mode-map [tab] #'company-complete))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vim S key fix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map! :i "C-SPC"  #'+company/complete)
(map! :n "s"  #'evil-substitute)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tabbar mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 "M-H" #'tabbar-forward
 "M-L" #'tabbar-backward)

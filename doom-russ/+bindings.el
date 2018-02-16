;;; private/russ/+bindings.el -*- lexical-binding: t; -*-


 (defun rm/move-window-right ()
   ""
   (interactive)
   (if (window-in-direction 'right)
     (evil-window-right 1)
     (shell-command-to-string "chunkc tiling::window --focus east")
   )
 )

(shell-command-to-string "chunkc tiling::query -w name")
(shell-command-to-string "echo hi")

 (defun rm/move-window-left ()
   ""
   (interactive)
   (if (window-in-direction 'left)
     (evil-window-left 1)
     (shell-command-to-string "chunkc tiling::window --focus west")
   )
 )

 (defun rm/move-window-above ()
   ""
   (interactive)
   (if (window-in-direction 'above)
     (evil-window-up 1)
     (shell-command-to-string "chunkc tiling::window --focus north")
   )
 )

 (defun rm/move-window-below ()
   ""
   (interactive)
   (if (window-in-direction 'below)
     (evil-window-down 1)
     (shell-command-to-string "chunkc tiling::window --focus south")
   )
 )

(map!
 ;; --- Global keybindings ---------------------------
 :nvime "M-x" #'execute-extended-command
 :nvime "A-x" #'execute-extended-command

 "A-b"      #'helm-mini
 "A-n"      #'+russ/neotree-find-current-file
 "A-N"      #'+russ/neotree-reveal-current-file
 "A-y"      #'counsel-yank-pop
 "A-<tab>"  #'evil-indent-line
 "A-t"      #'alchemist-project-toggle-file-and-tests
 "A-<backspace>"     #'backward-kill-word
 "A-r"      #'org-babel-execute-src-block

 :n "-"      #'dired-jump
 ;; :n "<return>"      #'evil-goto-line

 ;; TODO dwim eval family (cider/geiser:eval-sexp,region,buffer:help/send-to-buffer/run)
 :n "M-l"      #'cider-eval-last-sexp

 ;; Text-scaling
 "M-+"    (λ! (text-scale-set 0))
 "M-="    #'text-scale-increase
 "M--"    #'text-scale-decrease

 ;; Workspaces
 "A-c"    #'+workspace/new
 "A-1"    (λ! (+workspace/switch-to 1))
 "A-2"    (λ! (+workspace/switch-to 2))
 "A-3"    (λ! (+workspace/switch-to 3))
 "A-4"    (λ! (+workspace/switch-to 4))
 "A-5"    (λ! (+workspace/switch-to 5))
 "A-6"    (λ! (+workspace/switch-to 6))
 "A-7"    (λ! (+workspace/switch-to 7))
 "A-8"    (λ! (+workspace/switch-to 8))
 "A-9"    (λ! (+workspace/switch-to 9))
 "A-0"    (λ! (+workspace/switch-to 0))
 ;; "A-s"    #'+workspace/switch-to
 "A-h"    #'+workspace/switch-left
 "A-l"    #'+workspace/switch-right
 "A-["    #'+workspace/switch-left
 "A-]"    #'+workspace/switch-right
 "A-,"    #'+workspace/rename
 "A-L"    #'+workspace/load
 "A-S"    #'+workspace/save
 "A-P"    #'rs/projectile-switch-project-workspace

 "A-\\"    #'evil-window-vsplit
 "A--"    #'evil-window-split

 ;; window nav
 "C-`"    #'doom/popup-toggle
 "C-h"    #'rm/move-window-left
 "C-j"    #'rm/move-window-below
 "C-k"    #'rm/move-window-above
 "C-l"    #'rm/move-window-right


 "S-<left>"  #'evil-window-increase-width
 "S-<right>" #'evil-window-decrease-width

 ;; buffers
 :n  "]b" #'doom/next-buffer
 :n  "M-B" #'doom/next-buffer
 :n  "[b" #'doom/previous-buffer
 :n  "M-b" #'doom/previous-buffer

 :n "A-e" #'next-error
 :n "A-E" #'previous-error

 ;; eval exp and buffer
 :n "M-;"    #'eval-last-sexp
 :n "M-:"    #'eval-buffer

 ;; select all
 "M-a"    #'mark-whole-buffer
 ;; Copy and paste
 "M-c"    #'evil-yank
 "M-v"    #'clipboard-yank
 ;; quit emacs
 "M-q"    (if (daemonp) #'delete-frame #'save-buffers-kill-emacs)
 ;; find
 "M-f"    #'swiper
 ;; fullscreen
 "M-RET"   #'doom/toggle-fullscreen
 ;; hide
 "M-h"   #'ns-do-hide-emacs

 ;; quick hops
 :m "A-j" #'+russ:multi-next-line
 :m "A-k" #'+russ:multi-previous-line

 ;; jump to def/ref/doc
 ;; "A-."    #'+jump/definition
 ;; :m  "gd" #'+jump/definition
 ;; "A->"    #'+jump/references
 ;; :m  "gD" #'+jump/references
 ;; "A-d"    #'+jump/documentation
 ;; :m  "gh" #'+jump/documentation
)

;; Left as a relic.
;; (defun cleanup-file-data ()
;;   (interactive)
;;   (shell "/Users/russ/cleanup.sh"))

(map!
 "M-p"   #'projectile-find-file
 :n "C-p"   #'projectile-find-file

 :n "A-p"   #'projectile-find-file
 :n  "A-v"  #'evil-window-vsplit
 :n  "A-s"  #'evil-window-split


 :v "="  #'evil-indent
)

(map!
 ;; counsel
 (:after counsel
   (:map counsel-ag-map
     [backtab]  #'+ivy/wgrep-occur  ; search/replace on results
     "C-SPC"    #'counsel-git-grep-recenter   ; preview
     ))

     ; "M-RET"    (+ivy-do-action! #'+ivy-git-grep-other-window-action)))

 ;; flycheck
 :m  "]e" #'next-error
 :m  "[e" #'previous-error

 ;; flyspell
 :m  "]S" #'flyspell-correct-word-generic
 :m  "[S" #'flyspell-correct-previous-word-generic

 ;; hl-todo
 :m  "]t" #'hl-todo-next
 :m  "[t" #'hl-todo-previous

 ;; ivy
 (:after ivy
   :map ivy-minibuffer-map
   [escape] #'keyboard-escape-quit
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
   "C-f" #'forward-word)

 ;; markdown
 (:after markdown-mode
   (:map markdown-mode-map
     "<backspace>" nil
     "<M-left>"    nil
     "<M-right>"   nil
     "A-<tab>"     #'markdown-cycle))

 ;; help mode
 (:map help-mode-map
   :n "[["  #'help-go-back
   :n "]]"  #'help-go-forward
   :n "o"   #'ace-link-help
   :n "q"   #'quit-window
   :n "Q"   #'+ivy-quit-and-resume)

 ;; --- Built-in plugins -----------------------------
 (:after comint
   ;; TAB auto-completion in term buffers
   :map comint-mode-map [tab] #'company-complete)
 )

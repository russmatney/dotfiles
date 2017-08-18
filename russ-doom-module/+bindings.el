;;; private/russ/+bindings.el -*- lexical-binding: t; -*-

(map!
 ;; --- Global keybindings ---------------------------
 :nvime "M-x" #'execute-extended-command
 :nvime "A-x" #'execute-extended-command

 "A-b"     #'helm-mini
 "A-n"     #'+russ/neotree-project-root-dir-or-current-dir
 "A-y"     #'counsel-yank-pop

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
 "A-l"    #'+workspace/switch-to-last
 "A-s"    #'+workspace/switch-to

 ;; window nav
 "C-h"    #'evil-window-left
 "C-j"    #'evil-window-down
 "C-k"    #'evil-window-up
 "C-l"    #'evil-window-right

 ;; eval exp and buffer
 "M-;"    #'eval-last-sexp
 "M-b"    #'+eval/buffer

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

 ;; quick hops
 :m "A-j" #'+russ:multi-next-line
 :m "A-k" #'+russ:multi-previous-line)

(map!
 ;; counsel
 (:after counsel
   (:map counsel-ag-map
     [backtab]  #'+ivy/wgrep-occur  ; search/replace on results
     "C-SPC"    #'counsel-git-grep-recenter   ; preview
     "M-RET"    (+ivy-do-action! #'+ivy-git-grep-other-window-action)))

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

 ;; --- Built-in plugins -----------------------------
 (:after comint
   ;; TAB auto-completion in term buffers
   :map comint-mode-map [tab] #'company-complete))

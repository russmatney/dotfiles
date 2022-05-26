;;  -*- lexical-binding: t; -*-

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

;; TODO rewrite bindings to operate off of hyper, i suppose
(setq x-hyper-keysym 'super)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (map!
;;  :nmvo doom-leader-key nil
;;  :nmvo doom-localleader-key nil)

;; helper for defining evil commands
(defalias 'ex! 'evil-ex-define-cmd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rm/search ()
  (interactive)
  (evil-ex "pg "))

(map! :leader "a" #'rm/search)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ex! "x" #'evil-save-modified-and-close)

(map!
 :v "@" #'+evil:macro-on-all-lines
 :n "g@" #'+evil:macro-on-all-lines
 ;; repeat in visual mode
 :v "." #'evil-repeat
 ;; don't leave visual mode after shifting
 :v "<" #'+evil/visual-dedent           ; vnoremap < <gv
 :v ">" #'+evil/visual-indent           ; vnoremap > >gv

 ;; evil-commentary
 :n "gc" #'evil-commentary

 ;; evil-exchange
 :n "gx" #'evil-exchange

 :nv "TAB" #'evil-toggle-fold
 :i "TAB" #'+company/complete
 :i "C-SPC" #'+company/complete

 ;; evil-surround
 :v "S" #'evil-surround-region
 :o "s" #'evil-surround-edit
 :o "S" #'evil-Surround-edit

 ;; --- Custom evil text-objects ---------------------
 :textobj "a" #'evil-inner-arg #'evil-outer-arg
 :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
 :textobj "i" #'evil-indent-plus-i-indent #'evil-indent-plus-a-indent
 :textobj "I" #'evil-indent-plus-i-indent-up #'evil-indent-plus-a-indent-up
 :textobj "J" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down

 :n "gQ" #'org-fill-paragraph)

(map!
 :leader
 :desc "Universal argument" "u" #'universal-argument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open misc files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bookmarks
(map!
 (:leader :desc "Edit/Open (Hydra)" "e" #'hydra-visit-bookmark/body)
 (:desc "Sticky Hydra" "M-b" #'hydra-sticky/body)
 (:leader :desc "CLAWE Hydra" "C" #'hydra-clawe/body)

 ;; hydra base
 ;; :n "N" #'hydra-narrow-widen/body
 :nvime "M-y" #'hydra-main/body
 ;; (:leader :desc "Hydra entrypoint"       :n "l"  #'hydra-main/body)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defhydra hydra-snippets (:exit t)
  ("s" +snippets/new "Create new snippet")
  ("e" +snippets/find "Edit snippet")
  ("f" +snippets/find "Find snippet"))

(map!
 (:leader
  (:desc "Edit" :n "E" #'hydra-snippets/body))
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
 :nvime "M-x" #'execute-extended-command
 :nvime "s-x" #'execute-extended-command

 (:leader :desc "evil-ex" :nv ";" #'evil-ex)

 "M-<backspace>"     #'backward-kill-word

 ;; save file (buffer)
 (:leader
  :desc "Save buffer (write file)" :n  "RET" #'save-buffer)

 ;; Text-scaling
 "M-+"    (λ! (text-scale-set 0))
 "M-="    #'doom/increase-font-size
 "M--"    #'doom/decrease-font-size

 ;; eval exp and buffer
 :nvime "M-;" #'eval-last-sexp
 :nvime "M-:" #'eval-expression

 ;; org
 (:leader :desc "org-capture" :nv "x" #'org-capture)
 (:leader :desc "org-agenda"  :nv "A" #'org-agenda)

 :n "g d"   '+lookup/definition
 :n "g r"   '+lookup/references

 (:leader :desc "RAISE" :nv "r"   #'+popup/raise)

 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kill the things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defhydra hydra-kill (:exit t)
  ("k" delete-window "delete window" :column "Time to Kill")
  ("B" doom/kill-other-buffers "kill other buffers")
  ("b" kill-buffer "kill buffer (from list)")
  ("a" ace-delete-window "ace-delete-window")
  ("s" +workspace/delete "+workspace/delete")
  ("n" +treemacs/toggle "toggle treemacs"))

(map!
 (:leader
  (:desc "kill" :n "k" #'hydra-kill/body)))

(ex! "k" #'kill-this-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Splits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:leader
  :desc "Open vertical split" :n "v" #'evil-window-vsplit
  :desc "Open vertical split" :n "s" #'evil-window-split
  :desc "Open vertical split" :n "\\" #'evil-window-vsplit
  :desc "Open vertical split" :n "-" #'evil-window-split))

(map!
 (:leader
  :desc "Treemacs toggle" :n "t" #'+treemacs/toggle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigating Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 ;; movement
 "C-h" #'evil-window-left
 "C-l" #'evil-window-right
 "C-j" #'evil-window-down
 "C-k" #'evil-window-up

 "S-<left>"  #'evil-window-increase-width
 "S-<right>" #'evil-window-decrease-width
 "S-<up>"    #'evil-window-increase-height
 "S-<down>"  #'evil-window-decrease-height)

;; size Adjustments

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigating Files/Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 ;; next, previous buffer
 :n  "]b" #'next-buffer
 :n  "[b" #'previous-buffer

 ;; find file in project
 (:leader
  :desc "projectile-find-file"   :n  "p"   #'projectile-find-file
  :desc "projectile-find-file (burst cache)" :n  "P" (λ! (projectile-find-file t))
  :desc "Find file"              :n  "."   #'find-file)

 ;; recentf
 ;; (:leader :desc "recentf"                :n "l"    #'counsel-recentf)

 ;; toggle last two files
 (:leader :desc "last buffer"            :n "SPC"  #'evil-switch-to-windows-last-buffer)

 ;; find in open buffers
 (:leader :desc "Workspace buffers"      :n  "b"   #'switch-to-buffer)
 (:leader :desc "iBuffer"                :n  "B"   #'ibuffer)

 ;; test toggle
 (:leader :desc "projectile-test-toggle" :n  "T" #'projectile-toggle-between-implementation-and-test)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 ;; Workspaces
 ;; "M-c"    #'+workspace/new
 ;; "M-,"    #'+workspace/rename
 ;; "M-P"    #'russ/projectile-open-file-from-project
 (:leader "o" #'russ/projectile-open-file-from-project)

 ;; switch to
 (:leader "w" #'+workspace/switch-to)
 ;; "M-w" #'+workspace/switch-to
 :n "[w"    #'+workspace/switch-left
 :n "]w"    #'+workspace/switch-right
 "M-p"    #'+workspace/switch-right
 "M-n"    #'+workspace/switch-left)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defhydra git-hydra (:exit t)
  ("s" magit-status "Magit status" :column "Magit")
  ("m" magit "Magit")
  ("b" magit-blame "Magit blame")
  ("l" magit-log "Magit log")
  ("t" git-timemachine-toggle "Git Time Machine toggle" :column "Time Machine")
  ("r" git-gutter:revert-hunk "Git Gutter revert hunk" :column "Gutter")
  ("a" git-gutter:stage-hunk "Git Gutter stage hunk"))

(map!
 (:leader
  (:desc "git" :n "g" #'git-hydra/body)))

;; allow moving left/right in magit buffers
(use-package! magit
  :config
  (map!
   (:map magit-mode-map
    "l" nil
    "h" nil
    :n "C-k" nil
    :n "C-j" nil)
   (:map magit-diff-mode-map
    "C-k" nil
    "C-j" nil)
   (:map code-review-mode-map
    :n "C-k" nil
    :n "C-j" nil)
   ;; TODO this doesn't belong here!
   (:map org-roam-mode-map
    :n "C-k" nil
    :n "C-j" nil)))

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
  (enable-command 'dired-find-alternate-file)
  (map!
   ;; :n "-" #'dired-jump
   :n "-" #'dirvish
   :map dired-mode-map
   :n "-"        #'dired-up-directory
   :n "<return>" #'dired-find-alternate-file
   :n "/"        #'dired
   ;; :n "q"        (cmd! (quit-window t))
   :n "q"        #'+dired/quit-all
   ))

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

(map!
 :desc "swiper"                :nv "/"   #'evil-ex-search-forward
 (:leader
  :desc "Imenu"                 :nv "i"   #'imenu
  :desc "Imenu across buffers"  :nv "I"   #'imenu-anywhere
  :desc "swiper"                :nv "/"   #'swiper))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:map help-mode-map
  :n "q"   #'quit-window))

(defhydra hydra-help (:exit t)
  ;; TODO how to consume this help-map?
  ("h" (lambda () help-map) "Help map" :column "Emacs Help")
  ("i" info "Info")
  ("p" doom/toggle-profiler "Toggle profiler")
  ("R" doom/reload-theme "Reload theme")

  ("f" describe-function "Describe function" :column "Describe")
  ("k" describe-key "Describe key")
  ("c" describe-char "Describe char")
  ("M" describe-mode "Describe mode")
  ("v" describe-variable "Describe variable")
  ("F" describe-face "Describe face")
  ("d" doom/describe-module "Describe DOOM module")

  ("." +jump/definition "Find definition" :column "Find")
  ("/" +jump/references "Find references")
  ("l" find-library "Find library")
  ("a" apropos "Apropos"))

;; (map!
;;  (:leader
;;   (:desc "help" :n "h" nil)))

;; TODO use once help-map works from the above hydra
;; (map!
;;  (:leader
;;   (:desc "help" :n "h" hydra-help/body)))

(map!
 (:leader
  (:desc "help" :prefix "h"
   :desc "Help map"              :n "h" help-map
   :desc "Apropos"               :n "a" #'apropos
   :desc "Reload theme"          :n "R" #'doom/reload-theme
   :desc "Find library"          :n "l" #'find-library
   :desc "Describe function"     :n "f" #'describe-function
   :desc "Describe key"          :n "k" #'describe-key
   :desc "Describe char"         :n "c" #'describe-char
   :desc "Describe mode"         :n "M" #'describe-mode
   :desc "Describe variable"     :n "v" #'describe-variable
   :desc "Describe face"         :n "F" #'describe-face
   :desc "Describe DOOM module"  :n "d" #'doom/describe-module
   :desc "Find definition"       :n "." #'+jump/definition
   :desc "Find references"       :n "/" #'+jump/references
   :desc "Info"                  :n "i" #'info
   :desc "Toggle profiler"       :n "p" #'doom/toggle-profiler)))

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

;; TODO perhaps not a binding...
;; (after! company
;;   ;; intending to start it with TAB
;;   (setq company-minimum-prefix-length 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Term buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:after comint
   ;; TAB auto-completion in term buffers
   :map comint-mode-map [tab] #'company-complete))

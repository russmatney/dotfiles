;;  -*- lexical-binding: t; -*-

(map!
 :leader
 :desc "Universal argument" "u" #'universal-argument)

;; helper for defining evil commands
(defalias 'ex! 'evil-ex-define-cmd)

;; Search
(defun rm/search ()
  (interactive)
  (evil-ex "pg "))

(map! :leader "a" #'rm/search)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Bindings

(ex! "x" #'evil-save-modified-and-close)

(map!
 :v "@" #'+evil:macro-on-all-lines
 :n "g@" #'+evil:macro-on-all-lines
 ;; repeat in visual mode
 :v "." #'evil-repeat
 ;; don't leave visual mode after shifting
 ;; :v "<" #'+evil/visual-dedent           ; vnoremap < <gv
 ;; :v ">" #'+evil/visual-indent           ; vnoremap > >gv

 ;; evil-commentary
 :n "gc" #'evil-commentary

 ;; evil-exchange
 :n "gx" #'evil-exchange

 :nv "TAB" #'evil-toggle-fold
 ;; :i "TAB" #'+company/complete
 ;; :i "C-SPC" #'+company/complete
 :i "TAB" #'completion-at-point
 :i "C-SPC" #'completion-at-point

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

 (:leader :desc "RAISE" :nv "r"   #'+popup/raise))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kill the things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun russ/kill-and-reopen-this-buffer ()
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (kill-this-buffer)
    (find-file filename)))

(defhydra hydra-kill (:exit t)
  ("k" kill-this-buffer "kill-this-buffer" :column "THIS")
  ("K" delete-window "kill-this-window")
  ("f" doom/delete-this-file "doom/delete-this-file")
  ("r" russ/kill-and-reopen-this-buffer "russ/kill-and-reopen-this-buffer")

  ("B" doom/kill-other-buffers "all other buffers" :column "Other buffers")
  ("b" kill-buffer "kill buffer (from list)")

  ("F" delete-file "Delete other file" :column "Files")
  ("f" doom/delete-this-file "doom/delete-this-file")

  ("a" ace-delete-window "ace-delete-window" :column "Misc")
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
  :desc "Find file"              :n  "."   #'find-file
  :desc "Find file (Other project)" :n "o" #'doom/find-file-in-other-project)

 (:leader
  :desc "Imenu"                 :nv "i"   #'imenu
  :desc "Imenu across buffers"  :nv "I"   #'consult-imenu-multi
  :desc "swiper"                :nv "/"   #'swiper)

 ;; toggle last two files
 (:leader :desc "last buffer"            :n "SPC"  #'evil-switch-to-windows-last-buffer)

 ;; find in open buffers
 (:leader :desc "Workspace buffers"      :n  "b"   #'switch-to-buffer)
 (:leader :desc "iBuffer"                :n  "B"   #'ibuffer)

 ;; test toggle
 (:leader :desc "projectile-test-toggle" :n  "T" #'projectile-toggle-between-implementation-and-test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 ;; Workspaces
 ;; (:leader "o" #'russ/projectile-open-file-from-project)

 ;; switch to
 (:leader "w" #'+workspace/switch-to)
 :n "[w"    #'+workspace/switch-left
 :n "]w"    #'+workspace/switch-right
 "M-p"    #'+workspace/switch-right
 "M-n"    #'+workspace/switch-left)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 :n  "]e" #'next-error
 :n  "[e" #'previous-error)

(ex! "er[rors]"    #'flycheck-list-errors)

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
;; Term buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:after comint
  ;; TAB auto-completion in term buffers
  ;; :map comint-mode-map [tab] #'company-complete
  ))

(map!
 (:after comint
  :map comint-mode-map
  :n "C-j" nil
  :n "C-k" nil)
 (:map inferior-lisp-mode-map
  :n "C-j" nil
  :n "C-k" nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! Info-mode
  (map!
   (:map Info-mode-map
    :n "C-j" nil
    :n "C-k" nil)))

;;;;;;;;;;;;;;;;;;;;;;;;

(after! esh-mode
  (map! :map eshell-mode-map
        ;; normal history lookup
        :i "C-r"   #'+eshell/search-history
        ;; maintain window movement
        "C-l"   nil
        :n "C-j"    nil
        :n "C-k"    nil))

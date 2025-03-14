;;; init.el -*- lexical-binding: t; -*-

;; (when (and (boundp 'startup-notified) (not startup-notified)))
(setq-default startup-notified nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; super/meta keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; duped in config.el
(setq mac-command-modifier      'super
      ns-command-modifier       'super
      mac-option-modifier       'meta
      ns-option-modifier        'meta

      ;; required to send M-x instead of mac unicode shortcuts
      ;; TODO why isn't this being set at startup? perhaps it's overwritten later?
      ;; [[file:~/.emacs.d/lisp/doom-keybinds.el::mac-right-option-modifier 'none][maybe overwritten here?]]
      mac-right-option-modifier 'meta
      ns-right-option-modifier 'meta)

(setq x-hyper-keysym 'super)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-directory (file-truename "~/todo/"))
(setq org-roam-directory (file-truename "~/todo/"))
;; (setq org-roam-tag-sources '(prop all-directories))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (concat doom-user-dir "+custom.el"))

(defvar +workspaces-main "todo")
(defvar +workspaces-switch-project-function #'magit-status)

;; (when noninteractive
;;   ;; allow for connection to ssh-agent
;;   (add-to-list 'doom-env-whitelist "^SSH_"))

(setq native-comp-always-compile t)
(setq native-comp-jit-compilation nil)
(setq package-native-compile t)
(after! (doom-packages straight)
  (setq straight--native-comp-available t))

;; (after! comp
;;   (mapc (doom-partial #'add-to-list 'native-comp-bootstrap-deny-list)
;;         (list
;;           "/seq-tests\\.el\\'"
;;           "/seq/tests/seq-tests\\.el\\'"
;;           "/seq\\/tests\\/seq-tests\\.el\\'"
;;               ))
;;   ;; HACK Disable native-compilation for some troublesome packages
;;   (mapc (doom-partial #'add-to-list 'native-comp-jit-compilation-deny-list)
;;         (list "/emacs-jupyter.*\\.el\\'"
;;               "/evil-collection-vterm\\.el\\'"
;;               "/vterm\\.el\\'"
;;               "/with-editor\\.el\\'"
;;               "/seq-tests\\.el\\'"
;;               "/seq/tests/seq-tests\\.el\\'"
;;               "/seq\\/tests\\/seq-tests\\.el\\'"
;;               )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doom!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(doom! :input
       ;;chinese
       ;;japanese

       :completion
       ;; (company          ; the ultimate code completion backend
       ;;  +childframe)            ; as-you-type code completion
       ;; +childframe
       ;; a nicer company UI (Emacs 26+ only)
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ;; (ivy              ; a search engine for love and life
       ;;  ;;+fuzzy
       ;;  ;; +prescient
       ;;  +icons)
       ;;+childframe)     ; uses childframes for popups (Emacs 26+ only)
       (corfu
        +orderless)
       (vertico
        +orderless
        +icons)

       :ui
       ;; deft
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;;(emoji +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       hydra
       ;;indent-guides
       ;;ligatures         ; ligatures and symbols to make your code pretty again
       ;; minimap           ; show a map of the code on the side
       modeline     ; a snazzy Atom-inspired mode-line
       nav-flash         ; blink the current line after jumping
       ;; neotree           ; a project drawer, like NERDTree for vim
       ophints
       (popup            ; tame sudden yet inevitable temporary windows
        ;; +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       ;;tabs
       (treemacs +lsp)          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       vc-gutter
       ;; vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       ;; window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       ;; zen

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold
       ;; (format +onsave)  ; automated prettiness
       format
       ;;god
       ;; lispy             ; vim for lisp, for people who dont like vim
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;; parinfer          ; turn lisp into python, sort of
       ;; rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       ;;word-wrap

       :emacs
       (dired            ; making dired pretty [functional]
        ;;+ranger
        +icons
        +dirvish)
       electric        ; smarter, keyword-based electric-indent
       ibuffer
       (undo +tree)      ; persistent, smarter undo for your inevitable mistakes
       vc                ; remember, remember that commit in November

       :term
       eshell            ; a consistent, cross-platform shell (WIP)
       ;;shell
       term              ; terminals in Emacs
       ;;vterm           ; another terminals in Emacs

       :checkers
       syntax
       ;;spell
       ;;grammar

       :tools
       ;;ansible
       ;;biblio
       ;;collab
       debugger          ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein             ; tame Jupyter notebooks with emacs
       (eval +overlay)   ; run code, run (also, repls)
       ;;gist            ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       lsp
       (magit +forge)             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       ;;pass
       ;;pdf             ; pdf enhancements
       ;;prodigy         ; FIXME managing external services & code builders
       ;; rgb               ; creating color strings
       ;; taskrunner        ; taskrunner for all your projects
       ;;terraform       ; infrastructure as code
       tmux              ; an API for interacting with tmux
       tree-sitter
       ;;upload          ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;;beancount
       ;; (cc +lsp)                ; C/C++/Obj-C madness
       ;; (clojure +tree-sitter +lsp)           ; java with a lisp
       (clojure +lsp)           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       (csharp +dotnet +lsp +tree-sitter +unity) ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;; elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;; erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust
       ;;fsharp           ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;; (gdscript +lsp)          ; the language you waited for
       ;; (go +lsp)                ; the hipster dialect
       ;; (haskell +dante) ; a language that's lazier than I am
       ;; haskell ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       json              ; At least it ain't XML
       ;;julia             ; a better, faster MATLAB
       ;;kotlin
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;lean
       ;;ledger            ; an accounting system in Emacs
       lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;; nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        ;; +dragndrop       ; file drag & drop support
        ;; +hugo
        ;; +ipython         ; ipython support for babel
        ;; +noter
        ;; +pandoc          ; pandoc integration into org's exporter
        ;; +pomodoro
        +present ; using Emacs for presentations
        +roam2)
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       ;; (python
       ;;  +lsp
       ;;  +pyright
       ;;  ;; +poetry
       ;;  +pyenv
       ;;  )            ; beautiful is better than ugly
       ;;qt
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       ;; rest              ; Emacs as a REST client
       ;; rst               ; ReST in peace
       ;;ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp)              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;; (scheme +gambit)
       sh        ; she sells (ba|z|fi)sh shells on the C xor
       ;;sml
       ;;solidity
       ;; swift             ; who asked for emoji variables?
       ;;terra
       ;; web               ; the tubes
       yaml
       (zig +lsp +tree-sitter)

       :email
       ;;(mu4e +gmail)
       ;; notmuch
       ;; (wanderlust +gmail)

       :app
       ;;calendar
       ;;emms
       ;;everywhere
       ;;irc              ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       default)

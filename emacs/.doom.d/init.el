;;; init.el -*- lexical-binding: t; -*-

;; NOTE `doom/help' to access Doom's documentation.
;; NOTE Move your cursor over a module's name (or its flags)
;;      and press `+lookup/documentation' for module documentation,
;;      or `+lookup/definition' for quick access to the module's source code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-one-light)
;; (setq doom-theme 'doom-molokai)
;; (setq doom-theme 'doom-tomorrow-night)
;; (setq doom-theme 'doom-tomorrow-day)
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-solarized-light)
;; (setq doom-theme 'doom-moonlight)
;; (setq doom-theme 'doom-monokai-spectrum)

(setq org-directory (expand-file-name "~/todo/"))
(setq org-roam-directory (expand-file-name "~/Dropbox/todo/garden/"))
(setq org-roam-tag-sources '(prop all-directories))

(setq custom-file (concat doom-private-dir "+custom.el"))

(defvar +workspaces-main "todo")

(defvar +workspaces-switch-project-function #'russ/doom-project-find-file)

(when noninteractive
  ;; allow for connection to ssh-agent
  (add-to-list 'doom-env-whitelist "^SSH_"))

(doom! :input
       ;;chinese
       ;;japanese

       :completion
       (company          ; the ultimate code completion backend
        +childframe)            ; as-you-type code completion
       ;; +childframe
       ;; a nicer company UI (Emacs 26+ only)
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       (ivy              ; a search engine for love and life
        ;;+fuzzy
        +prescient
        +icons)
       ;;+childframe)     ; uses childframes for popups (Emacs 26+ only)

       :ui
       deft
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;;(emoji +unicode)  ; 🙂
       fill-column
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       hydra
       ;;indent-guides
       ;;ligatures         ; ligatures and symbols to make your code pretty again
       minimap           ; show a map of the code on the side
       modeline     ; a snazzy Atom-inspired mode-line
       nav-flash         ; blink the current line after jumping
       ;; neotree           ; a project drawer, like NERDTree for vim
       ophints
       (popup            ; tame sudden yet inevitable temporary windows
        ;; +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       ;;tabs
       treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       vc-gutter
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen

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
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       ;;word-wrap

       :emacs
       (dired            ; making dired pretty [functional]
        ;;+ranger
        +icons)
       ;;electric        ; smarter, keyword-based electric-indent
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
       magit             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       ;;pass
       ;;pdf             ; pdf enhancements
       ;;prodigy         ; FIXME managing external services & code builders
       rgb               ; creating color strings
       taskrunner        ; taskrunner for all your projects
       ;;terraform       ; infrastructure as code
       tmux              ; an API for interacting with tmux
       ;;upload          ; map local to remote projects via ssh/ftp

       :os
       ;; (:if IS-MAC macos)  ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;;cc                ; C/C++/Obj-C madness
       clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;(csharp +unity)            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;; erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;faust
       ;;fsharp           ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;;(go +lsp)                ; the hipster dialect
       ;; (haskell +dante) ; a language that's lazier than I am
       ;;haskell ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       json              ; At least it ain't XML
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;lean
       ;;factor
       ;;ledger            ; an accounting system in Emacs
       (lua +moonscript)               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +dragndrop       ; file drag & drop support
        +hugo
        +ipython         ; ipython support for babel
        +noter
        +pandoc          ; pandoc integration into org's exporter
        +pomodoro
        +present ; using Emacs for presentations
        +roam)
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python
        +lsp
        +pyright)            ; beautiful is better than ugly
       ;;qt
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       ;; rest              ; Emacs as a REST client
       ;; rst               ; ReST in peace
       ;;ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;scheme
       sh        ; she sells (ba|z|fi)sh shells on the C xor
       ;;sml
       ;;solidity
       ;;swift             ; who asked for emoji variables?
       ;;terra
       ;;web               ; the tubes
       yaml

       :email
       ;;(mu4e +gmail)
       ;; notmuch
       ;; (wanderlust +gmail)

       :app
       ;;calendar
       ;;irc              ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought
       ;;(write            ; emacs as a word processor (latex + org + markdown)
       ;; +wordnut         ; wordnet (wn) search
       ;; +langtool)       ; a proofreader (grammar/style check) for Emacs


       :config)
;; For literate config users. This will tangle+compile a config.org
;; literate config in your `doom-private-dir' whenever it changes.
;;literate

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#21242b" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(custom-safe-themes
   (quote
    ("0809c08440b51a39c77ec5529f89af83ab256a9d48107b088d40098ce322c7d8" "6e2d579b02aadc933f434003f49d269d004f5c7094eb53658afbacc817761d83" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" default)))
 '(fci-rule-color "#383a42")
 '(flycheck-javascript-flow-args nil)
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(objed-cursor-color "#e45649")
 '(safe-local-variable-values
   (quote
    ((checkdoc-package-keywords-flag)
     (eval progn
           (pp-buffer)
           (indent-buffer)))))
 '(vc-annotate-background "#fafafa")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50a14f")
    (cons 40 "#688e35")
    (cons 60 "#807b1b")
    (cons 80 "#986801")
    (cons 100 "#ae7118")
    (cons 120 "#c37b30")
    (cons 140 "#da8548")
    (cons 160 "#c86566")
    (cons 180 "#b74585")
    (cons 200 "#a626a4")
    (cons 220 "#ba3685")
    (cons 240 "#cf4667")
    (cons 260 "#e45649")
    (cons 280 "#d2685f")
    (cons 300 "#c07b76")
    (cons 320 "#ae8d8d")
    (cons 340 "#383a42")
    (cons 360 "#383a42")))
 '(vc-annotate-very-old-color nil)
 '(wakatime-api-key "dadfb163-294d-4248-83db-358ac307867e")
 '(wakatime-cli-path "wakatime")
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.

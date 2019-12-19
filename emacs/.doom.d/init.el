;;; init.el -*- lexical-binding: t; -*-
;;
;; Author     : russmatney
;; CreatedAt  : 15 April 2018
;; ModifiedAt : 15 April 2018
;; Status     : Usable
;;
;; Largely copied from `~/.emacs.d/init.example.el`
;;
;; Notable change is the `(:config private)` at the end,
;; which removes the default configuration and uses my own.
;;


(defvar +org-dir (expand-file-name "~/todo/"))

(doom! :input
       ;;chinese
       ;;japanese

       :completion
       (company          ; the ultimate code completion backend
        +auto)            ; as-you-type code completion
       ;; a nicer company UI (Emacs 26+ only)
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       (ivy              ; a search engine for love and life
        ;;+fuzzy
        +prescient
        +icons
        +childframe)     ; uses childframes for popups (Emacs 26+ only)

       :ui
       ;;deft
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       fill-column
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       ;;hydra
       ;;indent-guides
       modeline     ; a snazzy Atom-inspired mode-line
       nav-flash         ; blink the current line after jumping
       ;; neotree           ; a project drawer, like NERDTree for vim
       ophints
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       ;;pretty-code
       ;;tabs
       ;;treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       vc-gutter
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold
       (format +onsave)  ; automated prettiness
       ;;god
       ;;lispy             ; vim for lisp, for people who dont like vim
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       ;;word-wrap

       :emacs
       (dired            ; making dired pretty [functional]
        ;;+ranger
        +icons)
       ;;electric        ; smarter, keyword-based electric-indent
       ibuffer
       vc                ; remember, remember that commit in November

       :term
       eshell            ; a consistent, cross-platform shell (WIP)
       ;;shell
       term              ; terminals in Emacs
       ;;vterm           ; another terminals in Emacs

       :tools
       ;;ansible
       debugger          ; FIXME stepping through code, to help you add bugs
       ;;direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein             ; tame Jupyter notebooks with emacs
       (eval +overlay)   ; run code, run (also, repls)
       flycheck
       ;;flyspell
       ;;gist            ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       lsp
       ;;macos           ; MacOS-specific commands
       magit             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       ;;pass
       ;;pdf             ; pdf enhancements
       ;;prodigy         ; FIXME managing external services & code builders
       rgb               ; creating color strings
       ;;terraform       ; infrastructure as code
       tmux              ; an API for interacting with tmux
       ;;upload          ; map local to remote projects via ssh/ftp
       wakatime

       :lang
       ;;agda              ; types of types of types of types...
       ;;assembly          ; assembly for fun or debugging
       ;;cc                ; C/C++/Obj-C madness
       clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;; erlang            ; an elegant language for a more civilized age
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;ess               ; emacs speaks statistics
       ;;faust
       ;;fsharp           ; ML stands for Microsoft's Language
       ;;go                ; the hipster dialect
       ;; (haskell +dante) ; a language that's lazier than I am
       ;;haskell ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;lean
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +dragndrop       ; file drag & drop support
        ;;+hugo
        ;;+ipython         ; ipython support for babel
        +pandoc          ; pandoc integration into org's exporter
        ;;+pomodoro
        +present)        ; using Emacs for presentations
       ;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       ;;(python +lsp)            ; beautiful is better than ugly
       ;;qt
       ;;racket            ; a DSL for DSLs
       ;;rest              ; Emacs as a REST client
       ;;ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;scheme
       sh        ; she sells (ba|z|fi)sh shells on the C xor
       ;;solidity
       ;;swift             ; who asked for emoji variables?
       ;;terra
       ;;web               ; the tubes

       :email
       ;;(mu4e +gmail)
       ;; notmuch
       ;; (wanderlust +gmail)

       ;;       ;; Applications are complex and opinionated modules that transform Emacs
       ;;       ;; toward a specific purpose. They may have additional dependencies and
       ;;       ;; should be loaded late.

       :app
       ;;       ;;calendar
       ;;       ;;irc              ; how neckbeards socialize
       ;;       ;;(rss +org)        ; emacs as an RSS reader
       ;;       ;;twitter           ; twitter client https://twitter.com/vnought
       ;;       ;;(write            ; emacs as a word processor (latex + org + markdown)
       ;;       ;; +wordnut         ; wordnet (wn) search
       ;;       ;; +langtool)       ; a proofreader (grammar/style check) for Emacs


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
   '("10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" default))
 '(fci-rule-color "#383a42")
 '(flycheck-javascript-flow-args nil)
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(objed-cursor-color "#e45649")
 '(safe-local-variable-values '((eval progn (pp-buffer) (indent-buffer))))
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

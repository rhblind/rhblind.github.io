+++
title = "Emacs Configuration"
author = ["Rolf Håvard Blindheim"]
lastmod = 2022-08-15T09:27:19+02:00
tags = ["org-mode"]
categories = ["emacs"]
draft = false
toc = true
aliases = "/posts/emacs-configuration"
+++

<a href="https://github.com/rhblind/.doom.d" target="_blank" rel="noopener" title="Github" style="display: inline-flex; gap: 0.5em;">
  <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" style="vertical-align: sub;">
    <path d="M9 19c-5 1.5-5-2.5-7-3m14 6v-3.87a3.37 3.37 0 0 0-.94-2.61c3.14-.35 6.44-1.54 6.44-7A5.44 5.44 0 0 0 20 4.77 5.07 5.07 0 0 0 19.91 1S18.73.65 16 2.48a13.38 13.38 0 0 0-7 0C6.27.65 5.09 1 5.09 1A5.07 5.07 0 0 0 5 4.77a5.44 5.44 0 0 0-1.5 3.78c0 5.42 3.3 6.61 6.44 7A3.37 3.37 0 0 0 9 18.13V22" />
  </svg>View on GitHub
</a>

> I wish I knew an inspirational quote to put here...


## Introduction {#introduction}

Since everybody seems to be writing literate Emacs configs these days, I'm not going to be any worse!
I'm not really very proficient on `elisp`, so this config is mostly a collection of code I've stolen from others.

I've been using a lots of different coding environments over the years, but they all seems to be missing some
kind of crucial functionality. In my opinion Emacs is far from perfect, it just sucks a bit less than the others :)

I have always been using Vim for quick edits of random files, and are really fond of modal editing. So when I
decided to learn using Emacs, I was looking for a way to continue doing so. That lead me to the [Spacemacs](https://www.spacemacs.org/)
distribution, which I was quite happy with for some time. However, I kept reading about this [Doom Emacs](https://github.com/doomemacs/doomemacs)
that claimed to have an easier configuration scheme, faster startup times, and a lot of other good stuff.
So, one day I decided to give it a try (I don't regret it).

Lastly, since this document literally is my Emacs configuration, at times it may contain code that is a
work in progress, silly comments, `TODO` and `FIXME` comments and so on.


## References {#references}

As many others I've been using [tecosaurs Emacs config](https://tecosaur.github.io/emacs-config/config.html) as my base configuration. It's one of the most
elaborated literate configs I've seen, with a lot of nice little tweaks. My configuration broadly follows his,
but it's slowly morphing into my own :)

So a big thanks to tecosaur for making my onboarding to Doom Emacs a smooth experience!


## Configuration {#configuration}

Remember to run `doom sync` after modifiyng this file.
Make this file run (slighty) faster with lexical binding (I read it on the internet)

```emacs-lisp
;;; config.el -*- lexical-binding: t; -*-
```


### Personal preferences - The random config dumping ground {#personal-preferences-the-random-config-dumping-ground}

Me

```emacs-lisp
(setq user-full-name "Rolf Håvard Blindheim"
      user-mail-address "rhblind@gmail.com")
```

Some functionality uses this to identify you, e.g. GPG configuration, email
clients, file templates and snippets.

And some random stuff that doesn't fit into any particual category


### Better defaults {#better-defaults}

```emacs-lisp
(setq auto-save-default         t         ; I don't want to lose work
      delete-by-moving-to-trash t         ; Delete files to trash
      display-time-24hr-format  t         ; I dont know the difference between AM and PM
      evil-want-fine-undo       t         ; More granular undos in evil insert mode
      evil-ex-substitute-global t         ; More often than not, I want /s on ex commands
      evil-kill-on-visual-paste nil       ; Don't add overwritten text in visual mode to the kill ring
      scroll-margin             2         ; Keep a little scroll margin
      undo-limit                16000000  ; Increase undo limit to 16Mb
      vc-follow-symlinks        nil       ; Don't follow symlinks, edit them directly
      which-key-idle-delay      0.2       ; Feels which-key feels more responsive
      window-combination-resize t         ; Take new window space from all other windows (not just the current)
      x-stretch-cursor          t         ; Stretch cursor to the glyph width
      )
```

Some modes I always want active

```emacs-lisp
(display-time-mode              1)        ; I want to know what time it is
(drag-stuff-global-mode         1)        ; Drag text around
(global-company-mode            1)        ; Enable autocomplete all over
(global-subword-mode            1)        ; Iterate through CamelCase words - Not sure how I like this
(smartparens-global-mode        1)        ; Always enable smartparens
(smartparens-global-strict-mode 1)        ; And keep it strict
(ws-butler-global-mode          1)        ; Unobtrusive way to trim spaces on end of lines

;; (unless (string-match-p "^Power N/A" (battery))         ; On laptops...  FIXME - gives me error when running `doom doctor'
;;   (display-battery-mode 1))                             ; it's nice to know how much power you have
```

I like to have the local leader key bound to `,`

```emacs-lisp
(setq doom-localleader-key      ","
      doom-localleader-alt-key  "M-,")
```


#### Frame {#frame}

Configure default size for new Emacs frames. I'm usually at 1440p display size,
so this seems reasonable.

```emacs-lisp
(add-to-list 'default-frame-alist '(height . 72))
(add-to-list 'default-frame-alist '(width . 240))
```


#### Windows {#windows}

I find it rather handy to be asked which buffer I want to see after splitting
the window. Let's make that happen.

First, we'll enter the new window

```emacs-lisp
(setq evil-vsplit-window-right t
      evil-split-window-below t)
```

Then, we'll pull up a buffer prompt.

```emacs-lisp
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))
```


#### Buffers {#buffers}

<!--list-separator-->

-  Text scaling

    ```emacs-lisp
    (map! :n "C-+"          #'text-scale-increase   ; Increase buffer font
          :n "C--"          #'text-scale-decrease   ; Decrease buffer font
          :n "C-0"          #'doom/reset-font-size) ; Reset all font sizes
    ```


#### Hacks {#hacks}

<!--list-separator-->

-  Too many open files

    Sometimes I'm getting the dreaded "Too many open files" error in Emacs, which doesn't seem to take into
    account `ulimit`, nor `sysctl kern.maxfiles` / `sysctl kern.maxfilesperproc`.
    If you know a workaround for this, please let me know!

    Meanwhile, let's define a function that clears the existing file notification watches from Emacs as a workaround.

    ```emacs-lisp
    (require 'filenotify)
    (defun file-notify-rm-all-watches (&optional silent)
      "Remove all existing file notification watches from Emacs.
    When `silent' is non-nil, don't activate the progress-reporter."
      (interactive)
      (if silent (maphash (lambda (key _value) (file-notify-rm-watch key)) file-notify-descriptors)
        (let ((progress-reporter
               (make-progress-reporter
                (format "Clearing all existing file notification watches... "))))
          (maphash (lambda (key _value) (file-notify-rm-watch key)) file-notify-descriptors)
          (progress-reporter-done progress-reporter))))
    ```

    So, whenever we're getting the "Too many open files" error in Emacs, just `M-x file-notify-rm-all-watches`,
    and we should be good to go again.
    Also, since this seems to be happening quite frequently (after enabling `tree-sitter-mode`?), I'll just put it on a timer for now.

    ```emacs-lisp
    ;; Run every 5 minutes
    (run-with-timer 0 (* 5 60) 'file-notify-rm-all-watches t)
    ```


### GPG, SSH and encryption {#gpg-ssh-and-encryption}

For GPG I want to use `~/.authinfo.gpg` since I usually keep this file in my [dotfiles](https://github.com/rhblind/dotfiles) repository. It's easy to
just copy it in place whenever I'm on a new computer.

```emacs-lisp
(after! epa
  (setq auth-source-cache-expiry nil                            ; Don't expire cached auth sources.
        epa-file-select-keys     nil                            ; If non-nil, always asks user to select recipients.
        epa-file-cache-passphrase-for-symmetric-encryption t    ; Cache passphrase for symmetrical encryptions.
        epg-gpg-program (cond ((eq system-type 'darwin)     "/usr/local/bin/gpg")
                              ((eq system-type 'gnu/linux)  "/usr/bin/gpg")
                              ((eq system-type 'windows-nt) "C:/Program Files (x86)/GNU/GnuPG/gpg2")))

  (pinentry-start)
  ;; (load-file (expand-file-name "secrets.el.gpg" doom-private-dir)) ; This cause weird errors
  )

; I'm using gpg-agent instead of ssh-agent so we need to connect here.
(shell-command "gpg-connect-agent updatestartuptty /bye >/dev/null")
```


### Keybindings {#keybindings}

Here's some [examples](https://github.com/hlissner/doom-emacs/blob/master/modules/config/default/+evil-bindings.el) on how to bind keymaps


#### Window navigation {#window-navigation}

```emacs-lisp
(map! "M-0"  #'treemacs-select-window  ;; make treemacs accessible as Window #0
      "M-1"  #'winum-select-window-1
      "M-2"  #'winum-select-window-2
      "M-3"  #'winum-select-window-3
      "M-4"  #'winum-select-window-4
      "M-5"  #'winum-select-window-5
      "M-6"  #'winum-select-window-6
      "M-7"  #'winum-select-window-7
      "M-8"  #'winum-select-window-8
      "M-9"  #'winum-select-window-9)
```


#### Workspace navigation {#workspace-navigation}

Some of the default Doom workspace navigation keybindings seems a bit counter intuitive for
my workflow, so I rebind some of them.

```emacs-lisp
(map! :leader
      (:when (featurep! :ui workspaces)
       (:prefix-map ("l" . "workspace")         ; Rebind workspaces to SCP-l
        :desc "Delete this workspace"           "d"   #'+workspace/delete
                                                "."   nil
        :desc "Switch to workspace"             "l"   #'+workspace/switch-to
        :desc "Load workspace from file"        "L"   #'+workspace/load
                                                "]"   nil
        :desc "Next workspace"                  "k"   #'+workspace/switch-right
                                                "["   nil
        :desc "Previous workspace"              "j"   #'+workspace/switch-left
                                                "`"   nil
        :desc "Cycle last workspace"            "TAB" #'+workspace/other)))
```


#### Buffer navigation {#buffer-navigation}

I really like cycling between two buffers using `SPC-Tab`

```emacs-lisp
(map! :leader :desc "Cycle last buffer" "TAB" #'evil-switch-to-windows-last-buffer)     ; Use SPC-Tab to cycle between two last buffers
```

I really like to move buffers around using `SPC-b [1..9]`.
The following functions are ported directly from `Spacemacs`, and relies only at `winum`.

```emacs-lisp
(after! winum
  (defun move-buffer-to-window (windownum follow-focus-p)
    "Moves a buffer to a window, using the window numbering. follow-focus-p controls
whether focus moves to new window (with buffer), or stays on current."
    (interactive)
    (if (> windownum (length (window-list-1 nil nil t)))
        (message "No window numbered %s" windownum)
      (let ((b (current-buffer))
            (w1 (selected-window))
            (w2 (winum-get-window-by-number windownum)))
        (unless (eq w1 w2)
          (set-window-buffer w2 b)
          (switch-to-prev-buffer)
          (unrecord-window-buffer w1 b))
        (when follow-focus-p
          (select-window (winum-get-window-by-number windownum))))))

  (defun swap-buffers-to-window (windownum follow-focus-p)
    "Swaps visible buffers between active window and selected window. follow-focus-p
controls whether focus moves to new window (with buffer), or stays on current."
    (interactive)
    (if (> windownum (length (window-list-1 nil nil t)))
        (message "No window numbered %s" windownum)
      (let* ((b1 (current-buffer))
             (w1 (selected-window))
             (w2 (winum-get-window-by-number windownum))
             (b2 (window-buffer w2)))
        (unless (eq w1 w2)
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (unrecord-window-buffer w1 b1)
          (unrecord-window-buffer w2 b2)))
      (when follow-focus-p (winum-select-window-by-number windownum))))

  ;; define and evaluate three numbered functions
  ;; buffer-to-window-1 to 9
  ;; move-buffer-to-window-no-follow-1 to 9
  ;; swap-buffer-to-window-no-follow-1 to 9
  (dotimes (i 9)
    (let ((n (+ i 1)))
      (eval `(defun ,(intern (format "buffer-to-window-%s" n)) (&optional arg)
               ,(format "Move buffer to window number %i." n)
               (interactive "P")
               (if arg
                   (swap-buffers-to-window ,n t)
                 (move-buffer-to-window ,n t))))

      (eval `(defun ,(intern (format "move-buffer-to-window-no-follow-%s" n)) ()
               (interactive)
               (move-buffer-to-window ,n nil)))
      (eval `(defun ,(intern (format "swap-buffer-window-no-follow-%s" n)) ()
               (interactive)
               (swap-buffers-to-window ,n nil))))))
```

Let's map the newly defined function to `SPC-b [1..9]` for easy access.

```emacs-lisp
(map! :leader
      (:prefix-map ("b" . "buffer")
       :desc "Move buffer to window 1" "1" #'buffer-to-window-1
       :desc "Move buffer to window 2" "2" #'buffer-to-window-2
       :desc "Move buffer to window 3" "3" #'buffer-to-window-3
       :desc "Move buffer to window 4" "4" #'buffer-to-window-4
       :desc "Move buffer to window 5" "5" #'buffer-to-window-5
       :desc "Move buffer to window 6" "6" #'buffer-to-window-6
       :desc "Move buffer to window 7" "7" #'buffer-to-window-7
       :desc "Move buffer to window 8" "8" #'buffer-to-window-8
       :desc "Move buffer to window 9" "9" #'buffer-to-window-9))
```


#### Jumping around with Avy {#jumping-around-with-avy}

Doom Emacs comes pre-configures with `avy` and it is pretty reasonable configured out of the box.
However, I never remember how to use it, so I'll jot down some notes here. Here's a quick
[YouTube video](https://www.youtube.com/watch?v=zar4GsOBU0g) covering some basic usage.

The **basics**

-   `g s` - Opens the prefix menu for doing jump navigation
-   `g s` - Jump using `evil-avy-goto-char-2` requiring 2 characters before triggering
-   `g s SPC` - Jump in closure using `evil-avy-goto-char-timer` requiring 1 character before triggering


#### Misc {#misc}

Erase content of current buffer with `SPC-b e`

```emacs-lisp
(map! :leader
      (:prefix-map ("b" . "buffer")
       :desc "Erase buffer" "e" #'erase-buffer))
```

Copy the entire buffer to kill-ring with `SPC-b Y`

```emacs-lisp
(defun copy-buffer-to-clipboard ()
  "Copy entire buffer to clipboard.
This function is from stackoverflow.com (https://stackoverflow.com/a/10216310)"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(map! :leader
      (:prefix-map ("b" . "buffer")
       :desc "Copy buffer" "Y" #'copy-buffer-to-clipboard))
```

Copy clipboard and replace buffer with `SPC-b P`

```emacs-lisp
(defun copy-clipboard-to-buffer ()
  "Copy clipboard and replace buffer.
This function is shamelessly stolen from spacemacs
(https://github.com/syl20bnr/spacemacs/blob/3ba43e29165fb17d39baab528d63a63e907fa81a/layers/%2Bspacemacs/spacemacs-defaults/funcs.el#L1272)"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(map! :leader
      (:prefix-map ("b" . "buffer")
       :desc "Paste clipboard" "P" #'copy-clipboard-to-buffer))
```

List processes

```emacs-lisp
(map! :leader
      :desc "List processes" "P" #'list-processes)
```


### Doom configuration {#doom-configuration}

In this section we generate the `init.el` file.

```emacs-lisp
;;; init.el -*- lexical-binding: t; -*-
```


#### Sneaky garbage collection {#sneaky-garbage-collection}

Defer garbage collection further back in the startup process.

```emacs-lisp
(setq gc-cons-threshold most-positive-fixnum)
```

Adopt a sneaky garbage collection strategy of waiting until idle time to collect;
staving off the collector while I'm working.

```emacs-lisp
(add-hook 'emacs-startup-hook #'(lambda ()
                                  (setq gcmh-idle-delay  'auto                     ; or N seconds
                                        gcmh-high-cons-threshold (* 16 1024 1024)  ; 16mb
                                        gcmh-verbose nil)))
```


#### Load prefer newer {#load-prefer-newer}

Sometimes it's necessary to fix a bug, or otherwise change stuff in installed local packages.
Always load the newest version of a file.

```emacs-lisp
(setq load-prefer-newer t)
```


#### Doom Env {#doom-env}

Add some environmental variables to the `doom-env-whitelist`.

```emacs-lisp
(when noninteractive
  (dolist (var '("LANG" "LC_TYPE" "GPG_AGENT_INFO" "SSH_AUTH_SOCK"))
    (add-to-list 'doom-env-whitelist var)))
```

This is required since I like to use `gpg-agent` over `ssh-agent` for key management, authentication and signing operations.


#### Modules {#modules}

<a id="code-snippet--init.el"></a>
```emacs-lisp
(doom! :completion
       <<doom-completion>>

       :ui
       <<doom-ui>>

       :editor
       <<doom-editor>>

       :emacs
       <<doom-emacs>>

       :term
       <<doom-term>>

       :checkers
       <<doom-checkers>>

       :tools
       <<doom-tools>>

       :os
       <<doom-os>>

       :lang
       <<doom-lang>>

       :email
       <<doom-email>>

       :app
       <<doom-app>>

       :config
       <<doom-config>>)
```

<!--list-separator-->

-  Structure

    <a id="code-snippet--doom-config"></a>
    ```emacs-lisp
    literate
    (default +bindings +smartparens)
    ```

<!--list-separator-->

-  Interface

    <a id="code-snippet--doom-completion"></a>
    ```emacs-lisp
    (company            ; the ultimate code completion backend
     +childframe)       ; ...when your children are better than you
     ;;helm             ; the *other* search engine for love and life
     ;;ido              ; the other *other* search engine...
     ;;ivy              ; a search engine for love and life
     (vertico           ; the search engine of the future
      +icons)
    ```

    <a id="code-snippet--doom-ui"></a>
    ```emacs-lisp
    ;;deft              ; notational velocity for Emacs
    doom                ; what makes DOOM look the way it does
    doom-dashboard      ; a nifty splash screen for Emacs
    ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
    (emoji +unicode)    ; 🙂
    hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
    ;;hydra
    ;;indent-guides     ; highlighted indent columns
    (ligatures +extra)  ; ligatures and symbols to make your code pretty again
    ;;minimap           ; show a map of the code on the side
    modeline            ; snazzy, Atom-inspired modeline, plus API
    nav-flash           ; blink cursor line after big motions
    ;;neotree           ; a project drawer, like NERDTree for vim
    ophints             ; highlight the region an operation acts on
    (popup
     +all +defaults)    ; tame sudden yet inevitable temporary windows
    ;; tabs             ; a tab bar for Emacs
    treemacs            ; a project drawer, like neotree but cooler
    ;;unicode           ; extended unicode support for various languages
    vc-gutter           ; vcs diff in the fringe
    vi-tilde-fringe     ; fringe tildes to mark beyond EOB
    (window-select
     +numbers)          ; visually switch windows
    workspaces          ; tab emulation, persistence & separate workspaces
    zen                 ; distraction-free coding or writing
    ```

    <a id="code-snippet--doom-editor"></a>
    ```emacs-lisp
    (evil +everywhere)  ; come to the dark side, we have cookies
    file-templates      ; auto-snippets for empty files
    fold                ; (nigh) universal code folding
    (format +onsave)    ; automated prettiness
    ;;god               ; run Emacs commands without modifier keys
    ;;lispy             ; vim for lisp, for people who don't like vim
    multiple-cursors    ; editing in many places at once
    ;;objed             ; text object editing for the innocent
    ;;parinfer          ; turn lisp into python, sort of
    ;;rotate-text       ; cycle region at point between text candidates
    snippets            ; my elves. They type so I don't have to
    word-wrap           ; soft wrapping with language-aware indent
    ```

    <a id="code-snippet--doom-emacs"></a>
    ```emacs-lisp
    (dired +icons)      ; making dired pretty [functional]
    electric            ; smarter, keyword-based electric-indent
    (ibuffer +icons)    ; interactive buffer management
    (undo +tree)        ; persistent, smarter undo for your inevitable mistakes
    vc                  ; version-control and Emacs, sitting in a tree
    ```

    <a id="code-snippet--doom-term"></a>
    ```emacs-lisp
    ;;eshell            ; the elisp shell that works everywhere
    ;;shell             ; simple shell REPL for Emacs
    ;;term              ; basic terminal emulator for Emacs
    vterm               ; the best terminal emulation in Emacs
    ```

    <a id="code-snippet--doom-checkers"></a>
    ```emacs-lisp
    syntax              ; tasing you for every semicolon you forget
    (:if                ; tasing you for misspelling mispelling
     (executable-find "aspell") spell +aspell)
    grammar             ; tasing grammar mistake every you make
    ```

    <a id="code-snippet--doom-tools"></a>
    ```emacs-lisp
    ;;ansible
    ;;biblio            ; Writes a PhD for you (citation needed)
    (debugger
     +lsp)              ; FIXME stepping through code, to help you add bugs
    direnv
    docker
    editorconfig        ; let someone else argue about tabs vs spaces
    ;;ein               ; tame Jupyter notebooks with emacs
    (eval +overlay)     ; run code, run (also, repls)
    ;;gist              ; interacting with github gists
    (lookup
     +dictionary
     +docsets)          ; navigate your code and its documentation
    (lsp +peek)         ; M-x vscode
    (magit
     +forge)            ; a git porcelain for Emacs
    make                ; run make tasks from Emacs
    ;;pass              ; password manager for nerds
    pdf                 ; pdf enhancements
    prodigy             ; FIXME managing external services & code builders
    ;;rgb               ; creating color strings
    ;;taskrunner        ; taskrunner for all your projects
    ;;terraform         ; infrastructure as code
    ;;tmux              ; an API for interacting with tmux
    ;;upload            ; map local to remote projects via ssh/ftp
    ```

    <a id="code-snippet--doom-os"></a>
    ```emacs-lisp
    (:if IS-MAC macos)  ; improve compatibility with macOS
    tty                 ; improve the terminal Emacs experience
    ```

<!--list-separator-->

-  Languages

    Languages are (usually) not loaded until a file associated with it is opened.
    Thus we can be pretty liberal with what we enable because there won't be much overhead.

    <a id="code-snippet--doom-lang"></a>
    ```emacs-lisp
    ;;agda              ; types of types of types of types...
    ;;beancount         ; mind the GAAP
    ;;(cc +lsp)         ; C > C++ == 1
    ;;clojure           ; java with a lisp
    ;;common-lisp       ; if you've seen one lisp, you've seen them all
    ;;coq               ; proofs-as-programs
    ;;crystal           ; ruby at the speed of c
    (csharp
     +dotnet
     +lsp)              ; unity, .NET, and mono shenanigans
    data                ; config/data formats
    ;;(dart +flutter)   ; paint ui and not much else
    ;;dhall
    (elixir +lsp)       ; erlang done right
    ;;elm               ; care for a cup of TEA?
    emacs-lisp          ; drown in parentheses
    (erlang +lsp)       ; an elegant language for a more civilized age
    ;;ess               ; emacs speaks statistics
    ;;factor
    ;;faust             ; dsp, but you get to keep your soul
    ;;fortran           ; in FORTRAN, GOD is REAL (unless declared INTEGER)
    ;;fsharp            ; ML stands for Microsoft's Language
    ;;fstar             ; (dependent) types and (monadic) effects and Z3
    ;;gdscript          ; the language you waited for
    (go +lsp)           ; the hipster dialect
    ;;(haskell +lsp)    ; a language that's lazier than I am
    ;;hy                ; readability of scheme w/ speed of python
    ;;idris             ; a language you can depend on
    json                ; At least it ain't XML
    ;;(java +lsp)       ; the poster child for carpal tunnel syndrome
    (javascript +lsp)   ; all(hope(abandon(ye(who(enter(here))))))
    ;;julia             ; a better, faster MATLAB
    ;;kotlin            ; a better, slicker Java(Script)
    ;;latex             ; writing papers in Emacs has never been so fun
    ;;lean              ; for folks with too much to prove
    ;;ledger            ; be audit you can be
    ;;lua               ; one-based indices? one-based indices
    markdown            ; writing docs for people to ignore
    ;;nim               ; python + lisp at the speed of c
    ;;nix               ; I hereby declare "nix geht mehr!"
    ;;ocaml             ; an objective camel
    (org                ;organize your plain life in plain text
     +pretty            ; yessss my pretties! (nice unicode symbols)
     +dragndrop         ; drag & drop files/images into org buffers
     +hugo              ; use Emacs for hugo blogging
     +noter             ; enhanced PDF notetaking
     +jupyter           ; ipython/jupyter support for babel
     +pandoc            ; export-with-pandoc support
     +gnuplot           ; who doesn't like pretty pictures
     ;;+pomodoro        ; be fruitful with the tomato technique
     +present           ; using org-mode for presentations
     +roam2)            ; wander around notes
    ;;php               ; perl's insecure younger brother
    ;;plantuml          ; diagrams for confusing people more
    ;;purescript        ; javascript, but functional
    (python             ; beautiful is better than ugly
     +lsp
     +cython
     +poetry
     +pyright)
    ;;qt                ; the 'cutest' gui framework ever
    ;;racket            ; a DSL for DSLs
    ;;raku              ; the artist formerly known as perl6
    ;;rest              ; Emacs as a REST client
    ;;rst               ; ReST in peace
    ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
    (rust +lsp)         ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
    ;;scala             ; java, but good
    ;;(scheme +guile)   ; a fully conniving family of lisps
    sh                  ; she sells {ba,z,fi}sh shells on the C xor
    ;;sml
    ;;solidity          ; do you need a blockchain? No.
    ;;swift             ; who asked for emoji variables?
    ;;terra             ; Earth and Moon in alignment for performance.
    web                 ; the tubes
    (yaml               ; JSON, but readable
     +lsp)
    ;;zig               ; C, but simpler
    ```

<!--list-separator-->

-  Everything in Emacs

    <a id="code-snippet--doom-email"></a>
    ```emacs-lisp
    ;;(mu4e +org +gmail)
    ;;notmuch
    ;;(wanderlust +gmail)
    ```

    <a id="code-snippet--doom-app"></a>
    ```emacs-lisp
    ;;calendar
    ;;emms
    ;;everywhere        ; *leave* Emacs!? You must be joking
    ;;irc               ; how neckbeards socialize
    ;;(rss +org)        ; emacs as an RSS reader
    ;;twitter           ; twitter client https://twitter.com/vnought
    ```


### Visual settings {#visual-settings}


#### Font Faces {#font-faces}

Cursor seems to not always load the correct color. Explicitly set it to whatever themes "blue" color is.

```emacs-lisp
(custom-set-faces! `(cursor :background ,(doom-color 'blue)))
```

TODO: The "history" items in mini-buffers and eval-buffer is too pale. Needs better contrast.

Replace the "modified" buffer color in the modeline, so it doesn't look like something's wrong every time
we edit files.

```emacs-lisp
(custom-set-faces! `(doom-modeline-buffer-modified :foreground "Orange" :italic t))
```


#### Theme and modeline {#theme-and-modeline}

I like the Solarized Light theme, and doom has a pretty good version of it. But sometimes it's
nice to use a dark theme. Let's add favorite themes to a `doom-cycle-themes` list.

```emacs-lisp
(setq doom-cycle-themes '(doom-solarized-light  ;; a list of themes to cycle
                          doom-moonlight))
```

We'll use the first theme in the list as our default theme.

```emacs-lisp
(defun cust/get-theme-name (theme)
  "Returns the name of `theme'.
If `theme' is a list, return the first item in list."
  (if (listp theme) (car theme) theme))

(setq doom-theme (cust/get-theme-name doom-cycle-themes))
```

Add some functions to easily cycle through `doom-cycle-themes` list.

```emacs-lisp
(defun cust/cycle-theme (&optional backward)
  "Cycle through themes defined in `doom-cycle-themes'.
When `backward' is non-nil, cycle the list backwards."
  (interactive "P")
  (let* ((theme-names (mapcar #'cust/get-theme-name doom-cycle-themes))
         (themes (if backward (reverse theme-names) theme-names))
         (next-theme (car (or (cdr (memq doom-theme themes))
                              ;; if current theme isn't in cycleable themes,
                              ;; start over
                              themes))))
    (when doom-theme
      (disable-theme doom-theme))
    (let ((progress-reporter
           (make-progress-reporter
            (format "Loading theme %s... " next-theme))))
      (load-theme next-theme t nil)
      (progress-reporter-done progress-reporter))))

(defun cust/cycle-theme-backward ()
  "Cycle through themes defined in `doom-cycle-themes' backwards."
  (interactive)
  (cust/cycle-theme t))
```

And finally add some keybindings to easily toggle them.

```emacs-lisp
(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "Next theme"        "t" #'cust/cycle-theme
       :desc "Previous theme"    "T" #'cust/cycle-theme-backward))
```

Slightly prettier default buffer names.

```emacs-lisp
(setq +doom-dashboard-name "► Doom"
      doom-fallback-buffer-name "► Doom")
```

Add some extra bells and whistles to the Doom modeline.

```emacs-lisp
(setq doom-modeline-icon                        (display-graphic-p)
      doom-modeline-major-mode-icon             t
      doom-modeline-major-mode-color-icon       t
      doom-modeline-buffer-state-icon           t)
```


#### Fonts {#fonts}

I'll just keep the default config description around for now.

```emacs-lisp
;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
```

```emacs-lisp
(setq doom-font (font-spec :family "Fira Code" :size 13 :weight 'semi-light)
      doom-big-font (font-spec :family "Fira Code" :size 18)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 18)
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))
```

"In addition to these fonts, Merriweather is used with nov.el, and Alegreya as a serifed proportional font used by mixed-pitch-mode for writeroom-mode with Org files.
Because we care about how things look let’s add a check to make sure we’re told if the system doesn’t have any of those fonts."

<a id="code-snippet--detect-missing-fonts"></a>
```emacs-lisp
(defvar required-fonts '("JetBrainsMono.*" "Overpass" "JuliaMono" "IBM Plex Mono" "Merriweather" "Alegreya"))
(defvar available-fonts
  (delete-dups (or (font-family-list)
                   (split-string (shell-command-to-string "fc-list : family")
                                 "[,\n]"))))

(defvar missing-fonts
  (delq nil (mapcar
             (lambda (font)
               (unless (delq nil (mapcar (lambda (f)
                                           (string-match-p (format "^%s$" font) f))
                                         available-fonts))
                 font))
             required-fonts)))

(if missing-fonts
    (pp-to-string
     `(unless noninteractive
        (add-hook! 'doom-init-ui-hook
          (run-at-time nil nil
                       (lambda ()
                         (message "%s missing the following fonts: %s"
                                  (propertize "Warning!" 'face '(bold warning))
                                  (mapconcat (lambda (font)
                                               (propertize font 'face 'font-lock-variable-name-face))
                                             ',missing-fonts
                                             ", "))
                         (sleep-for 0.5))))))
  ";; No missing fonts detected!")
```

```emacs-lisp
<<detect-missing-fonts()>>
```


#### Line numbers {#line-numbers}

Set default line number mode to `'relative`, and disable it for certain modes.

```emacs-lisp
(setq display-line-numbers-type 'relative)

;; (dolist (mode '(org-mode-hook
;;                 term-mode-hook
;;                 shell-mode-hook)
;;   (add-hook mode (lambda () (display-line-numbers-mode 0)))))
```


#### Mixed pitch {#mixed-pitch}

> From the `:ui zen` module.

We’d like to use mixed pitch in certain modes. If we simply add a hook, when directly opening a file with (a new) Emacs `mixed-pitch-mode runs`
before UI initialisation, which is problematic. To resolve this, we create a hook that runs after UI initialisation and both

-   conditionally enables `mixed-pitch-mode`
-   sets up the mixed pitch hooks

<!--listend-->

```emacs-lisp
(defvar mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
  "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")

(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
  (when (memq major-mode mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))

(add-hook 'doom-init-ui-hook #'init-mixed-pitch-h)
```

As mixed pitch uses the variable `mixed-pitch-face`, we can create a new function to apply mixed pitch with a serif face instead of the default.
This was created for writeroom mode.

```emacs-lisp
(autoload #'mixed-pitch-serif-mode "mixed-pitch"
  "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch." t)

(after! mixed-pitch
  (defface variable-pitch-serif
    '((t (:family "serif")))
    "A variable-pitch face with serifs."
    :group 'basic-faces)
  (setq mixed-pitch-set-height t)
  (setq variable-pitch-serif-font (font-spec :family "Alegreya" :size 18))
  (set-face-attribute 'variable-pitch-serif nil :font variable-pitch-serif-font)
  (defun mixed-pitch-serif-mode (&optional arg)
    "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch."
    (interactive)
    (let ((mixed-pitch-face 'variable-pitch-serif))
      (mixed-pitch-mode (or arg 'toggle)))))
```

Now, as Harfbuzz is currently used in Emacs, we’ll be missing out on the following Alegreya ligatures:

<style>.org-center { margin-left: auto; margin-right: auto; text-align: center; }</style>

<div class="org-center">

ff _ff_ ffi _ffi_ ffj _ffj_ ffl _ffl_
fft _fft_ fi _fi_ fj _fj_ ft _ft_
Th _Th_

</div>

Thankfully, it isn't to hard to add these to the `composition-function-table`.

```emacs-lisp
;; (set-char-table-range composition-function-table ?f '(["\\(?:ff?[fijlt]\\)" 0 font-shape-gstring]))
;; (set-char-table-range composition-function-table ?T '(["\\(?:Th\\)" 0 font-shape-gstring]))
```


#### Helper macros {#helper-macros}


### Other things {#other-things}


#### Splash screen {#splash-screen}

This beauty is shamelessly ripped off from [tecosaur's](https://tecosaur.github.io/emacs-config/config.html#splash-screen) Emacs config (as many other things in this config 😇)

{{< figure src="/ox-hugo/emacs-e.svg" alt="Fancy Emacs \"E\"" >}}

Make it theme-appropriate, and resize with the frame.

```emacs-lisp
(defvar fancy-splash-image-template
  (expand-file-name "misc/splash-images/emacs-e-template.svg" doom-private-dir)
  "Default template svg used for the splash image, with substitutions from ")

(defvar fancy-splash-sizes
  `((:height 300 :min-height 50 :padding (0 . 2))
    (:height 250 :min-height 42 :padding (2 . 4))
    (:height 200 :min-height 35 :padding (3 . 3))
    (:height 150 :min-height 28 :padding (3 . 3))
    (:height 100 :min-height 20 :padding (2 . 2))
    (:height 75  :min-height 15 :padding (2 . 1))
    (:height 50  :min-height 10 :padding (1 . 0))
    (:height 1   :min-height 0  :padding (0 . 0)))
  "list of plists with the following properties
  :height the height of the image
  :min-height minimum `frame-height' for image
  :padding `+doom-dashboard-banner-padding' (top . bottom) to apply
  :template non-default template file
  :file file to use instead of template")

(defvar fancy-splash-template-colours
  '(("$colour1" . keywords) ("$colour2" . type) ("$colour3" . base5) ("$colour4" . base8))
  "list of colour-replacement alists of the form (\"$placeholder\" . 'theme-colour) which applied the template")

(unless (file-exists-p (expand-file-name "theme-splashes" doom-cache-dir))
  (make-directory (expand-file-name "theme-splashes" doom-cache-dir) t))

(defun fancy-splash-filename (theme-name height)
  (expand-file-name (concat (file-name-as-directory "theme-splashes")
                            theme-name
                            "-" (number-to-string height) ".svg")
                    doom-cache-dir))

(defun fancy-splash-clear-cache ()
  "Delete all cached fancy splash images"
  (interactive)
  (delete-directory (expand-file-name "theme-splashes" doom-cache-dir) t)
  (message "Cache cleared!"))

(defun fancy-splash-generate-image (template height)
  "Read TEMPLATE and create an image if HEIGHT with colour substitutions as
   described by `fancy-splash-template-colours' for the current theme"
  (with-temp-buffer
    (insert-file-contents template)
    (re-search-forward "$height" nil t)
    (replace-match (number-to-string height) nil nil)
    (dolist (substitution fancy-splash-template-colours)
      (goto-char (point-min))
      (while (re-search-forward (car substitution) nil t)
        (replace-match (doom-color (cdr substitution)) nil nil)))
    (write-region nil nil
                  (fancy-splash-filename (symbol-name doom-theme) height) nil nil)))

(defun fancy-splash-generate-images ()
  "Perform `fancy-splash-generate-image' in bulk"
  (dolist (size fancy-splash-sizes)
    (unless (plist-get size :file)
      (fancy-splash-generate-image (or (plist-get size :template)
                                       fancy-splash-image-template)
                                   (plist-get size :height)))))

(defun ensure-theme-splash-images-exist (&optional height)
  (unless (file-exists-p (fancy-splash-filename
                          (symbol-name doom-theme)
                          (or height
                              (plist-get (car fancy-splash-sizes) :height))))
    (fancy-splash-generate-images)))

(defun get-appropriate-splash ()
  (let ((height (frame-height)))
    (cl-some (lambda (size) (when (>= height (plist-get size :min-height)) size))
             fancy-splash-sizes)))

(setq fancy-splash-last-size nil)
(setq fancy-splash-last-theme nil)
(defun set-appropriate-splash (&rest _)
  (let ((appropriate-image (get-appropriate-splash)))
    (unless (and (equal appropriate-image fancy-splash-last-size)
                 (equal doom-theme fancy-splash-last-theme)))
    (unless (plist-get appropriate-image :file)
      (ensure-theme-splash-images-exist (plist-get appropriate-image :height)))
    (setq fancy-splash-image
          (or (plist-get appropriate-image :file)
              (fancy-splash-filename (symbol-name doom-theme) (plist-get appropriate-image :height))))
    (setq +doom-dashboard-banner-padding (plist-get appropriate-image :padding))
    (setq fancy-splash-last-size appropriate-image)
    (setq fancy-splash-last-theme doom-theme)
    (+doom-dashboard-reload)))

(add-hook 'window-size-change-functions #'set-appropriate-splash)
(add-hook 'doom-load-theme-hook #'set-appropriate-splash)
```

Now the only thing missing is a an extra interesting line, whether that be some
corporate BS, an developer excuse, or a fun (useless) fact.

The following is rather long, but it essentially

-   fetches a phrase from an API
-   inserts it into the dashboard (asynchronously)
-   moves `point` to the phrase
-   re-uses the last phrase for requests within a few seconds of it being fetched

<!--listend-->

```emacs-lisp
(defvar splash-phrase-source-folder
  (expand-file-name "misc/splash-phrases" doom-private-dir)
  "A folder of text files with a fun phrase on each line.")

(defvar splash-phrase-sources
  (let* ((files (directory-files splash-phrase-source-folder nil "\\.txt\\'"))
         (sets (delete-dups (mapcar
                             (lambda (file)
                               (replace-regexp-in-string "\\(?:-[0-9]+-\\w+\\)?\\.txt" "" file))
                             files))))
    (mapcar (lambda (sset)
              (cons sset
                    (delq nil (mapcar
                               (lambda (file)
                                 (when (string-match-p (regexp-quote sset) file)
                                   file))
                               files))))
            sets))
  "A list of cons giving the phrase set name, and a list of files which contain phrase components.")

(defvar splash-phrase-set
  (nth (random (length splash-phrase-sources)) (mapcar #'car splash-phrase-sources))
  "The default phrase set. See `splash-phrase-sources'.")

(defun splase-phrase-set-random-set ()
  "Set a new random splash phrase set."
  (interactive)
  (setq splash-phrase-set
        (nth (random (1- (length splash-phrase-sources)))
             (cl-set-difference (mapcar #'car splash-phrase-sources) (list splash-phrase-set))))
  (+doom-dashboard-reload t))

(defvar splase-phrase--cache nil)

(defun splash-phrase-get-from-file (file)
  "Fetch a random line from FILE."
  (let ((lines (or (cdr (assoc file splase-phrase--cache))
                   (cdar (push (cons file
                                     (with-temp-buffer
                                       (insert-file-contents (expand-file-name file splash-phrase-source-folder))
                                       (split-string (string-trim (buffer-string)) "\n")))
                               splase-phrase--cache)))))
    (nth (random (length lines)) lines)))

(defun splash-phrase (&optional set)
  "Construct a splash phrase from SET. See `splash-phrase-sources'."
  (mapconcat
   #'splash-phrase-get-from-file
   (cdr (assoc (or set splash-phrase-set) splash-phrase-sources))
   " "))

(defun doom-dashboard-phrase ()
  "Get a splash phrase, flow it over multiple lines as needed, and make fontify it."
  (mapconcat
   (lambda (line)
     (+doom-dashboard--center
      +doom-dashboard--width
      (with-temp-buffer
        (insert-text-button
         line
         'action
         (lambda (_) (+doom-dashboard-reload t))
         'face 'doom-dashboard-menu-title
         'mouse-face 'doom-dashboard-menu-title
         'help-echo "Random phrase"
         'follow-link t)
        (buffer-string))))
   (split-string
    (with-temp-buffer
      (insert (splash-phrase))
      (setq fill-column (min 70 (/ (* 2 (window-width)) 3)))
      (fill-region (point-min) (point-max))
      (buffer-string))
    "\n")
   "\n"))

(defadvice! doom-dashboard-widget-loaded-with-phrase ()
  :override #'doom-dashboard-widget-loaded
  (setq line-spacing 0.2)
  (insert
   "\n\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (doom-display-benchmark-h 'return))
    'face 'doom-dashboard-loaded)
   "\n"
   (doom-dashboard-phrase)
   "\n"))
```

Lastly, the doom dashboard "useful commands" are no longer useful to me.
So, we'll disable them and then for a particularly _clean_ look disable
the modeline and `hl-line-mode`, then also hide the cursor.

```emacs-lisp
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))
```

At the end, we have a minimal but rather nice splash screen.

I haven't forgotten about the ASCII banner though! Once again we're going for
something simple.

```emacs-lisp
(defun doom-dashboard-draw-ascii-emacs-banner-fn ()
  (let* ((banner
          '(",---.,-.-.,---.,---.,---."
            "|---'| | |,---||    `---."
            "`---'` ' '`---^`---'`---'"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(unless (display-graphic-p) ; for some reason this messes up the graphical splash screen atm
  (setq +doom-dashboard-ascii-banner-fn #'doom-dashboard-draw-ascii-emacs-banner-fn))
```

<!--list-separator-->

-  Frames

    TBD


#### End Of Line characters {#end-of-line-characters}

Some files (looking at you, `dotnet` generated projects) create files using DOS-style end of line characters (`CRLF`).
I always want to use unix-style end of line characters (`LF`).

```emacs-lisp
(defun ensure-crlf-end-of-line ()
  "Automate M-% C-q C-m RET C-q C-j RET"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t))))
```


## Packages {#packages}

This is where to install packages.
By declaring packages using the `package!` macro in `packages.el`, then running `doom refresh` from the command line
(or `M-x doom/refresh`).

The `packages.el` file should **not** be byte compiled!

```emacs-lisp
;; -*- no-byte-compile: t; -*-
```

A lot of these configurations are taken from [tecosaur's](https://github.com/tecosaur/emacs-config/blob/master/config.org#packages) config.
Great source of inspiration!


### Loading instructions {#loading-instructions}


#### Packages in MELPA/ELPA/emacsmirror {#packages-in-melpa-elpa-emacsmirror}

To install packages from the default locations:

```emacs-lisp
(package! some-package-name)
```


#### Packages from Git repositories {#packages-from-git-repositories}

To install packages from git repositories, we need to specify a `:recipe`.
Check out the [documentation here](https://github.com/raxod502/straight.el#the-recipe-format).

```emacs-lisp
(package! some-package-name
  :recipe (:host github :repo "username/repo"))
```

Unless the repository have a `PACKAGENAME.el` file, or it is located in a subdirectory in the repository, we must specify
the location using `:file`.

```emacs-lisp
(package! some-package-name
  :recipe (:host github :repo "username/repo"
           :files ("some-file.el" "src/lisp/*.el")))
```

It is also possible to install packages from any git repository. If we for example want to install a package from a gist,
we could do it by specifying the receipe like this.

```emacs-lisp
(package! some-package-name
  :recipe (:host nil
           :type git
           :repo "https://gist.github.com/61b34f9ca674495eac7f1fe990ebe966.git"
           :files ("some-package-name.el")))
```


#### Disable or override built-in packages {#disable-or-override-built-in-packages}

In order tp disable a built-in package (for whatever reason), we can use the `:disable` property.

```emacs-lisp
(package! built-in-package-name :disable t)
```

We can also override built-in packages without having to specify all `:recipe` properties. These will
inherit the rest from Doom or MELPA/ELPA/emacsmirror.

```emacs-lisp
(package! built-in-package-name :recipe (:nonrecursive t))
(package! built-in-package-name :recipe (:repo "some-fork/package-name"))
(package! built-in-package-name :recipe (:branch "develop"))
```


### Tools {#tools}


#### Auto highlight symbol {#auto-highlight-symbol}

Minor mode for automatically highlighting current symbol

```emacs-lisp
(package! auto-highlight-symbol)
```

```emacs-lisp
(setq ahs-idle-interval 0.2)
(add-hook! 'text-mode-hook #'auto-highlight-symbol-mode)
(add-hook! 'prog-mode-hook #'auto-highlight-symbol-mode)
```

The default AHS font faces don't match the theme at all, let's try to fix that!

```emacs-lisp
(custom-set-faces!
  `(ahs-face                            :foreground "White" :background ,(doom-color 'magenta))
  `(ahs-face-unfocused                  :foreground "White" :background ,(doom-color 'magenta))
  `(ahs-definition-face                 :foreground "White" :background ,(doom-color 'magenta))
  `(ahs-definition-face-unfocused       :foreground "White" :background ,(doom-color 'magenta))
  `(ahs-plugin-default-face             :inherit hl-line :foreground ,(doom-color 'fg))
  `(ahs-plugin-default-face-unfocused   :inherit hl-line :foreground ,(doom-color 'magenta))
  )
```


#### Company {#company}

> From the `:completion company` module

Auto-complete, yay!

```emacs-lisp
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-show-numbers t
        company-box-doc-enable nil)

  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; Makes aborting less annoying.
```

Improvements from `precedent` are mostly from history, let's improve its memory.

```emacs-lisp
(setq-default history-length 1000
              prescient-history-length 1000)
```

Ispell is nice, let’s have it in text, markdown, and GFM.

```emacs-lisp
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))
```

We configure [Ispell](#ispell) below


#### Consult {#consult}

> From the `:completion vertico` module

Since we're using [Marginalia](#marginalia), the separation between buffers and files is already clear,
and there's no need to use a different face.

```emacs-lisp
(after! consult
  (set-face-attribute 'consult-file nil :inherit 'consult-buffer)
  (setf (plist-get (alist-get 'perl consult-async-split-styles-alist) :initial) ";"))
```


#### Dired {#dired}

The `dired-recent` package provides us with is a convenient way to visit previously visited directories.

```emacs-lisp
(package! dired-recent)         ;; convenient package for visiting frequent folders
(package! dired-open)           ;; open files in correct applications
(package! dired-hide-dotfiles)  ;; hide or show those pesky .dotfiles
```

Add some extra configuration options for the `dired` file manager.

```emacs-lisp
(after! dired
  (dired-async-mode 1)
  (dired-recent-mode 1))

(use-package! dired
  :config
  (map! :leader :desc "Dired" "-" #'dired-jump)         ;; easy access shortcut
  (map! :map dired-recent-mode-map "C-x C-d" nil)       ;; hijacks `counsult-dir' command
  (evil-collection-define-key 'normal 'dired-mode-map
    "h"         #'dired-up-directory
    "l"         #'dired-find-file
    "K"         #'dired-do-kill-lines  ;; This currently conflicts with `+lookup/documentation' keybindings..
    "r"         #'dired-recent-open
    "G"         #'consult-dir))
```

The `dired-open` can help us making sure that various files are opened in the correct program. For example,
opening a video file in Emacs is probably not going to be a pleasent experience, and would be better to open
in for example Quicktime or, VLC.
On Linux (or XDG enabled environments), the `dired-open` package has a concept of "auto-detecting" which program that should be used to open certain files. I'm mostly using macOS, which does not support XDG very well.
So, we'll try to write a the `dired-open-macos` function to use the native `open` program instead.

```emacs-lisp
;; FIXME - Need to make this a bit smarter..
;; I want to open most files in Emacs, and this lets
;; macOS decide a bit too much.
;; (use-package! dired-open
;;   :config
;;   (add-to-list 'dired-open-functions #'dired-open-macos t)
;;   (setq dired-open-extensions '(("svg" . "qlmanage -p"))))

;; (eval-after-load "dired-open"
;;   (when (eq system-type 'darwin)
;;     '(defun dired-open-macos ()
;;        "Try to run `open' to open the file under point as long as the `file' is a regular file,
;; and file extension does not exists in the `dired-open-functions' alist."
;;        (interactive)
;;        (if (executable-find "open")
;;            (let ((file (ignore-errors (dired-get-file-for-visit))))
;;              (if (and (f-file-p file) (not (member (file-name-extension file) dired-open-extensions)))
;;                  (start-process "dired-open" nil
;;                                 "open" (file-truename file)) nil)) nil))))
```

Especially when visiting my `$HOME` directory, it's littered with all sorts of dot files. Let's add a convenient
way to show or hide them.

```emacs-lisp
(use-package! dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" #'dired-hide-dotfiles-mode))
```

Since I'm trying to learn using `dired`, here's a small tutorial (based on [Systemcrafters](https://www.youtube.com/c/SystemCrafters) ❤) on basic file operations.

<!--list-separator-->

-  File Operations (The tutorial)

    <!--list-separator-->

    -  Marking files

        -   `m` - Marks a file
        -   `u` - Unmarks a file
        -   `U` - Unmarks all files in buffer
        -   `t` - Inverts marked files in buffer
        -   `%m` - Marks files in buffer using regular expressions
        -   `*` - Open menu with lots of auto-marking operations
        -   `K` - "Kills" marked items (refresh buffer with `g r` to get them back) (Currently conflicts with `+lookup/documentation` keybinding)

        Many operations can be done on a single file if there are no active marks!

    <!--list-separator-->

    -  Copying and Renaming files

        -   `C` - Copy marked files (or if no files are marked, the current file)
        -   `R` - Rename marked files, renaming multiple files is a move!
        -   `% R` - Rename based on regular expression: `^test, old-\&`
        -   `C-x C-q` - Toggle read-only mode. Can edit file names using normal editing functionality. Use `Z-Z` to save, changes or `Z-Q` to abort.

    <!--list-separator-->

    -  Deleting files

        -   `D` - Delete marked file
        -   `d` - Mark file for deletion
        -   `x` - Execute deletion for marks

    <!--list-separator-->

    -  Creating and extracting archives

        -   `Z` - Compress or uncompress a file or folder (to `.tar.gz`)
        -   `c` - Compress selection to a specific file
        -   Pressing `<Enter>` on an archive an preview files inside

        Supported compression libraries can be controlled by setting the `dired-compress-files-alist` variable.

        ```emacs-lisp
        (after! dired-aux
         (setq dired-compress-file-alist
                (append dired-compress-file-alist '(("\\.tar\\.gz\\'" . "tar -cf - %i | gzip -c9 > %o")
                                                    ("\\.tar\\.bz2\\'" . "tar -cf - %i | bzip -c9 > %o")
                                                    ("\\.tar\\.xz\\'" . "tar -cf - %i | xz -c9 > %o")
                                                    ("\\.tar\\.zst\\'" . "tar -cf - %i | zstd -19 -o %o")
                                                    ("\\.zip\\'" . "zip %o -r --filesync %i")))))
        ```

    <!--list-separator-->

    -  Other common operations

        -   `&` - Open file using a sub-process. For example `& qlmanage -p` would open the file using macOS quick look.
        -   `T` - Touch file (update timestamp)
        -   `M` - Change file mode
        -   `O` - Change file owner
        -   `g G` - Change file group
        -   `S` - Create a symbolic link to this file
        -   `L` - Load an Emacs Lisp file into Emacs
        -   `S-<enter>` - Open file in a new window
        -   `S-I` - Peek inside folder using the `dired-maybe-insert-subdir` function in the same buffer.


#### Drag-Stuff {#drag-stuff}

I like to drag stuff up and down using `C-<up>` and `C-<down>`.

```emacs-lisp
(after! drag-stuff
  (global-set-key (kbd "<C-up>") 'drag-stuff-up)
  (global-set-key (kbd "<C-down>") 'drag-stuff-down))
```


#### Evil {#evil}

> From the `:editor evil` module.

<!--list-separator-->

-  Functions

    ```emacs-lisp
    (defun evil-execute-q-macro ()
      "Execute macro stores in q-register, ie. run `@q'."
      (interactive)
      (evil-execute-macro 1 "@q"))

    (defun evil-scroll-to-center-advice (&rest args)
      "Scroll line to center, for advising functions."
      (evil-scroll-line-to-center (line-number-at-pos)))

    (defun evil-end-of-line-interactive ()
      "Wrap `evil-end-of-line' in interactive, fix point being 1+ in vis state."
      (interactive)
      (evil-end-of-line))

    (defun evil-insert-advice (&rest args)
      "Tack on after eg. heading insertion for `evil-insert' mode."
      (evil-insert 1))

    (defun evil-scroll-other-window-interactive ()
      "Wrap `scroll-other-window' in interactive."
      (interactive)
      (scroll-other-window '-))

    (defun evil-scroll-other-window-down-interactive ()
      "Wrap `scroll-other-window-down' in interactive."
      (interactive)
      (scroll-other-window))
    ```

<!--list-separator-->

-  Keybindings

    ```emacs-lisp
    (after! evil
      (setq evil-escape-key-sequence "jk")
      (setq evil-escape-unordered-key-sequence nil)
      (setq evil-respect-visual-line-mode t)

      (evil-global-set-key 'normal "Q" #'evil-execute-q-macro)
      (define-key evil-normal-state-map (kbd "C-S-u") #'evil-scroll-other-window-interactive)
      (define-key evil-normal-state-map (kbd "C-S-d") #'evil-scroll-other-window-down-interactive)
      (evil-define-key '(normal visual motion) 'global
        "H"  #'evil-first-non-blank
        "L"  #'evil-end-of-line-interactive
        "0"  #'evil-jump-item)

      (advice-add 'evil-ex-search-next     :after #'evil-scroll-to-center-advice)
      (advice-add 'evil-ex-search-previous :after #'evil-scroll-to-center-advice)
      )
    ```

    Sometimes it's convenient to insert multiple cursors using the mouse. Inserts a new cursor using `C-S-<mouse-1>`.

    ```emacs-lisp
    (after! evil
      (global-set-key (kbd "C-S-<mouse-1>") #'evil-mc-toggle-cursor-on-click))
    ```


#### Forge {#forge}

> From the `:tools magit` module

Add custom hosts

```emacs-lisp
(after! forge
  (setq gitlab.user "user")
  (add-to-list 'forge-alist '("gitlab.intility.com" "gitlab.intility.com/api/v4" "gitlab.intility.com" forge-gitlab-repository)))
```


#### Ispell {#ispell}

<!--list-separator-->

-  Downloading dictionaries

    Currently using Aspell with Norwegian and English languages.
    Should probably document installation.

<!--list-separator-->

-  Configuration

    ```emacs-lisp
    (setq ispell-dictionary "en")
    ```

    For a personal dictionaries (with all the misspelled words and such)

    ```emacs-lisp
    (setq spell-fu-directory (expand-file-name ".spell-fu_personal" doom-private-dir)
          ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-private-dir))
    ```


#### Language tool {#language-tool}

> From the `:checkers grammar` module

This seems pretty neat, so I'll experiment a bit with it.
Some issues I'd like to see resolved though:

-   Don't check org src blocks
-   Dont check org headers and properties
-   Look into running the https server to avvoid JVM spinup every time I check grammars
-   Check grammar at point - now I need to run `langtool-correct-buffer` and chose from there,
    would be nice to just correct what's at point.

<!--list-separator-->

-  Installation

    Requires installation of `languagetool` which can easily be installed with homebrew on macOS.

    ```shell
    $ brew install languagetool
    ```

<!--list-separator-->

-  Configuration

    Next, we just need to point at the `languagetool-commandline.jar` file. Easily found somewhere in
    the `$HOMEBREW_PREFIX` directory. Should probably have a better way to point at this though.

    ```shell
    $ find $HOMEBREW_PREFIX -name languagetool-commandline.jar
    /usr/local/Cellar/languagetool/5.6/libexec/languagetool-commandline.jar
    ```

    ```emacs-lisp
    (defun langtool-autoshow-detail-popup (overlays)
      (when (require 'popup nil t)
        ;; Do not interrupt current popup
        (unless (or popup-instances
                    ;; suppress popup after type `C-g` .
                    (memq last-command '(keyboard-quit)))
          (let ((msg (langtool-details-error-message overlays)))
            (popup-tip msg)))))
    ```

    ```emacs-lisp
    (after! writegood-mode
      (if (file-exists-p "/usr/local/Cellar/languagetool/5.6/libexec/languagetool-commandline.jar")
          (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/5.6/libexec/languagetool-commandline.jar")
          (setq langtool-autoshow-message-function #'langtool-autoshow-detail-popup))

      ;; Ignoring these rules makes org files behave a little nicer
      (setq langtool-disabled-rules '("WHITESPACE_RULE"
                                      "MORFOLOGIK_RULE_EN_US"
                                      "DOUBLE_PUNCTUATION"
                                      "COMMA_PARENTHESIS_WHITESPACE")))



    ```


#### Lorem-Ipsum {#lorem-ipsum}

Sometimes I need some example text to work on, use as placement holders and so on.

```emacs-lisp
(package! lorem-ipsum)
```

```emacs-lisp
(use-package! lorem-ipsum
  :config (lorem-ipsum-use-default-bindings))
```

Default keybindings for this package is:

-   `C-c l s` - Insert sentences
-   `C-c l p` - Insert paragraphs
-   `C-c l l` - Insert lists


#### Magit {#magit}

> From the `:tools magit` module

Making `magit` a tad prettier.
(Taken from the [Modern Emacs](http://www.modernemacs.com/#spacemacs) blog)

```emacs-lisp
(after! magit
  (defvar pretty-magit--alist nil
    "An alist of regexes, an icon, and face properties to apply to icon.")

  (defvar pretty-magit--prompt nil
    "A list of commit leader prompt candidates.")

  (defvar pretty-magit--use-commit-prompt? nil
    "Do we need to use the magit commit prompt?")

  (defun pretty-magit--add-magit-faces ()
    "Add face properties and compose symbols for buffer from pretty-magit."
    (interactive)
    (with-silent-modifications
      (-each pretty-magit--alist
        (-lambda ((rgx char face-props))
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward rgx nil t)
              (-let [(start end) (match-data 1)]
                (compose-region start end char)
                (when face-props
                  (add-face-text-property start end face-props)))))))))

  (defun pretty-magit-add-leader (word char face-props)
    "Replace sanitized WORD with CHAR having FACE-PROPS and add to prompts."
    (add-to-list 'pretty-magit--alist
                 (list (rx-to-string `(: bow
                                       (group ,word ":")))
                       char face-props))
    (add-to-list 'pretty-magit--prompt
                 (concat word ": ")))

  (defun pretty-magit-add-leaders (leaders)
    "Map `pretty-magit-add-leader' over LEADERS."
    (-each leaders
      (-applify #'pretty-magit-add-leader)))

  (defun pretty-magit--use-commit-prompt (&rest args)
    (setq pretty-magit--use-commit-prompt? t))

  (defun pretty-magit-commit-prompt ()
    "Magit prompt and insert commit header with faces."
    (interactive)
    (when (and pretty-magit--use-commit-prompt?
               pretty-magit--prompt)
      (setq pretty-magit--use-commit-prompt? nil)
      (insert (completing-read "Commit Type " pretty-magit--prompt nil 'confirm))
      (pretty-magit--add-magit-faces)
      (evil-insert 1)))

  (defun pretty-magit-setup (&optional no-commit-prompts?)
    "Advise the appropriate magit funcs to add pretty-magit faces."
    (advice-add 'magit-status         :after 'pretty-magit--add-magit-faces)
    (advice-add 'magit-refresh-buffer :after 'pretty-magit--add-magit-faces)

    (unless no-commit-prompts?
      (remove-hook 'git-commit-setup-hook 'with-editor-usage-message)
      (add-hook    'git-commit-setup-hook 'pretty-magit-commit-prompt)

      (advice-add 'magit-commit-create :after 'pretty-magit--use-commit-prompt))))
```

```emacs-lisp
(after! magit
  (pretty-magit-add-leaders '(("feature" ? (:foreground "slate gray" :height 1.2))
                              ("add"     ? (:foreground "#375E97" :height 1.2))
                              ("fix"     ? (:foreground "#FB6542" :height 1.2))
                              ("clean"   ? (:foreground "#FFBB00" :height 1.2))
                              ("docs"    ? (:foreground "#3F681C" :height 1.2))))
  (pretty-magit-setup))
```


#### Marginalia {#marginalia}

> From the `:completion vertico` module

[Marginalia](https://github.com/minad/marginalia) is a tool (written by the same author as vertico) which adds marginalia to the
mini-buffer completions. Marginalia are marks and annotations placed at the margin of a page
in a book.

The below settings are basically just copied of [tecosaur's](https://tecosaur.github.io/emacs-config/config.html#marginalia) config, and makes it look a bit nicer.

-   Add color to file attributes
-   Don't display user:group information if I am the owner/group
-   When a file modified time is quite recent, use relative age (eg. `2h ago`)
-   Add fatter font face for bigger files

<!--listend-->

```emacs-lisp
(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))
```


#### Pinentry {#pinentry}

Emacs pinentry integration

```emacs-lisp
(package! pinentry)
```


#### Projectile {#projectile}

> From the `:core packages` module

Set some sensible `projectile` settings.

```emacs-lisp
(setq projectile-enable-caching               t
      projectile-project-search-path          '("~/workspace")  ;; A relic directory from when I used Eclipse back in the days
      projectile-globally-ignored-files       '(".DS_Store")    ;; Super annoying files
      projectile-globally-ignored-directories '(".git"          ;; I never want to cache files in these directories
                                                ".idea"
                                                ".import"
                                                ".elixir_ls"
                                                ".htmlcov"
                                                ".pytest_cache"
                                                "_build"
                                                "__pycache__"
                                                "deps"
                                                "node_modules"))
```


#### Smartparens {#smartparens}

> From the `:core packages` module.

```emacs-lisp
(after! smart-parens
  (sp-local-pair '(org-mode) "<<" ">>" :actions '(insert)))
```


#### Tramp {#tramp}

Tramp makes accessing remote file systems using Emacs a blast.

```emacs-lisp
(after! tramp
  (setq tramp-default-method "scp"))
```


#### Tree-sitter {#tree-sitter}

Tree-sitter is a general programming language parser that efficiently builds and updates ASTs (Abstract Syntax Trees)
for your code.
Let's give it a spin!

```emacs-lisp
(package! tree-sitter)
(package! tree-sitter-langs)
```

```emacs-lisp
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
```


#### Treemacs {#treemacs}

Make `treemacs` pretty and functional.

```emacs-lisp
(after! (treemacs winum)
  (setq doom-themes-treemacs-theme "doom-colors")       ; Enable nice colors for treemacs
  (setq doom-themes-treemacs-enable-variable-pitch nil) ; Don't use variable-pitch font
  (setq winum-ignored-buffers-regexp
        (delete (regexp-quote (format "%sFramebuffer-" treemacs--buffer-name-prefix))
                winum-ignored-buffers-regexp))

  (treemacs-project-follow-mode t)
  (treemacs-follow-mode t))
```

LSP-Treemacs is an integration package between `lsp-mode` and `treemacs`. It's nice :)

```emacs-lisp
(after! (treemacs lsp-mode)
  (lsp-treemacs-sync-mode 1))

(map! :leader
      :after lsp-treemacs
      (:prefix-map ("c" . "code")
       :desc "List errors (LSP Treemacs)" "X" #'lsp-treemacs-errors-list)
      (:prefix-map ("t" . "toggle")
       :desc "LSP symbols"                "y" #'lsp-treemacs-symbols))
```


#### Vertico {#vertico}

> From the `:completion vertico` module

[Vertico](https://github.com/minad/vertico) is the new kid on the block when it comes to searching in Emacs. Built on top of the native
Emacs search APIs instead of 3rd party tools.

```emacs-lisp
(after! vertico
  ;; Emacs 28+: Hide commands in M-x that doesn't work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; These mappings seems a bit inverted, but it works as I like it, so I dunno...?
  (map! :map vertico-map
        "C-u"   #'vertico-scroll-down
        "C-d"   #'vertico-scroll-up))
```

**Tips'n tricks**

-   Sometimes you want to input some value that's similar to a pre-selected choice. We can use the input
    value by hitting `M-<enter>` to use our input value instead of the pre-selected value.

<!--list-separator-->

-  Vertico Posframe (experimental)

    I've seen some cool stuff using a separate `posframe` for displaying `vertico` results. I'm still not convinced,
    because it tends to hide the content of my buffers when displayed on top of it.
    Either case; it can be enabled by installing the `vertico-posframe` package.

    ```emacs-lisp
    (package! vertico-posframe)
    ```

    And enabled like this.

    ```emacs-lisp
    (after! vertico
      (vertico-posframe-mode 1))
    ```


#### Very large files {#very-large-files}

The [very large files](https://github.com/m00natic/vlf) mode loads large files in chunks, letting us open ridiculously large files!

```emacs-lisp
(package! vlf
  :recipe (:host github :repo "m00natic/vlfi" :files ("*.el") :branch "master"))
```

We usually don't need this package right away, so we'll delay the loading a bit.

```emacs-lisp
(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)
```


#### VTerm {#vterm}

> From the `:term vterm` module

The `vterm` gives us native performing terminal emulation inside Emacs; what's not to like?

**TODO - Annoying stuff that doesn't work in vterm**

-   `<delete>` doesn't delete words forwards
-   `M-<backspace>` doesn't delete words backwards
-   Can't type `M`??

<!--listend-->

```emacs-lisp
(map! :after vterm
      :map vterm-mode-map
      "M-<backspace>" #'vterm-send-meta-backspace)
```


#### YASnippet {#yasnippet}

> From the `:editor snippets` module.

Enable nested snippets.

```emacs-lisp
(setq yas-triggers-in-field t)
```

**Extra snippets**
Snippets are basic text files (no extension), located in directories representing the mode they will
be active for. For a quick guide on how we can organize snippets, take a look at this [page](https://joaotavora.github.io/yasnippet/snippet-organization.html).

```nil
.
|-- c-mode
|   -- printf
|-- elixir-mode
|   -- defmodule
|   -- macro
|-- org-mode
    -- header-props
    -- and-so-on
```

I like to keep my custom stuff in the _misc_ directory.

```emacs-lisp
(setq +snippets-dir (concat (file-name-as-directory doom-private-dir) "misc/snippets"))
```


### Visuals {#visuals}


#### Info colors {#info-colors}

This makes manual pages look pretty by adding variable pitch fortification and colors 🤗

```emacs-lisp
(package! info-colors)
```

To use this we'll just hook into `Info`.

```emacs-lisp
(use-package! info-colors
  :after info
  :hook (Info-selection-hook . info-colors-fontify-node)
  :commands (info-colors-fontify-node))

```


#### Writeroom {#writeroom}

> From the `:ui zen` module.

I'd like to toggle `writeroom-mode` for all buffers at once.

```emacs-lisp
(map! :leader
      (:when (featurep! :ui zen)
       (:prefix-map ("t" . "toggle")
        :desc "Zen mode"        "z"     #'global-writeroom-mode)))
```

Slightly decrease the default text scale for `writeroom-mode`.

```emacs-lisp
(setq +zen-text-scale 0.8)
```

When using Writeroom mode with Org, make some additional aesthetic tweaks:

-   Use a serifed variable-pitch font
-   Hide headline leading stars
-   Nicer headline bullets
-   Hide line numbers
-   Remove outline indentation
-   Centering the text
-   Turn on `org-pretty-table-mode`

<!--listend-->

```emacs-lisp
(defvar +zen-serif-p t
  "Wheter to use a serifed font with `mixed-pitch-mode'.")

(after! writeroom-mode
  (defvar-local +zen--original-org-indent-mode-p nil)
  (defvar-local +zen--original-mixed-pitch-mode-p nil)
  (defvar-local +zen--original-org-pretty-table-mode-p nil)
  (defun +zen-enable-mixed-pitch-mode-h ()
    "Enable `mixed-pitch-mode' when in `+zen-mixed-pitch-modes'."
    (when (apply #'derived-mode-p +zen-mixed-pitch-modes)
      (if writeroom-mode
          (progn
            (setq +zen--original-mixed-pitch-mode-p mixed-pitch-mode)
            (funcall (if +zen-serif-p #'mixed-pitch-serif-mode #'mixed-pitch-mode) 1))
        (funcall #'mixed-pitch-mode (if +zen--original-mixed-pitch-mode-p 1 -1)))))
  (pushnew! writeroom--local-variables
            'display-line-numbers
            'visual-fill-column-width
            'org-adapt-indentation
            'org-superstar-headline-bullets-list
            'org-superstar-remove-leading-stars)
  (add-hook 'writeroom-mode-enable-hook
            (defun +zen-prose-org-h ()
              "Reformat the current Org buffer appearance for prose."
              (when (eq major-mode 'org-mode)
                (setq display-line-numbers nil
                      visual-fill-column-width 60
                      org-adapt-indentation nil)
                (when (featurep 'org-superstar)
                  (setq-local org-superstar-headline-bullets-list '("🙘" "🙙" "🙚" "🙛")
                              ;; org-superstar-headline-bullets-list '("🙐" "🙑" "🙒" "🙓" "🙔" "🙕" "🙖" "🙗")
                              org-superstar-remove-leading-stars t)
                  (org-superstar-restart))
                (setq
                 +zen--original-org-indent-mode-p org-indent-mode
                 +zen--original-org-pretty-table-mode-p (bound-and-true-p org-pretty-table-mode))
                (org-indent-mode -1)
                (org-pretty-table-mode 1))))
  (add-hook 'writeroom-mode-disable-hook
            (defun +zen-nonprose-org-h ()
              "Reverse the effect of `+zen-prose-org'."
              (when (eq major-mode 'org-mode)
                (when (featurep 'org-superstar)
                  (org-superstar-restart))
                (when +zen--original-org-indent-mode-p (org-indent-mode 1))
                ;; (unless +zen--original-org-pretty-table-mode-p (org-pretty-table-mode -1))

                ;; TODO disable ahs-mode
                ))))
```


## Language configurations {#language-configurations}


### General {#general}

General configuration


#### File templates {#file-templates}

For some file types, we overwrite defaults in the snippets directory, others need to have a template assigned.

```emacs-lisp
;; (set-file-template! "\\.tex$" :trigger "__" :mode 'latex-mode)
(set-file-template! "\\.org$" :trigger "__" :mode 'org-mode)
(set-file-template! "/LICEN[CS]E$" :trigger '+file-templates/insert-license)
```


### Yaml {#yaml}

Yaml comes in many different flavors, let's select schema with `,-s`.

```emacs-lisp
(map! :map yaml-mode-map
      :localleader
      :desc "Select buffer schema"      "s"     #'lsp-yaml-select-buffer-schema)
```


### Plaintext {#plaintext}

It’s nice to see ANSI colour codes displayed. However, until Emacs 28 it’s not possible to do this without modifying the buffer, so let’s condition this block on that.

```emacs-lisp
(after! text-mode
  (add-hook! 'text-mode-hook
             ;; Apply ANSI color codes
             (with-silent-modifications
               (ansi-color-apply-on-region (point-min) (point-max) t))))
```


### Org {#org}

Everybody likes org-mode!

```emacs-lisp
(setq org-directory                             "~/Dropbox/org"
      org-crypt-key                             "rhblind@gmail.com"
      org-tag-alist                             '(("crypt" . ?c))
      org-id-link-to-org-use-id                 t
      org-agenda-text-search-extra-files        '())
(setq org-download-image-dir (concat (file-name-as-directory org-directory) "images"))
```


#### Behaviour {#behaviour}

<!--list-separator-->

-  Keybindings

    Add some handy keybindings under `SPC-, x` for emphasizing text in `org-mode`.

    ```emacs-lisp
    (after! org
      (defmacro cust/org-emphasize (fname char)
        "Macro that creates a function for setting emphasises in org-mode"
        `(defun ,fname () (interactive)
                (org-emphasize ,char))))
    ```

    ```emacs-lisp
    (map! :map org-mode-map
          :after org
          :localleader "x" nil                                      ;; raises Key sequence error when trying to rebind below unless unset first.
          :desc "org-toggle-checkbox" "X"   #'org-toggle-checkbox)  ;; rebind org-toggle-checkbox to "X"
    (map! :map org-mode-map
          :after org
          :localleader
          (:prefix ("x" . "text")
           :desc "bold"             "b"     (cust/org-emphasize cust/org-emphasize-bold ?*)
           :desc "code"             "c"     (cust/org-emphasize cust/org-emphasize-code ?~)
           :desc "italic"           "i"     (cust/org-emphasize cust/org-emphasize-italic ?/)
           :desc "clear"            "r"     (cust/org-emphasize cust/org-emphasize-clear ? )
           :desc "strike-through"   "s"     (cust/org-emphasize cust/org-emphasize-strike-through ?+)
           :desc "underline"        "u"     (cust/org-emphasize cust/org-emphasize-underline ?_)
           :desc "verbatim"         "v"     (cust/org-emphasize cust/org-emphasize-verbatim ?=)))
    ```


#### Babel {#babel}

Org babel is used to evaluate code blocks in org files.

```emacs-lisp
(after! org
  (setq org-confirm-babel-evaluate   nil
        org-src-fontify-natively     t
        org-src-tab-acts-natively    t
        org-src-preserve-indentation t
        org-src-window-setup         'current-window
        org-babel-default-header-args '((:session . "none")
                                        (:results . "replace")
                                        (:exports . "code")
                                        (:cache   . "no")
                                        (:noweb   . "no")
                                        (:hlines  . "no")
                                        (:tangle  . "no")
                                        (:comment . "link")))

  (add-to-list 'org-babel-load-languages '(dot . t)))
```

Sometimes it's nice to auto-format code blocks.

```emacs-lisp
(after! org
  (defun org-indent-src-block ()
    "Indent the source block at point."
    (interactive)
    (when (org-in-src-block-p)
      (org-edit-special)
      (indent-region (point-min) (point-max))
      (org-edit-src-exit))))
```


#### Reveal export {#reveal-export}

By default reveal is rather nice, there are just a few tweaks that I consider a good idea.

```emacs-lisp
(setq org-re-reveal-theme "white"
      org-re-reveal-transition "slide"
      org-re-reveal-plugins '(markdown notes math search zoom))
```


#### Hugo (Blog) {#hugo--blog}

> From the `:lang org +hugo` module

Install the `hugo` blog engine using homebrew, and set the `org-hugo-base-dir` variable.
(We can also set the `HUGO_BASE_DIR` environmental variable).

[[<https://spcbfr.vercel.app/posts/blogging-setup-hugo-and-org>][How to make a blog with hugo and org-mode
: Youssef Bouzekri — A developer w...]]

[Host on GitHub | Hugo](https://gohugo.io/hosting-and-deployment/hosting-on-github/)

[Org-mode configuration for Emacs - Hugo Cisneros](https://hugocisneros.com/org-config/)

<!--listend-->

```emacs-lisp
(when (and (featurep! :lang org +hugo) (eq system-type 'darwin))
  (setq org-hugo-base-dir (concat (expand-file-name (file-name-as-directory "~")) "workspace/hugo-blog")))
```

Create a new blog

```sh
$ brew install hugo
$ hugo new site ~/Dropbox/org/hugo-blog
```


#### Roam {#roam}

> From the `:lang org +roam` module

```emacs-lisp
(setq org-roam-v2-ack        t
      org-roam-directory     (concat (file-name-as-directory org-directory) "roam"))
(setq org-roam-index-file    (concat (file-name-as-directory org-roam-directory) "index.org"))
```

Make sure roam is available on startup.

```emacs-lisp
(org-roam-db-autosync-mode)
```

Add all `org-roam` files to list of extra files to be searched by text commands.

```emacs-lisp
(after! (org org-roam)
  (setq org-agenda-text-search-extra-files (org-roam--list-files org-roam-directory)))
```


#### Krita {#krita}

Krita is a wonderful digital painting program which also can be made available in org-mode for doing quick
sketching or hand written notes and so on.

```emacs-lisp
(package! org-krita
  :recipe (:host github
           :repo "lepisma/org-krita"
           :files ("resources" "resources" "*.el" "*.el")))
```

```emacs-lisp
(use-package! org-krita
  :config
  (add-hook 'org-mode-hook 'org-krita-mode)
  (when (eq system-type 'darwin)
    (setq org-krita-executable "/Applications/krita.app/Contents/MacOS/krita")))
```


#### Visuals {#visuals}

<!--list-separator-->

-  Tables

    Org tables aren’t the prettiest thing to look at.
    This package is supposed to redraw them in the buffer with box-drawing characters.
    Sounds like an improvement to me! We’ll make use of this with writeroom-mode.

    ```emacs-lisp
    (package! org-pretty-table
      :recipe (:host github :repo "Fuco1/org-pretty-table" :branch "master"))
    ```

    ```emacs-lisp
    (use-package! org-pretty-table
      :hook (org-mode . org-pretty-table-mode)
      :commands (org-pretty-table-mod global-org-pretty-table-mode))
    ```

<!--list-separator-->

-  Emphasis markers

    While org-hide-emphasis-markers is very nice, it can sometimes make edits which
    occur at the border a bit more fiddley.
    We can improve this situation without sacrificing visual amenities with the org-appear package.

    ```emacs-lisp
    (package! org-appear :recipe (:host github :repo "awth13/org-appear"))
    ```

    ```emacs-lisp
    (use-package! org-appear
      :hook (org-mode . org-appear-mode)
      :config
      (setq org-appear-autoemphasis t
            org-appear-autosubmarkers t
            org-appear-autolinks nil)
      ;; for proper first-time setup, `org-appear--set-elements'
      ;; needs to be run after other hooks have acted.
      (run-at-time nil nil #'org-appear--set-elements))
    ```

<!--list-separator-->

-  Heading minimap

    Outline structure of the org documents.

    ```emacs-lisp
    (package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree"))
    ```

    ```emacs-lisp
    (use-package! org-ol-tree
      :commands org-ol-tree)

    (map! :map org-mode-map
          :after org
          :localleader
          :desc "Outline" "O" #'org-ol-tree)
    ```

<!--list-separator-->

-  Symbols

    This section is pretty much just copied from tecosaurs config (again...)

    ```emacs-lisp
    (after! org-superstar
      (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
            org-superstar-prettify-item-bullets t ))

    (setq org-ellipsis " ▾ "
          org-hide-leading-stars t
          org-priority-highest ?A
          org-priority-lowest ?E
          org-priority-faces
          '((?A . 'all-the-icons-red)
            (?B . 'all-the-icons-orange)
            (?C . 'all-the-icons-yellow)
            (?D . 'all-the-icons-green)
            (?E . 'all-the-icons-blue)))
    ```

    It's also nice to make use of the Unicode characters for check boxes, and other commands.

    ```emacs-lisp
    (appendq! +ligatures-extra-symbols
              `(:checkbox      "☐"
                :pending       "◼"
                :checkedbox    "☑"
                :list_property "∷"
                :em_dash       "—"
                :ellipses      "…"
                :arrow_right   "→"
                :arrow_left    "←"
                :title         "𝙏"
                :subtitle      "𝙩"
                :author        "𝘼"
                :date          "𝘿"
                :property      "☸"
                :options       "⌥"
                :startup       "⏻"
                :macro         "𝓜"
                :html_head     "🅷"
                :html          "🅗"
                :latex_class   "🄻"
                :latex_header  "🅻"
                :beamer_header "🅑"
                :latex         "🅛"
                :attr_latex    "🄛"
                :attr_html     "🄗"
                :attr_org      "⒪"
                :begin_quote   "❝"
                :end_quote     "❞"
                :caption       "☰"
                :header        "›"
                :results       "🠶"
                :begin_export  "⏩"
                :end_export    "⏪"
                :properties    "⚙"
                :end           "∎"
                :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
                :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
                :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
                :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
                :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)))

    (set-ligatures! 'org-mode
      :merge t
      :checkbox      "[ ]"
      :pending       "[-]"
      :checkedbox    "[X]"
      :list_property "::"
      :em_dash       "---"
      :ellipsis      "..."
      :arrow_right   "->"
      :arrow_left    "<-"
      :title         "#+title:"
      :subtitle      "#+subtitle:"
      :author        "#+author:"
      :date          "#+date:"
      :property      "#+property:"
      :options       "#+options:"
      :startup       "#+startup:"
      :macro         "#+macro:"
      :html_head     "#+html_head:"
      :html          "#+html:"
      :latex_class   "#+latex_class:"
      :latex_header  "#+latex_header:"
      :beamer_header "#+beamer_header:"
      :latex         "#+latex:"
      :attr_latex    "#+attr_latex:"
      :attr_html     "#+attr_html:"
      :attr_org      "#+attr_org:"
      :begin_quote   "#+begin_quote"
      :end_quote     "#+end_quote"
      :caption       "#+caption:"
      :header        "#+header:"
      :begin_export  "#+begin_export"
      :end_export    "#+end_export"
      :results       "#+RESULTS:"
      :property      ":PROPERTIES:"
      :end           ":END:"
      :priority_a    "[#A]"
      :priority_b    "[#B]"
      :priority_c    "[#C]"
      :priority_d    "[#D]"
      :priority_e    "[#E]")
    (plist-put +ligatures-extra-symbols :name "⁍")
    ```

<!--list-separator-->

-  Font Display

    Mixed pitch is pretty great, and so is `+org-pretty-mode`. Use them together!

    ```emacs-lisp
    (add-hook! 'org-mode-hook #'+org-pretty-mode)
    ```

    Make headings slightly bigger

    ```emacs-lisp
    (custom-set-faces!
      '(outline-1 :weight extra-bold :height 1.25)
      '(outline-2 :weight bold :height 1.15)
      '(outline-3 :weight bold :height 1.12)
      '(outline-4 :weight semi-bold :height 1.09)
      '(outline-5 :weight semi-bold :height 1.06)
      '(outline-6 :weight semi-bold :height 1.03)
      '(outline-8 :weight semi-bold)
      '(outline-9 :weight semi-bold))
    ```

    And the same with the title.

    ```emacs-lisp
    (custom-set-faces!
      '(org-document-title :height 1.2))
    ```

    It seems reasonable to have deadlines in the error face when they’re passed.

    ```emacs-lisp
    (setq org-agenda-deadline-faces
          '((1.001 . error)
            (1.0   . org-warning)
            (0.5   . org-upcoming-deadline)
            (0.0   . org-upcoming-distant-deadline)))
    ```

    We can then have quote blocks stand out a bit more by making them _italic_.

    ```emacs-lisp
    (setq org-fontify-quote-and-verse-blocks t)
    ```

<!--list-separator-->

-  Super agenda

    A super agenda

    ```emacs-lisp
    (package! org-super-agenda)
    ```

    ```emacs-lisp
    (use-package! org-super-agenda
      :commands org-super-agenda-mode)
    ```

    ```emacs-lisp
    (after! org-agenda
      (org-super-agenda-mode))

    (setq org-agenda-breadcrumbs-separator  " ❱ "
          org-agenda-compact-blocks         t
          org-agenda-include-deadlines      t       ;; Include deadlines in the agenda
          org-agenda-skip-deadline-if-done  t       ;; Don't include deadlines in the agenda if they're in the `DONE' state
          org-agenda-skip-scheduled-if-done t
          org-agenda-skip-scheduled-if-done t       ;; Don't include items in the agenda if they're in the `DONE' state
          org-agenda-tags-column            100     ;; from testing this seems to be a good value
          org-super-agenda-header-map       nil     ;; Fixes issues with evil-mode
          org-agenda-block-separator        9472)   ;; Use a straight line as separator between agenda agenda blocks


    (setq org-agenda-custom-commands
          '(("o" "Overview"
             ((agenda "" ((org-agenda-span 'day)
                          (org-super-agenda-groups
                           '((:name "Today"
                              :time-grid t
                              :date today
                              :todo "TODAY"
                              :scheduled today
                              :order 1)))))
              (alltodo "" ((org-agenda-overriding-header "")
                           (org-super-agenda-groups
                            '((:name "Next to do"
                               :todo "NEXT"
                               :order 1)
                              (:name "Important"
                               :tag "Important"
                               :priority "A"
                               :order 6)
                              (:name "Due Today"
                               :deadline today
                               :order 2)
                              (:name "Due Soon"
                               :deadline future
                               :order 8)
                              (:name "Overdue"
                               :deadline past
                               :face error
                               :order 7)
                              (:name "Work"
                               :tag "Work"
                               :order 10)
                              (:name "Issues"
                               :tag "Issue"
                               :order 12)
                              (:name "Emacs"
                               :tag "Emacs"
                               :order 13)
                              (:name "Projects"
                               :tag "Project"
                               :order 14)
                              (:name "Research"
                               :tag "Research"
                               :order 15)
                              (:name "To read"
                               :tag "Read"
                               :order 30)
                              (:name "Waiting"
                               :todo "WAITING"
                               :order 20)
                              (:name "University"
                               :tag "uni"
                               :order 32)
                              (:name "Trivial"
                               :priority<= "E"
                               :tag ("Trivial" "Unimportant")
                               :todo ("SOMEDAY" )
                               :order 90)
                              (:discard (:tag ("Chore" "Routine" "Daily")))))))))))
    ```

<!--list-separator-->

-  Capture templates (DOCT)

    ```emacs-lisp
    (package! doct)
    (package! doct-org-roam
      :recipe (:host nil :type git :repo "https://gist.github.com/f9b21eeea7d7c9123dc400a30599d50d.git" :files ("doct-org-roam.el")))
    ```

    ```emacs-lisp
    (use-package! doct :commands doct)
    ```

    <!--list-separator-->

    -  Visuals

        This piece of code improves how the capturing templates look.

        ```emacs-lisp
        (after! org-capture
          (defun org-capture-select-template-prettier (&optional keys)
            "Select a capture template, in a prettier way than default
        Lisp programs can force the template by setting KEYS to a string."
            (let ((org-capture-templates
                  (or (org-contextualize-keys
                        (org-capture-upgrade-templates org-capture-templates)
                        org-capture-templates-contexts)
                      '(("t" "Task" entry (file+headline "" "Tasks")
                          "* TODO %?\n  %u\n  %a")))))
              (if keys
                  (or (assoc keys org-capture-templates)
                      (error "No capture template referred to by \"%s\" keys" keys))
                (org-mks org-capture-templates
                        "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
                        "Template key: "
                        `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
          (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

          (defun org-mks-pretty (table title &optional prompt specials)
            "Select a member of an alist with multiple keys. Prettified.

        TABLE is the alist which should contain entries where the car is a string.
        There should be two types of entries.

        1. prefix descriptions like (\"a\" \"Description\")
          This indicates that `a' is a prefix key for multi-letter selection, and
          that there are entries following with keys like \"ab\", \"ax\"…

        2. Select-able members must have more than two elements, with the first
          being the string of keys that lead to selecting it, and the second a
          short description string of the item.

        The command will then make a temporary buffer listing all entries
        that can be selected with a single key, and all the single key
        prefixes.  When you press the key for a single-letter entry, it is selected.
        When you press a prefix key, the commands (and maybe further prefixes)
        under this key will be shown and offered for selection.

        TITLE will be placed over the selection in the temporary buffer,
        PROMPT will be used when prompting for a key.  SPECIALS is an
        alist with (\"key\" \"description\") entries.  When one of these
        is selected, only the bare key is returned."
            (save-window-excursion
              (let ((inhibit-quit t)
                    (buffer (org-switch-to-buffer-other-window "*Org Select*"))
                    (prompt (or prompt "Select: "))
                    case-fold-search
                    current)
                (unwind-protect
                    (catch 'exit
                      (while t
                        (setq-local evil-normal-state-cursor (list nil))
                        (erase-buffer)
                        (insert title "\n\n")
                        (let ((des-keys nil)
                              (allowed-keys '("\C-g"))
                              (tab-alternatives '("\s" "\t" "\r"))
                              (cursor-type nil))
                          ;; Populate allowed keys and descriptions keys
                          ;; available with CURRENT selector.
                          (let ((re (format "\\`%s\\(.\\)\\'"
                                            (if current (regexp-quote current) "")))
                                (prefix (if current (concat current " ") "")))
                            (dolist (entry table)
                              (pcase entry
                                ;; Description.
                                (`(,(and key (pred (string-match re))) ,desc)
                                (let ((k (match-string 1 key)))
                                  (push k des-keys)
                                  ;; Keys ending in tab, space or RET are equivalent.
                                  (if (member k tab-alternatives)
                                      (push "\t" allowed-keys)
                                    (push k allowed-keys))
                                  (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                                ;; Usable entry.
                                (`(,(and key (pred (string-match re))) ,desc . ,_)
                                (let ((k (match-string 1 key)))
                                  (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                                  (push k allowed-keys)))
                                (_ nil))))
                          ;; Insert special entries, if any.
                          (when specials
                            (insert "─────────────────────────\n")
                            (pcase-dolist (`(,key ,description) specials)
                              (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                              (push key allowed-keys)))
                          ;; Display UI and let user select an entry or
                          ;; a sub-level prefix.
                          (goto-char (point-min))
                          (unless (pos-visible-in-window-p (point-max))
                            (org-fit-window-to-buffer))
                          (let ((pressed (org--mks-read-key allowed-keys
                                                            prompt
                                                            (not (pos-visible-in-window-p (1- (point-max)))))))
                            (setq current (concat current pressed))
                            (cond
                            ((equal pressed "\C-g") (user-error "Abort"))
                            ;; Selection is a prefix: open a new menu.
                            ((member pressed des-keys))
                            ;; Selection matches an association: return it.
                            ((let ((entry (assoc current table)))
                                (and entry (throw 'exit entry))))
                            ;; Selection matches a special entry: return the
                            ;; selection prefix.
                            ((assoc current specials) (throw 'exit current))
                            (t (error "No entry available")))))))
                  (when buffer (kill-buffer buffer))))))
          (advice-add 'org-mks :override #'org-mks-pretty))
        ```

    <!--list-separator-->

    -  Templates

        Set up the actual templates for different categories.
        [Here's](https://www.orgmode.org/manual/Template-expansion.html) a nice overview of `org-capture-template` variables.

        ```emacs-lisp
        (after! org-capture
          (defun +doct-icon-declaration-to-icon (declaration)
            "Convert :icon declaration to icon"
            (let ((name (pop declaration))
                  (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
                  (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
                  (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
              (apply set `(,name :face ,face :v-adjust ,v-adjust))))

          (defun +doct-iconify-capture-templates (groups)
            "Add declaration's :icon to each template group in GROUPS."
            (let ((templates (doct-flatten-lists-in groups)))
              (setq doct-templates (mapcar (lambda (template)
                                             (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                         (spec (plist-get (plist-get props :doct) :icon)))
                                               (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                              "\t"
                                                                              (nth 1 template))))
                                             template)
                                           templates))))

          (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

          (defvar +org-capture-recipies
            (concat (file-name-as-directory org-directory) "capture-recipies.org"))

          (defvar +org-capture-blog-file
            (concat (file-name-as-directory org-hugo-base-dir) "blog.org"))

          (defun set-org-capture-templates ()
            ;; `org-roam' capture templates
            ;; Enabled by the `doct-org-roam' package installed from gist above.
            ;; FIXME - Can't make this work properly
            ;; (require 'doct-org-roam)
            ;; (setq org-roam-capture-templates
            ;;       (doct-org-roam `(("Default" :keys "d"
            ;;                         :type plain
            ;;                         :file "%<%Y%m%d%H%M%S>-${slug}.org"
            ;;                         :head "#+title: ${title}\n\n"
            ;;                         :template "* Hallo"
            ;;                         :unnarrowed t)
            ;;                        ("Blog entry (Hugo)" :keys "b"
            ;;                         :headline "Hugo Blog"
            ;;                         :type entry
            ;;                         :file "%<%Y%m%d%H%M%S>-${slug}.org"
            ;;                         :template ("* %{title}" ":properties:"
            ;;                                    ":export_file_name: %\\1"
            ;;                                    ":export_description: %^{Description}"
            ;;                                    ":export_date: %^{Date}t"
            ;;                                    ":export_author: %n"
            ;;                                    ":end:"
            ;;                                    ""
            ;;                                    "%?")
            ;;                         :custom (:title "%^{Title}")
            ;;                         :unnarrowed nil
            ;;                         ))))

            ;; `org' capture templates
            (setq org-capture-templates
                  (doct `(("Personal todo" :keys "t"
                           :icon ("checklist" :set "octicon" :color "green")
                           :file +org-capture-todo-file
                           :prepend t
                           :headline "Inbox"
                           :type entry
                           :template ("* TODO %? %^G"
                                      "%i"))
                          ("Personal note" :keys "n"
                           :icon ("sticky-note-o" :set "faicon" :color "green")
                           :file +org-capture-todo-file
                           :prepend t
                           :headline "Inbox"
                           :type entry
                           :template ("* %? %^G"
                                      "%i"))
                          ;; ("Roam" :keys "r"
                          ;;  :icon ("pied-piper" :set "faicon" :color "pink")
                          ;;  :function org-roam-capture)
                          ("Email" :keys "e"
                           :icon ("envelope" :set "faicon" :color "blue")
                           :file +org-capture-todo-file
                           :prepend t
                           :headline "Inbox"
                           :type entry
                           :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                                      "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                                      "about %^{topic}"
                                      "%U %i"))
                          ("Interesting" :keys "i"
                           :icon ("eye" :set "faicon" :color "lcyan")
                           :file +org-capture-todo-file
                           :prepend t
                           :headline "Interesting"
                           :type entry
                           :template ("* [ ] %{desc}%? :%{i-type}:"
                                      "%i")
                           :children (("Webpage" :keys "w"
                                       :icon ("globe" :set "faicon" :color "green")
                                       :desc "%(org-cliplink-capture) "
                                       :i-type "read:web")
                                      ("Article" :keys "a"
                                       :icon ("file-text" :set "octicon" :color "yellow")
                                       :desc ""
                                       :i-type "read:reaserch")
                                      ("\tRecipie" :keys "r"
                                       :icon ("spoon" :set "faicon" :color "dorange")
                                       :file +org-capture-recipies
                                       :headline "Unsorted"
                                       :template "%(org-chef-get-recipe-from-url)")
                                      ("Information" :keys "i"
                                       :icon ("info-circle" :set "faicon" :color "blue")
                                       :desc ""
                                       :i-type "read:info")
                                      ("Idea" :keys "I"
                                       :icon ("bubble_chart" :set "material" :color "purple")
                                       :desc ""
                                       :i-type "idea")))
                          ("Tasks" :keys "k"
                           :icon ("inbox" :set "octicon" :color "yellow")
                           :file +org-capture-todo-file
                           :prepend t
                           :headline "Tasks"
                           :type entry
                           :template ("* TODO %? %^G%{extra}"
                                      "%i")
                           :children (("General Task" :keys "k"
                                       :icon ("inbox" :set "octicon" :color "green")
                                       :extra "")
                                      ("Capture point Task" :keys "c"
                                       :icon ("pencil" :set "octicon" :color "yellow")
                                       :extra "")
                                      ("Task with deadline" :keys "d"
                                       :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                                       :extra "\nDEADLINE: %^{Deadline:}t")
                                      ("Scheduled Task" :keys "s"
                                       :icon ("calendar" :set "octicon" :color "orange")
                                       :extra "\nSCHEDULED: %^{Start time:}t")))
                          ("Project" :keys "p"
                           :icon ("repo" :set "octicon" :color "purple")
                           :prepend t
                           :type entry
                           :headline "Inbox"
                           :template ("* %{time-or-todo} %?"
                                      "%i"
                                      "%a")
                           :file ""
                           :custom (:time-or-todo "")
                           :children (("Project-local todo" :keys "t"
                                       :icon ("checklist" :set "octicon" :color "green")
                                       :time-or-todo "TODO"
                                       :file +org-capture-project-todo-file)
                                      ("Project-local note" :keys "n"
                                       :icon ("sticky-note" :set "faicon" :color "yellow")
                                       :time-or-todo "%U"
                                       :file +org-capture-project-notes-file)
                                      ("Project-local changelog" :keys "c"
                                       :icon ("list" :set "faicon" :color "blue")
                                       :time-or-todo "%U"
                                       :heading "Unreleased"
                                       :file +org-capture-project-changelog-file)))
                          ("Blog" :keys "b"
                           :icon ("pied-piper" :set "faicon" :color "pink")
                           :file +org-capture-blog-file
                           :prepend t
                           :type entry
                           :template ("* %{title} %^G"
                                      ":properties:"
                                      ":export_file_name: %\\1"
                                      ":export_description: %^{Description}"
                                      ":export_date: %^{Date}t"
                                      ":export_lastmod: %t"
                                      ":export_author: %n"
                                      ":end:"
                                      "\n%?")
                           :custom (:title "%^{Title}"))
                          ("\t\tCentralised project templates"
                           :keys "o"
                           :type entry
                           :prepend t
                           :template ("* %{time-or-todo} %?"
                                      "%i"
                                      "%a")
                           :children (("Project todo"
                                       :keys "t"
                                       :prepend nil
                                       :time-or-todo "TODO"
                                       :heading "Tasks"
                                       :file +org-capture-central-project-todo-file)
                                      ("Project note"
                                       :keys "n"
                                       :time-or-todo "%U"
                                       :heading "Notes"
                                       :file +org-capture-central-project-notes-file)
                                      ("Project changelog"
                                       :keys "c"
                                       :time-or-todo "%U"
                                       :heading "Unreleased"
                                       :file +org-capture-central-project-changelog-file)))
                          ))))

          (set-org-capture-templates)
          (unless (display-graphic-p)
            (add-hook 'server-after-make-frame-hook
                      (defun org-capture-reinitialise-hook ()
                        (when (display-graphic-p)
                          (set-org-capture-templates)
                          (remove-hook 'server-after-make-frame-hook
                                       #'org-capture-reinitialise-hook))))))
        ```

        It would also be nice to improve how the capture dialogue looks
        The org-capture bin is rather nice, but I’d be nicer with a smaller frame, and no modeline.

        ```emacs-lisp
        ;; (setf (alist-get 'height +org-capture-frame-parameters) 15)
        ;; ;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
        ;; (setq +org-capture-fn
        ;;       (lambda ()
        ;;         (interactive)
        ;;         (set-window-parameter nil 'mode-line-format 'none)
        ;;         (org-capture)))
        ```


### Web/Javascript/Typescript {#web-javascript-typescript}

TODO

-   Fix `.editorconfig` should override lsp-formatting
-   Must respect `.eslintrc` and `.prettierrc`
-   `.tsx` files are suuuper slow to load - figure out why!

Don't use LSP formatting for these modes. I usually have a .editorconfig file or something..

```emacs-lisp
(use-package! web-mode
  :init
  (setq-hook! 'javascript-mode-hook +format-with-lsp nil)
  (setq-hook! 'typescript-mode-hook +format-with-lsp nil)
  (setq-hook! 'typescript-tsx-mode-hook +format-with-lsp nil)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset   2)
  (web-mode-sql-indent-offset    2)
  (web-mode-css-indent-offset    2)
  (tab-width                     2)
  (evil-shift-width              2))
```


### Python {#python}


### Elixir {#elixir}

Install some extra flycheck packages for Elixir

```emacs-lisp
(package! flycheck-credo)
(package! flycheck-dialyxir)
(package! polymode)
```

```emacs-lisp
(after! (elixir flycheck lsp-ui)
  (flycheck-credo-setup)
  (flycheck-dialyxir-setup)
  (flycheck-add-next-checker 'lsp-ui 'elixir-credo))
```

Enable inline syntax highlighting for `~H` sigils.

```emacs-lisp
(use-package! polymode
  :init (setq web-mode-engines-alist '(("elixir" . "\\.ex\\'")))
  ;; :hook (elixir-mode . poly-elixir-web-mode)         ;; messes up rest of elixir-mode - enable manually for now!
  :config
  (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
  (define-innermode poly-liveview-expr-elixir-innermode
    :mode 'web-mode
    :head-matcher (rx line-start (* space) "~H" (= 3 (char "\"'")) line-end)
    :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
    :head-mode 'host
    :tail-mode 'host
    :allow-nested nil
    :keep-in-mode 'host
    :fallback-mode 'host)
  (define-polymode poly-elixir-web-mode
    :hostmode 'poly-elixir-hostmode
    :innermodes '(poly-liveview-expr-elixir-innermode)))
```


### Dotnet {#dotnet}


#### C-sharp {#c-sharp}

NOTE: After too many hours struggling with `omnisharp-rosly` without managing to make it work,
I settles on just using `csharp-ls`.
Let's revisit `omnisharp-roslyn` at a later point.

The default "latest" release of omnisharp-roslyn does not target .NET 6 SDK.  We override the default
and downloads a newer release that's compatible with .NET 6. See [this](https://github.com/OmniSharp/omnisharp-roslyn/issues/2407#issuecomment-1170354657) Github issue for more details.
Omnisharp-roslyn releases can be found [here](https://github.com/OmniSharp/omnisharp-roslyn/releases).

```emacs-lisp
;; (after! lsp-csharp
;;   (setq lsp-csharp-omnisharp-roslyn-download-url (cond ((eq system-type 'darwin) "https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v1.39.0/omnisharp-osx-x64-net6.0.zip")
;;                                                        ((eq system-type 'gnu/linux) "https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v1.39.0/omnisharp-linux-x64-net6.0.zip")
;;                                                        ((eq system-type 'windown-nt) "https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v1.39.0/omnisharp-win-x64-net6.0.zip")))
;;   (setq lsp-csharp-server-path (f-join lsp-csharp-omnisharp-roslyn-server-dir "OmniSharp"))
;;   )
```

NOTE: `lsp-csharp` appends "-lsp" at the end of "OmniSharp" command. This seems to not work too great with new version (and should be fixed upstreams).

Just changing the `lsp-csharp-omnisharp-roslyn-download-url` didn't work for me, so I manually downloaded the
zip file and extracted it to the `lsp-csharp-omnisharp-roslyn-server-dir` directory.

On MacOS, the "Malicious Software" warning triggered when trying to start the lsp-server, so I had to
whiteliste all the files in the directory manually.

```shell
$ xattr -r -d com.apple.quarantine ~/.emacs.d-doom/.local/etc/lsp/omnisharp-roslyn/latest/omnisharp-roslyn/*
```

Add the `dotnet.el` package and load it with `c-sharp-mode`.

```emacs-lisp
(package! dotnet)
```

```emacs-lisp
(use-package! dotnet
  :hook (csharp-mode . dotnet-mode))
```

<!--list-separator-->

-  Keybindings

    Add the `sharper-main-transient` menu to local leader.

    ```emacs-lisp
    (map! :map csharp-mode-map
          :after csharp
          :localleader
          :desc "Sharper" "s" #'sharper-main-transient)
    ```


### Rust {#rust}

[Here](https://robert.kra.hn/posts/rust-emacs-setup/) is a good guide for setting up Emacs for Rust development.

```emacs-lisp
(after! rustic
  (setq rustic-lsp-server 'rust-analyzer
        rustic-format-on-save t))
```

Rebind `rustic` shortcuts to the `localleader` menu.

`Rustic` is an extension of `rust-mode` which adds a number of useful features. Most `rustic` features are bound to the `C-c C-c`
prefix, which I finds a bit cumbersome. Let's rebind them to the `localleader` so we have them easy accessible.

```emacs-lisp
(map! :map rustic-mode-map
      :after rustic
      :localleader
      (:prefix ("r" . "rustic")
       :desc "recompile"               "TAB"   #'rustic-recompile
       :desc "cargo-add"               "a"     #'rustic-cargo-add
       :desc "cargo-bench"             "b"     #'rustic-cargo-bench
       :desc "cargo-clean"             "c"     #'rustic-cargo-clean
       :desc "cargo-doc"               "d"     #'rustic-cargo-doc
       :desc "cargo-clippy-fix"        "f"     #'rustic-cargo-clippy-fix
       :desc "cargo-init"              "i"     #'rustic-cargo-init
       :desc "cargo-clippy"            "k"     #'rustic-cargo-clippy
       :desc "cargo-new"               "n"     #'rustic-cargo-new
       :desc "cargo-rm"                "r"     #'rustic-cargo-rm
       :desc "cargo-upgrade"           "u"     #'rustic-cargo-upgrade
       :desc "docstring-dwim"          "C-,"   #'rustic-cargo-docstring-dwim
       :desc "cargo-build"             "C-b"   #'rustic-cargo-build
       :desc "cargo-current-test"      "C-c"   #'rustic-cargo-current-test
       :desc "racer-describe"          "C-d"   #'rustic-racer-describe
       :desc "cargo-fmt"               "C-f"   #'rustic-cargo-upgrade
       :desc "cargo-check"             "C-k"   #'rustic-cargo-check
       :desc "cargo-clippy"            "C-l"   #'rustic-cargo-clippy
       :desc "cargo-outdated"          "C-n"   #'rustic-cargo-outdated
       :desc "format-buffer"           "C-o"   #'rustic-format-buffer
       :desc "cargo-run"               "C-r"   #'rustic-cargo-run
       :desc "cargo-test"              "C-t"   #'rustic-cargo-test
       :desc "compile"                 "C-u"   #'rustic-compile)

       ;; lsp-rust-analyzer keybindings on localleader
       (:when (eq rustic-lsp-server 'rust-analyzer)
        :desc "Open Cargo.toml file"    "c"     #'lsp-rust-analyzer-open-cargo-toml
        :desc "Open external docs"      "D"     #'lsp-rust-analyzer-open-external-docs))
```

Advice the `*cargo-run*` buffer to accept user input (why doesn't it already?).

```emacs-lisp
(after! rustic
  (defadvice! rustic-cargo-run-accept-user-input ()
    "Advices the *cargo-run* buffer to run in comint-mode and accept user input"
    :after 'rustic-cargo-run
    (interactive)
    (let ((current-window (selected-window))
          (cargo-window (display-buffer (get-buffer "*cargo-run*") nil 'visible)))
      (select-window cargo-window)
      (comint-mode)
      (read-only-mode 0)
      (select-window current-window))))
```


#### LSP {#lsp}

Configures `lsp-mode` hints for Rust (using `rust-analyzer`)
(From [rust-emacs-setup](https://robert.kra.hn/posts/rust-emacs-setup/) guide)

```emacs-lisp
(after! (rustic lsp-mode)
  (when (eq rustic-lsp-server 'rust-analyzer)
    (setq lsp-eldoc-render-all                                                  t
          lsp-rust-analyzer-cargo-watch-command                                 "clippy"
          lsp-rust-analyzer-display-chaining-hints                              t
          lsp-rust-analyzer-display-closure-return-type-hints                   t
          lsp-rust-analyzer-display-lifetime-elision-hints-enable               "skip_trivial"
          lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names  nil
          lsp-rust-analyzer-display-parameter-hints                             nil
          lsp-rust-analyzer-display-reborrow-hints                              nil
          lsp-rust-analyzer-server-display-inlay-hints                          t)))
```

Configures `lsp-ui` for Rust

```emacs-lisp
(after! (rustic lsp-ui-mode)
  (setq lsp-ui-peek-always-show         t
        lsp-ui-sideline-show-hover      t
        lsp-ui-doc-enable               nil))
```


#### Debugging {#debugging}

Configures `dap-mode` for Rust.

We need to do some additional work for setting up debugging.
(From [rust-emacs-setup](https://robert.kra.hn/posts/rust-emacs-setup/) guide)

1.  Install `llvm` and `cmake` via homebrew
2.  Checkout the [lldb-mi](https://github.com/lldb-tools/lldb-mi) repo
3.  Build the `lldb-mi` binary
4.  Link to a location in `$PATH`

<!--listend-->

```shell
$ brew install cmake llvm
$ export LLVM_DIR=/usr/local/Cellar/llvm/14.0.6/lib/cmake
$ git clone https://github.com/lldb-tools/lldb-mi ~/.local/src/lldb-mi
$ mkdir -p ~/.local/src/lldb-mi/build
$ cd ~/.local/src/lldb-mi/build
$ cmake ..
$ cmake --build .
$ ln -s $PWD/src/lldb-mi ~/.local/bin/lldb-mi
```

TODO - Should set LLVM_DIR a better way?

```emacs-lisp
(after! (rustic dap-mode)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)
  (dap-register-debug-template "Rust::LLDB Run Configuration"
                               (list :type      "lldb"
                                     :request   "launch"
                                     :name      "LLDB::Run"
                                     :gdbpath   "rust-lldb"
                                     :target     nil
                                     :cwd       nil)))
```

> (dap-gdb-lldb-setup) will install a VS Code extension into user-emacs-dir/.extension/vscode/webfreak.debug.
> One problem I observed was that this installation is not always successful. Should you end up without
> a “webfreak.debug” directory you might need to delete the vscode/ folder and
> run (dap-gdb-lldb-setup) again.

EDIT - It turns out my "webfreak.debug" directory is empty - Figure this out at some time!

Finally run `sudo DevToolsSecurity --enable` to allow the debugger access to processes.

```shell
$ sudo DevToolsSecurity --enable
Enter PIN for 'Certificate For PIV Authentication (Yubico PIV Authentication)':
Developer mode is now enabled.
```

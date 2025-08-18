+++
title = "Emacs Configuration"
author = ["Rolf Håvard Blindheim"]
lastmod = 2025-08-18T08:29:00+02:00
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

Since everybody seems to be writing literate Emacs configs these days, I wanted to give it a spin as well.
I'm not really very proficient on `elisp`, so this config is mostly a collection of code I've stolen from others.

Since this document literally is my Emacs configuration, at times it may contain code that is a
work in progress, silly comments, `TODO` and `FIXMEs` and so on.


## References {#references}

As many others I've been using [tecosaurs Emacs config](https://tecosaur.github.io/emacs-config/config.html) as my base configuration. It's one of the most
elaborated literate configs I've seen, with a lot of nice little tweaks. My configuration broadly follows his,
but it's slowly morphing into my own :)

So a big thanks to `tecosaur` for making my onboarding to Doom Emacs a smooth experience!


### Great Emacs configs {#great-emacs-configs}

-   [tecosaurs Emacs config](https://tecosaur.github.io/emacs-config/config.html)
-   [nehrbashs Emacs config](https://github.com/nehrbash/dotfiles/blob/main/Emacs.org)


## Installing Emacs {#installing-emacs}

Emacs can be installed in many ways. I'm usually on a Mac, so I just use [Homebrew](https://brew.sh/).
There's a couple of Emacs packages in Homebrew, but the most popular seems to be [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus) and [emacs-mac](https://github.com/railwaycat/homebrew-emacsmacport).
I have been using `emacs-mac` for some time now, but was previously running `emacs-plus`.


### Fix annoying max open files for Emacs {#fix-annoying-max-open-files-for-emacs}

When using LSP servers with Emacs, it's easy to hit the "max open files" error. Emacs doesn't seem to use
operating system `ulimit`, but uses `pselect` which is limited to FD_SETSIZE file descriptors, usually 1024.
This means that changing `ulimit` will not change the value of FD_SETSIZE compiled into Emacs and macOS
libraries. To overcome this limitation we've to set the FD_SETSIZE CFLAG when compiling Emacs.

```shell
CFLAGS="-DFD_SETSIZE=10000 -DDARWIN_UNLIMITED_SELECT"
```

See [this](https://en.liujiacai.net/2022/09/03/emacs-maxopenfiles/) blog post for a more detailed description.


### Emacs-mac {#emacs-mac}

Installation instructions are in the [README](https://github.com/railwaycat/homebrew-emacsmacport/blob/master/README.md) file in the repository, but the TLDR; is something like this.

```shell
$ brew tap railwaycat/emacsmacport
$ CFLAGS="-DFD_SETSIZE=10000 -DDARWIN_UNLIMITED_SELECT" brew install emacs-mac --with-unlimited-select --with-tree-sitter --with-natural-title-bar --with-native-comp --with-xwidgets --with-librsvg --with-imagemagick --with-emacs-big-sur-icon
```

See the [emacs-mac.rb](https://github.com/railwaycat/homebrew-emacsmacport/blob/master/Formula/emacs-mac.rb) formula file for a list of supported compilation flags.


### Emacs-plus {#emacs-plus}

The [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus) is another Homebrew formula for installing Emacs on macOS.

```shell
$ CFLAGS="-DFD_SETSIZE=10000 -DDARWIN_UNLIMITED_SELECT" brew install emacs-plus@30 --with-xwidgets --with-imagemagick --with-native-comp --with-nobu417-big-sur-icon
```

To add Emacs to `/Applications` enter the following commands in a terminal (it's also printed to the screen after installing emacs-plus).

```shell
$ osascript -e 'tell application "Finder" to make alias file to posix file "/usr/local/opt/emacs-plus@30/Emacs.app" at POSIX file "/Applications" with properties {name:"Emacs.app"}'
$ sudo codesign --force --deep --sign - /usr/local/opt/emacs-plus@30/Emacs.app
```

Unless I signed the binaries, Finder refuses to open them. See [this Github issue](https://github.com/d12frosted/homebrew-emacs-plus/issues/742#issuecomment-2449092291).


## Getting started {#getting-started}

Describe steps required to do in order to get this up and running on a new machine.

1.  Install nerd-fonts
    Doom modeline required nerd-fonts now. run `M-x nerd-icons-install-fonts`.


## Configuration {#configuration}

Remember to run `doom sync` after editing this file.
See the [Common config anti-patterns](https://discourse.doomemacs.org/t/common-config-anti-patterns/119) thread to avoid common pitfalls.

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

```emacs-lisp
(defconst user-home-directory (expand-file-name (file-name-as-directory "~"))
  "A constant that holds the current users home directory.")
```


### Better defaults {#better-defaults}

General settings that makes life a bit easier.

```emacs-lisp
(setq auto-save-default                   t         ; I don't want to lose work.
      delete-by-moving-to-trash           t         ; Delete files to trash.
      confirm-kill-emacs                  nil       ; Don't ask if I really want to quit!
      diff-hl-flydiff-delay               2         ; Controls how often `diff-hl' updates in seconds.
      display-time-24hr-format            t         ; I dont know the difference between AM and PM.
      evil-want-fine-undo                 t         ; More granular undos in evil insert mode.
      evil-ex-substitute-global           t         ; More often than not, I want /s on ex commands.
      evil-kill-on-visual-paste           nil       ; Don't add overwritten text in visual mode to the kill ring.
      fill-column                         120       ; We have lot's of screen estate.
      history-length                      1000      ; Remember more history.
      warning-minimum-level               :error    ; The minimum severity level before displaying the warning buffer.
      mouse-wheel-tilt-scroll             t         ; Scroll horizontally using the mouse.
      mouse-wheel-flip-direction          t         ; Scrolling for oldies.
      pixel-scroll-precision-mode         t         ; Enable pixel scroll precision mode (requires Emacs 29).
      scroll-margin                       10        ; Keep a little scroll margin.
      sh-shell                            "sh"      ; The shell to use when spawning external commands.
      undo-limit                          16000000  ; Increase undo limit to 16Mb.
      vc-follow-symlinks                  nil       ; Don't follow symlinks, edit them directly.
      which-key-idle-delay                0.2       ; Makes which-key feels more responsive.
      window-combination-resize           t         ; Take new window space from all other windows (not just the current).
      x-stretch-cursor                    t         ; Stretch cursor to the glyph width.
      )
```

Here are some modes I always want active.

```emacs-lisp
(diff-hl-flydiff-mode           1)        ; This makes `diff-hl' updated asynchronous
(display-time-mode              1)        ; I want to know what time it is
(drag-stuff-global-mode         1)        ; Drag text around
(global-goto-address-mode       1)        ; A minor mode to render urls and like as links
(global-subword-mode            1)        ; Iterate through CamelCase words - Not sure how I like this
(smartparens-global-mode        1)        ; Always enable smartparens
(ws-butler-global-mode          1)        ; Unobtrusive way to trim spaces on end of lines

;; Enable visual-line-mode in certain modes (so I don't have to scroll horizontally).
(dolist (hook '(text-mode-hook
                comint-mode-hook))
  (add-hook hook #'visual-line-mode))
```

I like to have the local leader key bound to `,`.

```emacs-lisp
(setq doom-localleader-key      ","
      doom-localleader-alt-key  "M-,")
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
    Since this seems to be happening quite frequently, I'll just put it on a timer for now.

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
        ;; epg-pinentry-mode        'loopback                      ; Use pinentry to ask for passphrase.
        epg-gpg-program (cond ((eq system-type 'darwin)     "/opt/homebrew/bin/gpg")
                              ((eq system-type 'gnu/linux)  "/usr/bin/gpg")
                              ((eq system-type 'windows-nt) "C:/Program Files (x86)/GNU/GnuPG/gpg2")))

  (pinentry-start)
  ;; (load-file (expand-file-name "secrets.el.gpg" doom-private-dir)) ; This cause weird errors
  )
```

I'm using `gpg-agent` instead of `ssh-agent` so we need to connect here.

```emacs-lisp
(shell-command "gpg-connect-agent updatestartuptty /bye >/dev/null")
```


### Keybindings {#keybindings}

Even though Doom Emacs makes a lot of stuff easy, I always needs to [look up](https://github.com/hlissner/doom-emacs/blob/master/modules/config/default/+evil-bindings.el) how to bind keymaps.


#### Window navigation {#window-navigation}

Select windows using `M-[1..9]`.

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

Close temporary windows in the current frame.

```emacs-lisp
(defun cust/close-temporary-window ()
  "Close all temporary windows in the current frame.
Returns t if any windows were closed."
  (interactive)
  (save-selected-window
    (let (result)
      (dolist (window (window-list))
        (select-window window)
        (cond ((or (eq major-mode #'help-mode)
                   (eq major-mode #'compilation-mode)
                   (eq major-mode #'completion-list-mode)
                   (eq major-mode #'grep-mode)
                   (eq major-mode #'apropos-mode))
               (quit-window)
               (setf result t))
              ((eq major-mode #'magit-popup-mode)
               (magit-popup-quit)
               (setf result t))))
      result)))

;; Bind the function to ESC
(let ((key
       (if window-system (kbd "<escape>") "\M-q")))
  (global-set-key key #'cust/close-temporary-window))
```

When creating a new window, try to use the same window if possible or create a new below selected window.

```emacs-lisp
(setopt display-buffer-base-action '((display-buffer-reuse-window display-buffer-below-selected)))
```


#### Workspace navigation {#workspace-navigation}

Some of the default Doom workspace navigation keybindings seems a bit counter intuitive for
my workflow, so I rebind some of them.

```emacs-lisp
(map! :leader
      (:when (modulep! :ui workspaces)
       (:prefix-map ("l" . "workspace")         ; Rebind workspaces to SCP-l
        :desc "Delete this workspace"           "d"   #'+workspace/kill
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

I like cycling between the two last buffers using `SPC-Tab`.

```emacs-lisp
(map! :leader :desc "Cycle last buffer" "TAB" #'evil-switch-to-windows-last-buffer)     ; Use SPC-Tab to cycle between two last buffers
```

Move around buffers using `SPC-b [1..9]`.
The following functions are ported directly from `Spacemacs` and relies only at `winum`.

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
  ;; - buffer-to-window-1 to 9
  ;; - move-buffer-to-window-no-follow-1 to 9
  ;; - swap-buffer-to-window-no-follow-1 to 9
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

Map the newly defined function to `SPC-b [1..9]` for easy access.

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


#### Buffer manipulation {#buffer-manipulation}

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

Replace buffer with content from clipboard with `SPC-b P`.

```emacs-lisp
(defun replace-buffer-from-clipboard ()
  "Replace the buffer with content from clipboard."
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(map! :leader
      (:prefix-map ("b" . "buffer")
       :desc "Paste clipboard to buffer" "P" #'replace-buffer-from-clipboard))
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

List all processes running under Emacs.

```emacs-lisp
(map! :leader :desc "List processes" "P" #'list-processes)
```

**When on macOS, remember to disable the Mission Control shortcut keys as they override the inputs!**
Always use `C-<left>` and `C-<right>` to move one word left or right.

```emacs-lisp
(global-set-key (kbd "<C-right>") #'forward-word)
(global-set-key (kbd "<C-left>")  #'backward-word)
```

When in `evil-mode`, use `C-n` and `C-p` to move to next or previous functions, respectively.

```emacs-lisp
(defun cust/prog-mode-defun-navigation ()
  "Use Doom's defun navigation for C-n/C-p, and C-S-n for visual end-of-defun."
  (evil-define-key '(normal visual) (current-local-map)
    (kbd "C-n") #'+evil/next-beginning-of-method
    (kbd "C-p") #'+evil/previous-beginning-of-method
    (kbd "C-S-n") #'cust/visual-next-end-of-method
    (kbd "C-S-p") #'cust/visual-previous-beginning-of-method))

(add-hook 'evil-local-mode-hook #'cust/prog-mode-defun-navigation)
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

Add some environmental variables to the `doom-env-allow` list.
This is required since I like to use `gpg-agent` over `ssh-agent` for key management, authentication and signing operations.

```emacs-lisp
(when noninteractive
  (defvar doom-env-allow '())
  (dolist (var '("^LANG$" "^LC_TYPE$" "^GPG_AGENT_INFO$" "^SSH_AGENT_PID$" "^SSH_AUTH_SOCK$"))
    (add-to-list 'doom-env-allow var)))
```

To generate the Emacs environment file, simply run `doom env` from the terminal.


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

-  Unpinned core packages

    In some cases we want to install the latest and greatest version of a package. Doom allows us to unpin packages
    using the `unpin` macro.

    ```emacs-lisp
    (unpin! (:tools lsp tree-sitter))
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
    ;;(company          ; the ultimate code completion backend
    ;; +childframe)     ; ...when your children are better than you
    (corfu              ; complete with cap(f), cape and a flying feather!
     +icons
     +dabbrev
     +orderless)
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
    indent-guides       ; highlighted indent columns
    (ligatures +extra)  ; ligatures and symbols to make your code pretty again
    ;;minimap           ; show a map of the code on the side
    modeline            ; snazzy, Atom-inspired modeline, plus API
    nav-flash           ; blink cursor line after big motions
    ;;neotree           ; a project drawer, like NERDTree for vim
    ophints             ; highlight the region an operation acts on
    (popup
     +all +defaults)    ; tame sudden yet inevitable temporary windows
    ;;smooth-scroll     ; smooth scrolling
    ;;tabs              ; a tab bar for Emacs
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
    (dired              ; making dired pretty [functional]
     +icons)
    electric            ; smarter, keyword-based electric-indent
    (ibuffer +icons)    ; interactive buffer management
    (undo)              ; persistent, smarter undo for your inevitable mistakes
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
    ;; (:if                ; tasing you for misspelling mispelling
    ;;  (executable-find "aspell") spell +aspell +flyspell)
    (spell +aspell +flyspell)
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
    llm                 ; language model support
    (lookup
     +dictionary
     +docsets)          ; navigate your code and its documentation
    (lsp +peek)         ; M-x vscode
    (magit
     +forge)            ; a git porcelain for Emacs
    make                ; run make tasks from Emacs
    ;;pass              ; password manager for nerds
    pdf                 ; pdf enhancements
    ;;prodigy           ; FIXME managing external services & code builders
    ;;rgb               ; creating color strings
    ;;taskrunner        ; taskrunner for all your projects
    ;;terraform         ; infrastructure as code
    ;;tmux              ; an API for interacting with tmux
    tree-sitter         ; syntax and parsing, sitting in a tree...
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
     +tree-sitter
     +lsp)              ; unity, .NET, and mono shenanigans
    data                ; config/data formats
    ;;(dart +flutter)   ; paint ui and not much else
    ;;dhall
    (elixir
     +tree-sitter
     +lsp)              ; erlang done right
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
    (go
     +tree-sitter
     +lsp)              ; the hipster dialect
    ;;(haskell +lsp)    ; a language that's lazier than I am
    ;;hy                ; readability of scheme w/ speed of python
    ;;idris             ; a language you can depend on
    json                ; At least it ain't XML
    ;;(java +lsp)       ; the poster child for carpal tunnel syndrome
    (javascript
     +tree-sitter
     +lsp)              ; all(hope(abandon(ye(who(enter(here))))))
    ;;julia             ; a better, faster MATLAB
    ;;kotlin            ; a better, slicker Java(Script)
    ;;latex             ; writing papers in Emacs has never been so fun
    ;;lean              ; for folks with too much to prove
    ;;ledger            ; be audit you can be
    lua                 ; one-based indices? one-based indices
    markdown            ; writing docs for people to ignore
    ;;nim               ; python + lisp at the speed of c
    ;;nix               ; I hereby declare "nix geht mehr!"
    ;;ocaml             ; an objective camel
    (org                ;organize your plain life in plain text
     +pretty            ; yessss my pretties! (nice unicode symbols)
     +dragndrop         ; drag & drop files/images into org buffers
     +hugo              ; use Emacs for hugo blogging
     +noter             ; enhanced PDF notetaking
     ;; +jupyter           ; ipython/jupyter support for babel
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
     +tree-sitter
     +cython
     +poetry
     +pyright)
    ;;qt                ; the 'cutest' gui framework ever
    ;;racket            ; a DSL for DSLs
    ;;raku              ; the artist formerly known as perl6
    ;;rest              ; Emacs as a REST client
    ;;rst               ; ReST in peace
    ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
    (rust
     +lsp)              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
    ;;scala             ; java, but good
    ;;(scheme +guile)   ; a fully conniving family of lisps
    (sh
     +powershell
     +tree-sitter
     +lsp)              ; she sells {ba,z,fi}sh shells on the C xor
    ;;sml
    ;;solidity          ; do you need a blockchain? No.
    ;;swift             ; who asked for emoji variables?
    ;;terra             ; Earth and Moon in alignment for performance.
    (web
     +tree-sitter
     +lsp)              ; the tubes
    (yaml               ; JSON, but readable
     +lsp)
    ;;zig               ; C, but simpler
    ```

<!--list-separator-->

-  Everything in Emacs

    <a id="code-snippet--doom-email"></a>
    ```emacs-lisp
    ;; (mu4e +org +gmail)
    ;; notmuch
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

Replace the "modified" buffer color in the modeline, so it doesn't look like something's wrong every time we edit files.

```emacs-lisp
(custom-set-faces! `(doom-modeline-buffer-modified :foreground "Orange" :italic t))
```


#### Frame {#frame}

Configure default size for new Emacs frames. I'm usually at 1440p display size,
so this seems reasonable.

```emacs-lisp
(add-to-list 'default-frame-alist '(height . 72))
(add-to-list 'default-frame-alist '(width . 240))
```

To make Emacs look a bit more modern (at least on macOS) we can enable a "natural title bar", which makes
the color of the title bar match the color of the buffer.
For this to work, Emacs must be compiled using the `--with-natural-title-bar` flag.

Configuring transparent titlebar is supposed to be working by the following code,
however I'm unable to make it work by only configuring Emacs.

```emacs-lisp
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))  ;; or dark
  (setq ns-use-proxy-icon nil
        frame-title-format nil))
```

So by setting some `defaults` in macOS, we achieve the desired result.

```shell
$ defaults write org.gnu.Emacs HideDocumentIcon YES
$ defaults write org.gnu.Emacs TransparentTitleBar LIGHT  # or DARK; doesn't really matter when used with "frame-title-format nil"
```


#### Theme and modeline {#theme-and-modeline}

I currently use two themes - a light theme for usual work, and a dark theme for late night hacking sessions.
These days I'm using the `doom-tomorrow-day` light theme, and `doom-nord-aurora` dark theme. To easily cycle
between them, I keep my favorite themes in a  `doom-cycle-themes` list, and have a small function that just applies
the next theme in the list.

```emacs-lisp
(setq dark-mode-theme 'doom-nord-aurora
      light-mode-theme 'doom-tomorrow-day)

(setq doom-cycle-themes (list light-mode-theme dark-mode-theme))
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
      doom-modeline-env-version                 nil ;; too noisy
      doom-modeline-major-mode-icon             t
      doom-modeline-major-mode-color-icon       t
      doom-modeline-buffer-state-icon           t)
```

Also, in Emacs 29.1 we got support for transparent backgrounds. This can be enabled by the following
command if desired.

```emacs-lisp
(set-frame-parameter nil 'alpha-background 70)
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
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 18)
      doom-unicode-font (font-spec :family "IBM Plex Mono")
      doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))
```

"In addition to these fonts, Merriweather is used with nov.el, and Alegreya as a serifed proportional font used by mixed-pitch-mode for writeroom-mode with Org files.
Because we care about how things look let’s add a check to make sure we’re told if the system doesn’t have any of those fonts."

<a id="code-snippet--detect-missing-fonts"></a>
```emacs-lisp
(defvar required-fonts '("JetBrainsMono.*" "Overpass" "JuliaMono" "IBM Plex Mono" "Merriweather" "Alegreya" "Iosevka Aile"))
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
```


#### Mixed pitch {#mixed-pitch}

> From the `:ui zen` module.

We’d like to use mixed pitch in certain modes. If we simply add a hook, when directly opening a file with (a new) Emacs `mixed-pitch-mode runs`
before UI initialisation, which is problematic. To resolve this, we create a hook that runs after UI initialization and both:

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


#### Transparency {#transparency}

Emacs 29 got support for background transparency which looks pretty nice.
Set up a `toggle-transparency` function and map it to `M-x t B`.

```emacs-lisp
(set-frame-parameter nil 'alpha-background 100)               ;; Current frame
(add-to-list 'default-frame-alist '(alpha-background . 100))  ;; All new frames from now on

(defun cust/toggle-window-transparency ()
  "Toggle window transparency"
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          98 100))))

(map! :leader
      (:prefix-map ("t" . "toggle")
                   :desc "Background transparency" "B" #'cust/toggle-window-transparency))
```


#### Windows {#windows}

I find it rather handy to be asked which buffer I want to see after splitting
the window.

First, split the window...

```emacs-lisp
(setq evil-vsplit-window-right t
      evil-split-window-below t)
```

...then, pull up a buffer prompt.

```emacs-lisp
(defadvice! prompt-for-buffer (&rest _) :after '(evil-window-split evil-window-vsplit) (consult-buffer))
```

Next up, we want to keep a balanced layout of split windows based on the [golden ratio](https://en.wikipedia.org/wiki/Golden_ratio).
[Zoom](https://github.com/cyrus-and/zoom) does a pretty good job.
NOTE: I have issues with this package. It screws up the `which-key` buffer and some others.

```emacs-lisp
;; (package! zoom)
```

```emacs-lisp
;; (use-package zoom
;;   :hook (doom-first-input-hook . zoom-mode)
;;   :config
;;   (setq zoom-size '(0.618 . 0618)
;;         zoom-ignored-major-modes '(dired-mode vterm-mode help-mode helpful-mode help-mode-menu rxt-help-mode)
;;         zoom-ignored-buffer-names '("*doom:scratch*" "*info*" "*helpful variable: argv*" "*which-key*")
;;         zoom-ignored-buffer-name-regexps '("^\\*calc" "\\*helpful variable: .*\\*" "^\\*which-key\\*")
;;         zoom-ignore-predicates (list (lambda () (< (count-lines (point-min) (point-max)) 20))
;;                                      (lambda () (window-parameter (selected-window) 'window-side)))
;;         ))
```


#### Buffers {#buffers}

<!--list-separator-->

-  Text scaling

    The `default-text-scale` package works by adjusting the size of the `default` face,
    so that it affects all buffers at once.

    ```emacs-lisp
    (package! default-text-scale)
    ```

    Enable `default-text-scale-mode` global minor mode and add some extra keybindings for macOS. I think
    it's easier to just use `CMD-+/-/0` to increase, decrease and reset.

    ```emacs-lisp
    (default-text-scale-mode 1)
    (map! :map default-text-scale-mode-map
          (:when (eq system-type 'darwin)
            "s-+"     #'default-text-scale-increase
            "s--"     #'default-text-scale-decrease
            "s-0"     #'default-text-scale-reset)
          (:when (and (modulep! :ui workspaces) (eq system-type 'darwin)
                      (define-key evil-normal-state-map (kbd "s-0") nil))))
    ```

<!--list-separator-->

-  Buffer manipulation

    These little guys are helpful to remove duplicates in a region or buffer.

    ```emacs-lisp
    (defun uniquify-region-lines (beg end)
      "Remove duplicate adjacent lines in region."
      (interactive "*r")
      (save-excursion
        (goto-char beg)
        (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
          (replace-match "\\1"))))

    (defun uniquify-buffer-lines ()
      "Remove duplicate adjacent lines in the current buffer."
      (interactive)
      (uniquify-region-lines (point-min) (point-max)))
    ```


### Other things {#other-things}


#### Splash screen {#splash-screen}

This beauty is also shamelessly ripped off from [tecosaur's](https://tecosaur.github.io/emacs-config/config.html#splash-screen) Emacs config!

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


#### Utility functions {#utility-functions}

Here are just a collection of utility functions I use.

<!--list-separator-->

-  add-list-to-list

    Sometimes it's handy to merge a list into another.

    ```emacs-lisp
    (defun add-list-to-list (list-var elements &optional append compare-fn)
      "Add ELEMENTS to the value of LIST-VAR in order
    Behaves like `add-to-list', but accepts a list of new ELEMENTS to add."
      (interactive)
      (setq elements  (if append elements (reverse elements)))
      (let* ((val  (symbol-value list-var))
             (lst  (if append (reverse val) val)))
        (dolist (elt elements)
          (cl-pushnew elt lst :test compare-fn))
        (set list-var (if append (nreverse lst) lst)))
      (symbol-value list-var))
    ```

<!--list-separator-->

-  align-whitespace

    This little gem can align a block of text by whitespace columns, which makes it easy to align text so it
    looks a bit prettier.

    ```emacs-lisp
    (defun align-whitespace (start end)
      "Align columns by whitespace"
      (interactive "r")
      (align-regexp start end
                    "\\(\\s-*\\)\\s-" 1 0 t))
    ```

<!--list-separator-->

-  reload-buffer

    By default, `auto-revert-mode` will not reload a file if you have unsaved changes in the buffer. To override this (for example discard unsaved changes
    and reload the file from disk), we can use `revert-buffer` interactively.

    ```emacs-lisp
    (defun reload-buffer-no-confirm ()
      "Revert buffer without confirmation."
      (interactive)
      (revert-buffer :ignore-auto :noconfirm))

    ;; Bind the above function to a key, e.g., F5
    (global-set-key (kbd "<f5>") #'reload-buffer-no-confirm)
    ```

<!--list-separator-->

-  sort-words

    alphabetically handy It's sometimes sort to words.

    ```emacs-lisp
    (defun sort-words (reverse start end)
      "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.
    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.
    See `sort-regexp-fields'.
    https://www.emacswiki.org/emacs/SortWords"
      (interactive "*P\nr")
      (sort-regexp-fields reverse "\\w+" "\\&" start end))
    ```

<!--list-separator-->

-  sort-symbols

    ```emacs-lisp
    (as be deserved sorted symbols to well)
    ```

    ```emacs-lisp
    (defun sort-symbols (reverse start end)
      "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
      (interactive "*P\nr")
      (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" start end))
    ```

<!--list-separator-->

-  delete-carrage-returns

    Some files (looking at you, `dotnet` generated projects) create files using DOS-style end of line characters (`CRLF`).
    I always want to use unix-style end of line characters (`LF`).

    ```emacs-lisp
    (defun delete-carrage-returns ()
      "Deletes all carrage-returns characters in buffer"
      (interactive)
      (save-excursion
        (goto-char 0)
        (while (search-forward "\r" nil :noerror)
          (replace-match ""))))

    (defun delete-trailing-crlf ()
      "Remove trailing crlf (^M) end-of-line in the current buffer"
      (interactive)
      (save-match-data
        (save-excursion
          (let ((remove-count 0))
            (goto-char (point-min))
            (while (re-search-forward (concat (char-to-string 13) "$") (point-max) t)
              (setq remove-count (+ remove-count 1))
              (replace-match "" nil nil))
            (message (format "%d ^M removed from buffer." remove-count))))))

    (map! :leader
          (:prefix-map ("c" . "code")
                       :desc "Delete trailing crlf" "W" #'delete-trailing-crlf))
    ```


## Packages {#packages}

This is where to install packages.
By declaring packages using the `package!` macro in `packages.el`, then running `doom refresh` from the command line
(or `M-x doom/refresh`).

The `packages.el` file should **not** be byte compiled!

```emacs-lisp
;; -*- no-byte-compile: t; -*-
```


### Loading instructions {#loading-instructions}


#### Package Archives {#package-archives}

```emacs-lisp
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
```


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

In order to disable a built-in package (for whatever reason), we can use the `:disable` property.

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


#### AI {#ai}

> Large Language Models are the next best thing since...

<!--list-separator-->

-  Claude Code

    ```emacs-lisp
    (package! claude-code-ide
      :recipe (:host github :repo "manzaltu/claude-code-ide.el"))
    ```

    ```emacs-lisp
    (use-package! claude-code-ide
      :config
      (claude-code-ide-emacs-tools-setup)
      (setq claude-code-ide-terminal-backend 'vterm
            claude-code-ide-vterm-render-delay 0.001
            claude-code-ide-enable-mcp-server t)
      (map! :leader
            :desc "+Claude Code" "C" #'claude-code-ide-menu))
    ```

<!--list-separator-->

-  Github Copilot

    Unofficial GitHub Copilot plugin for Emacs.

    **Copilot code plugin**

    ```emacs-lisp
    (package! copilot
      :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
    ```

    ```emacs-lisp
    ;; accept completion from copilot and fallback to company
    (use-package! copilot
      :hook ((prog-mode . copilot-mode)
             (org-mode . copilot-mode))
      :bind (:map copilot-completion-map
                  ("<tab>" . 'copilot-accept-completion)
                  ("TAB" . 'copilot-accept-completion)
                  ("C-TAB" . 'copilot-accept-completion-by-word)
                  ("C-<tab>" . 'copilot-accept-completion-by-word))
      :config
      ;; Does this work?
      ;; https://github.com/copilot-emacs/copilot.el/issues/382#issuecomment-2823816333
      (setq lsp-copilot-enabled t
            copilot-lsp-settings '(:github (:copilot (:selectedCompletionModel "claude-3.7-sonnet"))))
      (add-to-list 'copilot-indentation-alist '(elixir-mode elixir-smie-indent-basic))
      (add-to-list 'copilot-indentation-alist '(elixir-ts-mode elixir-ts-indent-offset)))
    ```

    Required to run `M-x copilot-login` for using the plugin.

<!--list-separator-->

-  Aider

    Aider is a well-known and highly effective AI programming tool for the terminal.
    It can give us AI-features similar to those found in the [Cursor AI Code editor](https://www.cursor.com/), but in Emacs.

    ```emacs-lisp
    (unpin! transient)  ;; Aider requires a newer version of transient than Doom provides
    (package! transient :recipe (:host github :repo "magit/transient"))
    (package! aider :recipe (:host github :repo "tninja/aider.el" ))
    ```

    ```emacs-lisp
    (use-package aider
      :config
      (require 'aider-doom)
      (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
      (aider-magit-setup-transients)
      )
    ```

<!--list-separator-->

-  MCP Hub

    -   The MCP Servers [Github](https://github.com/modelcontextprotocol/servers/tree/main) repository.
    -   Check out a list of [Awesome MCP Servers.](https://github.com/punkpeye/awesome-mcp-servers)
    -   [A Quick Start Guide](https://github.com/lizqwerscott/mcp.el/wiki/quickstart)

    <!--listend-->

    ```emacs-lisp
    (package! mcp
      :recipe (:host github :repo "lizqwerscott/mcp.el" :files ("*.el")))
    ```

    ```emacs-lisp
    (use-package mcp
      :ensure t
      :after gptel
      :custom (mcp-hub-servers
               `(
                 ;; ("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "~/.local/src/mcp-servers")))
                 ;; ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
                 ))
      :config (require 'mcp-hub)
      :hook (after-init . mcp-hub-start-all-server))
    ```

<!--list-separator-->

-  MCP Server

    ```emacs-lisp
    (package! mcp-server :pin "adb93cf"
      :recipe (:host github :repo "rhblind/emacs-mcp-server"
               :files ("*.el" "mcp-wrapper.py" "mcp-wrapper.sh")))
    ```

    ```emacs-lisp
    (add-hook 'emacs-startup-hook #'mcp-server-start-unix)
    ```

    To hook up Claude Code to the MCP server, I use this command.

    ```shell
    $ claude mcp add emacs --scope user \
        ~/.config/emacs/.local/straight/build-30.1/mcp-server/mcp-wrapper.py \
        ~/.config/emacs/.local/cache/emacs-mcp-server.sock
    ```

<!--list-separator-->

-  GPTEL

    ```emacs-lisp
    (require 'gptel-integrations)

    ;; Configure API key retrieval from auth-source
    ;; Add to ~/.authinfo or ~/.authinfo.gpg:
    ;; machine api.anthropic.com login apikey password YOUR_API_KEY

    ;; Set up Anthropic Claude backend
    (setq ;; gptel-backend (gptel-make-anthropic "Claude" :stream t :key gptel-api-key)
          gptel-backend (gptel-make-gh-copilot "Copilot")
          gptel-model "claude-4-0-sonnet-20250805")
    ```


#### Auto themer {#auto-themer}

Nice way to create custom themes

```emacs-lisp
(package! autothemer)
```

```emacs-lisp
(use-package! autothemer)
```


#### Consult {#consult}

> From the `:completion vertico` module

Since we're using [Marginalia](#marginalia), the separation between buffers and files is already clear,
and there's no need to use a different face.

```emacs-lisp
(after! consult
  (set-face-attribute 'consult-file nil :inherit 'consult-buffer)
  (setf (plist-get (alist-get 'perl consult-async-split-styles-alist) :initial) ";")

  ;; Jump to an outline
  (map! :leader
        (:prefix-map ("s" . "search")
         :desc "Jump to outline" "J" #'consult-outline)))
```


#### Corfu {#corfu}

> From the `:completion corfu` module

```emacs-lisp
(after! corfu
  (map! :map corfu-map
        "SPC" #'corfu-insert-separator))  ;; Enables multiple orderless patterns
```


#### DAP {#dap}

> From the `:tools debugger` module

Debugging using `dap-mode` and `lsp` brings us breakpoints, a REPL, local variables view for current stack frames and more
to Emacs.

I have not used this too much, so it's currently at an experimental stage for my part.
[Here's](https://youtu.be/0bilcQVSlbM) a link to a Systemcrafters Youtube video tutorial on `dap-mode`.

```emacs-lisp
;; (after! dap-mode

;;   ;; There's a bug which cause the breakpoint fringe to disappear while
;;   ;; the debug session is running. This little hook seems to fix it.
;;   ;; https://github.com/emacs-lsp/dap-mode/issues/374#issuecomment-1140399819
;;   (add-hook! +dap-running-session-mode
;;     (set-window-buffer nil (current-buffer)))

;;   ;; Doesn't really seem to be working, gotta reopen the buffer...
;;   (add-hook! dap-terminated-hook (set-window-buffer nil (current-buffer)))

;;   ;; Windows to be shown when debugging
;;   (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip)))
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
  (require 'evil-collection)
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
opening a video file in Emacs is probably not going to be a pleasant experience, and would be better to open
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

```emacs-lisp
(package! drag-stuff)
```

I like to drag stuff up and down using `C-<up>` and `C-<down>`.

```emacs-lisp
(after! drag-stuff
  (global-set-key (kbd "<C-up>") #'drag-stuff-up)
  (global-set-key (kbd "<C-down>") #'drag-stuff-down))
```


#### Eat {#eat}

[Eat](https://codeberg.org/akib/emacs-eat) is a terminal emulator for Emacs, which is a bit more advanced than the built-in.

```emacs-lisp
(package! eat
  :recipe (:type git
           :host codeberg
           :repo "akib/emacs-eat"
           :files ("*.el" ("term" "term/*.el") "*.texi" "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el"))))
```


#### Evil {#evil}

> From the `:editor evil` module.

[Here](https://countvajhula.com/2021/01/21/vim-tip-of-the-day-a-series/) is a pretty good series on Vim.

<!--list-separator-->

-  Functions

    ```emacs-lisp
    (defun cust/evil-esc ()
      "Handle ESC in different evil states. Handy when needing to wrap advices."
      (interactive)
      (cond
       ((evil-normal-state-p) (call-interactively 'evil-force-normal-state))
       ((evil-visual-state-p) (call-interactively 'evil-exit-visual-state))
       ((evil-insert-state-p) (call-interactively 'evil-normal-state))
       (t (keyboard-escape-quit))))

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
      (evil-end-of-visual-line))

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

    (defun cust/visual-next-end-of-method ()
      "Enter visual mode and move to end of current function."
      (interactive)
      (unless (evil-visual-state-p)
        (evil-visual-state))
      (if (fboundp '+evil/next-end-of-method)
          (call-interactively #'+evil/next-end-of-method)
        (call-interactively #'end-of-defun)))

    (defun cust/visual-previous-beginning-of-method ()
      "Enter visual mode and move to beginning of previous function."
      (interactive)
      (unless (evil-visual-state-p)
        (evil-visual-state))
      (if (fboundp '+evil/previous-beginning-of-method)
          (call-interactively #'+evil/previous-beginning-of-method)
        (call-interactively #'beginning-of-defun)))

    (defun cust/evil-visual-shift-right ()
      "Wrap `evil-shift-right' in interactive and keeps visual mode"
      (interactive)
      (call-interactively 'evil-shift-right)
      (evil-normal-state)
      (evil-visual-restore))

    (defun cust/evil-visual-shift-left ()
      "Wrap `evil-shift-left' in interactive and keeps visual mode"
      (interactive)
      (call-interactively 'evil-shift-left)
      (evil-normal-state)
      (evil-visual-restore))

    (defun cust/consult-yank-pop ()
      "If there's an active region, delete it before running `consult-yank-pop'"
      (interactive)
      (if (use-region-p)
          (progn
            (delete-region (region-beginning) (region-end))
            (consult-yank-pop))
        (consult-yank-pop)))

    (defun cust/backward-kill-word ()
      "An `Intellij-style' smart backward-kill-word."
      (interactive)
      (let* ((cp (point))
             (backword)
             (end)
             (space-pos)
             (backword-char (if (bobp)
                                ""           ;; cursor in begin of buffer
                              (buffer-substring cp (- cp 1)))))
        (if (equal (length backword-char) (string-width backword-char))
            (progn
              (save-excursion
                (setq backword (buffer-substring (point) (progn (forward-word -1) (point)))))
              (save-excursion
                (when (and backword          ;; when backword contains space
                           (s-contains? " " backword))
                  (setq space-pos (ignore-errors (search-backward " ")))))
              (save-excursion
                (let* ((pos (ignore-errors (search-backward-regexp "\n")))
                       (substr (when pos (buffer-substring pos cp))))
                  (when (or (and substr (s-blank? (s-trim substr)))
                            (s-contains? "\n" backword))
                    (setq end pos))))
              (if end
                  (kill-region cp end)
                (if space-pos
                    (kill-region cp space-pos)
                  (backward-kill-word 1))))
          (kill-region cp (- cp 1))) ;; word is non-english word
        ))

    (defun cust/forward-kill-word ()
      "An `Intellij-style' smart forward-kill-word."
      (interactive)
      (let* ((cp (point))
             (forward-word)
             (end)
             (space-pos)
             (forward-word-char (if (eobp)
                                    ""           ;; cursor at end of buffer
                                  (buffer-substring cp (1+ cp)))))
        (if (equal (length forward-word-char) (string-width forward-word-char))
            (progn
              (save-excursion
                (setq forward-word (buffer-substring (point) (progn (forward-word 1) (point)))))
              (save-excursion
                (when (and forward-word          ;; when forward-word contains space
                           (string-match " " forward-word))
                  (setq space-pos (ignore-errors (search-forward " " nil t)))))
              (save-excursion
                (let* ((pos (ignore-errors (search-forward-regexp "\n" nil t)))
                       (substr (when pos (buffer-substring cp pos))))
                  (when (or (and substr (string-blank-p (string-trim substr)))
                            (string-match "\n" forward-word))
                    (setq end pos))))
              (if end
                  (kill-region cp end)
                (if space-pos
                    (kill-region cp space-pos)
                  (kill-word 1))))
          (kill-region cp (1+ cp)))  ;; word is non-English word
        ))
    ```

<!--list-separator-->

-  Keybindings

    ```emacs-lisp
    (after! evil
      (setq evil-escape-key-sequence "jk")
      (setq evil-escape-unordered-key-sequence nil)
      (setq evil-respect-visual-line-mode t)

      ;; I think the default `backward-kill-word' and `forward-kill-word' functions
      ;; are a little too greedy.
      (global-set-key [C-backspace] #'cust/backward-kill-word)
      (global-set-key [M-backspace] #'cust/backward-kill-word)
      (global-set-key [C-delete]    #'cust/forward-kill-word)
      (global-set-key [M-delete]    #'cust/forward-kill-word)

      (evil-global-set-key 'normal "Q" #'evil-execute-q-macro)
      (define-key evil-normal-state-map (kbd "C-S-u")     #'evil-scroll-other-window-interactive)
      (define-key evil-normal-state-map (kbd "C-S-d")     #'evil-scroll-other-window-down-interactive)
      (define-key evil-normal-state-map (kbd "M-y")       #'cust/consult-yank-pop) ;; Better "paste" from clipboard

      (evil-define-key '(visual motion) 'global
        "H"  #'evil-first-non-blank
        "L"  #'evil-end-of-line-interactive
        "0"  #'evil-jump-item)

      ;; Center text when scrolling and searching for text.
      (advice-add 'evil-ex-search-next     :after #'evil-scroll-to-center-advice)
      (advice-add 'evil-ex-search-previous :after #'evil-scroll-to-center-advice)
      (advice-add 'evil-scroll-up          :after #'evil-scroll-to-center-advice)
      (advice-add 'evil-scroll-down        :after #'evil-scroll-to-center-advice))
    ```

    Sometimes it's convenient to insert multiple cursors using the mouse. Inserts a new cursor using `C-S-<mouse-1>`.

    ```emacs-lisp
    (after! evil
      (global-set-key (kbd "C-S-<mouse-1>") #'evil-mc-toggle-cursor-on-click))
    ```


#### Folding {#folding}

> From the `:editor fold` module

Doom Emacs comes with a combination of `hideshow`, `vimish-fold` and `outline-minor-mode` to
enable folding for various modes. It doesn't seem to work properly with `LSP` mode, so we'll
configure `lsp-origami` to use for code folding with `lsp-mode`.

```emacs-lisp
(package! lsp-origami)
```

```emacs-lisp
(use-package! lsp-origami
  :hook (lsp-after-open-hook . #'lsp-origami-try-enable))
```


#### Formatting {#formatting}

> From the `:editor format` module

Doom Emacs now uses [aphelieia](https://github.com/radian-software/apheleia) as the default formatting system.
Override to use the latest version from Github.

```emacs-lisp
(package! apheleia :recipe (:repo "radian-software/apheleia"))
```


#### Just {#just}

Add support for the `justfiles`.

```emacs-lisp
(package! just-mode)
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

Here's my analysis of your Emacs configuration:

This is a well-structured literate Emacs configuration using Org mode with Doom Emacs as the framework. The configuration demonstrates thoughtful organization and contains many useful customizations. Here are some key observations:

1.  **Structure and Organization**:
    -   Your use of Org mode to organize your configuration is excellent, with a clear table of contents and logical sections
    -   Good separation of concerns between UI settings, keybindings, packages, and language-specific configurations

2.  **Notable Features**:
    -   Custom splash screen with theme-appropriate SVG images
    -   Transparent titlebar configuration for macOS
    -   Extensive window and buffer navigation keybindings
    -   Comprehensive dired configuration with useful extensions
    -   Thoughtful LSP and development tool integrations
    -   Well-configured spelling and grammar checking

3.  **Smart Optimizations**:
    -   Deferred garbage collection settings for better startup performance
    -   Workaround for the "too many open files" error with file notifications
    -   Font fallback detection to warn about missing required fonts
    -   Theme switching utilities with proper reloading

4.  **Modern Editor Features**:
    -   Integration with GitHub Copilot and other LLM tools
    -   Code folding with lsp-origami
    -   Golden ratio for automatic window sizing
    -   Comprehensive evil-mode configuration

Some suggestions for consideration:

1.  You might want to complete the Lorem-Ipsum section that appears to be incomplete at the end.

2.  The file-notify workaround (clearing file watches every 5 minutes) seems like a temporary solution - you might want to investigate a more permanent fix.

3.  Consider adding more documentation to your custom functions, especially the more complex ones.

4.  You have some commented out code (like the dired-open-macos function) that you might want to revisit.

Overall, this is an impressive and well-thought-out configuration that shows a good understanding of Emacs and attention to detail in creating a productive environment.

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


#### Version Control System {#version-control-system}

<!--list-separator-->

-  Magit

    > From the `:tools magit` module

    Taken from the [Modern Emacs](http://www.modernemacs.com/#spacemacs) blog

    ```emacs-lisp
    (after! magit
      (defvar pretty-magit--alist nil
        "An alist of regexes, an icon, and face properties to apply to icon.")

      (defvar pretty-magit--prompt nil
        "A list of commit leader prompt candidates.")

      (defvar pretty-magit--use-commit-prompt? nil
        "Do we need to use the magit commit prompt?")

      (defun pretty-magit--add-magit-faces (&rest _)
        "Add face properties and compose symbols for buffer from pretty-magit."
        (interactive)
        (with-silent-modifications
          (-each pretty-magit--alist
            (lambda (input0 &optional _index)
              (let* ((--dash-source-0-- input0)
                     (rgx (car-safe (prog1 --dash-source-0-- (setq --dash-source-0-- (cdr --dash-source-0--)))))
                     (char (car-safe (prog1 --dash-source-0-- (setq --dash-source-0-- (cdr --dash-source-0--)))))
                     (face-props (car --dash-source-0--)))
                (save-excursion
                  (goto-char (point-min))
                  (while (re-search-forward rgx nil t)
                    (let* ((--dash-source-1-- (match-data)))
                      (when (>= (length --dash-source-1--) 2)
                        (let ((start (nth 0 --dash-source-1--))
                              (end (nth 1 --dash-source-1--)))
                          (when (and start end)
                            (compose-region start end char)
                            (when face-props
                              (add-face-text-property start end face-props)))))))))))))

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
          (add-hook    'git-commit-setup-hook 'gptel-magit-generate-message)
          (advice-add 'magit-commit-create :after 'pretty-magit--use-commit-prompt)
          )))
    ```

    ```emacs-lisp
    (after! magit
      ;; I'm testing out `gptel' to write automatic commit messages, so this is currently disabled.

      ;; (pretty-magit-add-leaders '(("feat" ? (:foreground "#C2C8CD" :height 1.2))
      ;;                             ("add"     ? (:foreground "#375E97" :height 1.2))
      ;;                             ("fix"     ? (:foreground "#FB6542" :height 1.2))
      ;;                             ("clean"   ? (:foreground "#FFBB00" :height 1.2))
      ;;                             ("chore"   ? (:foreground "#CE98FF" :height 1.2))
      ;;                             ("docs"    ? (:foreground "#3F681C" :height 1.2))))
      (pretty-magit-setup))
    ```

    Run `vc-refresh-state()` for all buffers in Magit's `post-refresh-hook`.

    ```emacs-lisp
    (after! magit
      (defun cust/vc-refresh-all-buffers-state ()
        "Update version control state in all buffers"
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (vc-refresh-state))))

      (add-hook 'magit-post-refresh-hook #'cust/vc-refresh-all-buffers-state))
    ```

    When copying commit hashes I almost always just want the short version. This
    is controlled by the `magit-copy-revision-abbreviated` variable. The latest commit hash
    can be copied from a magit buffer using `M-w`.

    ```emacs-lisp
    (after! magit
      (setq git-commit-summary-max-length   80                  ;; Increase commit summary length
            magit-copy-revision-abbreviated t                   ;; Copy short version of hashes
            magit-list-refs-sortby          "-committerdate"))  ;; Sort by last commited date (latest on top)
    ```

<!--list-separator-->

-  Code Review

    ```emacs-lisp
    ;; Adds the *Code-Review* buffer to the current perspective
    (add-hook 'code-review-mode-hook
              (lambda ()
                (persp-add-buffer (current-buffer))))

    ;; Emojis are fun!
    (add-hook 'code-review-mode-hook #'emojify-mode)

    ;; Use the forge credentials for authentication (from ~/.authinfo.gpg)
    (setq code-review-auth-login-marker 'forge)
    ```

<!--list-separator-->

-  Custom Remotes

    ```emacs-lisp
    ;;; Forge
    (after! forge
      ;; GitLab
      (setq gitlab.user "user"
            forge-add-default-bindings nil)
      (add-to-list 'forge-alist '("gitlab.intility.com" "gitlab.intility.com/api/v4" "gitlab.intility.com" forge-gitlab-repository)))

    ;;; Browse remotes
    (require 'browse-at-remote)
    (add-to-list 'browse-at-remote-remote-type-regexps '("^gitlab\\.intility\\.com$" . "gitlab"))
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


#### Outshine {#outshine}

Outshine attempts to bring the look and feel of `org-mode` to the world outside Org major mode.
It's an extension of `outline-minor-mode` that should act as a replacement of `outline-mode`.

```emacs-lisp
(package! outshine)
```

```emacs-lisp
;; (use-package! outshine
;;   :hook ((prog-mode          . outline-minor-mode)
;;          (outline-minor-mode . outshine-mode))
;;   :init
;;   (progn
;;     (advice-add 'outshine-narrow-to-subtree :before #'outshine-fix-narrow-pos)
;;     (advice-add 'outshine-insert-heading    :before #'outshine-fix-insert-pos)
;;     (advice-add 'outshine-insert-heading    :after  #'evil-insert-advice)
;;     (advice-add 'outshine-insert-subheading :after  #'evil-insert-advice))
;;   :config
;;   (evil-collection-define-key '(normal visual motion) 'outline-minor-mode-map
;;     "gh"        #'outline-up-heading
;;     "gj"        #'outline-forward-same-level
;;     "gk"        #'outline-backward-same-level
;;     "gl"        #'outline-next-visible-heading
;;     "gu"        #'outline-previous-visible-heading))
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
      projectile-auto-cleanup-known-projects  t
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

;; DEPRECATED: Remove when projectile is replaced with project.el

(after! projectile
  (dolist (file '("mix.exs" "*.csproj" "*.fsproj"))
    (add-to-list 'projectile-project-root-files file)))
```


#### Spelling {#spelling}

Easily cycle between the languages I use (English and Norwegian) by hitting `F8`.

```emacs-lisp
(let ((langs '("en" "no")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-ispell-languages ()
  "Cycle between languages in the language ring `lang-ring`."
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

;; Cycle languages with F8
(global-set-key (kbd "<f8>") #'cycle-ispell-languages)

;; And add a Doom shortcut to the "toggle" menu for good measures
(map! :leader
      (:prefix-map ("t" . "toggle")
       :desc "Next spell checker language" "S" #'cycle-ispell-languages))
```

For a personal dictionaries (with all the misspelled words and such)

```emacs-lisp
(setq ispell-extra-args          '("--sug-mode=ultra" "--run-together")
      ispell-local-dictionary    "en"
      ispell-program-name        "aspell"
      ispell-personal-dictionary (expand-file-name ".dictionaries/personal.pws" doom-private-dir))

(unless (file-exists-p ispell-personal-dictionary)
  (write-region "" nil ispell-personal-dictionary nil 0))
```

```emacs-lisp
(use-package! flyspell
  :when (modulep! :checkers spell +flyspell)
  :after ispell
  ;; :hook (find-file . flyspell-on-for-buffer-type)  ;; Getting errors, needs fix
  :init
  (defun flyspell-on-for-buffer-type ()
    "Enable Flyspell appropriately for the major mode of the current buffer.
Uses `flyspell-prog-mode' for modes derived from `prog-mode', so only strings
and comments get checked. Other buffers get `flyspell-mode' to check all text.
If flyspell is already enabled for buffer, do nothing."
    (interactive)
    (if (not (symbol-value flyspell-mode)) ;; if not already enabled
        (progn
          (if (derived-mode-p 'prog-mode)
              (progn
                (message "Flyspell on (code)")
                (flyspell-prog-mode))
            (progn  ;; else
              (message "Flyspell on (text)")
              (flyspell-mode 1))))))
  :config
  ;; Significantly speeds up flyspell, which would otherwise print
  ;; messages for every word when checking the entire buffer
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))
```

<!--list-separator-->

-  Downloading dictionaries

    Currently using Aspell with Norwegian and English languages.
    Should probably document installation.


#### Smartparens {#smartparens}

> From the `:core packages` module.

```emacs-lisp
(after! smart-parens
  (sp-local-pair '(org-mode) "<<" ">>" :actions '(insert)))
```

Enable colored matching for delimiters

```emacs-lisp
(add-hook! 'prog-mode-hook #'rainbow-mode)
```


#### Symbol-Overlay {#symbol-overlay}

Highlights symbols with overlays while providing a keymap for various operations on the highlighted symbols.

```emacs-lisp
(package! symbol-overlay)
```

Advice removing all symbol overlays when hitting `ESC`.

```emacs-lisp
(defun cust/advice-symbol-overlay-remove-all (orig-fun &rest args)
  "Remove all symbol overlays when pressing ESC"
  (interactive)
  (when (symbol-overlay-get-list 0)
    (symbol-overlay-remove-all))
  (apply orig-fun args))
```

Set up some basic keybindings

```emacs-lisp
(use-package! symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  :config
  (evil-define-key '(normal insert) 'global (kbd "s-i") #'symbol-overlay-put)        ;; Put an overlay on the symbol on point
  (evil-define-key '(normal insert) 'global (kbd "s-n") #'symbol-overlay-jump-next)  ;; Jump to next symbol on point
  (evil-define-key '(normal insert) 'global (kbd "s-p") #'symbol-overlay-jump-prev)  ;; Jump to previous symbol on point
  (evil-define-key '(normal visual insert) 'global [escape] #'cust/evil-esc)         ;; We need a custom ESC function for the following advice
  (advice-add 'cust/evil-esc :around #'cust/advice-symbol-overlay-remove-all))       ;; Advice ESC to remove all symbol overlays
```


#### Tramp {#tramp}

Tramp makes accessing remote file systems using Emacs a blast.

```emacs-lisp
(after! tramp
  (setq tramp-default-method "scp"))
```


#### Treemacs {#treemacs}

Automatically remove projects from the `treemacs-persist` file when no longer present.

```emacs-lisp
(setq treemacs-persist-file nil) ;; Disable using the treemacs-persist file. It cause too much trouble when removing or moving projects around.
```

Make `treemacs` pretty and functional.

```emacs-lisp
(after! (treemacs winum)
  (setq doom-themes-treemacs-theme "doom-colors"        ; Enable nice colors for treemacs
        doom-themes-treemacs-enable-variable-pitch t)   ; Enable variable-pitch font

  (setq winum-ignored-buffers-regexp
        (delete (regexp-quote (format "%sFramebuffer-" treemacs--buffer-name-prefix))
                winum-ignored-buffers-regexp))

  ;; This PR cause treemacs to appear on the bottom instead of on the left/right hand side
  ;; https://github.com/Alexander-Miller/treemacs/pull/971
  ;; Workaround:
  (set-popup-rule! "^ \\*Treemacs"
    :side treemacs-position
    :width treemacs-width
    :quit nil)

  (treemacs-project-follow-mode t)
  (treemacs-follow-mode t)
  (treemacs-define-RET-action 'file-node-open   #'treemacs-visit-node-in-most-recently-used-window)
  (treemacs-define-RET-action 'file-node-closed #'treemacs-visit-node-in-most-recently-used-window))
```

`lsp-treemacs` integrates `treemacs` with `lsp-mode`.

```emacs-lisp
(after! (treemacs lsp-mode)
  (lsp-treemacs-sync-mode 1))

(map! :leader
      :after lsp-treemacs
      (:prefix-map ("c" . "code")
       :desc "List errors"  "x" #'lsp-treemacs-errors-list
       :desc "Show symbols" "y" #'lsp-treemacs-symbols))
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
        "C-u"     #'vertico-scroll-down
        "C-d"     #'vertico-scroll-up
        "C-c C-o" #'embark-export))
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

```emacs-lisp
(map! :after vterm
      :map vterm-mode-map
      :i "C-j"  (lambda () (interactive) (vterm-send-key "<down>"))
      :i "C-k"  (lambda () (interactive) (vterm-send-key "<up>"))
      :n "C-j"  (lambda () (interactive) (vterm-send-key "<down>"))
      :n "C-k"  (lambda () (interactive) (vterm-send-key "<up>"))
      ;; Delete word forward - Both M-delete and C-delete in normal and insert mode
      :i "C-<delete>"    (lambda () (interactive) (vterm-send-key "d" nil t))
      :n "C-<delete>"    (lambda () (interactive) (vterm-send-key "d" nil t))
      :i "M-<delete>"    (lambda () (interactive) (vterm-send-key "d" nil t))
      :n "M-<delete>"    (lambda () (interactive) (vterm-send-key "d" nil t))
      ;; Delete word backwards - both M-backspace and C-backspace in normal and insert mode
      :i "M-<backspace>" #'vterm-send-meta-backspace
      :n "M-<backspace>" #'vterm-send-meta-backspace
      :i "C-<backspace>" #'vterm-send-meta-backspace
      :n "C-<backspace>" #'vterm-send-meta-backspace
      :i "C-g"        #'vterm-send-escape
      ;; Meta+Enter sends a newline character to the vterm process
      :i "M-<return>" (lambda () (interactive) (process-send-string vterm--process "\n"))
      :n "M-<return>" (lambda () (interactive) (process-send-string vterm--process "\n"))
      "<deletechar>"  #'vterm-send-delete
      ;; Needs to bind these here as well for some reason..
      "M-0"  #'treemacs-select-window
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


### Visuals {#visuals}


#### Highlight Indent Guides {#highlight-indent-guides}

The [highlight-indent-guides](https://github.com/DarthFennec/highlight-indent-guides) minor mode package can be useful in certain `prog-modes`.

```emacs-lisp
(add-hook! 'prog-mode-hook #'indent-bars-mode)
```


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


#### Ultra Scroll {#ultra-scroll}

Instead of using Dooms built-in `smooth-scroll`, I'm using the [ultra-scroll](https://github.com/jdtsmith/ultra-scroll) package.
The only caveat (so far) is that it requires us to drop scroll-margin, but that will hopefully
be fixed in the future.

```emacs-lisp
(package! ultra-scroll
  :recipe (:host github :repo "jdtsmith/ultra-scroll" :files ("*.el")))
```

```emacs-lisp
(use-package! ultra-scroll
  :init
  (setq scroll-conservatively 3
        scroll-margin 0)          ; important: scroll-margin greater than 0 not yet supported
  :config
  (ultra-scroll-mode 1))
```


#### Writeroom / Zen mode {#writeroom-zen-mode}

> From the `:ui zen` module.

I like to toggle `writeroom-mode` for all buffers at once.

```emacs-lisp
(map! :leader
      (:when (modulep! :ui zen)
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
(defvar +zen-serif-p nil
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


#### LSP {#lsp}

```emacs-lisp
(after! lsp-mode
  (setq read-process-output-max (* 1024 1024) ;; 1mb
        lsp-auto-guess-root t
        lsp-file-watch-threshold 1000000
        lsp-idle-delay 0.500
        lsp-modeline-code-actions-segments '(count icon name)
        lsp-response-timeout 10))

(after! lsp-ui
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
```

<!--list-separator-->

-  Emacs LSP Performance Booster

    [Emacs LSP Performance Booster](https://github.com/blahgeek/emacs-lsp-booster) is a wrapper executable written in Rust that improves the performance
    of LSP mode. See build instructions in the Github repository.

    ```emacs-lisp
    (defun lsp-booster--advice-json-parse (old-fn &rest args)
      "Try to parse bytecode instead of json."
      (or
       (when (equal (following-char) ?#)
         (let ((bytecode (read (current-buffer))))
           (when (byte-code-function-p bytecode)
             (funcall bytecode))))
       (apply old-fn args)))
    (advice-add (if (progn (require 'json)
                           (fboundp 'json-parse-buffer))
                    'json-parse-buffer
                  'json-read)
                :around
                #'lsp-booster--advice-json-parse)

    (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
      "Prepend emacs-lsp-booster command to lsp CMD."
      (let ((orig-result (funcall old-fn cmd test?)))
        (if (and (not test?)                             ;; for check lsp-server-present?
                 (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                 lsp-use-plists
                 (not (functionp 'json-rpc-connection))  ;; native json-rpc
                 (executable-find "emacs-lsp-booster"))
            (progn
              (message "Using emacs-lsp-booster for %s!" orig-result)
              (cons "emacs-lsp-booster" orig-result))
          orig-result)))
    (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
    ```


#### LSP-ESLint {#lsp-eslint}

Start `lsp-eslint` in projects that supports it.

```emacs-lisp
(use-package! lsp-eslint
  :after lsp-mode)
```


#### LSP-TailwindCSS {#lsp-tailwindcss}

```emacs-lisp
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
```

Start `lsp-tailwindcss` in projects that supports it.

```emacs-lisp
(use-package! lsp-tailwindcss
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             typescript-tsx-mode
             js2-mode
             js-ts-mode
             clojure-mode
             heex-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))
```


#### Tree-sitter {#tree-sitter}

Use [treesit-auto](https://github.com/renzmann/treesit-auto) to automatically install language grammar files.

```emacs-lisp
(package! treesit-auto)
```

```emacs-lisp
(use-package! treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
```


### Yaml {#yaml}

Yaml comes in many different flavors, let's select schema with `,s`.

```emacs-lisp
;; Replace yaml-mode with yaml-ts-mode.
(add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))

(map! :map yaml-ts-mode-map
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
(setq org-directory                             "~/OneDrive/org"
      org-cliplink-transport-implementation     'curl
      org-crypt-key                             "rhblind@gmail.com"
      org-tag-alist                             '(("crypt" . ?c))
      org-id-link-to-org-use-id                 t
      org-agenda-text-search-extra-files        '())
(setq org-download-image-dir (concat (file-name-as-directory org-directory) "images"))
```


#### Behaviour {#behaviour}

Sometimes I'm a bit lazy and just want to click around with the mouse.

```emacs-lisp
(require 'org-mouse)
```

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
(when (and (modulep! :lang org +hugo) (eq system-type 'darwin))
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
(setq org-roam-v2-ack           t
      org-roam-db-autosync-mode t
      org-roam-directory        (concat (file-name-as-directory org-directory) "roam"))
(setq org-roam-index-file       (concat (file-name-as-directory org-roam-directory) "index.org"))
```

TODO Organize this better

```emacs-lisp
(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "reference/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new
         (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t
         :unnarrowed t)))
```

```emacs-lisp
(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))
```

```emacs-lisp
(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
```

Add all `org-roam` files to list of extra files to be searched by text commands.

```emacs-lisp
(after! (org org-roam)
  (setq org-agenda-text-search-extra-files (org-roam--list-files org-roam-directory)))
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
          '((?A . 'nerd-red)
            (?B . 'nerd-orange)
            (?C . 'nerd-yellow)
            (?D . 'nerd-green)
            (?E . 'nerd-blue)))
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
                :priority_a   ,(propertize "⚑" 'face 'nerd-icons-red)
                :priority_b   ,(propertize "⬆" 'face 'nerd-icons-orange)
                :priority_c   ,(propertize "■" 'face 'nerd-icons-yellow)
                :priority_d   ,(propertize "⬇" 'face 'nerd-icons-green)
                :priority_e   ,(propertize "❓" 'face 'nerd-icons-blue)))

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
                        `(("q" ,(concat (nerd-icons-octicon "nf-oct-stop" :face 'nerd-icons-red :v-adjust 0.01) "\tAbort")))))))
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
                              (insert (format "%s   %s\n" (propertize key 'face '(bold nerd-icons-red)) description))
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
                  (set  (intern (concat "nerd-icons-" (plist-get declaration :set))))
                  (face (intern (concat "nerd-icons-" (plist-get declaration :color))))
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
            (setq org-capture-templates
                  (doct `(("Todo" :keys "t"
                           :icon ("nf-oct-checklist" :set "octicon" :color "green")
                           :file +org-capture-todo-file
                           :prepend t
                           :headline "Inbox"
                           :type entry
                           :template ("* TODO %? %^G"
                                      "%i"))
                          ("Note" :keys "n"
                           :icon ("nf-fa-sticky_note_o" :set "faicon" :color "green")
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
                           :icon ("nf-fa-envelope" :set "faicon" :color "blue")
                           :file +org-capture-todo-file
                           :prepend t
                           :headline "Inbox"
                           :type entry
                           :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                                      "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                                      "about %^{topic}"
                                      "%U %i"))
                          ("Interesting" :keys "i"
                           :icon ("nf-fa-eye" :set "faicon" :color "lcyan")
                           :file +org-capture-todo-file
                           :prepend t
                           :headline "Interesting"
                           :type entry
                           :template ("* [ ] %{desc}%? :%{i-type}:"
                                      "%i")
                           :children (("Webpage" :keys "w"
                                       :icon ("nf-fa-globe" :set "faicon" :color "green")
                                       :desc "%(org-cliplink-capture) "
                                       :i-type "read:web")
                                      ("Article" :keys "a"
                                       :icon ("nf-fa-file_text" :set "faicon" :color "yellow")
                                       :desc ""
                                       :i-type "read:reaserch")
                                      ("\tRecipie" :keys "r"
                                       :icon ("nf-fa-spoon" :set "faicon" :color "orange")
                                       :file +org-capture-recipies
                                       :headline "Unsorted"
                                       :template "%(org-chef-get-recipe-from-url)")
                                      ("Information" :keys "i"
                                       :icon ("nf-fa-info_circle" :set "faicon" :color "blue")
                                       :desc ""
                                       :i-type "read:info")
                                      ("Idea" :keys "I"
                                       :icon ("nf-fa-lightbulb" :set "faicon" :color "purple")
                                       :desc ""
                                       :i-type "idea")))
                          ("Tasks" :keys "k"
                           :icon ("nf-fa-inbox" :set "faicon" :color "yellow")
                           :file +org-capture-todo-file
                           :prepend t
                           :headline "Tasks"
                           :type entry
                           :template ("* TODO %? %^G%{extra}"
                                      "%i")
                           :children (("General Task" :keys "k"
                                       :icon ("nf-fa-inbox" :set "faicon" :color "green")
                                       :extra "")
                                      ("Capture point Task" :keys "c"
                                       :icon ("nf-fa-pencil" :set "faicon" :color "yellow")
                                       :extra "")
                                      ("Task with deadline" :keys "d"
                                       :icon ("nf-oct-clock" :set "octicon" :color "orange")
                                       :extra "\nDEADLINE: %^{Deadline:}t")
                                      ("Scheduled Task" :keys "s"
                                       :icon ("nf-oct-calendar" :set "octicon" :color "orange")
                                       :extra "\nSCHEDULED: %^{Start time:}t")))
                          ("Project" :keys "p"
                           :icon ("nf-oct-repo" :set "octicon" :color "purple")
                           :prepend t
                           :type entry
                           :template ("* %{time-or-todo} %?"
                                      "%i")
                           :file ""
                           :custom (:time-or-todo "")
                           :children (("Project-local todo" :keys "t"
                                       :icon ("nf-oct-checklist" :set "octicon" :color "green")
                                       :time-or-todo "TODO"
                                       :headline "TODO"
                                       :file +org-capture-project-todo-file)
                                      ("Project-local capture point task" :keys "c"
                                       :icon ("nf-oct-pencil" :set "octicon" :color "yellow")
                                       :template ("* TODO %?"
                                                  "%a")
                                       :headline "TODO"
                                       :file +org-capture-project-todo-file)
                                      ("Project-local note" :keys "n"
                                       :icon ("nf-fa-note_sticky" :set "faicon" :color "yellow")
                                       :time-or-todo "%U"
                                       :headline "Notes"
                                       :file +org-capture-project-todo-file)
                                      ("Project-local changelog" :keys "c"
                                       :icon ("nf-oct-list_unordered" :set "octicon" :color "blue")
                                       :time-or-todo "%t"
                                       :headline "Changelog"
                                       :heading "Unreleased"
                                       :file +org-capture-project-changelog-file)))
                          ("Blog" :keys "b"
                           :icon ("nf-fa-pied_piper" :set "faicon" :color "pink")
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
                          ("Centralised project templates" :keys "o"
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
                                       :file +org-capture-projects-file)
                                      ("Project note"
                                       :keys "n"
                                       :time-or-todo "%U"
                                       :heading "Notes"
                                       :file +org-capture-notes-file)
                                      ("Project changelog"
                                       :keys "c"
                                       :time-or-todo "%U"
                                       :heading "Unreleased"
                                       :file +org-capture-changelog-file)))
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


### Markdown {#markdown}


#### Visuals {#visuals}

Make headers prettier.

```emacs-lisp
(custom-set-faces!
  '(markdown-header-delimiter-face :height 0.9)
  '(markdown-header-face-1 :weight bold :height 1.25 :inherit markdown-header-face)
  '(markdown-header-face-2 :weight bold :height 1.15 :inherit markdown-header-face)
  '(markdown-header-face-3 :weight bold :height 1.12 :inherit markdown-header-face)
  '(markdown-header-face-4 :weight semi-bold :height 1.09 :inherit markdown-header-face)
  '(markdown-header-face-5 :weight semi-bold :height 1.06 :inherit markdown-header-face)
  '(markdown-header-face-6 :weight semi-bold :height 1.03 :inherit markdown-header-face))
```


#### Preview {#preview}

Preview markdown files in a Xwidget window with `<localleader>-p`.

```emacs-lisp
(package! markdown-xwidget
  :recipe (:host github
           :repo "cfclrk/markdown-xwidget"
           :files (:defaults "resources")))
```

```emacs-lisp
(use-package! markdown-xwidget
  :after markdown-mode
  :init
  (map! :map markdown-mode-map
        :localleader
        "p" #'markdown-xwidget-preview-mode))
```


### Web/Javascript/Typescript {#web-javascript-typescript}

For a good guide on setting up this for tree-sitter, see here:

-   <https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config>


#### Formatting {#formatting}

[Prettier](https://prettier.io/) is an opinionated code formatter for many languages, most notably various Javascript dialects, HTML,
CSS and friends.

```emacs-lisp
(package! prettier)
```

```emacs-lisp
(use-package! prettier
  :hook (web-mode . prettier-mode))
```


#### Typescript {#typescript}

Remap `typescript-mode` to the tree-sitter equivalent.

```emacs-lisp
(add-list-to-list 'major-mode-remap-alist '((typescript-mode . typescript-ts-mode)
                                            (typescript-tsx-mode . typescript-tsx-mode)))
```

Hook up some extra tooling...

```emacs-lisp
(use-package! typescript-ts-mode
  :hook (typescript-ts-mode . prettier-mode)
  :config
  (add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook) #'lsp!))
```

... and optionally disable the old `typescript-mode`.

```emacs-lisp
;; (package! typescript-mode :disable t)
```


### Python {#python}


#### LSP Python {#lsp-python}

Add some extra `ignored-directories` for LSP.

```emacs-lisp
(after! (python lsp-mode)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.eggs\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\build\\'")

  ;; Make sure pyright can find the virtualenv
  ;; (add-hook 'pyvenv-post-activate-hooks (lambda ()
  ;;                                         (when (modulep! :lang python +pyright)
  ;;                                           (setq lsp-pyright-venv-path pyvenv-virtual-env))))
  )
```

Also, check out [this](https://emacs-lsp.github.io/lsp-mode/page/performance/) handy guide for extra performance tips!
It's recommended to use `plist` for deserialization. In order to achieve that we need to export a
`LSP_USE_PLISTS=true` environmental variable and ensure that Emacs knows about it (before `lsp-mode` is compiled).


#### Debugging {#debugging}

> DAP expects ptvsd by default as the Python debugger, however debugpy is recommended.

So be sure to install `debugpy`.

```shell
$ pip3 install debugpy --user
```

```emacs-lisp
(after! (python dap-mode)
  (setq dap-python-debugger 'debugpy))
```


### Elixir {#elixir}

Use the latest and greatest!

```emacs-lisp
(unpin! (:lang elixir))
(unpin! elixir-mode)
(package! flycheck-dialyxir)
```


#### Elixir tree-sitter support {#elixir-tree-sitter-support}

Tree-sitter is a wide-spread system for parsing text, and is the "new" way to support syntax highlighting in many text editors, including Emacs.

To use tree-sitter, either Emacs 29.1 compiled with tree-sitter support or Emacs 30 (which has it built-in) is required. To enable tree-sitter for Elixir with Doom Emacs, some extra steps are required (as per october 2024).

**Step 1** - Install tree-sitter support for `.heex` and `.ex` files.

```emacs-lisp
(package! heex-ts-mode)
(package! elixir-ts-mode)
```

**Step 2** - Install grammar files
Execute `M-x treesit-install-language-grammar` and enter `elixir`.  When prompted if you want to do it interactively, enter "Yes",
and paste the Github repository (see below) for the grammar files. Just keep pressing `<enter>` to accept defaults for the rest of the process.
Repeat the step for `heex`.

**Github repositories:**

-   Elixir: <https://github.com/elixir-lang/tree-sitter-elixir>
-   Heex: <https://github.com/phoenixframework/tree-sitter-heex>

**Step 3** - Configure the modes

```emacs-lisp
;; Replace the elixir-mode with elixir-ts-mode
(add-to-list 'major-mode-remap-alist '(elixir-mode . elixir-ts-mode))

(add-hook! 'elixir-ts-mode-hook #'lsp)

(use-package! elixir-ts-mode
  :defer t
  :init (provide 'smartparens-elixir)
  :config
  (set-ligatures! 'elixir-ts-mode
    :def "def"
    :lambda "fn"
    :not "!"
    :in "in"
    :not_in "not in"
    :and "and"
    :or "or"
    :for "for"
    :return "return"
    :yield "use")
  (sp-with-modes 'elixir-ts-mode
      (sp-local-pair "do" "end"
                  :when '(("RET" "<evil-ret>"))
                  :unless '(sp-in-comment-p sp-in-string-p)
                  :post-handlers '("||\n[i]"))
      (sp-local-pair "do " " end" :unless '(sp-in-comment-p sp-in-string-p))
      (sp-local-pair "fn " " end" :unless '(sp-in-comment-p sp-in-string-p)))

  (after! highlight-numbers
    (puthash 'elixir-ts-mode
             "\\_<-?[[:digit:]]+\\(?:_[[:digit:]]\\{3\\}\\)*\\_>"
             highlight-numbers-modelist)))

;; Override the `lsp-credo-version' variable to get the latest version.
;; It has to be set before `lsp-credo.el' is loaded.
(custom-set-variables '(lsp-credo-version "0.3.0"))
(use-package! flycheck-credo
  :when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
  :after elixir-ts-mode
  :config (flycheck-credo-setup))

(use-package! flycheck-dialyxir
  :when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
  :after elixir-ts-mode
  :config (flycheck-dialyxir-setup))

(use-package! exunit
  :hook (elixir-ts-mode . exunit-mode)
  :init
  (map! :after elixir-ts-mode
        :localleader
        :map elixir-ts-mode-map
        :prefix ("t" . "test")
        "a" #'exunit-verify-all
        "r" #'exunit-rerun
        "v" #'exunit-verify
        "T" #'exunit-toggle-file-and-test
        "t" #'exunit-toggle-file-and-test-other-window
        "s" #'exunit-verify-single
        "X" #'exunit-iex-mode))
```


#### ElixirLS {#elixirls}

The "default" language server for Elixir. A bit slow and heavy on memory usage, but packed with most features.
[See this thread](https://elixirforum.com/t/emacs-elixir-setup-configuration-wiki/19196/194)

```emacs-lisp
(after! lsp-mode
  (add-hook! 'lsp-after-initialize-hook
             (lambda ()
               ;; Disable the ElixirLS Dialyzer
               (setq lsp-elixir-dialyzer-enabled nil))))
```

Available options

```emacs-lisp
(lsp-register-custom-settings
 '(("elixirLS.dialyzerEnabled" lsp-elixir-dialyzer-enabled t)
   ("elixirLS.dialyzerWarnOpts" lsp-elixir-dialyzer-warn-opts)
   ("elixirLS.dialyzerFormat" lsp-elixir-dialyzer-format)
   ("elixirLS.mixEnv" lsp-elixir-mix-env)
   ("elixirLS.mixTarget" lsp-elixir-mix-target)
   ("elixirLS.projectDir" lsp-elixir-project-dir)
   ("elixirLS.fetchDeps" lsp-elixir-fetch-deps t)
   ("elixirLS.suggestSpecs" lsp-elixir-suggest-specs t)
   ("elixirLS.signatureAfterComplete" lsp-elixir-signature-after-complete t)
   ("elixirLS.enableTestLenses" lsp-elixir-enable-test-lenses t)))
```

```emacs-lisp
(after! lsp-mode
  (defvar lsp-elixir--config-options (make-hash-table))

  ;; Disable Dialyzer in elixir-ls
  (puthash "dialyzerEnabled" :json-false lsp-elixir--config-options)

  (add-hook! 'lsp-after-initialize-hook
             (lambda () (lsp--set-configuration `(:elixirLS, lsp-elixir--config-options)))))
```


#### Tidewave (MCP) {#tidewave--mcp}

```emacs-lisp
(defun cust/mcp-hub-start-elixir-servers ()
  ;; Tidewave MCP (https://github.com/tidewave-ai/tidewave_phoenix) for Phoenix Framework
  (add-to-list 'mcp-hub-servers '("tidewave" . (:url "http://localhost:4000/tidewave/mcp")))

  ;; HexDocsMCP (https://github.com/bradleygolden/hexdocs-mcp) allows agents to download
  ;; documentation from hexdocs.pm
  ;; NOTE: HexDocsMPC requires ollama for generating embeddings using the `mxbai-embed-large' model.
  ;; $ ollama pull mxbai-embed-large
  ;; (add-to-list 'mcp-hub-servers '("hexdocs" . (:command "npx" :args ("-y" "hexdocs-mcp@0.6.0"))))
  (mcp-hub-start-all-server))

(add-hook! 'elixir-ts-mode-hook #'cust/mcp-hub-start-elixir-servers)
```


#### HexDocsMCP {#hexdocsmcp}


### C-Sharp {#c-sharp}

**Step 1**
Remap `csharp-mode` to the tree-sitter equivalent.

```emacs-lisp
(add-list-to-list 'major-mode-remap-alist '((csharp-mode . csharp-ts-mode)))
(add-hook! 'csharp-ts-mode-hook #'lsp)
```

```emacs-lisp
(defun my-csharp-prettify-cleanup ()
  "Remove unwanted prettify symbols from C# tree-sitter mode."
  (when (eq major-mode 'csharp-ts-mode)
    (require 'cl-lib)
    (let ((original-alist prettify-symbols-alist))
      (setq-local prettify-symbols-alist
                  (cl-remove-if (lambda (pair)
                                  (member (car pair) '("List" "bool" "string" "float" "int" "true" "false")))
                                prettify-symbols-alist))
      ;; Force refresh if prettify-symbols-mode is active
      (when (and (boundp 'prettify-symbols-mode) prettify-symbols-mode)
        (prettify-symbols-mode -1)
        (prettify-symbols-mode 1)))))

(add-hook 'csharp-ts-mode-hook
          (lambda ()
            (run-with-timer 0.2 nil #'my-csharp-prettify-cleanup)))
```

C-sharp is supported by default in newer versions of Emacs through `csharp-tree-sitter-mode`.
Install the `dotnet` package as well to get some extra goodies.

```emacs-lisp
(package! dotnet)
```

**Step 2** - Install grammar files
Execute `M-x treesit-install-language-grammar` and enter `c-sharp`.  When prompted if you want to do it interactively, enter "Yes",
and paste the Github repository (`tree-sitter/tree-sitter-c-sharp`) for the grammar files.
Just keep pressing `<enter>` to accept defaults for the rest of the process.


#### LSP Dotnet {#lsp-dotnet}

```emacs-lisp
(add-hook! 'csharp-ts-mode-hook #'lsp)
```

```emacs-lisp
(use-package! dotnet
  :hook ((csharp-ts-mode . dotnet-mode))
  :config
  (setq dotnet-project-search-max-depth 10) ; Search up to 10 directories for project files
  (add-hook 'dotnet-mode-hook
            (lambda ()
              (setq-local dotnet-project-directory
                         (or (locate-dominating-file default-directory "*.sln")
                             (locate-dominating-file default-directory "*.csproj")
                             (locate-dominating-file default-directory "*.fsproj")
                             default-directory))))
)

(add-to-list 'auto-mode-alist
             '("\\.csproj\\'" . (lambda () (csproj-mode))))
```


#### DAP {#dap}

To make DAP work nicely we need to install the netcoredbg. It's supposed to be able to
install automatically using `M-x dap-netcore-update-debugger`, but it's not working correctly for me.
The correct version can be downloaded from <https://github.com/Samsung/netcoredbg/releases>,
and put in `~/.config/emacs/.local/cache/.cache/lsp/netcoredbg`.

```emacs-lisp
(after! (dotnet dap-mode)
  (require 'dap-netcore)
  (setq dap-netcore-install-dir (f-join user-emacs-directory ".cache" "lsp")))
```


#### Keybindings {#keybindings}

Add the `sharper-main-transient` menu to local leader.

```emacs-lisp
;; (map! :map csharp-ts-mode-map
;;       :after csharp-ts-mode
;;       :localleader
;;       :desc "Sharper" "s" #'sharper-main-transient)
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


#### LSP Rust {#lsp-rust}

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


### Golang {#golang}

<!--list-separator-->

-  Installing Go

    I'm using [ASDF](https://asdf-vm.com/guide/getting-started.html) for managing various programming languages.

    ```shell
    $ asdf install golang latest
    $ asdf global golang latest
    ```

    Next, there's a bunch of dependencies we need to install in order to have a smooth experience.

    > Make sure the `GOPATH` environment variable is properly set!

    -   `gocode` for code completion and `eldoc` support
    -   `godoc` for documentation lookup
    -   `gorename` for extra refactoring commands
    -   `guru` for code navigation and refactoring commands
    -   `gore` REPL
    -   `goimports` optional auto-formatting on saving files and fixing imports
    -   `gotest` for generating test code
    -   `gomodifytags` for manipulating tags
    -   `gopls` language server
    -   `gotestsum` a friendly test runner
    -   `govulncheck` finds known vulnerabilities in project dependencies

    <!--listend-->

    ```shell
    $ go install github.com/x-motemen/gore/cmd/gore@latest
    $ go install github.com/stamblerre/gocode@latest
    $ go install golang.org/x/tools/cmd/godoc@latest
    $ go install golang.org/x/tools/cmd/goimports@latest
    $ go install golang.org/x/tools/cmd/gorename@latest
    $ go install golang.org/x/tools/cmd/guru@latest
    $ go install github.com/cweill/gotests/gotests@latest
    $ go install github.com/fatih/gomodifytags@latest
    $ go install golang.org/x/tools/gopls@latest
    $ go install gotest.tools/gotestsum@latest
    $ go install golang.org/x/vuln/cmd/govulncheck@latest
    ```

    **Security checker**

    The `gosec` tool inspects source code for security problems by scanning the Go AST.

    ```shell
    $ curl -sfL https://raw.githubusercontent.com/securego/gosec/master/install.sh | sh -s -- -b $(go env GOPATH)/bin
    ```

    **Linting**

    Finally, install `golangci-lint` for `flycheck` integration. It is recommended to install pre-built binaries.

    ```shell
    $ curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s -- -b $(go env GOPATH)/bin v1.54.2
    ```

<!--list-separator-->

-  Configuration

    When using the new `go-ts-mode` instead of `go-mode`, we loose some functionality that probably will
    be fixed in upstreams Doom config soon.

    ```emacs-lisp
    (defun lsp-go-install-save-hooks ()
      "Set up some before-save hooks to format buffer and add/delete imports"
      (add-hook! 'before-save-hook #'lsp-format-buffer t t)
      (add-hook! 'before-save-hook #'lsp-organize-imports t t))
    ```

    Add the `go-ts-mode` to `major-mode-remap-alist` so that we use the tree-sitter mode when coding Go.

    ```emacs-lisp
    (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
    ```

    ```emacs-lisp
    ;; (use-package! go-ts-mode
    ;;   :config
    ;;   (add-hook! 'go-ts-mode-hook #'lsp!)
    ;;   (add-hook! 'go-ts-mode-hook #'lsp-go-install-save-hooks))
    ```

    ```emacs-lisp
    ;; (use-package emacs
    ;;   :ensure nil
    ;;   :config
    ;;   (setq major-mode-remap-alist
    ;;   '((go-mode . go-ts-mode))))
    ```

    ```emacs-lisp
    (after! (go flycheck lsp-ui)
      (flycheck-golangci-lint-setup))
    ```

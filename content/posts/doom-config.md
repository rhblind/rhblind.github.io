+++
title = "Doom Emacs Configuration"
author = ["Rolf Håvard Blindheim"]
lastmod = 2026-05-20T01:10:31+02:00
tags = ["org-mode"]
categories = ["emacs"]
draft = false
toc = true
aliases = "/posts/doom-emacs-configuration"
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
      auto-revert-use-notify              t         ; Use file notifications for instant updates
      auto-revert-interval                1         ; Fallback poll interval (seconds)
      auto-revert-avoid-polling           t         ; Prefer notifications over polling
      revert-without-query                '(".*")   ; Never prompt, revert all files silently
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
      window-divider-default-right-width  3         ; Thicker window dividers.
      x-stretch-cursor                    t         ; Stretch cursor to the glyph width.
      )
```

Here are some modes I always want active.

```emacs-lisp
(diff-hl-flydiff-mode                1)       ; This makes `diff-hl' updated asynchronous
(display-time-mode                   1)       ; I want to know what time it is
(global-auto-revert-mode             1)       ; Auto-revert buffers when files change on disk
(drag-stuff-global-mode              1)       ; Drag text around
(global-treesit-fold-mode            1)       ; Enable tree-sitter based code folding
(global-treesit-fold-indicators-mode 1)       ; Show fold indicators in the fringe
(global-goto-address-mode            1)       ; A minor mode to render urls and like as links
(global-subword-mode                 1)       ; Iterate through CamelCase words - Not sure how I like this
(smartparens-global-mode             1)       ; Always enable smartparens
(ws-butler-global-mode               1)       ; Unobtrusive way to trim spaces on end of lines
(window-divider-mode                 1)       ; Show window dividers

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
        epg-pinentry-mode        'loopback                      ; Use Emacs pinentry for passphrase prompts.
        epg-gpg-program (cond ((eq system-type 'darwin)     "/opt/homebrew/bin/gpg")
                              ((eq system-type 'gnu/linux)  "/usr/bin/gpg")
                              ((eq system-type 'windows-nt) "C:/Program Files (x86)/GNU/GnuPG/gpg2")))

  (pinentry-start)
  ;; (load-file (expand-file-name "secrets.el.gpg" doom-private-dir)) ; This cause weird errors
  )
```

I'm using `gpg-agent` instead of `ssh-agent` so we need to connect here.
Update the TTY at startup and before GPG operations to avoid pinentry issues.

```emacs-lisp
(defun cust/gpg-update-tty ()
  "Update GPG agent's TTY information."
  (shell-command "gpg-connect-agent updatestartuptty /bye >/dev/null"))

;; Update at startup
(cust/gpg-update-tty)

;; Update before magit commit to ensure pinentry works
(advice-add 'magit-commit-create :before (lambda (&rest _) (cust/gpg-update-tty)))
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

When connecting via `emacsclient`, resume the last active workspace instead of creating a new empty one.
Doom's default `persp-emacsclient-init-frame-behaviour-override` runs `+workspaces-associate-frame-fn` which
creates a fresh workspace per connection. Setting it to `t` instead uses `set-default-persp` which, combined
with `persp-set-last-persp-for-new-frames`, resumes the last active workspace.

Important: `persp-init-frame-behaviour` must stay `t` (Doom's default). It is checked inside `persp-activate`
on every workspace switch - setting it to `nil` prevents `persp-restore-window-conf` from running, so the
last buffer is never shown when switching workspaces.

```emacs-lisp
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override t
        persp-init-frame-behaviour t))
```

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

Erase content of current buffer with `SPC-b e`.

```emacs-lisp
(map! :leader
      (:prefix-map ("b" . "buffer")
       :desc "Erase buffer" "e" #'erase-buffer))
```

Copy the file path of the current buffer to clipboard with `SPC-b Y`.

```emacs-lisp
(defun copy-buffer-filepath-to-clipboard ()
  "Copy the filepath of the current buffer to the clipboard."
  (interactive)
  (if-let ((filepath (buffer-file-name)))
      (progn
        (kill-new filepath)
        (message "Copied: %s" filepath))
    (message "Buffer is not visiting a file")))

(map! :leader
      (:prefix-map ("b" . "buffer")
       :desc "Copy filepath with line" "Y" #'copy-buffer-filepath-point-to-clipboard))
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

Use `SPC-!` to list all flycheck errors in the current buffer.

```emacs-lisp
(defun cust/flycheck-list-errors-and-focus ()
  "List flycheck errors and focus the error list window."
  (interactive)
  (flycheck-list-errors)
  (when-let ((window (get-buffer-window flycheck-error-list-buffer)))
    (select-window window)))

(map! :leader :desc "List flycheck errors" "!" #'cust/flycheck-list-errors-and-focus)
```

Disable proselint checker (CLI changed in 0.16.0, now requires subcommands).

```emacs-lisp
(after! flycheck
  (setq-default flycheck-disabled-checkers '(proselint)))
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
  (when (derived-mode-p 'prog-mode 'text-mode)
    (evil-define-key '(normal visual) (current-local-map)
      (kbd "C-n") #'+evil/next-beginning-of-method
      (kbd "C-p") #'+evil/previous-beginning-of-method
      (kbd "C-S-n") #'cust/visual-next-end-of-method
      (kbd "C-S-p") #'cust/visual-previous-beginning-of-method)))

(add-hook 'evil-local-mode-hook #'cust/prog-mode-defun-navigation)
```


### Doom configuration {#doom-configuration}

In this section we generate the `init.el` and `early-init.el` files.


#### Early Init {#early-init}

Emacs 27+ introduces early-init.el, which is run before init.el, before package and UI initialization happens, and before site files are loaded. This is perfect for startup performance optimizations.

```emacs-lisp
;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs 27+ introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens, and before site files are loaded.

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Temporarily disable file-name-handler-alist for startup performance
(defvar doom--initial-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default
(setq frame-inhibit-implied-resize t)

;; Disable native-comp warnings popup
(setq native-comp-async-report-warnings-errors nil)

;; Prevent native-comp from JIT compiling during startup
(setq native-comp-deferred-compilation nil
      native-comp-jit-compilation nil)

;; Re-enable JIT after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq native-comp-deferred-compilation t
                  native-comp-jit-compilation t)))

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned)
(advice-add #'x-apply-session-resources :override #'ignore)

;; Prevent stray instances of package.el from polluting our startup
(setq package-enable-at-startup nil)

;; Faster to disable these here (before they've been initialized)
(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil)

;; Increase the amount of data Emacs reads from processes
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

;; Optimize for modern machines with more memory
(setq large-file-warning-threshold 100000000) ;; 100MB

;; Don't compact font caches during GC
(setq inhibit-compacting-font-caches t)

;; Suppress warnings during native compilation
(when (boundp 'native-comp-eln-load-path)
  (setq native-comp-async-report-warnings-errors 'silent))

;; Disable site-start.el from loading
(setq site-run-file nil)

;; No need for titlebar on macOS
(when (eq system-type 'darwin)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
```


#### Init File Header {#init-file-header}

```emacs-lisp
;;; init.el -*- lexical-binding: t; -*-
```


#### Sneaky garbage collection {#sneaky-garbage-collection}

The initial GC threshold is set in early-init.el. Here we configure the garbage collection mode hook (gcmh) to adopt a sneaky garbage collection strategy of waiting until idle time to collect, staving off the collector while working.

```emacs-lisp
(add-hook 'emacs-startup-hook #'(lambda ()
                                  (setq gcmh-idle-delay 5                           ;; Wait 5 seconds idle before GC
                                        gcmh-high-cons-threshold (* 100 1024 1024)  ;; 100mb
                                        gcmh-verbose nil)))
```


#### Load prefer newer {#load-prefer-newer}

Sometimes it's necessary to fix a bug, or otherwise change stuff in installed local packages.
Always load the newest version of a file.

```emacs-lisp
(setq load-prefer-newer t)
```


#### Native Compilation {#native-compilation}

Configure native compilation for better performance with Emacs 28+.

```emacs-lisp
;; Native compilation settings for performance
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  ;; Compile packages asynchronously after installation
  (setq package-native-compile t)

  ;; Silence native-comp warnings
  (setq native-comp-async-report-warnings-errors 'silent)

  ;; Set native compilation speed (0-3, higher is faster but uses more resources)
  (setq native-comp-speed 2)

  ;; Compile ahead of time for better startup performance
  (setq native-comp-deferred-compilation t
        native-comp-jit-compilation t))
```


#### File Loading Optimizations {#file-loading-optimizations}

Optimize file loading performance by deferring fontification, reducing syntax highlighting overhead, and lazy-loading modeline updates.

```emacs-lisp
;; Defer jit-lock fontification for smoother file opening
(setq jit-lock-defer-time 0.05)

;; Reduce font-lock decoration level (2 = medium, t = maximum)
(setq font-lock-maximum-decoration '((t . 2)))

;; Disable global emojify-mode (significant CPU overhead during fontification)
(after! emojify
  (global-emojify-mode -1))

;; Defer modeline updates to idle time instead of immediate on file open
(defvar cust/pending-modeline-update nil)

(defun cust/deferred-modeline-update ()
  "Run modeline updates after idle time."
  (when cust/pending-modeline-update
    (setq cust/pending-modeline-update nil)
    (when (fboundp 'doom-modeline-update-buffer-file-name)
      (doom-modeline-update-buffer-file-name))
    (when (fboundp 'doom-modeline-update-buffer-file-icon)
      (doom-modeline-update-buffer-file-icon))
    (when (fboundp 'doom-modeline-update-vcs)
      (doom-modeline-update-vcs))))

(defun cust/schedule-modeline-update ()
  "Schedule modeline update for idle time."
  (setq cust/pending-modeline-update t)
  (run-with-idle-timer 0.5 nil #'cust/deferred-modeline-update))

;; Replace immediate modeline hooks with deferred version
(after! doom-modeline
  (remove-hook 'find-file-hook #'doom-modeline-update-buffer-file-name)
  (remove-hook 'find-file-hook #'doom-modeline-update-buffer-file-icon)
  (remove-hook 'find-file-hook #'doom-modeline-update-vcs)
  (add-hook 'find-file-hook #'cust/schedule-modeline-update))
```

Defer `undo-fu-session` recovery to idle time. The package saves and restores undo history across sessions,
but it loads synchronously on `find-file-hook`, reading from disk and computing a SHA1 checksum of the buffer.
This is expensive for large files and adds noticeable delay.

```emacs-lisp
(after! undo-fu-session
  ;; Remove the synchronous hook
  (remove-hook 'find-file-hook #'undo-fu-session--recover-safe)

  (defun +undo-fu-session--recover-in-buffer (buf)
    "Recover undo-fu-session in BUF if it's still live."
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (undo-fu-session--recover-safe))))

  ;; Add a deferred version that runs when Emacs is idle
  (defun +undo-fu-session--recover-deferred ()
    "Defer undo-fu-session recovery to idle time for faster file loading."
    (run-with-idle-timer 0.5 nil #'+undo-fu-session--recover-in-buffer (current-buffer)))

  (add-hook 'find-file-hook #'+undo-fu-session--recover-deferred))
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

    In some cases we want to install the latest and greatest version of a package.
    Doom allows us to unpin packages using the `unpin` macro.

    ```emacs-lisp
    (unpin! (:tools lsp magit forge transient))
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
    ;;company           ; the ultimate code completion backend
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
    (treemacs +lsp)     ; a project drawer, like neotree but cooler
    ;;unicode           ; extended unicode support for various languages
    (vc-gutter +pretty) ; vcs diff in the fringe
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
    eww                 ; the internet is gross
    (ibuffer +icons)    ; interactive buffer management
    undo                ; persistent, smarter undo for your inevitable mistakes
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
    (spell
     +aspell +flyspell) ; tasing you for misspelling mispelling
    grammar             ; tasing grammar mistake every you make
    ```

    <a id="code-snippet--doom-tools"></a>
    ```emacs-lisp
    ;;ansible
    ;;biblio            ; Writes a PhD for you (citation needed)
    debugger
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
    ;;terraform         ; infrastructure as code
    tmux                ; an API for interacting with tmux
    tree-sitter         ; syntax and parsing, sitting in a tree...
    ;;upload            ; map local to remote projects via ssh/ftp
    ```

    <a id="code-snippet--doom-os"></a>
    ```emacs-lisp
    (:if (featurep :system 'macos) macos) ; improve compatibility with macOS
    tty                                   ; improve the terminal Emacs experience
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
    (elixir
     +lsp
     +tree-sitter)      ; erlang done right
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
     +lsp)              ; the hipster dialect
    ;;(haskell +lsp)    ; a language that's lazier than I am
    ;;hy                ; readability of scheme w/ speed of python
    ;;idris             ; a language you can depend on
    json                ; At least it ain't XML
    ;;(java +lsp)       ; the poster child for carpal tunnel syndrome
    (javascript
     +lsp)              ; all(hope(abandon(ye(who(enter(here))))))
    ;;julia             ; a better, faster MATLAB
    ;;kotlin            ; a better, slicker Java(Script)
    ;;latex             ; writing papers in Emacs has never been so fun
    ;;lean              ; for folks with too much to prove
    ;;ledger            ; be audit you can be
    lua                 ; one-based indices? one-based indices
    (markdown
     +grip)             ; writing docs for people to ignore
    ;;nim               ; python + lisp at the speed of c
    ;;nix               ; I hereby declare "nix geht mehr!"
    ;;ocaml             ; an objective camel
    (org                ; organize your plain life in plain text
     +pretty            ; yessss my pretties! (nice unicode symbols)
     +dragndrop         ; drag & drop files/images into org buffers
     +hugo              ; use Emacs for hugo blogging
     +noter             ; enhanced PDF notetaking
     ;; +jupyter        ; ipython/jupyter support for babel
     +pandoc            ; export-with-pandoc support
     +gnuplot           ; who doesn't like pretty pictures
     ;;+pomodoro        ; be fruitful with the tomato technique
     +present           ; using org-mode for presentations
     +roam)             ; wander around notes
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
    (rust
     +lsp)              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
    ;;scala             ; java, but good
    ;;(scheme +guile)   ; a fully conniving family of lisps
    (sh
     +powershell
     +lsp)              ; she sells {ba,z,fi}sh shells on the C xor
    ;;sml
    ;;solidity          ; do you need a blockchain? No.
    ;;swift             ; who asked for emoji variables?
    ;;terra             ; Earth and Moon in alignment for performance.
    (web
     +lsp)              ; the tubes
    (yaml               ; JSON, but readable
     +lsp-)
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


#### Terminal settings {#terminal-settings}

Disable the menu bar when running in terminal mode. The `default-frame-alist` setting of `menu-bar-lines . 0`
only applies to GUI frames, so we need an explicit hook for terminal frames.

```emacs-lisp
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (unless (display-graphic-p frame)
              (menu-bar-mode -1))))
(unless (display-graphic-p)
  (menu-bar-mode -1))
```


#### Terminal ligatures {#terminal-ligatures}

In terminal frames, `ligature.el` (used by Doom's `(ligatures +extra)` module) has no effect because it relies on HarfBuzz font shaping, which only runs in GUI frames. As a fallback, `prettify-symbols-mode` substitutes common programming sequences with standard Unicode characters, which any terminal font can render.

```emacs-lisp
(defvar +my/terminal-ligatures
  '(;; Arrows
    ("->"  . ?→)
    ("->>" . ?↠)
    ("=>"  . ?⇒)
    ("=>>" . ?⇉)
    ("<-"  . ?←)
    ("<->" . ?↔)
    ("<=>" . ?⟺)
    ;; Pipe
    ("|>"  . ?▷)
    ("<|"  . ?◁)
    ;; Comparison
    ("!="  . ?≠)
    ("!==" . ?≢)
    ("=="  . ?≡)
    ("===" . ?≣)
    (">="  . ?≥)
    ;; Logic / math
    ("&&"  . ?∧)
    ("||"  . ?∨)
    ("::"  . ?∷)
    ("..." . ?…)
    (".."  . ?‥)
    ;; Functional
    ("lambda" . ?λ))
  "Prettify-symbols substitutions using standard Unicode for terminal frames.")

;; Only apply prettify-symbols-mode ligatures in terminal frames.
;; In GUI frames, ligature.el handles this via HarfBuzz font shaping.
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (unless (display-graphic-p frame)
              (with-selected-frame frame
                (setq-default prettify-symbols-alist
                              (append +my/terminal-ligatures (default-value 'prettify-symbols-alist)))))))

;; Apply for initial frame if it's a terminal
(unless (display-graphic-p)
  (setq-default prettify-symbols-alist
                (append +my/terminal-ligatures (default-value 'prettify-symbols-alist))))
```


#### Theme and modeline {#theme-and-modeline}

```emacs-lisp
(package! ef-themes)
```

I currently use two themes - a light theme for usual work, and a dark theme for late night hacking sessions.
These days I'm using the `doom-tomorrow-day` light theme, and `doom-nord-aurora` dark theme. To easily cycle
between them, I keep my favorite themes in a  `doom-cycle-themes` list, and have a small function that just applies
the next theme in the list.

```emacs-lisp
(setq dark-mode-theme 'doom-nord
      light-mode-theme 'doom-tomorrow-day)


(setq doom-cycle-themes (list dark-mode-theme light-mode-theme))
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

(defun splash-phrase-set-random-set ()
  "Set a new random splash phrase set."
  (interactive)
  (setq splash-phrase-set
        (nth (random (1- (length splash-phrase-sources)))
             (cl-set-difference (mapcar #'car splash-phrase-sources) (list splash-phrase-set))))
  (+doom-dashboard-reload t))

(defvar splash-phrase--cache nil)

(defun splash-phrase-get-from-file (file)
  "Fetch a random line from FILE."
  (let ((lines (or (cdr (assoc file splash-phrase--cache))
                   (cdar (push (cons file
                                     (with-temp-buffer
                                       (insert-file-contents (expand-file-name file splash-phrase-source-folder))
                                       (split-string (string-trim (buffer-string)) "\n")))
                               splash-phrase--cache)))))
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

-  copy-buffer-filepath-point-to-clipboard

    This is a small function to copy the current buffer and point location to the clipboard.
    Supports visual selection ranges in the format: filepath:line or filepath:startLine-endLine

    ```emacs-lisp
    (defun copy-buffer-filepath-point-to-clipboard ()
      "Copy the current buffer's filepath with line or line range to clipboard.
    Format: filepath:line or filepath:startLine-endLine (if region is active)"
      (interactive)
      (if-let ((filepath (buffer-file-name)))
          (let* ((start-line (line-number-at-pos (if (region-active-p)
                                                      (region-beginning)
                                                    (point))))
                 (end-line (line-number-at-pos (if (region-active-p)
                                                    (region-end)
                                                  (point))))
                 (filepath-with-point (if (= start-line end-line)
                                          (format "%s:%d" filepath start-line)
                                        (format "%s:%d-%d" filepath start-line end-line))))
            (kill-new filepath-with-point)
            (message "Copied: %s" filepath-with-point))
        (message "Buffer is not visiting a file")))
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


#### Packages from local repositories {#packages-from-local-repositories}

If we have local packages in the `.elisp/packages/` folder in our `doom-private-dir`, we can install them like this:

```emacs-lisp
;; To install a local package, use the :local-repo property.
;; The path is relative to doom-private-dir (i.e., ~/.doom.d/)
;;
;; Example for a package in elisp/packages/my-package/:
;; (package! my-package :recipe (:local-repo "elisp/packages/my-package"))
;;
;; For packages with multiple files or specific file patterns:
;; (package! my-package :recipe (:local-repo "elisp/packages/my-package" :files ("*.el")))
;;
;; For packages that also have build steps or additional files:
;; (package! my-package :recipe (:local-repo "elisp/packages/my-package" :files ("*.el" "resources/*")))
```

As an example, if you have a package called `my-cool-package` with its files in
`~/.doom.d/elisp/packages/my-cool-package/my-cool-package.el`, you would declare it as:

```emacs-lisp
(package! my-cool-package :recipe (:local-repo "elisp/packages/my-cool-package"))
```

After adding a new local package:

1.  Run `doom sync` to install and build the package
2.  Restart Emacs or run `doom/reload`


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
    (package! claude-code-ide :pin "56db02e"
      :recipe (:host github :repo "manzaltu/claude-code-ide.el"))
    ```

    ```emacs-lisp
    (use-package! claude-code-ide
      :defer t
      :commands (claude-code-ide-menu claude-code-ide-emacs-tools-setup)
      :init
      (map! :leader
            :desc "+Claude Code" "C" #'claude-code-ide-menu)
      :config
      (claude-code-ide-emacs-tools-setup)
      (setq claude-code-ide-terminal-backend  'vterm
            claude-code-ide-vterm-render-delay 0.001
            claude-code-ide-enable-mcp-server  t
            claude-code-ide-no-flicker         t
            claude-code-ide-vterm-anti-flicker t
            claude-code-ide-enable-mcp-server  nil))
    ```

<!--list-separator-->

-  Minuet - AI Code Completion

    Minuet provides inline code completions using any LLM backend (similar to GitHub Copilot).
    It syncs its provider configuration from gptel to avoid duplicate API key management.

    Based on [Mike Olson's Emacs AI Setup](https://mwolson.org/guides/emacs-ai-setup/).

    ```emacs-lisp
    ;; Disabled - too slow with Azure Foundry, using Copilot instead
    ;; (package! minuet
    ;;   :recipe (:host github :repo "milanglacier/minuet-ai.el"))
    ```

    ```emacs-lisp
    ;; Disabled - too slow with Azure Foundry, using Copilot instead
    ;; (use-package! minuet
    ;;   :hook ((prog-mode . minuet-auto-suggestion-mode)
    ;;          (org-mode . minuet-auto-suggestion-mode))
    ;;   :config
    ;;   ;; Fix bug in minuet where comment-start can be nil
    ;;   (defun cust/minuet--add-tab-comment-fixed ()
    ;;     "Add comment string for tab use into the prompt.
    ;; Fixed version that handles nil comment-start."
    ;;     (if-let* ((language-p (derived-mode-p 'prog-mode 'text-mode 'conf-mode))
    ;;               (comment-str (or comment-start "#"))
    ;;               (commentstring (format "%s %%s%s"
    ;;                                      (replace-regexp-in-string "^%" "%%" comment-str)
    ;;                                      (or comment-end ""))))
    ;;         (if indent-tabs-mode
    ;;             (format commentstring "indentation: use \t for a tab")
    ;;           (format commentstring (format "indentation: use %d spaces for a tab" tab-width)))
    ;;       ""))
    ;;   (advice-add 'minuet--add-tab-comment :override #'cust/minuet--add-tab-comment-fixed)
    ;;
    ;;   (defun cust/minuet--add-language-comment-fixed ()
    ;;     "Add comment string for language use into the prompt.
    ;; Fixed version that handles nil comment-start."
    ;;     (if-let* ((language-p (derived-mode-p 'prog-mode 'text-mode 'conf-mode))
    ;;               (mode (symbol-name major-mode))
    ;;               (mode (replace-regexp-in-string "-ts-mode" "" mode))
    ;;               (mode (replace-regexp-in-string "-mode" "" mode))
    ;;               (comment-str (or comment-start "#"))
    ;;               (commentstring (format "%s %%s%s"
    ;;                                      (replace-regexp-in-string "^%" "%%" comment-str)
    ;;                                      (or comment-end ""))))
    ;;         (format commentstring (concat "language: " mode))
    ;;       ""))
    ;;   (advice-add 'minuet--add-language-comment :override #'cust/minuet--add-language-comment-fixed)
    ;;
    ;;   ;; Use Claude provider with Azure Foundry endpoint
    ;;   (setq minuet-provider 'claude)
    ;;
    ;;   ;; Override only the necessary keys in minuet-claude-options
    ;;   (setq minuet-claude-options
    ;;         (plist-put minuet-claude-options :end-point
    ;;                    (format "https://%s/anthropic/v1/messages" authinfo-claude-host)))
    ;;   (setq minuet-claude-options
    ;;         (plist-put minuet-claude-options :api-key #'cust/azure-claude-api-key))
    ;;   (setq minuet-claude-options
    ;;         (plist-put minuet-claude-options :model "claude-haiku-4-5"))
    ;;   (setq minuet-claude-options
    ;;         (plist-put minuet-claude-options :max_tokens 128))
    ;;
    ;;   ;; Minuet settings - optimized for speed
    ;;   (setq minuet-n-completions 1
    ;;         minuet-auto-suggestion-debounce-delay 0.3  ; Blog recommends 0.3s
    ;;         minuet-auto-suggestion-throttle-delay 0.5
    ;;         minuet-add-single-line-entry nil
    ;;         minuet-context-window 4000       ; Smaller context = faster responses
    ;;         minuet-request-timeout 3)
    ;;
    ;;   ;; Make suggestions visible (gray italic ghost text)
    ;;   (set-face-attribute 'minuet-suggestion-face nil
    ;;                       :foreground "gray50"
    ;;                       :slant 'italic)
    ;;
    ;;   ;; Keybindings
    ;;   (map! :map minuet-active-mode-map
    ;;         "M-p" #'minuet-previous-suggestion
    ;;         "M-n" #'minuet-next-suggestion
    ;;         "<tab>" #'minuet-accept-suggestion
    ;;         "TAB" #'minuet-accept-suggestion
    ;;         "M-a" #'minuet-accept-suggestion-line
    ;;         "M-e" #'minuet-dismiss-suggestion))
    ```

<!--list-separator-->

-  Github Copilot

    Unofficial GitHub Copilot plugin for Emacs.
    Required to run `M-x copilot-login` for using the plugin.

    **Copilot code plugin**

    ```emacs-lisp
    (package! copilot
      :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
    ```

    ```emacs-lisp
    (use-package! copilot
      :hook ((prog-mode . copilot-mode)
             (org-mode . copilot-mode))
      :bind (:map copilot-completion-map
                  ("<tab>" . 'copilot-accept-completion)
                  ("TAB" . 'copilot-accept-completion)
                  ("C-TAB" . 'copilot-accept-completion-by-word)
                  ("C-<tab>" . 'copilot-accept-completion-by-word))
      :config
      (setq copilot-idle-delay 0.3)  ;; Show suggestions after 0.3s of idle time
      (add-to-list 'copilot-major-mode-alist '("elixir-ts-mode" . "elixir"))
      (add-to-list 'copilot-indentation-alist '(elixir-ts-mode elixir-ts-indent-offset))
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

    Pure Elisp implementation of an MCP server for Emacs.

    ```emacs-lisp
    (package! mcp-server :pin "1947617"
      :recipe (:host github :repo "rhblind/emacs-mcp-server"
               :files ("*.el" "tools/*.el" "mcp-wrapper.py" "mcp-wrapper.sh")))
    ```

    ```emacs-lisp
    ;; Start MCP server after 2 seconds of idle time to avoid blocking startup
    (run-with-idle-timer 2 nil #'mcp-server-start-unix)
    ```

    When developing this package, load the local version of the MCP server.
    Comment this section out when not developing.

    ```emacs-lisp
    (package! mcp-server :disable t)
    ```

    ```emacs-lisp
    (add-to-list 'load-path "~/workspace/emacs-mcp-server/.worktrees/org-tools")
    (require 'mcp-server)
    ```

    To hook up Claude Code to the MCP server, I use this command.

    ```shell
    $ claude mcp add emacs -- socat - UNIX-CONNECT:$HOME/.config/emacs/.local/cache/emacs-mcp-server.sock
    ```

    Things to fix:

    -   When asking for permission to do something (tool usage), it should use the agents UI instead of asking in the Emacs minibuffer.

<!--list-separator-->

-  GPTEL

    Shared configuration for LLM API access, used by `gptel`.

    Set `cust/gptel-backend-type` to switch between:

    -   `'copilot` - GitHub Copilot (requires `copilot-login`)
    -   `'claude-foundry` - Azure Foundry Claude API (work)
    -   `'claude-api-key` - Anthropic API key (console.anthropic.com)

    <!--listend-->

    ```emacs-lisp
    ;; Choose which gptel backend to use
    (defvar cust/gptel-backend-type 'copilot
      "Which gptel backend to use.
    Set to `copilot' for GitHub Copilot.
    Set to `claude-foundry' for Azure Foundry API (work).
    Set to `claude-api-key' for Anthropic API key (console.anthropic.com).")

    ;; Azure Foundry configuration
    ;; Add to ~/.authinfo.gpg:
    ;;   machine dadp-openai-us2-resource.openai.azure.com login apikey password YOUR_API_KEY
    (defvar cust/claude-foundry-host "dadp-openai-us2-resource.openai.azure.com")

    (defun cust/claude-foundry-api-key ()
      "Retrieve Claude API key from auth-source for Azure Foundry."
      (auth-source-pick-first-password :host cust/claude-foundry-host :user "apikey"))

    ;; Anthropic API key configuration (console.anthropic.com)
    ;; Add to ~/.authinfo.gpg:
    ;;   machine api.anthropic.com login apikey password YOUR_API_KEY
    (defvar cust/claude-api-host "api.anthropic.com")

    (defun cust/claude-api-key ()
      "Retrieve Claude API key from auth-source for Anthropic API."
      (auth-source-pick-first-password :host cust/claude-api-host :user "apikey"))

    ;; Configure gptel backend based on cust/gptel-backend-type
    (pcase cust/gptel-backend-type
      ('copilot
       ;; GitHub Copilot - requires M-x copilot-login first
       (setq gptel-backend (gptel-make-gh-copilot "Copilot")
             gptel-model 'claude-sonnet-4.5))
      ('claude-foundry
       ;; Azure Foundry uses the Anthropic Messages API format
       ;; Requires x-api-key header and anthropic-version header
       (setq gptel-backend (gptel-make-anthropic "Azure-Claude"
                             :host (concat cust/claude-foundry-host "/anthropic")
                             :endpoint "/v1/messages"
                             :stream t
                             :key #'cust/claude-foundry-api-key
                             :header (lambda ()
                                       (list (cons "x-api-key" (cust/claude-foundry-api-key))
                                             (cons "anthropic-version" "2023-06-01")))
                             :models '(claude-sonnet-4-5 claude-opus-4-5 claude-haiku-4-5))
             gptel-model 'claude-sonnet-4-5))
      ('claude-api-key
       ;; Standard Anthropic API (console.anthropic.com)
       (setq gptel-backend (gptel-make-anthropic "Claude"
                             :stream t
                             :key #'cust/claude-api-key
                             :models '(claude-sonnet-4-5 claude-opus-4-5 claude-haiku-4-5))
             gptel-model 'claude-sonnet-4-5)))
    ```

    Cancel any pending `gptel` commit message generation when navigating commit history with `M-p` / `M-n`.

    ```emacs-lisp
    (defvar cust/gptel-commit-cancelled nil
      "Non-nil when gptel commit generation was cancelled.")

    (defvar cust/gptel-magit-saved-callback nil
      "Stored callback for gptel-magit cancellation handling.")

    (defun cust/gptel-abort-before-commit-nav (&rest _)
      "Abort any pending gptel request before navigating commit history."
      (setq cust/gptel-commit-cancelled t)
      (when (fboundp 'gptel-abort)
        (ignore-errors (gptel-abort))))

    (defun cust/gptel-magit-callback-wrapper (message)
      "Callback wrapper that checks cancellation flag."
      (if cust/gptel-commit-cancelled
          (message "gptel commit message cancelled")
        (when cust/gptel-magit-saved-callback
          (funcall cust/gptel-magit-saved-callback message))))

    (defun cust/gptel-magit-generate-wrapper (orig-fn cb)
      "Wrap gptel-magit--generate to check cancellation flag."
      ;; Clear cancelled flag when starting new request
      (setq cust/gptel-commit-cancelled nil)
      (setq cust/gptel-magit-saved-callback cb)
      (funcall orig-fn #'cust/gptel-magit-callback-wrapper))

    (after! git-commit
      (advice-add 'git-commit-prev-message :before #'cust/gptel-abort-before-commit-nav)
      (advice-add 'git-commit-next-message :before #'cust/gptel-abort-before-commit-nav))

    (after! gptel-magit
      (advice-add 'gptel-magit--generate :around #'cust/gptel-magit-generate-wrapper)

      ;; Customize the commit prompt to prevent markdown formatting
      (setq gptel-magit-commit-prompt
            (string-join
             (list gptel-magit-prompt-conventional-commits
                   ""
                   "IMPORTANT:"
                   "- Commit message MUST follow Conventional Commits specification, with release-please compatible conventions for releases."
                   "- Return ONLY the raw commit message text"
                   "- The summary should never exceed 80 characters, unless absolutely necessary for clarity"
                   "- If including a body, ALWAYS separate it from the summary with a blank line"
                   "- Do NOT wrap the output in markdown code blocks, backticks, or any other formatting"
                   "- NEVER add triple backticks (```) before or after the commit message!")
             "\n")))
    ```

<!--list-separator-->

-  Agent Shell

    Agent Shell provides a native Emacs user interface for interacting with AI agents over Agent Client Protocol (ACP).

    ```emacs-lisp
    (package! shell-maker :recipe (:host github :repo "xenodium/shell-maker")
      :pin "6eafe72")
    (package! acp)
    (package! agent-shell)
    ```

    ```emacs-lisp
    (require 'acp)
    (require 'agent-shell)
    ```

    <!--list-separator-->

    -  Claude Code integration

        For Claude Code integration we need to install an ACP adapter. The Zed adapter works well:

        ```shell
        npm install -g @zed-industries/claude-code-acp
        ```

        ```emacs-lisp
        (setq agent-shell-anthropic-authentication (agent-shell-anthropic-make-authentication :login t)
              agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config)
              agent-shell-file-completion-enabled t)
        ```

    <!--list-separator-->

    -  Keybindings

        Custom keybindings for `agent-shell` buffers. We override some default behaviors
        to better integrate with Evil mode and general Emacs workflows.

        ```emacs-lisp
        ;; Multi-line input: M-<return> inserts a newline instead of submitting
        (map! :map shell-maker-mode-map
              :i "M-<return>" #'newline
              :n "M-<return>" #'newline)

        (map! :map agent-shell-mode-map
              ;; C-<tab> cycles session mode (plan/code/etc) - overrides aya-expand
              :i "C-<tab>" #'agent-shell-cycle-session-mode
              :n "C-<tab>" #'agent-shell-cycle-session-mode
              ;; C-g interrupts without confirmation prompt
              :i "C-g" (cmd! (agent-shell-interrupt t))
              :n "C-g" (cmd! (agent-shell-interrupt t))
              ;; Disable arrow keys for history (use M-p/M-n instead)
              :i "<up>" nil
              :i "<down>" nil
              ;; TAB activates/toggles expandable items (like RET does)
              :n "TAB" #'cust/agent-shell--activate-item-at-point
              :n "<tab>" #'cust/agent-shell--activate-item-at-point
              ;; C-c C-v pastes image from clipboard (requires pngpaste)
              :i "C-c C-v" #'cust/agent-shell--send-clipboard-image
              :n "C-c C-v" #'cust/agent-shell--send-clipboard-image
              ;; C-n/C-p navigates between items in normal mode
              :n "C-n" #'agent-shell-next-item
              :n "C-p" #'agent-shell-previous-item)
        ```

    <!--list-separator-->

    -  Custom commands

        Helper functions for features not provided by `agent-shell` out of the box.

        <!--list-separator-->

        -  TAB to activate items

            `agent-shell` makes certain items (expandable sections, buttons) interactive via
            text properties. This function finds and invokes the RET binding at point,
            allowing TAB to toggle/expand items just like pressing RET.

            ```emacs-lisp
            (defun cust/agent-shell--activate-item-at-point ()
              "Activate the item at point by invoking its RET binding."
              (interactive)
              (let ((fn nil)
                    (pos (point)))
                ;; Check point and a few chars ahead (for icons like ▶/▼)
                (while (and (not fn) (<= pos (+ (point) 3)))
                  (when-let ((km (get-text-property pos 'keymap)))
                    (setq fn (lookup-key km (kbd "RET"))))
                  (setq pos (1+ pos)))
                (when fn (funcall fn))))
            ```

        <!--list-separator-->

        -  Paste image from clipboard

            Insert an image from the system clipboard into the prompt. The image is saved
            to `.agent-shell/screenshots/` in the project root, and the file path is
            inserted at point. Requires `pngpaste` (install via `brew install pngpaste`).

            ```emacs-lisp
            (defun cust/agent-shell--send-clipboard-image ()
              "Paste image from clipboard and insert it into `agent-shell'."
              (interactive)
              (let* ((project-root (or (projectile-project-root) default-directory))
                     (screenshots-dir (expand-file-name ".agent-shell/screenshots" project-root))
                     (timestamp (format-time-string "%Y%m%d_%H%M%S"))
                     (filename (expand-file-name (format "clipboard_%s.png" timestamp) screenshots-dir)))
                (make-directory screenshots-dir t)
                (if (zerop (call-process "pngpaste" nil nil nil filename))
                    (progn
                      (goto-char (point-max))
                      (insert filename)
                      (message "Inserted clipboard image: %s" filename))
                  (user-error "No image in clipboard (or pngpaste not installed)"))))
            ```

    <!--list-separator-->

    -  Navigation behavior fix

        I use `C-n=/=C-p` in Evil normal mode to navigate between items. This advice
        ensures navigation always works, even when the cursor is at the prompt.

        ```emacs-lisp
        (defun cust/agent-shell--force-navigate-in-normal-state (orig-fn &rest args)
          "Advice to skip self-insert behavior in evil normal state."
          (if (and (bound-and-true-p evil-local-mode)
                   (eq evil-state 'normal))
              (cl-letf (((symbol-function 'shell-maker-point-at-last-prompt-p) #'ignore))
                (apply orig-fn args))
            (apply orig-fn args)))

        (advice-add 'agent-shell-next-item :around #'cust/agent-shell--force-navigate-in-normal-state)
        (advice-add 'agent-shell-previous-item :around #'cust/agent-shell--force-navigate-in-normal-state)
        ```

    <!--list-separator-->

    -  Keyword trigger highlighting

        Claude Code supports different "thinking" levels that allocate more tokens for
        complex reasoning. These keywords are highlighted with rainbow colors to make
        them visually distinct when typing prompts.

        -   **ultrathink** (~32k tokens): "ultrathink", "think harder", "think intensely", etc.
        -   **megathink** (~10k tokens): "megathink", "think deeply", "think hard", etc.
        -   **think** (~4k tokens): "think"

        <!--listend-->

        ```emacs-lisp
        (defface agent-shell-ultrathink-face
          '((t :foreground "#ff79c6" :weight bold :inherit rainbow-delimiters-depth-1-face))
          "Face for ultrathink-level trigger words.")

        (defface agent-shell-megathink-face
          '((t :foreground "#8be9fd" :weight bold :inherit rainbow-delimiters-depth-2-face))
          "Face for megathink-level trigger words.")

        (defface agent-shell-think-face
          '((t :foreground "#50fa7b" :weight bold :inherit rainbow-delimiters-depth-3-face))
          "Face for think-level trigger words.")

        (defvar agent-shell-thinking-keywords
          `((,(regexp-opt '("ultrathink" "think harder" "think intensely" "think longer"
                            "think really hard" "think super hard" "think very hard")
                          'words)
             . 'agent-shell-ultrathink-face)
            (,(regexp-opt '("megathink" "think about it" "think a lot" "think deeply"
                            "think hard" "think more")
                          'words)
             . 'agent-shell-megathink-face)
            (,(concat "\\<think\\>")
             . 'agent-shell-think-face))
          "Font-lock keywords for Claude Code thinking triggers.")

        (add-hook 'agent-shell-mode-hook
                  (lambda ()
                    (when (eq 'claude-code (map-elt (map-elt agent-shell--state :agent-config) :identifier))
                      (font-lock-add-keywords nil agent-shell-thinking-keywords))))
        ```

    <!--list-separator-->

    -  Slash command and file completion fixes

        Improvements to `@` file references and `/` slash command completion:

        -   `/` commands only trigger at the start of the prompt (they're commands, not mid-sentence)
        -   `@` file references can appear anywhere (for referencing files mid-sentence)
        -   Pre-populate common slash commands so they work before the agent handshake completes

        <!--listend-->

        ```emacs-lisp
        ;; Pre-populate core Claude Code slash commands for immediate completion
        (defvar cust/agent-shell--default-commands-claude
          '[((name . "compact") (description . "Clear conversation history but keep a summary in context"))
            ((name . "init") (description . "Initialize a new CLAUDE.md file with codebase documentation"))
            ((name . "resume") (description . "Resume a previous conversation"))
            ((name . "review") (description . "Review a pull request"))
            ((name . "pr-comments") (description . "Get comments from a GitHub pull request"))
            ((name . "security-review") (description . "Complete a security review of pending changes"))]
          "Core Claude Code slash commands for immediate completion.")

        (defun cust/agent-shell--prepopulate-commands-advice (result)
          "Pre-populate slash commands for Claude Code sessions."
          (when (and (eq 'claude-code (map-elt (map-elt result :agent-config) :identifier))
                     (null (map-elt result :available-commands)))
            (map-put! result :available-commands cust/agent-shell--default-commands-claude))
          result)

        (advice-add 'agent-shell--make-state :filter-return #'cust/agent-shell--prepopulate-commands-advice)

        (defun cust/agent-shell--at-input-start-p (trigger-pos)
          "Return non-nil if TRIGGER-POS is at the start of user input."
          (let ((input-start (comint-line-beginning-position)))
            (save-excursion
              (goto-char trigger-pos)
              (skip-chars-backward " \t")
              (<= (point) input-start))))

        (defun cust/agent-shell--file-completion-at-point-advice (orig-fn)
          "Allow @ file completion anywhere in the prompt."
          (when-let* ((at-pos (save-excursion
                                (skip-chars-backward "[:alnum:]/_.-")
                                (when (eq (char-before) ?@)
                                  (1- (point)))))
                      (start (1+ at-pos))
                      (end (point))
                      (files (agent-shell--project-files)))
            (list start end files
                  :exclusive 'no
                  :exit-function (lambda (_string _status) (insert " ")))))

        (defun cust/agent-shell--command-completion-at-point-advice (orig-fn)
          "Only complete / commands at prompt start."
          (when-let* ((slash-pos (save-excursion
                                   (skip-chars-backward "[:alnum:]_-")
                                   (when (eq (char-before) ?/)
                                     (1- (point)))))
                      (at-start (cust/agent-shell--at-input-start-p slash-pos))
                      (start (1+ slash-pos))
                      (end (point))
                      (cmds (map-elt agent-shell--state :available-commands))
                      (cmd-names (mapcar (lambda (cmd) (map-elt cmd 'name)) cmds)))
            (list start end cmd-names
                  :exclusive t
                  :exit-function (lambda (_string _status) (insert " ")))))

        (advice-add 'agent-shell--file-completion-at-point :around
                    #'cust/agent-shell--file-completion-at-point-advice)
        (advice-add 'agent-shell--command-completion-at-point :around
                    #'cust/agent-shell--command-completion-at-point-advice)
        ```

    <!--list-separator-->

    -  Resume previous conversations

        The `/resume` slash command allows resuming previous Claude Code conversations.
        Conversations are stored in `~/.claude/projects/<encoded-project-path>/` as JSONL
        files. Each file contains summary entries with conversation titles.

        This implementation provides:

        -   Resume conversations from the current project (default)
        -   Resume conversations from all projects (press `A` in the selection buffer)
        -   Show conversation summaries with timestamps for easy identification

        <!--listend-->

        ```emacs-lisp
        (defvar cust/agent-shell-claude-projects-dir
          (expand-file-name "projects" (expand-file-name ".claude" "~"))
          "Directory where Claude Code stores conversation files.")

        (defun cust/agent-shell--encode-project-path (path)
          "Encode PATH to Claude's project directory format.
        Claude encodes paths by replacing both / and . with -.
        Trailing slashes are removed first to match Claude Code's behavior."
          (let ((clean-path (directory-file-name path)))  ; Remove trailing slash
            (replace-regexp-in-string "[/.]" "-" clean-path)))

        (defun cust/agent-shell--get-project-dir-for-path (path)
          "Find the Claude project directory that matches PATH.
        Returns the directory name or nil if not found."
          (let ((encoded (cust/agent-shell--encode-project-path path))
                (dirs (directory-files cust/agent-shell-claude-projects-dir nil "^-")))
            ;; Try exact match first
            (or (cl-find encoded dirs :test #'string=)
                ;; Fallback: find a dir that could match (handle trailing slash variations)
                (cl-find-if (lambda (dir)
                              (or (string= dir encoded)
                                  (string= dir (concat encoded "-"))
                                  (string-prefix-p (concat encoded "-") dir)))
                            dirs))))

        (defun cust/agent-shell--extract-project-name (dir-name)
          "Extract a human-readable project name from DIR-NAME.
        Attempts to show a meaningful directory name by looking for patterns
        like 'workspace-X' or dotfiles like '--dirname'."
          (cond
           ;; Pattern: -Users-xxx-workspace-projectname -> projectname
           ((string-match "-workspace-\\(.+\\)$" dir-name)
            (match-string 1 dir-name))
           ;; Pattern: -Users-xxx--dotdir (dotfiles like .doom.d) -> .dotdir
           ((string-match "--\\([^-]+\\)\\(-[^-]+\\)?$" dir-name)
            (concat "." (match-string 1 dir-name)
                    (when (match-string 2 dir-name)
                      (match-string 2 dir-name))))
           ;; Fallback: use last segment
           ((string-match "-\\([^-]+\\)$" dir-name)
            (match-string 1 dir-name))
           (t dir-name)))

        (defun cust/agent-shell--parse-conversation-file (file)
          "Parse a conversation JSONL FILE and return conversation metadata.
        Returns a plist with :id, :summary, :timestamp, :message-count, :first-message, :git-branch, :cwd, and :file."
          (let ((summaries nil)
                (latest-timestamp nil)
                (message-count 0)
                (first-user-message nil)
                (git-branch nil)
                (cwd nil)
                (session-id (file-name-sans-extension (file-name-nondirectory file))))
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (while (not (eobp))
                (let* ((line (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position)))
                       (json (ignore-errors (json-parse-string line :object-type 'alist))))
                  (when json
                    (let ((type (alist-get 'type json))
                          (message (alist-get 'message json)))
                      ;; Extract cwd from first entry that has it
                      (unless cwd
                        (when-let ((dir (alist-get 'cwd json)))
                          (setq cwd dir)))
                      ;; Extract git branch from first entry that has it
                      (unless git-branch
                        (when-let ((branch (alist-get 'gitBranch json)))
                          (setq git-branch branch)))
                      (cond
                       ;; Collect summaries
                       ((string= type "summary")
                        (push (alist-get 'summary json) summaries))
                       ;; Count and capture first user message (skip meta/system messages)
                       ((and message
                             (string= (alist-get 'role message) "user")
                             (not (eq (alist-get 'isMeta json) t)))
                        (cl-incf message-count)
                        (unless first-user-message
                          (let* ((content (alist-get 'content message))
                                 (text (cond
                                        ((stringp content) content)
                                        ((vectorp content)
                                         (cl-loop for item across content
                                                  when (and (listp item)
                                                            (string= (alist-get 'type item) "text"))
                                                  return (alist-get 'text item)))
                                        (t nil))))
                            ;; Skip system-like messages
                            (when (and text
                                       (not (string-prefix-p "Unknown skill:" text))
                                       (not (string-prefix-p "<local-command" text))
                                       (not (string-match-p "^/[a-z]" text)))
                              (setq first-user-message text)))))
                       ;; Count assistant messages
                       ((and message (string= (alist-get 'role message) "assistant"))
                        (cl-incf message-count)))
                      ;; Track latest timestamp
                      (when-let ((ts (alist-get 'timestamp json)))
                        (when (or (null latest-timestamp)
                                  (string> ts latest-timestamp))
                          (setq latest-timestamp ts))))))
                (forward-line 1)))
            ;; Return metadata even without summaries
            (when (or summaries (> message-count 0))
              (list :id session-id
                    :summary (car (last summaries))
                    :summaries (nreverse summaries)
                    :timestamp latest-timestamp
                    :message-count message-count
                    :first-message first-user-message
                    :git-branch git-branch
                    :cwd cwd
                    :file file))))

        (defun cust/agent-shell--extract-conversation-messages (file)
          "Extract user/assistant messages from conversation JSONL FILE.
        Returns a list of message plists with :role and :content.
        Messages are returned in chronological order, suitable for replay."
          (let ((messages nil))
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (while (not (eobp))
                (let* ((line (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position)))
                       (json (ignore-errors (json-parse-string line :object-type 'alist))))
                  (when json
                    (let ((type (alist-get 'type json))
                          (message (alist-get 'message json))
                          (is-meta (alist-get 'isMeta json))
                          (is-sidechain (alist-get 'isSidechain json)))
                      ;; Only process user/assistant messages, skip meta and sidechain
                      ;; Note: JSON false becomes :false symbol (truthy!), true becomes t
                      (when (and message
                                 (not (eq is-meta t))
                                 (not (eq is-sidechain t))
                                 (member type '("user" "assistant")))
                        (let* ((role (alist-get 'role message))
                               (content (alist-get 'content message))
                               ;; Extract text from content array
                               (text (cond
                                      ((stringp content) content)
                                      ((vectorp content)
                                       (mapconcat
                                        (lambda (item)
                                          (when (and (listp item)
                                                     (string= (alist-get 'type item) "text"))
                                            (alist-get 'text item)))
                                        content ""))
                                      (t nil))))
                          ;; Skip empty messages and tool results
                          (when (and text (not (string-empty-p text)))
                            (push (list :role role :content text) messages)))))))
                (forward-line 1)))
            ;; Return in chronological order
            (nreverse messages)))

        (defun cust/agent-shell--format-history-for-context (messages &optional max-messages)
          "Format MESSAGES list into a context string for Claude.
        MAX-MESSAGES limits how many recent messages to include (default 20)."
          (let* ((max-msgs (or max-messages 20))
                 (recent-msgs (seq-take (reverse messages) max-msgs))
                 (msgs-to-format (reverse recent-msgs)))
            (mapconcat
             (lambda (msg)
               (let ((role (plist-get msg :role))
                     (content (plist-get msg :content)))
                 (format "[%s]: %s"
                         (if (string= role "user") "User" "Assistant")
                         ;; Truncate very long messages
                         (if (> (length content) 500)
                             (concat (substring content 0 497) "...")
                           content))))
             msgs-to-format
             "\n\n")))

        (defun cust/agent-shell--get-session-file (session-id)
          "Get the JSONL file path for SESSION-ID in the current project."
          (let* ((project-dir (cust/agent-shell--get-project-dir-for-path
                               (directory-file-name (or (projectile-project-root)
                                                        default-directory))))
                 (session-file (when project-dir
                                 (expand-file-name
                                  (concat session-id ".jsonl")
                                  (expand-file-name project-dir
                                                    cust/agent-shell-claude-projects-dir)))))
            (when (and session-file (file-exists-p session-file))
              session-file)))

        (defun cust/agent-shell--list-conversations (&optional project-path)
          "List all conversations, optionally filtered to PROJECT-PATH.
        Returns a list of conversation metadata plists sorted by timestamp (newest first)."
          (let ((conversations nil)
                (target-dir (when project-path
                              (cust/agent-shell--get-project-dir-for-path
                               (directory-file-name project-path))))
                (all-dirs (directory-files cust/agent-shell-claude-projects-dir nil "^-")))
            ;; Determine which directories to scan
            (let ((dirs (if target-dir
                            (list target-dir)
                          all-dirs)))
              (dolist (dir-name dirs)
                (let ((dir (expand-file-name dir-name cust/agent-shell-claude-projects-dir)))
                  (when (file-directory-p dir)
                    (dolist (file (directory-files dir t "\\.jsonl$"))
                      (when-let ((meta (cust/agent-shell--parse-conversation-file file)))
                        ;; Add project info - use directory name for display
                        (setq meta (plist-put meta :project-dir dir-name))
                        (setq meta (plist-put meta :project-name
                                              (cust/agent-shell--extract-project-name dir-name)))
                        (push meta conversations)))))))
            ;; Sort by timestamp, newest first
            (sort conversations
                  (lambda (a b)
                    (let ((ts-a (or (plist-get a :timestamp) ""))
                          (ts-b (or (plist-get b :timestamp) "")))
                      (string> ts-a ts-b))))))

        (defun cust/agent-shell--format-timestamp (iso-timestamp &optional include-date)
          "Format ISO-TIMESTAMP to a human-readable string.
        If INCLUDE-DATE is non-nil, include the date alongside relative time."
          (if (null iso-timestamp)
              "unknown"
            (let* ((time (date-to-time (replace-regexp-in-string "T" " "
                                         (replace-regexp-in-string "\\.[0-9]+Z$" "" iso-timestamp))))
                   (diff (float-time (time-subtract (current-time) time)))
                   (date-str (format-time-string "%Y-%m-%d %H:%M" time))
                   (relative (cond
                              ((< diff 60) "just now")
                              ((< diff 3600) (format "%d min ago" (/ diff 60)))
                              ((< diff 86400) (format "%d hours ago" (/ diff 3600)))
                              ((< diff 604800) (format "%d days ago" (/ diff 86400)))
                              (t nil))))
              (if include-date
                  (if relative
                      (format "%s (%s)" date-str relative)
                    date-str)
                (or relative date-str)))))

        (defun cust/agent-shell--resume-conversation (session-id &optional cwd session-file)
          "Resume a Claude Code conversation with SESSION-ID.
        CWD is the working directory for the session.
        SESSION-FILE is the path to the conversation JSONL file."
          ;; Start a new agent-shell with the resume session ID
          (cust/agent-shell-with-resume session-id cwd session-file))

        (defun cust/acp-make-session-resume-request (session-id cwd mcp-servers)
          "Create an ACP session/resume request to resume an existing session.
        SESSION-ID is the ID of the session to resume.
        CWD is the working directory.
        MCP-SERVERS is a list of MCP server configurations.

        Note: session/resume is an unstable ACP method that continues a conversation
        without replaying history. Claude Code advertises 'resume' capability, not 'load'."
          (list (cons 'method "session/resume")
                (cons 'params
                      (list (cons 'sessionId session-id)
                            (cons 'cwd cwd)
                            (cons 'mcpServers (or mcp-servers []))))))



        (defun cust/agent-shell--send-history-context (session-file buffer)
          "Load conversation history from SESSION-FILE and send as context prompt.
        BUFFER is the agent-shell buffer to send the prompt in."
          (when (and session-file (file-exists-p session-file))
            (let* ((messages (cust/agent-shell--extract-conversation-messages session-file))
                   (history-text (when messages
                                   (cust/agent-shell--format-history-for-context messages 30))))
              (when (and history-text (not (string-empty-p history-text)))
                ;; Send the history as a context-setting prompt
                (let ((context-prompt (format "I'm resuming our previous conversation. Here's the context from our last session:

        <previous-conversation>
        %s
        </previous-conversation>

        Please acknowledge that you have this context and are ready to continue. You don't need to summarize it - just confirm you're ready." history-text))
                      (target-buffer buffer))
                  ;; Use run-at-time to let the UI settle first
                  (run-at-time 0.5 nil
                               (lambda ()
                                 (when (buffer-live-p target-buffer)
                                   (with-current-buffer target-buffer
                                     (agent-shell--send-command
                                      :prompt context-prompt
                                      :shell (agent-shell--state)))))))))))

        (defun cust/agent-shell--initiate-session-with-resume (shell on-session-init resume-session-id)
          "Initiate ACP session resumption with SHELL, resuming RESUME-SESSION-ID.
        Uses the ACP session/resume method to continue the conversation.
        Must provide ON-SESSION-INIT (lambda ())."
          (unless on-session-init
            (error "Missing required argument: :on-session-init"))
          (unless resume-session-id
            (error "Missing required argument: resume-session-id"))
          ;; Capture the session file from pending state (set by cust/agent-shell-with-resume)
          (let ((session-file cust/agent-shell--pending-resume-session-file))
            ;; Clear the pending file now that we've captured it
            (setq cust/agent-shell--pending-resume-session-file nil)
            (with-current-buffer (map-elt (agent-shell--state) :buffer)
              (agent-shell--update-fragment
               :state (agent-shell--state)
               :block-id "starting"
               :body (format "\n\nResuming session %s..." (substring resume-session-id 0 8))
               :append t))
            (acp-send-request
             :client (map-elt (agent-shell--state) :client)
             :request (cust/acp-make-session-resume-request
                       resume-session-id
                       (agent-shell--resolve-path (agent-shell-cwd))
                       (agent-shell--mcp-servers))
             :buffer (current-buffer)
             :on-success (lambda (response)
                           ;; Store session info
                           (map-put! agent-shell--state
                                     :session (list (cons :id resume-session-id)
                                                    (cons :mode-id (map-nested-elt response '(modes currentModeId)))
                                                    (cons :modes (mapcar (lambda (mode)
                                                                           `((:id . ,(map-elt mode 'id))
                                                                             (:name . ,(map-elt mode 'name))
                                                                             (:description . ,(map-elt mode 'description))))
                                                                         (map-nested-elt response '(modes availableModes))))
                                                    (cons :model-id (map-nested-elt response '(models currentModelId)))
                                                    (cons :models (mapcar (lambda (model)
                                                                            `((:model-id . ,(map-elt model 'modelId))
                                                                              (:name . ,(map-elt model 'name))
                                                                              (:description . ,(map-elt model 'description))))
                                                                          (map-nested-elt response '(models availableModels))))))
                           (agent-shell--update-fragment
                            :state agent-shell--state
                            :block-id "starting"
                            :label-left (format "%s %s"
                                                (agent-shell--status-label "completed")
                                                (propertize "Resumed session" 'font-lock-face 'font-lock-doc-markup-face))
                            :body (if session-file
                                      "\n\nLoading conversation history..."
                                    "\n\nReady (no history file found)")
                            :append t)
                           (agent-shell--update-header-and-mode-line)
                           ;; Call the session init callback
                           (funcall on-session-init)
                           ;; Now send the conversation history as context (if we have a file)
                           (when session-file
                             (cust/agent-shell--send-history-context
                              session-file
                              (current-buffer))))
             :on-error (lambda (error)
                         (agent-shell--update-fragment
                          :state agent-shell--state
                          :block-id "starting"
                          :label-left (format "%s %s"
                                              (agent-shell--status-label "failed")
                                              (propertize "Session resume" 'font-lock-face 'font-lock-doc-markup-face))
                          :body (format "\n\nError: %s" (or (map-elt error 'message) error))
                          :append t)))))

        (defvar cust/agent-shell--pending-resume-session-id nil
          "Session ID to resume when initiating the next session.
        This is a global variable used during the brief window between
        starting agent-shell and session initialization.")

        (defvar cust/agent-shell--pending-resume-session-file nil
          "Session file path for the pending resume.
        Set alongside the session ID, cleared after use.")

        (defun cust/agent-shell-with-resume (session-id &optional cwd session-file)
          "Start agent-shell and resume SESSION-ID.
        CWD is the working directory (defaults to current project or default-directory).
        SESSION-FILE is the path to the conversation JSONL file for history replay."
          (interactive "sSession ID to resume: ")
          (let* ((target-dir (or cwd
                                 (projectile-project-root)
                                 default-directory))
                 (default-directory target-dir))
            ;; Set the resume session ID and file BEFORE starting agent-shell
            (setq cust/agent-shell--pending-resume-session-id session-id)
            (setq cust/agent-shell--pending-resume-session-file session-file)
            ;; Start agent-shell in the target directory
            (agent-shell)
            (message "Resuming session %s in %s" session-id target-dir)))

        ;; Advice to intercept session initiation and inject resume parameter
        (defun cust/agent-shell--initiate-session-advice (orig-fn &rest args)
          "Advice to inject resume session ID into session initiation."
          (if cust/agent-shell--pending-resume-session-id
              (let ((resume-id cust/agent-shell--pending-resume-session-id)
                    ;; Extract keyword args - args is (:shell val :on-session-init val)
                    (shell (plist-get args :shell))
                    (on-session-init (plist-get args :on-session-init)))
                ;; Clear it so it's only used once
                (setq cust/agent-shell--pending-resume-session-id nil)
                ;; Call our custom initiate with resume
                (cust/agent-shell--initiate-session-with-resume
                 shell
                 on-session-init
                 resume-id))
            ;; Normal flow
            (apply orig-fn args)))

        (advice-add 'agent-shell--initiate-session :around #'cust/agent-shell--initiate-session-advice)

        (defun cust/agent-shell-resume (&optional all-projects)
          "Interactively select and resume a previous Claude Code conversation.
        With prefix argument ALL-PROJECTS or when called with non-nil argument,
        show conversations from all projects. Otherwise show only conversations
        from the current project."
          (interactive "P")
          (let* ((project-path (unless all-projects
                                 (or (projectile-project-root)
                                     default-directory)))
                 (conversations (cust/agent-shell--list-conversations project-path)))
            (if (null conversations)
                (if all-projects
                    (user-error "No conversations found in ~/.claude/projects/")
                  (if (y-or-n-p "No conversations for current project. Show all projects? ")
                      (cust/agent-shell-resume t)
                    (user-error "No conversations found")))
              ;; Build candidates for completing-read
              (let* ((summary-width 45)
                     (msgs-width 6)
                     (branch-width 15)
                     (candidates
                      (mapcar (lambda (conv)
                                (let* ((summary (plist-get conv :summary))
                                       (first-msg (plist-get conv :first-message))
                                       (raw-text (or summary
                                                     (when first-msg
                                                       (string-trim
                                                        (replace-regexp-in-string "[\n\r]+" " " first-msg)))
                                                     "(empty)"))
                                       ;; Truncate/pad summary to fixed width
                                       (display-text (if (> (length raw-text) summary-width)
                                                         (concat (substring raw-text 0 (- summary-width 3)) "...")
                                                       (format (format "%%-%ds" summary-width) raw-text)))
                                       (msg-count (or (plist-get conv :message-count) 0))
                                       (branch (or (plist-get conv :git-branch) "-"))
                                       (branch-text (if (> (length branch) branch-width)
                                                        (concat (substring branch 0 (- branch-width 2)) "..")
                                                      (format (format "%%-%ds" branch-width) branch)))
                                       (time (cust/agent-shell--format-timestamp
                                              (plist-get conv :timestamp) nil))
                                       (project-name (plist-get conv :project-name))
                                       (id (plist-get conv :id))
                                       (cwd (plist-get conv :cwd))
                                       ;; Format with colors
                                       (formatted
                                        (concat
                                         display-text
                                         "  "
                                         (propertize (format "%5d" msg-count) 'face 'font-lock-number-face)
                                         "  "
                                         (propertize branch-text 'face 'font-lock-constant-face)
                                         "  "
                                         (propertize time 'face 'font-lock-comment-face)
                                         (when all-projects
                                           (concat "  " (propertize (format "[%s]" project-name)
                                                                    'face 'font-lock-comment-face))))))
                                  ;; Store id, cwd, and file path
                                  (cons formatted (list :id id :cwd cwd :file (plist-get conv :file)))))
                              conversations))
                     ;; Disable vertico sorting to preserve our timestamp order
                     (vertico-sort-function nil)
                     (vertico-sort-override-function nil)
                     (selection (completing-read
                                 (if all-projects
                                     "Resume conversation (all projects): "
                                   "Resume conversation: ")
                                 candidates nil t))
                     (selected (cdr (assoc selection candidates)))
                     (session-id (plist-get selected :id))
                     (session-cwd (plist-get selected :cwd))
                     (session-file (plist-get selected :file)))
                (when session-id
                  (cust/agent-shell--resume-conversation session-id session-cwd session-file))))))

        (defun cust/agent-shell-resume-all ()
          "Resume a conversation from any project."
          (interactive)
          (cust/agent-shell-resume t))

        ;; Keybinding for quick access
        (map! :leader
              (:prefix-map ("o" . "open")
               :desc "Resume Claude conversation" "R" #'cust/agent-shell-resume))
        ```

    <!--list-separator-->

    -  Conversation persistence (Claude Code compatible)

        This section implements bidirectional conversation sharing between agent-shell
        and Claude Code CLI. Conversations from agent-shell are written in Claude's
        JSONL format, allowing them to be resumed from the terminal and vice versa.

        <!--list-separator-->

        -  Session state tracking

            Track conversation state needed for Claude Code format: UUIDs, timestamps,
            parent chains, etc.

            ```emacs-lisp
            (defvar-local cust/agent-shell--claude-session nil
              "Claude Code compatible session state for the current buffer.
            Contains: session-id, parent-uuid, message-count, project-dir, jsonl-file.")

            (defvar cust/agent-shell--claude-version "2.1.7"
              "Version string to use in Claude Code JSONL entries.
            This should match Claude Code CLI version for compatibility.")

            (defun cust/agent-shell--init-claude-session ()
              "Initialize Claude Code compatible session tracking for current buffer.
            Called lazily when first message is submitted."
              (when-let* (((derived-mode-p 'agent-shell-mode))
                          ((boundp 'agent-shell--state))
                          (agent-shell--state)
                          ((eq 'claude-code (map-elt (map-elt agent-shell--state :agent-config) :identifier)))
                          (session-id (map-nested-elt agent-shell--state '(:session :id)))
                          ;; Use default-directory instead of agent-shell-cwd to avoid shell-maker dependency
                          (cwd (expand-file-name default-directory))
                          (project-dir-name (cust/agent-shell--encode-project-path cwd))
                          (project-dir (expand-file-name project-dir-name cust/agent-shell-claude-projects-dir))
                          (jsonl-file (expand-file-name (concat session-id ".jsonl") project-dir)))
                ;; Create project directory if needed
                (make-directory project-dir t)
                ;; Initialize session state
                (setq cust/agent-shell--claude-session
                      (list :session-id session-id
                            :parent-uuid nil
                            :message-count 0
                            :cwd cwd
                            :project-dir project-dir
                            :jsonl-file jsonl-file
                            :git-branch (cust/agent-shell--get-git-branch cwd)))
                ;; Write initial queue-operation entry
                (let* ((timestamp (cust/agent-shell--iso-timestamp))
                       (entry (list (cons 'type "queue-operation")
                                    (cons 'operation "dequeue")
                                    (cons 'timestamp timestamp)
                                    (cons 'sessionId session-id))))
                  (cust/agent-shell--write-jsonl-entry entry))
                (message "Claude session: %s" (file-name-nondirectory jsonl-file))))

            (defun cust/agent-shell--get-git-branch (dir)
              "Get current git branch for DIR, or nil if not in a git repo."
              (let ((default-directory dir))
                (when (file-directory-p (expand-file-name ".git" dir))
                  (string-trim
                   (shell-command-to-string "git rev-parse --abbrev-ref HEAD 2>/dev/null")))))

            (defun cust/agent-shell--iso-timestamp ()
              "Return current time as ISO 8601 timestamp with milliseconds."
              (let ((time (current-time)))
                (format "%s.%03dZ"
                        (format-time-string "%Y-%m-%dT%H:%M:%S" time t)
                        (/ (nth 2 time) 1000))))

            (defun cust/agent-shell--generate-uuid ()
              "Generate a UUID v4 string."
              (format "%08x-%04x-%04x-%04x-%012x"
                      (random (expt 16 8))
                      (random (expt 16 4))
                      (logior (ash 4 12) (random (expt 16 3)))  ; Version 4
                      (logior (ash 2 14) (random (expt 16 4)))  ; Variant 1
                      (random (expt 16 12))))
            ```

        <!--list-separator-->

        -  JSONL writing

            Write conversation entries in Claude Code's JSONL format.

            ```emacs-lisp
            (defun cust/agent-shell--write-jsonl-entry (entry)
              "Write ENTRY as a JSON line to the current session's JSONL file."
              (when-let ((session cust/agent-shell--claude-session)
                         (jsonl-file (plist-get session :jsonl-file)))
                (let ((json-str (json-encode entry)))
                  (with-temp-buffer
                    (insert json-str "\n")
                    (write-region (point-min) (point-max) jsonl-file t 'silent)))))

            (defun cust/agent-shell--write-user-message (text)
              "Write a user message entry to the Claude JSONL file."
              (when-let ((session cust/agent-shell--claude-session))
                (let* ((uuid (cust/agent-shell--generate-uuid))
                       (parent-uuid (plist-get session :parent-uuid))
                       (cwd (plist-get session :cwd))
                       (session-id (plist-get session :session-id))
                       (git-branch (plist-get session :git-branch))
                       (timestamp (cust/agent-shell--iso-timestamp))
                       (content (vector (list (cons 'type "text") (cons 'text text))))
                       (message (list (cons 'role "user") (cons 'content content)))
                       (entry (list (cons 'parentUuid parent-uuid)
                                    (cons 'isSidechain json-false)
                                    (cons 'userType "external")
                                    (cons 'cwd cwd)
                                    (cons 'sessionId session-id)
                                    (cons 'version cust/agent-shell--claude-version)
                                    (cons 'gitBranch git-branch)
                                    (cons 'type "user")
                                    (cons 'message message)
                                    (cons 'uuid uuid)
                                    (cons 'timestamp timestamp))))
                  ;; Update parent UUID for next message
                  (plist-put cust/agent-shell--claude-session :parent-uuid uuid)
                  (plist-put cust/agent-shell--claude-session :message-count
                             (1+ (plist-get session :message-count)))
                  (cust/agent-shell--write-jsonl-entry entry)
                  uuid)))

            (defun cust/agent-shell--write-assistant-message (text &optional tool-uses)
              "Write an assistant message entry to the Claude JSONL file.
            TEXT is the assistant's response. TOOL-USES is an optional list of tool calls."
              (when-let ((session cust/agent-shell--claude-session))
                (let* ((uuid (cust/agent-shell--generate-uuid))
                       (parent-uuid (plist-get session :parent-uuid))
                       (cwd (plist-get session :cwd))
                       (session-id (plist-get session :session-id))
                       (git-branch (plist-get session :git-branch))
                       (timestamp (cust/agent-shell--iso-timestamp))
                       (content (if tool-uses
                                    (vconcat
                                     (when text (vector (list (cons 'type "text") (cons 'text text))))
                                     (mapcar (lambda (tool)
                                               (list (cons 'type "tool_use")
                                                     (cons 'id (plist-get tool :id))
                                                     (cons 'name (plist-get tool :name))
                                                     (cons 'input (plist-get tool :input))))
                                             tool-uses))
                                  (vector (list (cons 'type "text") (cons 'text text)))))
                       (message (list (cons 'role "assistant") (cons 'content content)))
                       (entry (list (cons 'parentUuid parent-uuid)
                                    (cons 'isSidechain json-false)
                                    (cons 'userType "external")
                                    (cons 'cwd cwd)
                                    (cons 'sessionId session-id)
                                    (cons 'version cust/agent-shell--claude-version)
                                    (cons 'gitBranch git-branch)
                                    (cons 'type "assistant")
                                    (cons 'message message)
                                    (cons 'uuid uuid)
                                    (cons 'timestamp timestamp))))
                  ;; Update parent UUID
                  (plist-put cust/agent-shell--claude-session :parent-uuid uuid)
                  (cust/agent-shell--write-jsonl-entry entry)
                  uuid)))

            (defun cust/agent-shell--write-tool-result (tool-use-id result)
              "Write a tool result entry for TOOL-USE-ID with RESULT."
              (when-let ((session cust/agent-shell--claude-session))
                (let* ((uuid (cust/agent-shell--generate-uuid))
                       (parent-uuid (plist-get session :parent-uuid))
                       (cwd (plist-get session :cwd))
                       (session-id (plist-get session :session-id))
                       (git-branch (plist-get session :git-branch))
                       (timestamp (cust/agent-shell--iso-timestamp))
                       (content (vector (list (cons 'tool_use_id tool-use-id)
                                              (cons 'type "tool_result")
                                              (cons 'content result))))
                       (message (list (cons 'role "user") (cons 'content content)))
                       (entry (list (cons 'parentUuid parent-uuid)
                                    (cons 'isSidechain json-false)
                                    (cons 'userType "external")
                                    (cons 'cwd cwd)
                                    (cons 'sessionId session-id)
                                    (cons 'version cust/agent-shell--claude-version)
                                    (cons 'gitBranch git-branch)
                                    (cons 'type "user")
                                    (cons 'message message)
                                    (cons 'uuid uuid)
                                    (cons 'timestamp timestamp))))
                  (plist-put cust/agent-shell--claude-session :parent-uuid uuid)
                  (cust/agent-shell--write-jsonl-entry entry)
                  uuid)))
            ```

        <!--list-separator-->

        -  Summary generation

            Generate and prepend conversation summaries to the JSONL file.
            Claude Code stores summaries at the beginning of the file.

            ```emacs-lisp
            (defun cust/agent-shell--generate-summary (text)
              "Generate a short summary from TEXT (first user message or significant content).
            Returns a string suitable for the summary field."
              (let* ((clean-text (replace-regexp-in-string "[\n\r]+" " " text))
                     (truncated (if (> (length clean-text) 60)
                                    (concat (substring clean-text 0 57) "...")
                                  clean-text)))
                truncated))

            (defun cust/agent-shell--write-summary (summary-text)
              "Write a summary entry to the beginning of the JSONL file.
            SUMMARY-TEXT is the conversation summary to add."
              (when-let* ((session cust/agent-shell--claude-session)
                          (jsonl-file (plist-get session :jsonl-file))
                          (leaf-uuid (plist-get session :parent-uuid)))
                (when (file-exists-p jsonl-file)
                  (let* ((summary-entry `((type . "summary")
                                          (summary . ,summary-text)
                                          (leafUuid . ,leaf-uuid)))
                         (summary-json (concat (json-encode summary-entry) "\n"))
                         (existing-content (with-temp-buffer
                                             (insert-file-contents jsonl-file)
                                             (buffer-string))))
                    ;; Prepend summary to file
                    (with-temp-file jsonl-file
                      (insert summary-json)
                      (insert existing-content))))))

            (defun cust/agent-shell--update-summary-on-idle ()
              "Update conversation summary after idle time.
            This runs periodically to keep the summary current."
              (when-let* ((session cust/agent-shell--claude-session)
                          (jsonl-file (plist-get session :jsonl-file))
                          ((> (plist-get session :message-count) 0)))
                ;; Read first user message for summary if we don't have one yet
                (unless (plist-get session :summary-written)
                  (when-let ((first-msg (cust/agent-shell--get-first-user-message jsonl-file)))
                    (cust/agent-shell--write-summary
                     (cust/agent-shell--generate-summary first-msg))
                    (plist-put cust/agent-shell--claude-session :summary-written t)))))

            (defun cust/agent-shell--get-first-user-message (jsonl-file)
              "Get the first user message text from JSONL-FILE."
              (when (file-exists-p jsonl-file)
                (with-temp-buffer
                  (insert-file-contents jsonl-file)
                  (goto-char (point-min))
                  (catch 'found
                    (while (not (eobp))
                      (let* ((line (buffer-substring-no-properties
                                    (line-beginning-position) (line-end-position)))
                             (json (ignore-errors (json-parse-string line :object-type 'alist))))
                        (when (and json
                                   (string= (alist-get 'type json) "user")
                                   (alist-get 'message json))
                          (let* ((message (alist-get 'message json))
                                 (content (alist-get 'content message)))
                            (when (and content (> (length content) 0))
                              (throw 'found (alist-get 'text (aref content 0)))))))
                      (forward-line 1))
                    nil))))
            ```

        <!--list-separator-->

        -  Hooks and integration

            Hook into agent-shell to capture messages and write them in Claude format.

            ```emacs-lisp
            (defvar cust/agent-shell--pending-assistant-text ""
              "Buffer for accumulating assistant message chunks.")

            (defun cust/agent-shell--ensure-session ()
              "Ensure Claude session is initialized, initializing lazily if needed.
            Returns non-nil if session is ready."
              (or cust/agent-shell--claude-session
                  (when (and (derived-mode-p 'agent-shell-mode)
                             (boundp 'agent-shell--state)
                             agent-shell--state
                             (map-nested-elt agent-shell--state '(:session :id))
                             (eq 'claude-code (map-elt (map-elt agent-shell--state :agent-config) :identifier)))
                    (cust/agent-shell--init-claude-session)
                    cust/agent-shell--claude-session)))

            (defun cust/agent-shell--capture-user-input (input)
              "Capture user INPUT and write to Claude JSONL.
            This is called via advice on shell-maker-submit."
              (when (and (derived-mode-p 'agent-shell-mode)
                         (stringp input)
                         (not (string-empty-p (string-trim input)))
                         (cust/agent-shell--ensure-session))
                (cust/agent-shell--write-user-message (string-trim input))))

            (defun cust/agent-shell--capture-assistant-chunk (text)
              "Accumulate assistant TEXT chunks for later writing."
              (when cust/agent-shell--claude-session
                (setq cust/agent-shell--pending-assistant-text
                      (concat cust/agent-shell--pending-assistant-text text))))

            (defun cust/agent-shell--flush-assistant-message ()
              "Write accumulated assistant message to JSONL and reset buffer."
              (when (and cust/agent-shell--claude-session
                         (not (string-empty-p cust/agent-shell--pending-assistant-text)))
                (cust/agent-shell--write-assistant-message
                 cust/agent-shell--pending-assistant-text)
                (setq cust/agent-shell--pending-assistant-text "")))

            ;; Advice to capture user input before submission
            (defun cust/agent-shell--submit-advice (orig-fn &rest args)
              "Advice for shell-maker-submit to capture user input."
              (when (derived-mode-p 'agent-shell-mode)
                (let ((input (string-trim
                              (buffer-substring-no-properties
                               (save-excursion
                                 (goto-char (point-max))
                                 (comint-line-beginning-position))
                               (point-max)))))
                  (unless (string-empty-p input)
                    (cust/agent-shell--capture-user-input input))))
              (apply orig-fn args))

            (advice-add 'shell-maker-submit :around #'cust/agent-shell--submit-advice)

            ;; No timers or mode hooks needed - session is initialized lazily
            ;; on first message via cust/agent-shell--ensure-session below.
            ```

        <!--list-separator-->

        -  Message capture via transcript advice

            Hook into agent-shell's existing transcript system to capture messages.

            ```emacs-lisp
            (defun cust/agent-shell--transcript-advice (orig-fn &rest args)
              "Advice for agent-shell--append-transcript to also write Claude JSONL.
            Intercepts transcript writes to extract message content."
              ;; Call original function
              (apply orig-fn args)
              ;; Extract text from args and determine message type
              (when cust/agent-shell--claude-session
                (let ((text (plist-get args :text)))
                  (when (stringp text)
                    (cond
                     ;; Agent message header - flush any pending and prepare for new
                     ((string-match "^## Agent" text)
                      (cust/agent-shell--flush-assistant-message))
                     ;; User message header - already captured via submit advice
                     ((string-match "^## User" text)
                      nil)
                     ;; Actual content (not headers)
                     ((and (not (string-match "^## " text))
                           (not (string-match "^### Tool Call" text)))
                      ;; This is message content, accumulate for assistant
                      (cust/agent-shell--capture-assistant-chunk text)))))))

            (advice-add 'agent-shell--append-transcript :around #'cust/agent-shell--transcript-advice)

            ;; Flush assistant message when command finishes
            (defun cust/agent-shell--on-command-finished ()
              "Called when a command/response cycle completes."
              (when cust/agent-shell--claude-session
                (cust/agent-shell--flush-assistant-message)
                (cust/agent-shell--update-summary-on-idle)))

            ;; Hook into shell-maker's on-command-finished if available
            ;; Note: shell-maker--config may be nil during early initialization,
            ;; so we check both boundp AND non-nil. If nil, we skip - the transcript
            ;; advice will still capture messages via agent-shell--append-transcript.
            (add-hook 'agent-shell-mode-hook
                      (lambda ()
                        (when (and (boundp 'shell-maker--config) shell-maker--config)
                          (let ((orig-fn (shell-maker-config-on-command-finished shell-maker--config)))
                            (setf (shell-maker-config-on-command-finished shell-maker--config)
                                  (lambda (input output success)
                                    (when orig-fn (funcall orig-fn input output success))
                                    (cust/agent-shell--on-command-finished)))))))
            ```


#### IBuffer {#ibuffer}

<!--list-separator-->

-  Projectile integration

    By default, `ibuffer-projectile` only groups file-visiting buffers by project.
    This advice extends it to include non-file buffers (like agent-shell, vterm,
    compilation, etc.) based on their `default-directory`.

    ```emacs-lisp
    (defun cust/ibuffer-projectile-root-include-non-file-buffers (orig-fn buf)
      "Advice to include non-file buffers in projectile groups.
    Falls back to `default-directory' when `buffer-file-name' is nil."
      (or (funcall orig-fn buf)
          (with-current-buffer buf
            (when-let ((root (ignore-errors (projectile-project-root))))
              (cons (projectile-project-name) root)))))

    (advice-add 'ibuffer-projectile-root :around #'cust/ibuffer-projectile-root-include-non-file-buffers)
    ```

<!--list-separator-->

-  Casual Transient Menus

    [Casual](https://github.com/kickingvegas/casual) provides discoverable transient menus for ibuffer operations
    including filtering, sorting, and marking.

    ```emacs-lisp
    (package! casual)
    ```

    ```emacs-lisp
    (defun cust/ibuffer-special-buffer-p (buf)
      "Return non-nil if BUF name starts with *."
      (string-prefix-p "*" (buffer-name buf)))

    (defvar cust/ibuffer-hide-special-buffers nil
      "When non-nil, hide buffers starting with *.")

    (defun cust/ibuffer-toggle-special-buffers ()
      "Toggle visibility of special buffers (those starting with *)."
      (interactive)
      (setq cust/ibuffer-hide-special-buffers (not cust/ibuffer-hide-special-buffers))
      (if cust/ibuffer-hide-special-buffers
          (progn
            (add-to-list 'ibuffer-never-show-predicates #'cust/ibuffer-special-buffer-p)
            (message "Hiding special buffers"))
        (setq ibuffer-never-show-predicates
              (remove #'cust/ibuffer-special-buffer-p ibuffer-never-show-predicates))
        (message "Showing all buffers"))
      (ibuffer-update nil t))

    (use-package! casual
      :after ibuffer
      :config
      (map! :map ibuffer-mode-map
            :n "?"   #'casual-ibuffer-tmenu
            :n "H"   #'cust/ibuffer-toggle-special-buffers
            :n "F"   #'casual-ibuffer-filter-tmenu
            :n "s"   #'casual-ibuffer-sortby-tmenu
            :n "p"   #'ibuffer-backwards-next-marked
            :n "n"   #'ibuffer-forward-next-marked
            :n "C-k" #'ibuffer-backward-filter-group
            :n "C-j" #'ibuffer-forward-filter-group
            :n "$"   #'ibuffer-toggle-filter-group)

      ;; Update transient menu keybindings to match our ibuffer-mode-map
      (with-eval-after-load 'casual-ibuffer
        (transient-suffix-put 'casual-ibuffer-tmenu "p" :key "k")   ; Move default p (prev line) to k
        (transient-suffix-put 'casual-ibuffer-tmenu "n" :key "j")   ; Move default n (next line) to j
        (transient-suffix-put 'casual-ibuffer-tmenu "{" :key "p")   ; Previous marked → p
        (transient-suffix-put 'casual-ibuffer-tmenu "}" :key "n")   ; Next marked → n
        (transient-suffix-put 'casual-ibuffer-tmenu "[" :key "C-k") ; Previous group → C-k
        (transient-suffix-put 'casual-ibuffer-tmenu "]" :key "C-j")) ; Next group → C-j

      (add-hook 'ibuffer-mode-hook #'hl-line-mode)
      (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode))
    ```


#### Startup Profiling and Optimizations {#startup-profiling-and-optimizations}

Benchmark startup time to identify bottlenecks.

**Testing Startup Performance:**

1.  Run `doom sync` to apply all configuration changes
2.  Start Emacs and run `M-x benchmark-init/show-durations-tree` to see startup timings
3.  Check overall startup time with `M-x emacs-init-time`
4.  For more detailed analysis, start Emacs with `emacs --debug-init`
5.  To profile specific functions: `M-x profiler-start`, restart Emacs, then `M-x profiler-report`

<!--listend-->

```emacs-lisp
(package! benchmark-init)
```

```emacs-lisp
(use-package! benchmark-init
  :demand t
  :config
  ;; To disable collection after init
  (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))

;; Report startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
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
  (setq corfu-quit-no-match t)  ;; Quit immediately when no match
  (map! :map corfu-map
        "SPC" #'corfu-insert-separator  ;; Enables multiple orderless patterns
        ;; Unbind C-n/C-p to free them for other modes (use C-j/C-k instead)
        "C-n" nil
        "C-p" nil))
```

Corfu uses child frames for its popup, which do not work in terminal Emacs. `corfu-terminal` replaces the popup with overlays so completion works correctly in `emacsclient -nw` sessions.

```emacs-lisp
(package! corfu-terminal
  :recipe (:host codeberg :repo "akib/emacs-corfu-terminal"))
```

```emacs-lisp
(after! corfu
  (corfu-terminal-mode +1)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (unless (display-graphic-p frame)
                (corfu-popupinfo-mode -1)))))
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
(use-package! dired
  :defer t
  :commands (dired dired-jump)
  :init
  (map! :leader :desc "Dired" "-" #'dired-jump)         ;; easy access shortcut
  :config
  (require 'evil-collection)
  (dired-async-mode 1)
  (dired-recent-mode 1)
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

    (defun cust/smart-tab ()
      "Smart TAB for Evil insert mode.
    Prioritizes completions over indentation:
    - If Corfu popup is visible → complete the selection
    - If Copilot suggestion is showing → accept it
    - In Org mode → use org-cycle (handles structure templates, etc.)
    - Otherwise → use indent-for-tab-command for context-aware indentation"
      (interactive)
      (cond
       ;; Corfu popup frame is actually visible
       ((and (bound-and-true-p corfu-mode)
             (boundp 'corfu--frame)
             corfu--frame
             (frame-visible-p corfu--frame))
        (corfu-complete))
       ;; Copilot overlay is actually visible in current buffer
       ((and (bound-and-true-p copilot-mode)
             (boundp 'copilot--overlay)
             copilot--overlay
             (overlayp copilot--overlay)
             (overlay-buffer copilot--overlay))
        (copilot-accept-completion))
       ;; Org mode: use org-cycle which handles structure templates (<s, <q, etc.)
       ((derived-mode-p 'org-mode)
        (org-cycle))
       ;; Default: context-aware indentation via the mode's indent command
       (t
        (indent-for-tab-command))))
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
      (advice-add 'evil-scroll-down        :after #'evil-scroll-to-center-advice)

      ;; Smart TAB in insert mode: completions take priority over indentation
      (define-key evil-insert-state-map (kbd "TAB") #'cust/smart-tab)
      (define-key evil-insert-state-map (kbd "<tab>") #'cust/smart-tab))
    ```

    Sometimes it's convenient to insert multiple cursors using the mouse. Inserts a new cursor using `C-S-<mouse-1>`.

    ```emacs-lisp
    (after! evil
      (global-set-key (kbd "C-S-<mouse-1>") #'evil-mc-toggle-cursor-on-click))
    ```


#### Folding {#folding}

> From the `:editor fold` module

For tree-sitter based modes, we can use the `treesit-fold` package instead.

```emacs-lisp
(package! treesit-fold)
```

Make folding work from anywhere within a foldable construct, not just on the exact folding marker:

```emacs-lisp
;; TODO AI: We need to work more on this.
;; (after! treesit-fold
;;   ;; Make folding work from anywhere within a foldable construct
;;   (defun treesit-fold--find-foldable-node-at-point ()
;;     "Find a foldable node at or around point."
;;     (let ((node (treesit-node-at (point)))
;;           (foldable-types (mapcar #'car (alist-get major-mode treesit-fold-range-alist))))
;;       ;; First check if current node is already foldable
;;       (if (member (treesit-node-type node) foldable-types)
;;           node
;;         ;; Otherwise, look for foldable nodes in the ancestry and their children
;;         (let ((current node))
;;           (while current
;;             ;; Check if current node has foldable children
;;             (let ((foldable-child (seq-find
;;                                   (lambda (child)
;;                                     (member (treesit-node-type child) foldable-types))
;;                                   (treesit-node-children current nil))))
;;               (when foldable-child
;;                 (cl-return foldable-child)))
;;             (setq current (treesit-node-parent current)))
;;           nil))))

;;   ;; Override the default node finding behavior
;;   (advice-add 'treesit-fold-close :around
;;               (lambda (orig-fun)
;;                 (let ((node (treesit-fold--find-foldable-node-at-point)))
;;                   (if node
;;                       (let ((pos (treesit-node-start node)))
;;                         (save-excursion
;;                           (goto-char pos)
;;                           (funcall orig-fun)))
;;                     (funcall orig-fun)))))

;;   (advice-add 'treesit-fold-open :around
;;               (lambda (orig-fun)
;;                 (let ((node (treesit-fold--find-foldable-node-at-point)))
;;                   (if node
;;                       (let ((pos (treesit-node-start node)))
;;                         (save-excursion
;;                           (goto-char pos)
;;                           (funcall orig-fun)))
;;                     (funcall orig-fun))))))
```


#### Formatting {#formatting}

> From the `:editor format` module

Doom Emacs now uses [aphelieia](https://github.com/radian-software/apheleia) as the default formatting system.
Override to use the latest version from Github.

```emacs-lisp
(package! apheleia :pin "e6e5d55" :recipe (:repo "radian-software/apheleia"))
```

<!--list-separator-->

-  WS Butler

    An unobtrusive way to trim spaces from end of lines.

    ```emacs-lisp
    (package! ws-butler)
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
      (when (file-exists-p "/usr/local/Cellar/languagetool/5.6/libexec/languagetool-commandline.jar")
        (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/5.6/libexec/languagetool-commandline.jar"
              langtool-autoshow-message-function #'langtool-autoshow-detail-popup))

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


#### Version Control System {#version-control-system}

Configure `diff-hl` to use margin mode to avoid fringe conflicts with `treesit-fold`.

```emacs-lisp
(after! diff-hl
  (diff-hl-margin-mode 1))
```

```emacs-lisp
(map! :leader
      :desc "Previous hunk" "g p" #'+vc-gutter/previous-hunk  ;; From the =:tools vc= module
      :desc "Next hunk"     "g n" #'+vc-gutter/next-hunk      ;; From the =:tools vc= module
      "g [" nil                                               ;; Unbind default evil keybindings that conflict with ibuffer
      "g ]" nil                                               ;; Unbind default evil keybindings that conflict with ibuffer
      )
```

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
      (setq git-commit-summary-max-length       80                  ;; Increase commit summary length
            git-commit-post-finish-hook-timeout 2                   ;; Wait longer for GPG/YubiKey PIN entry
            magit-copy-revision-abbreviated     t                   ;; Copy short version of hashes
            magit-list-refs-sortby              "-committerdate")   ;; Sort by last commited date (latest on top)

      ;; Always suggest .worktrees directory for new worktrees
      (setq magit-worktree-read-directory-name-function
            (lambda (prompt)
              (let* ((default-dir (expand-file-name ".worktrees" (magit-toplevel))))
                (unless (file-exists-p default-dir)
                  (make-directory default-dir t))
                (read-directory-name prompt default-dir)))))
    ```

    Render ANSI escape codes (colors, icons, etc.) in the `*magit-process*` buffer.
    This is useful when git hooks output colored text with Unicode characters.

    Some tools output broken ANSI sequences where the ESC character (`\e`) is
    stripped, leaving just `[37m` instead of `\e[37m`. We advise the magit process
    filter to strip these broken sequences in real-time as output arrives.

    ```emacs-lisp
    (after! magit
      (defun my/magit-process-filter-handle-ansi (fn proc string)
        "Process ANSI sequences in magit process output.
    Applies ansi-color to render valid sequences and strips broken ones."
        ;; First strip broken sequences (missing ESC character)
        (setq string (replace-regexp-in-string "\\[\\([0-9;]*\\)m" "" string))
        ;; Then apply ansi-color to handle valid sequences
        (setq string (ansi-color-apply string))
        (funcall fn proc string))

      (advice-add 'magit-process-filter
                  :around #'my/magit-process-filter-handle-ansi))
    ```

<!--list-separator-->

-  Branch Cleanup

    Interactive command to bulk-delete local branches that are merged or whose
    upstream tracking branch is gone. Accessible from the branch transient via `D`.

    ```emacs-lisp
    (after! magit
      ;; --- Helper functions ---

      (defvar cust/magit-cleanup--protected-branches '("main" "master" "trunk")
        "Branch names that should never appear as cleanup candidates.")

      (defun cust/magit-cleanup--merged-branches (base)
        "Return list of local branches merged into BASE.
    Excludes BASE itself, the current branch, and protected branches."
        (let* ((current (magit-get-current-branch))
               (output (magit-git-lines "branch" "--merged" base))
               (branches
                (cl-loop for line in output
                         for branch = (string-trim line)
                         ;; Strip leading "* " (current) or "+ " (worktree) markers
                         for name = (cond
                                     ((string-prefix-p "* " branch) (substring branch 2))
                                     ((string-prefix-p "+ " branch) (substring branch 2))
                                     (t branch))
                         unless (or (string= name base)
                                    (string= name current)
                                    (string-empty-p name)
                                    (member name cust/magit-cleanup--protected-branches))
                         collect name)))
          branches))

      (defun cust/magit-cleanup--gone-branches ()
        "Return list of local branches whose upstream tracking branch is gone.
    Excludes protected branches."
        (let* ((output (magit-git-lines
                        "branch" "--format=%(refname:short) %(upstream:track)")))
          (cl-loop for line in output
                   for trimmed = (string-trim line)
                   when (string-match "\\`\\(.+\\) \\[gone\\]\\'" trimmed)
                   unless (member (match-string 1 trimmed)
                                  cust/magit-cleanup--protected-branches)
                   collect (match-string 1 trimmed))))

      ;; --- Buffer state ---

      (defvar-local cust/magit-cleanup--branches nil
        "Alist of (BRANCH . SELECTED-P) for cleanup candidates.")

      (defvar-local cust/magit-cleanup--prune nil
        "Whether to prune remote tracking branches.")

      (defvar-local cust/magit-cleanup--base nil
        "The base branch used for merge detection.")

      (defvar-local cust/magit-cleanup--merged nil
        "List of branch names that are merged.")

      (defvar-local cust/magit-cleanup--gone nil
        "List of branch names whose upstream is gone.")

      ;; --- Mode and rendering ---

      (defvar magit-branch-cleanup-mode-map
        (make-sparse-keymap)
        "Keymap for `magit-branch-cleanup-mode'.")

      (define-derived-mode magit-branch-cleanup-mode special-mode "Branch-Cleanup"
        "Major mode for selecting branches to delete."
        (setq truncate-lines t)
        (setq buffer-read-only t))

      (evil-define-key 'normal magit-branch-cleanup-mode-map
        (kbd "t")   #'cust/magit-cleanup--toggle
        (kbd "RET") #'cust/magit-cleanup--toggle
        (kbd "a")   #'cust/magit-cleanup--select-all
        (kbd "u")   #'cust/magit-cleanup--deselect-all
        (kbd "x")   #'cust/magit-cleanup--execute
        (kbd "j")   #'cust/magit-cleanup--next
        (kbd "n")   #'cust/magit-cleanup--next
        (kbd "k")   #'cust/magit-cleanup--prev
        (kbd "p")   #'cust/magit-cleanup--prev
        (kbd "q")   #'quit-window)

      (defun cust/magit-cleanup--render ()
        "Render the cleanup buffer contents."
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize (format "Branch Cleanup (base: %s)\n\n"
                                      cust/magit-cleanup--base)
                              'face 'magit-section-heading))
          ;; Merged branches
          (when cust/magit-cleanup--merged
            (insert (propertize (format "Merged into %s:\n" cust/magit-cleanup--base)
                                'face 'magit-section-secondary-heading))
            (dolist (branch cust/magit-cleanup--merged)
              (let ((selected (alist-get branch cust/magit-cleanup--branches
                                         nil nil #'string=)))
                (insert (format "%s %s\n"
                                (if selected "[x]" "[ ]")
                                (propertize branch 'face 'magit-branch-local)))))
            (insert "\n"))
          ;; Gone branches
          (when cust/magit-cleanup--gone
            (insert (propertize "Upstream gone:\n"
                                'face 'magit-section-secondary-heading))
            (dolist (branch cust/magit-cleanup--gone)
              (let ((selected (alist-get branch cust/magit-cleanup--branches
                                         nil nil #'string=)))
                (insert (format "%s %s\n"
                                (if selected "[x]" "[ ]")
                                (propertize branch 'face 'magit-branch-local)))))
            (insert "\n"))
          ;; Prune option
          (insert "---\n")
          (insert (format "%s Prune remote tracking branches (git fetch --prune)\n\n"
                          (if cust/magit-cleanup--prune "[x]" "[ ]")))
          ;; Help line
          (insert (propertize "[t] toggle  [a] all  [u] none  [x] execute  [q] quit"
                              'face 'font-lock-comment-face))
          (goto-char (point-min))
          ;; Move to first selectable item
          (cust/magit-cleanup--next)))

      ;; --- Navigation ---

      (defun cust/magit-cleanup--branch-at-point ()
        "Return the branch name at point, or nil."
        (save-excursion
          (beginning-of-line)
          (when (looking-at "\\[.\\] \\(.+\\)$")
            (match-string-no-properties 1))))

      (defun cust/magit-cleanup--prune-line-p ()
        "Return non-nil if point is on the prune option line."
        (save-excursion
          (beginning-of-line)
          (looking-at "\\[.\\] Prune remote")))

      (defun cust/magit-cleanup--next ()
        "Move to the next selectable item."
        (interactive)
        (let ((found nil))
          (while (and (not found) (not (eobp)))
            (forward-line 1)
            (when (or (cust/magit-cleanup--branch-at-point)
                      (cust/magit-cleanup--prune-line-p))
              (setq found t)))
          (beginning-of-line)))

      (defun cust/magit-cleanup--prev ()
        "Move to the previous selectable item."
        (interactive)
        (let ((found nil))
          (while (and (not found) (not (bobp)))
            (forward-line -1)
            (when (or (cust/magit-cleanup--branch-at-point)
                      (cust/magit-cleanup--prune-line-p))
              (setq found t)))
          (beginning-of-line)))

      ;; --- Toggle and selection ---

      (defun cust/magit-cleanup--toggle ()
        "Toggle the item under point."
        (interactive)
        (cond
         ((cust/magit-cleanup--branch-at-point)
          (let* ((branch (cust/magit-cleanup--branch-at-point))
                 (current (alist-get branch cust/magit-cleanup--branches
                                    nil nil #'string=)))
            (setf (alist-get branch cust/magit-cleanup--branches nil nil #'string=)
                  (not current))
            (cust/magit-cleanup--refresh-line)))
         ((cust/magit-cleanup--prune-line-p)
          (setq cust/magit-cleanup--prune (not cust/magit-cleanup--prune))
          (cust/magit-cleanup--refresh-line))))

      (defun cust/magit-cleanup--refresh-line ()
        "Refresh the checkbox on the current line."
        (let ((inhibit-read-only t))
          (save-excursion
            (beginning-of-line)
            (when (looking-at "\\[.\\]")
              (cond
               ((cust/magit-cleanup--branch-at-point)
                (let* ((branch (cust/magit-cleanup--branch-at-point))
                       (selected (alist-get branch cust/magit-cleanup--branches
                                            nil nil #'string=)))
                  (delete-region (line-beginning-position) (line-end-position))
                  (insert (format "%s %s"
                                  (if selected "[x]" "[ ]")
                                  (propertize branch 'face 'magit-branch-local)))))
               ((cust/magit-cleanup--prune-line-p)
                (delete-region (line-beginning-position) (line-end-position))
                (insert (format "%s Prune remote tracking branches (git fetch --prune)"
                                (if cust/magit-cleanup--prune "[x]" "[ ]")))))))))
      (defun cust/magit-cleanup--select-all ()
        "Select all branches."
        (interactive)
        (setq cust/magit-cleanup--branches
              (mapcar (lambda (pair) (cons (car pair) t))
                      cust/magit-cleanup--branches))
        (cust/magit-cleanup--render))

      (defun cust/magit-cleanup--deselect-all ()
        "Deselect all branches."
        (interactive)
        (setq cust/magit-cleanup--branches
              (mapcar (lambda (pair) (cons (car pair) nil))
                      cust/magit-cleanup--branches))
        (cust/magit-cleanup--render))

      ;; --- Execution ---

      (defun cust/magit-cleanup--execute ()
        "Delete selected branches and optionally prune remotes."
        (interactive)
        (let* ((selected (cl-loop for (branch . sel) in cust/magit-cleanup--branches
                                  when sel collect branch))
               (prune cust/magit-cleanup--prune)
               (count (length selected)))
          (when (and (= count 0) (not prune))
            (user-error "No branches selected and prune not enabled"))
          (when (yes-or-no-p
                 (format "Delete %d branch%s%s? "
                         count
                         (if (= count 1) "" "es")
                         (if prune " and prune remotes" "")))
            (dolist (branch selected)
              (magit-run-git "branch" "-d" branch))
            (when prune
              (magit-run-git "fetch" "--prune"))
            (magit-refresh)
            (quit-window t)
            (message "Deleted %d branch%s%s."
                     count
                     (if (= count 1) "" "es")
                     (if prune ", pruned remotes" "")))))

      ;; --- Entry point ---

      (defun cust/magit-branch-cleanup ()
        "Interactively select and delete merged/orphaned branches."
        (interactive)
        (let* ((base (magit-main-branch))
               (merged (cust/magit-cleanup--merged-branches base))
               (gone (cust/magit-cleanup--gone-branches))
               (all-branches (cl-union merged gone :test #'string=)))
          (if (null all-branches)
              (message "No branches to clean up.")
            (let ((buf (get-buffer-create "*magit-branch-cleanup*")))
              (with-current-buffer buf
                (magit-branch-cleanup-mode)
                (setq cust/magit-cleanup--base base)
                (setq cust/magit-cleanup--merged merged)
                (setq cust/magit-cleanup--gone gone)
                (setq cust/magit-cleanup--branches
                      (mapcar (lambda (b) (cons b t)) all-branches))
                (setq cust/magit-cleanup--prune nil)
                (cust/magit-cleanup--render))
              (let ((win (display-buffer-in-side-window
                          buf '((side . bottom)))))
                (when win
                  (fit-window-to-buffer win)
                  (select-window win)))))))

      ;; Register in branch transient
      (transient-append-suffix 'magit-branch "x"
        '("D" "Cleanup" cust/magit-branch-cleanup)))
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

    ;; Bind ? to the transient menu in code-review buffers
    (map! :after code-review
          :map code-review-mode-map
          :n "?" #'code-review-transient-api
          :n "C-n" #'magit-section-forward
          :n "C-p" #'magit-section-backward)

    ;; Use the forge credentials for authentication (from ~/.authinfo.gpg)
    (setq code-review-auth-login-marker 'forge)

    ;; Open the review buffer in the current window instead of a bottom split
    (setq code-review-new-buffer-window-strategy #'switch-to-buffer)

    ;; Fix closql v2.3+ compatibility: closql--remake-instance produces objects
    ;; with an eieio--class struct at index 0 instead of a class name symbol.
    ;; When closql-insert later calls closql--abbrev-class on the coerced list,
    ;; it fails because the method only dispatches on (subclass closql-object).
    ;; Normalizing index 0 before insert fixes this.
    (define-advice closql-insert (:before (_db obj &optional _replace) code-review-fix-class-tag)
      (when (eieio--class-p (aref obj 0))
        (aset obj 0 (eieio--class-name (aref obj 0)))))

    ;; Fix stale reference to code-review-db-connection (removed in closql v2 migration).
    ;; The old variable no longer exists; closql manages connections internally now.
    (define-advice code-review-db-all-unfinished (:override () code-review-fix-stale-connection)
      (let ((class 'code-review-db-pullreq)
            (db (code-review-db)))
        (->> (emacsql db
                      [:select :*
                       :from 'pullreq
                       :where (and (= saved 't)
                                   (is finished nil))])
             (mapcar
              (lambda (row) (closql--remake-instance class db row))))))
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
    ```


#### Marginalia {#marginalia}

> From the `:completion vertico` module

[Marginalia](https://github.com/minad/marginalia) is a tool (written by the same author as vertico) which adds marginalia to the mini-buffer completions. Marginalia are marks and annotations placed at the margin of a page in a book.

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


#### Mise {#mise}

Enable automatic detection of language versions using [mise-en-place](https://mise.jdx.dev).

```emacs-lisp
(package! mise
  :recipe (:host github :repo "eki3z/mise.el" :files ("*.el")))
```

```emacs-lisp
(require 'mise)
(add-hook 'after-init-hook #'global-mise-mode)
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
                                                ".expert"
                                                ".elixir_ls"
                                                ".htmlcov"
                                                ".pytest_cache"
                                                "_build"
                                                "__pycache__"
                                                "deps"
                                                "node_modules"))

;; Elixir umbrella project detection
(defun projectile-elixir-umbrella-root (dir)
  "Find the Elixir umbrella project root if DIR is inside one."
  (when-let* ((current-dir (or dir default-directory))
              (project-dir (locate-dominating-file current-dir "mix.exs")))
    ;; Check if this project is inside an apps/ directory
    (let* ((project-path (directory-file-name project-dir))
           (parent-dir (file-name-directory project-path)))
      (when (and parent-dir
                 (string-match-p "/apps/?$" parent-dir))
        ;; We're inside an apps/ directory, look for umbrella root
        (let ((umbrella-root (file-name-directory (directory-file-name parent-dir))))
          (when (file-exists-p (expand-file-name "mix.exs" umbrella-root))
            umbrella-root))))))

(after! projectile
  ;; Add C# and F# project files as root markers
  (dolist (file '("*.csproj" "*.fsproj"))
    (add-to-list 'projectile-project-root-files file))

  ;; Add umbrella detection function to the beginning of the list
  ;; This ensures umbrella projects are detected before individual apps
  (add-to-list 'projectile-project-root-functions
               #'projectile-elixir-umbrella-root))
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
(after! smartparens
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
        (delete (regexp-quote (format "%sFramebuffer-" treemacs-buffer-name-prefix))
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
(after! lsp-treemacs
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
(package! vterm-anti-flicker-filter :pin "b17b013"
  :recipe (:host github :repo "martinbaillie/vterm-anti-flicker-filter" :files ("*.el")))
```

Load `vterm-anti-flicker-filter` before `vterm` so the hook attaches properly:

```emacs-lisp
(use-package! vterm-anti-flicker-filter
  :defer t
  :hook (vterm-mode . vterm-anti-flicker-filter-enable)
  :config
  (setq claude-code-ide-vterm-render-delay nil))
```

```emacs-lisp
(use-package! vterm
  :config
  (setq vterm-timer-delay nil)
  ;; Fix: vterm-send-return sends LF when icrnl=t, but terminal expects CR
  (defun vterm-send-return ()
    "Send CR to the libvterm."
    (interactive)
    (deactivate-mark)
    (when vterm--term
      (process-send-string vterm--process "\C-m"))))
```

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
      ;; Send C-g to cancel shell operations, but send ESC in claude-code-ide buffers
      ;; to avoid triggering VS Code (Claude Code interprets C-g as "open in editor")
      :i "C-g"        (lambda () (interactive)
                        (if (string-prefix-p "*claude-code[" (buffer-name))
                            (vterm-send-escape)
                          (process-send-string vterm--process "\C-g")))
      :n "C-g"        (lambda () (interactive)
                        (if (string-prefix-p "*claude-code[" (buffer-name))
                            (vterm-send-escape)
                          (process-send-string vterm--process "\C-g")))
      ;; TAB for shell completion
      :i "TAB"        #'vterm-send-tab
      :i "<tab>"      #'vterm-send-tab
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

Enable Evil motion state for `vterm-copy-mode` (read-only friendly):

```emacs-lisp
(after! vterm
  (add-hook 'vterm-copy-mode-hook
            (lambda ()
              (if vterm-copy-mode
                  (evil-motion-state)
                (evil-insert-state)))))
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

Doom already enables `indent-bars` via the `:ui indent-guides` module and sets `indent-bars-prefer-character t`
on macOS (bitmaps are slower on Mac). We add performance optimizations:

```emacs-lisp
(after! indent-bars
  ;; Performance optimizations
  (setq indent-bars-treesit-support nil          ; Disable tree-sitter scope detection (significant overhead)
        indent-bars-display-on-blank-lines nil   ; Don't render on blank lines (reduces fontification work)
        indent-bars-depth-update-delay 0.1))     ; Slightly slower updates during scrolling (default 0.075)
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
        lsp-keep-workspace-alive nil
        lsp-modeline-code-actions-segments '(count icon name)
        lsp-response-timeout 10)

  (defun cust/kill-lsp-child-processes (workspace)
    "Kill child processes of WORKSPACE's LSP server.
Some LSP servers spawn child processes that don't get cleaned up
when the workspace is shut down. This ensures orphaned processes
are terminated."
    (when-let* ((proc (lsp--workspace-proc workspace))
                (pid (process-id proc)))
      (call-process "pkill" nil nil nil "-P" (number-to-string pid))))

  (add-hook 'lsp-after-uninitialized-functions #'cust/kill-lsp-child-processes))
```

<!--list-separator-->

-  Emacs LSP Performance Booster

    [Emacs LSP Performance Booster](https://github.com/blahgeek/emacs-lsp-booster) is a wrapper executable written in Rust that improves the performance
    of LSP mode. See build instructions in the Github repository.

    ```emacs-lisp
    ;; (defun lsp-booster--advice-json-parse (old-fn &rest args)
    ;;   "Try to parse bytecode instead of json."
    ;;   (or
    ;;    (when (equal (following-char) ?#)
    ;;      (let ((bytecode (read (current-buffer))))
    ;;        (when (byte-code-function-p bytecode)
    ;;          (funcall bytecode))))
    ;;    (apply old-fn args)))
    ;; (advice-add (if (progn (require 'json)
    ;;                        (fboundp 'json-parse-buffer))
    ;;                 'json-parse-buffer
    ;;               'json-read)
    ;;             :around
    ;;             #'lsp-booster--advice-json-parse)

    ;; (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    ;;   "Prepend emacs-lsp-booster command to lsp CMD."
    ;;   (let ((orig-result (funcall old-fn cmd test?)))
    ;;     (if (and (not test?)                             ;; for check lsp-server-present?
    ;;              (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
    ;;              lsp-use-plists
    ;;              (not (functionp 'json-rpc-connection))  ;; native json-rpc
    ;;              (executable-find "emacs-lsp-booster"))
    ;;         (progn
    ;;           (message "Using emacs-lsp-booster for %s!" orig-result)
    ;;           (cons "emacs-lsp-booster" orig-result))
    ;;       orig-result)))
    ;; (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
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
  ;; Pre-compute available grammars once at startup to avoid slow runtime checks
  ;; on every file open. This scans once instead of on each set-auto-mode call.
  ;; This gives a MAJOR boost in increasing the time it takes to load files!!
  (setq treesit-auto-langs
        (cl-loop for recipe in treesit-auto-recipe-list
                 when (treesit-language-available-p (treesit-auto-recipe-lang recipe))
                 collect (treesit-auto-recipe-lang recipe)))
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
(setq org-directory                             (concat (file-name-as-directory user-home-directory) "OneDrive/org")
      org-agenda-files                          (list org-directory)
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
(with-eval-after-load 'org
  (require 'org-mouse))
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
(after! org-roam
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error ""))))
```

```emacs-lisp
(after! org-roam
  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag))))
```

Ensure org-roam database is properly initialized and maintained:

```emacs-lisp
(after! org-roam
  ;; Enable autosync lazily when first accessing org-roam
  (add-hook 'org-roam-mode-hook #'org-roam-db-autosync-enable)

  ;; Ensure database is updated when org-roam files are saved
  (add-hook 'after-save-hook
            (lambda ()
              (when (and (derived-mode-p 'org-mode)
                         (org-roam-file-p))
                (org-roam-db-update-file)))))
```

Add all `org-roam` files to list of extra files to be searched by text commands.

```emacs-lisp
(after! (org org-roam)
  ;; Set org-roam files lazily to avoid startup delays
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (unless org-agenda-text-search-extra-files
                (setq org-agenda-text-search-extra-files (org-roam--list-files org-roam-directory))))))
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
      '(outline-7 :weight semi-bold :height 1.01)
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
        ;; ;; (setf (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
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
Also install `marked` in order to support more than basic markdown functionality.

```shell
$ npm install -g marked
```

```emacs-lisp
(package! markdown-xwidget :pin "223e699"
  :recipe (:host github :repo "rhblind/markdown-xwidget" :files (:defaults "resources")))
```

```emacs-lisp
(use-package! markdown-xwidget
  :after markdown-mode

  :init
  (map! :map markdown-mode-map
        :localleader
        "p" #'markdown-xwidget-preview-mode)
  :config
  (setq markdown-xwidget-mermaid-theme "default"
        markdown-xwidget-code-block-theme "github"))
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
(defun cust/typescript-lsp-maybe ()
  "Start LSP for TypeScript only if not in a Deno project.
Deno projects use deno-ls via their own mode hooks."
  (unless (deno-ts-project-p)
    (lsp!)))

(defun cust/typescript-prettier-maybe ()
  "Enable prettier-mode only if not in a Deno project."
  (unless (deno-ts-project-p)
    (prettier-mode 1)))

(use-package! typescript-ts-mode
  :hook (typescript-ts-mode . cust/typescript-prettier-maybe)
  :config
  (add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook) #'cust/typescript-lsp-maybe))
```


#### Deno {#deno}

This section configures support for Deno, inspired by [this post](https://www.mgmarlow.com/words/2023-08-31-deno-tree-sitter-emacs/).

First we need a way to know if we're in a Deno project or not.

```emacs-lisp
(defun deno-ts-project-p ()
  "Return non-nil if current buffer is in a Deno project.
Checks for deno.json or deno.jsonc in any parent directory."
  (let ((dir (or (and buffer-file-name (file-name-directory buffer-file-name))
                 default-directory)))
    (or (locate-dominating-file dir "deno.json")
        (locate-dominating-file dir "deno.jsonc"))))
```

Override the default apheleia deno formatters to include the `--config` flag.
This ensures `deno fmt` finds the project's `deno.json` with formatting options like `singleQuote`.

```emacs-lisp
(after! apheleia
  (setf (alist-get 'denofmt-ts apheleia-formatters)
        '("deno" "fmt" "-" "--ext" "ts"
          (when-let ((config (locate-dominating-file default-directory "deno.json")))
            (list "--config" (expand-file-name "deno.json" config)))))
  (setf (alist-get 'denofmt-tsx apheleia-formatters)
        '("deno" "fmt" "-" "--ext" "tsx"
          (when-let ((config (locate-dominating-file default-directory "deno.json")))
            (list "--config" (expand-file-name "deno.json" config)))))
  (setf (alist-get 'denofmt-js apheleia-formatters)
        '("deno" "fmt" "-" "--ext" "js"
          (when-let ((config (locate-dominating-file default-directory "deno.json")))
            (list "--config" (expand-file-name "deno.json" config)))))
  (setf (alist-get 'denofmt-json apheleia-formatters)
        '("deno" "fmt" "-" "--ext" "json"
          (when-let ((config (locate-dominating-file default-directory "deno.json")))
            (list "--config" (expand-file-name "deno.json" config)))))
  (setf (alist-get 'denofmt-md apheleia-formatters)
        '("deno" "fmt" "-" "--ext" "md"
          (when-let ((config (locate-dominating-file default-directory "deno.json")))
            (list "--config" (expand-file-name "deno.json" config))))))
```

Define new major modes for Deno. We'll activate these modes if we're in a Deno project.

```emacs-lisp
;; Deno TypeScript mode (for .ts files)
(define-derived-mode deno-ts-mode typescript-ts-mode "Deno[TS]"
  "Major mode for Deno TypeScript files."
  :group 'deno
  (setq-local apheleia-formatter 'denofmt-ts)
  ;; Disable TypeScript LSP servers - we use deno-ls instead
  (setq-local lsp-disabled-clients '(ts-ls jsts-ls)))

;; Deno TSX mode (for .tsx files)
(define-derived-mode deno-tsx-mode tsx-ts-mode "Deno[TSX]"
  "Major mode for Deno TSX files."
  :group 'deno
  (setq-local apheleia-formatter 'denofmt-tsx)
  (setq-local lsp-disabled-clients '(ts-ls jsts-ls)))

;; Deno JavaScript mode (for .js files)
(define-derived-mode deno-js-mode js-ts-mode "Deno[JS]"
  "Major mode for Deno JavaScript files."
  :group 'deno
  (setq-local apheleia-formatter 'denofmt-js)
  (setq-local lsp-disabled-clients '(ts-ls jsts-ls)))

;; Deno JSON mode (for .json files)
(define-derived-mode deno-json-mode json-ts-mode "Deno[JSON]"
  "Major mode for Deno JSON files."
  :group 'deno
  (setq-local apheleia-formatter 'denofmt-json))

;; Deno Markdown mode (for .md files)
(define-derived-mode deno-md-mode markdown-mode "Deno[MD]"
  "Major mode for Deno Markdown files."
  :group 'deno
  (setq-local apheleia-formatter 'denofmt-md))

;; Auto-switch to Deno modes after the base mode is set
;; This runs after treesit-auto has set up the tree-sitter mode
(defun deno--maybe-activate-deno-mode ()
  "Switch to appropriate Deno mode if in a Deno project."
  (when (deno-ts-project-p)
    (pcase major-mode
      ('typescript-ts-mode (deno-ts-mode))
      ('tsx-ts-mode (deno-tsx-mode))
      ('js-ts-mode (deno-js-mode))
      ('json-ts-mode (deno-json-mode))
      ('markdown-mode (deno-md-mode)))))

(add-hook 'after-change-major-mode-hook #'deno--maybe-activate-deno-mode)

;; Register Deno LSP server
(defun deno--register-lsp-client ()
  "Register the Deno LSP client if not already properly registered."
  (require 'lsp-mode)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda () (list (or (executable-find "deno") "deno") "lsp")))
    :major-modes '(deno-ts-mode deno-tsx-mode deno-js-mode)
    :priority 10
    :server-id 'deno-ls
    :initialization-options (lambda () (list :clientInfo (list :name "deno-ls")))
    :notification-handlers (lsp-ht ("window/logMessage" 'ignore))
    :add-on? nil)))

(defun deno--lsp-start ()
  "Ensure Deno LSP client is registered and start LSP."
  (deno--register-lsp-client)
  (lsp))

(add-hook! '(deno-ts-mode-hook deno-tsx-mode-hook deno-js-mode-hook) #'deno--lsp-start)
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


### Elixir {#elixir}

Use the latest and greatest!

```emacs-lisp
(package! flycheck-dialyxir)
```


#### Flycheck integration {#flycheck-integration}

```emacs-lisp
;; Override the `lsp-credo-version' variable to get the latest version.
;; It has to be set before `lsp-credo.el' is loaded.
(custom-set-variables '(lsp-credo-version "0.3.0"))
(use-package! flycheck-dialyxir
  :when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
  :after elixir-mode
  :config (flycheck-dialyxir-setup))
```


#### Dexter LSP {#dexter-lsp}

[Dexter](https://github.com/remoteoss/dexter) is Remote's take on an LSP server for Elixir written in Go.

Install Dexter globally using mise:

```shell
mise plugin add dexter https://github.com/remoteoss/dexter.git && mise use -g dexter@latest
```

Remember to add `.dexter.db*` to your `.gitignore` file.

```emacs-lisp
(after! lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda () (list (string-trim (shell-command-to-string "mise which dexter")) "lsp")))
    :major-modes '(elixir-mode elixir-ts-mode heex-ts-mode)
    :priority 2  ; Higher priority than expert-ls, so dexter is preferred
    :server-id 'dexter-ls
    :initialization-options (lambda ()
                             (list :followDelegates t
                                   :debug nil))
    :notification-handlers (lsp-ht ("window/logMessage" 'ignore))
    :add-on? nil)))

;; Note: The hook for lsp is already set up for elixir-ts-mode after expert-ls configuration
```


#### Expert LSP {#expert-lsp}

The official language server for Elixir. Kept as a fallback but disabled from auto-starting
so dexter-ls is always used. Enable manually with `M-x lsp` if needed.

```emacs-lisp
(after! lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda () (list (concat user-home-directory ".local/bin/expert_darwin_arm64") "--stdio")))
    :major-modes '(elixir-mode elixir-ts-mode heex-ts-mode)
    :priority 1
    :server-id 'expert-ls
    :initialization-options (lambda () (list :clientInfo (list :name "expert-ls")))
    :notification-handlers (lsp-ht ("window/logMessage" 'ignore))
    :add-on? nil))
  )

(add-hook! 'elixir-ts-mode-hook #'lsp)
```


#### Formatting {#formatting}

Configure `mix format` to run from the project root where `.formatter.exs` lives.
This ensures it respects project-specific formatting settings like line length.

CRITICAL: We MUST redirect stderr to prevent compilation output from overwriting files.
We save stdin to a temp file, format it, and output the result to ensure clean handling.

```emacs-lisp
(after! apheleia
  (setf (alist-get 'mix-format apheleia-formatters)
        '("sh" "-c"
          "t=$(mktemp); cat > \"$t\"; cd $(git rev-parse --show-toplevel 2>/dev/null || pwd) && mix format \"$t\" 2>/dev/null && cat \"$t\"; rm -f \"$t\"")))
```


### Erlang {#erlang}


#### Formatting {#formatting}

Configure apheleia to use `rebar3 fmt` for Erlang formatting. The formatter must run from
the project root (where `rebar.config` lives), so we use `apheleia-from-project-root`.

```emacs-lisp
(after! apheleia
  (setf (alist-get 'erlfmt apheleia-formatters)
        '("apheleia-from-project-root" "rebar.config" "rebar3" "fmt" "-")))

(add-hook! 'erlang-mode-hook #'apheleia-mode)
```


#### ELP (Erlang Language Platform LSP Server) {#elp--erlang-language-platform-lsp-server}

```emacs-lisp
  (after! lsp-mode
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection
                       (lambda () (list (concat user-home-directory ".local/bin/elp") "server")))
      :major-modes '(erlang-mode)
      :priority 1
      :server-id 'elp)))

  (add-hook! 'erlang-mode-hook #'lsp)
```


### CSharp {#csharp}

Remap `csharp-mode` to the tree-sitter equivalent.

```emacs-lisp
(add-list-to-list 'major-mode-remap-alist '((csharp-mode . csharp-ts-mode)))
```

```emacs-lisp
;; Disable prettify-symbols-mode for C# modes
(defun +csharp--disable-prettify-symbols-a (&rest _)
  "Prevent prettify-symbols-mode from being enabled in C# buffers."
  (not (or (eq major-mode 'csharp-mode)
           (eq major-mode 'csharp-ts-mode))))

(advice-add 'prettify-symbols-mode :before-while #'+csharp--disable-prettify-symbols-a)
```

Install `CSharpier` globally or as a local tool:

```shell
$ dotnet tool install -g csharpier        # global
$ dotnet tool install csharpier           # local (uses .config/dotnet-tools.json)
```

```emacs-lisp
(after! apheleia
  ;; Override csharpier to use `dotnet csharpier` for local tool support.
  (setf (alist-get 'csharpier apheleia-formatters)
        '("dotnet" "csharpier" "format" "--write-stdout")))
```

C-sharp is supported by default in newer versions of Emacs through `csharp-tree-sitter-mode`.
Install the `dotnet` package as well to get some extra goodies.

```emacs-lisp
(package! dotnet)
```

Execute `M-x treesit-install-language-grammar` and enter `c-sharp`.  When prompted if you want to do it interactively, enter "Yes",
and paste the Github repository (`tree-sitter/tree-sitter-c-sharp`) for the grammar files.
Just keep pressing `<enter>` to accept defaults for the rest of the process.


#### LSP Dotnet {#lsp-dotnet}

Use `csharp-ls` as the language server. If a project has `csharp-ls` installed as a local dotnet tool
(in `.config/dotnet-tools.json`), use that version. Otherwise, fall back to the globally installed
version at `~/.local/bin/csharp-ls`.

```emacs-lisp
(after! lsp-mode
  (defun +csharp--project-has-local-csharp-ls-p ()
    "Check if the current project has csharp-ls as a local dotnet tool."
    (when-let* ((project-root (or (lsp-workspace-root)
                                  (locate-dominating-file default-directory ".sln")
                                  (locate-dominating-file default-directory ".csproj")))
                (tools-file (expand-file-name ".config/dotnet-tools.json" project-root)))
      (when (file-exists-p tools-file)
        (condition-case nil
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (content (json-read-file tools-file))
                   (tools (alist-get 'tools content)))
              (assoc 'csharp-ls tools))
          (error nil)))))

  (defun +csharp--cls-find-executable ()
    "Find csharp-ls executable, preferring project-local over global.
If project has csharp-ls as a local dotnet tool, use `dotnet tool run csharp-ls`.
Otherwise, use the global installation at ~/.local/bin/csharp-ls."
    (if (+csharp--project-has-local-csharp-ls-p)
        (list "dotnet" "tool" "run" "csharp-ls")
      (expand-file-name "~/.local/bin/csharp-ls")))

  ;; Override the default executable finder
  (advice-add 'lsp-csharp--cls-find-executable :override #'+csharp--cls-find-executable))

;; Use lsp-deferred to avoid blocking file loading - LSP starts when Emacs is idle
(add-hook! 'csharp-ts-mode-hook #'lsp-deferred)
```

```emacs-lisp
(use-package! dotnet
  :defer t  ; Don't load until explicitly needed
  :commands (dotnet-mode dotnet-run dotnet-build dotnet-test)
  :config
  (setq dotnet-project-search-max-depth 5)) ; Reduce search depth for faster lookups

;; Defer dotnet-mode activation to idle time
(defun +csharp--setup-dotnet-mode-deferred ()
  "Set up dotnet-mode after a short delay to avoid blocking file loading."
  (run-with-idle-timer
   1.0 nil
   (lambda ()
     (when (and (buffer-live-p (current-buffer))
                (derived-mode-p 'csharp-ts-mode 'csharp-mode))
       (dotnet-mode 1)
       ;; Set project directory lazily
       (setq-local dotnet-project-directory
                   (or (locate-dominating-file default-directory ".sln")
                       (locate-dominating-file default-directory ".csproj")
                       default-directory))))))

(add-hook! 'csharp-ts-mode-hook #'+csharp--setup-dotnet-mode-deferred)

(add-to-list 'auto-mode-alist
             '("\\.csproj\\'" . csproj-mode))
```


#### Keybindings {#keybindings}

Add the `sharper-main-transient` menu to local leader.

```emacs-lisp
(map! :map csharp-ts-mode-map
      :after csharp-ts-mode
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


#### LSP Rust {#lsp-rust}

Auto-detect `Cargo.toml` by walking up from the current file. This handles Rust crates nested inside
non-Rust projects (e.g. a NIF inside an Elixir umbrella).

```emacs-lisp
(defun cust/find-cargo-toml ()
  "Walk up from `buffer-file-name' and return the nearest Cargo.toml path, or nil."
  (when-let ((file (buffer-file-name)))
    (let ((dir (locate-dominating-file (file-name-directory file) "Cargo.toml")))
      (when dir
        (expand-file-name "Cargo.toml" dir)))))

(after! (rustic lsp-mode)
  (add-hook 'rustic-mode-hook
            (lambda ()
              (when-let ((cargo (cust/find-cargo-toml)))
                (setq lsp-rust-analyzer-linked-projects (vector cargo))))
            -90))
```

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

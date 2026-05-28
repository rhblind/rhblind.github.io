+++
title = "Scratch Emacs Configuration"
author = ["Rolf Håvard Blindheim"]
lastmod = 2026-05-28T22:15:50+02:00
tags = ["org-mode"]
categories = ["emacs", "scratch"]
draft = false
toc = true
aliases = "/posts/emacs-configuration"
+++

<a href="https://github.com/rhblind/.scratch.d" target="_blank" rel="noopener" title="Github" style="display: inline-flex; gap: 0.5em;">
  <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" style="vertical-align: sub;">
    <path d="M9 19c-5 1.5-5-2.5-7-3m14 6v-3.87a3.37 3.37 0 0 0-.94-2.61c3.14-.35 6.44-1.54 6.44-7A5.44 5.44 0 0 0 20 4.77 5.07 5.07 0 0 0 19.91 1S18.73.65 16 2.48a13.38 13.38 0 0 0-7 0C6.27.65 5.09 1 5.09 1A5.07 5.07 0 0 0 5 4.77a5.44 5.44 0 0 0-1.5 3.78c0 5.42 3.3 6.61 6.44 7A3.37 3.37 0 0 0 9 18.13V22" />
  </svg>View on GitHub
</a>


## Introduction {#introduction}

I've been running Emacs for a number of years and used a few different starter kits.
I started with [Spacemacs](https://www.spacemacs.org/), then I moved over to [Doom](https://github.com/doomemacs/doomemacs). I liked them both very much and they
really helped me getting into Emacs.

A while ago I decided that I wanted to write my own config from scratch, taking with me the
habits (good and bad) I've accumulated over the years. Since I've become very accustomed
to the great user experience provided by Doom Emacs, I decided to write my new config as a
personal Doom inspired micro-framework called [Scratch Emacs](https://github.com/rhblind/scratch-emacs).

This file is my personal `~/.scratch.d/config.org` file. It contains additional config to what's included in Scratch,
as well as notes, TODOs, FIXMEs and other useful information I like to keep close to my Emacs config.


## Installing Emacs {#installing-emacs}

Emacs can be installed in many ways, but these days I'm usually on a Mac so I just use [Homebrew](https://brew.sh).
There's a couple of Emacs packages in Homebrew, but I'm using the [emacs-mac](https://github.com/railwaycat/homebrew-emacsmacport) version.


### Fix annoying max open files for Emacs {#fix-annoying-max-open-files-for-emacs}

When using LSP servers with Emacs, it’s easy to hit the “max open files” error. Emacs doesn’t seem to use operating system ulimit, but uses pselect which is limited to FD_SETSIZE file descriptors, usually 1024. This means that changing ulimit will not change the value of FD_SETSIZE compiled into Emacs and macOS libraries. To overcome this limitation we’ve to set the FD_SETSIZE CFLAG when compiling Emacs.

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


## Getting Started {#getting-started}

After installing Emacs, follow the installation [instructions for Scratch Emacs](https://github.com/rhblind/scratch-emacs#install).
Next clone this repository to `~/.scratch.d` and run `scratch sync` and `scratch env` to complete the setup.


## Configuration {#configuration}

```emacs-lisp
;;; config.el -*- lexical-binding: t; -*-
```


### Identity {#identity}

```emacs-lisp
(setq user-full-name    "Rolf Håvard Blindheim"
      user-mail-address "rhblind@gmail.com")

(defconst user-home-directory
  (expand-file-name (file-name-as-directory "~"))
  "Absolute path to the current user's home directory.
Some of the config below builds paths relative to this -- handier
than scattering ~ expansions everywhere.")
```


### Better defaults {#better-defaults}

Settings that the framework doesn't already cover (or that I want
tweaked further) -- editor + file + display behaviour. Things like
`vc-follow-symlinks`, `which-key-idle-delay`, `smartparens-global-mode`,
`ws-butler-global-mode` and the `pixel-scroll-precision` stack are
already framework defaults via the corresponding modules.

```emacs-lisp
(setq auto-revert-use-notify         t            ; file-system notifications, not polling
      auto-revert-interval           1            ; fallback poll interval (seconds)
      auto-revert-avoid-polling      t            ; prefer notifications over polling
      revert-without-query           '(".*")      ; never prompt; always revert silently
      delete-by-moving-to-trash      t            ; deletions go to the OS trash
      confirm-kill-emacs             nil          ; just close, don't ask twice
      display-time-24hr-format       t
      evil-want-fine-undo            t            ; granular undos in evil insert state
      evil-ex-substitute-global      t            ; default `:s' to global, no /g needed
      fill-column                    120          ; we have screen real estate
      history-length                 1000
      warning-minimum-level          :error       ; suppress noisy *Warnings* on info / warning
      sh-shell                       "sh"
      undo-limit                     (* 16 1024 1024)  ; 16 MB undo
      window-combination-resize      t            ; new windows take space from all neighbours
      window-divider-default-right-width 3        ; thicker dividers
      x-stretch-cursor               t            ; stretch block cursor to glyph width
      diff-hl-flydiff-delay          2)           ; throttle diff-hl recompute
```

Modes to keep on globally:

```emacs-lisp
(diff-hl-flydiff-mode      1)        ; async diff-hl updates
(display-time-mode         1)        ; clock in the modeline
(global-goto-address-mode  1)        ; render URLs as clickable
(global-subword-mode       1)        ; CamelCase = multiple "words"
(window-divider-mode       1)        ; visible vertical / horizontal dividers

;; Wrap long lines at the window edge in prose-y modes.
(dolist (hook '(text-mode-hook comint-mode-hook))
  (add-hook hook #'visual-line-mode))
```


#### Hack: clear file-notify watches periodically {#hack-clear-file-notify-watches-periodically}

On macOS I sometimes hit "Too many open files" because Emacs's
file-watch descriptors don't respect `ulimit` or `sysctl
kern.maxfiles`. Workaround: drop all file-notify watches every five
minutes. The cost is rebuilding watches on next save, which is
imperceptible.

```emacs-lisp
(require 'filenotify)

(defun my/file-notify-rm-all-watches (&optional silent)
  "Drop all `file-notify' watches. Reset for the \"too many open files\" bug."
  (interactive)
  (if silent
      (maphash (lambda (k _) (file-notify-rm-watch k)) file-notify-descriptors)
    (let ((rep (make-progress-reporter "Clearing file-notify watches... ")))
      (maphash (lambda (k _) (file-notify-rm-watch k)) file-notify-descriptors)
      (progress-reporter-done rep))))

(run-with-timer 0 (* 5 60) #'my/file-notify-rm-all-watches t)
```


### GPG, SSH and encryption {#gpg-ssh-and-encryption}

Keep auth secrets in `~/.authinfo.gpg` (lives in my dotfiles repo --
copy in place on a new machine). Use `gpg-agent` as the SSH agent
too, and let Emacs handle the pinentry prompt in the minibuffer
(loopback mode) so encryption flows don't break in TTY frames.

The `pinentry` package is required for `pinentry-start`; install in
the eager-packages block at the bottom of this file.

```emacs-lisp
(straight-use-package 'pinentry)
```

```emacs-lisp
(with-eval-after-load 'epa
  (setq auth-source-cache-expiry                       nil
        epa-file-select-keys                           nil
        epa-file-cache-passphrase-for-symmetric-encryption t
        epg-pinentry-mode                              'loopback
        epg-gpg-program
        (cond ((eq system-type 'darwin)     "/opt/homebrew/bin/gpg")
              ((eq system-type 'gnu/linux)  "/usr/bin/gpg")
              ((eq system-type 'windows-nt) "C:/Program Files (x86)/GNU/GnuPG/gpg2")
              (t                            "gpg"))))

;; Start a pinentry server so GPG passphrase prompts land in the
;; minibuffer instead of spawning a graphical pinentry. Requires
;; `allow-loopback-pinentry' in ~/.gnupg/gpg-agent.conf.
(when (require 'pinentry nil t)
  (pinentry-start)
  ;; Prevent async timers / process filters from aborting the PIN
  ;; prompt. Without this, anything that touches the minibuffer
  ;; while `read-passwd' is active kills the pinentry (because
  ;; `enable-recursive-minibuffers' defaults to nil).
  (defun my/pinentry-no-interrupt (orig-fn &rest args)
    "Prevent async minibuffer access from aborting PIN entry."
    (let ((enable-recursive-minibuffers t)
          (inhibit-message t))
      (apply orig-fn args)))
  (advice-add 'pinentry--prompt :around #'my/pinentry-no-interrupt))

;; gpg-agent's notion of "current TTY" goes stale across SSH /
;; tmux / Emacs daemon restarts. Refresh it at startup and before
;; every magit commit (the most common trigger for a passphrase
;; prompt) so signing doesn't fall over with "Inappropriate ioctl".
(defun my/gpg-update-tty ()
  "Tell gpg-agent the current TTY so pinentry can attach to it."
  (call-process "gpg-connect-agent" nil nil nil
                "updatestartuptty" "/bye"))

(my/gpg-update-tty)

(with-eval-after-load 'magit
  (advice-add 'magit-commit-create :before
              (lambda (&rest _) (my/gpg-update-tty))))
```


### Project discovery {#project-discovery}

`~/workspace` is the parent directory holding all my git repos. Listing
it as a discovery root makes the framework auto-register every git
repo found under it (up to `scratch-projects-search-depth` levels)
on `emacs-startup-hook`, so `SPC p p` surfaces them without having
to `SPC p A` each one manually.

```emacs-lisp
(setq scratch-projects-search-path '("~/workspace"))
```


### evil (personal tweaks) {#evil--personal-tweaks}

Personal evil rebindings on top of the framework's `:editor evil`
defaults. `H` / `L` become first-non-blank / end-of-visual-line
(easier to reach than `^` / `$`), `0` becomes a paren-jump (Doom's
default `%` replacement).

The `end-of-visual-line` wrapper lives in the framework
(`scratch-evil/end-of-visual-line`); only the key bindings are personal.

```emacs-lisp
(with-eval-after-load 'evil
  (evil-define-key '(visual motion) 'global
    "H" #'evil-first-non-blank
    "L" #'scratch-evil/end-of-visual-line
    "0" #'evil-jump-item))
```


### marginalia (personal tweaks) {#marginalia--personal-tweaks}

Two add-ons over the framework's `:completion vertico` defaults:

1.  Show variable values verbatim in `M-x describe-variable` etc.
    instead of marginalia's auto-mask. Trade-off: easier to inspect,
    but values matching auth-source / password / secret patterns
    render in the clear (mind your screenshots / screen-shares).
2.  Color-code the size and modification-time columns in file pickers
    (`SPC f f`, `consult-buffer`, ...) so newer / smaller files glow,
    older / larger files fade. `doom-blend` replaced with a builtin
    `color.el`-based blender so we don't pull Doom code.

<!--listend-->

```emacs-lisp
(defun scratch-color-blend (c1 c2 alpha)
  "Blend color strings C1 and C2 by ALPHA (0 = pure C1, 1 = pure C2).
Returns a `#rrggbb' hex string."
  (require 'color)
  (cl-destructuring-bind (r1 g1 b1) (color-name-to-rgb c1)
    (cl-destructuring-bind (r2 g2 b2) (color-name-to-rgb c2)
      (color-rgb-to-hex (+ (* r1 (- 1 alpha)) (* r2 alpha))
                        (+ (* g1 (- 1 alpha)) (* g2 alpha))
                        (+ (* b1 (- 1 alpha)) (* b2 alpha))
                        2))))

(with-eval-after-load 'marginalia
  (setq marginalia-censor-variables nil)

  (defun scratch-marginalia--time-colorful (time)
    "Foreground for TIME blended between marginalia-date (fresh) and
marginalia-documentation (old) -- log scale on age in days."
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (scratch-color-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun scratch-marginalia--file-size-colorful (size)
    "Foreground for SIZE: green (small) -> orange (medium) -> red (huge).
Log10-scaled so each order of magnitude shifts the hue noticeably."
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 1.0)
                      (scratch-color-blend "green" "orange" size-index)
                    ;; > ~10MB: continue blending orange -> red.
                    (scratch-color-blend "orange" "red"
                                         (min 1.0 (- size-index 1.0))))))
      (propertize (file-size-human-readable size)
                  'face (list :foreground color))))

  (advice-add 'marginalia--annotate-local-file :override
              #'scratch-marginalia--annotate-local-file)

  (defun scratch-marginalia--annotate-local-file (cand)
    "Colorful override of `marginalia--annotate-local-file'."
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((scratch-marginalia--file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((scratch-marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12)))))
```


### vterm (personal tweaks) {#vterm--personal-tweaks}

Layered on top of the framework's `:term vterm` defaults (which
already handle `TAB` completion, `M-/C-<backspace>` word delete, and
`vterm-copy-mode` evil states). Personal preferences:

-   `vterm-timer-delay nil` -- snappier redraw, no batched output buffering.
-   Override `vterm-send-return` to emit CR (`\C-m`) instead of LF.
    Some shells / TTY setups (`icrnl=t`) expect CR on enter; LF causes
    the cursor to move down without executing the line.
-   `C-j` / `C-k` as down / up arrow (vim-style scrollback navigation
    in shells).
-   `C-<delete>` / `M-<delete>` as delete-word-forward (sends `M-d`).
-   `M-<return>` as literal newline (multi-line shell input).
-   `C-g` cancels the running shell command, except inside
    `claude-code-ide` buffers where C-g would trigger "open in editor"
    -- there it sends ESC instead.

<!--listend-->

```emacs-lisp
(with-eval-after-load 'vterm
  (setq vterm-timer-delay nil)
  (defun vterm-send-return ()
    "Send CR to the libvterm process (override)."
    (interactive)
    (deactivate-mark)
    (when vterm--term
      (process-send-string vterm--process "\C-m")))
  (with-eval-after-load 'evil
    (map! :map vterm-mode-map
      :i "C-j"           (lambda () (interactive) (vterm-send-key "<down>"))
      :i "C-k"           (lambda () (interactive) (vterm-send-key "<up>"))
      :n "C-j"           (lambda () (interactive) (vterm-send-key "<down>"))
      :n "C-k"           (lambda () (interactive) (vterm-send-key "<up>"))
      :i "C-<delete>"    (lambda () (interactive) (vterm-send-key "d" nil t))
      :n "C-<delete>"    (lambda () (interactive) (vterm-send-key "d" nil t))
      :i "M-<delete>"    (lambda () (interactive) (vterm-send-key "d" nil t))
      :n "M-<delete>"    (lambda () (interactive) (vterm-send-key "d" nil t))
      :i "M-<return>"    (lambda () (interactive)
                           (process-send-string vterm--process "\n"))
      :n "M-<return>"    (lambda () (interactive)
                           (process-send-string vterm--process "\n"))
      :i "C-g"           (lambda () (interactive)
                           (if (string-prefix-p "*claude-code[" (buffer-name))
                               (vterm-send-escape)
                             (process-send-string vterm--process "\C-g")))
      :n "C-g"           (lambda () (interactive)
                           (if (string-prefix-p "*claude-code[" (buffer-name))
                               (vterm-send-escape)
                             (process-send-string vterm--process "\C-g"))))))
```


### Org root {#org-root}

Top-level org settings that the rest of the org / org-roam / capture
config keys off (\`org-roam-directory' under \`org-directory', etc.).

```emacs-lisp
(setq org-directory                  (expand-file-name "OneDrive/org" (getenv "HOME"))
      org-agenda-files               (list org-directory)
      ;; Insertion-time link IDs survive heading rename and refile
      ;; better than name-based links.
      org-id-link-to-org-use-id      t
      ;; Predefined tag for org-crypt-protected subtrees (the
      ;; `org-crypt' package itself isn't enabled here; tag is harmless
      ;; without it and ready to use if/when you turn it on).
      org-tag-alist                  '(("crypt" . ?c))
      ;; Roam already broadens the agenda extra-files via the hook
      ;; below; start from a clean slate.
      org-agenda-text-search-extra-files '())

(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "|" "WAITING" "DONE" "KILL" "SOMEDAY")))
```


### Org crypt {#org-crypt}

GPG-encrypt subtrees tagged `:crypt:` on save (the tag is already in
`org-tag-alist` above; press `,q` in an org buffer and add `crypt` to a
heading). `org-crypt` ships with org, so no extra install.

```emacs-lisp
(with-eval-after-load 'org
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  ;; Don't propagate the `crypt' tag to child headings (otherwise
  ;; encrypting a parent encrypts every descendant whose own tag is
  ;; inherited).
  (setq org-tags-exclude-from-inheritance '("crypt")
        ;; Encrypt to this GPG identity. `nil' would prompt every save.
        org-crypt-key "rhblind@gmail.com"))
```


### ox-hugo {#ox-hugo}

Auto-export-on-save for blog files under `org-hugo-base-dir`.
The package itself and `use-package` setup come from the framework's
`+hugo` flag.

```emacs-lisp
(add-hook 'org-mode-hook
          (lambda ()
            (when (and (boundp 'org-hugo-base-dir)
                       (buffer-file-name)
                       (string-prefix-p (file-truename org-hugo-base-dir)
                                        (file-truename (buffer-file-name))))
              (org-hugo-auto-export-mode 1))))
```


### org-cliplink (personal tweak) {#org-cliplink--personal-tweak}

Framework already installs and autoloads `org-cliplink`. Personal
preference: shell out to curl for the title fetch (more robust on
flaky TLS / redirects than url.el).

```emacs-lisp
(with-eval-after-load 'org-cliplink
  (setq org-cliplink-transport-implementation 'curl))
```


### Org agenda {#org-agenda}

Personal "o" Overview command with project-specific super-agenda groups.
Base agenda settings (olivetti, org-super-agenda, prefix format, skip
rules, visual polish) come from the framework's `+pretty` flag.

```emacs-lisp
(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :scheduled today
                            :order 1)
                           (:name "Overdue"
                            :deadline past
                            :face error
                            :order 2)
                           (:name "Due Soon"
                            :deadline future
                            :order 3)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-agenda-todo-ignore-deadlines 'past)
                         (org-agenda-todo-ignore-scheduled 'past)
                         (org-super-agenda-groups
                          '((:name "Next"        :todo "NEXT"     :order 1)
                            (:name "Important"   :priority "A"    :order 4)
                            (:name "Work"        :tag "work"      :order 10)
                            (:name "Personal"    :tag "personal"  :order 11)
                            (:name "Interesting" :tag ("read" "idea") :order 20)
                            (:name "Waiting"     :todo "WAITING"  :order 30)
                            (:name "Someday"     :todo "SOMEDAY"  :order 90)
                            (:discard (:tag ("Routine" "Daily")))
                            (:name "Inbox"       :anything t      :order 99))))))))))
```


### Capture templates {#capture-templates}

Pretty capture templates using `doct` (provided by `+pretty`) with
nerd-icon decorations. The `:icon` property on each group is
automatically converted to a glyph by the framework.

```emacs-lisp
(defvar scratch-org-capture-todo-file
  (expand-file-name "todo.org" org-directory))

(with-eval-after-load 'org-capture
  (setq org-capture-templates
        (doct `(("Todo" :keys "t"
                 :icon ("nf-oct-checklist" :set "octicon" :color "green")
                 :file scratch-org-capture-todo-file
                 :prepend t
                 :headline "Inbox"
                 :type entry
                 :template ("* TODO %? %^G%{extra}"
                            "%i")
                 :custom (:extra "")
                 :children (("Todo" :keys "t"
                             :icon ("nf-oct-checklist" :set "octicon" :color "green")
                             :extra "")
                            ("With deadline" :keys "d"
                             :icon ("nf-oct-clock" :set "octicon" :color "orange")
                             :extra "\nDEADLINE: %^{Deadline:}t")
                            ("Scheduled" :keys "s"
                             :icon ("nf-oct-calendar" :set "octicon" :color "orange")
                             :extra "\nSCHEDULED: %^{Start time:}t")))
                ("Note" :keys "n"
                 :icon ("nf-fa-sticky_note_o" :set "faicon" :color "green")
                 :file scratch-org-capture-todo-file
                 :prepend t
                 :headline "Inbox"
                 :type entry
                 :template ("* %? %^G"
                            "%i"))
                ("Interesting" :keys "i"
                 :icon ("nf-fa-eye" :set "faicon" :color "lcyan")
                 :file scratch-org-capture-todo-file
                 :prepend t
                 :headline "Interesting"
                 :type entry
                 :template ("* [ ] %{desc}%? :%{i-type}:"
                            "%i")
                 :children (("Webpage" :keys "w"
                             :icon ("nf-fa-globe" :set "faicon" :color "green")
                             :desc "%(org-cliplink-capture) "
                             :i-type "read")
                            ("Idea" :keys "i"
                             :icon ("nf-fa-lightbulb" :set "faicon" :color "purple")
                             :desc ""
                             :i-type "idea")))
                ("Project" :keys "p"
                 :icon ("nf-oct-repo" :set "octicon" :color "purple")
                 :prepend t
                 :type entry
                 :template ("* %{time-or-todo} %?"
                            "%i"
                            "%a")
                 :file ""
                 :custom (:time-or-todo "")
                 :children (("Project todo" :keys "t"
                             :icon ("nf-oct-checklist" :set "octicon" :color "green")
                             :time-or-todo "TODO"
                             :headline "TODO"
                             :file scratch-org-project-todo-file)
                            ("Project note" :keys "n"
                             :icon ("nf-fa-sticky_note_o" :set "faicon" :color "yellow")
                             :time-or-todo "%U"
                             :headline "Notes"
                             :file scratch-org-project-notes-file)
                            ("Project changelog" :keys "c"
                             :icon ("nf-oct-list_unordered" :set "octicon" :color "blue")
                             :time-or-todo "%t"
                             :headline "Unreleased"
                             :file scratch-org-project-changelog-file)))
                ,@(when scratch-org-capture-blog-file
                    `(("Blog" :keys "b"
                       :icon ("nf-fa-pied_piper" :set "faicon" :color "pink")
                       :file scratch-org-capture-blog-file
                       :prepend t
                       :type entry
                       :template ("* %{title} %^G"
                                  ":PROPERTIES:"
                                  ":EXPORT_FILE_NAME: %\\1"
                                  ":EXPORT_DESCRIPTION: %^{Description}"
                                  ":EXPORT_DATE: %^{Date}t"
                                  ":EXPORT_LASTMOD: %t"
                                  ":EXPORT_AUTHOR: %n"
                                  ":END:"
                                  "\n%?")
                       :custom (:title "%^{Title}"))))))))
```


### org-roam {#org-roam}

Personal org-roam customization, ported from my Doom config.

```emacs-lisp
;; Set the roam directory at top level (NOT inside `with-eval-after-load')
;; so it's in place before the package first loads. The framework's
;; `(use-package org-roam :config (org-roam-db-autosync-enable))' wires
;; the file watcher / DB on first load -- if `org-roam-directory' isn't
;; set yet, the DB syncs against the default `~/org/roam/' (which is
;; empty / nonexistent here), and `SPC n r f' returns no nodes.
(setq org-roam-directory  (file-name-as-directory
                           (expand-file-name "roam" org-directory))
      org-roam-index-file (expand-file-name "index.org" org-roam-directory))

(with-eval-after-load 'org-roam
  ;; Capture templates: main / reference / article subdirectories.
  (setq org-roam-capture-templates
        '(("m" "main" plain "%?"
           :if-new (file+head "main/${slug}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new (file+head "reference/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("a" "article" plain "%?"
           :if-new (file+head "articles/${title}.org"
                              "#+title: ${title}\n#+filetags: :article:\n")
           :immediate-finish t
           :unnarrowed t)))

  ;; Type column = subdirectory name (main / reference / articles / ...).
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))

  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))

  ;; Update the database whenever a roam file is saved.
  (add-hook 'after-save-hook
            (lambda ()
              (when (and (derived-mode-p 'org-mode)
                         (org-roam-file-p))
                (org-roam-db-update-file)))))

;; Make roam files searchable from agenda commands without paying the
;; listing cost on startup.
(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (when (and (featurep 'org-roam)
                         (not org-agenda-text-search-extra-files))
                (setq org-agenda-text-search-extra-files
                      (org-roam--list-files org-roam-directory))))))
```


### gptel {#gptel}

[gptel](https://github.com/karthink/gptel) is the LLM client used here for in-buffer chat and the
`gptel-magit` commit-message integration below. Backend is GitHub
Copilot's chat endpoint (no API key needed, uses its own device
OAuth flow). Model is `claude-sonnet-4.5` via the Copilot proxy.

```emacs-lisp
(straight-use-package 'gptel)
```

```emacs-lisp
(with-eval-after-load 'gptel
  (require 'gptel-gh)
  (setq gptel-backend (gptel-make-gh-copilot "Copilot")
        gptel-model   'claude-sonnet-4.5))
```


### MCP Server {#mcp-server}

[emacs-mcp-server](https://github.com/rhblind/emacs-mcp-server) exposes Emacs to MCP clients (Claude Code, etc.)
over a Unix socket. Not on MELPA, so install straight from GitHub on
the `main` branch. The recipe pulls in the tool sources and wrapper
scripts alongside the elisp.

```emacs-lisp
(straight-use-package
 '(mcp-server :type git :host github
              :repo "rhblind/emacs-mcp-server"
              :branch "main"
              :files ("*.el" "tools/*.el")))
```

Start the Unix-socket server after Emacs goes idle for the first time.

```emacs-lisp
(use-package mcp-server
  :defer t
  :init
  (setq mcp-server-security-allowed-dangerous-functions
        '(find-file with-current-buffer))
  (run-with-idle-timer 1 nil #'mcp-server-start-unix))
```


### gptel-magit (auto commit messages) {#gptel-magit--auto-commit-messages}

[gptel-magit](https://github.com/ragnard/gptel-magit) auto-fills the magit commit buffer with an LLM-generated
message via gptel.

```emacs-lisp
(straight-use-package 'gptel-magit)
```

Trigger generation as soon as the commit buffer is set up, and tighten
the prompt so the model emits raw Conventional Commits text (no
markdown fences), with the summary capped at 80 characters.

```emacs-lisp
  (with-eval-after-load 'magit
    (require 'gptel-magit nil t)
    (add-hook 'git-commit-setup-hook #'gptel-magit-generate-message))

  (with-eval-after-load 'gptel-magit
    (setq gptel-magit-commit-prompt
          (string-join
           (list gptel-magit-prompt-conventional-commits
                 ""
                 "IMPORTANT:"
                 "- Return ONLY the raw commit message text"
                 "- The summary should NEVER exceed 60 characters and never contain line breaks"
                 "- Do NOT wrap the output in markdown code blocks, backticks, or any other formatting"
                 "- NEVER add triple backticks (```) before or after the commit message!")
           "\n")))
```


#### Cancel generation when cycling commit history {#cancel-generation-when-cycling-commit-history}

`M-p` / `M-n` in the magit commit buffer cycle through previous commit
messages. If a gptel request is still in-flight when `M-p` is pressed,
the model's reply lands _after_ the recalled message and silently
overwrites it. The advice below aborts the request and swallows any
late-arriving callback.

```emacs-lisp
(defvar my--gptel-commit-cancelled nil
  "Non-nil when gptel commit generation has been cancelled.")

(defvar my--gptel-magit-saved-callback nil
  "Stored callback for `gptel-magit' cancellation handling.")

(defun my/gptel-abort-before-commit-nav (&rest _)
  "Abort any pending gptel request before navigating commit history."
  (setq my--gptel-commit-cancelled t)
  (when (fboundp 'gptel-abort)
    (ignore-errors (gptel-abort))))

(defun my/gptel-magit-callback-wrapper (msg)
  "Forward MSG to the saved callback, unless generation was cancelled."
  (cond
   (my--gptel-commit-cancelled
    (message "gptel: commit message generation cancelled"))
   ((null msg)
    (message "gptel: commit message generation failed (empty response)"))
   (my--gptel-magit-saved-callback
    (funcall my--gptel-magit-saved-callback msg))))

(defun my/gptel-magit-generate-wrapper (orig-fn cb)
  "Wrap ORIG-FN (`gptel-magit--generate', original CB) with cancel tracking."
  (setq my--gptel-commit-cancelled nil
        my--gptel-magit-saved-callback cb)
  (message "gptel: generating commit message...")
  (funcall orig-fn #'my/gptel-magit-callback-wrapper))

(with-eval-after-load 'git-commit
  (advice-add 'git-commit-prev-message :before #'my/gptel-abort-before-commit-nav)
  (advice-add 'git-commit-next-message :before #'my/gptel-abort-before-commit-nav))

(with-eval-after-load 'gptel-magit
  (advice-add 'gptel-magit--generate :around #'my/gptel-magit-generate-wrapper))
```


### ECA {#eca}

Point [ECA](https://github.com/editor-code-assistant/eca-emacs) at a self-hosted GLM-5.1 instance behind an
OpenAI-compatible endpoint. The API key is read from
`~/.authinfo.gpg` at session start so it never appears in config
files.

```emacs-lisp
(defun my/eca-set-api-env ()
  "Set ECA environment variables from authinfo before the server starts."
  (require 'auth-source)
  (let ((found (car (auth-source-search :host "rolf-020871d4-llm.ai.intility.app"
                                        :max 1))))
    (when found
      (let ((secret (auth-info-password found)))
        (when secret
          (setenv "OPENAI_API_KEY" secret)))))
  (setenv "OPENAI_API_URL" "https://rolf-020871d4-llm.ai.intility.app"))

(with-eval-after-load 'eca
  (add-hook 'eca-before-initialize-hook #'my/eca-set-api-env))
```


## Packages {#packages}

Blocks tagged `:tangle packages.el` end up in a separate file that loads
_before_ `config.el` -- the place to declare packages that need to install
(and possibly load) before the main configuration runs.

With `straight-use-package-by-default` on, a plain `(use-package foo)` in
`config.el` already installs and configures in one step; reach for
`packages.el` for things that must install eagerly (themes, foundational
libraries) or for plain `(straight-use-package 'foo)` declarations.

```emacs-lisp
;;; packages.el -*- lexical-binding: t; no-byte-compile: t; -*-
```


### Fonts {#fonts}

Set font variables BEFORE `scratch!` runs so the `:ui fonts` module
picks them up on init. Each variable accepts a `font-spec`, a font
object, or an XFT string. Use `M-x scratch/reload-font` to apply
changes without restarting.

```emacs-lisp
(setq scratch-font (font-spec :family "Fira Code" :size 13 :weight 'semi-light)
      scratch-variable-pitch-font (font-spec :family "Iosevka Aile" :size 18)
      scratch-serif-font (font-spec :family "IBM Plex Mono" :weight 'light)
      scratch-symbol-font (font-spec :family "Apple Symbols")
      scratch-big-font (font-spec :family "Fira Code" :size 18))
```


### Themes {#themes}

Install [doom-themes](https://github.com/doomemacs/themes) eagerly so the framework's `:ui theme` module
can load them, and override `scratch-theme-{dark,light}` BEFORE
`scratch!` runs (the module reads these on init). Dark =
`doom-nord`, light = `doom-tomorrow-day` -- same pair I've been
running in Doom for years.

```emacs-lisp
(straight-use-package 'doom-themes)

(setq scratch-theme-dark  'doom-nord
      scratch-theme-light 'doom-tomorrow-day)
```


### Modules {#modules}

Modules are loaded eagerly via the `scratch!` macro. Each module's
`packages.el` runs first (across all enabled modules), then each
module's `config.el`. Putting the call in this file ensures modules
are fully wired up before any user code in `config.el` runs.

```emacs-lisp
(scratch! :editor     (evil +everywhere) leader smartparens
                      ws-butler drag-stuff tree-sitter vlf
                      symbol-overlay outline snippets format
          :completion vertico corfu
          :emacs      (vc +forge +gutter) ibuffer (dired +preview)
          :checkers   syntax
          :lang       (org +roam +hugo +pretty) markdown csharp elixir erlang json yaml
          :tools      (lsp +peek) direnv mise just
          :llm        (claude-ide +mcp +ide-diff) (eca +completion)
          :term       vterm
          :os         macos
          :ui         dashboard theme modeline (fonts +ligatures) treemacs workspaces
                      smooth-scroll hl-todo info-colors rainbow)
```


### Example: install a package eagerly {#example-install-a-package-eagerly}

```emacs-lisp
;; (straight-use-package 'magit)
```


### Hugo {#hugo}

Point ox-hugo at the Hugo site root and set the blog capture file.

```emacs-lisp
(setq org-hugo-base-dir "~/workspace/hugo-blog"
      scratch-org-capture-blog-file
      (expand-file-name "blog.org" org-hugo-base-dir))
```


### Example: configure (in config.el) {#example-configure--in-config-dot-el}

```emacs-lisp
;; (use-package magit
;;   :defer t)
```

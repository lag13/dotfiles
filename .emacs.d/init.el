;; My Emacs configuration file. The path to this file is determined by
;; user-init-file which gets dynamically set when emacs starts. I
;; believe that when emacs starts up it does something like "find the
;; first non empty file in the list (~/.emacs ~/.emacs.d/init.el).
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'aggressive)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(bookmark-save-flag 1)
 '(bs-attributes-list '(("File" 12 12 left bs--get-file-name)))
 '(bs-string-current "/")
 '(centaur-tabs-cycle-scope 'tabs)
 '(centaur-tabs-set-bar 'under)
 '(centaur-tabs-show-count t)
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#3cafa5")
 '(cua-normal-cursor-color "#8d9fa1")
 '(cua-overwrite-cursor-color "#c49619")
 '(cua-read-only-cursor-color "#93a61a")
 '(custom-enabled-themes '(vscode-dark-plus))
 '(custom-safe-themes
   '("a226e096b9c4924c93b920ba50e545fb2d37c6d95d6b62b44e62cb6f03b081fa" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "23c806e34594a583ea5bbf5adf9a964afe4f28b4467d28777bcba0d35aa0872e" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" "d6844d1e698d76ef048a53cefe713dbbe3af43a1362de81cdd3aefa3711eae0d" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "f6665ce2f7f56c5ed5d91ed5e7f6acb66ce44d0ef4acfaa3a42c7cfe9e9a9013" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "c5ded9320a346146bbc2ead692f0c63be512747963257f18cc8518c5254b7bf5" "0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(delete-selection-mode t)
 '(desktop-save-mode t)
 '(eval-expression-print-length 1000)
 '(eval-expression-print-level 10)
 '(evil-cross-lines t)
 '(evil-disable-insert-state-bindings t)
 '(evil-exchange-highlight-face 'link)
 '(evil-kill-on-visual-paste nil)
 '(evil-replace-with-register-indent t)
 '(evil-replace-with-register-key "s")
 '(evil-search-module 'evil-search)
 '(evil-want-fine-undo nil)
 '(evil-want-keybinding nil)
 '(explicit-shell-file-name "bash")
 '(exwm-floating-border-color "#191b20")
 '(fci-rule-color "#56697A")
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "vendor"))
 '(groovy-indent-offset 2)
 '(help-window-select t)
 '(highlight-changes-colors '("#e2468f" "#7a7ed2"))
 '(highlight-symbol-colors
   '("#3c6f408d329d" "#0c4a45f64ce3" "#486e33913532" "#1fac3bea568d" "#2ec943ac3324" "#449935a7314d" "#0b04411b5986"))
 '(highlight-symbol-foreground-color "#9eacac")
 '(highlight-tail-colors ((("#283637") . 0) (("#25373e") . 20)))
 '(history-delete-duplicates t)
 '(hl-bg-colors
   '("#936d00" "#a72e01" "#ae1212" "#a81761" "#3548a2" "#0069b0" "#008981" "#687f00"))
 '(hl-fg-colors
   '("#002732" "#002732" "#002732" "#002732" "#002732" "#002732" "#002732" "#002732"))
 '(hl-paren-colors '("#3cafa5" "#c49619" "#3c98e0" "#7a7ed2" "#93a61a"))
 '(icomplete-compute-delay 0.0)
 '(icomplete-mode nil)
 '(initial-buffer-choice "~/.emacs.d/init.el")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(lsp-ui-doc-border "#9eacac")
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((meta)) ((control) . text-scale)))
 '(nrepl-message-colors
   '("#ec423a" "#db5823" "#c49619" "#687f00" "#c3d255" "#0069b0" "#3cafa5" "#e2468f" "#7a7ed2"))
 '(objed-cursor-color "#ff6c6b")
 '(org-startup-indented t)
 '(package-selected-packages
   '(solaire-mode vscode-dark-plus-theme powershell evil-replace-with-register centaur-tabs multiple-cursors evil-mc evil-multiedit evil-visualstar evil-exchange evil-surround evil-indent-plus evil-numbers orderless evil-args evil-nerd-commenter evil-commentary evil-collection rg deadgrep vertico helm ivy projectile fsharp-mode expand-region evil xmlgen web-server go-snippets company yasnippet lsp-mode dumb-jump solarized-theme doom-themes vterm-toggle vterm magit paredit plantuml-mode groovy-mode nginx-mode jinja2-mode systemd terraform-mode cider typescript-mode edit-indirect clojure-mode haskell-mode php-mode dockerfile-mode elm-mode restclient yaml-mode markdown-mode go-guru editorconfig go-mode))
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#282c34"))
 '(pos-tip-background-color "#01323d")
 '(pos-tip-foreground-color "#9eacac")
 '(projectile-indexing-method 'alien)
 '(recentf-mode t)
 '(rg-command-line-flags '("--hidden"))
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(savehist-autosave-interval 15)
 '(savehist-mode t)
 '(send-mail-function 'smtpmail-send-it)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#93a61a" "#01323d" 0.2))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(term-default-bg-color "#002732")
 '(term-default-fg-color "#8d9fa1")
 '(vc-annotate-background "#1D252C")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (list
    (cons 20 "#8BD49C")
    (cons 40 "#abcd93")
    (cons 60 "#cbc68b")
    (cons 80 "#EBBF83")
    (cons 100 "#e5ae6f")
    (cons 120 "#df9e5b")
    (cons 140 "#D98E48")
    (cons 160 "#dc885f")
    (cons 180 "#df8376")
    (cons 200 "#E27E8D")
    (cons 220 "#df7080")
    (cons 240 "#dc6274")
    (cons 260 "#D95468")
    (cons 280 "#b35365")
    (cons 300 "#8d5163")
    (cons 320 "#675160")
    (cons 340 "#56697A")
    (cons 360 "#56697A")))
 '(vc-annotate-very-old-color nil)
 '(vterm-max-scrollback 100000)
 '(vterm-toggle-hide-method 'reset-window-configration)
 '(weechat-color-list
   '(unspecified "#002732" "#01323d" "#ae1212" "#ec423a" "#687f00" "#93a61a" "#936d00" "#c49619" "#0069b0" "#3c98e0" "#a81761" "#e2468f" "#008981" "#3cafa5" "#8d9fa1" "#60767e"))
 '(xterm-color-names
   ["#01323d" "#ec423a" "#93a61a" "#c49619" "#3c98e0" "#e2468f" "#3cafa5" "#faf3e0"])
 '(xterm-color-names-bright
   ["#002732" "#db5823" "#62787f" "#60767e" "#8d9fa1" "#7a7ed2" "#9eacac" "#ffffee"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; NEAT COMMANDS:
;; C-h K/F - like their lowercase variants but opens any info docs.

;; C-h l - views the last 300 input keystrokes in case something weird
;; happens and you want to figure out what it whas.

;; Pressing M-n with the cursor on a link/file while doing a C-x C-f
;; will autofill that link/file.

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; https://www.emacswiki.org/emacs/MidnightMode TODO: Interesting
;; thing I noticed. This midnight mode cleared all the buffers
;; underneath a projectile project but projectile still had knowledge
;; of the project. I wonder if there's a way to configure projectile
;; so it deletes projects when they have no buffers underneath them.
(require 'midnight)
(midnight-delay-set 'midnight-delay "9:00am")

(toggle-frame-fullscreen)

;; The order of these package related function calls I copied from
;; here: https://melpa.org/#/getting-started
(require 'package)
;; package-archives is a list of package archives to search through
;; when running commands like list-packages. The default package
;; archive is more strict to modify so I think more packages end up
;; getting added elsewhere such as http://melpa.org/packages/.
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Will install all the packages declared by this init file. This is
;; only useful for new computers which do not have my selected
;; packages. The package-refresh-contents will slow things down on
;; start up but I don't startup emacs that often so its fine with me.
(package-refresh-contents)
(package-install-selected-packages)

;; More on emacs' built in completion stuff within the minibuffer:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
;; https://github.com/oantolin/orderless is a user defined completion
;; style which is pretty neat! Good for when I know a couple words in
;; what I'm searching for but don't know their order.
(setq completion-styles '(basic partial-completion flex orderless))

(add-hook 'c-mode-common-hook
	  (lambda()
	    (setq comment-start "// ")
	    (setq comment-end "")))

;; Have PATH be the same as exec-path. We do this because sometimes
;; emacs will invoke a shell to run a program and the shell needs PATH
;; set appropriately to work. Figured it would be a good idea to have
;; exec-path be the source of truth with all of that.
(setenv "PATH" (mapconcat 'identity exec-path path-separator))

;; Go specific hooks
;; (setq go-mode-hook
;;       (lambda ()
;; 	(setq godoc-command "godoc")
;; 	(setq godoc-use-completing-read t)
;; 	(setq gofmt-command "goimports")
;; 	(add-hook 'before-save-hook 'gofmt-before-save)))

;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md#loading-lsp-mode-in-emacs
;; https://geeksocket.in/posts/emacs-lsp-go/
;; https://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; TODO: Do haskell stuff one thing that looked cool was configuring
;; "stylish-haskell" to run on save. Maybe this configuring this
;; haskell-process-args-ghci would be useful? I think ghci runs by
;; default for interactive haskell in emacs.

;; https://orgmode.org/worg/org-contrib/babel/languages.html#configure.
;; TODO: I'm still kind of confused how this works. For example I was
;; able to run an elisp code snippet even though I don't currently see
;; that as a potential language in org-babel-load-languages but I did
;; see it earlier. Also I *swear* I did not have to add support for
;; shell before and it magically started working but later when I
;; tried that was not the case and I had to do something like this.

;; TODO: Before emacs 26 it complains about "ob-shell" not being
;; loadable but apparently ob-sh works and on emacs 26 it complains
;; about ob-sh not being loadable. Maybe I should just stick with
;; emacs 26 and not care about backwards compatability, either way
;; though I wanted to make a note that maybe I should modify this to
;; be backwards compaitble.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (plantuml . t)))

;; Purpose is so that if I evaluate some plant-uml in a code block,
;; the resulting image will be displayed
(add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

;; TODO: Why is it done this way instead of with a hook? I commented this out since it seemed to mess up the interactive session if I ran it.
;; (eval-after-load "haskell-mode"
;;   '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

;; Enables creating interactive shell stuff
(add-hook 'haskell-mode-hook #'interactive-haskell-mode)
(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring))
(setq haskell-process-type 'stack-ghci)
      
;; ;; Haskell specific hooks
;; (add-hook 'haskell-mode-hook
;; 	  (lambda ()
;; 	    (interactive-haskell-mode 1)))

;; https://github.com/avh4/elm-format, npm install -g elm-format
(setq elm-format-on-save t)

;; Turn on editorconfig mode
(setq editorconfig-exclude-modes '(org-mode))
(editorconfig-mode 1)

;; By default restclient mode does not define a file type where it
;; will be triggered.
(add-to-list 'auto-mode-alist (cons "\\.http\\'" 'restclient-mode))

;; I'm not interested in eldoc mode
(global-eldoc-mode 0)

;; Hitting return will open the link under the point.
(setq org-return-follows-link t)

;; Although, from a computing standpoint, two spaces separating
;; sentences probably makes more sense, because then programs like
;; emacs can accurately and easily determine sentence boundaries, one
;; space seems to be the norm.
(setq sentence-end-double-space nil)

;; To make it easy to store a link that can be inserted into an org
;; document.
(global-set-key (kbd "C-c l") 'org-store-link)

(defun lag13-left-char-or-previous-buffer ()
  "Repeats `previous-buffer' if the last command was
`previous-buffer' or `next-buffer', otherwise it does
`left-char'."
  (interactive)
  (if (or (eq last-command 'previous-buffer)
	  (eq last-command 'next-buffer))
      (progn
	(previous-buffer)
	(setq this-command 'previous-buffer))
    (left-char)))

(defun lag13-right-char-or-next-buffer ()
  "Repeats `next-buffer' if the last command was `next-buffer' or
`previous-buffer', otherwise it does `right-char'."
  (interactive)
  (if (or (eq last-command 'previous-buffer)
	  (eq last-command 'next-buffer))
      (progn
	(next-buffer)
	(setq this-command 'next-buffer))
    (right-char)))

(defun lag13/get-buffer-sym (buf sym)
  "Gets the value of a symbol in a buffer. For
example: (lag13/get-buffer-sym (current-buffer) 'major-mode) will
get the major mode of the current buffer."
  (with-current-buffer buf (symbol-value sym)))

(defun lag13/is-special-buffer (buf)
  "Typically \"special\" buffers (which include things like the
  terminal emulators or repls or the help pages) start with a
  \"*\" and typically end with a \"*\" as well but sometimes the
  buffer name will look something like *something*<3>. So I just
  look for a starting \"*\" and another \"*\" sometime later."
  (string-match "^\\*.+\\*"
		(buffer-name buf)))

(defun lag13/is-magit-buffer (buf)
  (or (eq 'magit-mode
	  (lag13/get-buffer-sym buf 'major-mode))
      (with-current-buffer buf (derived-mode-p 'magit-mode))))

;; Must return nil if we want to switch to the buffer that gets passed
;; to this function. Also, wanted to mention the concept of a
;; "next/previous buffer" is local to a window (i.e. each window keeps
;; a separate history of the buffers it's visited) and then will fall
;; back to the global buffer list if that window specific list is
;; exhausted. This is different than the behavior of the
;; "other-buffer" function which will visit the previous buffer that
;; was visited in the current frame).
(setq switch-to-prev-buffer-skip
      (lambda (window buf bury-or-kill)
	(and (not bury-or-kill)
	     (or (lag13/is-special-buffer buf)
		 (lag13/is-magit-buffer buf)))))

;; My first stab at emacs programming. Makes buffer switching with C-x
;; <left>/<right> a little quicker when you need to repeat it. Kind of
;; don't need these anymore because of the bindings below but I'll
;; keep it around for now I guess.
(global-set-key (kbd "<left>") 'lag13-left-char-or-previous-buffer)
(global-set-key (kbd "<right>") 'lag13-right-char-or-next-buffer)
;; Faster buffer switching. The shifted '9' character is '(' and the
;; shifted '0' character is ')' which feel like backwards and forwards
;; to me respectively hence these bindings.
(global-set-key (kbd "C-9") #'previous-buffer)
(global-set-key (kbd "C-0") #'next-buffer)


;; If there is a key binding for the command just run, let us know
;; more quickly. It will also shorten how long the message is
;; displayed but we can always check *Messages* if we're interested.
(setq suggest-key-bindings 1)

;; I feel like backup files just clutter things and I've never had a
;; use for them as of yet (crosses fingers).
(setq make-backup-files nil)

;; Similar to backup files, these lock files clutter things up. And in
;; this case (at least at the time of this writing) they give no
;; benefit because I'm the only one editing files on my computer.
(setq create-lockfiles nil)

;; The auto save feature that this variable controls creates SEPARATE
;; files in which data is saved and I don't feel the need for such a
;; feature and don't want those files hanging around anyway.
(setq auto-save-default nil)

;; Today is 2021-12-07 and I feel like as of recently (and on and off
;; in prior months/years) I've been starting to feel the dreaded
;; "emacs pinky" where the tendons on my left arm that attach to my
;; pinky feel... sliiiighly painful (most likely because, while
;; editing, that pinky holds down the ctrl key almost constantly). So
;; I want to try eliminating the need to explicitly save my files as a
;; first step towards alleviating this (since it's almost a habit to
;; save a file immediately after I'm done typing). Eventually I'll
;; probably switch to evil mode (I think I want to get back to vim
;; like keybindings eventually anyway) but I'll start with this.
(setq auto-save-visited-interval 1)
(auto-save-visited-mode 1)

;; I've never had any use for specific settings for particular files
;; or directories so I figured I'd disable those features. (setq
;; enable-local-variables t)
(setq enable-dir-local-variables nil)

;; Highlight matches immediately as you type.
(setq lazy-highlight-initial-delay 0)

;; Pause tetris then switch to a different buffer.
(defun lag13-tetris-suspend ()
  "Pauses the game of tetris (unless it is already paused) and
switches to buffer `other-buffer'. Useful for when you are struck
by inspiration while playing and quickly need to act on that
inspiration. Also useful if someone of importance happens to be
walking by and you don't want them to see you plaing tetris, that
works too."
  (interactive)
  (or tetris-paused (tetris-pause-game))
  (message nil)
  (switch-to-buffer (other-buffer)))

(add-hook 'tetris-mode-hook
	  (lambda ()
	    ;; This keymap is active when tetris-mode-hook is running
	    ;; and when a game has ended. The only reason I added it
	    ;; is so if I switch to the buffer of a finished game I
	    ;; can still get out of it the same way. Completeness
	    ;; right?
	    (define-key tetris-null-map "s" 'lag13-tetris-suspend)
	    (define-key tetris-mode-map "s" 'lag13-tetris-suspend)))


;; Jump to the "alternate" file. Useful when editing C code and you
;; want to jump back and forth between a file and it's header file.
(global-set-key (kbd "C-x C-a") 'ff-get-other-file)

(defun view-help-buffer ()
  "Quickly view the help buffer."
  (interactive)
  (switch-to-buffer-other-window (help-buffer)))
(global-set-key (kbd "C-h h") 'view-help-buffer)

(defun insert-timestamp ()
  "Inserts the current time in UTC. This is nifty for me because
I oftentimes find myself taking notes as something happens and
writing down that time. I use UTC because I want my time to have
a consistent format. Timezone format is ISO 8601:
https://en.wikipedia.org/wiki/ISO_8601#Time_zone_designators. "
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t)))

;; TODO: Sometimes I'm recording something I did in the past and I
;; want to enter accurate times for the events that transpired. It
;; would be neat to be able to do that quickly instead of having to do
;; some conversions in my head (since I try to record everything in
;; UTC time)
(global-set-key (kbd "C-c t") 'insert-timestamp)

(defun insert-date ()
  "Inserts the current date. As opposed to `insert-timestamp' I
  insert the local date since I'm usually just recording when I
  work on things"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; TODO: Give a C-u modification to this function which will allow you
;; to insert the date X days ago. My use case here is that I wanted to
;; record a list of support tickets we had received for the past 30
;; days and when I was writing notes on that I wanted to just be able
;; to say, "these are a list of support tickets from "X-30" to X" but
;; I couldn't easily say that and ended up googling it. Speaking of
;; googling... google has so much functionality to answer little
;; questions like this without even going to a link, they just give
;; you the answer. Could I add a "ask google" function which pastes
;; that first response into the buffer if they have one.
(global-set-key (kbd "C-c d") 'insert-date)

;; TODO: Another date related thing is I think it could be nice to be
;; able to count the number of weekdays between 2 dates. Again, this
;; is motivated because I was seeing the number of support requests
;; that came in over the past 30 days and I was curious how many of
;; those 30 days were actual business days. Maybe that calculation is
;; actually really simple though and I could just do it in my head.

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Grep-Searching.html
(setq grep-save-buffers nil)

;; How to do global search and replace:

;; 1. Go into Dired mode and hit Q on the file or directory where this
;; search and replace should happen.

;; 2. After doing that run the save-some-buffers command which will
;; save modified file buffers.


;; How to work with org mode tables: C-c | to create a table, if a
;; table does not have a header then you can open up a line below the
;; row you want for a header and type |-<TAB>.

;; TODO: When running C-c | over a region org mode assumes that the
;; region is comma delimited and create the table based on that. You
;; can do C-u C-u C-u to enter in a regex to delimit on instead. I
;; think that should be the default though since I do NOT do this
;; often enough to warrant a default behavior which I will probably
;; never remember.

;; TODO: Make an escape and unescape function similar to tpope's [y
;; and ]y commands in https://github.com/tpope/vim-unimpaired

;; TODO: Make artist-mode's C-n the same as picture-mode's C-n in the
;; sense that it appends to the buffer. Why would it not do this by
;; default?

;; TODO: I want to be able to paste in picture mode and not have the
;; text shift.

;; TODO: In artist-mode I want to be able to move rectangles around
;; and also have text wrap withing that rectangle. Is something like
;; that possible?

;; TODO: I should make sure that artist mode only uses spaces (no
;; tabs) so that I can copy the document elsewhere and the formatting
;; will not get messed up:
;; https://stackoverflow.com/questions/43976371/artist-mode-drawing-characters-disorder

;; TODO: Can I configure C-c C-c in org mode to not try to put the
;; output of a program into a table? I feel like the raw output would
;; be more useful.

;; TODO: Learn from this guy: https://github.com/cgore He seems super
;; sharp and seems like a super lisp geek.

;; TODO: Prevent emacs quitting when Cmd-Q is hit, I just did that
;; accidentally and its a little annoying.

;; TODO: Configure terraform so it runs terraform fmt on save.

;; I tend to be okay not using line numbers when coding (I did for a
;; while in fact) but they are useful if any sort of pair programming
;; is being done because then the other person can be like "I think we
;; should edit line XYZ" https://www.emacswiki.org/emacs/LineNumbers.
(global-display-line-numbers-mode)

;; TODO: Sometimes I want to "find" a specific file like how the
;; "find" command works. How could I do that with emacs? Should I just
;; bite the bullet and install some sort of fuzzy file finder? It
;; might be useful especially when working in large projects. I wonder
;; if projectile could come in handy here too for this. Or maybe dired
;; can help out somehow?
;; https://www.masteringemacs.org/article/working-multiple-files-dired

;; https://www.emacswiki.org/emacs/UnfillParagraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))


;; Yesterday (2019-03-14 i.e. pi day) I learned about tau
;; (https://tauday.com) and now I'm a tau convert. I can't say I've
;; EVER needed to use pi before within emacs but I just wanted to
;; write this down.
(setq tau (* 2 pi))
;; And of course: https://xkcd.com/1292/
(setq pau (* 1.5 pi))

;; TODO: It would be cool if whenever I type in an orgmode buffer it
;; timestamped my edits or something. Then I could do things like:
;; "hey emacs, show me my edits within the past day" or something like
;; that and then I'd be able to see what I was working on. That would
;; also mean that I get sort of a builtin scrum thingy because during
;; scrum I can just be like "hey emacs, show my edits for the past
;; day" and I'll just read off what is there. I'm sure this would be a
;; difficult thing to do but I think it could be cool.

;; TODO: Learn more about babel to create results. I think I'd like
;; the default behavior to be to not try and put the results in a
;; table. Also I tried to reevaluate the same expression and it made a
;; new RESULTS section rather than replacing the old one. Can I change
;; that behavior? That would be nice I think. I think it would also be
;; cool if babel could nicely format a JSON response or something
;; similar. I suppose I could use that http mode that I played around
;; with a bit! That might be a lot easier than doing curl stuff
;; actually.

;; TODO: In orgmode when editing a go code block saving does not cause
;; goimports to be run so the imports are sadly not automatically
;; managed. I'm not sure how best to resolve this or if it's worth
;; doing. I guess for the time being I can manually edit go files and
;; copy them in but I think it would be neat to have a solution to
;; this. Documenting things in org mode is just too much fun. Or
;; perhaps I'll just have to pick another language which is easier to
;; work with? Not sure.

;; TODO: projectile, helm, magit, tramp, eshell,
;; https://github.com/wasamasa/dotemacs,
;; https://github.com/aterweele/emacs.d,
;; https://github.com/atw-gr/emacs.d

;; TODO: I had to delete every buffer who's file lived in the packerex
;; directory because I moved them to another directory and didn't want
;; name conflicts when I started using the files in the new directory
;; location. I would like to know of a way to quickly delete multiple
;; buffers matching some sort of regex? Alternatively if I could run a
;; command to delete a buffer which was associated with a file that no
;; longer exists then that would be handy.

;; Rename files:
;; C-x d (dired mode)
;; C-x C-q
;; Edit files
;; C-c C-c (or save buffer)

(add-hook 'js-mode-hook (lambda() (setq indent-tabs-mode nil)))

(setq groovy-mode-hook (lambda() (setq indent-tabs-mode nil)))

(setq dockerfile-mode-hook (lambda() (setq indent-tabs-mode nil)))

;; TODO: I feel like it would be handy if ALT-Q would NOT wrap space
;; delimited words if those words are enclosed in quotations or
;; something like that. Then I could more easily put code snippets and
;; not have to worry about it getting wrapped around when pressing
;; alt-q. Also, it would be nice to be able to do numbered lists and
;; stuff like that all within comments and have alt-q wrap things
;; appropriately.

(setq plantuml-jar-path "~/Downloads/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)
;; So plantuml diagrams in org mode get displayed instantly
(setq org-plantuml-jar-path (expand-file-name "~/Downloads/plantuml.jar"))
(global-auto-revert-mode t)

;; https://github.com/flyingmachine/emacs-for-clojure/blob/master/customizations/setup-clojure.el
(add-hook 'clojure-mode-hook #'enable-paredit-mode)

;; (defun lag13/clojure-extra-keybindings ()
  ;; (define-key icomplete-minibuffer-map (kbd "DEL") 'icomplete-fido-backward-updir))
;; (add-hook 'clojure-mode-hook 'lag13/icomplete-modifications)

(add-hook 'cider-repl-mode-hook 'paredit-mode)
;; https://www.emacswiki.org/emacs/ParEdit
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)

;; TODO: Be able to switch a variable name between different casings
;; seems like it would be fun: camel, kebab, snake

(global-so-long-mode 1)
(put 'downcase-region 'disabled nil)

;; TODO: Could we use magit to review PRs? I think I'm a lot faster
;; looking at code when inside my editor instead of looking in the
;; browser.

;; TODO: Get some documentation autocompletion going for terraform
;; (and whatever else I can think of too for that matter). I feel like
;; I've moved beyond the "I should just try to keep it all in my head"
;; phase of programming. Computers are here to make things easier and
;; I should lean on them more. I also feel like constantly looking up
;; things in the browser is inefficient. A couple candidates
;; https://www.reddit.com/r/emacs/comments/cmb8zl/terraformdoc_look_up_the_terraform_documentation/
;; https://github.com/rafalcieslak/emacs-company-terraform

;; TODO: I really like the emacs help manual thingy (C-h i). I want to
;; learn to use it better AND figure out if I can add manuals for
;; anything/everything??

(defun lag13/replace-last-sexp ()
  "Sometimes if I'm writing docs or whatever I just want to
insert the value of an emacs expression and then get rid of the
expression."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(global-set-key (kbd "C-x M-e") 'lag13/replace-last-sexp)

;; I learned from section 8.5 of the emacs manual on minibuffer
;; history that if you type C-x C-f and then M-n, it will, by default,
;; try to find the file at the point.

;; ;; The vterm-mode-hook gets modified by the vterm-toggle package to
;; ;; call a function which modifies a list of vterm buffers internal to
;; ;; vterm but it only gets modified once the package gets used (i.e. if
;; ;; I tried running "M-x vterm" without ever using vterm-toggle then
;; ;; vterm-toggle would not see the vterm buffer for the purpose of the
;; ;; vterm-toggle-forward/backwards commands. Pretty minor edge case but
;; ;; just thought I'd get it out of the way. TODO: Would it make sense
;; ;; to add the hook adding bit to the autoloads portion of the code so
;; ;; this require is not necessary?
;; (require 'vterm-toggle)

;; (add-hook
;;  'vterm-mode-hook
;;  (lambda ()
;;    (define-key vterm-mode-map [(control return)] #'vterm-toggle-insert-cd)
;;    (define-key vterm-mode-map (kbd "s-n") 'vterm-toggle-forward)
;;    (define-key vterm-mode-map (kbd "s-p") 'vterm-toggle-backward)
;;    (display-line-numbers-mode 0)))

;; Right now (2021-07-13) I want to experiment with having a buffer
;; list that: only includes file buffers, is sorted by buffers most
;; recently visited, point is at the top of the buffer, and when I
;; select a file to go to, the buffer list buffer will NOT be returned
;; by the "other-buffer" function (i.e. I can do C-x b <enter> after
;; selecting a buffer and end up at the buffer I was looking at BEFORE
;; I opened the buffer list). bs-show seems to provide that
;; functionality (it seems to delete itself after selecting a buffer
;; which is why "other-buffer" doesn't pick it up) so I'm using it. I
;; modified a couple configs namely I ONLY show the full file path
;; (bs-attributes-list) and I had to adjust the variable
;; bs-string-current so the cursor is still properly focused at the
;; top of the buffer. My current use case for this buffer list stuff
;; is when I'm in a situation where I remember I want to go to a file
;; that exists in a certain directory but I can't for the life of me
;; remember the file name OR I want to open up any old file in a
;; particular directory/project so I can do something there like run
;; magit-status. TODO: Maybe more project awareness could be a way to
;; solve both of these problems so perhaps I should look into
;; something like projectile which I think might solve other things I
;; was thinking about too. TODO: It could be interesting to also come
;; up with another minibuffer thing (maybe the key binding could be
;; C-x f) which completes on full filenames just to see what goes into
;; doing something like that.
(global-set-key (kbd "C-x C-b") 'bs-show)

;; (global-set-key (kbd "C-x C-p") 'vterm-toggle)
;; (global-set-key (kbd "C-x p") 'vterm-toggle-cd)

;; TODO: I feel like when clojure is activated C-c C-z (which switches
;; from the buffer to the repl) is not always accurate. Like I'll be
;; in a repl in one project and it C-c C-z will take me to a file in a
;; different project kind of thing. Keep an eye on this.

;; TODO: I wish I understood the emacs mark better so I could better
;; jump around files. This seems to have some interesting tidbits too:
;; https://emacs.stackexchange.com/questions/3421/how-to-switch-back-and-forth-between-two-locations-in-a-buffer
;; Because I feel like sometimes I want to mark a specific point of
;; interest and then jump back to it no matter where I am. Or maybe
;; jump back and forth between two points? I also kind of feel like if
;; my ability to copy more information at a time or have better
;; knowledge of using emacs completion mechanisms or maybe even
;; multiple cursors then this could also solve this problem a bit.
;; Because a lot of time I think I jump between places to copy
;; information (being able to copy more or better completion could
;; solve this) or do some editing command in multiple places (multiple
;; cursors could maybe solve this). A PART OF THE EMACS DOCS MENTION
;; THAT REGISTERS SHOULD MAYBE BE USED TO REPEATEDLY JUMP BACK TO A
;; PARTICULAR SPOT.

;; TODO: Sometimes I feel like I copy something to multiple spots in
;; emacs but have to delete something too but it's frustrating because
;; whenever I delete the thing I overwrite what I copied. Is there a
;; way to improve this somehow? I know M-y can take a negative
;; argument to go backwards but is there a faster way? Or just better
;; management of the kill ring in general? Or if we had registers to
;; copy to like in vim that could solve this. Or if I knew of better
;; ways to delete lots of text in emacs without actually copying it
;; then that would work too.

;; TODO: I think I'm really starting to miss some of the vim text
;; objects and movements and things. Like commenting the current
;; paragraph or moving to an indent level the same as your own. I want
;; to think about learning if some of those things exist or finding a
;; way to get them back somehow whether through evil mode or other
;; ways.

;; TODO: At some point I was watching a talk about CIDER and the
;; presenter mentioned I believe this package avy:
;; https://emacsredux.com/blog/2015/07/19/ace-jump-mode-is-dead-long-live-avy/
;; I should check it out for the purpose of navigating more
;; efficiently in emacs. I think ace jump was the original
;; implementation of this sort of idea but now avy is the cool new
;; thing.

;; TODO: I want to learn about how to send email through emacs and
;; what that all involves to set that up.

;; TODO: I've gotten so into the habit of hitting M-q to format my
;; text as a neat little paragraph but I think it would be so great if
;; I didn't have to do that. On a related note, sometimes I feel like
;; I like composing long messages in emacs (not sure why) but it
;; doesn't work the best because the slack message itself will just be
;; one long line but when I'm typing it in emacs I end up hitting M-q
;; and thus having this paragraph which I need to un-paragraph (hence
;; the unfill-paragraph function in this file). Is there a better way
;; to compose messages in emacs which I can just copy over to other
;; places? The emacs manual mentions a minor mode called "auto fill"
;; which "splits lines automatically when they get too long". Maybe
;; that could help? It also mentions "visual line" mode which also
;; seems maybe related.

;; Some info about ido, incomplete, fido:
;; https://lists.gnu.org/archive/html/emacs-devel/2019-11/msg00197.html
;; Had a lot of neat things:
;; https://karthinks.com/software/more-batteries-included-with-emacs/

;; TODO: I accidentally typed C-\ and it was prompting me to input
;; text in another language! The first one offerred was
;; korean-hangul3. Not that I know any languages but it would be fun
;; to play around with that I think.

;; TODO: Make an idempotent add-hook function to use in my init.el? I
;; like the idea that at least my emacs config could be idempotent in
;; case I re-evaluate an add-hook form by accident.

;; TODO: I really want something similar to paredit mode but other
;; languages. I just really like the idea of more structured editing.
;; Why would I want the ability to input syntactically incorrect code?
;; I have a machine to help me! Let's actually use it!

;; TODO: This link mentions a feature called "indirect-buffers" and
;; they've inspired a picture in my head of using them to better
;; explore and understand code:
;; https://emacs.stackexchange.com/questions/3421/how-to-switch-back-and-forth-between-two-locations-in-a-buffer
;; They might not be useful for what I'm thinking but here it is.
;; Basically what I want is to be able to look at code and mark
;; specific points of interest (function definitions, variable
;; definitions) along the execution path and then go forward and
;; backward through that execution path so I can better see what the
;; code is doing. I'm picturing using narrowing to only display the
;; exact function definition and nothing more. I'm picturing maybe
;; automatically splitting off windows for each marked point of
;; interest so you can see everything at the same time. I wonder if
;; something like this exists.

;; TODO: Does emacs have a directory tree browser? I haven't felt the
;; need to have one but maybe I'm missing out?

;; TODO: Go through some items on
;; https://github.com/emacs-tw/awesome-emacs and see if anything looks
;; good.

;; TODO: Some motions I want: Up one structure, down into a structure,
;; to the next structure on the same level, to the end of the current
;; structure, to the very top of the current top level structure
;; (optionally be able to jump 1 level away from the top level), to
;; the very end of the current top level structure.

;; TODO: I want to be able to step back through git history to see
;; different versions of a file and, ideally, also have the commits
;; messages readily visible. Is there a quick/easy way to do that?
;; Motivation is to see if there's a reason something in a file is the
;; way it is or if someone just happened to code it like that.

;; TODO: Is there a way to make a bulleted list in a github commit
;; which I can wrap correctly with M-q? I feel like oftentimes I want
;; to make bulleted lists but they're kind of annoying. Can I enable
;; the markdown major mode in the commit buffer? Because it is
;; markdown anyway and I think that would solve this. Or is it just
;; okay to just have long lines? I know the pope says no
;; (https://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html)
;; but I wonder if that will ever change.

(defun lag13/icomplete-modifications ()
  "Changing some things about icomplete so I like it a bit
better. Kind of makes it a bit more like fido honestly."
  (define-key icomplete-minibuffer-map (kbd "RET") 'icomplete-fido-ret)
  (define-key icomplete-minibuffer-map (kbd "C-j") 'exit-minibuffer)
  (define-key icomplete-minibuffer-map (kbd "DEL") 'icomplete-fido-backward-updir))
(add-hook 'icomplete-minibuffer-setup-hook 'lag13/icomplete-modifications)

;; Setting this PLUS setting RET to 'icomplete-fido-ret seems to give
;; the behavior I want where if I hit RET to choose a directory it
;; will complete that directory in the minibuffer instead of editing
;; that directory (i.e. going into dired mode). I think it's needed in
;; the situation where we start another absolute path from within the
;; minibuffer because the icomplete-fido-ret function will grab ALL
;; the minibuffer contents to make some decision which probably gets
;; messed up with two absolute paths in the minibuffer.
(setq icomplete-tidy-shadowed-file-names t)

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;; So I can paste over a highlighted area of text without overwriting
;; what I have copied.
(delete-selection-mode 1)

;; TODO: I think it could be nice to have the inactive buffers display
;; in a slightly darker background active one. On a related note,
;; maybe something like https://github.com/hlissner/emacs-solaire-mode
;; could be nice?

;; TODO: I would love it if M-. would jump to the definition of an
;; elisp function even if said function was written in C. How can we
;; make that happen? Do I have to compile emacs from source?

;; TODO: I think it would be cool if the uniquify feature for making
;; buffer names unique could be aware of projects and then add the
;; project name to the buffer name within the <>. Or maybe I just
;; won't have any need for this if I install projectile :shrug:. At
;; the end of the day I just want to improve my buffer switching.

;; TODO: Seems like the magit status buffer is being listed by the
;; bs-show buffer list. I'll have to dig into that logic to figure out
;; why that is because I'd prefer it if there are ONLY real files
;; getting shown.

;; TODO: Have C-c C-n/C-p in vterm also put me in copy mode. I don't
;; think I'd have another reason to look back at a previous command
;; except to copy and even if that wasn't the case I could exit copy
;; mode again.

;; TODO: I'm not sure if I like how the vterm toggle project works. I
;; think I'd prefer it if it brought up the terminal in the current
;; window or something. As it stands now I feel like I don't know what
;; it's going to do (spawn a new window or occupy the current one).
;; Either figure that out or use something else or roll my own thing.
;; On a similar note, I feel like the "moving the directory to the
;; prevoius buffer doesn't always work like I expect it to. Like I
;; think I want it to go to the buffer where my point was previously
;; but I don't think it does that. Look into that functionality too.

;; TODO: I think I would really like it if prev-buffer and next-buffer
;; ONLY respected the files previously visited with respect to the
;; frame instead of consulting the window specific list first. I just
;; don't think I want to remember which window recently viewed which
;; buffer. All I usually remember is that in some window I viewed this
;; buffer and I want to go back to it. Maybe other people are more
;; regimented with their window usage but for me I feel like they're
;; pretty ephemeral and I'm just using them for a viewing
;; configuration sort of thing and don't want them to impact any
;; functionality. I THINK this could be accomplished by calling the
;; functions set-window-prev-buffers and setting the window's buffers
;; to nil every time prev-buffer gets called:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-History.html
;; ALSO might be cool if I could have a next-buffer that only works
;; within a project sort of feature.

;; TODO: I wonder if saving which files Emacs had open (so if I stop
;; emacs and start it up again it will load those same files) would be
;; a useful feature for me to employ.

;; TODO: I learned from section 8.5 of the emacs manual about
;; minibuffer history that when doing C-x b it keeps track of your
;; arguments which seems like another neat way to switch back to
;; previous buffers you've been to instead of doing the C-x
;; <left/right>. Maybe I'd even prefer it since I think that will be
;; independent of any windows. Anyway. Just wanted to note this here
;; because I think there could be some extra configuring I want to do
;; like erasing duplicate entries in that history or something. I
;; notice that when I do a bookmark-set command and go back through
;; the history it's not JUST past bookmark names that show up, it's
;; also other stuff and I'm curious what it would take to make ONLY
;; bookmarks show up in the history. Some autocomplete here could be
;; nice too. Wonder what that would take to do.

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; TODO: I think it would be fun to make a "random page from the emacs
;; manual generator" program which will pull a random page from the
;; emacs manual to read. It will not repeat pages. That way you get
;; reminders on what all emacs has to offer!

;; TODO: C-h C-t displays the emacs to-do list which could be a fun
;; way to contribute to emacs if I want to.

;; So links in emacs are clickable and open a browser tab.
(goto-address-mode 1)

;; TODO: When looking up documentation in cider mode I think it would
;; be so great if it kept track of which documentation pages I've
;; looked up and I can go back to previous ones.

;; TODO: Lookup how to do proper and efficient golang development in
;; emacs. Feels like it could be nice to do things like compile from
;; within emacs (so I could jump to errors) and run tests from within
;; emacs too. Also it's just been a while since I've done it so I
;; wonder if any changes have occurred. I do feel like I remmeber
;; being able to jump to errors reported from goimports but that no
;; longer seems to be the case.

;; TODO: Look into org roam:
;; https://www.youtube.com/watch?v=AyhPmypHDEw I've heard it can be
;; used for better notetaking. That video description describes it as
;; using it to build a personal knowledgebase which I feel like is
;; something I've wanted and kind of hack by just putting files in
;; various directories which maybe is just fine but also I'm curious
;; what note management things help with.

;; TOOD: For vterm I do rather like the RET character which copies the
;; last command that was run. I think I would also love one more
;; command which is to copy the command PLUS the output.

;; TODO: If a vterm window is narrow and I resize it to be wider, the
;; text from the previous commands are still narrow. Is this a
;; terminal problem? I feel like I've seen this issue before. I wonder
;; if it can be fixed though.

;; TODO: I would like to learn more about developing web applications
;; really quickly in emacs. I remember this looked so cool to me when
;; I saw it a while back: http://emacsrocks.com/e11.html. It feels
;; like more and more lately what I really want is a faster feedback
;; loop when editing. I haven't even used clojure that much but it's
;; making me want it more.

;; TODO: Do I want that expand-region package? Seems potentially
;; useful. Although if I go the vim route I don't think I'd have any
;; use for it.

;; TODO: It would be nice to be able to go to all places in code where
;; some function gets called. Is that kind of functionality built in?
;; Maybe the dumb-jump package has info about that?

;; TODO: If I do rgrep with the cursor on a url it tries to like ping
;; that url? What's up with that?

;; TODO: Should I install ripgrep or ag to speed up rgrep? I think it
;; would be nice in general to mess around with that tool and see all
;; what it can do.

;; TODO: In org mode how do I search in JUST the headers? Can I?

;; TODO: Make it consistent how links get opened across different
;; modes. Also, would be cool if the link opening would also search
;; forward for the next link by default I think.

;; TODO: If I do utilize "dumb jump" in terraform on a local variable,
;; it appears to still look for an actual variable (like in a
;; variables.tf file). I think it could be improved to look for a
;; locals block in the current directory.

;; TODO: How do I get better about spellchecking my stuff? Like
;; spellchecking my comments in particular.

;; Making the text a little bigger. This might mean that I'm getting
;; old (cur date is 2021-08-01).
(set-face-attribute 'default nil :height 140)

;; TODO: It would be neat to have a "search text only" option when
;; looking at an html document in emacs. Just sounds like a cool idea.
;; Would also be nice then to extend that with a "regex search text
;; only".

(defun lag13/delete-all-whitespace ()
  "Created and used when I wrote a function in clojure to pull
all of the text out of an html file but there were too many
newlines and I wanted to collapse them."
  (interactive)
  (goto-char (point-min))
  (replace-regexp "^\\s-+" "")
  (delete-trailing-whitespace 0))

;; TODO: Wonder if this snippets package would be useful:
;; http://emacsrocks.com/e06.html

;; TODO: I'd be really interested in checking out this key chord thing
;; in emacs where you can press keys simultaneously and they'll run
;; some command: http://emacsrocks.com/e07.html. I would be curious if
;; that idea could be used alongside evil mode. Like, part of kind of
;; enjoys just being able to type and not have to worry about
;; switching modes but also it would be cool to have less key chords.
;; I wonder if I can use both somehow? It seems fun to try at any
;; rate: https://www.emacswiki.org/emacs/KeyChord

;; TODO: I feel like I should try out multiple cursors or something
;; like http://emacsrocks.com/e08.html to more efficiently change
;; multiple occurrences of something. Or maybe string replace is just
;; fine? http://emacsrocks.com/e13.html. Actually multiple cursors
;; seem superior here since any cool editing stuff will then happen in
;; multiple places (like if you're making changes in paredit:
;; http://emacsrocks.com/e14.html )

;; TODO: I think it would be cool to, when you're in a project or
;; file, type the name of a function to jump to and you will jump
;; there. I guess ido mode can do this somehow:
;; http://emacsrocks.com/e10.html. It also seems that giving a prefix
;; to M-. lets you do this but I don't see autocomplete working for
;; clojure or golang files.

;; TODO: I think I want to be able to not pop the mark but just travel
;; along the mark stack and not set the mark or something. Seems neat.
;; And maybe something for also going to the top of the mark but not
;; setting anything?

;; TODO: I'd like to get better at working with html. I honestly don't
;; have a use case at the moment but it seems like it would be fun!
;; http://emacsrocks.com/e12.html

;; TODO: Query replace can do a lot more than just replacing things
;; one at a time I'm realizing look into that.

;; TODO: The more I think about doing more structured editing, the
;; more I feel like all text sort of operations have so much room for
;; improvement! Like instead of a generic string replace I want to be
;; able to rename a function and have emacs be aware enough to be able
;; to replace that function usage everywhere!

;; TODO: that vterm copy command function doesn't appear to be able to
;; copy a multi-line command.

;; TODO: Would it be useful/helpful when finding files to have a
;; "super complete" option which just drills down as far as possible
;; and picks the first file there? Pretty minor but I was picturing
;; that when I open clojure projects that initial core.clj file is a
;; couple folders deep and I just hammer on TAB to get there.

;; TODO: Sometimes in clojure I'll find myself basically wanting to
;; copy the line and put it below but the "line" I want to copy is a
;; form and it's the last form in the tree so I need to copy the form,
;; move my cursor to the end, hit RET, and paste. Is there a cooler
;; way to do this? Again I feel like I just want better tree
;; manipulation editor commands. Or maybe they're already there and I
;; don't know about them?

;; TODO: I remember that Alex Ter Wheel who used to work at GR would
;; interact with k8s via emacs. I wonder what package he used. Seems
;; like it could be fun.

;; TODO: In paredit mode sometimes I want to kill to the end of the
;; current node/sexp and that works for single lines but when the rest
;; of the sexp spreads over more lines I have to hit C-k a couple more
;; times. Is there a way around this?

;; TODO: Is there a way in cider to eval the current region? Asking
;; because I came up with a different implementation of something but
;; kept the old one commented out and then I wanted to comment out the
;; old one and re-eval it.

;; TODO: Increase the size of the *Messages* buffer. There was one
;; time where cider threw some stacktrace stuff and it totally filled
;; the buffer which seems to only accomodate 1000 lines.

;; TODO: I feel like this behavior of the DEL key should be default in
;; paredit. Should I suggest this to the maintainer?
(defun lag13/paredit-delete-region-or-backward ()
  (interactive)
  (if (region-active-p)
      (paredit-delete-region (point) (mark))
    (paredit-backward-delete)))

;; TODO: I'd like to better understand the nuances of when I should
;; use eval-after-load vs. using add-hook. They seem similar in the
;; problems they solve. I used eval-after-load here since that is what
;; the source code advised.
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "DEL")
       'lag13/paredit-delete-region-or-backward)))

;; TODO: I feel like I'd like to have the option of killing backwards
;; in paredit. Keep an eye on that thought to see if it's legitimate.


;; TODO: I think this page of gif's
;; http://danmidwood.com/content/2014/11/21/animated-paredit.html
;; noting what Paredit can do is great. I also think that. It would be
;; cool to see tree representation of what Paredit is doing. We could
;; show little nodes getting deleted or highlighted when running commands

;; Paredit motion commands:
;; BACKWARDS:
;; C-M-a - beggining of defun
;; C-M-b - move over S expression
;; C-M-u - Move up out of the enclosing list 
;; C-M-p - Move backwards down into the next list
;; FORWARDS:
;; C-M-e - end of defun
;; C-M-f - move over S expression
;; C-M-d - move down into next expresson
;; C-M-n : basically ')'

(defun lag13/reverse-yank-pop ()
  (interactive)
  (yank-pop -1))

;; Because it's really inconvenient to type a negative prefix argument
;; if you go past the text you want to yank.
(global-set-key (kbd "M-Y") 'lag13/reverse-yank-pop)

;; TODO: I wonder if it would be nice to be able to search through the
;; kill ring in an icomplete sor of way and the text you find gets
;; yanked.

;; TODO: Maybe, if I do a C-x b, configure left and right arrows to do
;; their buffer switching thing. Motivation is I saw myself once
;; switch to the previous buffer thinking it was the one I wanted to
;; go to only to realize that it wasn't and I wished I could just hit
;; <left> since I knew it would have been there.

;; TODO: I would love if the cider repl had a reverse search feature
;; just like bash repls. Is that a shortcoming of nrepl? Or maybe the
;; feature exists but I don't know about it? Because I love that
;; feature. So much. <cries>

;; TODO: Is there a "search through every key of the map looking for a
;; reged" operation in clojure? Because I would love that as a way to
;; quickly explore data where I know that the value I'm looking for
;; exists somewhere but I don't really know where yet since the data
;; is kind of new to me.

;; TODO: Is there a way in clojure to clear all symbols from the
;; current namespace in the repl? Because sometimes I feel like I'll
;; define things, they make their way into other functions, I rename
;; or delete them, then forget to remove them and the code compiles
;; just fine (as expected) but I want to get a fresh start so I know
;; everythings kosher. I think "getting slimed" was the name for this
;; occurrence.

;; TODO: Write something to rearrange the current s expression into a
;; threading macro and vice versa. I think that would be cool. We
;; could bind it to C-> since it seems unused.

;; TODO: There's a bunch of movement options I think would be nice to
;; have. Moving by syntactic units of course (as you can do in
;; paredit). The vim f/t commands. Perhaps that avy thing where you
;; can jump precisely where you want to go. Regular ol' search of
;; course. Perhaps going to the next/previous number (or at the very
;; least I'd love a text object which would let me change the
;; next/prev number, I had that in vim, it would be cool to get it
;; into emacs too).

;; TODO: I think this coincides with my desire to explore code. Maybe
;; I just need to have a feature where I can manipulate a map of an
;; arbitrary string name I define to a list of buffers and be able to
;; cycle through said list. Then if I'm working on some code I can say
;; that I want to start a new group of buffers and then add buffers to
;; this arbitrary group and then cycle quickly between them.

;; TODO: Is there a library for quickly adding print statements for
;; things? Maybe I should write something. Use case is quickly echoing
;; out the values of multiple variables when debugging.

;; Giving this a shot. Let's see if I notice anything.
(setq scroll-preserve-screen-position t)

;; describe-bindings displays all defined keys. Good for when you want
;; to make a new keybinding but you're not sure what to use.

;; If over 100 then moving the cursor off the screen will just move
;; the screen down one line at a time. I mostly want this because it's
;; like this way in most other editors and when pair programming it
;; looks less jarring for folks watching.
(setq scroll-conservatively 101)

;; TODO: Narrowing seems really cool and I feel like that is another
;; fantastic candidate for a text object motion feature in vim. I
;; don't use it much but I wonder if I could incorporate it into my
;; workflow. Or maybe it would mostly be useful for my mythical
;; "understand code better" tool I want to write because I could have
;; each function along the tree narrowed.

;; TODO: Reading a bit about colors in emacs and I thought it could be
;; neat to write some sort of holidy themed colorscheme. The specific
;; thing I had in my head is highlighting a pumpkin on the screen. It
;; would always be in the same position on screen :). There are lots
;; of built in emacs faces too, I bet you could so some quirky stuff
;; like play some animation when the isearch face is being applied to
;; text. There's also apparently something called the "display table"
;; which according to the manual you can use to customize the way any
;; character is displayed. I bet I could do some wacky stuff there
;; too. OH! We could change what the cursor looks like too! That is
;; 100% something that should be done for a holiday themed thing.

;; TODO: I'd like to play around with having emacs be better able to
;; read large files (mostly just files with really really large lines
;; I guess). I assume turning off font lock mode in a big buffer would
;; be one thing that should be done.

;; TODO: Learned that there's a mode called "highlight-changes-mode"
;; which highlights text in the buffer which was recently changed. Now
;; it's got me thinking, "is this useful?" Like, it could help clarify
;; what you've done. I also wonder if you can jump to the
;; next/previous text that was changed by you.

;; TODO: How do you play music in emacs? Can you? The thought that
;; jumped into my head was that there's that M-r command in paredit
;; mode which deletes it's siblings and "raises" it up to the level of
;; the parent (also deleting the parent). Pretty brutal honestly. But
;; what if every time I hit it it would play the beginning of "you
;; raise me up" by Josh Groban. That would be pretty funny.

;; TODO: I think turning on whitespace mode seems like a potentially
;; good idea because then I can see tabs vs spaces which is useful
;; because sometimes I unintentionally mix them up when I don't want
;; to.

(blink-cursor-mode 0)
;; TODO: Turn this off in vterm mode unless copy mode is on.
(global-hl-line-mode t)

;; TODO: I think there's a bug in cider-jack-in. I renamed a directory
;; via dired and then when I tried to do cider-jack-in on that
;; directory it complained that it couldn't find the directory and
;; referenced the old directory that I renamed. Doing a C-x C-v on the
;; file and then running cider-jack-in got things flowing again.

;; TODO: Emacs really struggles with longs lines. Apparently when you
;; open a file with really long lines it will try and detect it and
;; activate "so-long-mode":
;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html
;; But a couple times I've made the mistake of pasting some
;; excessively large text into a buffer and it slows things down to a
;; crawl. I think I'll be more careful about that going forward but
;; could there be a way to do something like detect if what I'm about
;; to paste is too long and if so prompt me or disable font-lock-mode
;; and line numbers before doing the paste?

;; TODO: Check out this person's config
;; https://github.com/munen/emacs.d. Also check out their video:
;; https://www.youtube.com/watch?v=gfZDwYeBlO4&ab_channel=AlainM.Lafon
;; It feels like they really know what they're doing and I bet I could
;; learn something.

;; TODO: I think it would be cool if vterm would exit copy mode and
;; start typing for any key you enter. Right now it just says that I
;; can't exit copy mode but I think I'd prefer the behavior I just
;; described.

;; TODO: When displaying a long number, could I configure emacs to
;; somehow add commas to help break it up? Use case is that I'm
;; running golang benchmarks and for timings they output in
;; nanoseconds and it is just so hard to eyeball that.

;; TODO: We have a lot of account IDs in AWS and sometimes I end up
;; writing some of those account IDs into a file if I'm messing around
;; and wouldn't it be neat if, when emacs detects one of those account
;; ids, it will overlay it with the human readable name of the
;; account? Or something like that? Wonder what that would look like.

;; TODO: I would love it if the ff-get-other-file command in clojure
;; would have me jump between unit tests and non unit tests.

;; TODO: When I execute clojure unit tests via cider, at the start it
;; outputs what I expect it to, namely, it will output the exact forms
;; (like the function call and parameters of the unit under test which
;; is documented here:
;; https://clojure.github.io/clojure/clojure.test-api.html) but after
;; I mess around a bit it will instead start outputting the "diff"
;; between expected and actual and I don't get to see what the
;; original input was. What is doing that? I want to fix it.

;; TODO: One time when using magit something weird happened where the
;; minibuffer kind of got stuck? By "stuck" I mean a shadow of my
;; cursor was always in the minibuffer and when I would switch window
;; focus it would travel through the minibuffer and this error message
;; was happening: user-error: Cannot invoke transient magit-commit
;; while minibuffer is active. I was using version "27.2" of emacs and
;; version magit-20210701.2128 of magit.

;; Manually invoke company mode completions
(setq company-idle-delay 0.0)
(defun lag13/company-mode-customizations ()
  "Experimenting with triggering autocompletion manually instead
of automatically."
  ;; company-complete
  (define-key company-mode-map (kbd "<tab>") #'company-indent-or-complete-common))
(add-hook 'company-mode-hook #'lag13/company-mode-customizations)
;; (add-hook 'after-init-hook 'global-company-mode)

;; TODO: I miss vim's autocomplete features like file completion,
;; basic keyword matching, continuting to complete adjacent words, and
;; completing whole lines. They felt really simple but very useful and
;; I want to see if emacs has something similar. It seems that company
;; mode might be the way to go. People say it's easier to setup. Also
;; there is the function comint-dynamic-complete-filename
;; (https://superuser.com/questions/67170/how-do-i-complete-file-paths-in-emacs)
;; which might be just what I need. Just need to think of a good
;; keybinding to use. There is also M-/ similar to vim's C-p and C-n
;; commands. Maybe that's all I'll need. Read this for inspiration:
;; https://www.emacswiki.org/emacs/Completion#completion. This section
;; of the manual also mentions some stuff: 26.8 Completion for Symbol
;; Names.
;; (global-set-key (kbd "<S-tab> f") #'company-files)

(require 'lsp)
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("terraform-ls" "serve"))
                  :major-modes '(terraform-mode)
                  :server-id 'terraform-ls))
(add-hook 'terraform-mode-hook #'lsp-deferred)

;; (yas-global-mode 1)


;; TODO: I'm watching
;; https://www.youtube.com/watch?v=74zOY-vgkyw&list=PLEoMzSkcN8oPH1au7H6B7bBJ4ZO7BXjSZ&ab_channel=SystemCrafters
;; and he uses ivy to do code completion in the minibuffer. At first I
;; thought to myself, "I feel like icomplete gives me everything I
;; need" but then I was looking at the ivy UI and it displays the list
;; of options vertically instead of horizaontally and I feel like that
;; alone makes me kind of want it because sometimes if I see
;; myselection a couple choices away, horizontally it's kind of hard
;; to guage since each choice takes up a different amount of space but
;; if they were stacked vertically that would be much simpler I think.

;; TODO: For that golang table driven test yasnippet, make it so it is
;; more dynamic (i.e. upon expansion, it will ask how many function
;; arguments will be there and work appropriately). Perhaps this could
;; help: https://github.com/joaotavora/yasnippet/issues/348

;; TODO: in
;; https://www.youtube.com/watch?v=74zOY-vgkyw&list=PLEoMzSkcN8oPH1au7H6B7bBJ4ZO7BXjSZ&ab_channel=SystemCrafters
;; he got rid of the visual bell and replaced it with a little flash.
;; Think about doing this.

;; TODO: Make expanding a macro a keybinding, seems like a useful
;; thing to have at your fingertips within emacs.

;; TODO: Make a something which will tell you if today is a bones or
;; no bones day.

;; TODO: Doing C-h F will lookup the entered "command" in the manual
;; but there doesn't seem to be one for looking up any ol' "function"
;; in the manual. Can that be remedied? My use case was that I knew
;; that there was a url-retrieve function which limited the number of
;; concurrent processes and I knew it was on the same manual page as
;; url-retrieve but I couldn't conveniently get to that manual page!
;; This was the one btw:
;; https://www.gnu.org/software/emacs/manual/html_node/url/Retrieving-URLs.html

;; TODO: I think with an idle timer I could legitimately create a
;; screensaver within emacs. Make a screensaver!

;; TODO: I just did the github ssh key setup for a new laptop I have.
;; I think it could be cool if that setup could be automated entirely.
;; Maybe I could write something in emacs? Or just a script?

;; TODO: I built emacs on windows by using msys2 but I noticed that
;; lots of info manuals (like if I do C-h i) are missing (even for
;; emacs itself for instance). Learn about how to get those info
;; manuals back. I do see what I think is those manuals in ls
;; /msys64/mingw64/share/info/ so I'm not sure why it can't find them.
;; The already compiled binary of emacs that I downloaded does NOT
;; seem to have this issue IF you launch it via powershell but if I
;; launch it via git bash, same issue. So weird. Anyway... I should
;; stop looking at this and get back to other things. Maybe this is
;; helpful:
;; https://emacs.stackexchange.com/questions/10814/c-h-i-info-directory-is-nearly-empty.
;; It CAN find the emacs manual though by doing M-x info-emacs-manual
;; so it knows its there... it just doesn't show up in the info
;; listing for some reason.

;; TODO: It looks like C-) on windows does NOT work. Something about
;; the OS using that key for something else. I looked at this briefly
;; but couldn't follow the directions as listed:
;; https://stackoverflow.com/questions/10644131/paredit-forward-slurp-c-does-not-work-for-emacs-on-windows-7

;; TODO: I saw this and it just looked cool. Take a look!
;; https://github.com/sachac/waveform-el In general, she seems super
;; knowledgeable about emacs and I should probably check out our her
;; stuff perhaps starting with https://sachachua.com/dotemacs/

;; TODO: I was starting on my "trailhead" journey and it was defining
;; terms like trail, module, etc... All in all the relationship was
;; something like (borrowing haskell like syntax):
;;
;; Module = [Badge]
;; Badge = Module | Project
;; Module = [Unit]
;; Unit = a subtopic in the module with a hands on challenge or a quiz at the end
;; Project = list of instructions to follow which will ultimately produce something
;;
;; I'm not REALLY sure what I want here but I feel like writing things
;; in that syntax is helpful to me and I think it would be nice to be
;; able to write it as a regular data structure in lisp because... I
;; don't know what I want to do with it but if something is just data
;; then I can do... something with it!

;; TODO: I have this enabled but read this to understand it more:
;; https://www.reddit.com/r/orgmode/comments/j253ya/org_modes_intended_indentation/
;; That being said, I feel like having this makes org mode better
;; behaved. Without it indentation is added by default by adding
;; literal spaces when you're writing under a heading, that's annoying
;; though because if you want to copy this data somewhere else then it
;; will have that indentation too. Anyway, it seems good.

;; TODO: I've heard a couple mentions of org mode's "agenda" feature
;; for managing the stuff you have to do next like meetings or just
;; straight up todos. I wonder if I could integrate my work calendar
;; with it somehow. I think I like the idea of pulling most everything
;; into emacs. Even if it's not any more useful than using a separate
;; application I think going through the process of integrating it
;; with emacs would give me a better "low level" understanding of
;; things

;; TODO: I feel like I want org mode to be a little smarter. The
;; specific use case I have in mind at the moment is that if I press
;; return while editing a list, I want org mode to open up a new list
;; item. If I hit return again it could exit the list.

;; TODO: I think it would be cool if, from a documentation kind of
;; standpoint, you were able to ask if a function has an "inverse"
;; (like an isomorphic kind of thing) and it could tell you. Like the
;; opposite of a split string algorithm would be one that joins
;; strings.

;; TODO: I'd like to review tim pope's plugins for vim to see if
;; there's anything I can bring over for emacs. He just has so many
;; good ideas!

;; TODO: How do I do timer stuff in emacs? Use case was that at one of
;; my companies we wanted to see how long a feature flag flip would
;; actually take to take effect. I know I could just google a timer
;; but I'm curious, I want to do everything in emacs!! lol. One thing
;; I might like this for honestly though is to have emacs tell me when
;; I've been online for ~8 hours or something like that so I don't
;; over work.

(load "~/.emacs.d/lag13")

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)

;; On a windows machine I kept accidentally doing this action because
;; my palm would accidentally click the mouse. It would open a buffer
;; menu which I have no use for. TODO: I would like to understand how
;; keybinding works better. Like, usually I do the "kbd" thing but
;; here I've just got []?
(global-unset-key [C-down-mouse-1])

;; TODO: Another thought (which I may have already had lol) in the
;; interest of code splunking. If I see something which I think is
;; useful, I think it would be nice to "mark" the buffer I'm in so I
;; know to return to it. Maybe if the buffers were displayed in a
;; tabline across the top then that tab could be highlighted.

;; TODO: I think somehow my "buffer-file-coding-system" variable is
;; set to utf-8-unix. I say this because I was noticing those carriage
;; return characters (displayed as ^M) in my windows files but when I
;; opened emacs without any configuration, "buffer-file-coding-system"
;; was set to utf-8-dos. I'm not sure where this gets set but sniff it
;; out.

;; TODO: Mess around with trying to get a feature where I can
;; temporarily make a specific window the largest and only window but
;; then I can revert later:
;; https://stackoverflow.com/questions/7641755/maximizing-restoring-a-window-in-emacs

;; TODO: I think that for code splunking it could be helpful to be
;; able to have point on a place in a git managed file and have it
;; generate the "permalink" for that spot in the file. Furthermore,
;; emacs could then open that link locally instead of opening up the
;; link in github.

;; I think I've given vanilla emacs a fair shot I've used it for at
;; least 2 years as of 2022-01-04) and I want that I want my vim
;; editing back! TODO: make this sentence funnier like the intro quote
;; in doom emacs. Thanks to https://github.com/hlissner/doom-emacs for
;; some great documentation and inspirations on what evil packages to
;; add. Honestly, part of me wonders if I should just start using that
;; configuration framework! I get the feeling that in another
;; universe, I might have created something like doom emacs had it not
;; already been there.
(evil-mode 1)

;; I am only one man so I have not read EVERYTHING that this does but
;; it seems to be a fantastic package which makes sure that evil mode
;; is consistently used in "all" emacs modes. Go community!
(evil-collection-init)

(require 'evil-nerd-commenter-operator)
(evil-define-key '(normal visual) 'global "gc" #'evilnc-comment-operator)
;; TODO: I feel like the comment object for
;; https://github.com/redguardtoo/evil-nerd-commenter should default
;; to operating line-wise since that's the structure of line-wise
;; comments by definition. I tried my hand on doing that but it wasn't
;; working like I was expecting. At the end of the day I want "dic" to
;; delete ALL lines that the comment occupied. If I do "cic" I want it
;; to leave me with ONE empty line where the comment once was I'm not
;; sure why this is.
(define-key evil-inner-text-objects-map "c" #'evilnc-outer-commenter)

(define-key evil-motion-state-map (kbd "SPC") #'switch-to-buffer)
(evil-global-set-key 'motion (kbd "C-SPC") #'projectile-switch-to-buffer)

;; projectile also has a "project wide grep" command but it's pretty
;; lame because it defaults to non-regex searches (with no way to
;; configure it) AND it doesn't let you filter by file type (which I
;; think is handy). I also tried deadgrep but I didn't care for it's
;; interface (perhaps I'm too used to using a compilation-mode based
;; search interface). I also tried the other ripgrep integration that
;; projectile integrates with
;; (https://github.com/nlamirault/ripgrep.el) but it seemed less
;; polished as the one I neded up with
;; (https://github.com/dajva/rg.el) which has lots of nice features
;; like a built in project wide search!
(define-key evil-normal-state-map "S" #'rg-menu)

;; Argument text objects. TODO: I notice that if an argument is a
;; string with a "," in it then it doesn't work. Could we make it
;; work?
(define-key evil-inner-text-objects-map "a" #'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" #'evil-outer-arg)
(define-key evil-normal-state-map "ga" #'evil-jump-out-args)

;; Bring back vim's C-a/C-x commands. TODO:
;; https://github.com/cofi/evil-numbers is where this comes from and I
;; feel like that package could be an appropriate place to add number
;; text objects. Potential text objects could be "in" to select a
;; number MINUS any prefix like "0x" and "an" could be to include that
;; prefix.
(evil-define-key '(normal visual) 'global "+" #'evil-numbers/inc-at-pt)
(evil-define-key '(normal visual) 'global "-" #'evil-numbers/dec-at-pt)

(evil-define-key '(normal visual) 'global "Q" #'query-replace)

;; TODO: I'm not sure if there are emacs/evil motions to move by
;; indentation level even though I feel like there should be? After
;; all, emacs has things like forward-sexp, backward-sexp and
;; backward-up-list for it's own stuff, why wouldn't they have
;; structural stuff like that for other languages? Vim has
;; https://github.com/jeetsukumaran/vim-indentwise and I feel like I
;; should consider adding something like it to emacs. EDIT: Nevermind,
;; there seem to be some candidates:
;; https://gitlab.com/emacs-stuff/indent-tools/,
;; https://github.com/nixin72/block-nav.el

;; These bindings were listed in doom's documentation and I rather
;; liked them over the defaults from the package itself. TODO: I
;; notice that if I do "dii" then it ends up leaving one empty line
;; instead of deleting everything. Similarly, if I do "cii" there ends
;; up being TWO empty lines and we are in insert mode. It feels like
;; there is an off by one error here. I noticed the same thing with
;; the comment text objects too in
;; https://github.com/redguardtoo/evil-nerd-commenter. It looks like
;; within evil mode they use this "bounds-of-thing-at-point" function
;; within the library thingatpt.el. I wonder if these should be
;; expanding that library instead, the comments make it sound like
;; you're able to define other "things".

;; TODO: Speaking of that thingatpt.el library, I think there are some
;; other useful text objects that could be created using what is
;; already defined. It looks like the key to do this (or at least this
;; is the way evil seems to do it) is to define a function called:
;; "forward-comment &optional count" and then you can call
;; (bounds-of-thing-at-point 'comment) and it would work.

;; Got the mappings from what the OG plugin
;; https://github.com/michaeljsmith/vim-indent-object lays out.
(define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
(define-key evil-outer-text-objects-map "i" 'evil-indent-plus-i-indent-up)
(define-key evil-outer-text-objects-map "I" 'evil-indent-plus-i-indent-up-down)

;; TODO: This is my attempt to define more text objects like how evil
;; does it with some of the THINGS that thingpat already knows about.
;; The inner one seems to work no problem but the
;; evil-select-an-object doesn't work for some reason. I think the
;; problem is that the function evil-bounds-of-not-thing-at-point
;; needs to work and I think in order for that to work, a function
;; "forward-THING" must be defined AND it has to return a number.
(evil-define-text-object evil-inner-url (count &optional beg end type)
  "Select inner url."
  (evil-select-inner-object 'url beg end type count))
(evil-define-text-object evil-a-url (count &optional beg end type)
  "Select an url."
  (evil-select-an-object 'url beg end type count))
(define-key evil-inner-text-objects-map "u" 'evil-inner-url)
(define-key evil-outer-text-objects-map "u" 'evil-a-url)

(defun lag13-evil-macro-on-visual-selection ()
  "Makes it a bit quicker to execute a macro on multiple visually
selected lines."
  (interactive)
  (let ((evil-ex-initial-input "normal @"))
    (call-interactively #'evil-ex)))

(define-key evil-visual-state-map "@" #'lag13-evil-macro-on-visual-selection)

;; TODO: Figure out how to get this working. I'm not sure how to
;; straight up execute a desired EX command. I can load it up, but
;; loading it up and then executing it is different. I don't think I
;; have a lot of use for this tbh but I had it in my old config and it
;; seems like a fun exercise to get it working in emacs too.
(defun lag13-evil-dot-on-visual-selection ()
  "Makes it a bit quicker to execute a dot command on multiple
lines."
  (interactive)
  (let ((evil-ex-initial-input "normal ."))
    (call-interactively #'evil-ex)))
(define-key evil-visual-state-map "." #'lag13-evil-dot-on-visual-selection)

;; TODO: See if we want https://github.com/cute-jumper/evil-embrace.el
;; at any point for additional surrounding power.
(global-evil-surround-mode 1)

(define-key evil-motion-state-map "H" #'evil-first-non-blank)
(define-key evil-motion-state-map "L" #'evil-last-non-blank)

;; TODO: https://github.com/Dewdrops/evil-exchange says that the "cx"
;; binding that vim exchange is traditionally bound to in vim is not
;; possible in emacs and yet it is? I'm confused by that blurb in
;; their README. Understand it better. Also, that code which seems to
;; make this cx thing work feels super hacky, can it be improved
;; somehow? TODO: Also, the vim-exchange docs say that if one region
;; is completely contained within the other, then the containing
;; region will replace. I don't think this behaves in that manner.
(evil-exchange-cx-install)

;; Just gotta have it!
(global-evil-visualstar-mode)

;; TODO: See todos in this file, I think it's best that I add the
;; functionality I'm achieving in this file by making an addition to
;; evil-mode itself.
(load "~/.emacs.d/evil-cutlass")

;; TODO: https://github.com/PythonNut/evil-easymotion is listed in
;; doom emacs and apparently it's built on
;; https://github.com/abo-abo/avy. What's the relationship there?
;; Which one should I get?

;; TODO: Bring in https://github.com/noctuid/targets.el for my ol'
;; friend targets.vim.

;; TODO: Might need to look at this for multiple cursors in emacs:
;; https://github.com/gabesoft/evil-mc Or maybe we need just the
;; vanilla emacs one? Not sure

;; TODO: It's kind of a bummer now that doing "C-h k" doesn't work as
;; effectively when you've got operators like "y" AND "ys" defined. Is
;; there a way to improve on that?

;; TODO: How do I go to definition of something I type? Like in emacs
;; if I do C-u M-. then it'll let me type out the thing I want to
;; lookup the definition of. Not so with evil's "gd"

;; TODO: I feel like I want to be able to paste text from past copied
;; text by searching it using the same completion facilities that I
;; get when switching buffers or looking up commands.

;; TODO: I figured out the line ending issue where I was seeing the
;; windows line endings even though emacs should have known it was a
;; dos file (I knew that emacs knew because I ran it with no
;; customizations and it worked properly). The issue was that there
;; was an .editorconfig file in that repo and it was instructing emacs
;; to have line endings as just LF and, since I had the editorconfig
;; plugin, emacs obliged. That's all well and good but I really wish
;; there was some more insight into HOW the variable in question got
;; set. Like I feel like I wish that there was just more of a paper
;; trail with all this stuff that I could look back through to easily
;; figure it out. I wonder if this could help at all?
;; https://github.com/noctuid/annalist.el. What follows is my original
;; writing on this, feel my frustration! It's really so frustrating
;; sometimes how much time I end up fighting with my editor sometimes.
;; Sometimes it's not even that any thing is SUPER wrong per se but
;; something is not right and I want to figure out why so I get
;; distracted. Like with why the normal info documentation isn't
;; showing up on this windows version of emacs or why the carriage
;; return character is getting displayed on windows in my configured
;; emacs but not an emacs without configuration. Maybe other editors
;; would be like this too but it feels like other editors might just
;; be more... polished or something. Emacs and vim allow so much
;; customization (especially emacs) that I feel like it might be
;; tougher to track down what the problem is but with another editor
;; that is more managed, maybe that isn't the case? Maybe I should
;; check out visual studio code, I get the impression that it "just
;; works".

;; TODO: I think I'd like to bind C-[ and C-] to some functionality.
;; It would seem that C-[ is the same as ESC though so I need to do
;; something more. I think this is a way around it:
;; https://emacs.stackexchange.com/questions/7832/how-to-bind-c-for-real

(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; TODO: Revisit this to see if I like it. Originally I did it because
;; I was working in a really large repo so switching to another
;; project would just cause things to hang while it tried to get the
;; list of files. I need to work on that too but that's a separate
;; thing.back to a project 
(setq projectile-switch-project-action #'projectile-switch-to-buffer)

;; The inspiration for using vertico was:
;;
;; 1. I've wanted a minibuffer completion thing that stacks the
;; results vertically since it's easier to read than horizontally
;;
;; 2. I briefly briefly (i.e. like over 1 hour) tried ivy and helm and
;; liked vertico better out-of-the-box than the other two. One reason
;; for that is because it used emacs' built in completion capabilities
;; (https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion.html)
;; by default and I was already used to how those worked. Ivy and helm
;; on the other hand seemed to do their own thing and I wasn't sure
;; how to change them. I liked the default bindings too (RET to select
;; from the list and C-RET to use the text as is just makes sense to
;; me). Ivy also seemed to replace built in emacs commands with it's
;; own functions (like C-x b became ivy-switch-buffer) which I'm not
;; sure I like. It's not bad but I guess I trust that the stuff that
;; is core to emacs will get better over time and has more force
;; behind it to get better than some random package (I suppose the
;; opposite could be true too lol, smaller things like a single
;; package can have more agility than a big thing like emacs). Still,
;; I am a fan of using the "built in" stuff as much as possible and
;; only going outside for truly new things. Helm didn't work out of
;; the box for things like M-x so that's a drawback to it as well.
;;
;; 3. I was trying out projectile while using icomplete and noticed
;; that if I ran the projectile-switch-to-buffer command and hit RET
;; WITHOUT entering anything, then I'd get the error
;; "window-normalize-buffer-to-switch-to: Empty string for buffer name
;; is not allowed" but I wanted it to switch to the alternate buffer
;; since that's the default behavior if you do a regular ol' "C-x b".
;; I realize there is the command
;; projectile-project-buffers-other-buffer as well but I'm used to
;; buffer switching going to the previous buffer if there is no input.
;; Anyway, using vertico (ivy and helm did this too) just so happens
;; to make that behavior happen. I believe that using icomplete mode
;; results in that error because projectile will get the input with
;; the function completing-read and that will return the empty string
;; if I hit RET with no input which then results in a call to
;; (switch-to-buffer "") and leads to our error message.
(vertico-mode 1)

;; TODO: Holy shit I think I have some more packages to check out
;; thanks to our good friend:
;; https://www.youtube.com/watch?v=43Dg5zYPHTU&t=2s&ab_channel=ProtesilaosStavrou
;; There's a package called embark on there which seems particularly
;; interesting. Honestly I don't even know what I'd use it for but it
;; just looked so... cool. Felt like it opens up a more
;; flexible/powerful way of interacting with emacs. It feels like it's
;; the kind of thing that can move emacs closer to my brain: Here's a
;; writeup about it as well:
;; https://karthinks.com/software/fifteen-ways-to-use-embark/
;; Orderless also seems like a potentially useful completion framework
;; for something like emacs commands where I can remember words that
;; make up the command name but maybe not the order in which they
;; appear.

;; TODO: This seems like a neat package as well:
;; https://github.com/emacsmirror/0x0 Think about if we should use it.

;; TODO: Instead of company mode for in-buffer completions, maybe we
;; should take a look at https://github.com/minad/corfu and
;; https://github.com/minad/cape which were written by the same person
;; that did vertico.

;; TODO: dumb jump doesn't support "rg" for some regex's which
;; confuses me, I guess I thought it would just work for whatever the
;; underlying tool is?
(setq dumb-jump-find-rules
      '((:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "elisp"
				   :regex "\\\((defun|cl-defun)\\s+JJJ\\j"
				   ;; \\j usage see `dumb-jump-ag-word-boundary`
				   :tests ("(defun test (blah)" "(defun test\n" "(cl-defun test (blah)" "(cl-defun test\n")
				   :not ("(defun test-asdf (blah)" "(defun test-blah\n" "(cl-defun test-asdf (blah)"
					 "(cl-defun test-blah\n"  "(defun tester (blah)" "(defun test? (blah)" "(defun test- (blah)"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "elisp"
				   :regex "\\\(defvar\\b\\s*JJJ\\j"
				   :tests ("(defvar test " "(defvar test\n")
				   :not ("(defvar tester" "(defvar test?" "(defvar test-"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "elisp"
				   :regex "\\\(defcustom\\b\\s*JJJ\\j"
				   :tests ("(defcustom test " "(defcustom test\n")
				   :not ("(defcustom tester" "(defcustom test?" "(defcustom test-"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "elisp"
				   :regex "\\\(setq\\b\\s*JJJ\\j" :tests ("(setq test 123)")
				   :not ("setq test-blah 123)" "(setq tester" "(setq test?" "(setq test-"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "elisp"
				   :regex "\\\(JJJ\\s+" :tests ("(let ((test 123)))") :not ("(let ((test-2 123)))"))

			    ;; variable in method signature
			    (:type "variable" :supports ("ag" "rg" "git-grep") :language "elisp"
				   :regex "\\((defun|cl-defun)\\s*.+\\\(?\\s*JJJ\\j\\s*\\\)?"
				   :tests ("(defun blah (test)" "(defun blah (test blah)" "(defun (blah test)")
				   :not ("(defun blah (test-1)" "(defun blah (test-2 blah)" "(defun (blah test-3)"))

			    ;; common lisp
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "commonlisp"
				   :regex "\\\(defun\\s+JJJ\\j"
				   ;; \\j usage see `dumb-jump-ag-word-boundary`
				   :tests ("(defun test (blah)" "(defun test\n")
				   :not ("(defun test-asdf (blah)" "(defun test-blah\n"
					 "(defun tester (blah)" "(defun test? (blah)" "(defun test- (blah)"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "commonlisp"
				   :regex "\\\(defparameter\\b\\s*JJJ\\j"
				   :tests ("(defparameter test " "(defparameter test\n")
				   :not ("(defparameter tester" "(defparameter test?" "(defparameter test-"))

			    ;; racket
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "racket"
				   :regex "\\\(define\\s+\\(\\s*JJJ\\j"
				   :tests ("(define (test blah)" "(define (test\n")
				   :not ("(define test blah" "(define (test-asdf blah)" "(define test (lambda (blah"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "racket"
				   :regex "\\\(define\\s+JJJ\\s*\\\(\\s*lambda"
				   :tests ("(define test (lambda (blah" "(define test (lambda\n")
				   :not ("(define test blah" "(define test-asdf (lambda (blah)" "(define (test)" "(define (test blah) (lambda (foo"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "racket"
				   :regex "\\\(let\\s+JJJ\\s*(\\\(|\\\[)*"
				   :tests ("(let test ((blah foo) (bar bas))" "(let test\n" "(let test [(foo")
				   :not ("(let ((test blah"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "racket"
				   :regex "\\\(define\\s+JJJ\\j"
				   :tests ("(define test " "(define test\n")
				   :not ("(define (test"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "racket"
				   :regex "(\\\(|\\\[)\\s*JJJ\\s+"
				   :tests ("(let ((test 'foo" "(let [(test 'foo" "(let [(test 'foo" "(let [[test 'foo" "(let ((blah 'foo) (test 'bar)")
				   :not ("{test foo"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "racket"
				   :regex "\\\(lambda\\s+\\\(?[^\(\)]*\\s*JJJ\\j\\s*\\\)?"
				   :tests ("(lambda (test)" "(lambda (foo test)" "(lambda test (foo)")
				   :not ("(lambda () test"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "racket"
				   :regex "\\\(define\\s+\\\([^\(\)]+\\s*JJJ\\j\\s*\\\)?"
				   :tests ("(define (foo test)" "(define (foo test bar)")
				   :not ("(define foo test" "(define (test foo" "(define (test)"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "racket"
				   :regex "\\(struct\\s+JJJ\\j"
				   :tests ("(struct test (a b)"))

			    ;; scheme
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "scheme"
				   :regex "\\\(define\\s+\\(\\s*JJJ\\j"
				   :tests ("(define (test blah)" "(define (test\n")
				   :not ("(define test blah" "(define (test-asdf blah)" "(define test (lambda (blah"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "scheme"
				   :regex "\\\(define\\s+JJJ\\s*\\\(\\s*lambda"
				   :tests ("(define test (lambda (blah" "(define test (lambda\n")
				   :not ("(define test blah" "(define test-asdf (lambda (blah)" "(define (test)" "(define (test blah) (lambda (foo"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "scheme"
				   :regex "\\\(let\\s+JJJ\\s*(\\\(|\\\[)*"
				   :tests ("(let test ((blah foo) (bar bas))" "(let test\n" "(let test [(foo")
				   :not ("(let ((test blah"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "scheme"
				   :regex "\\\(define\\s+JJJ\\j"
				   :tests ("(define test " "(define test\n")
				   :not ("(define (test"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "scheme"
				   :regex "(\\\(|\\\[)\\s*JJJ\\s+"
				   :tests ("(let ((test 'foo" "(let [(test 'foo" "(let [(test 'foo" "(let [[test 'foo" "(let ((blah 'foo) (test 'bar)")
				   :not ("{test foo"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "scheme"
				   :regex "\\\(lambda\\s+\\\(?[^\(\)]*\\s*JJJ\\j\\s*\\\)?"
				   :tests ("(lambda (test)" "(lambda (foo test)" "(lambda test (foo)")
				   :not ("(lambda () test"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "scheme"
				   :regex "\\\(define\\s+\\\([^\(\)]+\\s*JJJ\\j\\s*\\\)?"
				   :tests ("(define (foo test)" "(define (foo test bar)")
				   :not ("(define foo test" "(define (test foo" "(define (test)"))

			    ;; c++
			    (:type "function" :supports ("ag" "rg" "git-grep") :language "c++"
				   :regex "\\bJJJ(\\s|\\))*\\((\\w|[,&*.<>:]|\\s)*(\\))\\s*(const|->|\\{|$)|typedef\\s+(\\w|[(*]|\\s)+JJJ(\\)|\\s)*\\("
				   :tests ("int test(){" "my_struct (*test)(int a, int b){" "auto MyClass::test ( Builder::Builder& reference, ) -> decltype( builder.func() ) {" "int test( int *random_argument) const {" "test::test() {" "typedef int (*test)(int);")
				   :not ("return test();)" "int test(a, b);" "if( test() ) {" "else test();"))

			    ;; (:type "variable" :supports ("grep") :language "c++"
			    ;;        :regex "(\\b\\w+|[,>])([*&]|\\s)+JJJ\\s*(\\[([0-9]|\\s)*\\])*\\s*([=,){;]|:\\s*[0-9])|#define\\s+JJJ\\b"
			    ;;        :tests ("int test=2;" "char *test;" "int x = 1, test = 2" "int test[20];" "#define test" "unsigned int test:2;"))

			    (:type "variable" :supports ("ag" "rg") :language "c++"
				   :regex "\\b(?!(class\\b|struct\\b|return\\b|else\\b|delete\\b))(\\w+|[,>])([*&]|\\s)+JJJ\\s*(\\[(\\d|\\s)*\\])*\\s*([=,(){;]|:\\s*\\d)|#define\\s+JJJ\\b"
				   :tests ("int test=2;" "char *test;" "int x = 1, test = 2" "int test[20];" "#define test" "typedef int test;" "unsigned int test:2")
				   :not ("return test;" "#define NOT test" "else test=2;"))

			    (:type "type" :supports ("ag" "rg" "git-grep") :language "c++"
				   :regex "\\b(class|struct|enum|union)\\b\\s*JJJ\\b\\s*(final\\s*)?(:((\\s*\\w+\\s*::)*\\s*\\w*\\s*<?(\\s*\\w+\\s*::)*\\w+>?\\s*,*)+)?((\\{|$))|}\\s*JJJ\\b\\s*;"
				   :tests ("typedef struct test {" "enum test {" "} test;" "union test {" "class test final: public Parent1, private Parent2{" "class test : public std::vector<int> {")
				   :not("union test var;" "struct test function() {"))

			    ;; clojure
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
				   :regex "\\(def\\s+JJJ\\j"
				   :tests ("(def test (foo)"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
				   :regex "\\(defn-?\\s+JJJ\\j"
				   :tests ("(defn test [foo]" "(defn- test [foo]")
				   :not ("(defn test? [foo]" "(defn- test? [foo]"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
				   :regex "\\(defmacro\\s+JJJ\\j"
				   :tests ("(defmacro test [foo]"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
				   :regex "\\(deftask\\s+JJJ\\j"
				   :tests ("(deftask test [foo]"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
				   :regex "\\(deftype\\s+JJJ\\j"
				   :tests ("(deftype test [foo]"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
				   :regex "\\(defmulti\\s+JJJ\\j"
				   :tests ("(defmulti test fn"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
				   :regex "\\(defmethod\\s+JJJ\\j"
				   :tests ("(defmethod test type"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
				   :regex "\\(definterface\\s+JJJ\\j"
				   :tests ("(definterface test (foo)"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
				   :regex "\\(defprotocol\\s+JJJ\\j"
				   :tests ("(defprotocol test (foo)"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "clojure"
				   :regex "\\(defrecord\\s+JJJ\\j"
				   :tests ("(defrecord test [foo]"))

			    ;; coffeescript
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "coffeescript"
				   :regex "^\\s*JJJ\\s*[=:].*[-=]>"
				   :tests ("test = ()  =>" "test= =>" "test = ->" "test=()->"
					   "test : ()  =>" "test: =>" "test : ->" "test:()->")
				   :not ("# test = =>" "test = 1"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "coffeescript"
				   :regex "^\\s*JJJ\\s*[:=][^:=-][^>]+$"
				   :tests ("test = $" "test : [" "test = {" "test = a")
				   :not ("test::a" "test: =>" "test == 1" "# test = 1"))

			    (:type "class" :supports ("ag" "grep" "rg" "git-grep") :language "coffeescript"
				   :regex "^\\s*\\bclass\\s+JJJ"
				   :tests ("class test" "class test extends")
				   :not ("# class"))

			    ;; obj-c
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "objc"
				   :regex "\\\)\\s*JJJ(:|\\b|\\s)"
				   :tests ("- (void)test" "- (void)test:(UIAlertView *)alertView")
				   :not ("- (void)testnot" "- (void)testnot:(UIAlertView *)alertView"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "objc"
				   :regex "\\b\\*?JJJ\\s*=[^=\\n]+"
				   :tests ("NSString *test = @\"asdf\"")
				   :not ("NSString *testnot = @\"asdf\"" "NSString *nottest = @\"asdf\""))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "objc"
				   :regex "(@interface|@protocol|@implementation)\\b\\s*JJJ\\b\\s*"
				   :tests ("@interface test: UIWindow")
				   :not ("@interface testnon: UIWindow"))


			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "objc"
				   :regex "typedef\\b\\s+(NS_OPTIONS|NS_ENUM)\\b\\([^,]+?,\\s*JJJ\\b\\s*"
				   :tests ("typedef NS_ENUM(NSUInteger, test)")
				   :not ("typedef NS_ENUMD(NSUInteger, test)"))

			    ;; swift
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "swift"
				   :regex "(let|var)\\s*JJJ\\s*(=|:)[^=:\\n]+"
				   :tests ("let test = 1234" "var test = 1234" "private lazy var test: UITapGestureRecognizer")
				   :not ("if test == 1234:"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "swift"
				   :regex "func\\s+JJJ\\b\\s*(<[^>]*>)?\\s*\\("
				   :tests ("func test(asdf)" "func test()" "func test<Value: Protocol>()")
				   :not ("func testnot(asdf)" "func testnot()"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "swift"
				   :regex "(class|struct|protocol|enum)\\s+JJJ\\b\\s*?"
				   :tests ("struct test" "struct test: Codable" "struct test<Value: Codable>"
					   "class test:" "class test: UIWindow" "class test<Value: Codable>")
				   :not ("class testnot:" "class testnot(object):" "struct testnot(object)"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "swift"
				   :regex "(typealias)\\s+JJJ\\b\\s*?="
				   :tests ("typealias test =")
				   :not ("typealias testnot"))

			    ;; c#
			    (:type "function" :supports ("ag" "rg") :language "csharp"
				   :regex "^\\s*(?:[\\w\\[\\]]+\\s+){1,3}JJJ\\s*\\\("
				   :tests ("int test()" "int test(param)" "static int test()" "static int test(param)"
					   "public static MyType test()" "private virtual SomeType test(param)" "static int test()")
				   :not ("test()" "testnot()" "blah = new test()"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "csharp"
				   :regex "\\s*\\bJJJ\\s*=[^=\\n)]+" :tests ("int test = 1234") :not ("if test == 1234:" "int nottest = 44"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "csharp"
				   :regex "(class|interface)\\s*JJJ\\b"
				   :tests ("class test:" "public class test : IReadableChannel, I")
				   :not ("class testnot:" "public class testnot : IReadableChannel, I"))

			    ;; java (literally the same regexes as c#, but different tests)
			    (:type "function" :supports ("ag" "rg") :language "java"
				   :regex "^\\s*(?:[\\w\\[\\]]+\\s+){1,3}JJJ\\s*\\\("
				   :tests ("int test()" "int test(param)" "static int test()" "static int test(param)"
					   "public static MyType test()" "private virtual SomeType test(param)" "static int test()"
					   "private foo[] test()")
				   :not ("test()" "testnot()" "blah = new test()" "foo bar = test()"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "java"
				   :regex "\\s*\\bJJJ\\s*=[^=\\n)]+" :tests ("int test = 1234") :not ("if test == 1234:" "int nottest = 44"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "java"
				   :regex "(class|interface)\\s*JJJ\\b"
				   :tests ("class test:" "public class test implements Something")
				   :not ("class testnot:" "public class testnot implements Something"))

			    ;; vala (again just like c#, exactly the same..)
			    (:type "function" :supports ("ag" "rg") :language "vala"
				   :regex "^\\s*(?:[\\w\\[\\]]+\\s+){1,3}JJJ\\s*\\\("
				   :tests ("int test()" "int test(param)" "static int test()" "static int test(param)"
					   "public static MyType test()" "private virtual SomeType test(param)" "static int test()")
				   :not ("test()" "testnot()" "blah = new test()"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "vala"
				   :regex "\\s*\\bJJJ\\s*=[^=\\n)]+" :tests ("int test = 1234") :not ("if test == 1234:" "int nottest = 44"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "vala"
				   :regex "(class|interface)\\s*JJJ\\b"
				   :tests ("class test:" "public class test : IReadableChannel, I")
				   :not ("class testnot:" "public class testnot : IReadableChannel, I"))

			    ;; coq
			    (:type "function" :supports ("ag" "rg" "git-grep") :language "coq"
				   :regex "\\s*Variable\\s+JJJ\\b"
				   :tests ("Variable test")
				   :not ("Variable testx"))

			    (:type "function" :supports ("ag" "rg" "git-grep") :language "coq"
				   :regex "\\s*Inductive\\s+JJJ\\b"
				   :tests ("Inductive test")
				   :not ("Inductive testx"))

			    (:type "function" :supports ("ag" "rg" "git-grep") :language "coq"
				   :regex "\\s*Lemma\\s+JJJ\\b"
				   :tests ("Lemma test")
				   :not ("Lemma testx"))

			    (:type "function" :supports ("ag" "rg" "git-grep") :language "coq"
				   :regex "\\s*Definition\\s+JJJ\\b"
				   :tests ("Definition test")
				   :not ("Definition testx"))

			    (:type "function" :supports ("ag" "rg" "git-grep") :language "coq"
				   :regex "\\s*Hypothesis\\s+JJJ\\b"
				   :tests ("Hypothesis test")
				   :not ("Hypothesis testx"))

			    (:type "function" :supports ("ag" "rg" "git-grep") :language "coq"
				   :regex "\\s*Theorm\\s+JJJ\\b"
				   :tests ("Theorm test")
				   :not ("Theorm testx"))

			    (:type "function" :supports ("ag" "rg" "git-grep") :language "coq"
				   :regex "\\s*Fixpoint\\s+JJJ\\b"
				   :tests ("Fixpoint test")
				   :not ("Fixpoint testx"))

			    (:type "function" :supports ("ag" "rg" "git-grep") :language "coq"
				   :regex "\\s*Module\\s+JJJ\\b"
				   :tests ("Module test")
				   :not ("Module testx"))

			    (:type "function" :supports ("ag" "rg" "git-grep") :language "coq"
				   :regex "\\s*CoInductive\\s+JJJ\\b"
				   :tests ("CoInductive test")
				   :not ("CoInductive testx"))

			    ;; python
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "python"
				   :regex "\\s*\\bJJJ\\s*=[^=\\n]+"
				   :tests ("test = 1234")
				   :not ("if test == 1234:" "_test = 1234"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "python"
				   :regex "def\\s*JJJ\\b\\s*\\\("
				   :tests ("\tdef test(asdf)" "def test()")
				   :not ("\tdef testnot(asdf)" "def testnot()"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "python"
				   :regex "class\\s*JJJ\\b\\s*\\\(?"
				   :tests ("class test(object):" "class test:")
				   :not ("class testnot:" "class testnot(object):"))

			    ;; matlab
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "matlab"
				   :regex "^\\s*\\bJJJ\\s*=[^=\\n]+"
				   :tests ("test = 1234")
				   :not ("for test = 1:2:" "_test = 1234"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "matlab"
				   :regex "^\\s*function\\s*[^=]+\\s*=\\s*JJJ\\b"
				   :tests ("\tfunction y = test(asdf)" "function x = test()" "function [x, losses] = test(A, y, lambda, method, qtile)")
				   :not ("\tfunction testnot(asdf)" "function testnot()"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "matlab"
				   :regex "^\\s*classdef\\s*JJJ\\b\\s*"
				   :tests ("classdef test")
				   :not ("classdef testnot"))

			    ;; nim
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "nim"
				   :regex "(const|let|var)\\s*JJJ\\*?\\s*(=|:)[^=:\\n]+"
				   :tests ("let test = 1234" "var test = 1234" "var test: Stat" "const test = 1234" "const test* = 1234")
				   :not ("if test == 1234:"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "nim"
				   :regex "(proc|func|macro|template)\\s*`?JJJ`?\\b\\*?\\s*\\\("
				   :tests ("\tproc test(asdf)" "proc test()" "func test()" "macro test()" "template test()" "proc test*()")
				   :not ("\tproc testnot(asdf)" "proc testnot()"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "nim"
				   :regex "type\\s*JJJ\\b\\*?\\s*(\\{[^}]+\\})?\\s*=\\s*\\w+"
				   :tests ("type test = object" "type test {.pure.} = enum" "type test* = ref object")
				   :not ("type testnot = object"))

			    ;; nix
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "nix"
				   :regex "\\b\\s*JJJ\\s*=[^=;]+"
				   :tests ("test = 1234;" "test = 123;" "test=123")
				   :not ("testNot = 1234;" "Nottest = 1234;" "AtestNot = 1234;"))

			    ;; ruby
			    (:type "variable" :supports ("ag" "rg" "git-grep") :language "ruby"
				   :regex "^\\s*((\\w+[.])*\\w+,\\s*)*JJJ(,\\s*(\\w+[.])*\\w+)*\\s*=([^=>~]|$)"
				   :tests ("test = 1234" "self.foo, test, bar = args")
				   :not ("if test == 1234" "foo_test = 1234"))

			    (:type "function" :supports ("ag" "rg" "git-grep") :language "ruby"
				   :regex "(^|[^\\w.])((private|public|protected)\\s+)?def\\s+(\\w+(::|[.]))*JJJ($|[^\\w|:])"
				   :tests ("def test(foo)" "def test()" "def test foo" "def test; end"
					   "def self.test()" "def MODULE::test()" "private def test")
				   :not ("def test_foo"))

			    (:type "function" :supports ("ag" "rg" "git-grep") :language "ruby"
				   :regex "(^|\\W)define(_singleton|_instance)?_method(\\s|[(])\\s*:JJJ($|[^\\w|:])"
				   :tests ("define_method(:test, &body)"
					   "mod.define_instance_method(:test) { body }"))

			    (:type "type" :supports ("ag" "rg" "git-grep") :language "ruby"
				   :regex "(^|[^\\w.])class\\s+(\\w*::)*JJJ($|[^\\w|:])"
				   :tests ("class test" "class Foo::test"))

			    (:type "type" :supports ("ag" "rg" "git-grep") :language "ruby"
				   :regex "(^|[^\\w.])module\\s+(\\w*::)*JJJ($|[^\\w|:])"
				   :tests ("module test" "module Foo::test"))

			    (:type "function" :supports ("ag" "rg" "git-grep") :language "ruby"
				   :regex "(^|\\W)alias(_method)?\\W+JJJ(\\W|$)"
				   :tests ("alias test some_method"
					   "alias_method :test, :some_method"
					   "alias_method 'test' 'some_method'"
					   "some_class.send(:alias_method, :test, :some_method)")
				   :not ("alias some_method test"
					 "alias_method :some_method, :test"
					 "alias test_foo test"))

			    ;; Groovy
			    (:type "variable" :supports ("ag" "rg" "git-grep") :language "groovy"
				   :regex "^\\s*((\\w+[.])*\\w+,\\s*)*JJJ(,\\s*(\\w+[.])*\\w+)*\\s*=([^=>~]|$)"
				   :tests ("test = 1234" "self.foo, test, bar = args")
				   :not ("if test == 1234" "foo_test = 1234"))

			    (:type "function" :supports ("ag" "rg" "git-grep") :language "groovy"
				   :regex "(^|[^\\w.])((private|public)\\s+)?def\\s+(\\w+(::|[.]))*JJJ($|[^\\w|:])"
				   :tests ("def test(foo)" "def test()" "def test foo" "def test; end"
					   "def self.test()" "def MODULE::test()" "private def test")
				   :not ("def test_foo"))

			    (:type "type" :supports ("ag" "rg" "git-grep") :language "groovy"
				   :regex "(^|[^\\w.])class\\s+(\\w*::)*JJJ($|[^\\w|:])"
				   :tests ("class test" "class Foo::test"))

			    ;; crystal
			    (:type "variable" :supports ("ag" "rg" "git-grep") :language "crystal"
				   :regex "^\\s*((\\w+[.])*\\w+,\\s*)*JJJ(,\\s*(\\w+[.])*\\w+)*\\s*=([^=>~]|$)"
				   :tests ("test = 1234" "self.foo, test, bar = args")
				   :not ("if test == 1234" "foo_test = 1234"))

			    (:type "function" :supports ("ag" "rg" "git-grep") :language "crystal"
				   :regex "(^|[^\\w.])((private|public|protected)\\s+)?def\\s+(\\w+(::|[.]))*JJJ($|[^\\w|:])"
				   :tests ("def test(foo)" "def test()" "def test foo" "def test; end"
					   "def self.test()" "def MODULE::test()" "private def test")
				   :not ("def test_foo"))

			    (:type "type" :supports ("ag" "rg" "git-grep") :language "crystal"
				   :regex "(^|[^\\w.])class\\s+(\\w*::)*JJJ($|[^\\w|:])"
				   :tests ("class test" "class Foo::test"))

			    (:type "type" :supports ("ag" "rg" "git-grep") :language "crystal"
				   :regex "(^|[^\\w.])module\\s+(\\w*::)*JJJ($|[^\\w|:])"
				   :tests ("module test" "module Foo::test"))

			    (:type "type" :supports ("ag" "rg" "git-grep") :language "crystal"
				   :regex "(^|[^\\w.])struct\\s+(\\w*::)*JJJ($|[^\\w|:])"
				   :tests ("struct test" "struct Foo::test"))

			    (:type "type" :supports ("ag" "rg" "git-grep") :language "crystal"
				   :regex "(^|[^\\w.])alias\\s+(\\w*::)*JJJ($|[^\\w|:])"
				   :tests ("alias test" "alias Foo::test"))

			    ;; scad
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "scad"
				   :regex "\\s*\\bJJJ\\s*=[^=\\n]+" :tests ("test = 1234") :not ("if test == 1234 {"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "scad"
				   :regex "function\\s*JJJ\\s*\\\("
				   :tests ("function test()" "function test ()"))

			    (:type "module" :supports ("ag" "grep" "rg" "git-grep") :language "scad"
				   :regex "module\\s*JJJ\\s*\\\("
				   :tests ("module test()" "module test ()"))

			    ;; scala
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "scala"
				   :regex "\\bval\\s*JJJ\\s*=[^=\\n]+" :tests ("val test = 1234") :not ("case test => 1234"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "scala"
				   :regex "\\bvar\\s*JJJ\\s*=[^=\\n]+" :tests ("var test = 1234") :not ("case test => 1234"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "scala"
				   :regex "\\btype\\s*JJJ\\s*=[^=\\n]+" :tests ("type test = 1234") :not ("case test => 1234"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "scala"
				   :regex "\\bdef\\s*JJJ\\s*\\\("
				   :tests ("def test(asdf)" "def test()"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "scala"
				   :regex "class\\s*JJJ\\s*\\\(?"
				   :tests ("class test(object)"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "scala"
				   :regex "trait\\s*JJJ\\s*\\\(?"
				   :tests ("trait test(object)"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "scala"
				   :regex "object\\s*JJJ\\s*\\\(?"
				   :tests ("object test(object)"))

			    ;; solidity
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "solidity"
				   :regex  "function\\s*JJJ\\s*\\\("
				   :tests ("function test() internal" "function test (uint x, address y)" "function test() external"))

			    (:type "modifier" :supports ("ag" "grep" "rg" "git-grep") :language "solidity"
				   :regex  "modifier\\s*JJJ\\s*\\\("
				   :tests ("modifier test()" "modifier test ()"))

			    (:type "event" :supports ("ag" "grep" "rg" "git-grep") :language "solidity"
				   :regex  "event\\s*JJJ\\s*\\\("
				   :tests ("event test();" "event test (uint indexed x)" "event test(uint x, address y)"))

			    (:type "error" :supports ("ag" "grep" "rg" "git-grep") :language "solidity"
				   :regex  "error\\s*JJJ\\s*\\\("
				   :tests ("error test();" "error test (uint x)" "error test(uint x, address y)"))

			    ;; R
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "r"
				   :regex "\\bJJJ\\s*=[^=><]" :tests ("test = 1234") :not ("if (test == 1234)"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "r"
				   :regex "\\bJJJ\\s*<-\\s*function\\b"
				   :tests ("test <- function" "test <- function(")
				   :not   ("test <- functionX"))

			    ;; perl
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "perl"
				   :regex "sub\\s*JJJ\\s*(\\{|\\()"
				   :tests ("sub test{" "sub test {" "sub test(" "sub test ("))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "perl"
				   :regex "JJJ\\s*=\\s*"
				   :tests ("$test = 1234"))

			    ;; Tcl
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "tcl"
				   :regex "proc\\s+JJJ\\s*\\{"
				   :tests ("proc test{" "proc test {"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "tcl"
				   :regex "set\\s+JJJ"
				   :tests ("set test 1234"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "tcl"
				   :regex "(variable|global)\\s+JJJ"
				   :tests ("variable test" "global test"))

			    ;; shell
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "shell"
				   :regex "function\\s*JJJ\\s*"
				   :tests ("function test{" "function test {" "function test () {")
				   :not   ("function nottest {"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "shell"
				   :regex "JJJ\\\(\\\)\\s*\\{"
				   :tests ("test() {")
				   :not ("testx() {"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "shell"
				   :regex "\\bJJJ\\s*=\\s*"
				   :tests ("test = 1234") :not ("blahtest = 1234"))

			    ;; php
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "php"
				   :regex "function\\s*JJJ\\s*\\\("
				   :tests ("function test()" "function test ()"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "php"
				   :regex "\\*\\s@method\\s+[^ \t]+\\s+JJJ\\("
				   :tests ("/** @method string|false test($a)" " * @method bool test()"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "php"
				   :regex "(\\s|->|\\$|::)JJJ\\s*=\\s*"
				   :tests ("$test = 1234" "$foo->test = 1234"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "php"
				   :regex "\\*\\s@property(-read|-write)?\\s+([^ \t]+\\s+)&?\\$JJJ(\\s+|$)"
				   :tests ("/** @property string $test" "/** @property string $test description for $test property"  " * @property-read bool|bool $test" " * @property-write \\ArrayObject<string,resource[]> $test"))
			    (:type "trait" :supports ("ag" "grep" "rg" "git-grep") :language "php"
				   :regex "trait\\s*JJJ\\s*\\\{"
				   :tests ("trait test{" "trait test {"))

			    (:type "interface" :supports ("ag" "grep" "rg" "git-grep") :language "php"
				   :regex "interface\\s*JJJ\\s*\\\{"
				   :tests ("interface test{" "interface test {"))

			    (:type "class" :supports ("ag" "grep" "rg" "git-grep") :language "php"
				   :regex "class\\s*JJJ\\s*(extends|implements|\\\{)"
				   :tests ("class test{" "class test {" "class test extends foo" "class test implements foo"))

			    ;; dart
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "dart"
				   :regex "\\bJJJ\\s*\\([^()]*\\)\\s*[{]"
				   :tests ("test(foo) {" "test (foo){" "test(foo){"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "dart"
				   :regex "class\\s*JJJ\\s*[\\\(\\\{]"
				   :tests ("class test(object) {" "class test{"))

			    ;; faust
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "faust"
				   :regex "\\bJJJ\(\\\(.+\\\)\)*\\s*="
				   :tests ("test = osc + 0.5;" "test(freq) = osc(freq) + 0.5;"))

			    ;; fennel
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "fennel"
				   :regex "\\((local|var)\\s+JJJ\\j"
				   :tests ("(local test (foo)"
					   "(var test (foo)"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "fennel"
				   :regex "\\(fn\\s+JJJ\\j"
				   :tests ("(fn test [foo]")
				   :not ("(fn test? [foo]"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "fennel"
				   :regex "\\(macro\\s+JJJ\\j"
				   :tests ("(macro test [foo]"))

			    ;; fortran
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "fortran"
				   :regex "\\s*\\bJJJ\\s*=[^=\\n]+"
				   :tests ("test = 1234")
				   :not ("if (test == 1234)"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "fortran"
				   :regex "\\b(function|subroutine|FUNCTION|SUBROUTINE)\\s+JJJ\\b\\s*\\\("
				   :tests ("function test (foo)" "integer function test(foo)"
					   "subroutine test (foo, bar)" "FUNCTION test (foo)"
					   "INTEGER FUNCTION test(foo)" "SUBROUTINE test (foo, bar)")
				   :not ("end function test" "end subroutine test" "END FUNCTION test"
					 "END SUBROUTINE test"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "fortran"
				   :regex "^\\s*(interface|INTERFACE)\\s+JJJ\\b"
				   :tests ("interface test" "INTERFACE test")
				   :not ("interface test2" "end interface test" "INTERFACE test2"
					 "END INTERFACE test"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "fortran"
				   :regex "^\\s*(module|MODULE)\\s+JJJ\\s*"
				   :tests ("module test" "MODULE test")
				   :not ("end module test" "END MODULE test"))

			    ;; go
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "go"
				   :regex "\\s*\\bJJJ\\s*=[^=\\n]+" :tests ("test = 1234") :not ("if test == 1234 {"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "go"
				   :regex "\\s*\\bJJJ\\s*:=\\s*" :tests ("test := 1234"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "go"
				   :regex "func\\s+\\\([^\\\)]*\\\)\\s+JJJ\\s*\\\("
				   :tests ("func (s *blah) test(filename string) string {"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "go"
				   :regex "func\\s+JJJ\\s*\\\("
				   :tests ("func test(url string) (string, error)"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "go"
				   :regex "type\\s+JJJ\\s+struct\\s+\\\{"
				   :tests ("type test struct {"))

			    ;; javascript extended
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "javascript"
				   :regex "(service|factory)\\\(['\"]JJJ['\"]" :tags ("angular")
				   :tests ("module.factory('test', [\"$rootScope\", function($rootScope) {"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "javascript"
				   :regex "\\bJJJ\\s*[=:]\\s*\\\([^\\\)]*\\\)\\s+=>" :tags ("es6")
				   :tests ("const test = (foo) => " "test: (foo) => {" "  test: (foo) => {"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "javascript"
				   :regex "\\bJJJ\\s*\\([^()]*\\)\\s*[{]" :tags ("es6")
				   :tests ("test(foo) {" "test (foo){" "test(foo){")
				   :not ("test = blah.then(function(){"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "javascript" :tags ("es6")
				   :regex "class\\s*JJJ\\s*[\\\(\\\{]"
				   :tests ("class test(object) {" "class test{"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "javascript" :tags ("es6")
				   :regex "class\\s*JJJ\\s+extends"
				   :tests ("class test extends Component{"))

			    ;; javascript
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "javascript"
				   :regex "\\s*\\bJJJ\\s*=[^=\\n]+" :tests ("test = 1234" "const test = props =>") :not ("if (test === 1234)"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "javascript"
				   :regex "\\bfunction\\b[^\\(]*\\\(\\s*[^\\)]*\\bJJJ\\b\\s*,?\\s*\\\)?"
				   :tests ("function (test)" "function (test, blah)" "function somefunc(test, blah) {" "function(blah, test)")
				   :not ("function (testLen)" "function (test1, blah)" "function somefunc(testFirst, blah) {" "function(blah, testLast)"
					 "function (Lentest)" "function (blahtest, blah)" "function somefunc(Firsttest, blah) {" "function(blah, Lasttest)"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "javascript"
				   :regex "function\\s*JJJ\\s*\\\("
				   :tests ("function test()" "function test ()"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "javascript"
				   :regex "\\bJJJ\\s*:\\s*function\\s*\\\("
				   :tests ("test: function()"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "javascript"
				   :regex "\\bJJJ\\s*=\\s*function\\s*\\\("
				   :tests ("test = function()"))

			    ;; hcl terraform
			    (:type "block" :supports ("ag" "grep" "rg" "git-grep") :language "hcl"
				   :regex "(variable|output|module)\\s*\"JJJ\"\\s*\\\{"
				   :tests ("variable \"test\" {"
					   "output \"test\" {"
					   "module \"test\" {"))

			    (:type "block" :supports ("ag" "grep" "rg" "git-grep") :language "hcl"
				   :regex "(data|resource)\\s*\"\\w+\"\\s*\"JJJ\"\\s*\\\{"
				   :tests ("data \"openstack_images_image_v2\" \"test\" {"
					   "resource \"google_compute_instance\" \"test\" {"))

			    ;; typescript
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
				   :regex "(service|factory)\\\(['\"]JJJ['\"]" :tags ("angular")
				   :tests ("module.factory('test', [\"$rootScope\", function($rootScope) {"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
				   :regex "\\bJJJ\\s*[=:]\\s*\\\([^\\\)]*\\\)\\s+=>"
				   :tests ("const test = (foo) => " "test: (foo) => {" "  test: (foo) => {"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
				   :regex "\\bJJJ\\s*\\([^()]*\\)\\s*[{]"
				   :tests ("test(foo) {" "test (foo){" "test(foo){")
				   :not ("test = blah.then(function(){"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
				   :regex "class\\s*JJJ\\s*[\\\(\\\{]"
				   :tests ("class test{"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
				   :regex "class\\s*JJJ\\s+extends"
				   :tests ("class test extends Component{"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
				   :regex "function\\s*JJJ\\s*\\\("
				   :tests ("function test()" "function test ()"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
				   :regex "\\bJJJ\\s*:\\s*function\\s*\\\("
				   :tests ("test: function()"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
				   :regex "\\bJJJ\\s*=\\s*function\\s*\\\("
				   :tests ("test = function()"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
				   :regex "\\s*\\bJJJ\\s*=[^=\\n]+" :tests ("test = 1234" "const test = props =>") :not ("if (test === 1234)"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "typescript"
				   :regex "\\bfunction\\b[^\\(]*\\\(\\s*[^\\)]*\\bJJJ\\b\\s*,?\\s*\\\)?"
				   :tests ("function (test)" "function (test, blah)" "function somefunc(test, blah) {" "function(blah, test)")
				   :not ("function (testLen)" "function (test1, blah)" "function somefunc(testFirst, blah) {" "function(blah, testLast)"
					 "function (Lentest)" "function (blahtest, blah)" "function somefunc(Firsttest, blah) {" "function(blah, Lasttest)"))

			    ;; julia
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "julia"
				   :regex "(@noinline|@inline)?\\s*function\\s*JJJ(\\{[^\\}]*\\})?\\("
				   :tests ("function test()" "@inline function test()"
					   "function test{T}(h)"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "julia"
				   :regex "(@noinline|@inline)?JJJ(\\{[^\\}]*\\})?\\([^\\)]*\\)\s*="
				   :tests ("test(a)=1" "test(a,b)=1*8"
					   "@noinline test()=1" "test{T}(x)=x"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "julia"
				   :regex "macro\\s*JJJ\\("
				   :tests ("macro test(a)=1" " macro test(a,b)=1*8"))

			    (:type "variable" :supports ("ag" "rg") :language "julia"
				   :regex "const\\s+JJJ\\b"
				   :tests ("const test = "))

			    (:type "type" :supports ("ag" "rg") :language "julia"
				   :regex "(mutable)?\\s*struct\\s*JJJ"
				   :tests ("struct test"))

			    (:type "type" :supports ("ag" "rg") :language "julia"
				   :regex "(type|immutable|abstract)\\s*JJJ"
				   :tests ("type test" "immutable test" "abstract test <:Testable" ))

			    ;; haskell
			    (:type "module" :supports ("ag") :language "haskell"
				   :regex "^module\\s+JJJ\\s+"
				   :tests ("module Test (exportA, exportB) where"))

                                        ; TODO Doesn't support any '=' in arguments. E.g. 'foo A{a = b,..} = bar'.
			    (:type "top level function" :supports ("ag") :language "haskell"
				   :regex "^\\bJJJ(?!(\\s+::))\\s+((.|\\s)*?)=\\s+"
				   :tests ("test n = n * 2"
					   "test X{..} (Y a b c) \n bcd \n =\n x * y"
					   "test ab cd e@Datatype {..} (Another thing, inTheRow) = \n undefined"
					   "test = runRealBasedMode @ext @ctx identity identity"
					   "test unwrap wrap nr@Naoeu {..} (Action action, specSpecs) = \n    undefined")
				   :not ("nottest n = n * 2"
					 "let testnot x y = x * y" "test $ y z" "let test a o = mda"
					 "test :: Sometype -> AnotherType aoeu kek = undefined"))

			    (:type "type-like" :supports ("ag") :language "haskell"
				   :regex "^\\s*((data(\\s+family)?)|(newtype)|(type(\\s+family)?))\\s+JJJ\\s+"
				   :tests ("newtype Test a = Something { b :: Kek }"
					   "data Test a b = Somecase a | Othercase b"
					   "type family Test (x :: *) (xs :: [*]) :: Nat where"
					   "data family Test "
					   "type Test = TestAlias")
				   :not ("newtype NotTest a = NotTest (Not a)"
					 "data TestNot b = Aoeu"))

                                        ; datatype contstuctor that doesn't match type definition.
			    (:type "(data)type constructor 1" :supports ("ag") :language "haskell"
				   :regex "(data|newtype)\\s{1,3}(?!JJJ\\s+)([^=]{1,40})=((\\s{0,3}JJJ\\s+)|([^=]{0,500}?((?<!(-- ))\\|\\s{0,3}JJJ\\s+)))"
				   :tests ("data Something a = Test { b :: Kek }"
					   "data Mem a = TrueMem { b :: Kek } | Test (Mem Int) deriving Mda"
					   "newtype SafeTest a = Test (Kek a) deriving (YonedaEmbedding)")
				   :not ("data Test = Test { b :: Kek }"))


			    (:type "data/newtype record field" :supports ("ag") :language "haskell"
				   :regex "(data|newtype)([^=]*)=[^=]*?({([^=}]*?)(\\bJJJ)\\s+::[^=}]+})"
				   :tests ("data Mem = Mem { \n mda :: A \n  , test :: Kek \n , \n aoeu :: E \n }"
					   "data Mem = Mem { \n test :: A \n  , mda :: Kek \n , \n aoeu :: E \n }"
					   "data Mem = Mem { \n mda :: A \n  , aoeu :: Kek \n , \n test :: E \n }"
					   "data Mem = Mem { test :: Kek } deriving Mda"
					   "data Mem = Mem { \n test :: Kek \n } deriving Mda"
					   "newtype Mem = Mem { \n test :: Kek \n } deriving (Eq)"
					   "newtype Mem = Mem { -- | Some docs \n test :: Kek -- ^ More docs } deriving Eq"
					   "newtype Mem = Mem { test :: Kek } deriving (Eq,Monad)"
					   "newtype NewMem = OldMem { test :: [Tx] }"
					   "newtype BlockHeaderList ssc = BHL\n { test :: ([Aoeu a], [Ssss])\n    } deriving (Eq)")
				   :not ("data Heh = Mda { sometest :: Kek, testsome :: Mem }"))

			    (:type "typeclass" :supports ("ag") :language "haskell"
				   :regex "^class\\s+(.+=>\\s*)?JJJ\\s+"
				   :tests (
					   "class (Constr1 m, Constr 2) => Test (Kek a) where"
					   "class  Test  (Veryovka a)  where ")
				   :not ("class Test2 (Kek a) where"
					 "class MakeTest (AoeuTest x y z) where"))

			    ;; ocaml
			    (:type "type" :supports ("ag" "rg") :language "ocaml"
				   :regex "^\\s*(and|type)\\s+.*\\bJJJ\\b"
				   :tests ("type test ="
					   "and test ="
					   "type 'a test ="
					   "type ('a, _, 'c) test"))

			    (:type "variable" :supports ("ag" "rg") :language "ocaml"
				   :regex "let\\s+JJJ\\b"
				   :tests ("let test ="
					   "let test x y ="))

			    (:type "variable" :supports ("ag" "rg") :language "ocaml"
				   :regex "let\\s+rec\\s+JJJ\\b"
				   :tests ("let rec test ="
					   "let rec  test x y ="))

			    (:type "variable" :supports ("ag" "rg") :language "ocaml"
				   :regex "\\s*val\\s*\\bJJJ\\b\\s*"
				   :tests ("val test"))

			    (:type "module" :supports ("ag" "rg") :language "ocaml"
				   :regex "^\\s*module\\s*\\bJJJ\\b"
				   :tests ("module test ="))

			    (:type "module" :supports ("ag" "rg") :language "ocaml"
				   :regex "^\\s*module\\s*type\\s*\\bJJJ\\b"
				   :tests ("module type test ="))

			    ;; lua
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "lua"
				   :regex "\\s*\\bJJJ\\s*=[^=\\n]+" :tests ("test = 1234") :not ("if test === 1234"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "lua"
				   :regex "\\bfunction\\b[^\\(]*\\\(\\s*[^\\)]*\\bJJJ\\b\\s*,?\\s*\\\)?"
				   :tests ("function (test)" "function (test, blah)" "function somefunc(test, blah)" "function(blah, test)")
				   :not ("function (testLen)" "function (test1, blah)" "function somefunc(testFirst, blah)" "function(blah, testLast)"
					 "function (Lentest)" "function (blahtest, blah)" "function somefunc(Firsttest, blah)" "function(blah, Lasttest)"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "lua"
				   :regex "function\\s*JJJ\\s*\\\("
				   :tests ("function test()" "function test ()"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "lua"
				   :regex "function\\s*.+[.:]JJJ\\s*\\\("
				   :tests ("function MyClass.test()" "function MyClass.test ()"
					   "function MyClass:test()" "function MyClass:test ()"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "lua"
				   :regex "\\bJJJ\\s*=\\s*function\\s*\\\("
				   :tests ("test = function()"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "lua"
				   :regex "\\b.+\\.JJJ\\s*=\\s*function\\s*\\\("
				   :tests ("MyClass.test = function()"))

			    ;; rust
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
				   :regex "\\blet\\s+(\\\([^=\\n]*)?(mut\s+)?JJJ([^=\\n]*\\\))?(:\\s*[^=\\n]+)?\\s*=\\s*[^=\\n]+"
				   :tests ("let test = 1234;"
					   "let test: u32 = 1234;"
					   "let test: Vec<u32> = Vec::new();"
					   "let mut test = 1234;"
					   "let mut test: Vec<u32> = Vec::new();"
					   "let (a, test, b) = (1, 2, 3);"
					   "let (a, mut test, mut b) = (1, 2, 3);"
					   "let (mut a, mut test): (u32, usize) = (1, 2);"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
				   :regex "\\bconst\\s+JJJ:\\s*[^=\\n]+\\s*=[^=\\n]+"
				   :tests ("const test: u32 = 1234;"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
				   :regex "\\bstatic\\s+(mut\\s+)?JJJ:\\s*[^=\\n]+\\s*=[^=\\n]+"
				   :tests ("static test: u32 = 1234;"
					   "static mut test: u32 = 1234;"))

			    ;; variable in method signature
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
				   :regex "\\bfn\\s+.+\\s*\\\((.+,\\s+)?JJJ:\\s*[^=\\n]+\\s*(,\\s*.+)*\\\)"
				   :tests ("fn abc(test: u32) -> u32 {"
					   "fn abc(x: u32, y: u32, test: Vec<u32>, z: Vec<Foo>)"
					   "fn abc(x: u32, y: u32, test: &mut Vec<u32>, z: Vec<Foo>)"))

			    ;; "if let" and "while let" desugaring
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
				   :regex "(if|while)\\s+let\\s+([^=\\n]+)?(mut\\s+)?JJJ([^=\\n\\\(]+)?\\s*=\\s*[^=\\n]+"
				   :tests ("if let Some(test) = abc() {"
					   "if let Some(mut test) = abc() {"
					   "if let Ok(test) = abc() {"
					   "if let Ok(mut test) = abc() {"
					   "if let Foo(mut test) = foo {"
					   "if let test = abc() {"
					   "if let Some(test) = abc()"
					   "if let Some((a, test, b)) = abc()"
					   "while let Some(test) = abc() {"
					   "while let Some(mut test) = abc() {"
					   "while let Ok(test) = abc() {"
					   "while let Ok(mut test) = abc() {")
				   :not ("while let test(foo) = abc() {"))

			    ;; structure fields
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
				   :regex "struct\\s+[^\\n{]+[{][^}]*(\\s*JJJ\\s*:\\s*[^\\n},]+)[^}]*}"
				   :tests ("struct Foo { abc: u32, test: Vec<String>, b: PathBuf }"
					   "struct Foo<T>{test:Vec<T>}"
					   "struct FooBar<'a> { test: Vec<String> }")
				   :not ("struct Foo { abc: u32, b: Vec<String> }"
					 "/// ... construct the equivalent ...\nfn abc() {\n"))

			    ;; enum variants
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
				   :regex "enum\\s+[^\\n{]+\\s*[{][^}]*\\bJJJ\\b[^}]*}"
				   :tests ("enum Foo { VariantA, test, VariantB(u32) }"
					   "enum Foo<T> { test(T) }"
					   "enum BadStyle{test}"
					   "enum Foo32 { Bar, testing, test(u8) }")
				   :not ("enum Foo { testing }"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
				   :regex "\\bfn\\s+JJJ\\s*\\\("
				   :tests ("fn test(asdf: u32)" "fn test()" "pub fn test()"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
				   :regex "\\bmacro_rules!\\s+JJJ"
				   :tests ("macro_rules! test"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
				   :regex "struct\\s+JJJ\\s*[{\\\(]?"
				   :tests ("struct test(u32, u32)"
					   "struct test;"
					   "struct test { abc: u32, def: Vec<String> }"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
				   :regex "trait\\s+JJJ\\s*[{]?"
				   :tests ("trait test;" "trait test { fn abc() -> u32; }"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
				   :regex "\\btype\\s+JJJ([^=\\n]+)?\\s*=[^=\\n]+;"
				   :tests ("type test<T> = Rc<RefCell<T>>;"
					   "type test = Arc<RwLock<Vec<u32>>>;"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
				   :regex "impl\\s+((\\w+::)*\\w+\\s+for\\s+)?(\\w+::)*JJJ\\s+[{]?"
				   :tests ("impl test {"
					   "impl abc::test {"
					   "impl std::io::Read for test {"
					   "impl std::io::Read for abc::test {"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "rust"
				   :regex "mod\\s+JJJ\\s*[{]?"
				   :tests ("mod test;" "pub mod test {"))

			    ;; elixir
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "elixir"
				   :regex "\\bdef(p)?\\s+JJJ\\s*[ ,\\\(]"
				   :tests ("def test do"
					   "def test, do:"
					   "def test() do"
					   "def test(), do:"
					   "def test(foo, bar) do"
					   "def test(foo, bar), do:"
					   "defp test do"
					   "defp test(), do:"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "elixir"
				   :regex "\\s*JJJ\\s*=[^=\\n]+"
				   :tests ("test = 1234")
				   :not ("if test == 1234"))

			    (:type "module" :supports ("ag" "grep" "rg" "git-grep") :language "elixir"
				   :regex "defmodule\\s+(\\w+\\.)*JJJ\\s+"
				   :tests ("defmodule test do"
					   "defmodule Foo.Bar.test do"))

			    (:type "module" :supports ("ag" "grep" "rg" "git-grep") :language "elixir"
				   :regex "defprotocol\\s+(\\w+\\.)*JJJ\\s+"
				   :tests ("defprotocol test do"
					   "defprotocol Foo.Bar.test do"))

			    ;; erlang
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "erlang"
				   :regex "^JJJ\\b\\s*\\\("
				   :tests ("test() ->"
					   "test()->"
					   "test(Foo) ->"
					   "test (Foo,Bar) ->"
					   "test(Foo, Bar)->"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "erlang"
				   :regex "\\s*JJJ\\s*=[^:=\\n]+"
				   :tests ("test = 1234")
				   :not ("if test =:= 1234"
					 "if test == 1234"))

			    (:type "module" :supports ("ag" "grep" "rg" "git-grep") :language "erlang"
				   :regex "^-module\\\(JJJ\\\)"
				   :tests ("-module(test)."))

			    ;; scss
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "scss"
				   :regex "@mixin\\sJJJ\\b\\s*\\\("
				   :tests ("@mixin test()"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "scss"
				   :regex "@function\\sJJJ\\b\\s*\\\("
				   :tests ("@function test()"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "scss"
				   :regex "JJJ\\s*:\\s*"
				   :tests ("test  :"))

			    ;; sml
			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "sml"
				   :regex "\\s*(data)?type\\s+.*\\bJJJ\\b"
				   :tests ("datatype test ="
					   "datatype test="
					   "datatype 'a test ="
					   "type test ="
					   "type 'a test ="
					   "type 'a test"
					   "type test")
				   :not ("datatypetest ="))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "sml"
				   :regex "\\s*val\\s+\\bJJJ\\b"
				   :tests ("val test ="
					   "val test="
					   "val test : bool"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "sml"
				   :regex "\\s*fun\\s+\\bJJJ\\b.*\\s*="
				   :tests ("fun test list ="
					   "fun test (STRING_NIL, a) ="
					   "fun test ((s1,s2): 'a queue) : 'a * 'a queue ="
					   "fun test (var : q) : int ="
					   "fun test f e xs ="))

			    (:type "module" :supports ("ag" "grep" "rg" "git-grep") :language "sml"
				   :regex "\\s*(structure|signature|functor)\\s+\\bJJJ\\b"
				   :tests ("structure test ="
					   "structure test : MYTEST ="
					   "signature test ="
					   "functor test (T:TEST) ="
					   "functor test(T:TEST) ="))

			    ;; sql
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "sql"
				   :regex "(CREATE|create)\\s+(.+?\\s+)?(FUNCTION|function|PROCEDURE|procedure)\\s+JJJ\\s*\\\("
				   :tests ("CREATE FUNCTION test(i INT) RETURNS INT"
					   "create or replace function test (int)"
					   "CREATE PROCEDURE test (OUT p INT)"
					   "create definer = 'test'@'localhost' procedure test()"))

			    (:type "table" :supports ("ag" "grep" "rg" "git-grep") :language "sql"
				   :regex "(CREATE|create)\\s+(.+?\\s+)?(TABLE|table)(\\s+(IF NOT EXISTS|if not exists))?\\s+JJJ\\b"
				   :tests ("CREATE TABLE test ("
					   "create temporary table if not exists test"
					   "CREATE TABLE IF NOT EXISTS test ("
					   "create global temporary table test"))

			    (:type "view" :supports ("ag" "grep" "rg" "git-grep") :language "sql"
				   :regex "(CREATE|create)\\s+(.+?\\s+)?(VIEW|view)\\s+JJJ\\b"
				   :tests ("CREATE VIEW test ("
					   "create sql security definer view test"
					   "CREATE OR REPLACE VIEW test AS foo"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "sql"
				   :regex "(CREATE|create)\\s+(.+?\\s+)?(TYPE|type)\\s+JJJ\\b"
				   :tests ("CREATE TYPE test"
					   "CREATE OR REPLACE TYPE test AS foo ("
					   "create type test as ("))

			    ;; systemverilog
			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "systemverilog"
				   :regex "\\s*class\\s+\\bJJJ\\b"
				   :tests ("virtual class test;" "class test;" "class test extends some_class")
				   :not ("virtual class testing;" "class test2;" "class some_test" "class some_class extends test"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "systemverilog"
				   :regex "\\s*task\\s+\\bJJJ\\b"
				   :tests ("task test (" "task test(")
				   :not ("task testing (" "task test2("))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "systemverilog"
				   :regex "\\s*\\bJJJ\\b\\s*="
				   :tests ("assign test =" "assign test=" "int test =" "int test=")
				   :not ("assign testing =" "assign test2="))

			    (:type "function" :supports ("ag" "rg" "git-grep") :language "systemverilog"
				   :regex "function\\s[^\\s]+\\s*\\bJJJ\\b"
				   :tests ("function Matrix test ;" "function Matrix test;")
				   :not ("function test blah"))

			    ;; matches SV class handle declarations
			    (:type "function" :supports ("ag" "rg" "git-grep") :language "systemverilog"
				   :regex "^\\s*[^\\s]*\\s*[^\\s]+\\s+\\bJJJ\\b"
				   :tests ("some_class_name test" "  another_class_name  test ;" "some_class test[];" "some_class #(1) test")
				   :not ("test some_class_name" "class some_class extends test"))

			    ;; vhdl
			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "vhdl"
				   :regex "\\s*type\\s+\\bJJJ\\b"
				   :tests ("type test is" "type test  is")
				   :not ("type testing is" "type test2  is"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "vhdl"
				   :regex "\\s*constant\\s+\\bJJJ\\b"
				   :tests ("constant test :" "constant test:")
				   :not ("constant testing " "constant test2:"))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "vhdl"
				   :regex "function\\s*\"?JJJ\"?\\s*\\\("
				   :tests ("function test(signal)" "function test (signal)" "function \"test\" (signal)")
				   :not ("function testing(signal"))

			    ;; latex
			    (:type "command" :supports ("ag" "grep" "rg" "git-grep") :language "tex"
				   :regex "\\\\.*newcommand\\\*?\\s*\\\{\\s*(\\\\)JJJ\\s*}"
				   :tests ("\\newcommand{\\test}" "\\renewcommand{\\test}" "\\renewcommand*{\\test}" "\\newcommand*{\\test}" "\\renewcommand{ \\test }")
				   :not("\\test"  "test"))

			    (:type "command" :supports ("ag" "grep" "rg" "git-grep") :language "tex"
				   :regex "\\\\.*newcommand\\\*?\\s*(\\\\)JJJ\\j"
				   :tests ("\\newcommand\\test {}" "\\renewcommand\\test{}" "\\newcommand \\test")
				   :not("\\test"  "test"))

			    (:type "length" :supports ("ag" "grep" "rg" "git-grep") :language "tex"
				   :regex "\\\\(s)etlength\\s*\\\{\\s*(\\\\)JJJ\\s*}"
				   :tests ("\\setlength { \\test}" "\\setlength{\\test}" "\\setlength{\\test}{morecommands}" )
				   :not("\\test"  "test"))

			    (:type "counter" :supports ("ag" "grep" "rg" "git-grep") :language "tex"
				   :regex "\\\\newcounter\\\{\\s*JJJ\\s*}"
				   :tests ("\\newcounter{test}" )
				   :not("\\test"  "test"))

			    (:type "environment" :supports ("ag" "grep" "rg" "git-grep") :language "tex"
				   :regex "\\\\.*newenvironment\\s*\\\{\\s*JJJ\\s*}"
				   :tests ("\\newenvironment{test}" "\\newenvironment {test}{morecommands}" "\\lstnewenvironment{test}" "\\newenvironment {test}" )
				   :not("\\test"  "test" ))

			    ;; pascal (todo: var, type, const)
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "pascal"
				   :regex "\\bfunction\\s+JJJ\\b"
				   :tests ("  function test : "))

			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "pascal"
				   :regex "\\bprocedure\\s+JJJ\\b"
				   :tests ("  procedure test ; "))

			    ;; f#
			    (:type "variable" :supports ("rg" "ag" "grep" "git-grep") :language "fsharp"
				   :regex "let\\s+JJJ\\b.*\\\="
				   :tests ("let test = 1234" "let test() = 1234" "let test abc def = 1234")
				   :not ("let testnot = 1234" "let testnot() = 1234" "let testnot abc def = 1234"))

			    (:type "interface" :supports ("rg" "ag" "grep" "git-grep") :language "fsharp"
				   :regex "member(\\b.+\\.|\\s+)JJJ\\b.*\\\="
				   :tests ("member test = 1234" "member this.test = 1234")
				   :not ("member testnot = 1234" "member this.testnot = 1234"))

			    (:type "type" :supports ("rg" "ag" "grep" "git-grep") :language "fsharp"
				   :regex "type\\s+JJJ\\b.*\\\="
				   :tests ("type test = 1234")
				   :not ("type testnot = 1234"))

			    ;; kotlin
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "kotlin"
				   :regex "fun\\s*(<[^>]*>)?\\s*JJJ\\s*\\("
				   :tests ("fun test()" "fun <T> test()"))
			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "kotlin"
				   :regex "(val|var)\\s*JJJ\\b"
				   :not ("val testval" "var testvar")
				   :tests ("val test " "var test"))
			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "kotlin"
				   :regex "(class|interface)\\s*JJJ\\b"
				   :tests ("class test" "class test : SomeInterface" "interface test"))

			    ;; zig
			    (:type "function" :supports ("ag" "grep" "rg" "git-grep") :language "zig"
				   :regex "fn\\s+JJJ\\b"
				   :tests ("fn test() void {"
					   "fn test(a: i32) i32 {"
					   "pub fn test(a: i32) i32 {"
					   "export fn test(a: i32) i32 {"
					   "extern \"c\" fn test(a: i32) i32 {"
					   "inline fn test(a: i32) i32 {"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "zig"
				   :regex "(var|const)\\s+JJJ\\b"
				   :tests ("const test: i32 = 3;"
					   "var test: i32 = 3;"
					   "pub const test: i32 = 3;"))

			    ;; protobuf
			    (:type "message" :supports ("ag" "grep" "rg" "git-grep") :language "protobuf"
				   :regex "message\\s+JJJ\\s*\\\{"
				   :tests ("message test{" "message test {"))

			    (:type "enum" :supports ("ag" "grep" "rg" "git-grep") :language "protobuf"
				   :regex "enum\\s+JJJ\\s*\\\{"
				   :tests ("enum test{" "enum test {"))

			    ;; apex (literally the same regexes as java)
			    (:type "function" :supports ("ag" "rg") :language "apex"
				   :regex "^\\s*(?:[\\w\\[\\]]+\\s+){1,3}JJJ\\s*\\\("
				   :tests ("int test()" "int test(param)" "static int test()" "static int test(param)"
					   "public static MyType test()" "private virtual SomeType test(param)" "static int test()"
					   "private foo[] test()")
				   :not ("test()" "testnot()" "blah = new test()" "foo bar = test()"))

			    (:type "variable" :supports ("ag" "grep" "rg" "git-grep") :language "apex"
				   :regex "\\s*\\bJJJ\\s*=[^=\\n)]+" :tests ("int test = 1234") :not ("if test == 1234:" "int nottest = 44"))

			    (:type "type" :supports ("ag" "grep" "rg" "git-grep") :language "apex"
				   :regex "(class|interface)\\s*JJJ\\b"
				   :tests ("class test:" "public class test implements Something")
				   :not ("class testnot:" "public class testnot implements Something"))))

(setq dumb-jump-language-file-exts
  '((:language "elisp" :ext "el" :agtype "elisp" :rgtype "elisp")
    (:language "elisp" :ext "el.gz" :agtype "elisp" :rgtype "elisp")
    (:language "commonlisp" :ext "lisp" :agtype "lisp" :rgtype "lisp")
    (:language "commonlisp" :ext "lsp" :agtype "lisp" :rgtype "lisp")
    (:language "c++" :ext "c" :agtype "cc" :rgtype "c")
    (:language "c++" :ext "h" :agtype "cc" :rgtype "c")
    (:language "c++" :ext "C" :agtype "cpp" :rgtype "cpp")
    (:language "c++" :ext "H" :agtype "cpp" :rgtype "cpp")
    (:language "c++" :ext "tpp" :agtype "cpp" :rgtype nil)
    (:language "c++" :ext "cpp" :agtype "cpp" :rgtype "cpp")
    (:language "c++" :ext "hpp" :agtype "cpp" :rgtype "cpp")
    (:language "c++" :ext "cxx" :agtype "cpp" :rgtype "cpp")
    (:language "c++" :ext "hxx" :agtype "cpp" :rgtype nil)
    (:language "c++" :ext "cc" :agtype "cpp" :rgtype "cpp")
    (:language "c++" :ext "hh" :agtype "cpp" :rgtype "cpp")
    (:language "c++" :ext "c++" :agtype nil :rgtype nil)
    (:language "c++" :ext "h++" :agtype nil :rgtype nil)
    (:language "coq" :ext "v" :agtype nil :rgtype nil)
    (:language "ocaml" :ext "ml" :agtype "ocaml" :rgtype "ocaml")
    (:language "ocaml" :ext "mli" :agtype "ocaml" :rgtype "ocaml")
    (:language "ocaml" :ext "mll" :agtype "ocaml" :rgtype "ocaml")
    (:language "ocaml" :ext "mly" :agtype "ocaml" :rgtype "ocaml")
    ;; groovy is nil type because jenkinsfile is not in searcher type lists
    (:language "groovy" :ext "gradle" :agtype nil :rgtype nil)
    (:language "groovy" :ext "groovy" :agtype nil :rgtype nil)
    (:language "groovy" :ext "jenkinsfile" :agtype nil :rgtype nil)
    (:language "haskell" :ext "hs" :agtype "haskell" :rgtype "haskell")
    (:language "haskell" :ext "lhs" :agtype "haskell" :rgtype "haskell")
    (:language "objc" :ext "m" :agtype "objc" :rgtype "objc")
    (:language "csharp" :ext "cs" :agtype "csharp" :rgtype "csharp")
    (:language "java" :ext "java" :agtype "java" :rgtype "java")
    (:language "vala" :ext "vala" :agtype "vala" :rgtype "vala")
    (:language "vala" :ext "vapi" :agtype "vala" :rgtype "vala")
    (:language "julia" :ext "jl" :agtype "julia" :rgtype "julia")
    (:language "clojure" :ext "clj" :agtype "clojure" :rgtype "clojure")
    (:language "clojure" :ext "cljc" :agtype "clojure" :rgtype "clojure")
    (:language "clojure" :ext "cljs" :agtype "clojure" :rgtype "clojure")
    (:language "clojure" :ext "cljx" :agtype "clojure" :rgtype "clojure")
    (:language "coffeescript" :ext "coffee" :agtype "coffee" :rgtype "coffeescript")
    (:language "faust" :ext "dsp" :agtype nil :rgtype nil)
    (:language "faust" :ext "lib" :agtype nil :rgtype nil)
    (:language "fennel" :ext "fnl" :agtype nil :rgtype nil)
    (:language "fortran" :ext "F" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "f" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "f77" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "f90" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "f95" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "F77" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "F90" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "F95" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "f03" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "for" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "ftn" :agtype "fortran" :rgtype "fortran")
    (:language "fortran" :ext "fpp" :agtype "fortran" :rgtype "fortran")
    (:language "go" :ext "go" :agtype "go" :rgtype "go")
    (:language "javascript" :ext "js" :agtype "js" :rgtype "js")
    (:language "javascript" :ext "jsx" :agtype "js" :rgtype "js")
    (:language "javascript" :ext "vue" :agtype "js" :rgtype "js")
    (:language "javascript" :ext "html" :agtype "html" :rgtype "html")
    (:language "javascript" :ext "css" :agtype "css" :rgtype "css")
    (:language "typescript" :ext "ts" :agtype "ts" :rgtype "ts")
    (:language "typescript" :ext "tsx" :agtype "ts" :rgtype "ts")
    (:language "typescript" :ext "vue" :agtype "ts" :rgtype "ts")
    (:language "dart" :ext "dart" :agtype nil :rgtype "dart")
    (:language "lua" :ext "lua" :agtype "lua" :rgtype "lua")
    ;; the extension "m" is also used by obj-c so must use matlab-mode
    ;; since obj-c will win by file extension, but here for searcher types
    (:language "matlab" :ext "m" :agtype "matlab" :rgtype "matlab")
    (:language "nim" :ext "nim" :agtype "nim" :rgtype "nim")
    (:language "nix" :ext "nix" :agtype "nix" :rgtype "nix")
    (:language "org" :ext "org" :agtype nil :rgtype "org")
    (:language "perl" :ext "pl" :agtype "perl" :rgtype "perl")
    (:language "perl" :ext "pm" :agtype "perl" :rgtype "perl")
    (:language "perl" :ext "pm6" :agtype "perl" :rgtype nil)
    (:language "perl" :ext "perl" :agtype nil :rgtype "perl")
    (:language "perl" :ext "plh" :agtype nil :rgtype "perl")
    (:language "perl" :ext "plx" :agtype nil :rgtype "perl")
    (:language "perl" :ext "pod" :agtype "perl" :rgtype "pod")
    (:language "perl" :ext "t" :agtype "perl" :rgtype nil)
    (:language "php" :ext "php" :agtype "php" :rgtype "php")
    (:language "php" :ext "php3" :agtype "php" :rgtype "php")
    (:language "php" :ext "php4" :agtype "php" :rgtype "php")
    (:language "php" :ext "php5" :agtype "php" :rgtype "php")
    (:language "php" :ext "phtml" :agtype "php" :rgtype "php")
    (:language "php" :ext "inc" :agtype "php" :rgtype nil)
    (:language "python" :ext "py" :agtype "python" :rgtype "py")
    (:language "r" :ext "R" :agtype "r" :rgtype "r")
    (:language "r" :ext "r" :agtype "r" :rgtype "r")
    (:language "r" :ext "Rmd" :agtype "r" :rgtype "r")
    (:language "r" :ext "Rnw" :agtype "r" :rgtype "r")
    (:language "r" :ext "Rtex" :agtype "r" :rgtype nil)
    (:language "r" :ext "Rrst" :agtype "r" :rgtype nil)
    (:language "racket" :ext "rkt" :agtype "racket" :rgtype "lisp")
    (:language "crystal" :ext "cr" :agtype "crystal" :rgtype "crystal")
    (:language "crystal" :ext "ecr" :agtype "crystal" :rgtype nil)
    (:language "ruby" :ext "rb" :agtype "ruby" :rgtype "ruby")
    (:language "ruby" :ext "erb" :agtype "ruby" :rgtype nil)
    (:language "ruby" :ext "haml" :agtype "ruby" :rgtype nil)
    (:language "ruby" :ext "rake" :agtype "ruby" :rgtype nil)
    (:language "ruby" :ext "slim" :agtype "ruby" :rgtype nil)
    (:language "rust" :ext "rs" :agtype "rust" :rgtype "rust")
    (:language "zig" :ext "zig" :agtype nil :rgtype "zig")
    (:language "scad" :ext "scad" :agtype nil :rgtype nil)
    (:language "scala" :ext "scala" :agtype "scala" :rgtype "scala")
    (:language "scheme" :ext "scm" :agtype "scheme" :rgtype "lisp")
    (:language "scheme" :ext "ss" :agtype "scheme" :rgtype "lisp")
    (:language "scheme" :ext "sld" :agtype "scheme" :rgtype "lisp")
    (:language "shell" :ext "sh" :agtype nil :rgtype nil)
    (:language "shell" :ext "bash" :agtype nil :rgtype nil)
    (:language "shell" :ext "csh" :agtype nil :rgtype nil)
    (:language "shell" :ext "ksh" :agtype nil :rgtype nil)
    (:language "shell" :ext "tcsh" :agtype nil :rgtype nil)
    (:language "sml" :ext "sml" :agtype "sml" :rgtype "sml")
    (:language "solidity" :ext "sol" :agtype nil :rgtype nil)
    (:language "sql" :ext "sql" :agtype "sql" :rgtype "sql")
    (:language "swift" :ext "swift" :agtype nil :rgtype "swift")
    (:language "tex" :ext "tex" :agtype "tex" :rgtype "tex")
    (:language "elixir" :ext "ex" :agtype "elixir" :rgtype "elixir")
    (:language "elixir" :ext "exs" :agtype "elixir" :rgtype "elixir")
    (:language "elixir" :ext "eex" :agtype "elixir" :rgtype "elixir")
    (:language "erlang" :ext "erl" :agtype "erlang" :rgtype "erlang")
    (:language "systemverilog" :ext "sv" :agtype "verilog" :rgtype "verilog")
    (:language "systemverilog" :ext "svh" :agtype "verilog" :rgtype "verilog")
    (:language "vhdl" :ext "vhd" :agtype "vhdl" :rgtype "vhdl")
    (:language "vhdl" :ext "vhdl" :agtype "vhdl" :rgtype "vhdl")
    (:language "scss" :ext "scss" :agtype "css" :rgtype "css")
    (:language "pascal" :ext "pas" :agtype "delphi" :rgtype nil)
    (:language "pascal" :ext "dpr" :agtype "delphi" :rgtype nil)
    (:language "pascal" :ext "int" :agtype "delphi" :rgtype nil)
    (:language "pascal" :ext "dfm" :agtype "delphi" :rgtype nil)
    (:language "fsharp" :ext "fs" :agtype "fsharp" :rgtype "fsharp")
    (:language "fsharp" :ext "fsi" :agtype "fsharp" :rgtype "fsharp")
    (:language "fsharp" :ext "fsx" :agtype "fsharp" :rgtype "fsharp")
    (:language "kotlin" :ext "kt" :agtype "kotlin" :rgtype "kotlin")
    (:language "kotlin" :ext "kts" :agtype "kotlin" :rgtype "kotlin")
    (:language "protobuf" :ext "proto" :agtype "proto" :rgtype "protobuf")
    (:language "hcl" :ext "tf" :agtype "terraform" :rgtype "tf")
    (:language "hcl" :ext "tfvars" :agtype "terraform" :rgtype nil)
    (:language "apex" :ext "cls" :agtype nil :rgtype nil)
    (:language "apex" :ext "trigger" :agtype nil :rgtype nil)))

(setq dumb-jump-force-searcher 'rg)

;; TODO: dumb jump says that if you include a .dumbjump file and
;; reference directories outside of the project root, then git-grep
;; doesn't go there but rg and the like do. Why is that? If you
;; specify another git repo then why couldn't dumb jump run git-grep
;; there as well and then combine the results?

;; TODO: I learned that by default, when you're viewing files in
;; visual studio code, the file doesn't become a current part of your
;; "buffer list" (lets call it) until you double click it or
;; something. I wonder if something like that could be useful when you
;; grep for a search term and are jumping through the buffers, because
;; there might be a lot of junk you don't care about and they just
;; clutter up the buffer list.

;; TODO: I want to look into how emacs does find file and find buffer
;; commands more. Like, couldn't they essentially be almost exactly
;; the same where you: 1. Get a list of the thing in question 2.
;; Filter the list? 2 would be the same for both so really 1 is the
;; only unique thing. Do they both do that? If not, why not? It feels
;; so simple and consistant! This operation would be useful really for
;; anything collection of information that you want to filter down.
;; Whether that be a list of words in a dictionary, looking up
;; definitions to get words (thesaurus style), filtering down
;; available functions, filtering down available namespaces in a
;; language, etc... EDIT: Looks like thre's a function called
;; "completing-read" which I think does what I'm curious about.

;; TODO: Thoughts about getting dumb jump to run more quickly in the
;; monolith. So I think I've confirmed that the regex which dumb jump
;; executes is just slow with rg. I suspect that it literally takes
;; too long for rg to scan the files but I'm not sure yet. I want to
;; investigate more. Part of me suspects that parameters don't filter
;; by files? Or even if they do does it really just take that long?? EDIT: Confirmed, it looks like the rg command is:

;; rg --color never --no-heading --line-number -U --pcre2 let\\s\+asOption\\b.\*\\\=\|member\(\\b.\+\\.\|\\s\+\)asOption\\b.\*\\\=\|type\\s\+asOption\\b.\*\\\= d:/inetpub

;; While the git grep command is:

;; git grep --color=never --line-number --untracked -E let\\s\+asOption\\b.\*\\\=\|member\(\\b.\+\\.\|\\s\+\)asOption\\b.\*\\\=\|type\\s\+asOption\\b.\*\\\= -- d\:/inetpub/\*.fs d\:/inetpub/\*.fsi d\:/inetpub/\*.fsx

;; I think the actual regex is this when I remove extra backslashes:

;; let\s+asOption\b.*\=|member(\b.+\.|\s+)asOption\b.*\=|type\s+asOption\b.*\='

;; We have a couple solutions to get implemented here and I think
;; they're all separate things to implement. First and foremost, we
;; should get that "--type fsharp" filter added to the ripgrep
;; commands. THEN I'm wondering if we should fix how dumb jump works
;; works with multiple file paths, either by having the logic not use
;; git grep automatically (as the README recommends) OR perhaps we
;; could choose the command to run based on the file path? That would
;; require a rework of the logic though. I think we might also need to
;; see if emacs is capable of running a process in a particular
;; direcotry. Finally I want to make the "rg" command be considered
;; for more file types. Like, I'm not sure why every command isn't
;; just automatically considered? It doesn't seem like the regex is
;; unique between commands or anything like that. I'll have to ask the
;; maintainer.

;; So, it looks like they do already add the appropriate --type
;; extension for rg but a certain field has to be set in the
;; dumb-jump-language-file-exts data structure. Again, is there a
;; reason why some of this stuff is not set already? It seems like it
;; should just default to working?

;; TODO: Do the bindings myself:
;; (evilnc-default-hotkeys)
;; (evil-global-set-key 'normal "gc" #'evilnc-comment-operator)

;; TODO: I'd be curious about what it would take to get fully
;; functional web browser within emacs. On
;; https://www.emacswiki.org/emacs/CategoryWebBrowser they mention
;; https://github.com/emacs-eaf/emacs-application-framework which
;; feels quite interesting. I also see this video:
;; https://www.youtube.com/watch?v=y1k_lA2VUYg&ab_channel=AnandTamariya

;; TODO: I did a ripgrep in a new repo I was exploring and basically
;; it returned a lot of test and non-test files and I only wanted to
;; look at the non-test files so I copied that ripgrep output to a
;; file and did a "keep-lines" for just the files in question and then
;; did "gf" on each one and poked around. I also felt like, "what if I
;; wanted to do a follow up grep in just these files?". I don't know,
;; just got me curious how doing that might be possible. In my head, I
;; feel like what is on the page is structured data and I want to be
;; able to better manipulate it. Oh woa, I found this for a way to do
;; search and replace with the grep output it's built into emacs:
;; https://github.com/mhayashi1120/Emacs-wgrep I wonder if there are
;; other goodies related to what I want.

;; TODO: I would love to get word wrapping stuff better handled in
;; emacs. Pressing M-q as I type has become a habit but I don't think
;; I want to use it when I'm typing out things that I could
;; potentially copy and share with other folks. Also, the word
;; wrapping, if it is there, just looks bad sometimes! For instance,
;; in markdown that's viewed on github if you have a long line on a
;; numbered list, the wrapped around portion will appear at an indent
;; equal to the text above. On emacs it just wraps to the beginning of
;; the next line. Ugly!

;; TODO: This seems to be another guy also demoing the same
;; embark+friends commands that I saw in another video:
;; https://www.youtube.com/watch?v=5ffb2at2d7w&ab_channel=MikeZamansky
;; I think his blog on emacs is probably worth checking out.

;; TODO: I noticed that the evil binding "] SPC" did not work in a
;; buffer called "asdf" that I opened. I assume that this buffer
;; didn't really have a mode and so evil didn't bind some stuff? Also,
;; I think wherever the unimpaired functionality is coming from, it's
;; not complete. I feel like I want to look into this more and figure
;; out what's going on.

;; TODO: I think I like emacs M-f/M-b motions for moving the cursor by
;; words better than vim. Like if you have hello.there.everyone then
;; it just moves over the words pretty much but vim will move on the
;; '.' characters too which makes it slower. I think I like those
;; motions from emacs for quickly motoring over on the same line and
;; it seems easier than vim because the motion feels more predictable.

;; TODO: An evil text object for the entire buffer

;; TODO: Just for fun, would it be possible to hack emacs such that it
;; is a lisp 1 instead of a lisp 2? I feel like parts of it could be.
;; I feel like every time you define something you just have to store
;; the value in both slots.

;; TODO: I like projectile's ability to switch between buffers in the
;; same project, could we also have the ability to switch between
;; buffers within or beneath the current directory? My use case was
;; exploring some of the installed emacs packages (the evil mode
;; packages) and I knew I had opened a file but I couldn't remember
;; the name and wished I could do a "switch to buffer within this
;; directory" action.

;; TODO: Feels like having a "statement" text object would be
;; something useful. Oftentimes a statment is just one line but
;; oftentimes it might get spread out over several. Like this one
;; specifically for python for example:
;; https://github.com/wbolster/evil-text-object-python

;; TODO: I noticed that the top of my init file was mysteriously
;; getting deleted sometimes and although I still don't understand WHY
;; it is happening, I think I can trigger it happening if I type
;; something like:
;;
;; d C-h k d
;;
;; So it would seem that getting help on evil operators does not work
;; properly (or perhaps I have some customizations which messes stuff
;; up somehow).

;; TODO: It looks like (warning: very rough understanding here) the
;; way that evil mode gets bindings like "dd" and "yy" working is that
;; the function evil-operator-range will bind things to the map
;; evil-operator-shortcut-map temporarily as the operator is
;; executing. I bring this up because I wanted to add a commentary
;; text object "gc" to https://github.com/linktohack/evil-commentary
;; like Tim Pope has defined for his vim plugin but with this
;; functionality that is seemingly built into evil, I don't think I
;; can do that.

;; TODO: I notice that the "i(" text object doesn't work when the
;; parentheses are within comments and the open and closed parentheses
;; are on different lines. I wonder why this is?

;; TODO: I feel like I've said this many times throughtout the TODOs
;; of this file but I want to say it again. I feel like the main
;; editor things I want to get down pat are: 1. Have a better method
;; for exploring new code. I feel like right now I'll hop around a lot
;; and I'll see areas which, in hindsight, are points of interest that
;; I should keep open in a small window or something so I can quickly
;; refer back to it but during the code exploration I hop around so
;; quickly sometimes that I don't even know what file I'm in. There
;; could be two points of interest which are literally one page scroll
;; away from eachother but I don't know that and the only way I know
;; how to find is by doing a "jump to definition" if I can remember
;; the function name OR by spamming C-o until I end up where I was
;; before. And it doesn't help too that the "previous/next-buffer"
;; commands are local to a window because sometimes I'll have splits
;; and I'll do previous/next-buffer but it will take me somewhere
;; totally unexpected. Yeah... I bet a lot of improving this will be
;; coming up with a better method of code searching but at the same
;; time I feel like I should be able to lean on emacs somehow to help
;; me (either with builtin functionality or writing something). 2.
;; Project wide search and replace. 3. Some sort of consistant way to
;; navigate structured text. Navigating lisp'y things is pretty much
;; there but I'd like one for indentation as well and then I'd be
;; curious to see if the bindings I can come up with can be somewhat
;; consistant between the two. 4. A text object for a "top level form"
;; (i.e. probably a function). 5. Multiple cursors 6. Fuzzy file
;; search inside ENTIRE project (i.e. ctrl-p like functionality)

;; TODO: Doing :global//whatever doesn't seem to work when there are
;; multiple lines in the search pattern. Is this typical? I was trying
;; to cleanup the output from a command which had a lot of those
;; colore terminal characters and it was easier to just do a visual
;; star highlight (as opposed to trying to get the regex working
;; properly) and try to run the global command but it wasn't working.

;; TODO: I want to be able to better sift through a json object in
;; emacs. Like, I ran a: npm audit --json command and I wanted to do
;; something like "look for all packages with high severity and see
;; which packages brought those in or if they are in the root
;; project.json". Could be cool to have something interactive (in
;; general I feel like I'm starting to want more dwim sorts of things)
;; but maybe I just need to learn how to do json stuff in emacs.

;; TODO: I feel like it should be more possible in an interactive
;; environment to doing some computation, pause it, do something else,
;; then resume that computation. Like maybe in emacs we start telling
;; it to do a query replace and we've typed in a really complicated
;; regex but then we realize we want to do something else. What do we
;; do? Ideally I feel like we could pause it and come back to it but
;; right now I feel like I have to quit out of the query replace
;; entirely and start that flow over again later. All these things use
;; the minibuffer so surely it feels like, at the very least, the text
;; I typed could be saved and then recalled?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Line stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(line-number-mode 0)

(defvar lag13-mode-line-buffer-identification
  (propertized-buffer-identification "%b")
  "My version of the `mode-line-buffer-identification'. The only
difference is that mine does not include padding which I want
because I want to try having a more concise modeline.")

;; Tell what mode I'm in purely based on the cursor.
(setq evil-mode-line-format nil)
(setq evil-emacs-state-cursor 'hollow)

(defun lag13-mode-line-project-info ()
  "Returns information about the current project."
  (format "[%s]" (projectile-project-name)))

;; Straight up modifying the mode line to include just the info I
;; want. I looked through the default value of the modeline and
;; removed anything I didn't feel I wanted.
(setq mode-line-format
      '("%e"
	mode-line-mule-info
	mode-line-client
	mode-line-modified
	mode-line-remote
	" "
	mode-line-position
	" "
	lag13-mode-line-buffer-identification
	" "
	(:eval (lag13-mode-line-project-info))
	vc-mode
	mode-line-end-spaces
	))

;; TODO: Modify the mode-line-inactive face so that it is more obvious
;; which window is active and which is not.

;; TODO: I feel like saving the recent ripgrep search in a register "g
;; seems like a good idea. Because sometimes I do a ripgrep and then
;; want to go into wgrep mode to do search and replace stuff but I
;; might have to type out the regex again to get it.

;; TODO: How do we do ripgrep in multiple directories?

;; TODO: doing the command "dap" in an org mode document does not seem
;; to be working like I expect it to. If you are at the end of a '*'
;; header, then doing "dap" also targets the NEXT header down. Not
;; sure what's up with that.

;; More and more lately I'm feeling like if there's ANY way to help
;; add more context to what I'm doing, that will help me. In this
;; case, I feel like I'll jump around too quickly and I don't even
;; know what files I've looked at or how many I've looked at. It could
;; feel like I've looked at 10 different files in a project when
;; really I've only looked at 2 simply because I'm too focused on the
;; content instead of the file names. Maybe that means I just need to
;; slow down... but I'm not so sure. Adding another sensory cue (i.e.
;; showing the files up top) is going to be my attempt to solve for
;; it. TODO: I'd be curious to see how easy it would be to bend emacs'
;; default tab-line feature to do what I want instead of using
;; centaur-tabs. I mean, I guess centaur tabs is just leaning on
;; tab-line anyway but still, I'm curious about the internals.
(require 'centaur-tabs)
(centaur-tabs-mode 1)
(centaur-tabs-headline-match)
;; TODO: Only add the buffers to the list of previously visited
;; buffers if you do something within that buffer or remain on that
;; buffer for more than X seconds or something like that.
(define-key evil-normal-state-map (kbd "<C-tab>") 'centaur-tabs-forward)
(define-key evil-normal-state-map (kbd "<S-C-tab>") 'centaur-tabs-backward)

;; TODO: It would be interesting to compare centaur with emacs' built
;; in global-tab-line-mode

;; TODO: If I have a vertically split window and 5 tabs open (or at
;; least enough to cover >=3/4 of the screen) and I navigate to the
;; final tab, the beginning tab gets shifted out of view, I don't like
;; that. Could we try to keep all tabs in view? Or at least give an
;; indication when tabs are hidden?

;; TODO: Would it be worth making a PR to add this to the centaur
;; repo? Maybe it could be the default behavior?
(setq centaur-tabs-tab-label-function
      (lambda (cur-tab)
	(cl-flet ((get-tab-filename
		   (tab)
		   (-if-let (buf-file-name (buffer-file-name (centaur-tabs-tab-value tab)))
		       (file-name-nondirectory buf-file-name))))
	  (let ((cur-tab-filename (get-tab-filename cur-tab)))
	    (if (or (not cur-tab-filename)
		    (> (seq-count (lambda (filename) (equal cur-tab-filename filename))
				  (->> (centaur-tabs-tab-tabset cur-tab)
				       (centaur-tabs-view)
				       (seq-map #'get-tab-filename)))
		       1))
		(buffer-name (centaur-tabs-tab-value cur-tab))
	      cur-tab-filename)))))

;; TODO: Looks like centaur-tabs-buffer-groups does not assign
;; magit-revision-mode to the "Emacs" tab group which happens with
;; other magit buffers. Seems like I should make a PR to fix that.
;; Might be worth changing the check to be (string-prefix-p "magit"
;; (format "%s" major-mode)) or maybe just add another line to the
;; check.

;; TODO: The function centaur-tabs-hide-tab tries to hide magit
;; buffers but misses a lot of them because a lot of them will have a
;; file extensions which seems to be the name of the project. So fix
;; that or at least ask about it.

;; Like many things it is, strictly speaking, unnecessary but I think
;; it's a useful action to have! I think the only thing really going
;; for it over using more primitive actions (like 'c'hanging a text
;; object then pasting or 'v'isually highlighting a region then
;; pasting) is that it will automatically indent the pasted code.
(require 'evil-replace-with-register)
(evil-replace-with-register-install)

;; ' is easier to reach on my keyboard than `
(define-key evil-motion-state-map "'" 'evil-goto-mark)
(define-key evil-motion-state-map "`" 'evil-goto-mark-line)

;; TODO: There are a couple TODOs on
;; https://github.com/noctuid/targets.el to implement an "entire
;; buffer" text object as well as a "line" text object (I know we have
;; a repeated operator key to do a line but these are dedicated ones
;; and I think the "inner" text object copies by characters).
;; https://www.reddit.com/r/emacs/comments/3ctjsv/evil_ilal_and_ieae_textobjects/
;; https://github.com/supermomonga/evil-textobj-entire

;; TODO: Could be useful to store the last emacs minibuffer code
;; evaluation in a register?

;; TODO: When switching projects in projectile, can the projects be
;; ordered by most recently used? They seem to just be sorted
;; alphabetically or something.

;; TODO: The g; command is very handy to go back to previous changes.
;; I feel like it would also be useful to have a similar command which
;; goes back to a previous change that happened somewhere NOT
;; currently on screen so we don't have to spam g; if we've made lot's
;; of changes on screen and want to go somewhere farther away. In this
;; line of thinking... would it be helpful to highlight areas of the
;; buffer where changes have happened recently? i.e. where g; would
;; jump to?

;; TODO: I feel like I need a jump list per buffer. Because sometimes
;; I know I want to go go back to place I've been in the file before
;; so I spam C-o but if I've jumped between files then I'll have to
;; iterate through that stuff first.

;; TODO: Tim Pope had a vim plugin to try and figure out what the
;; indentation style SHOULD be based on the file that is being read. I
;; would love something like that. This is motivated by the fact that
;; I'm editing some javascript and my emacs indentation settings are
;; not the same as what this project wants (I use spaces they use
;; tabs) and I just feel to lazy to try and figure out how to fix it
;; myself. I just want it to work!

;; TODO: Could we create a binding to repeat the last emacs command
;; invocation (i.e. whatever was run with M-x)? I know vim has @: to
;; repeat the last ex command so it would seem fitting to allow the
;; same with emacs. Just seems like a fun thing to try.

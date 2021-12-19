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
 '(completion-styles '(basic partial-completion emacs22 flex))
 '(custom-enabled-themes '(solarized-dark-high-contrast))
 '(custom-safe-themes
   '("830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(delete-selection-mode t)
 '(eval-expression-print-length 100)
 '(eval-expression-print-level 10)
 '(evil-cross-lines t)
 '(evil-disable-insert-state-bindings t)
 '(evil-want-fine-undo t)
 '(explicit-shell-file-name "bash")
 '(exwm-floating-border-color "#191b20")
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "vendor"))
 '(groovy-indent-offset 2)
 '(help-window-select t)
 '(history-delete-duplicates t)
 '(icomplete-compute-delay 0.0)
 '(icomplete-mode t)
 '(initial-buffer-choice "~/.emacs.d/init.el")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((meta)) ((control) . text-scale)))
 '(objed-cursor-color "#ff6c6b")
 '(org-startup-indented t)
 '(package-selected-packages
   '(evil xmlgen web-server go-snippets company yasnippet lsp-mode dumb-jump solarized-theme doom-themes vterm-toggle vterm magit paredit plantuml-mode groovy-mode nginx-mode jinja2-mode systemd terraform-mode cider typescript-mode edit-indirect clojure-mode haskell-mode php-mode dockerfile-mode elm-mode restclient yaml-mode markdown-mode go-guru editorconfig go-mode))
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#282c34"))
 '(recentf-mode t)
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(vterm-max-scrollback 100000)
 '(vterm-toggle-hide-method 'reset-window-configration))
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

;; https://www.emacswiki.org/emacs/MidnightMode
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

;; I like specifying case when typing file names. To be honest a good
;; reason I want this is probably so typing just 'D' can complete on
;; "Dockerfile" and it won't match on something else like
;; "docker-compose.yml".
(setq read-file-name-completion-ignore-case nil)

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
;; <left>/<right> a little quicker when you need to repeat it.
(global-set-key (kbd "<left>") 'lag13-left-char-or-previous-buffer)
(global-set-key (kbd "<right>") 'lag13-right-char-or-next-buffer)

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

;; In emacs scrolling up means that buffer scrolls up relative to the
;; window hence why scrolling "down" really means you'll view early
;; portions of the file.
(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-n") 'scroll-up-line)

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

;; TODO: Is it possible to auto save a file after emacs loses focus
;; (i.e. we switch to another app)? Not sure if I need to do it but
;; I'm just curious if that kind of "hook" is available.

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
;; but I'm curious, I want to do everything in emacs!! lol.

(load "~/.emacs.d/lag13")

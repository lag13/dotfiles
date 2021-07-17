;; My Emacs configuration file. The path to this file is determined by
;; user-init-file which gets dynamically set when emacs starts. I
;; believe that when emacs starts up it does something like "find the
;; first non empty file in the list (~/.emacs ~/.emacs.d/init.el).
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(clean-buffer-list-delay-general 7)
 '(custom-enabled-themes nil)
 '(explicit-shell-file-name "bash")
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "vendor"))
 '(groovy-indent-offset 2)
 '(package-selected-packages
   '(vterm magit paredit plantuml-mode groovy-mode nginx-mode jinja2-mode systemd terraform-mode cider typescript-mode edit-indirect clojure-mode haskell-mode php-mode dockerfile-mode elm-mode restclient yaml-mode markdown-mode go-guru editorconfig go-mode))
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; https://www.emacswiki.org/emacs/MidnightMode
(require 'midnight)
(midnight-delay-set 'midnight-delay "9:00am")

(toggle-frame-maximized)

;; Load packages. As of Emacs 25.1 the (package-initialize) function
;; will actually write a call to (package-initialize) in the init file
;; if such a call does not already exist (describe-function
;; package--ensure-init-file). So it would appear that the elpa
;; package authors prefer it if package loading happens before the
;; init file runs. Doing it this way does feel a bit hacky though. I
;; wonder if they are trying to make loading packages before the init
;; file the standard way of doing things? Also, I'm so used to a 1-1
;; relationship between a vim "package" and the repository that holds
;; the code for that package (I suppose in part because I didn't
;; really use a package manager for vim and was downloading the
;; repositories). In emacs though, which has a package manager built
;; in, this relationship need not hold. For example this single
;; repository https://github.com/dominikh/go-mode.el has multiple
;; packages associated with it:
;; https://github.com/melpa/melpa/blob/master/recipes/go-mode,
;; https://github.com/melpa/melpa/blob/master/recipes/go-guru. I think
;; I like that, it gives the author more freedom on how to structure
;; their code (for like the above author they can have all the code in
;; one repository but still divy it up into multiple packages allowing
;; users to only install the things they want to).
(package-initialize)

;; package-archives is a list of package archives to search through
;; when running commands like list-packages. The default package
;; archive is more strict to modify so I think more packages end up
;; getting added elsewhere such as http://melpa.org/packages/.
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/"))

;; Will install all the packages declared by this init file. This is
;; only useful for new computers which do not have my selected
;; packages. The package-refresh-contents will slow things down on
;; start up but I don't startup emacs that often so its fine with me.
;; TODO: Should the call to package-initialize go after this??
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
(add-hook 'go-mode-hook
	  (lambda ()
	    (setq godoc-command "godoc")
	    (setq godoc-use-completing-read t)
	    (local-set-key (kbd "M-.") #'godef-jump)
	    (setq gofmt-command "goimports")
	    (add-hook 'before-save-hook 'gofmt-before-save)))

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
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; TODO: Why is it done this way instead of with a hook? I commented this out since it seemed to mess up the interactive session if I ran it.
;; (eval-after-load "haskell-mode"
;;   '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

;; Enables creating interactive shell stuff
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
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
;; "docker-compose.yml". DISABLE THIS IF IDO MODE IS TURNED ON SINCE
;; IT SEEMS LIKE IDO MODE DOESN'T USE IT.
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

;; My first stab at emacs programming. Makes buffer switching with C-x
;; <left>/<right> a little quicker when you need to repeat it. TODO: I
;; still don't fully understand how the "most recent buffer" or
;; whatever happens. Sometimes it seems to work as expected but other
;; times it I feel like it brings me to buffers I wouldn't expect (or
;; want) like the completion buffer. I would like to get a better
;; handle on that. TODO: I'm starting to play around with having using
;; terminals via emacs and I think I'd like to avoid including them in
;; the next/previous stuff or at least allow me to cycle through them
;; which is not happening currently. I should definitely try to better
;; understand how the next/previous buffers get determined.
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

;; I tend to save files very often so auto-saving is not helping me
;; much and I did just get bit in the butt for having it. I was making
;; a game in emacs and my file contents got lost because I
;; accidentally started the game in the buffer containing the file,
;; the file was auto-saved, and I could not undo since starting the
;; game disables it. Reading this later on 2021-07-13 I'm having a bit
;; of a laugh. What a uniquely emacs'y kind of problem to run into
;; lol. Then again I have this TODO as well: TODO: Had an embarassing
;; moment pair programming where the issue was actually fixed but I
;; forgot to save the file so we thought it was not fixed. Look into
;; saving the file automatically. Perhaps when I tab away from emacs
;; or something.
(setq auto-save-default nil)

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


;; Jump to the "alternate" file.
(global-set-key (kbd "C-x C-a") 'ff-get-other-file)

(defun view-help-buffer ()
  "Quickly view the help buffer. This code was copied from
`view-echo-area-messages' and modified to work with the help
buffer."
  (interactive)
  (with-current-buffer (help-buffer)
    (goto-char (point-max))
    (display-buffer (current-buffer))))

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

;; TODO: Is there an org mode command to undo the expansion that
;; happens after you hit tab? Sometimes I just want to see a quick
;; view of the headings and then undo that expansion.

;; TODO: Make artist-mode's C-n the same as picture-mode's C-n in the
;; sense that it appends to the buffer. Why would it not do this by
;; default?

;; TODO: I want to be able to paste in picture mode and not have the
;; text shift.

;; TODO: In artist-mode I want to be able to move rectangles around
;; and also have text wrap withing that rectangle. Is something like
;; that possible?

;; TODO: I miss vim's autocomplete features like file completion and
;; basic keyword matching. They felt really simple and I want to see
;; if emacs has something similar. It seems that company mode might be
;; the way to go. People say it's easier to setup. Also there is the
;; function comint-dynamic-complete-filename
;; (https://superuser.com/questions/67170/how-do-i-complete-file-paths-in-emacs)
;; which might be just what I need. Just need to think of a good
;; keybinding to use. There is also M-/ similar to vim's C-p and C-n
;; commands. Maybe that's all I'll need. Read this for inspiration:
;; https://www.emacswiki.org/emacs/Completion#completion
(global-set-key (kbd "M-\\") 'comint-dynamic-complete-filename)

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
;; might be useful.

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
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
;; https://www.emacswiki.org/emacs/ParEdit
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)

;; TODO: In paredit mode, how to delete the current s expression you
;; are in all the way up to the top level?

;; TODO: In paredit mode, if you highlight entire s-expressions and
;; press delete it does not actually work. Even for comments nothing
;; happens! I think that would be nice if paredit was smart and
;; detected that the deletion would maintain overall structure and let
;; you do it if so.

;; TODO: Be able to switch a variable name between different casings
;; seems like it would be fun: camel, kebab, snake

(global-so-long-mode 1)
(put 'downcase-region 'disabled nil)

;; TODO: Could we use magit to review PRs? I'm a lot faster when
;; inside my editor instead of looking in the browser. Watch this
;; video to get a general tour of the funcationality:
;; https://www.youtube.com/watch?v=_zfvQkJsYwI&t=44s

;; TODO: Get some documentation autocompletion going for terraform
;; (and whatever else I can think of too for that matter). I feel like
;; I've moved beyond the "I should just try to keep it all in my head"
;; phase. Computers are here to make things easier and I should lean
;; on them more. I also feel like constantly looking up things in the
;; browser is inefficient.

;; TODO: I really like the emacs help manual thingy (C-h i). I want to
;; learn to use it better AND figure out if I can add manuals for
;; anything??

(defun lag13-term-toggle-line-char-mode ()
  "Toggles between line and char mode in a terminal emulator so I
only have to remember one command instead of two:
https://www.masteringemacs.org/article/running-shells-in-emacs-overview"
  (interactive)
  (if (term-in-char-mode)
      (term-line-mode)
    (term-char-mode)))

(setq
 term-mode-hook
 (lambda ()
   (define-key term-mode-map (kbd "C-.") 'lag13-term-toggle-line-char-mode)
   (define-key term-raw-map (kbd "C-.") 'lag13-term-toggle-line-char-mode)
   ;; TODO: I need to figure out how to set this prompt appropriately.
   ;; Right now it seems to match on blank lines too. No idea why.
   (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
   ;; So I can press C-c once in a terminal as god intended. The way
   ;; this is working is that, as of emacs version 27.2, term.el
   ;; explicitly calls (term-set-escape-char (or term-escape-char
   ;; ?\C-c)) when that file gets evaluated so, since this hook
   ;; executes before term.el gets evaluated, the escape char will NOT
   ;; be bound to C-c and so C-c will be sent directly to the
   ;; terminal.
   (term-set-escape-char ?\C-x)
   ;; Displaying line numbers appears to mess up the terminal
   ;; emulation at least as of emacs version 27.2. Bash actually seems
   ;; to work mostly fine (there are a couple oddities though) but zsh
   ;; in particular is completely unusable and I'll see errors like
   ;; 'error in process filter: Args out of range: " ", 0, -43'
   (display-line-numbers-mode 0)))

(defun lag13/replace-last-sexp ()
  "Sometimes if I'm writing docs or whatever I just want to
insert the value of an emacs expression and then get rid of the
expression."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(global-set-key (kbd "C-x M-e") 'lag13/replace-last-sexp)

;; TODO: Is there the equivalent of the "gf" command in emacs? I'm
;; sure there must be. I feel like it would be nice to have that.

;; TODO: For mode hooks the emacs info documentation reccomends that
;; you modify hook variables by using add-hook but I feel like that's
;; bad because then things are no longer idempotent? Or is there a
;; cool way in emacs to first wipe out any customizations and THEN
;; reload the init file?

;; TODO: No biggie at all. I notice that when I play some of my
;; terminal games whilst using the emacs terminal emulator, the cursor
;; will show up even if on a normal terminal emulator this doesn't
;; happen. I'm curious if anything can be done to remedy it.

(defun lag13/get-buffer-major-mode (b)
  (with-current-buffer b major-mode))

(defun lag13/terminal-bufferp (b)
  (eq 'term-mode (lag13/get-buffer-major-mode b)))

(defun lag13/get-most-recent-terminal-buffer (buffers)
  (car (seq-filter 'lag13/terminal-bufferp buffers)))

(defun lag13/switch-to-most-recent-terminal-buffer ()
  (interactive)
  (let ((term-buffer (lag13/get-most-recent-terminal-buffer (buffer-list))))
    (if term-buffer
	(switch-to-buffer term-buffer)
      (ansi-term explicit-shell-file-name))))

;; TODO: Pick another key, this one seems to get used by some things
;; like python and clojure. In general it kind of feels like bindings
;; starting with C-c tend to end up in language specific modes.
(global-set-key (kbd "C-c C-t") 'lag13/switch-to-most-recent-terminal-buffer)

;; TODO: I learned about this other terminal emulator which claims to
;; have better performance than the built in emacs ones:
;; https://www.reddit.com/r/emacs/comments/jxt39s/why_does_emacs_dont_have_a_good_terminal_mode/
;; https://github.com/akermu/emacs-libvterm. Might want to try it out
;; sometime if I'm really hurting for better terminal performance.

;; TODO: When running "saml2aws login" and it prompted me to choose
;; the role I don't think it let me use the arrow keys (but I was able
;; to use C-n/C-p) but when I did that it would re-print the choices
;; which seems wrong. Can this be fixed? Should I try that "vterm"
;; terminal emulator? I've also been having some trouble I guess with
;; terraform because when I do a "terraform apply" sometimes I'll
;; scroll up via keyboard commands but that might inadvertantly add
;; some stuff to the buffer and then saying yes fails or something
;; like that. Not sure... either way I just feel like I'm having
;; trouble using the terminal emulator. Nothing major I suppose but
;; just little things. Yeah and I notice that when I'm looking at a
;; man page or output in "less" I can't properly delete things. Like
;; if I do a search with / and then try to delete the characters just
;; get displayed. I guess you can get around with going to line mode
;; but that seems kind of janky tbh. Also, after pasting something and
;; switching back to char mode, it seems that it just hints enter on
;; the command which I do not want.
;;
;; ? Please choose the role  [Use arrows to move, type to filter]
;;   Account: rate-iam (592986690814) / Okta-DeliveryLeads
;;   Account: rate-iam (592986690814) / Okta-Developers
;; > Account: rate-iam (592986690814) / Okta-GR-DataPlatformUsers
;;   Account: rate-iam (592986690814) / Okta-GlobalAdmins
;;   Account: rate-iam (592986690814) / Okta-NetworkAdmins
;;   Account: rate-iam (592986690814) / Okta-SecurityAdmins
;;   Account: rate-iam (592986690814) / Okta-SystemsTeam
;; ? Please choose the role  [Use arrows to move, type to filter]
;;   Account: rate-iam (592986690814) / Okta-DeliveryLeads
;;   Account: rate-iam (592986690814) / Okta-Developers
;;   Account: rate-iam (592986690814) / Okta-GR-DataPlatformUsers
;; > Account: rate-iam (592986690814) / Okta-GlobalAdmins
;;   Account: rate-iam (592986690814) / Okta-NetworkAdmins
;;   Account: rate-iam (592986690814) / Okta-SecurityAdmins
;;   Account: rate-iam (592986690814) / Okta-SystemsTeam

;; TODO: I noticed that in the ansi-term emulator, the history seems
;; to be different if you're in line mode vs char mode (i.e. if I do
;; M-p in line mode different things show up when compared to doing
;; C-p in char mode). Why is this? What is going on under the hood?

;; Right now (2021-07-13) I want to experiment with having a buffer
;; list that gets opened with the point focused on it and when I
;; select a file to go to, the buffer list is NOT set itself as the
;; last buffer I went to (i.e. I can do C-x b <enter> after selecting
;; a buffer and know I'm going to the buffer before I opened the
;; buffer list). bs-show seems to provide that functionality so I'm
;; using it. My current use case for having a buffer list is when I'm
;; in a situation where I remember I want to go to a file in a certain
;; directory but I can't for the life of me remember the file. OR I
;; want to go to any old file in a particular directory/project so I
;; can do something there like run magit-status. TODO: Maybe more
;; project awareness would also be a way to solve this so perhaps I
;; should look into something like projectile.
(global-set-key (kbd "C-x C-b") 'bs-show)

;; TODO: I feel like when clojure is activated C-c C-z (which switches
;; from the buffer to the repl) is not always accurate. Like I'll be
;; in a repl in one project and it C-c C-z will take me to a file in a
;; different project kind of thing. Keep an eye on this.

;; TODO: I wish I understood the emacs mark better so I could better
;; jump around files. Might be interesting to read up on that.

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

;; TODO: Have C-y in term-mode actually paste text. Also, I notice
;; some weirdness when I switch to line mode to paste text in that I
;; feel like if I switch back to char mode, the pasted text is not
;; able to be interacted with which I think makes sense because that
;; pasted text was never sent to the subprocess. Hmmmm.

;; TODO: I would like to be able to scroll up the window in emacs just
;; a little. How can I do this? Vim had the C-e and C-y commands.

;; TODO: Sometimes I'll do C-x b and then start typing right away but
;; then realize that the alternate buffer is the one I want to go to.
;; Would be neat if I could just hit something like Shift-Enter and it
;; would forget about what I typed and just bring me there.

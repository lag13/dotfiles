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
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(bs-attributes-list '(("File" 12 12 left bs--get-file-name)))
 '(bs-string-current "/")
 '(completion-category-overrides '((file (styles basic partial-completion emacs22))))
 '(custom-enabled-themes nil)
 '(explicit-shell-file-name "bash")
 '(fido-mode t)
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "vendor"))
 '(groovy-indent-offset 2)
 '(help-window-select t)
 '(icomplete-compute-delay 0.0)
 '(icomplete-mode nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((meta)) ((control) . text-scale)))
 '(package-selected-packages
   '(vterm-toggle vterm magit paredit plantuml-mode groovy-mode nginx-mode jinja2-mode systemd terraform-mode cider typescript-mode edit-indirect clojure-mode haskell-mode php-mode dockerfile-mode elm-mode restclient yaml-mode markdown-mode go-guru editorconfig go-mode))
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
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; https://www.emacswiki.org/emacs/MidnightMode
(require 'midnight)
(midnight-delay-set 'midnight-delay "9:00am")

(toggle-frame-maximized)

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
	(or (lag13/is-special-buffer buf)
	    (lag13/is-magit-buffer buf))))

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

;; TODO: I miss vim's autocomplete features like file completion and
;; basic keyword matching. They felt really simple but very useful and
;; I want to see if emacs has something similar. It seems that company
;; mode might be the way to go. People say it's easier to setup. Also
;; there is the function comint-dynamic-complete-filename
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

;; TODO: In paredit mode, if you highlight entire s-expressions and
;; press delete it does not actually work. Even for comments nothing
;; happens! I think that would be nice if paredit was smart and
;; detected that the deletion would maintain overall structure and let
;; you do it if so.

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

;; TODO: Is there the equivalent of the "gf" command in emacs? I'm
;; sure there must be. I feel like it would be nice to have that.

;; The vterm-mode-hook gets modified by the vterm-toggle package to
;; call a function which modifies a list of vterm buffers internal to
;; vterm but it only gets modified once the package gets used (i.e. if
;; I tried running "M-x vterm" without ever using vterm-toggle then
;; vterm-toggle would not see the vterm buffer for the purpose of the
;; vterm-toggle-forward/backwards commands. Pretty minor edge case but
;; just thought I'd get it out of the way. TODO: Would it make sense
;; to add the hook adding bit to the autoloads portion of the code so
;; this require is not necessary?
(require 'vterm-toggle)

(add-hook
 'vterm-mode-hook
 (lambda ()
   (define-key vterm-mode-map [(control return)] #'vterm-toggle-insert-cd)
   (define-key vterm-mode-map (kbd "s-n") 'vterm-toggle-forward)
   (define-key vterm-mode-map (kbd "s-p") 'vterm-toggle-backward)
   (display-line-numbers-mode 0)))

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

(global-set-key (kbd "C-x C-p") 'vterm-toggle)
(global-set-key (kbd "C-x p") 'vterm-toggle-cd)

;; TODO: I feel like when clojure is activated C-c C-z (which switches
;; from the buffer to the repl) is not always accurate. Like I'll be
;; in a repl in one project and it C-c C-z will take me to a file in a
;; different project kind of thing. Keep an eye on this.

;; TODO: I wish I understood the emacs mark better so I could better
;; jump around files. Might be interesting to read up on that. This
;; seems to have some interesting tidbits too:
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
;; cursors could maybe solve this).

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

;; TODO: I would like to be able to scroll up the window in emacs just
;; a little. How can I do this? Vim had the C-e and C-y commands.

;; TODO: Get some smooth scrolling going in emacs
;; https://www.emacswiki.org/emacs/SmoothScrolling so when I
;; screenshare with people it's easier to see stuff when I'm moving
;; around.

;; TODO: At some point I was watching a talk about CIDER and the
;; presenter mentioned I believe this package avy:
;; https://emacsredux.com/blog/2015/07/19/ace-jump-mode-is-dead-long-live-avy/
;; I should check it out for the purpose of navigating more
;; efficiently in emacs.

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
;; places?

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

;; TODO: See how I feel about next-buffer and previous-buffer behavior
;; now that I know a bit more how they work under the hood (i.e. the
;; previous buffer lists are local to windows). Part of me feels like
;; I would prefer that those functions ONLY use the globally scoped
;; list of buffers kind of like how the function "other-buffer"
;; behaves.

;; TODO: This sounds intriguing to me. Think about if it would be
;; useful to install: https://github.com/jacktasia/dumb-jump

;; TODO: I wonder if it would be useful to display recently used
;; buffers on a "tabline" like with tabs in a browser. Might be an
;; extra bit of information which helps you remember what you were
;; recently editing. Seems like "global-tab-line-mode" might be what I
;; want if I want this.

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

;; TODO: Get a better color theme for emacs perhaps. I was pairing
;; with someone and they said some it was tough to see some of the
;; highlighting.

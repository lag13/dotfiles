;; My Emacs configuration file. The path to this file is determined by
;; user-init-file which gets dynamically set when emacs starts. I
;; believe that when emacs starts up it does something like "find the
;; first non empty file in the list (~/.emacs ~/.emacs.d/init.el).

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clean-buffer-list-delay-general 7)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "vendor")))
 '(groovy-indent-offset 2)
 '(package-selected-packages
   (quote
    (groovy-mode nginx-mode jinja2-mode systemd terraform-mode cider typescript-mode edit-indirect clojure-mode haskell-mode php-mode dockerfile-mode elm-mode restclient yaml-mode markdown-mode go-guru editorconfig go-mode)))
 '(send-mail-function (quote smtpmail-send-it))
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

;; exec-path is like "PATH" but for emacs. When emacs tries to run a
;; binary, it will search through exec-path to find it.
(setq exec-path (append (list "/usr/local/bin")
			exec-path))

;; Need to set GOPATH so tools like goimports will work:
;; https://www.emacswiki.org/emacs/ExecuteExternalCommand,
;; http://ergoemacs.org/emacs/modernization_elisp_lib_problem.html,
;; https://stackoverflow.com/questions/14074912/how-do-i-delete-the-newline-from-a-process-output,
;; [[info:elisp#Regexp%20Backslash][info:elisp#Regexp Backslash]].
;; TODO: I want this to work but for some reason when this code gets
;; run as part of startup, it cannot find the go command but after I'm
;; inside the editor running the command works as expected so for now
;; I'll just hardcode the GOPATH.

;; (setenv "GOPATH" (replace-regexp-in-string "\n*\\'" "" (shell-command-to-string "go env GOPATH")))

(setenv "GOPATH" (concat (getenv "HOME") "/go"))

(setq exec-path
      (append (list (concat (getenv "GOPATH") "/bin"))
	      exec-path))

;; Have PATH be the same as exec-path. We do this because emacs will
;; invoke a shell to run a program and the shell needs PATH set
;; appropriately.
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
 '((shell . t)))

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

;; Start emacs fullscreen.
(toggle-frame-fullscreen)

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
;; game disables it.
(setq auto-save-default nil)

;; So when buffer names are created, they show up as (concat bufname
;; uniquify-separator dir) instead of (concat bufname "<" dir ">")
;; which is one less character to type.
(setq uniquify-buffer-name-style 'post-forward)
;; (setq uniquify-separator ";")

;; I've never had any use for specific settings for particular files
;; or directories so I figured I'd disable those features.
;; (setq enable-local-variables t)
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

(global-set-key (kbd "C-c d") 'insert-date)

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
;; commands. Maybe that's all I'll need.

(global-set-key (kbd "M-\\") 'comint-dynamic-complete-filename)

;; TODO: Speed up replacing a string with another string. Like could I
;; highlight the two strings I need instead of typing them?

;; TODO: I should make sure that artist mode only uses spaces (no
;; tabs) so that I can copy the document elsewhere and the formatting
;; will not get messed up:
;; https://stackoverflow.com/questions/43976371/artist-mode-drawing-characters-disorder

;; TODO: Command to cycle through files in a directory, handy when
;; there are only a couple files in the directory and you want to move
;; around between them. At this point I wonder if I should just get a
;; more full featured file opener thingy.

;; TODO: Can I configure C-c C-c in org mode to not try to put the
;; output of a program into a table? I feel like the raw output would
;; be more useful.

;; TODO: Learn how to increase the emacs font size for presentation
;; purposes.

;; TODO: Learn from this guy: https://github.com/cgore He seems super
;; sharp and seems like a super lisp geek.

;; TODO: Prevent emacs quitting when Cmd-Q is hit, I just did that
;; accidentally and its a little annoying.

;; TODO: Configure terraform so it runs terraform fmt on save.

;; I tend to be okay not using line numbers when coding (I did for a
;; while in fact) but they are useful if any sort of pair programming
;; is being done because then the other person can be like "I think we
;; should edit line XYZ" https://www.emacswiki.org/emacs/LineNumbers.
;; TODO: Figure out how to install the latest emacs because linum-mode
;; really is slow (on my work.org buffer the cursor has trouble
;; moving)
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode 1))

;; TODO: Sometimes I want to "find" a specific file like how the find
;; command works. How could I do that with emacs? Should I just bite
;; the bullet and install some sort of fuzzy file finder?

;; TODO: Had an embarassing moment pair programming where the issue
;; was actually fixed but I forgot to save the file so we thought it
;; was not fixed. Look into saving the file automatically. Perhaps
;; when I tab away from emacs or something.


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
;; EVER needed to use pi before but I just wanted to write this down.
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

;; My Emacs configuration file -*- lexical-binding: t; -*-

;; Profile emacs startup: https://config.daviwil.com/emacs
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Load work specific customizations after emacs loads the init file
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((custom-work-elisp-file "~/.emacs.d/work.el"))
              (when (file-exists-p custom-work-elisp-file)
                (load-file custom-work-elisp-file)))))

(setq use-package-always-ensure t)

;; On 2022-06-02 I was having EXTREME slowness when editing files with
;; emacs and it seemed to only be when editing files that were managed
;; by version control. This was while using my job's windows machine.
;; Setting this seemed to alleviate the issue and it would make sense
;; that it would because, from what I understand, windows is TERRIBLY
;; slow at launching sub processes and I think emacs does a ton of
;; that with this VC feature and I don't use this anyway so begone!
;; This also fixed an issue I was having where emacs would be
;; extremely slow to startup (once I clocked it in at 8 minutes!!) and
;; I suspect that too was because it was opening all the files it had
;; open previously (via the desktop feature) and almost all of those
;; files were managed by version control.
(setq vc-handled-backends nil)

;; The path to this file is determined by user-init-file which gets
;; dynamically set when emacs starts. I believe that when emacs starts
;; up it does something like "find the first non empty file in the
;; list (~/.emacs ~/.emacs.d/init.el).

;; I like being able to seperate changes I manually make vs ones
;; automatically made by the system. Some of the custom changes ARE
;; started by me of course but others kind of seem to just happen.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; I think I wasn't loading use-package before? God I really need to
;; cleanup my emacs file. I feel like it's getting sooooo crufty.
(eval-when-compile
  (require 'use-package))

;; straight.el seems to be all the rage so I thought I'd give it a
;; shot. The real motivation for installing this though is that I
;; wanted to install https://github.com/noctuid/targets.el but it is
;; not a melpa package (see
;; https://github.com/noctuid/targets.el/issues/29). TODO: I feel like
;; it's time to start moving away from the built in emacs package
;; management... I'm writing this on 2022-04-09 as I upgrade to emacs
;; 28 while at the same time bringing up my dotfiles from scratch and
;; I noticed that some packages were not added to the
;; package-selected-packages variable AND when I install them now they
;; STILL don't get added... What the hell man....
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; NEAT COMMANDS:
;; C-h K/F - like their lowercase variants but opens any info docs.

;; C-h l - views the last 300 input keystrokes in case something weird
;; happens and you want to figure out what it whas.

;; Pressing M-n with the cursor on a link/file while doing a C-x C-f
;; will autofill that link/file.

;; For some reason setting visual-bell to t didn't have any effect but
;; luckly I can configure something else. Thank you
;; https://www.emacswiki.org/emacs/AlarmBell for inspiration.
(defun lag13-ding-flash ()
  (dolist (face '(mode-line mode-line-inactive))
      ;; (face '(centaur-tabs-unselected centaur-tabs-selected mode-line mode-line-inactive))
    ;; TODO: Instead of invert-face, I wonder what it would look on if
    ;; we made the background color literally the "opposite" color of
    ;; whatever is being shown.
    (invert-face face)
    (run-with-timer 0.1 nil #'invert-face face)))
(setq visible-bell nil
      ring-bell-function 'lag13-ding-flash)

;; Enable ALL the disabled things!
;; https://www.emacswiki.org/emacs/DisabledCommands
(setq disabled-command-function nil)

;; TODO: See what commands are disabled.
(defun lag13-enable-all-commands ()
  "Enable all commands, reporting on which were disabled."
  (interactive)
  (with-output-to-temp-buffer "*Commands that were disabled*"
    (mapatoms
     (function
      (lambda (symbol)
	(when (get symbol 'disabled)
	  (put symbol 'disabled nil)
	  (prin1 symbol)
	  (princ "\n")))))))

;; https://www.emacswiki.org/emacs/MidnightMode TODO: Interesting
;; thing I noticed. This midnight mode cleared all the buffers
;; underneath a projectile project but projectile still had knowledge
;; of the project. I wonder if there's a way to configure projectile
;; so it deletes projects when they have no buffers underneath them.
(require 'midnight)
(midnight-delay-set 'midnight-delay "9:00am")

(toggle-frame-fullscreen)

;; I don't know why but on windows I always get this error:
;; https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure
;; Windows is starting to frickin' annoy me in terms of emacs...
;; Finding the info manuals still doesn't work for example... Why the
;; fuck not.... if I download the windows emacs binary and invoke it
;; from powershell THEN it can find them but if I invoke it from mingw
;; (side note, I'm still confused about the difference between msys2
;; and mingw) then I get nothing. So frustrating... because I want to
;; use mingw because starting emacs through it seems to enable native
;; compliation which is new as of emacs 28 and I want to try. None of
;; these issues are huge for sure but they're just bothersome... Maybe
;; I should get on windows 11 so I can start trying out their feature
;; of running GUI apps in WSL so I can just have emacs installed in a
;; linux environment and hopefully not face these issues.
(when (seq-contains-p '(ms-dos windows-nt cygwin) system-type)
  (setq package-check-signature nil))

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

(load "~/.emacs.d/lag13")

;; More on emacs' built in completion stuff within the minibuffer:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
;; https://github.com/oantolin/orderless is a user defined completion
;; style which is pretty neat! Good for when I know a couple words in
;; what I'm searching for but don't know their order.
(setq completion-styles '(basic partial-completion flex orderless))
;; TODO: Trying out just nuking this so we have the same completion
;; for everything and then I can add to it where I feel it's helpful.
;; I think my ideal completion engine is one that does flex but the
;; earlier results are always the ones that have less distance between
;; the consecutive letters followed by orderless I suppose. Also maybe
;; prioritizing files that START with what is being typed (i.e. if you
;; had secret.txt and cool-secret.txt, typing "secret" would always
;; put "secret.txt" first because it matches at the beginning (or
;; maybe at word boundaries generally). I also notice that the builtin
;; flex seems to function better (for me at least) than orderless's
;; flex at least to me. For example, I wrote a completing-read wrapper
;; to show me the absolute file names of buffers (in the situation
;; where I know I've opened some stuff from a particular directory but
;; can't remember the file name but remember the directory so I can
;; type the directory and go from there) and some files were in a
;; directory "misc" so I typed that and the builtin flex matched those
;; first, perfect, the orderless though matched some MUCH longer file
;; paths where the "misc" were separated by >=4 chars each. No bueno.
(setq completion-category-defaults nil)
;; TODO: Improve the face contrast for things highlighted with the
;; default completion mechanism. It seems a lot weaker than what
;; orderless has for highlighting. Or just figure out how to get
;; exactly what I want from orderless which seems to do pretty well
;; highlighting'wise.
(setq completion-styles '(orderless))

;; TODO: Even though I set these, it seems that if I have a situation
;; like FIRST.SecondThird and I try to complete with FIR.*thir then it
;; does not match. Why is that? Note that I'm using just orderless as
;; my only completion framework.
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

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

(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
;; https://github.com/dominikh/go-mode.el#gopls-integration
;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md#loading-lsp-mode-in-emacs
;; https://geeksocket.in/posts/emacs-lsp-go/
;; https://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; TODO: Do haskell stuff one thing that looked cool was configuring
;; "stylish-haskell" to run on save. Maybe this configuring this
;; haskell-process-args-ghci would be useful? I think ghci runs by
;; default for interactive haskell in emacs.

;; Purpose is so that if I evaluate some plant-uml in a code block,
;; the resulting image will be displayed
(add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)

;; (eval-after-load "haskell-mode"
;;   '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

;; ;; Enables creating interactive shell stuff
;; (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
;; (eval-after-load "haskell-mode"
;;   '(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring))
;; (setq haskell-process-type 'stack-ghci)

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

;; Hitting return will open the link under the point.
(setq org-return-follows-link t)

;; Although, from a computing standpoint, two spaces separating
;; sentences probably makes more sense, because then programs like
;; emacs can accurately and easily determine sentence boundaries, one
;; space seems to be the norm.
(setq sentence-end-double-space nil)

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
(auto-save-visited-mode 0)
;; Replacing the auto save stuff with this package. Originally this
;; happened because I was experiencing EXTREME slowness when using
;; emacs on my windows work laptop and I attributed it to saving.
;; Technically I think the issue was the VC related features that
;; emacs has that would trigger upon save but on the path to figure
;; that out I tried reducing the number of saves that happen. I wanted
;; to do something like this anyway because for a language like go for
;; which I run goimports on save, I don't want to be saving just
;; because I'm idle, I want to save at a "proper time" and this
;; package gives that.
(use-package super-save
  :init
  (setq super-save-hook-triggers '(focus-out-hook))
  :config
  (super-save-mode +1))

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

;; Sometimes I record timestamps when I do things.
(global-set-key (kbd "C-c t") 'insert-timestamp)

(defun lag13-insert-date ()
  "Inserts the current date. As opposed to `insert-timestamp' I
  insert the local date since I'm usually just recording when I
  work on things"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; TODO: At some point I wanted to insert a date X days ago. It would
;; be cool if I could have emacs do it but at the time I just ended up
;; googling it and it made me thing. Goggle has so much functionality
;; to answer little questions like this and oftentimes it seems that
;; they just display the result at the top of the page. Could we make
;; a "let-google-figure-it-out" sort of package to do some of those
;; things?
(global-set-key (kbd "C-c d") 'lag13-insert-date)

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

;; TODO: Make a PR to add escape and unescape function similar to
;; tpope's [y and ]y commands in
;; https://github.com/tpope/vim-unimpaired

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

;; TODO: Configure terraform so it runs terraform fmt on save.

;; I tend to be okay not using line numbers when coding (I did for a
;; while in fact) but they are useful if any sort of pair programming
;; is being done because then the other person can be like "I think we
;; should edit line XYZ" https://www.emacswiki.org/emacs/LineNumbers.
(global-display-line-numbers-mode)

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
;; And of course pau: https://xkcd.com/1292/
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

;; TODO: See if there's anything good in these repos:
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
;; anything/everything?? Like, are their manuals for other unix
;; utilities as well?

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
;; magit-status. TODO: It could be interesting to also come
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

;; TODO: I would love it if M-. would jump to the definition of an
;; elisp function even if said function was written in C. How can we
;; make that happen? Do I have to compile emacs from source?

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
;; what note management things help with. Yeah, I'm revisiting this on
;; 2022-02-23 and I feel much more strongly that it would be something
;; helpful for me. It just feels like one of those things that make
;; sense to do. Like, I feel like this situation happened so often:
;; when I was in APUSH and taking notes in that hierarchical manner I
;; was taught. I'd read about some notable fact that I wanted to write
;; down but there were multiple sections I wanted to put it under! I'd
;; have to just pick one and move on. With networked note taking
;; though, who cares! Just link the fact to both relevant things and
;; move on. Other documentation:
;; https://www.youtube.com/watch?v=-TpWahIzueg&ab_channel=SystemCrafters,
;; https://www.orgroam.com/manual.html

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

;; TODO: It would be nice to be able to go to all places in code where
;; some function gets called. Is that kind of functionality built in?
;; Maybe the dumb-jump package has info about that?

;; TODO: If utilize "dumb jump" in terraform on a local variable, it
;; appears to still look for an actual variable (like in a
;; variables.tf file). I think it could be improved to look for a
;; locals block in the current directory.

;; TODO: How do I get better about spellchecking my stuff? Like
;; spellchecking my comments in particular.

;; Making the text a little bigger. This might mean that I'm getting
;; old (cur date is 2021-08-01).
(set-face-attribute 'default nil :height 120)
;; Trying out the font used by visual studio code because it felt more
;; readable than the default emacs font.
(set-face-attribute 'default nil :family "Consolas")

;; TODO: It would be neat to have a "search text only" option when
;; looking at an html document in emacs. Just sounds like a cool idea.
;; Would also be nice then to extend that with a "regex search text
;; only". I feel like there could be some other ways to limit searches
;; to certain parts of a buffer which could be good like only search
;; in comments or only search in strings or only search in org mode
;; headers.

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

;; TODO: I feel like this behavior of the DEL key should be default in
;; paredit. Should I suggest this to the maintainer?
(defun lag13/paredit-delete-region-or-backward ()
  (interactive)
  (if (region-active-p)
      (paredit-delete-region (point) (mark))
    (paredit-backward-delete)))

;; I believe that a good candidate for using eval-after-load is making
;; keybindings within a mode. Because the keybindings of a mode are
;; defined only initially when the mode's code is loaded (i.e. there
;; is only one copy of the keymap). You could put the keybinding in a
;; mode hook too but it would just be overkill because you'd be
;; redefining the same key every time you start paredit mode in a new
;; buffer.
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
;; the parent (also deleting the parent). Pretty brutal honestly
;; (killing your parent that is). But what if every time I hit it it
;; would play the beginning of "you raise me up" by Josh Groban. That
;; would be pretty funny. Maybe this could help:
;; https://github.com/emacs-tw/awesome-emacs#music. Or maybe this
;; could be useful? https://github.com/mrBliss/helm-rhythmbox

(blink-cursor-mode 0)
(global-hl-line-mode t)

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

(yas-global-mode 1)

;; TODO: For that golang table driven test yasnippet, make it so it is
;; more dynamic (i.e. upon expansion, it will ask how many function
;; arguments will be there and work appropriately). Perhaps this could
;; help: https://github.com/joaotavora/yasnippet/issues/348

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
;; then I can do... something with it! Along this line of reasoning, I
;; was learning about the slot architecture and was writing out the
;; definitions for a bunch of terms and I thought it would be really
;; neat if when you defined something and it used a previously used
;; term that that term would be highlighted or something. Again I feel
;; like it would just be useful to get rid of the fluff of
;; descriptions and see how these different components relate to one
;; another. I feel like it would be a helpful and sort of abstract way
;; to gain an understanding of game rules too.

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
;; item. If I hit return again it could exit the list. This would
;; behave more consistantly with other editing things I feel.

;; TODO: I think it would be cool if, from a documentation kind of
;; standpoint, you were able to ask if a function has an "inverse"
;; (like an isomorphic kind of thing) and it could tell you. Like the
;; opposite of a split string algorithm would be one that joins
;; strings (<-- that was what motivated me to write this actually
;; because the naming in emacs is weird).

;; TODO: How do I do timer stuff in emacs? Use case was that at one of
;; my companies we wanted to see how long a feature flag flip would
;; actually take to take effect. I know I could just google a timer
;; but I'm curious, I want to do everything in emacs!! lol. One thing
;; I might like this for honestly though is to have emacs tell me when
;; I've been online for ~8 hours or something like that so I don't
;; over work.

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)

;; On a windows machine I kept accidentally executing this keybinding
;; because my palm would accidentally click the mouse. It would open a
;; buffer menu which I have no use for.
(global-unset-key [C-down-mouse-1])

;; TODO: Another thought (which I may have already had lol) in the
;; interest of code splunking. If I see something which I think is
;; useful, I think it would be nice to "mark" the buffer I'm in so I
;; know to return to it. Maybe if the buffers were displayed in a
;; tabline across the top then that tab could be highlighted.

;; TODO: Mess around with trying to get a feature where I can
;; temporarily make a specific window the largest and only window but
;; then I can revert later:
;; https://stackoverflow.com/questions/7641755/maximizing-restoring-a-window-in-emacs

;; TODO: I think that for code splunking it could be helpful to be
;; able to have point on a place in a git managed file and have it
;; generate the "permalink" for that spot in the file. Because
;; sometimes I want to show some code to my coworker and the fastest
;; way to me feels like to do something like this.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EVIL MODE: Something wicked this way comes
;;
;; I've given vanilla emacs a fair shot (I've used it for at least 2
;; years as of 2022-01-04) and I want my vim editing back! It's time
;; to return to the dark side, it's time to embrace evil-mode. Thanks
;; to https://github.com/hlissner/doom-emacs for some great
;; documentation and inspirations on what evil packages to add.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'evil)
(evil-mode 1)

;; Emacs' undo system is... fine and I have gotten used to it (here's
;; a great writeup on it
;; https://www.reddit.com/r/emacs/comments/6yzwic/how_emacs_undo_works/)
;; but it does get unwieldy to use once the undo chain gets really
;; long so I want a more vim like editing experience and that means
;; giving undo-tree a shot! I really liked the philosophy of undo-fu
;; too (especially considering the saga which followed of me debugging
;; an oddity I observed with undo-tree) so I might go back to it but
;; representing undo's as a tree just makes sense so let's try that first.
(require 'undo-tree)
(global-undo-tree-mode)
(add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)
;; Bind C-/ to the undo command so I can do regular emacs undo's while
;; in insert mode and, after leaving insert mode, the whole thing will
;; be treated as one undoable unit. Side note, Why does undo-tree do
;; this remap binding anyway? They already rebind C-/ after all. Man,
;; what a saga it was to figure this out. The reason I'm doing this is
;; because I want to be able to undo while in insert mode but after
;; exiting insert mode I want everything entered to be undoable as one
;; unit and when using undo-tree this does not happen by default. The
;; reason for this is that if you execute the undo-tree-undo command
;; while in insert mode, it will copy all the undo information
;; accumulated in buffer-undo-list to undo-tree's local variable
;; buffer-undo-tree. Then, after exiting insert mode, even though evil
;; restores the value of buffer-undo-list, it does nothing about
;; buffer-undo-tree and thus the insert mode changes are no longer
;; undoable in one chunk. TODO: PR material. I feel like evil should
;; also take care to restore the value of buffer-undo-tree to before
;; insert mode was entered because as it stands now using undo-tree
;; will not necessarily respect the evil-want-fine-undo setting which
;; feels wrong.
(define-key undo-tree-map [remap undo] nil)
(define-key undo-tree-map (kbd "C-/") nil)

;; I am only one man so I have not read EVERYTHING that this does but
;; it seems to be a fantastic package which makes sure that evil mode
;; is consistently used in "all" emacs modes. Go community!
(evil-collection-init)

;; TODO: PR material. In evil-collection they bind gd to godef-jump
;; and I don't believe they should do that by default seeing as how if
;; you use some sort of lsp related package (lsp-mode or eglot) then
;; gd should "just work" because I think it integrates with xref or
;; whatever:
;; https://github.com/emacs-evil/evil-collection/blob/8342a50830d4ba20c589396ee4c9fce6284f7981/modes/go-mode/evil-collection-go-mode.el#L40
(evil-collection-define-key 'normal 'go-mode-map
    "gd" 'nil)

(require 'evil-nerd-commenter-operator)
(evil-define-key '(normal visual) 'global "gc" #'evilnc-comment-operator)
;; TODO: PR material. I feel like the comment object for
;; https://github.com/redguardtoo/evil-nerd-commenter should default
;; to operating line-wise since that's the structure of line-wise
;; comments by definition. I tried my hand on doing that but it wasn't
;; working like I was expecting. At the end of the day I want "dic" to
;; delete ALL lines that the comment occupied. If I do "cic" I want it
;; to leave me with ONE empty line where the comment once was I'm not
;; sure why this is.
(define-key evil-inner-text-objects-map "c" #'evilnc-outer-commenter)

;; TODO: I wonder what significance the first symbol has to the
;; overall function of straight.el. I used a dummy symbol before and
;; this still worked which makes sense to me since we're specifying a
;; literal repo from where to get it. I guess the symbol must just
;; serve an organizational purpose for straight.el.
(straight-use-package
 '(targets :type git :host github :repo "noctuid/targets.el"))
(require 'targets)
;; I'm used to typing "ib" to select parentheses so want it to be able
;; to do target'y things.
(setq targets-user-text-objects '((paren "(" ")" pair :more-keys "b")))
;; Pretty neat! Not sure if I'll use it but they let you have one
;; keybinding work for multiple kinds of text objects:
;; https://github.com/noctuid/targets.el#targets-define-composite-to
(setq targets-composite-text-objects
      '((all-quotes
	  (("\"" "\"" quote)
	   ("'" "'" quote)
	   ("`" "`" quote)
	   ("‘" "’" quote)
	   ("“" "”" quote))
	  :bind t
	  :keys "q")))
(define-key evil-visual-state-map (kbd "RET") #'targets-last-text-object)
(define-key evil-operator-state-map (kbd "RET") #'targets-last-text-object)
(targets-setup t
               :inside-key nil
               :around-key nil
               :remote-key nil)

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

(evil-global-set-key 'normal (kbd "j") #'evil-next-visual-line)
(evil-global-set-key 'normal (kbd "gj") #'evil-next-line)
(evil-global-set-key 'normal (kbd "k") #'evil-previous-visual-line)
(evil-global-set-key 'normal (kbd "gk") #'evil-previous-line)
(evil-global-set-key 'visual (kbd "j") #'evil-next-visual-line)
(evil-global-set-key 'visual (kbd "gj") #'evil-next-line)
(evil-global-set-key 'visual (kbd "k") #'evil-previous-visual-line)
(evil-global-set-key 'visual (kbd "gk") #'evil-previous-line)
;; In org mode these get bound to going to the next previous heading
;; level but I don't want that. TODO: Find out where these get bound.
;; I didn't see them in evil-collection like I was expecting.
(evil-define-key 'normal org-mode-map (kbd "gj") nil)
(evil-define-key 'normal org-mode-map (kbd "gk") nil)
(evil-define-key 'insert org-mode-map (kbd "g") #'org-self-insert-command)
(evil-global-set-key 'insert (kbd "C-v") #'yank)

(evil-global-set-key 'motion (kbd "gL") #'evil-end-of-visual-line)
(evil-global-set-key 'motion (kbd "gH") #'evil-first-non-blank-of-visual-line)
;; TODO: PR material. Add functionality to the rg package so we can
;; tell it to do whole word searches.

;; Argument text objects. TODO: I notice that if an argument is a
;; string with a "," in it then it doesn't work. Could we make it
;; work?
(define-key evil-inner-text-objects-map "a" #'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" #'evil-outer-arg)
(define-key evil-normal-state-map "ga" #'evil-jump-out-args)

;; Bring back vim's C-a/C-x commands. TODO: PR material.
;; https://github.com/cofi/evil-numbers is where this comes from and I
;; feel like that package could be an appropriate place to add number
;; text objects. Potential text objects could be "in" to select a
;; number MINUS any prefix like "0x" and "an" could be to include that
;; prefix. This could be helpful in figuring out how to create those
;; text objects:
;; https://github.com/noctuid/targets.el#method-for-implementing-text-objects-using-things
(evil-define-key '(normal visual) 'global "+" #'evil-numbers/inc-at-pt)
(evil-define-key '(normal visual) 'global "-" #'evil-numbers/dec-at-pt)

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

;; TODO: PR material. I notice that if I do "dii" then it ends up
;; leaving one empty line instead of deleting everything. Similarly,
;; if I do "cii" there ends up being TWO empty lines and we are in
;; insert mode. It feels like there is an off by one error here. I
;; noticed the same thing with the comment text objects too in
;; https://github.com/redguardtoo/evil-nerd-commenter. It looks like
;; within evil mode they use this "bounds-of-thing-at-point" function
;; within the library thingatpt.el. I wonder if these should be
;; expanding that library instead, the comments make it sound like
;; you're able to define other "things".
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
;; "forward-THING" must be defined AND it has to return a number. This
;; will help:
;; https://github.com/noctuid/targets.el#method-for-implementing-text-objects-using-things
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

;; I'm modifying these keybindings to make "gs" take the place of "ys"
;; which then means I'll use "gs" in visual mode instead of "S" which
;; frees up "S" to do something else I want. As neat as I think it is
;; to combine an operator (like 'y') with a non-text object (like 's')
;; to make a completely new binding (because it opens the possibility
;; for so many other keybindings!) I don't care for it because then
;; you need a unique binding for things to work in visual mode (so
;; that is not consistant) PLUS it makes it hard to get C-h help on a
;; key (i.e. if you try to do "C-h ys" you'll never get to the "s"
;; because it'll match on the "y" first). Anyway... Also, fun fact,
;; the way in which evil-surround is setup (at least as of 2022-01-27)
;; is to add "s" to the operator pending map which defaults to the
;; same action "evil-surround-edit" which will check to see what
;; operator was called and perform the appropriate action. So if you
;; type 'd' then it will do a delete, 'c' will cause a change,
;; OTHERWISE it adds surrounding characters. Yes you read that right,
;; "otherwise". That means you can invoke other operators to do
;; surround stuff too like g~siw". Quirky
(with-eval-after-load 'evil-surround
  (evil-define-key 'normal evil-surround-mode-map "gs" #'evil-surround-region)
  (evil-define-key 'normal evil-surround-mode-map "gS" #'evil-Surround-region)
  (evil-define-key 'visual evil-surround-mode-map "gs" #'evil-surround-region)
  (evil-define-key 'visual evil-surround-mode-map "gS" #'evil-Surround-region)
  (evil-define-key 'visual evil-surround-mode-map "S" nil))
(global-evil-surround-mode 1)

;; I feel like this is a better use for these keys.
(define-key evil-motion-state-map "H" #'evil-first-non-blank)
(define-key evil-motion-state-map "L" #'evil-last-non-blank)
;; This is easier to reach than % and I don't really use marks anyway.
;; Mnemonic is "[m]atching"
(define-key evil-motion-state-map "m" #'evil-jump-item)
(define-key evil-normal-state-map "m" #'evil-jump-item)
;; Retain original set mark behavior just in case
(define-key evil-normal-state-map "M" #'evil-set-marker)
;; I don't really use the default H, M, L behavior but let's keep it
;; around just in case.
(define-key evil-motion-state-map "gh" 'evil-window-top)
(define-key evil-motion-state-map "gm" 'evil-window-middle)
(define-key evil-motion-state-map "gl" 'evil-window-bottom)

;; TODO: PR material. Also, the vim-exchange docs say that if one
;; region is completely contained within the other, then the
;; containing region will replace. I don't think this behaves in that
;; manner. Maybe I should try to get that fixed.
(require 'evil-exchange)
(evil-exchange-install)

;; Since "gx" is being overwritten by evil-exchange I'll remap it's
;; functionality to this keybinding (evil-downcase) which I don't use
;; much anyway
(define-key evil-normal-state-map "gu" #'browse-url-at-point)

;; Just gotta have it!
(global-evil-visualstar-mode)

;; TODO: PR material. See todos in this file, I think it's best that I
;; add the functionality I'm achieving in this file by making an
;; addition to evil-mode itself.
(load "~/.emacs.d/evil-cutlass")

;; I'm a fan of shifted letters to do "opposite" sort of things. I
;; also think it's kind of weird the inconsistancy of keybindings for
;; the default undo/redo (undo being an unshifed "u" where redo is a
;; ctrl chord).
(define-key evil-normal-state-map "U" #'evil-redo)

;; It would seem that when setting evil-search-module to evil-search,
;; which I want to do so I get those "gn" text objects, the function
;; evil-ex-search gets called and the ONLY thing it does when a search
;; wraps is display a message saying "Search wrapped". This is a shame
;; because it's very easy to miss this. It's also a bit surprising
;; because if evil-search-module is set to the default "isearch" then
;; it calls out to evil-search which will call the ding function if
;; the search wraps and a setting is configured. It's just odd that
;; within the same module there's a way to have it ding or not. So
;; I've gotta use some advice! TODO: PR material. Make a PR to get
;; this added to evil mode. It seems like it should and the change
;; should be easy, there is already a configuration option
;; evil-search-wrap-ring-bell which we could probably use.
(defun lag13-make-evil-search-ding-on-wraparound (evil-ex-search-fn &rest args)
  (let ((start-point (point)))
    (apply evil-ex-search-fn args)
    (when (or (and (equal evil-ex-search-direction 'forward)
		   (<= (point) start-point))
	      (and (equal evil-ex-search-direction 'backward)
		   (>= (point) start-point)))
      ;; TODO: I think I need to give this arg so the 'gn' text object
      ;; works properly. Double check this. I'm curious on the exact
      ;; situations that trigger this but if I don't do this I see an
      ;; error message: After 0 kbd macro iterations:
      ;; evil-motion-range: Keyboard macro terminated by a command
      ;; ringing the bell. Oh, actually I think I know what's
      ;; happening. When the cursor is on the search tearm and gn is
      ;; pressed, it changes the current search term which (I guess)
      ;; means that a search happens but doesn't move and thus the
      ;; ding happens. I feel like that's fine and easily avoidable by
      ;; adding the argument to 'ding' but it's a bit ugly and should
      ;; be fixed. Looks like there's some more work to do before this
      ;; is truly PR ready.
      (ding 1))))
(advice-add 'evil-ex-search :around #'lag13-make-evil-search-ding-on-wraparound)

;; TODO: https://github.com/PythonNut/evil-easymotion is listed in
;; doom emacs and apparently it's built on
;; https://github.com/abo-abo/avy. What's the relationship there?
;; Which one should I get?

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
;; something more. I think this is a way around it though:
;; https://emacs.stackexchange.com/questions/7832/how-to-bind-c-for-real
;; It all seems rather annoying though. C-] IS escape by definition
;; and there also just seems to be a lot of old cruft with emacs'
;; keybinding stuff.

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

;; TODO: PR material. dumb jump doesn't support "rg" for some regex's
;; which confuses me, I guess I thought it would just work for
;; whatever the underlying tool is?
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

;; TODO: PR material. Thoughts about getting dumb jump to run more
;; quickly in the monolith. So I think I've confirmed that the regex
;; which dumb jump executes is just slow with rg. I suspect that it
;; literally takes too long for rg to scan the files but I'm not sure
;; yet. I want to investigate more. Part of me suspects that
;; parameters don't filter by files? Or even if they do does it really
;; just take that long?? EDIT: Confirmed, it looks like the rg command
;; is:
;;
;; rg --color never --no-heading --line-number -U --pcre2 let\\s\+asOption\\b.\*\\\=\|member\(\\b.\+\\.\|\\s\+\)asOption\\b.\*\\\=\|type\\s\+asOption\\b.\*\\\= d:/inetpub
;;
;; While the git grep command is:
;;
;; git grep --color=never --line-number --untracked -E let\\s\+asOption\\b.\*\\\=\|member\(\\b.\+\\.\|\\s\+\)asOption\\b.\*\\\=\|type\\s\+asOption\\b.\*\\\= -- d\:/inetpub/\*.fs d\:/inetpub/\*.fsi d\:/inetpub/\*.fsx
;;
;; I think the actual regex is this when I remove extra backslashes:
;;
;; let\s+asOption\b.*\=|member(\b.+\.|\s+)asOption\b.*\=|type\s+asOption\b.*\='
;;
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
;;
;; So, it looks like they do already add the appropriate --type
;; extension for rg but a certain field has to be set in the
;; dumb-jump-language-file-exts data structure. Again, is there a
;; reason why some of this stuff is not set already? It seems like it
;; should just default to working?

;; TODO: I'd be curious about what it would take to get fully
;; functional web browser within emacs. On
;; https://www.emacswiki.org/emacs/CategoryWebBrowser they mention
;; https://github.com/emacs-eaf/emacs-application-framework which
;; feels quite interesting. I also see this video:
;; https://www.youtube.com/watch?v=y1k_lA2VUYg&ab_channel=AnandTamariya
;; Maybe this too since I feel like functionality like this is at a
;; minimum what I might want:
;; https://www.reddit.com/r/emacs/comments/syih7g/fuzzy_searching_apples_online_docs_w_ivy/

;; TODO: I did a ripgrep in a new repo I was exploring and basically
;; it returned a lot of test and non-test files and I only wanted to
;; look at the non-test files so I copied that ripgrep output to a
;; file and did a "keep-lines" for just the files in question and then
;; did "gf" on each one and poked around. I just feel like there
;; should be a better way to remove those files I don't want from the
;; results? It's structured data after all, I should be able to mess
;; with it.

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

;; TODO: PR material. I noticed that the evil binding "] SPC" did not
;; work in a buffer called "asdf" that I opened. I assume that this
;; buffer didn't really have a mode and so evil didn't bind some
;; stuff? Also, I think wherever the unimpaired functionality is
;; coming from, it's not complete (for example there's a command in
;; unimpaired to go to the next file in the current directory or
;; something which I don't think evil stuff defines). I feel like I
;; want to look into this more and figure out what's going on.

;; TODO: I think I like emacs M-f/M-b motions for moving the cursor by
;; words better than vim. Like if you have hello.there.everyone then
;; it just moves over the words pretty much but vim will move on the
;; '.' characters too which makes it slower. I think I like those
;; motions from emacs for quickly motoring over on the same line and
;; it seems easier than vim because the motion feels more predictable.

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

;; TODO: PR material. I noticed that the top of my init file was
;; mysteriously getting deleted sometimes and although I still don't
;; understand WHY it is happening, I think I can trigger it happening
;; if I type something like:
;;
;; d C-h k d
;;
;; So it would seem that getting help on evil operators does not work
;; properly (or perhaps I have some customizations which messes stuff
;; up somehow). I bet I could figure this out and fix it.

;; TODO: PR material. It looks like (warning: very rough understanding
;; here) the way that evil mode gets bindings like "dd" and "yy"
;; working is that the function evil-operator-range will bind things
;; to the map evil-operator-shortcut-map temporarily as the operator
;; is executing. I bring this up because I wanted to add a commentary
;; text object "gc" to https://github.com/linktohack/evil-commentary
;; like Tim Pope has defined for his vim plugin but with this
;; functionality that is seemingly built into evil, I don't think I
;; can do that.

;; TODO: I notice that the "i(" text object doesn't work when the
;; parentheses are within comments and the open and closed parentheses
;; are on different lines. I wonder why this is? Just curious. This
;; does work in vim (although not as well).

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

;; TODO: I wonder if it would be possible in emacs to implement
;; continuations. I feel like it would be so cool to have in an
;; interactive environment because I feel like you could kick off some
;; long running process and, if you want to do something else, you
;; stop it and grab a little continuation of it. Whenever you want to
;; resume said process you just tell the continuation to go again. I
;; feel like once you had that continuation too, then you could
;; analyze it to see how far along the process is or things like that.
;; Maybe I'm misunderstanding how they're used or maybe there's a way
;; to get what I'm asking for without them but at least in my head it
;; feels like a super neat thing that one could have.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode line stuff
;;
;; i also used the customize interface to change the value of
;; mode-line-format to include just the info i want (i looked through
;; the default value and took what i thought would be useful). i used
;; the customize interface because when i tried just setq, it only
;; changed it for this buffer but not others and i didn't bother
;; investigating further because using customize "just worked".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(line-number-mode 0)

(defvar lag13-mode-line-buffer-identification
  (propertized-buffer-identification "%b")
  "My version of the `mode-line-buffer-identification'. The only
difference is that mine does not include padding which I want
because I want to try having a more concise modeline.")

;; The default value for this is "%12b" but I didn't want all that
;; padding. Originally I tried setq but it would seem that this
;; variable is buffer local and so doing that only changed that
;; buffer. Using setq-default like this seems to have worked.
(setq-default mode-line-buffer-identification
	      (propertized-buffer-identification "%b"))

;; Tell what mode I'm in purely based on the cursor.
(setq evil-mode-line-format nil)
(setq evil-emacs-state-cursor 'hollow)

(defun lag13-adjust-color-brightness (color-str multiplier)
  "Takes a color string (like #005aa3) and uniformly adjusts each
component of the color string with MULTIPLIER which effectively
brightens/darkens the color. Originally created because I was
using the vscode-dark-plus theme and wanted to make the
mode-line-inactive face even darker."
  (->> color-str
       (color-name-to-rgb)
       (seq-map (lambda (c) (* multiplier c)))
       (apply #'color-rgb-to-hex)))

(defun lag13-modify-face-brightness (face face-attribute multiplier)
  "Modifies the brightness of a particular face's attribute. See
`lag13-adjust-color-brightness' for why I created this."
  (set-face-attribute
   face
   nil
   face-attribute
   (lag13-adjust-color-brightness (face-attribute face face-attribute) multiplier)))

;; I wanted the mode lines to be a bit darker for this theme and for
;; the contrast to be greater between active and inactive mode lines.
;; Not sure if this is the "right" way to do it but it works! TODO:
;; How to have this run every time a theme is selected? Is there a
;; hook for that?
(when (equal (car custom-enabled-themes) 'vscode-dark-plus)
  ;; Original colors for this theme
  (comment
   (set-face-background 'mode-line "#007acc")
   (set-face-background 'mode-line-inactive  "#005aa3")
   )
  (lag13-modify-face-brightness 'mode-line :background 0.7)
  (lag13-modify-face-brightness 'mode-line-inactive :background 0.3))

;; TODO: I think I want the buffer name in the mode line to be JUST
;; the file name if it is unique within a project in order to save
;; screen real estate.

;; TODO: I think I'd like to experiment with highlighting the buffer
;; name in the mode line differently so it stands out more from the
;; rest of the mode line. Right now it feels kind of hard to tell at a
;; glance where I am.

;; TODO: I feel like saving the recent ripgrep search in a register "g
;; seems like a good idea. Because sometimes I do a ripgrep and then
;; want to go into wgrep mode to do search and replace stuff but I
;; might have to type out the regex again to get it.

;; TODO: How do we do ripgrep in multiple directories? Occasionally I
;; feel like I want to do this.

;; TODO: PR material. doing the command "dap" in an org mode document
;; does not seem to be working like I expect it to. If you are at the
;; end of a '*' header, then doing "dap" also targets the NEXT header
;; down. Not sure what's up with that.

;; More and more lately I'm feeling like if there's ANY way to help
;; add more context to what I'm doing, that will help me. In this
;; case, I feel like I'll jump around too quickly and I don't even
;; know what files I've looked at or how many I've looked at. It could
;; feel like I've looked at 10 different files in a project when
;; really I've only looked at 2 simply because I'm too focused on the
;; content instead of the file names. Maybe that means I just need to
;; slow down... but I'm not so sure. Adding another sensory cue (i.e.
;; showing the files up top) is going to be my attempt to solve for
;; it.
(require 'centaur-tabs)
;; I'm used to the default behavior of emacs where, if I kill a
;; buffer, it will open the last buffer shown in that window (or
;; something like that). I feel like that is more useful too than
;; centaur's default behavior where it tries to show a buffer within
;; the current tab group because if I'm already doing a lot of work
;; in a particular project/tab-group then centaur's behavior will
;; happen naturally and if I've only viewed one buffer in a tab
;; group then killed it, I'm not sure I'm doing enough work in there
;; anyway such that it should try to keep me there.
(add-hook
 'centaur-tabs-mode-hook
 (lambda ()
   (remove-hook 'kill-buffer-hook 'centaur-tabs-buffer-track-killed)
   ;; TODO: PR material. Would it be worth making a PR to add this to
   ;; the centaur repo? Maybe it could be the default behavior? TODO:
   ;; There is a bug somewhere in this. I'm in the monolith with all
   ;; those little sub projects and somehow the QueryEngineAPITests.cs
   ;; file for smoke and regression show the full buffername even
   ;; though the tab (we're using centaur's default tab thingy) only
   ;; shows one of those buffers at a time (I think centaur finds the
   ;; project earlier on).
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
		 cur-tab-filename)))))))
;; I'm disabling this for now. I think I still want something like it
;; when exploring new code but I notice that I really don't look at it
;; normally.
(centaur-tabs-mode 0)
(centaur-tabs-headline-match)
;; TODO: PR material. Only add the buffers to the list of previously
;; visited buffers if you do something within that buffer or remain on
;; that buffer for more than X seconds or something like that.
;; (define-key evil-normal-state-map (kbd "<C-tab>") 'centaur-tabs-forward)
;; (define-key evil-normal-state-map (kbd "<S-C-tab>") 'centaur-tabs-backward)

;; TODO: If I have a vertically split window and 5 tabs open (or at
;; least enough to cover >=3/4 of the screen) and I navigate to the
;; final tab, the beginning tab gets shifted out of view, I don't like
;; that. Could we try to keep all tabs in view? Or at least give an
;; indication when tabs are hidden?

;; TODO: Continue this experiment. I want to try replacing the stuff I
;; did with that bs-show stuff with this because I think it will feel
;; more natural. TODO: Also make sure that embark export properly
;; exports buffers selected in this way to an ibuffer.
(defun lag13-switch-buffer-on-complete-filepath ()
  (interactive)
  (let* ((buffer-alist (->> (buffer-list)
                            (seq-filter #'buffer-file-name)
                            (seq-map (lambda (b) (cons (buffer-file-name b) b)))))
         (presorted-completions (seq-map #'car buffer-alist))
         ;; Seems that we have to do all this nonsense just to get
         ;; completing-read to not alphabetically sort the list I give
         ;; it... Seems like too much. Thanks for these links for
         ;; inspiration:
         ;; https://emacs.stackexchange.com/questions/41801/how-to-stop-completing-read-ivy-completing-read-from-sorting,
         ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-List.html,
         ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion.html,
         ;; https://kisaragi-hiu.com/emacs-completion-metadata
         (completion-table
          (lambda (string pred action)
            (if (eq action 'metadata)
                '(metadata (category . buffer)
                           (display-sort-function . identity)
                           (cycle-sort-function . identity))
              (complete-with-action action presorted-completions string pred)))))
    (switch-to-buffer (cdr (assoc (completing-read "Buffer: " completion-table) buffer-alist)))))

;; TODO: PR material. Looks like centaur-tabs-buffer-groups does not
;; assign magit-revision-mode to the "Emacs" tab group which happens
;; with other magit buffers. Seems like I should make a PR to fix
;; that. Might be worth changing the check to be (string-prefix-p
;; "magit" (format "%s" major-mode)) or maybe just add another line to
;; the check.

;; TODO: PR material. The function centaur-tabs-hide-tab tries to hide
;; magit buffers but misses a lot of them because a lot of them will
;; have a file extensions which seems to be the name of the project.
;; So fix that or at least ask about it.

;; TODO: PR material. There are a couple todos on
;; https://github.com/noctuid/targets.el to implement an "entire
;; buffer" text object as well as a "line" text object (I know we have
;; a repeated operator key to do a line but these are dedicated ones
;; and I think the "inner" text object copies by characters).
;; https://www.reddit.com/r/emacs/comments/3ctjsv/evil_ilal_and_ieae_textobjects/
;; https://github.com/supermomonga/evil-textobj-entire

;; TODO: Could be useful to store the last emacs minibuffer code
;; evaluation in a register for easy future pasting? This reminds me
;; too that maybe I just want the ability to, in insert mode, search
;; through recent history and paste that (like C-r in a shell)

;; TODO: PR material. When switching projects in projectile, can the
;; projects be ordered by most recently used? They seem to just be
;; sorted alphabetically or something.

;; TODO: Make a hydra sort of thing where, after typing g;, I could
;; just keep hitting ';' to keep going back to older changes.

;; TODO: I wonder if it could be useful to have a gi sort of
;; keybinding which would KEEP jumping back to previous places that I
;; was in insert mode

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

;; TODO: This seems to be a neat assortment of packages:
;; https://two-wrongs.com/why-you-should-buy-into-the-emacs-platform.
;; I'm particularly curious about the calc package to do unit
;; conversions inside of emacs because I like the idea of being able
;; to do it within emacs instead of constantly googling.

;; TODO: I think I should start looking into improving emacs' startup
;; speed. Obviously it's not a huge priority but on 2022-05-20 I used
;; my work laptop (a windows machine which runs emacs via mingw) and
;; there wer 258 buffers (Desktop mode was enabled) and it took 8
;; MINUTES to startup. I'm also wondering why I have so many buffers
;; actually. I thought midnight mode should clean some of those up.
;; Anyway. That's way too slow. Not that I'm starting up emacs a lot
;; but still. Also worth mentioning though that my work laptop seems a
;; bit slow in general. Even unzipping something was apparently
;; instant on my friends work computer but took mine like 1 minute or
;; so. 
;; https://www.youtube.com/watch?v=9i_9hse_Y08&ab_channel=SystemCrafters

;; TODO: Mess around with https://github.com/gabesoft/evil-mc (this is
;; the one we need for evil mode) and see if it can be improved. I was
;; noticing that my rebind of 'x' to cut text just straight up didn't
;; work and I feel like I noticed other jankiness with it and it made
;; me curious about what the implementation looks like. Not urgent at
;; all. Multiple cursors is definitely something which is neat but
;; it's not at all necessary when you have features like macros,
;; search+replace, using the "gn" text object for quick search+replace
;; things, and visual block mode.

;; TODO: PR material (technically new codebase material). This seems
;; neat: https://github.com/wellle/context.vim I think I might enjoy
;; trying to port that over to emacs assuming something like it
;; doesn't already exist. I think it would be super cool to also
;; somehow combine that plugin with the grep functionality within
;; emacs because then you can see which top level form the items
;; matched against. I think it could also be a neat idea if you can
;; toggle a "show all context" sort of thing where it will show all
;; branch points that led to the code you're looking at or something
;; like that.

;; TODO: PR material. I think I should extend the visual start plugin
;; so that it works for g* as well mostly for completeness. The use
;; case is if you were searching for a string but also wanted to
;; include substring matches.

;; TODO: I think swiper or ivy or something has a thing where you do a
;; completing read on the lines in the current file and let's you jump
;; to them and I'm curious if something like that is actually useful
;; or if it's just another way to do a regular' ol search. I will say
;; that I think being able to do my completion styles (like orderless)
;; could be useful for me because I'll often remember a little phrase
;; I have typed before and I want to find it but cannot.

;; TODO: I want to improve my fuzzy file finding capabilities in
;; emacs. Projectile seems fine but I want to be able to use it
;; anywhere like in the current directory or in any old directory
;; which is NOT a project. Maybe I'll have to look into
;; https://github.com/bling/fzf.el or something. It appears that fzf
;; is a way to do essentially the "completing-read" emacs function but
;; for command line shells which feels super handy honestly. So you
;; could do something like: cat file.txt | fzf and then start typing
;; and it will filter for whatever you type. I wonder if there's
;; another way to do fuzzy file finding in emacs but this seems neat.
;; Heck, or maybe I do want to give helm another shot:
;; https://github.com/ibmandura/helm-fzf/blob/master/helm-fzf.el, they
;; seem to be able to do so many different types of completions. Like
;; wow, so many: http://tuhdo.github.io/helm-intro.html. I should
;; honestly probably take a look at helm for all the things I might
;; want to complete on. Off the top of my head I could see these being
;; things: copied text, shell history, buffers, buffers within a
;; project, all files beneath a directory, files within just one
;; directory, recorded macros, executed M-x commands, eval'd lisp
;; code, the jump list(?), looking up help documentation on things
;; like functions or variables (for emacs and other languages I
;; suppose), complete on buffers but include the directory path as a
;; searchable unit (for situations where I forget the file name but
;; know it's in a certain directory), history of searches, a bunch of
;; important links (like maybe for work) which I can then have emacs
;; open (or figure out more why chrome doesn't seem to want to
;; remember some of the links I've been to).

;; TODO: It would be interesting to have a search which would search
;; for different cases of a base word. Like searching for the word
;; "complete" would also match "completion".

;; TODO: I feel like this person has some cool stuff in their emacs
;; config: https://github.com/abo-abo/oremacs

;; TODO: Could emacs be used to take screenshots and annotate them?
;; Motivation is that the snip and sketch tool doesn't let one draw
;; rectangles by default so I always: take the screenshot, open the
;; screenshot in the snip and sketch app, tell the snip and sketch app
;; to open this file in Paint, draw the rectangle, save, go to the
;; file in a file explorer, copy the file, and (finally) upload it.
;; It's just a pretty hairy process and it would be nice if I didn't
;; have to have 3 apps (snip and sketch, paint, and file explorer)
;; just to annotate some screenshot.

;; TODO: The default comment for an ssh config file is ';' instead of
;; '#'. What's up with that? Fix it.

;; TODO: PR material? the rg plugin defines a couple searches to do a
;; ripgrep for the last isearch'd thing. I should make a PR to define
;; some rg searches that work for evil mode or something? Or maybe I
;; just need to start using vim's insert mode bindings (because then I
;; can just start a search and insert the text from the / register).

;; TODO: What if your editor could tell where your looking and jump to
;; that spot? Crazy idea (I'd need to get some sort of equipment) but
;; I think it would be super fun to mess around with.

;; TODO: Feel like I've written something like this. I've been
;; noticing that when I do an evil search, some of the past highlights
;; remain highlighted even if I search for something else. I think it
;; happens when I'm in buffer A, do a search, go to buffer B, do
;; different search, go back to buffer A and the original search terms
;; are still highlighted. It would be neat I think if there was a key
;; to jump to highlighted things.

;; TODO: I'm curious about this package:
;; https://www.reddit.com/r/emacs/comments/gmh8bs/beta_testers_ann_lets_make_emacs_better/

;; TODO: Maybe flash the cursor or an area around the cursor when
;; going to the next search? I say that because on 2022-01-31 I had an
;; rg search (so already the search terms were highlighted which makes
;; it a bit harder to see the cursor) and then I wanted to search for
;; just the regex \<2\> (i.e. very tiny) and a couple times I lost
;; track of where the cursor was. I feel like there was a vim talk
;; where a guy did this, yeah I think it was this guy, Damien Conway:
;; https://www.youtube.com/watch?v=aHm36-na4-4&ab_channel=O%27Reilly

;; TODO: Can I count the number of search results when doing a regular
;; ol' evil search?

;; TODO: Is there a way to do an rg search for a set of things to
;; appear in a line? Effectively (assuming you're searching for 3
;; terms) you'd do the first search, get the result lines, then do the
;; rg on those lines and you're left with the lines that only have
;; those 3 terms. Maybe there's just a regex to do this too, in my
;; head it seemed like chaining together the rg searches would be
;; faster though but maybe I'm wrong. My use case was that I wanted to
;; search for lines containing "AsyncRequestStatusTypeID" and "2" and
;; I didn't think the order particularly mattered.

;; TODO: The indicator in compilation mode which points out which
;; match we're on is not particularly obvious and, at a glance, looks
;; similar to the markers which show that the line is wrapped. Can we
;; fix this?

;; TODO: How do I go to the first/last error in the compilation buffer
;; and go to the current one (like if we navigated away and wanted to
;; go back to it).

;; TODO: Another guy which probably has a lot of good config:
;; https://www.youtube.com/watch?v=46w9e4GAjsU&ab_channel=ProtesilaosStavrou

;; TODO: I think I would like orderless to have a completion mode
;; where the text written after a separator can start ANYWHERE in the
;; next word. For example if we have first.secondthird then I can type
;; fir.thir and it would complete. As it stands now I don't think that
;; exists. Flex is an option but I'm starting to think that perhaps it
;; is too powerful for it's own good. The closest feature is
;; orderless-prefixes but it only allows for letters that start
;; EXACTLY after the separator. Worth pointing out that I feel like
;; orderless completion when writing lisp functions in the minibuffer
;; seems not so good it feels like it matches too much and I can't
;; select. Something closer to baisc completion might be better.

;; TODO: I was playing around with eshell and wanted to have better
;; filename completion. By default (assuming you set completion-styles
;; to "basic") it feels like it's gonna behave pretty much like bash.
;; But I think I'd like something better. It feels kind of annoying
;; when there are two completion candidates and I have to use the
;; mouse to click on the one I want OR just keep typing. I'm so used
;; to vertico I think I'd like something like that. I think this
;; package might be able to help, check it out:
;; https://github.com/minad/corfu

;; TODO: Check this out
;; https://github.com/emacs-tree-sitter/elisp-tree-sitter

;; TODO: PR material. I think I found a bug in the rg.el plugin. I'm
;; not sure what is happening but it seems that when the search
;; pattern starts with a '/' then nothing will be returned even if
;; there are matches. For example if /hello/there is in a file then
;; "hello/there" will match but searching for "/hello/there" will not.

;; TODO: Can I tell evil to do literal searches by default or
;; something? Or at least auto-escape when I paste something in?

;; TODO: I've probably said this before but this guy has a lot of
;; useful stuff. This is one I saw the title of today (perspective)
;; which I feel like would be useful in achieving my goal of being
;; better able to explore new code:
;; https://www.youtube.com/watch?v=uyMdDzjQFMU&ab_channel=SystemCrafters

;; TODO: I feel like I remember a keybinding in vim to go to the first
;; occurrence of a symbol within the current file. I guess it was like
;; gd? I feel though like it could be nice to have 'gd' look in the
;; current file before it goes traipsing about the entire filesystem.
;; Salvation could be closer than it thinks.

;; TODO: I think being able to fold code on indentation level could be
;; a helpful tool in better understanding what a file contains.

;; TODO: I think it would be helpful to me if, when jumping to
;; definition or something, it could alert me if I stay in the same
;; buffer vs go to a new one. I suppose I could just slow down too...
;; I should probably just pay more attention.

;; TODO: I notice with emacs' eshell doing C-c C-c doesn't seem to
;; kill the underlying process. I have no idea why this is. Is it
;; something to do with me running emacs on windows? I wouldn't think
;; so since it's a shell written entirely within emacs. So strange
;; though... I feel like I want to figure this out since I think it
;; could be neat to try it.

;; TODO: Could it be possible to say "git checkout the commit for this
;; branch corresponding to a date". Use case is that we have smoke
;; tests which run every day and, when debugging them, it feels like
;; it would be cool to checkout the exact version that failed. It is
;; pretty easy to just find the commit too but still! Seems neat.

;; TODO: Be able to tell rg to search in ALL files (right now it
;; respects .gitignore and such).

;; TODO: Get that evil diff plugin which lets you diff visual lines.

;; TODO: If I leave the state of being fullscreen (with
;; toggle-frame-fullscreen) then there is that ugly white bar at the
;; top of emacs where I can minimize it or close it (all that jazz). I
;; think I want to learn if I can either remove it or at least
;; colorize it. This might be helpful:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Frame-Layout.html

;; TODO: inspiration? https://codeinthehole.com/tips/vim-lists/

;; TODO: vim like keybindings in browser. surfkeys:
;; https://www.youtube.com/watch?v=E-ZbrtoSuzw&ab_channel=Leeren
;; https://chrome.google.com/webstore/detail/surfingkeys/gfbliohnnapiefjpjlpjnehglfpaknnc?hl=en-US

;; TODO: I feel like it could be useful to have a keybinding which
;; adds the current location to the jump list. Like maybe I'm just
;; gonna spam j to get where I want to go (I sometimes think that's
;; quicker still than figuring out the "optimal" way to get there) but
;; know I want to return to this position after I do whatever I'm
;; doing elsewhere.

;; TODO: I kind of feel like an initial n command should add to the
;; jumplist and then repeated spams of it should not. If you're
;; repeating the 'n' command then that gives me the impression that
;; you haven't found what you were looking for so why remember all
;; those extra spots you know? Actually, I'm kind of starting to think
;; that ALL repeated jump commands should not add to the jump list
;; unless the previous command was a non-jumping command.

;; Doing g; will put the cursor one character past the end of the most
;; recent change (at least for typing). Repeated uses of g; will jump
;; back to older changes but will SKIP a previous change if these
;; ranges overlap:
;;
;; [cur-change-start - glc-default-span, cur-change-end + glc-default-span]
;; [prev-change-start, prev-change-end]
;;
;; I think so at least. Honestly, it feels like this feature could use
;; some polishing. Basically I feel like vim just does it better.
;; First off, if I do a paste, then that seems to generate two changes
;; to jump back through for some reason (in standard vim it's just
;; one). If I do M-q (or gwip) then the beginning of every line in the
;; paragraph (except the first line) is a place to jump to (very
;; annoying and vim does not do this either). Uncommenting text with
;; gc I have no idea what's happening sometimes it doesn't even seem
;; to register as a change (which really makes no sense to me since
;; it's something that can be undone). TODO: PR material. Investigate
;; this weirdness I'm observing with the goto-last-change feature. An
;; initial finding:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Undo.html
;; buffer-undo-list is a big list of different things (it seems fairly
;; complicated, not sure why) BUT it seems that each undoable chunk is
;; delimited by nil values. I think goto-chg.el tries to visit EVERY
;; position in this buffer-undo-list variable when I kind of feel like
;; it should just visit the first item after a nil.

;; TODO: I don't think this quite gets me what I want. It only
;; autofills paragraphs as I type them but doesn't retroactively fill
;; them like M-q does. See if I can get this fixed. Also, this isn't
;; even enabled everywhere, so I should probably figure out how to do
;; that. https://www.emacswiki.org/emacs/AutoFillMode
(setq comment-auto-fill-only-comments t)
(auto-fill-mode 1)

;; I tried two other packages before settling on
;; https://github.com/dajva/rg.el.
;; https://github.com/nlamirault/ripgrep.el just seemed much less
;; polished so that's a no. I also tried
;; https://github.com/Wilfred/deadgrep and initially I didn't like it
;; but perhaps I'm just too used to the compilation-mode style
;; interface and it was also before I started using evil-collection so
;; maybe it's worth a retry. TODO: Try out deadgrep again, see if I
;; like it. I mean, with a name like that it has to be good.
(define-key evil-normal-state-map "S" #'rg-menu)

(rg-define-search lag13-rg-literal-project
  "Search for a literal string within the current project"
  :format literal
  :dir project)

(defun lag13-rg-visual-dwim (beg end)
  "Does a literal search for the visually selected region in the
project."
  (interactive "r")
  (evil-exit-visual-state)
  (lag13-rg-literal-project (buffer-substring-no-properties beg end) "*"))

(define-key evil-visual-state-map "S" #'lag13-rg-visual-dwim)

;; TODO: rg let's you list the available buffers in ibuffer mode but I
;; think it would also be nice to start a completing-read session and
;; let the user select the buffer.
(defun lag13-rg-switch-buffer ()
  (interactive)
  (let (res)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(when (equal major-mode 'rg-mode)
	  (push (buffer-name buf) res))))
    (switch-to-buffer-other-window (completing-read "RG buffer: " res))))

(defun lag13-query-replace-rg-mode-modified ()
  "Does the same thing as the regular `query-replace' command but
with a twist that if the major mode is `rg-mode' (and we started
`wgrep' of course), it will autofill the thing to be replaced
with the previously searched term to speedup that process."
  (interactive)
  (if (eq major-mode 'rg-mode)
      (progn
	(barf-if-buffer-read-only)
	;; TODO: Is it considered better practice have these things be
	;; arguments to this function and then read them in the
	;; interactive bit? Not really sure. Also, it seems a shame to
	;; kind of reinvent the wheel with reading this stuff but I
	;; don't think I can do the default stuff I want to do.
	(let* ((from-string (read-from-minibuffer "RG Query replace: " (rg-search-pattern rg-cur-search)))
	       (to-string (read-from-minibuffer (format "RG Query replace %s with: " from-string))))
	  (query-replace from-string to-string)))
    (call-interactively #'query-replace)))

(evil-define-key '(normal visual) 'global "Q" #'lag13-query-replace-rg-mode-modified)
(evil-define-key '(normal visual) 'global (kbd "C-Q") #'query-replace-regexp)

;; TODO: Learn more about working with corfu:
;; https://github.com/minad/corfu. I'd like to better understand the
;; built in completion stuff especially as it compares to dabbrev and
;; pcomplete.
(corfu-global-mode)
(setq tab-always-indent 'complete)

(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input))
    ;; (setq-local corfu-auto nil) Enable/disable auto completion
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local corfu-auto nil)
            (corfu-mode)))

;; TODO: This looks like a nice theme!
;; https://www.reddit.com/r/emacs/comments/slsvpq/i_created_a_new_dark_theme/.
;; Based on https://github.com/rebelot/kanagawa.nvim.

;; TODO: That ^^ has inspired me a bit. I think my favorite painting
;; is Nighthawks and I'd be curious about making that into a color
;; theme.

;; TODO: Rgrep but just within the open buffers which are beneath a
;; particular directory? I guess I'm trying to optimize here but my
;; use case is that I work in a very large codebase so ripgrep's can
;; take a little while sometimes but if I know that something is in a
;; file I've recently opened (but I just can't remember the name) it
;; could be convenient to do.

;; TODO: For whatever narrowing operator I end up making I feel like
;; it should center the narrowed text on the window.

;; TODO: Have C-o/C-i ding or something when it jumps across files?
;; Again, sometimes I think I just get lost in what is going on. Again
;; though, maybe I should just slow down a bit. I guess in general I'm
;; playing around with the idea of adding more context clues to these
;; sorts of jumping things.

;; Make it nicer to read lines that wrap
(setq-default word-wrap t)
;; Primary motivation for using this visual-fill-column-mode stuff is
;; to make README.md files nicer to read within emacs. Because most
;; README's I encounter have super long lines.
(setq-default visual-fill-column-width 90)
(add-hook 'markdown-mode-hook #'visual-fill-column-mode)
(global-visual-line-mode 1)

;; TODO: Default ediff to do a vertical split.

;; TODO: When doing M-g M-n I feel like it could be nice to center the
;; cursor? Might be nice when searching with / for that matter.

;; TODO: Code splunking idea. 1. Make named perspective buffer group.
;; 2. Start looking through the code, if we find something promising
;; do a clone-indirect-buffer, narrow the buffer to the relevant area
;; of interest, and add that buffer to the perspective buffer group.
;; EDIT: A quick dive into perspective and I'm not sure if it's really
;; what I want tbh... It seems more about opening up a perspective and
;; then starting to explore but I feel like that seems a little messy
;; because then every file you explore (even the dead ends) end up
;; getting added to this perpsective. I feel like I want to explore
;; and not worry about making a cluttered list and then when I find
;; something I add JUST that thing to a curated list. Who knows
;; though, maybe perspective is a good first step since the buffer
;; list will be smaller. Hmmm I wonder if this could be useful too!
;; https://github.com/bastibe/annotate.el. I feel like you could
;; annotate the code you're interested in and then jump around those
;; annotations? I'd also be curious about building my own tree like
;; structure of files. I wonder if poking around undo-tree could be
;; useful inspiration here. Or maybe this thread
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2014-11/msg00022.html
;; which mentions a tree-widget package within emacs. This might be
;; the next evolution from tree-widget:
;; https://github.com/DamienCassou/hierarchy (oh man! That hierarchy
;; thing also mentions using it for navigating json which I know was a
;; previous ask of mine). More inspiration for other splunking related
;; things:
;; https://ag91.github.io/blog/2020/12/18/emacs-as-your-code-compass-finding-code-hotspots/,
;; https://news.ycombinator.com/item?id=8263402,
;; https://news.ycombinator.com/item?id=9784008
;;
;; Another good point brought up in some of those articles is that a
;; good general "understand this codebase" thing could be to see the
;; frequency that developers work on certain files. I also feel like
;; perhaps I should just get better with chrome developer tools? Maybe
;; that could help? Or maybe that's difficult to do with minified
;; files. ALSO, I feel like there should be some code analysis tools
;; out there make a relationship graph or whatever. Why do I have to
;; manually do this when the machine should already know!
;;
;; New method proposal. Just make org links to file locations and add
;; descriptive names to those links. I can just organize them however
;; I feel is useful. I'll always write a human readable description
;; for each link I insert which will force me to slow down. I think I
;; might always want to write the full path to the file from the
;; project root in the link description to force me to slow down and
;; get a better grasp of which files I'm looking at. It could be cool
;; to be able to tab complete it though (like maybe a shadowed version
;; is displayed which I can then type out or just tab complete). It's
;; funny because I used to kind of do this (I think I just inserted
;; the link with no description though) so I guess I'm returning to
;; this. I suppose at GR the depth of the code wasn't as great though
;; so maybe I didn't need to do it. I think this method is kind of
;; free form though and making your own curated list will help me
;; remember more I think.
;;
;; TODO: hierarchy.el is part of emacs as of v 28.

;; (persp-mode 1)

;; TODO: Looks like emacs also has tabs in the vim sense (window
;; layout holders):
;; https://www.youtube.com/watch?v=C7ZlNRbWdVI&ab_channel=SystemCrafters
;; Might want to poke around with that.

;; TODO: Under what condition can I cycle through entries in the
;; compliation buffer but do NOT cause that buffer to pop up? I feel
;; like it could be nice to just keep a vertical split window layout
;; where I cycle through matches in one buffer and take notes in
;; another.

;; TODO: The org link face (or maybe every link face?) has an
;; underline which makes it hard to see '_' characters in the text.

;; TODO: Any org link to a file location seem jump to a location based
;; off a text search (which is cool because then those links could
;; keep working as files change) but if the same string exists in two
;; places then it would go to the first one! Is there a way to
;; alleviate this? Like it would be cool to add a line number too and
;; it will pick the closer string match to the given line or
;; something.

;; TODO: For some reason gj and gk to move by visual lines don't seem
;; to register until I do something like look up the help
;; documentation on them.

;; TODO: Mode line customization ideas:
;; https://www.reddit.com/r/emacs/comments/6ftm3x/share_your_modeline_customization/
;; https://seagle0128.github.io/doom-modeline/

;; TODO: My cursor was between the last search result and the line "rg
;; finished (x matches found)" in a rg buffer and pressing 'k' would
;; not move the cursor up a line. It was strange.

;; TAB STUFF
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; TODO: I think I'm starting to need my own internal database of
;; "useful things". Like I feel like sometimes I get a "unit" of
;; knowledge which I want to save off somewhere and search for later
;; but I don't have a good way to do that. At the moment I usually
;; just accumulate things in a giant text file which quickly gets
;; unwieldy since there's a lot of junk in there too.

;; TODO: I feel like I want to make a function in org mode to sort
;; tables by multiple columns and sensible defaults. NOTE that using
;; the org-sort function (which calls out to org-table-sort-lines) CAN
;; work to sort multiple columns, you just have to call it on each
;; column in question but I thought it'd be nice to be able to just do
;; it one one fell swoop? Or maybe what I really want is to somehow
;; lock a table in a particular sorting setup?

;; Trying out org roam
(setq org-roam-directory (file-truename "D:/src/github.com/lgroenendaal-salesforce/work-notes/"))
(org-roam-db-autosync-mode)
(global-set-key (kbd "C-c r f") #'org-roam-node-find)
(global-set-key (kbd "C-c r i") #'org-roam-node-insert)
(global-set-key (kbd "C-c r l") #'org-roam-buffer-toggle)
(require 'org-roam-dailies)
(global-set-key (kbd "C-c r d") org-roam-dailies-map)

;; TODO: Things I want from org-roam and questions. 1. Be able to find
;; all the work-items/jira-tickets I've been involved with. More
;; generally I think I want to be able to query for all nodes of a
;; particular type/category. Does that mean the process here would be
;; to create a node per work item (totally on board with that, it
;; makes sense) and then somehow tag them? Or if there is no tagging
;; mechanism would I somehow need to maintain a separate node just
;; linking to all the other work items? Like I could have a node
;; called "person" and then any other node linking to this "person"
;; node could be interpreted as meaning that that node is a person. A
;; directed arrow from A -> B basically could mean that A is a B. Or
;; maybe I just do use tags whenever I want to categorize something.
;; I'm really torn... maybe that means it doesn't really matter and I
;; should just pick something. Tags seem nice because I know how I can
;; make them appear when switching to a node which means I can select
;; on them easily. But I feel like tags aren't really part of the
;; graph? Like if I have 10 nodes and each one is a person (and is
;; tagged as such) then it feels like they should be related somehow
;; on a graphical level but the tag doesn't feel like you get this
;; kind of added relationship. Maybe it doesn't matter though, I
;; should probably just focus on actual use cases here and just do
;; what works. Queries I can think to write with categories or
;; potential relationship quoted: See all "resturants" in "new york"
;; that I've been too with the "boggle" group. Find all "vegan"
;; "recipes". Find all "recipes" that I made with "Juanita". Find all
;; "recipes" with a specific "set of ingredients". Maybe this could be
;; helpful:
;; https://www.reddit.com/r/emacs/comments/p6w3dx/org_roam_can_i_filtercomplete_on_a_subset_based/
;; https://www.reddit.com/r/orgmode/comments/iljluu/tags/. I notice
;; too that apparently org has the concept of a "category" already, I
;; wonder if I should use that:
;; https://github.com/org-roam/org-roam/issues/1844. Maybe I need to
;; be reading up on just org mode itself and getting good with that:
;; https://orgmode.org/worg/org-glossary.html. I was also kind of
;; curious if it would be possible to find a node based on the nodes
;; it relates to. Like in all those example I gave (like find all
;; "recipes" I made with "Juanita") I could go to the "Juanita" node
;; and then search for neighbors and assuming "recipes" is a tag then
;; that works but what if I want to find a node related to MULTIPLE
;; other nodes? I can't do that neighbor trick. Maybe it doesn't
;; really matter but I'm curious if there is a way to easily find a
;; node based on related nodes like it seems to be possible for tags.
;; I feel like if I was to do this I'd need to change the minibuffer
;; completion interface. Maybe
;; https://karthinks.com/software/fifteen-ways-to-use-embark/ or
;; https://github.com/minad/marginalia or
;; https://www.reddit.com/r/emacs/comments/tnih4d/vertico_marginalia_miniframe_lot_of_hacks/
;; could offer some inspiration. 2. I'm not sure which direction I
;; should make a link in. Like, if person X teaches me topic Y, in Y
;; should I link to X? Or the other way around? Both? Not sure what a
;; good methedology here is. Maybe it doesn't really matter and it's
;; just whatever makes sense to you. Maybe just establish one link in
;; either direction and it doesn't really matter since the backlinks
;; stuff essentially establishes a bidirectional thing? Gotta think
;; through this more. 3. It feels like it could be nice to pull a
;; subset of nodes into ONE document for a presentation or something
;; like that. Basically a particular view on the network that has been
;; built out. 4. Should I keep one giant TODO document? Or put TODOs
;; local to nodes? I want to find them all later though. How do I do
;; that? 5. If I rename a node title, how do I propagate that to all
;; links into that renamed node? What about renaming the file as well?
;; 6. Similar to the TODO thing I think. I feel like I might, in
;; separate nodes, add notes like how to do a splunk query to analyze
;; the data. But I also feel like I might want to get a view into ALL
;; the splunk queries I know? Do I just make a new node for each
;; splunk query? Or maybe this view is less useful than I think? Still
;; it feels like an operation I can "imagine" so maybe it's useful and
;; partly I just want to have the ability anyway. 7. I feel like I
;; want to categorize EVERYTHING because I want to be able to think
;; about things in different ways. Like maybe I created a node about
;; HAR files but sometimes I know I want to find that HAR file node
;; but I just can't remember the "HAR" name but I DO remember that I'm
;; trying to think of a "file type". I feel like I should be able to
;; look for "file type" and pull it up. I guess I could just grep for
;; it too? Maybe I'm trying to much to pre-optimize this stuff though,
;; this hasn't been an issue yet and maybe I shouldn't address it
;; until it is. I feel like I might want to do more complicated things
;; too which might involve some parsing of files like: pull up all
;; "recipe" nodes which use a particular ingredient or pull up all
;; "song" nodes which have a particular chord progression. There's so
;; many types of queries I would like to be able to do!! Seems like a
;; potentially good source of organization tips:
;; https://www.youtube.com/watch?v=ljyo_WAJevQ&ab_channel=ShuOmi. Also
;; maybe good
;; https://www.reddit.com/r/emacs/comments/lhqwyw/orgroam_and_tags_for_querying_philosophy_issues/
;; 8. Find out a good methedology of using tags vs refs vs just
;; linking to specific pages who's main purpose is to serve as a
;; grouping of other ideas (i.e. similar pages will link to this one
;; so when you visit that page you can see all the backlinks). This
;; guy seems to exclusively create dedicated pages with specific
;; categories: https://www.nateliason.com/blog/roam. It's a great
;; article honestly. Roam seems super neat. I like that it can do
;; renames and that it has a feature where you can basically recursive
;; grep for a node title and anywhere that title shows up in other
;; nodes but is NOT a link then it optionally let's you make it a
;; link.

;; TODO: Open a new buffer, hit G to go the bottom of it, then hit C-o
;; and you end up in a different file. I think this only works for non
;; file buffers? There's definitely something wonky going on though. I
;; think you get the same behavior if you type '' instead of C-o

(cl-defmethod org-roam-node-closed-time ((node org-roam-node))
  "Returns the value of the CLOSED property which is the time that
an item moves from TODO to DONE. This CLOSED property gets added
due to the `org-log-done' setting.

My thought for having this was that I could include the date
things get closed in the `org-roam-node-display-template' just to
add even MORE context in case it can help with selecting a node."
  (alist-get "CLOSED"
             (org-roam-node-properties node)
             nil
             nil
             #'equal))
;; Looks like this is a way to display and complete on tags when
;; running a find on a node. TODO: I feel like it could be cool if in
;; addition to the tags I could define a section of the org mode
;; document where I link to other pages AND those pages also show up
;; when completing. Because I feel like I might want to have both a
;; dedicated page I can navigate to and see a bunch of backlinks
;; related to it OR just start completing on that page and then it'll
;; bring up the backlinked things as well which I can more quickly
;; complete and select.
(setq org-roam-node-display-template
      (concat "${title}"
              (propertize "${todo}" 'face 'org-todo)
              (propertize "${tags}" 'face 'org-tag)
              (propertize "${closed-time}" 'face 'org-date)))

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

;; Just makes sense doesn't it?
(define-key org-roam-mode-map (kbd "C-<return>") (lambda ()
                                                   (interactive)
                                                   (let ((current-prefix-arg '(4)))
                                                     (call-interactively #'org-roam-preview-visit))))

;; Originally added because the docs
;; https://www.orgroam.com/manual.html#Browsing-History-with-winner_002dmode
;; reccommended using winner mode as a way to browse history.
;; Originally I used it and bound it to their recommended keys (meta
;; left and right) but abandoned that because it interfered with org
;; modes default bindings for these keys.
(winner-mode 1)

;; TODO: Feels like we should create an org-roam function to rename a
;; node which renames both the title and the file name.
;; https://org-roam.discourse.group/t/how-to-rename-a-note-with-everything-updated-at-the-same-time/300/4
;; https://github.com/org-roam/org-roam/pull/124

;; TODO: Make an org roam function to follow the link under the cursor
;; to another node which references it. Gonna use org-roam-ref-find
;; for this one. Just seems convenient to be able to paste a link but
;; still be able to follow it internally if I want.

;; TODO: Hitting TAB on the "backlinks" section for org roam actually
;; does evil-jump-forward. Why is that?
(defun lag13-org-roam-get-node-neighbors (node-id)
  "Returns a list of NODE-ID's neighbors. I consider nodes X and Y
to be neighbors if:

1. Node X has a link going to Y and vice versa.
2. Node X is contained within node Y and vice versa

That first definition is pretty self explanatory. For the second
one it felt right to me to have one node that contains another to
have a relationship.

"
  (let ((outgoing-link-ids
         (seq-map #'car (org-roam-db-query
                         [:select
                          dest
                          :from links
                          :where (= type "id")
                          :and (= source $s1)]
                         node-id)))
        (incoming-link-ids
         (seq-map #'car (org-roam-db-query
                         [:select
                          source
                          :from links
                          :where (= type "id")
                          :and (= dest $s1)]
                         node-id)))
        ;; Currently this implementation assumes no more than one
        ;; level of node nesting (i.e. at most a file node and a
        ;; header node but no header nodes inside the other header
        ;; nodes). It won't break if you have more nesting but I don't
        ;; think I would like the result this function produces. In my
        ;; head I think it makes sense that node X is a neighbor of
        ;; node Y if X DIRECTLY contains Y. For example, in the
        ;; situation of node X contains Y contains Z, I don't think X
        ;; and Z should have a direct relationship. X and Y would be
        ;; neighbors and Y and Z would be neighbors. I haven't had the
        ;; desire to do nesting beyond a file and a header node so I'm
        ;; leaving this simpler implementation for now. If I wanted to
        ;; get fancier I'd probably need to use org-roam-node-olp
        ;; which returns a list of org headings which I could feed
        ;; into org-roam-node-from-title-or-alias to find out which
        ;; are actual nodes and which are just headings and then get
        ;; the containing node that way. I'd also need to do something
        ;; different than just using org-roam-db-map-nodes because I
        ;; just want the nodes that are immediately contained by the
        ;; file, not every node.
        (nodes-within-nodes
         (let (res
               (node (org-roam-node-from-id node-id)))
           (if (equal (org-roam-node-title node)
                      (org-roam-node-file-title node))
               (org-roam-db-map-nodes
                (list (lambda () (push (org-roam-id-at-point) res))))
             (push (org-roam-node-id (org-roam-node-from-title-or-alias (org-roam-node-file-title node)))
                   res))
           ;; TODO: This needs to be fixed so I'm just returning nil
           ;; for now. I have a org todo about it somewhere
           nil)))
    (seq-uniq (append outgoing-link-ids incoming-link-ids nodes-within-nodes))))

(defun lag13-org-roam-node-find-neighbors ()
  "Find and open an org-roam node that is a neighbor of the
current one by it's title or alias."
  (interactive)
  (let* ((current-node-id (org-roam-id-at-point))
         (neighbors (lag13-org-roam-get-node-neighbors current-node-id)))
    (org-roam-node-find nil
                        nil
                        (lambda (node)
                          (seq-contains neighbors (org-roam-node-id node))))))

(defun lag13-org-roam-node-find-neighbors-walk ()
  "Continually \"walks\" to neighboring nodes."
  (interactive)
  (while t (lag13-org-roam-node-find-neighbors)))

(defun lag13-org-roam-node-find-journey ()
  "When initially invoked it travels to any arbitary node and then
travels by neighbors from there."
  (interactive)
  (org-roam-node-find)
  (while t (lag13-org-roam-node-find-neighbors)))

(global-set-key (kbd "C-c r n") #'lag13-org-roam-node-find-neighbors)
(global-set-key (kbd "C-c r w") #'lag13-org-roam-node-find-neighbors-walk)
(global-set-key (kbd "C-c r j") #'lag13-org-roam-node-find-journey)
(global-set-key (kbd "C-c r r") #'org-roam-refile)

;; https://zettelkasten.de/posts/overview/

;; TODO: Add a splunk major mode so I can add org mode source blocks
;; with splunk as the language. My use case atm is that I'm doing
;; some... literate code spelunking I guess I'll call it where I
;; explore code and data and documentation to try and figure out how
;; some code is working and I want to be able to add "splunk" source
;; blocks so, when my file gets really large, I can easily find them
;; again.

;; TODO: Org mode links break when they get split over a couple lines
;; (which is not uncommon when I'm spamming M-q). It's interesting
;; actually, when you are initially in a session and do M-q then the
;; link still seems fine but if you kill and reload the file then the
;; link is broken. Is there a way to fix this?

;; TODO: This guy seems to have some super quick and good videos about
;; emacs: https://www.youtube.com/channel/UCifbsgGiPLXEBPDKe8_NggQ

;; TODO: Another code spelunking thought. I think I want something
;; like perspective but less... restrictive? Maybe I should revisit
;; perspective honestly (or https://github.com/Bad-ptr/persp-mode.el
;; perhaps?) because I wouldn't be surprised if it could be configured
;; to do what I want but from what I remember I didn't like how it
;; felt hard to switch to files that were not in the perspective (for
;; example I always have notes I work on regardless of what project
;; I'm in or I always visit init.el whenever ideas strike). Also, I
;; didn't like how a buffer added in a perspective was not part of the
;; main buffer list. Again, I should revisit that plugin. I think all
;; I really want is to ALWAYS explicitly add a buffer to a perspective
;; and every buffer I open will get added to the global list of
;; buffers as is the norm. I seem to remember that when I briefly
;; played around with perspective I still ended up accumulating more
;; buffers than I wanted (like dired buffers, buffers I'd step through
;; from RG results but didn't find helpful, the *Messages* buffer,
;; help buffers, etc...) and I feel like I want a truly hand curated
;; list of just files I'm interested in. I'm writing this after
;; watching Anitha do some code spelunking and man is she fast and I
;; don't think I'm naturally that good but I think I could be better
;; if I had a tool like this to lean on. I know earlier I said that
;; the way I'm gonna do code spelunking is just keeping org mode links
;; and I still feel like that is something I'll do but I am also kind
;; of thinking that that org link approach is kind of heavy and not
;; always necessary where this is kind of lightweight and just helps
;; give a little strength to my brain to remember what I've looked at
;; recently that is relevant to the research at hand. In addition I
;; feel I should write a function (which is probably separate from
;; this functionality) which takes a list of buffers and inserts a
;; bunch of org links into the current file for every buffer that was
;; a file. In theory this one two punch means I could explore new code
;; and have a higher likelihood of keeping track of it and then save
;; that knowledge off to some documentation somewhere for future me.
;; I'd also like to be able to do things like grep just within this
;; set of files.

;; TODO: I think I wrote this somewhere before but in case not. I
;; think if could be useful to setup the hjkl functionality such that
;; if you repeatedly hit only those keys "enough" times (which I might
;; do when reading a file) that it adds a jump point at where you
;; started hitting those keys. Then, after kind of travelling around
;; for a while, I can just jump back to where I started.

;; TODO: I feel like I might want C-e and C-y to scroll the cursor as
;; well. Or maybe keep the cursor in the middle of the page? Because I
;; feel like I'm always pretty much just staring at the middle of the
;; page here.

;; TODO: Define the org-link-make-description-function function so
;; that when inserting a link toa file it just inserts the file name
;; because I think that's all I want most of the time.

;; TODO: I learned that org-insert-link can be given a C-u argument
;; and with that argument it prompts for a file to link to which I
;; think is pretty cool but I think I would be more useful to me if it
;; could complete a file path from a buffer name. I could also just

;; TODO: I think it could be helpful in the RG buffer to display files
;; with a different highlighting if I already have them open. That way
;; I'll know at a glance if I've already looked at that file.

;; TODO: Is there a way to continue executing a macro until it hits an
;; "error" which could be something like "the search wrapped around to
;; the beginning of the buffer".

;; TODO: When I have a list of org links representing a manually
;; constructed call stack I think it would be nice to put that list in
;; a dedicated window off to the side and then whenever we click on
;; one it opens in a split. Perhaps perhaps.

;; TODO: Getting this error. I think it's somehow not properly
;; constructring the path to the GPG keys:
;;
;; Failed to verify signature sql-indent-1.6.tar.sig:
;; No public key for 066DAFCB81E42C40 created at 2021-06-27T09:10:02+0000 using RSA
;; Command output:
;; gpg: keyblock resource '/c/Users/lgroenendaal/.emacs.d/c:/Users/lgroenendaal/.emacs.d/elpa/gnupg/pubring.kbx': No such file or directory
;; gpg: Signature made Sun Jun 27 09:10:02 2021 CUT
;; gpg:                using RSA key C433554766D3DDC64221BFAA066DAFCB81E42C40
;; gpg: Can't check signature: No public key

;; TODO: It's so annoying to me that each package seems to handle
;; indentation completely uniquely. Why can't they all lean on the
;; same variables or something so I always know how to customize it. I
;; want https://github.com/tpope/vim-sleuth but for emacs. Maybe this
;; is what I need: https://github.com/jscheid/dtrt-indent
;; https://blog.meain.io/2021/Emacs-alternatives-for-the-Vim-plugins-you-know-and-love/

;; TODO: I've been battling with trying to get indentation working
;; properly for our transact sql sprocs and it's not looking
;; promising. First off the indentation in our files is all over the
;; place, it's a mess. Secondly though is that I can't seem to find a
;; package I'm satisfied with in terms of indentation. The
;; sqlind-minor-mode thing doesn't work well because it will indent
;; anything after that "GO" statement when I don't think you're
;; supposed to. sql-indent seemed potentially the nicest I guess but
;; it still made indentation more different than expected compared to
;; the file so I'm not sure. I think at this point I think I want to
;; just manually handle indentation. I got these two files from:
;; https://www.emacswiki.org/emacs/tsql-indent.el and
;; http://nullman.net/emacs/files/local-modules/tsql.el.html On first
;; pass this online converter thing actually didn't seem half bad:
;; http://poorsql.com/ from https://stackoverflow.com/a/8592300
(comment (add-hook 'sql-mode-hook (lambda ()
                                    ;; TODO: This is apparently a
                                    ;; minor mode as of emacs 28. What
                                    ;; does that mean for us?
                                    (setq indent-tabs-mode t)
                                    (setq sqlind-basic-offset 8))))
;; Let's just do indentation ourselves
(setq sql-use-indent-support nil)
(comment
 (with-eval-after-load "sql"
   (load "~/.emacs.d/tsql-indent")
   (load "~/.emacs.d/tsql"))

 )

;; TODO: Sometimes when exploring code I manually build out the tree
;; of execution but I still haven't developed a good method of doing
;; that. I'd like to perfect that a bit or at least get more of a
;; process going with it. Some ideas. Use org mode headers. Only link
;; to files at "function calls" i.e. places that add to the stack.
;; What do I do if I want to document something about the function
;; call itself? Like, maybe the function does a lot of setup and I
;; want to explain that a bit before linking to other function calls.
;; Always go increase the indentation for nested calls. Do I add
;; documentation about the function being called next to the actual
;; function definition or where a caller calls out to it. Probably the
;; former right? Does this mean I should really define like flat a
;; list of functions and what they do and then just link out to those
;; definitions when constructing my manual call stack? Could I also
;; have the option of visualizing this as a graph? Heck, at this point
;; should I just be taking notes in elisp? Or could there be a benefit
;; to doing it in org mode and then parsing it somehow? I think I
;; would be curious to try both actually.

;; TODO: Is this something cool I should learn about?
;; https://www.reddit.com/r/emacs/comments/kxdr7x/programming_proving_orgmode_lists_as_proof_trees/

;; TODO: Is there a way to do a ripgrep in all my open buffers?
;; There's a query I'm searching for and I remember part of it and
;; know I have it open but can't remember where it is.

;; TODO: Sometimes I feel like I should have a separate sort jump list
;; for when I'm scrolling around files so I can just scroll around but
;; then go right back to where I was before.

;; TODO: I think it could be useful to be able to just pull a list of
;; questions I have out of org mode documents. Then I could just write
;; questions as I have them but not lose them.

(defun lag13-orderless-flex-if-twiddle (pattern _index _total)
  (cond
   ((string-prefix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 1)))
   ((string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1)))))

(defun lag13-orderless-without-if-bang (pattern _index _total)
  (cond
   ((equal "!" pattern)
    '(orderless-literal . ""))
   ((string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1)))))

;; TODO: With this orderless stuff I think it could be fun to add a
;; customization to allow us to do one of the "orderless-initialism"
;; matching style so it's super quick to complete some emacs specific
;; stuff. I feel like I'd only want it for emacs things (functions,
;; variables, commands) so maybe we can modify the functions which
;; start completions for those candidates? Or I could have a generic
;; character (like how we have '~' and '!) for triggering this
;; behavior.
(setq orderless-matching-styles '(orderless-regexp)
      orderless-style-dispatchers '(lag13-orderless-flex-if-twiddle
                                    lag13-orderless-without-if-bang))


;; TODO: My markdown files have that visual line wrapping thing which
;; I like but I don't think I want the indication that the lines are
;; wrapping on the right side of the window. I've already got it on
;; the left side, why do I need more visual noise.

;; TODO: Why can't I have nice wrapping in org mode like I have with
;; markdown but it just knows not to wrap on things like tables? Seems
;; like that should just work. Or maybe I just need to get something
;; which aggressively does M-q for me as I type and not care about it.

;; TODO: I think I'd like a command which like * but it goes to the
;; first occurrence of the search term in the file. A little cleaner
;; than what I do now namely *, gg, n. Not the worst but it adds an
;; extra thing to the jumplist which is annoying.

;; TODO: One time I wanted to look at a bunch of files with a similar
;; name so I feel like I want to type out completions until I the
;; files I want and then just open them all. How can I do that? I
;; think embark might be able to help.

;; TODO: I feel like I sometimes want to be able to bring up the
;; completion thing I typed rather the thing that was completed on in
;; the history. How can I do that? I guess my use case is that
;; sometimes a set of files look similar and I want to look through
;; them all so I want to keep brining up that same set of completions.
;; Maybe there's a better way to do this like using embark. I was
;; curious though.

;; TODO: I want to get my keybindings consistent for selecting some
;; item from a menu but opening it in a new buffer. ibuffer and dired
;; is my only use case at the moment. ibuffer uses C-o (and I think
;; also doesn't move the cursor to the other window?) and dired uses
;; S-SPC. I feel like it could be cool to have a "go to next
;; file/buffer" too like how you can iterate through ripgrep stuff.
;; Basically my use cas is that I wanted to look at a set of files for
;; some string. As I say that though maybe I should be leaning more
;; heavily on things like trying to get ripgrep working on a set of
;; files. I think ibuffer let's you just do plain searches too, I
;; wonder what that looks like. I feel like the bindings are:
;; - RET - open the file/buffer under cursor and replace the window
;; - C-RET - open in another window but maintain CONTROL and the cursor stays in the window
;; - S-RET - open in another window and SHIFT the cursor to the other window
;;
;; I also had a thought that, should those bindings ^^ change
;; depending on the kind of window it is? Like for org-roam, that
;; "links" buffer I have configured to open up small so I don't think
;; I'd ever want to have the buffer I select open up in that window.
;; More thought is needed. Another option too is to just have
;; universal arguments control the kind of behavior that goes on with
;; these things. Whatever I do for the universal argument for this
;; could then apply to other window things too like when I lookup help
;; documentation or open the messages buffer. These are the situations
;; I think of where I open a split window:
;;
;; - Executing a command that opens dedicated window (like messages)
;; - Starting a completion to open another buffer
;; - Selecting a buffer to open from a displayed list

;; TODO: I feel like I want to get better about being able to pull
;; "important" information out of org files. Like, I feel like I end
;; up writing a lot of prose and then sometimes the more important
;; things get lost in the shuffle. Maybe that means I should just
;; write less prose... but I think it would be cool to be able to say
;; "give me all the links in this file and the sentence preceding it
;; (assuming that that preceding sentence describes what the link
;; does). Or "get me all the source code blocks in this file. I don't
;; know... again, maybe I just need to get rid of my fluffy prose and
;; just keep the essential commands, links, queries, etc...

;; TODO: This seems like it could be a good file tree:
;; https://github.com/jojojames/dired-sidebar. I'm curious about all
;; the things they talk about on this thread too:
;; https://www.reddit.com/r/emacs/comments/rm8hl1/dirvish_a_minimalistic_file_manager_based_on/
;; I never really have used dired much, would using it help in some
;; way? I guess I just don't really know what I'd use it for even? I
;; guess I feel like all I've really used dired for is renaming files
;; because it renames the file and the buffer. I don't know if I feel
;; like I need a file explorer, really I think I'd just like
;; https://github.com/tpope/vim-eunuch but for emacs. Or maybe I
;; should change my point of view and embrace dired? On a similar sort
;; of note, I feel like I should be able to right click on the current
;; file name in the mode line and copy it. It just feels like it
;; should be possible. It seems to be possible with the centaur tabs,
;; why not the modeline? It feels like you should be able to rename
;; the file from there too. Just contextually I feel like this should
;; all be possible. Or maybe dired is really the correct textual
;; scenario? After all, what is in the mode line is just a buffer name
;; I think, not the file name. Looks like VScode behaves like this too
;; actually, you can right click the tab and do some things but only
;; right clicking on the file explorer is where you can actually
;; rename a file. I guess I could get behind this but I also guess I
;; sometimes see my list of completion options as kind of a contextual
;; list of things I should be able to act properly on. Maybe this
;; could help with the buffer name thing:
;; https://emacs.stackexchange.com/questions/10779/how-can-i-display-a-list-of-all-buffers-when-clicking-on-the-buffer-name

;; TODO: I might want to install this:
;; https://www.emacswiki.org/emacs/download/dired%2b.el to be able to
;; do a grep in just the files that appear in a dired buffer. It's not
;; on melpa, just gotta download it from the emacswiki:
;; https://emacs.stackexchange.com/questions/38553/dired-missing-from-melpa

;; TODO: Modify embark so it can copy the buffer name that you are
;; trying to switch to. This is useful for documentation where I want
;; to reference other scripts by name. The alternative that I see is
;; switching to the buffer, dired-jump, switch back and this seems a
;; little cleaner. Similarly I think I'd like to have the option to
;; copy the full file path for the buffer.

;; TODO: I feel like page-wise movements with C-f are too much,
;; scrolling line by line is much better because the code stays on the
;; screen longer thus you have more time to recognize the "shape" of
;; certain bits of code you are interested in. Just rambling here
;; because I knew the code I was interested in was "nearby"

;; TODO: In my org roam usage I've sometimes created pages who's main
;; purpose is to be a page for others to link back to. Kind of a way
;; to categorize things. Not sure if it's useful but that's another
;; story. For example I have a "sql queries" page and usually any
;; other page which adds a sql query I'll add a link back to that "sql
;; queries" page. I'm still kind of curiouse if this is a good
;; approach, I'm also curious about if putting the links at the top or
;; just kind of scattering them around the page is better. I like to
;; be able to just look at the top of the page and see what links are
;; there but at the same time should I just add links adjacent to
;; where I'm writing things? Even if there are multiple links from
;; A->B I think there's still just one actual link so there's no
;; cluttering. I was also wondering if I should add links to the "sql
;; queries" pages whenever I add a query but I'm not sure if that's a
;; good idea or maybe it's a moot point. ANYWAY. I thought it could be
;; cool if I could somehow SHOW all sql queries on that page. Like, is
;; there a way to create some sort of "view" in org mode? Because it
;; feels like it could be neat to find all linked pages, grap all
;; "sql" source blocks and display them. Then I could get a high level
;; view of sql stuff I know. Maybe something like:
;; https://github.com/nobiot/org-transclusion

;; TODO: How could I run sql queries from emacs. I'm just curious.

;; TODO: Might be a good blog about org-roam and org mode:
;; https://d12frosted.io/posts/2020-06-23-task-management-with-roam-vol1.html

;; TODO: Seems interesting:
;; https://org-roam.discourse.group/t/implementing-hierarchies-namespaces-in-org-roam-v2/1504/10
;; He talks about https://www.dendron.so/about which seems cool too.

;; TODO: Embark open up a file in another program. For example I
;; wanted to open a CSV in excel or whatever.
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file

;; ;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
;; (defun rename-file-and-buffer (new-name)
;;   "Renames both current buffer and file it's visiting to NEW-NAME."
;;   (interactive "sNew name: ")
;;   (let ((name (buffer-name))
;;         (filename (buffer-file-name)))
;;     (if (not filename)
;;         (message "Buffer '%s' is not visiting a file!" name)
;;       (if (get-buffer new-name)
;;           (message "A buffer named '%s' already exists!" new-name)
;;         (progn
;;           (rename-file filename new-name 1)
;;           (rename-buffer new-name)
;;           (set-visited-file-name new-name)
;;           (set-buffer-modified-p nil))))))

(defun lag13-rename-buffer-and-file-fn (buf newname)
  "Function to rename a buffer and it's corresponding file. It
will just rename the buffer if there is no corresponding file."
  (with-current-buffer buf
    (when (buffer-file-name)
      (rename-file (buffer-file-name) newname 1))
    (set-visited-file-name newname)))

(defun lag13-rename-buffer-and-file (buf newname)
  "Renames a buffer and it's corresponding file if it has one.

Writing this function was a slight breakthrough in me
understanding how the interactive special form works. Basically,
the logic for when emacs executes a command is that it will look
for this interactive special form and evaulate it. What is
returned is just a list of arguments which emacs passes to the
function itself. So in many ways \"interactive\" is a completely
separate thing from the function execution. It's executed by the
emacs command framework/infrastructure logic to get arguments and
then passes in those arguments to the function. It also does
double duty of marking a function as a command. I learned this by
reading the docs if you can believe it:
https://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Interactive.html"
  (interactive
   (let* ((presorted-completions (seq-map #'buffer-name (buffer-list)))
          (completion-table
           (lambda (string pred action)
             (if (eq action 'metadata)
                 '(metadata (category . buffer)
                            (display-sort-function . identity)
                            (cycle-sort-function . identity))
               (complete-with-action action presorted-completions string pred))))
          (buf (completing-read "Buffer: " completion-table))
          (new-buf-name (read-string "New Name: " buf)))
     (list buf new-buf-name)))
  (lag13-rename-buffer-and-file-fn buf newname))

;; TODO: In this talk
;; https://www.youtube.com/watch?v=oJTwQvgfgMM&ab_channel=GoogleTechTalks
;; the guy talked about how org mode is his default mode for any
;; buffer I think I'd like to do that too actually because I feel like
;; sometimes I'll open up an empty buffer to just jot some random
;; thing down that I don't even want to remember later and it could be
;; nice if it was org mode. That was a very good talk too by the way.
;; It's cool to see the origins of org mode.

;; TODO: I think I might have already written something down about
;; some org mode addon to do a kind of "flashcard quiz" style thing
;; with stuff you've written. I think I want that for org roam nodes
;; too where it showed you nodes that you have not visited in a while.
;; Maybe it could even prioritize nodes that have fewer connections or
;; something. Because a couple of times now I've forgotten about a
;; node which could have helped me.

(defun lag13-get-file-path-relative-to-project ()
  (lag13-s-remove-common-prefix (buffer-file-name) (cdr (project-current))))

;; TODO: I feel like I should be able to invoke this within a dired
;; buffer (like make it an embark target or whatever). I also feel
;; like I should be able to invoke this via embark-act in the
;; minibuffer. It seems like I'm wanting a bunch of little entrypoints
;; to this functionality aren't I? I feel like I need to get this
;; stuff straight in my head so I remember that it exists AND that it
;; works in every context that "makes sense" to my mind so there is
;; less friction. I notice that there is a function
;; "embark-save-relative-path" which is accessible when invoking
;; embark-act in a dired buffer. For the example of getting a file
;; path of a buffer relative to the project root I think there could
;; be 3 contexts: 1. Invoke the command on it's own, it will bring up
;; a buffer prompt with the default being the current buffer. Part of
;; me is curious if this use case should be replaced by just having
;; the command work on the current buffer without a prompt because we
;; can effectively get the prompt via embark. Hmmm. 2. Invoke
;; embark-act a buffer in ibuffer (I assume we could have this same
;; command work for dired too) 3. Invoke the command via embark-act.
(defun lag13-copy-file-path-relative-to-project ()
  (interactive)
  (let ((path (lag13-s-remove-common-prefix (buffer-file-name) (cdr (project-current)))))
    (kill-new
     (if (seq-contains-p '(ms-dos windows-nt cygwin) system-type)
         (s-replace "/" "\\" path)
       path))))

(defun lag13-get-github-url ()
  (let ((ref (s-trim (shell-command-to-string "git rev-parse HEAD")))
        (url 
         (s-replace ".git" "" (s-replace "git@" "https://" (s-replace ":" "/" (s-trim (shell-command-to-string "git remote get-url origin"))))))
        (project-path-to-file (lag13-get-file-path-relative-to-project (buffer-file-name))))
    (kill-new (if (evil-visual-state-p)
                  (let ((start-line-number (line-number-at-pos (car (evil-visual-range))))
                        (end-line-number (line-number-at-pos
                                          (if (equal (evil-visual-type) 'line)
                                              (1- (cadr (evil-visual-range)))
                                            (cadr (evil-visual-range))))))
                    (if (= start-line-number end-line-number)
                        (format "%s/blob/%s/%s#L%d" url ref project-path-to-file start-line-number)
                      (format "%s/blob/%s/%s#L%d-L%d" url ref project-path-to-file start-line-number end-line-number)))
                (format "%s/blob/%s/%s" url ref project-path-to-file)))))

;; TODO: Could I perform a rg for something but then remove duplicates
;; to avoid clutter? More detail, if I'm looking for a link I
;; documented somewhere in my org-roam files I feel like it could be
;; cool to grep from "https://" but then remove any items from the
;; result which had the same link in them. This was just a random
;; thought I had, I don't actually have a use case for this so it
;; could be pointless. I guess we would mess with one of those filter
;; function things that sit on the other side of a process though? It
;; could look at every item that comes in and keep track of the urls
;; it sees and toss out any duplicates.

;; TODO: This seems cool: https://github.com/storax/graph.el. It
;; hasn't been maintained in 5 years though, I wonder if it still
;; works. Woaaa!!! This seems even cooler:
;; https://github.com/storax/graph.el/issues/1 (which is actually
;; https://www.reddit.com/r/emacs/comments/eyuehj/orggraphviewgraphviz_interactive_visual_org/)

;; TODO: Oh interesting, there's a package in emacs for writing
;; screenplays! https://github.com/rnkn/fountain-mode. Stumbled on
;; this when I was searching for a way to automatically tangle source
;; code blocks:
;; https://emacs.stackexchange.com/questions/12889/syncing-changes-on-a-tangled-file-back-to-the-original-org-file
;; The package https://github.com/phillord/lentic also came up and it
;; seemed pretty neat.

;; TODO: The org babel tangle feature seems super nifty (i.e. writing
;; source code blocks to files) because then you can do your literate
;; programming within emacs but still export artifacts in case that is
;; needed. I think manually exporting that data seems silly though,
;; why can't it automatically export everytime you edit that source
;; block? Also, I suspsect that this could be useful for doing the
;; reverse: https://gitlab.com/mtekman/org-tanglesync.el. Maybe
;; https://emacs.stackexchange.com/questions/45182/exit-hook-for-org-src-mode?
;; Or maybe https://github.com/yilkalargaw/org-auto-tangle

;; TODO: Org mode's TODO gather feature, although I have not used it,
;; seems pretty cool because then you can write little todo's right in
;; the context of where it came up. But you need to have a heading to
;; do that right? And doesn't that break the flow of writing? What if
;; you are in some "Summary" heading and you want to create a TODO but
;; can't because then everything after that TODO heading will exist
;; underneath it. Is there a way to write inline sort of TODOs that
;; can later get gathered?

;; TODO: https://orgmode.org/worg/org-faq.html and
;; https://www.reddit.com/r/orgmode/comments/tptwum/i_created_an_in_depth_guide_for_new_org_mode/
;; for some potential org mode reading.

;; TODO: I made a very wide table in org mode and needed to
;; horizontally scroll to see all of it. On first pass, horizontal
;; scrolling in emacs seems abysmal or at least the defaults seem to
;; be. Why can I not just scroll side to side with the mouse? Why does
;; C-x < prevent the cursor from going back to the true beginning of
;; the line? Why do your defaults seem so bad Emacs... Figure this
;; out.

;; TODO: Would it be possible to qualify what kind of relationship two
;; nodes have? I was picturing using org-roam as kind of a hacky way
;; to map out a db relationship diagram but to really do that I think
;; I would need to be able to somehow say: "this edge in this
;; direction represents a 1 to many relationship". Just seems like it
;; would be fun to do seeing as how org-roam already offers graphing
;; capabilities based on it's org-roam-ui-mode. This discussion seems
;; semi-promising
;; https://www.reddit.com/r/emacs/comments/ljms7v/does_orgroam_support_different_link_types/gnh57qs/
;; I also notice that there is a "type" and "properties" columns on
;; the "links" table. The above reddit thing seems to be related to
;; the type which is an org mode concept of "what type of link this
;; is". The properties thing seems to always be
;;
;; (list :outline (org-get-outline-path 'with-self 'use-cache))
;;
;; Where that function returns a list of headers that the current text
;; falls on or within. I suppose I could even hack that to work by
;; having a special header called "relationship type" or something I
;; know to look for. Not sure how I'd change the UI portion though
;; which I feel like is definitely something I'd like to do.

;; TODO: I feel like I should start thinking about a "leader" key to
;; do start doing a completion and same for things like switching
;; buffers. I sort of have one for window management (C-w) but it
;; doesn't work in insert mode so that should be fixed. All in all I
;; think I need better organization of my keybindings.

;; TODO: I think I want this mostly just as a cool visual way to see
;; my knowledge graph and show friends but I've consistently had
;; trouble with this on windows so I'm just not touching it. It seems
;; that even if I actively deactivate the mode during startup, it
;; throws an error (something weird like the *scratch* buffer doesn't
;; have a process). Maybe I'm just holding it wrong.
;;
;; (org-roam-ui-mode 0)
;; https://github.com/org-roam/org-roam-ui/issues/213
;; (setq org-roam-ui-open-on-start nil)

;; TODO: Whenever I search I feel like it should show the number of
;; search results and also maybe which search item I'm currently on?
;; It seems silly that this does not exist. I feel like these days I
;; want more context in all situations. If it is not helpful fine,
;; then I ignore it but if it is helpful wonderful! I don't have to go
;; hunting for it, I can just see it.

;; TODO: Again, going back to the code spelunking stuff. Where I'm at
;; currently is still that it would be nice to have a perspective like
;; thing but that is a little less rigid namely that I can still
;; access ALL files/buffers when in the perspective and when I open a
;; file/buffer it doesn't automatically get added to the current
;; perspective, rather, I have to explicitly add it. Anyway. I also
;; wonder if it could be cool to build out a little tree relationship
;; between files and how they relate? Like if you open file A from
;; file B with some special keybinding could it make a parent child
;; relationship between them? I was also wondering about a situation
;; where you could get a set of strings (which are probably functions
;; to be called) and a set of files containing those functions and it
;; would build out a little call tree for you because it would look
;; for those function names in those files and show the relationship
;; between files. Again, especially for code bases with super nested
;; function calls I want to get better about seeing new code and
;; understanding it quickly. Maybe I just need to be more rigourous
;; about using some diagramming tool too (graphviz or plantuml?) but I
;; also like the idea of trying to automate it a bit somehow.

;; TODO: Maybe has some good stuff?
;; https://zeekat.nl/articles/making-emacs-work-for-me.html#sec-10-6

(setq-default ediff-ignore-similar-regions t)
(setq-default ediff-ignore-case t)

;; TODO: I learned about flymake mode recently that I can use to jump
;; to errors automatically. I feel like it would be cool to have
;; keybindings for flymake-goto-next-error and the like? I'd like to
;; make sure that the keybindings are consistent with other
;; functionality too.

;; TODO: I notice that if I'm on a line with an org mode link with a
;; description then evil-last-non-blank doesn't seem to work properly.
;; evil-end-of-line works just fine though. On a related note, it
;; seems that having links messes up org-fill-paragraph

;; TODO: I think it would be interesting if we had an alternative to
;; the 'n/N' commands which "slowly" scroll the window to get to the
;; next match to help give better awareness about how close some code
;; is to other code. I think I've written stuff like this before.
;; Basically it's happened before where I've been working on code and
;; trying to jump back and forth only to realize that the to code
;; blocks were pretty much right next to eachother but I never
;; realized because I usually don't scroll.

;; TODO: Could we mark files in the rg buffer that are currently being
;; visited? I think that could be useful so we don't revisit files
;; we've already looked at.

;; TODO: I made a macro one time which used undo in it and when I
;; tried to execute it on multiple visual lines it completely froze
;; emacs. Not sure if undo was the culprit but maybe we test this out
;; again sometime.

;; TODO: How can I tell rg to grep in all files EXCEPT files mathing
;; some sort of regex.

;; TODO: I think I should allow the evil cursor to travel one
;; character past the last character on the line. Without something
;; like that we could never have the option to highlight from the end
;; of the line to a semicolon several lines down and have it snap to
;; the end of the line.

;; TODO: I feel like I could learn a lot from this guy:
;; https://mullikine.github.io/posts/universal-antlr-parser-in-emacs/

;; TODO: I wonder if I could learn anything from this post
;; https://www.reddit.com/r/emacs/comments/fmxwd6/gnu_emacs_as_a_lightweight_sql_ide/

;; TODO: context-menu-mode was added in emacs 28. I wonder if we could
;; add generic buffer things there like renaming the buffer and file
;; or copying the full path to the buffer.

;; TODO: As of emacs 28 there's a standalone M-y command which allows
;; for interactive selection from previous kills. I think I'd like
;; something like that, it would make registers mostly obsolete. Evil
;; mode has it's own binding though so I'd need to mess with that.

;; TODO: eshell-hist-ignoredups is a thing in emacs 28

;; TODO: Woa there's a function called group-function in emacs 28 for
;; grouping candidates. Just seems interesting. I feel like consult
;; related functions would do something like this. There's also a
;; completion function affixation-function. They really seemed to have
;; beefed up completion stuff in emacs 28. Feels like marginallia kind
;; of stuff.

;; TODO: Feels like org mode should have a better return key
;; functionality that is more similar to other editors:
;; https://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/

;; TODO: Is there a convenient way to see what files are part of the
;; current git commit and visit them? Sometimes I've needed to go back
;; to a branch and update things and it would be cool to have a view
;; where you could switch between JUST the buffers that have been
;; commited (or changed for instance!). Huh, yeah, that would be very
;; cool actually. Could be another way to sort of keep awareness about
;; what files you're working on using information that is already out
;; there and doesn't have to be separately maintained.

;; TODO: I'd be curious to learn how this guy uses emacs for writing:
;; https://www.youtube.com/watch?v=FtieBc3KptU&ab_channel=thoughtbot I
;; think his dotfiles would have a lot of neat stuff in them:
;; https://github.com/incandescentman

;; TODO: In org-roam is there a way I could do a mass change of tag
;; names from one thing to another thing? Like I wanted to change the
;; tags: "status;in-progress" to "status;ongoing" for a couple files.

;; TODO: I feel like I should be able to google from within emacs! It
;; could just launch a browser or keep the results in emacs. I think
;; "gnugol" is a command but I don't have it.

;; TODO: I should have the command browse-url-at-point have an option
;; to just open the url but keep focus on emacs.

;; TODO: Mess around more with literate programming:
;; https://www.youtube.com/watch?v=dljNabciEGg&ab_channel=HowardAbrams

;; TODO: Read the book "getting things done" and learn org mode stuff
;; from this video:
;; https://www.youtube.com/watch?v=SzA2YODtgK4&ab_channel=thoughtbot
;; Look into "owncloud" too ^^

;; TODO: If that rg plugin has the capability of going to the previous
;; search that was executed then surely we could also search for that
;; information right? Like do a little completion search?

;; TODO:
;; https://emacs.stackexchange.com/questions/35076/graph-with-clickable-nodes
;; google "emacs graph traversal"
;; https://www.reddit.com/r/emacs/comments/2tbpok/orgmode_as_a_node_graph/

;; TODO: Woa... what is this!
;; https://www.reddit.com/r/emacs/comments/67lkso/emacs_major_mode_for_editing_knowledge_graphs/
;; "Semantic Synchrony" its called:
;; https://github.com/synchrony/smsn-why/blob/master/invitations/to-coders.md
;; Another knowledge graph thing like org-roam. But they talk about
;; graph traversal a bit too. Just seems interesting. Seems
;; unmaintained though? https://github.com/synchrony/smsn. Or maybe
;; it's just done.

(defun lag13-test-completion ()
  "Playing around with emacs' built in completion framework:
https://www.gnu.org/software/emacs/manual/html_node/elisp/Programmed-Completion.html"
  (let* ((presorted-completions (seq-map #'buffer-name (buffer-list)))
         (completion-table
          (lambda (string pred action)
            (if (eq action 'metadata)
                '(metadata (category . buffer)
                           (annotation-function . (lambda (completion) "annotation function here!"))
                           ;; (affixation-function . (lambda (completion) (list "prefix" )))
                           (group-function . (lambda (completion transform)
                                               (if transform
                                                   completion
                                                 (if (buffer-file-name (get-buffer completion))
                                                     "File buffer"
                                                   "Process buffer"))))
                           (display-sort-function . identity)
                           (cycle-sort-function . identity)
                           )
              (complete-with-action action presorted-completions string pred))))
         (buf (completing-read "Buffer: " completion-table)))
    (message buf)
    (lag13-test-completion)))

(defun lag13-test-completion ()
  (let* ((completion-candidates
          (seq-map-indexed (lambda (elt index) (cons elt index))
                           '("hey" "you" "there")))
         (completion-table
          (lambda (string pred action)
            (if (eq action 'metadata)
                '(metadata
                  (affixation-function . (lambda (completions) (seq-map (lambda (completion)  (list (s-left 2 completion) "prefix " " suffix")) completions)))
                  )
              (complete-with-action action completion-candidates string pred)))))
    (completing-read "hello " completion-table)))

(use-package consult
  :straight t
  :ensure t
  :bind (("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         )
  :init
  (define-key evil-motion-state-map (kbd "SPC") #'consult-buffer))

(defun lag13-org-roam-find-node-based-on-neighbors (&optional selected-neighboring-nodes)
  "Selects a node based on a shared set of neighbors.

Stuff we could do to improve:

Figure out what we want for history with this thing. At the
moment I'm not even sure how the order of this stuff is
happening.

I think I do want to add another neighbor node sort of selection.
I was picturing that I might want to do something like:
\"find all the restaurants I've been to with person XYZ\"

I picture that the relationship between person XYZ and a
restaurant might not be direct, it'll probably be a situation
where I write in my journal \"hey, I went to this restaurant with
XYZ\"

In that case then I think the imperfect way of narrowing this
down would be something like:
1. Select the \"journal\" category which links to all journal entries
2. Select the XYZ person so we'll just have journal entries where I've interacted with that person
3. Select the \"restaurant\" category so we get all restaurants

That 3rd bullet is where I need to make the modification. Because
right now (1) and (2) are just the usual intersection of
neighbors thing but with (3) we're basically taking the union of
the neighbors of all restaurant nodes and intersecting that with
what we've produced so far.

At that point part of me starts to wonder if I should just be
writing sql queries instead? I think I like the idea of being
able to do this interactively though. I should probably figure
out what that sql query would look like though.
"
  (interactive)
  ;; get the node ids from the passed in list of selected neighboring
  ;; nodes because my functions work off of the org-roam-node-id for
  ;; better or worse.
  (let* ((selected-neighboring-nodes-ids (seq-map #'org-roam-node-id selected-neighboring-nodes))
         ;; this is the filter function which, when plugged into
         ;; org-roam-node-read--completions, will return all possible
         ;; nodes we can visit.
         (all-possible-nodes-to-visit-filter-fn
          (let ((possible-nodes
                 (-reduce
                  (lambda (&optional set1 set2)
                    (if (and (null set1)
                             (null set2))
                        nil
                      (if (null set2)
                          set1
                        (seq-intersection set1 set2))))
                  (->> selected-neighboring-nodes-ids
                       (seq-map #'lag13-org-roam-get-node-neighbors)))))
            (lambda (node)
              (seq-contains possible-nodes (org-roam-node-id node)))))
         ;; Get all possible nodes that we could visit
         (all-possible-nodes-to-visit
          (org-roam-node-read--completions
           all-possible-nodes-to-visit-filter-fn))
         ;; this is the filter function which, when plugged into
         ;; org-roam-node-read--completions, will return all possible
         ;; neighbor nodes which are possible to select based on the
         ;; other neighbors which have already been selected.
         (all-possible-neighbor-nodes-filter-fn
          (let ((possible-nodes
                 (->> all-possible-nodes-to-visit
                      (seq-map #'cdr)
                      (seq-map #'org-roam-node-id)
                      (seq-mapcat #'lag13-org-roam-get-node-neighbors)
                      (seq-uniq)
                      (seq-remove (lambda (node-id) (seq-contains selected-neighboring-nodes-ids node-id))))))
            (lambda (node)
              (if possible-nodes
                  (seq-contains possible-nodes (org-roam-node-id node))
                (lag13-org-roam-get-node-neighbors (org-roam-node-id node))))))
         ;; Get all possible neighbor nodes left to select. The only
         ;; neighbor nodes that can be selected are ones which share
         ;; common neighbors with the other selected neighbor nodes
         ;; AND we have not already selected it.
         (all-possible-neighbor-nodes-to-select
          (org-roam-node-read--completions
           all-possible-neighbor-nodes-filter-fn))
         ;; The list of nodes that have been selected transformed into
         ;; an alist which will work with the completing-read thing
         ;; we've got going on.
         (selected-neighbor-nodes-items
          (org-roam-node-read--completions
           (lambda (org-roam-node)
             (seq-contains selected-neighboring-nodes org-roam-node)))))
    (consult-buffer
     (list (list
            :name "Node to visit"
            :narrow ?v
            :default t
            :category 'org-roam-node
            :action (lambda (node)
                      (org-roam-node-visit (cdr (assoc node all-possible-nodes-to-visit))))
            :items (lambda ()
                     (seq-map #'car all-possible-nodes-to-visit)))
           (list
            :name "Neighboring Nodes"
            :narrow ?n
            :category 'org-roam-node
            :action (lambda (selected-neighbor-node)
                      (lag13-org-roam-find-node-based-on-neighbors
                       (cons (cdr (assoc selected-neighbor-node all-possible-neighbor-nodes-to-select))
                             selected-neighboring-nodes)))
            :items (lambda ()
                     (seq-map #'car all-possible-neighbor-nodes-to-select)))
           ;; Allows one to unselect a selected neighboring node
           (list
            :name "Selected Neighboring Nodes"
            :narrow ?s
            :category 'org-roam-node
            :action (lambda (selected-neighboring-node)
                      (lag13-org-roam-find-node-based-on-neighbors
                       (seq-remove-first (lambda (neighboring-org-roam-node)
                                           (equal (org-roam-node-id (cdr (assoc selected-neighboring-node selected-neighbor-nodes-items)))
                                                  (org-roam-node-id neighboring-org-roam-node)))
                                         selected-neighboring-nodes)))
            :items (lambda ()
                     (seq-map #'car selected-neighbor-nodes-items)))))))

(defun lag13-org-roam-perform-action-on-selected-nodes (fn)
  "Runs an arbitrary function FN within the context of a node's
file for selected nodes. Original use case was for quickly adding
tags to a list of selected nodes which I wanted to do after I had
already accumulated a bunch of nodes and realized that maybe it
would be useful to have certain groupings of things."
  (let* ((node-completions (org-roam-node-read--completions))
         (selected-nodes (completing-read-multiple
                          "nodes:"
                          node-completions)))
    (dolist (selected-node selected-nodes)
      (org-roam-with-file
          (org-roam-node-file (alist-get selected-node node-completions nil nil #'equal))
          nil
        (funcall fn)))))

(defun lag13-org-roam-add-tags-to-nodes (tags)
  "Adds a list of tags TAGS to the selected nodes."
  (interactive
   (list (completing-read-multiple "Tag: " (org-roam-tag-completions))))
  (let ((fn (lambda () (org-roam-tag-add tags))))
    (lag13-org-roam-perform-action-on-selected-nodes fn)))

;; TODO: Great talk: William Byrd on "The Most Beautiful Program Ever
;; Written" [PWL NYC]
;; https://www.youtube.com/watch?v=OyfBQmvr2Hc&ab_channel=PapersWeLove
;; Also, read "cons should not evaluate its arguments" and "cons
;; should not cons its arguments". Also there's this fun tutorial:
;; https://eblong.com/zarf/zweb/lists/. Also
;; https://matt.might.net/articles/i-love-you-in-racket/ Honestly
;; there's a lot of nice offshoots from this talk.

(add-hook 'fsharp-mode-hook
          (lambda ()
            ;; Too many files don't have newlines in the repo I'm
            ;; working on so this just makes things noisy with it's
            ;; original value of "visit-save"
            (setq require-final-newline nil)))

;; TODO: It would be interesting if I could define "aliases" with my
;; completion where typing XYZ would also match ABC assuming that ABC
;; is an alias/synonym of XYZ. My use case was that I was trying to
;; lookup an org roam node but couldn't find it but I WOULD have found
;; it if one of the words was switched out to be an alias. Then I
;; wouldn't have to remember to put every alias for something on a
;; given node I create. Maybe this is the kind of thing I'd only need
;; to enable when searching org roam nodes. The node in this case was
;; "Investigation W-10668021: Slow creation/update of DE fields" and I
;; needed "DE" to be an alias for "custom object".

;; TODO: Get a better understanding of the org-roam-ref stuff. I
;; notice that if I do org-roam-ref-find, the leading "https:" is
;; missing. Not that it matters much since I don't use it but it makes
;; me think I'm not using it properly. Also I think it would be cool
;; if when a node represents a link like a work item, that there could
;; be a command to just open up that work item in a browser.

(require 'engine-mode)
(engine-mode t)

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "G")

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(defengine ecosia
  "https://www.ecosia.org/search?q=%s"
  :keybinding "e")

;; TODO: When completing on org roam nodes, I think it should omit the
;; current file from possible selection. No sense in switching to a
;; buffer that you're already visiting! Really I think it should order
;; them by whenever the file was visited last REGARDLESS of how it was
;; visited. Like if we just do a buffer switch to a file, that should
;; count as having switched to that file. I guess we'd have to modify
;; the history of the org roam file switching command or something.

(setq org-agenda-files (list org-roam-directory))
(setq org-archive-location (concat org-roam-directory "archive/archive.org::datetree/"))
;; I figure it could be nice to save as much context as possible so I
;; listed everything
(setq org-archive-save-context-info '(time file ltags itags todo category olpath))
(setq org-enforce-todo-dependencies t)
(setq org-log-into-drawer "LOGBOOK")
;; I like the idea of having a state to represent work that I'm
;; actively working on. I'm calling it "ACHIEVING" because it feels
;; positive and because the first two letters don't match any other
;; todo states which is handy when finding a node via
;; org-roam-node-find. I feel like it'll be handy for me to have this
;; extra state because I tend to accumulate a LOT of TODOs and I'm
;; working on could get lost in the mix. Ideally I should remember
;; what I'm working on and I bet most of the time I will but there are
;; definitely moments where you get totally torn away from something
;; and forget to come back to it at all. I suppose that if you forget
;; to come back it, that could be an indication that it is not really
;; important but on the flip side, very few things I do are truly
;; important. Relatively speaking the only things that are important
;; are life necessities
(setq org-todo-keywords
        '((sequence "TODO(t!)" "ACHIEVING(a!)" "|" "DONE(d@)")))
;; I know I'm already tracking todo state changes above so in some
;; ways this is redundant but I thought that it might be nice to get
;; the CLOSED date of the item so if I archive into a datetree then it
;; will get put at the spot when it was closed which might be neat if
;; I mark something as DONE and then forget to archive it.
(setq org-log-done 'note)

;; TODO: I was listening to some of the Johnny Depp court stuff and it
;; got me curious if emacs has a mode to emulate the kind of typing
;; that the folks do in court so they record everything. Stenographer
;; right? Isn't it supposed to be super fast to type or something? I
;; wonder how it works and if I could do it with emacs.

;; TODO: Check out the other org-modules. I first learned about this
;; variable when I learned about habit:
;; https://orgmode.org/manual/Tracking-your-habits.html and some of
;; the other options sounded interesting. Such as
;; https://www.emacswiki.org/emacs/OrgAnnotateFile

;; TODO: If g; takes me to a place of an org mode document that is
;; folded, the fold doesn't open automatically. I kind of feel like it
;; should though.

;; TODO: Can I use org mode to look for tasks I've recently completed?
;; I think that should be possible since I've started timestamping
;; them.

;; TODO: In org mode could we set something up where every time a file
;; is changed it adds a timestamp for that day indicating that a
;; change was made? My use case is I'm curious if it would be useful
;; to be able to see all the things I was working on during the
;; previous day or something like that. Maybe it would be possible to
;; search through just those files that were touched as well?

;; TODO: Cool!! https://github.com/Chobbes/org-chef

;; TODO: I was making a presentation of what I had been working on and
;; I wanted to define some terms up front (RFC style) and I thought it
;; would be cool if I could do that and then have those terms be
;; automatically highlighted whenever they appeared on the page to
;; draw attention to these small important bits. Words are important!

(setq org-tree-slide-slide-in-blank-lines 3)

;; TODO: On the README for org-tree-slide mode it mispelled the option
;; org-tree-slide-slide-in-brank-lines: "brank" -> "blank"

;; TODO: I learned that you can display images in org mode but ONLY
;; for images which are file links (you can't for example paste the
;; url to a file and expect that image to display). It looks like you
;; can write some code to do this though! I'd be curious to see how
;; this work:
;; https://emacs.stackexchange.com/questions/26613/is-it-possible-to-insert-images-from-the-web-with-its-url
;; Although apprently this should be possible by configuring
;; org-display-remote-inline-images... didn't work for me though.

;; TODO: I gave a demo in org mode about some code changes I made and
;; I thought it could be cool to have a section in my presentation
;; which documents all the files I've changed. I imagine giving a list
;; of GH commits and maybe it spits out two headers, one with a list
;; of file links to the files that got changed and maybe another with
;; the actual changes? Not sure. I guess I want to be able to be like
;; "hey team, these are the changes I've made". Maybe I just gather a
;; list of GH commit urls and call it a day?

(setq org-table-convert-region-max-lines 5000)

(setq org-ellipsis " ▾")

;; TODO: I feel like fairly often I'll have 2 sets of lines and I want
;; to turn them into a list with one item per line and intersect these
;; two sets. I guess it's not to hard to write the code but it feels
;; like it would be cool to make this faster somehow.

;; TODO: Checkout https://github.com/alphapapa/org-ql (mentioned on
;; https://www.youtube.com/watch?v=PNE-mgkZ6HM&ab_channel=SystemCrafters).
;; I'm imagining it can help with writing queries to pull back certain
;; data from org mode files which I imagine I'd like.

;; TODO: See how evil-auto-indent interacts with the rest of emacs's
;; indenting stuff. I think this must have been the setting that was
;; messing up 

;; TODO: Inserting dates with the org mode date time prompt seems
;; pretty cool:
;; https://orgmode.org/manual/The-date_002ftime-prompt.html. I wonder
;; if it could be configured to understand other strings like
;; "christmas" meaning the 25th or "easter" meaning whenever the heck
;; easter is.

;; TODO: I created bindings to insert dates C-c d and C-c t but I feel
;; like it would be cool to be able to just use org mode's date
;; inserting mechanisms EVERYWHERE. Because then you get the cool
;; features like being able to insert a date X days in the past or
;; something like that. It just gives more power. Speaking of that, is
;; there an easy way in this date completion UI to say "a specific day
;; earlier this year". I guess I can just type the year, no biggie.
;; Just curious since it always tries to go forward.

;; TODO: I learned that doing org-timer-set-timer will do a little
;; desktop popup thingy. I'd be curious to see how that works.

;; I thought it might be nice to have this piece of info
(setq org-treat-insert-todo-heading-as-state-change t)
;; I thought it would be nice to easily see habits regardless of the
;; day. Then I could show friends or something without having to find
;; this info somehow.
(setq org-habit-show-all-today t)
;; I wanted to see roughly a month's worth of habit
(setq org-habit-preceding-days 31)
(setq org-habit-graph-column 55)

;; TODO: Feels like it could be useful to jump to different links
;; within the same org mode file. Maybe even a consult style jumping
;; menu where you can see the context around the link as you try to
;; filter for one particular one.

;; TODO: Function to turn a table into a bulleted list for when I want
;; to copy data in a table to another system like jira. The first
;; column could be the top level list and then I start a sublist which
;; starts with the column name and then has the value.

;; TODO: Could I give temporary aliases for buffers? Sometimes when I
;; start working on something I feel like a might assign a "short
;; name" of it in my head and I keep referring to it as that. Like I
;; was basing a script I was writing off of another guy Doug's script
;; and in my head I kept wanting to say "switch to Doug's script" and
;; thought that would be neat if something like that could be done.
;; This desire to quickly switch to code related to what I'm working
;; on also probably falls within the "organize my buffers by the task
;; at hand" sort of desire.

;; TODO: How are the consult buffer previews so quick but actually
;; opening a buffer takes a second or two?

;; Trying out the capture feature and keeping it next to my org roam
;; files.
(setq org-default-notes-file (concat org-roam-directory "/capture.org"))
(setq org-capture-templates '(("c" "Generic capture (no structure)" entry (file "") "* %?")))
(defun lag13-org-capture (&optional goto)
  (interactive "P")
  (org-capture goto "c"))
;; So we start in insert mode
(add-hook 'org-capture-mode-hook 'evil-insert-state)
;; https://orgmode.org/manual/Activation.html
(global-set-key (kbd "C-c l") #'org-store-link)
(defun lag13-org-agenda ()
  "Calls `org-agenda' and sets the value of `org-agenda-files'
before doing so. Useful for me because at the moment my only
desired value of `org-agenda-files' is `org-roam-directory' and I
have a buffer local value of that so with this it will load a
different agenda based on the buffer local value of
`org-roam-directory'."
  (interactive)
  (setq org-agenda-files (list org-roam-directory))
  (org-agenda))
(global-set-key (kbd "C-c a") #'lag13-org-agenda)
;; At this moment in time I don't have any desire to setup multiple
;; capture templates. In my mind I only want to use the capture
;; feature to quickly jot something down so I remember it and then get
;; back to whatever and then, at a later point in time, move it to a
;; more proper location. As such I'm binding this to a more specific
;; keybinding to save a keystroke.
(global-set-key (kbd "C-c c") #'lag13-org-capture)
(defun lag13-org-capture-next-task (&optional goto)
  "I'm trying out a system where I have a dedicated file for
small'ish atomic'ish tasks that I immediately want to be working
on and I'll lean on the org-capture feature to accomplish this.

Use case is that I find myself in a situation (as of 2022-06-10)
where I have an \"ACHIEVING\" TODO state which I like because
then I can start the day and see what it is that I'm working on
at a high level. But sometimes those tasks are fairly large and
sometimes I have more than one happening at the same time so I
find it tough to think to myself \"atomically, what action should
I take next\". That's what I intend this file to be, a place
where I take small tasks and add them and then chug through them.
Probably archiving them as I go."
  (interactive "P")
  (let ((org-default-notes-file (concat org-roam-directory "/next-tasks.org")))
    (org-capture goto "c")))
(global-set-key (kbd "C-c n") #'lag13-org-capture-next-task)

(setq org-catch-invisible-edits 'show)

;; I don't think I actually use this and I just thought it was cool. I
;; think what I'd really be interested in is being able to start
;; entering some keys then hit C-h or whatever and have it open up a
;; minibuffer with commands that start with these keys.
;;
;; (which-key-mode)
;; (setq which-key-idle-delay 0.5)

;; TODO: I want to play around more with imenu mode more. It's really
;; handy for jumping around elisp files and I'd love it if I could get
;; that kind of functionality elsewhere too:
;; https://www.reddit.com/r/emacs/comments/ujuokl/my_take_on_dotemacsorg/

;; TODO: Ooooo org mode can do that embedding thing which I saw in
;; logseq: https://github.com/nobiot/org-transclusion

;; TODO: Stop using projectil in favor of the built in emacs project
;; stuff. Or at least look into if that is possible. Also, when doing
;; this I'd like the ability to be looking at a file in one project
;; and start trying to find a file in a different project. Currently I
;; have it configured to only looks for a buffer in a different
;; project. Could use a capital letter B and F to indicate buffer or
;; file from a different project.

;; TODO:
;; https://www.youtube.com/watch?v=VXL_2S86g2E&ab_channel=JeffreyWebber
;; on that video he mentions that logseq can be used to annotate PDFs
;; where you can even highlight sections and jump back to them. I
;; think that would be super cool to have in org mode because then I
;; could be reading a textbook or something and be able to quickly
;; jump to bits I found interesting.

;; TODO: Categorizations of buffers/files that I want to be able to
;; work with: Maybe C-j is how I'll control "jumping" to different buffers
;; 1. All buffers
;; 1. Different categorizations of special buffers (like maybe shell buffers or a repl?)
;; 1. All buffers belonging to a particular repo (and not). Maybe just file buffers corresponding to a repo as well.
;; 1. All files belonging to a particular repo
;; 1. All files recursively below a particular directory (like project search but in any arbitrary directory)
;; 1. An arbitrarily user defined collection of buffers. Like I could have a list of buffers that correspond to a feature that I work on. (and not)
;; 1. My org roam notes
;; 1. Recently opened files I guess? I need to learn about that feature more
;; 1. "Alternate" files/buffers (like a test file corresponding to the current file)
;; 1. A buffer that exists within the same directory as the current buffer
;; 1. A not yet opened file in the same directory as the current buffer
;; 1. A buffer that is recursively contained by the directory of the current buffer
;; 1. "Special" files. Like maybe my init.el file or the README of a repo or whatever other files like that I can think of
;; 1. Maybe bookmarks? I've never used them but I know they're there. Heck, maybe I can use them for some of the project specific things I had been thinking about

;; TODO: This was a fun video:
;; https://www.youtube.com/watch?v=grbtRhFiPrw&ab_channel=VaibhavPatil

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %?" :target
         (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>
#+filetags: :%<%Y_%m_%d>:journal:
"))))

(setq org-roam-capture-templates '(("d" "default" plain "%?" :target
  (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}
")
  :unnarrowed t)))

(use-package org-drill)

(add-hook 'org-clock-out-hook
          (lambda ()
            (org-entry-put nil
                           "LAST_CLOCKED_OUT_TIME"
                           (format-time-string
					        (org-time-stamp-format t t)))))

;; TODO: I don't fully understand this tbh. When I followed the
;; installation instructions to the T
;; (http://lilypond.org/doc/v2.21/Documentation/usage/text-editor-support.html)
;; it didn't work and I guess that is because load-path doesn't load
;; things it just, gives a place for things like "require" to look. I
;; tried use-package and I guess the package name has to actually map
;; to one of those provide'd things. Bah. Well, I think this works so
;; we'll stick with this.
(use-package lilypond-mode
  :load-path "lilypond"
  :ensure nil)

;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-lilypond.html
(require 'ob-lilypond)

(defun lag13-replacement-org-babel-execute:lilypond (body params)
  "A function which will, via advice, replace the
`org-babel-execute:lilypond' function defined in ob-lilypond.

The reason I'm replacing that function is simply because it
doesn't do what I want it to do. What I want out of my lilypond
org babel integration is:

1. C-c C-c compiles a SINGLE source code block
2. Display compilation warnings/errors (stdout in general really)
3. Let me specify a file where this compiled score is being written to
4. Don't open the compiled score or anything, I'll just open it myself

I'm honestly surprised ob-lilypond doesn't do this stuff already
because this feels like the bare minimum to me. Maybe I missed
something but from what I could see in the babel code, there were
two little sub modes \"basic\" and \"arrangement\". \"basic\" did
everything I wanted EXCEPT it didn't show warnings/errors/stdout
which is a bummer because then you won't see things like bar
checks:
https://lilypond.org/doc/v2.23/Documentation/learning/bar-lines-and-bar-checks.
\"arrangement\" mode seemed to go completely in a different
direction where it unconditionally tangles ALL source blocks into
a SINGLE file (which is always the name of the org file with an
.ly extension) and then compiles that file. I'm assuming this is
helpful because then you could have a separate source block per
part/instrument and then they'd all get stitched together? I'm
not sure though and either way it holds no interest for me."
  (let* ((out-file (cdr (assq :file params)))
	     (cmdline (or (cdr (assq :cmdline params))
		              ""))
	     (in-file (org-babel-temp-file "lilypond-"))
         (lilypond-stdout-buffer "*lilypond*"))
    (with-temp-file in-file
      (insert (org-babel-expand-body:generic body params)))
    (org-switch-to-buffer-other-window lilypond-stdout-buffer)
    (erase-buffer)
    (call-process org-babel-lilypond-ly-command
                  nil
                  lilypond-stdout-buffer
                  t
                  ;; TODO: If I wanted to be more general, I suppose I
                  ;; could look for the file extension in the file
                  ;; parameter of the source block and add the
                  ;; appropriate switch to the lilypond executable but
                  ;; I only want pdf's now anyway so I'm fine with
                  ;; this.
                  "--pdf"
                  (concat "--output=" (file-name-sans-extension out-file))
                  in-file)
    (goto-char (point-min))
    (when (org-babel-lilypond-check-for-compile-error in-file)
      (error "Error in Compilation!"))))

(advice-add 'org-babel-execute:lilypond :override #'lag13-replacement-org-babel-execute:lilypond)

;; Original motivation was because I wanted to open pdfs in something
;; that could actually display them as opposed to emacs.
(use-package openwith
  :config
  (openwith-mode 1))

;; Trying out including the diary in the agenda view:
;; https://orgmode.org/manual/Weekly_002fdaily-agenda.html. I'm not
;; sure if I'll use the diary feature standalone but I think it
;; already has holidays and stuff built into it which I feel like is
;; nice to display as part of the org mode agenda.
(setq org-agenda-include-diary t)

(setq org-agenda-custom-commands
      '(("n" "Everything that is/can-be worked on today. The current day's agenda and the ACHIEVING items are self explanatory. TODO's within subheadings will, for me, indicate subtasks."
         ((agenda "" ((org-agenda-span 'day)))
          (todo "ACHIEVING")
          (tags "+LEVEL>1+TODO=\"TODO\"")))))

(setq ob-lilypond-header-args
      '((:tangle . "yes")
        (:noweb . "yes")
        (:results . "silent")
        (:cache . "yes")
        (:comments . "yes")))

(setq-default ediff-highlight-all-diffs nil)

(add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 0)))

;; TODO: Make this an actual function call instead of a macro
;;; Sample

;; 	at D:\src\github.com\sfdc-mc-mj\monolith\MetaDataServices\DataQueryEngine\DataQueryEngine.Impl\MetaExpressionGenerator\MetaDataExpressionGenerator.cs(74)
;; SFMC.MetaDataServices.DataQueryEngine.Impl.dll!SFMC.MetaDataServices.DataQueryEngine.Impl.MetaDataQuery.BuildDataSource() Line 85
;; 	at D:\src\github.com\sfdc-mc-mj\monolith\MetaDataServices\DataQueryEngine\DataQueryEngine.Impl\MetaDataQuery.cs(85)
;; SFMC.MetaDataServices.DataQueryEngine.Impl.dll!SFMC.MetaDataServices.DataQueryEngine.Impl.MetaDataQuery.ProjectFinalResults.AnonymousMethod__0() Line 101
;; 	at D:\src\github.com\sfdc-mc-mj\monolith\MetaDataServices\DataQueryEngine\DataQueryEngine.Impl\MetaDataQuery.cs(101)
;; [External Code]
;; SFMC.MetaDataServices.MetaModel.MetaModelImpl.dll!SFMC.MetaDataServices.MetaModel.MetaModelImpl.Client.Diagnostics.MetaStats.Time(string tag, System.Action action, SFMC.MetaDataServices.MetaModel.MetaModelImpl.Client.MetaRequestContext requestContext, SFMC.MetaDataServices.MetaModel.MetaModelImpl.Diagnostics.LogLevelEnum SplunkLogLevel, System.Collections.Generic.List<string> paramsList) Line 37

;; TODO: It would be cool to take a macro like this and turn it into emacs code
;; https://stackoverflow.com/questions/22817120/how-can-i-save-evil-mode-vim-style-macros-to-my-init-el
(fset 'c-sharp-stacktrace-to-org-link
   (kmacro-lambda-form [?J ?H ?/ ?  ?a ?t ?  return ?l ?d ?a ?w ?x ?L ?d ?l ?H ?P ?r ?\] ?F ?\( ?c ?l ?: ?: escape ?I ?\[ ?\[ escape ?m ?a ?\[ escape ?A ?\] ?\] escape] 0 "%d"))
(evil-define-key 'normal org-mode-map (kbd "C-c s") #'c-sharp-stacktrace-to-org-link)

;; TODO: Would it be possible to show the last modified time of the
;; file when asking emacs to open files? Feels interesting. Use case
;; was I wanted to open a file I downloaded but it had a name similar
;; to other files and having the date would have been a quick way to
;; pick out the one I wanted

;; TODO: Is it possible to search/complete on past rg searches? That
;; seems handy. Like "I know I've grep'd for something like XYZ before
;; but I can't quite remember, can I look for that.

(setq-default major-mode 'org-mode)

;; TODO: I'm having trouble getting this package to load at all using
;; these methods. I was able to load it manually as per the directions
;; (which is yet another one of those moments which make me feel like
;; I'd prefer to just manage my own damn packages) but even when I
;; loaded it myself it still ran into an error when doing go-play.....

;; (defmacro defsetf (name setter)
;;   `(gv-define-simple-setter ,name ,setter))
;; (use-package el-go
;;   :straight '(el-go :type git :host github :repo "eschulte/el-go"))
;; (straight-use-package '(el-go :type git :host github :repo "eschulte/el-go"))


;; SO FRICKIN' COOL!!!
;; https://www.reddit.com/r/emacs/comments/knj5gz/play_go_in_orgmode/,
;; https://github.com/misohena/el-igo I feel like I gotta reach out to
;; this person and thank them or something.
;;
;; TODO: I still need to figure out how to properly load this with
;; use-package. I've tried using :after "org" thinking it would match
;; the README which has this bit of code but it didn't work:
(comment (with-eval-after-load "org"
           (require 'igo-org)
           (igo-org-setup)))
(use-package igo-org
  :straight
  '(igo-org :type git :host github :repo "misohena/el-igo")
  :config
  (igo-org-setup)
  :bind (:map igo-editor-graphical-mode-map
              ("x" . igo-editor-cut-current-node))
  )

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              (" " . nil)))
;; I still don't really have one unified way of binding keys it would
;; seem. Feels like it would be nice to have that. Maybe I should look
;; into https://github.com/noctuid/general.el or something
(evil-collection-define-key 'normal 'dired-mode-map
    " " nil)

(evil-collection-define-key 'normal 'help-mode-map
    " " nil)

;; TODO: I feel like I've heard whispers of "icicles" and I'd be
;; curious what features it might have that I'd be interested in:
;; https://www.reddit.com/r/emacs/comments/vr81n6/understanding_minibuffer_completion/

;; Around 2022-01-31 I started noticing that my emacs would hang for a
;; couple seconds when trying to open a file for the first time from
;; the results of doing a https://github.com/dajva/rg.el search. I ran
;; the command toggle-debug-on-quit and hit C-g during the next lag
;; and saw this:
;;
;; call-process("C:\\Program Files\\Git\\usr\\bin\\bash.exe" nil (t "c:/Users/lgroenendaal/AppData/Local/Temp/scor48hAj...") nil "-c" "git submodule --quiet foreach 'echo $displaypath' ...")
;; call-process-shell-command("git submodule --quiet foreach 'echo $displaypath' ..." nil (t "c:/Users/lgroenendaal/AppData/Local/Temp/scor48hAj..."))
;; shell-command("git submodule --quiet foreach 'echo $displaypath' ..." t "*projectile-files-errors*")
;; projectile-files-via-ext-command("d:/src/github.com/sfdc-mc-mj/APPCORE.Wiki/" "git submodule --quiet foreach 'echo $displaypath' ...")
;; projectile-get-immediate-sub-projects("d:/src/github.com/sfdc-mc-mj/APPCORE.Wiki/")
;; projectile-get-all-sub-projects("d:/src/github.com/sfdc-mc-mj/APPCORE.Wiki/")
;; projectile-get-sub-projects-files("d:/src/github.com/sfdc-mc-mj/APPCORE.Wiki/" git)
;; projectile-dir-files-alien("d:/src/github.com/sfdc-mc-mj/APPCORE.Wiki/")
;; projectile-project-files("d:/src/github.com/sfdc-mc-mj/APPCORE.Wiki/")
;; projectile-project-dirs("d:/src/github.com/sfdc-mc-mj/APPCORE.Wiki/")
;; projectile-current-project-dirs()
;; compilation-find-file-projectile-find-compilation-buffer(#f(compiled-function (marker filename directory &rest formats) "Find a buffer for file FILENAME.\nIf FILENAME is not found at all, ask the user where to find it.\nPop up the buffer containing MARKER and scroll to MARKER if we ask\nthe user where to find the file.\nSearch the directories in `compilation-search-path'.\nA nil in `compilation-search-path' means to try the\n\"current\" directory, which is passed in DIRECTORY.\nIf DIRECTORY is relative, it is combined with `default-directory'.\nIf DIRECTORY is nil, that means use `default-directory'.\nFORMATS, if given, is a list of formats to reformat FILENAME when\nlooking for it: for each element FMT in FORMATS, this function\nattempts to find a file whose name is produced by (format FMT FILENAME)." #<bytecode 0x94eade4af1>) #<marker at 4526 in *rg*> #(".\\docs\\docker\\tagging_and_branching.md" 0 38 (fontified t font-lock-face rg-filename-face)) nil)
;; apply(compilation-find-file-projectile-find-compilation-buffer #f(compiled-function (marker filename directory &rest formats) "Find a buffer for file FILENAME.\nIf FILENAME is not found at all, ask the user where to find it.\nPop up the buffer containing MARKER and scroll to MARKER if we ask\nthe user where to find the file.\nSearch the directories in `compilation-search-path'.\nA nil in `compilation-search-path' means to try the\n\"current\" directory, which is passed in DIRECTORY.\nIf DIRECTORY is relative, it is combined with `default-directory'.\nIf DIRECTORY is nil, that means use `default-directory'.\nFORMATS, if given, is a list of formats to reformat FILENAME when\nlooking for it: for each element FMT in FORMATS, this function\nattempts to find a file whose name is produced by (format FMT FILENAME)." #<bytecode 0x94eade4af1>) (#<marker at 4526 in *rg*> #(".\\docs\\docker\\tagging_and_branching.md" 0 38 (fontified t font-lock-face rg-filename-face)) nil))
;; compilation-find-file(#<marker at 4526 in *rg*> #(".\\docs\\docker\\tagging_and_branching.md" 0 38 (fontified t font-lock-face rg-filename-face)) nil)
;; apply(compilation-find-file #<marker at 4526 in *rg*> #(".\\docs\\docker\\tagging_and_branching.md" 0 38 (fontified t font-lock-face rg-filename-face)) nil nil)
;; compilation-next-error-function(0 nil)
;; next-error-internal()
;; compile-goto-error(return)
;; funcall-interactively(compile-goto-error return)
;; call-interactively(compile-goto-error nil nil)
;; command-execute(compile-goto-error)
;;
;; From there I figured out that the reason this is slow is because
;; projectile adds advice around the compilation-find-file function
;; which will first load ALL directories within the project and add it
;; to the search path. Apparently the motivation was that
;; compilation-mode doesn't work well with java stack traces:
;; https://github.com/bbatsov/projectile/issues/331. I don't want it
;; here though so I'm removing it. Note that without that advice (i.e.
;; the more vanilla behavior) selecting a new buffer takes 0.316906
;; seconds but with that advice it takes a whopping 1.445613. So yeah,
;; I don't want it. Timings achieved by running this code:
;; (benchmark-elapse (compile-goto-error))
(add-hook
 'projectile-mode-hook
 (lambda ()
   (advice-remove 'compilation-find-file #'compilation-find-file-projectile-find-compilation-buffer)))
(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-root-files-bottom-up
      (cons "BUILD.proj"
            projectile-project-root-files-bottom-up))
;; TODO: Feels like I should have a keybinding to switch to a buffer
;; NOT within the current project list. Or just generally some sort of
;; list that doesn't intersect with another.

;; As I've seen before, completing-read by default will sort the list
;; it is given alphabetically but it ALSO seems to be the case that it
;; will put items closer to the top of the list based on how recently
;; they were selected. That recency of selection ordering is
;; determined by the HIST argument to completing-read which by default
;; is the variable minibuffer-history:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffer-History.html.
;; It appears that the switch-to-buffer command uses a SEPARATE
;; history list than that default which explains why switching to a
;; buffer within a project using that command and then using the
;; project buffer switching command, that buffer will not appear as a
;; recently switched to buffer. The switch-to-buffer command seems to
;; not even use completing-read instead it calls read-buffer-to-switch
;; which calls read-buffer and read-buffer is written in C so I have
;; NO idea what it does.
;;
;; TODO: PR IDEA. Add this functionality into projectile so I don't
;; need my own custom thing. Not sure the cleanest way to do that...
;; Just kind of feels like it's gonna be messy because every caller of
;; projectile-completing-read is going to have to tell it what is the
;; type of thing being completed. Maybe it's fine but it seems a bit
;; annoying no? For buffers we'd set the display-sort-function but for
;; files we'd probably leave it out so it could fallback to the
;; default behavior.
;;
;; Originaly I just did a completing-read call but I think I'm going
;; all in on consult. Keeping this around for posterity though:
(comment
 (let* ((project-buffers
            (delete (buffer-name (current-buffer))
                    (projectile-project-buffer-names)))
           (completion-table
            (lambda (string pred action)
              (if (eq action 'metadata)
                  '(metadata (category . buffer)
                             (display-sort-function . identity)
                             (cycle-sort-function . identity))
                (complete-with-action action project-buffers string pred)))))
   (switch-to-buffer
    (completing-read
     (projectile-prepend-project-name "Switch to buffer: ")
     completion-table))))
(defun lag13-projectile-switch-to-buffer (&optional switch-project)
  "Switch to a project buffer but order the possible completions to
match the order of the recently switched to buffers. I like this
because then if I switch to buffers within a project via
`switch-to-buffer' or `consult-buffer' and then later use this
project focused buffer switching functionality, the order will be
consistent."
  (interactive "P")
  (if switch-project
      (let ((projectile-switch-project-action #'lag13-projectile-switch-to-buffer))
        (projectile-switch-project))
    (let ((project-buffers
           (delete (buffer-name (current-buffer))
                   (projectile-project-buffer-names))))
      (consult-buffer
       (list (list
              :category 'buffer
              :items project-buffers
              :name (projectile-prepend-project-name "Switch to buffer: ")
              :state #'consult--buffer-state
              :default t))))))

(evil-global-set-key 'motion (kbd "C-SPC") #'lag13-projectile-switch-to-buffer)

;; TODO: I think it makes more sense to have a separate jump list per
;; buffer that just stays within the buffer. I'd want a separate jump
;; list for jumps across files as well. I think I'd want these lists
;; to not have duplicates in them.

;; The inspiration for using vertico was:
;;
;; 1. I've wanted a minibuffer completion thing that stacks the
;; results vertically since it's easier to read than horizontally like
;; icomplete does.
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
;; only going outside for truly novel things. Helm didn't work out of
;; the box for things like M-x so that was a dealbreaker. The whole
;; interface just felt different too.
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
;;
;; TODO: As of emacs 28 there is an icomplete-vertical-mode so do I
;; need vertico anymore?
(use-package vertico
  :ensure t
  :straight t
  :init
  (vertico-mode))

;; TODO: I think I should just start using "straight" by default with
;; use-package. I feel like elpa and the like are just too far behind.
;; Better to get it all from the source I say!

;; TODO: Figure out what this really gets me. Originally I thought it
;; would correctly annotate the projectile-find-file stuff so it could
;; export the results to a dired buffer but that's not the case. Seems
;; that there's more that needs doing to get something like that
;; working: https://github.com/bbatsov/projectile/issues/1664. <--
;; That last comment also makes me wonder if I should just ditch
;; projectile and start using project.el
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(defun lag13-embark-dired-jump (buffer-or-file-name)
  "Perform `dired-jump' for a specified buffer or file name.
Intended to be invoked via `embark-act'. Definitely not a
strictly speaking necessary thing since I could just switch to a
buffer or file then do `dired-jump' once there but... I've wanted
to do this so I think that's a good enough reason as any to add
it."
  (interactive
   (list (read-string "buffer or file: ")))
  (if (get-buffer buffer-or-file-name)
      (with-current-buffer buffer-or-file-name
        (dired-jump))
    (dired-jump nil buffer-or-file-name)))

(use-package embark
  :ensure t
  :bind
  (("C-," . embark-act)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; TODO: Sometimes I load a list of buffers or whatever and I
;; sometimes want to just execute this command DIRECTLY and not even
;; invoke it via embark-act. I'm just curious how that might be
;; possible.
(define-key minibuffer-local-map (kbd "C-x C-j") #'lag13-embark-dired-jump)

;; TODO: What do I about stuff like this which is already installed as
;; part of emacs but apparently just not loaded or something. Should I
;; still use use-package somehow?
(require 'dired-subtree)

;; TODO: When I do C-x C-f on a dired subtree, I feel like it should
;; use the subtree value as the base directory instead of the
;; directory of the dired buffer iteslf.

;; TODO: Is it possible to drag a file from a file explorer into
;; dired? Probably not. I wonder how I could do a "drag and drop" sort
;; of thing between dired buffers or if that's even possible.

;; TODO: In any language I want to be able to be execute a command
;; which prompts me for a symbol/function/variable/whatever and I'll
;; jump to it when I select it. xref-find-definitions does this for
;; elisp but I'm not sure to get it working generally. I'm also not
;; sure how this all jives with evil-goto-definition or if I even want
;; to use that. I'd like the same sort of thing for finding references
;; like xref-find-references does.

;; TODO: I don't understand what is so hard about getting a buffer
;; previous thing working. Neither of these packages are working like
;; I expect. I just want a previous buffer thing that will go through
;; buffers in the same order that switch-to-buffer would show them.
;; Maybe I should look at consult-buffer. And... actually... I think
;; I'm speaking too soon. It's true that I don't like these packages
;; because they are showing EVERY buffer (although I bet that could be
;; configured) but more importantly, I'm definitely seeing that
;; switch-to-buffer is not showing buffers in the minibuffer in the
;; order that they were recently visited. WTH. I had seen this before
;; but I thought it might have been a fluke or something, but no, it's
;; happening. Like, I had opened the file consult.el via C-h f and now
;; it's listed as number 97 (out of 400) buffers when I do
;; switch-to-buffer. What the hell is that?? buffer-list properly
;; shows consult.el as like 4th or whatever as does consult-buffer.
;; I'm thinking it's time to just move away from switch-to-buffer. If
;; I could figure out why this is happening I would but I don't think
;; I can because the functionality is being controlled by read-buffer
;; which is defined in C and there doesn't seem to be anything obvious
;; I can do to change it's behavior via parameters. Heck I suppose I
;; could write my own buffer switching function too just using
;; buffer-list but I suppose I might as well use consult-buffer. I
;; should take a closer look at that function too though, feel like it
;; might have some interesting things.
;;
;; (use-package nswbuff
;;   :ensure t
;;   :straight t
;;   :bind (("C-9" . nswbuff-switch-to-previous-buffer)
;;          ("C-0" . nswbuff-switch-to-next-buffer)))

;; (use-package swbuff
;;   :ensure t
;;   :straight t
;;   :bind (("C-9" . swbuff-switch-to-previous-buffer)
;;          ("C-0" . swbuff-switch-to-next-buffer)))

;; Faster buffer switching. The shifted '9' character is '(' and the
;; shifted '0' character is ')' which feel like backwards and forwards
;; to me respectively hence these bindings.
(global-set-key (kbd "C-9") #'previous-buffer)
(global-set-key (kbd "C-0") #'next-buffer)

;; TODO: Yet again I'm thinking about keeping more curated lists of
;; buffers via perspective. I recently learned that this package
;; (https://github.com/Bad-ptr/persp-mode.el) could be closer to what
;; I want than https://github.com/nex3/perspective-el because the
;; former has configurations which make it so buffers do NOT get added
;; to the perspective by default where the latter does not seem to
;; make that possible. So even though I seemingly found a package that
;; would fit my needs, I've started thinking that what I REALLY want
;; is to be able to tag a buffer with a string and then when I swtich
;; buffers that string will show up in the minibuffer like:
;;
;; mybuffer-name             #category1#category2
;;
;; In other words, this would be just like how I can tag org roam
;; headings and those tags come up as selectable entities when I
;; complete on them. I feel like this sort of thing could be super
;; simple too? As I glance through the perpective code it all seems a
;; bit complicated because you have to worry about things like "if I
;; kill this buffer in one perspective should it be killed in all
;; perspectives?" and all this kind of state stuff. With this though,
;; killing a buffer is just killing a buffer. End of story. The tag
;; will still remain so if we later load that file again then all
;; those tags will be there. I'll probably want to integrate it with
;; projectile so that all the project names that could be associated
;; with the file are associated with it. I guess the data structure
;; would just be a list of alists lists so it's easy to write to a
;; file and re-load later. Something like:
(comment
 (("file1.txt" "tag1" "tag2")
  ("file2.org" "tag3" "tag1")))
;; Then I would just need a way to display the categories in the
;; minibuffer and that they can be selected and junk.

;; TODO: backspace, S-backspace, C-backspace is a good key that I
;; haven't utilized in my code. In my vim setup it used to go to the
;; previous buffer.

;; TODO: Using the C-f and C-b keys when doing a vim search don't do
;; the emacs things like I would want them to.


;; TODO: There are no marks that get set when you yank like there were
;; in vim.

;; TODO: I need to get pasting via register going while in insert
;; mode. I think the only reason I haven't enabled it is because if I
;; use a shell in emacs I'd want C-r to be the reverse history search.

;; TODO: I think I want to do a grep of all the buffers I have open or
;; something akin to it. My actual use case was that I knew I wrote
;; the string UseSubscriberStatus in one of my buffers and I wanted to
;; find it quick. I guess I envisioned a consult like approach where I
;; just started typing that string and it popped up with buffers that
;; matched it. Then I could switch to that buffer and copy said string
;; or whatever.

;; TODO: I did an embark export of some org-roam buffers and when I
;; selected one then tried to go back to the previous buffer I was
;; viewing BEFORE doing the embark export, it was not the previous
;; buffer according to the (buffer-list). It was like the 2nd
;; alternate. I wonder why?

;; TODO: Would it be useful to be able to mark certain org-roam nodes
;; also as tags? They wouldn't be official tags but the idea is that
;; if the node was marked as a tag then it would show up in the
;; completion window. So basically I'd add another function which
;; would get the neighbors of a node that have that special tag marker
;; and then add that to org-roam-node-display-template.

;; TODO: g; should open org mode headings

;; TODO: Could have configure consult-line on the fly so it puts the
;; line in question at the top/bottom of the screen? Use case was the
;; line pattern I was looking for was at the top of a structure that I
;; wanted to see and compare with other lines+structures.

;; TODO: There is a "clone indirect buffer" I think it could be
;; interesting to have a "clone indirect org roam node". Use case is
;; that you want to clone an indirect buffer of the org roam node
;; (maybe to have one window on one part of the roam node and another
;; on another) but you want to still use the org-roam find
;; functionality.

;; TODO: I really like how in visual studio when doing C-t to jump to
;; a different file, you can jump to functions. This should be a thing
;; in emacs too! Having more "hooks" into a file just seems good
;; because maybe you don't remember the name of the file but you
;; remember the name of a function in the file. Or maybe you just want
;; to jump to a function!

;; TODO: I think I should be able to add places of interest (maybe
;; this would be the bookmarks feature of emacs?) and jump to them.
;; Doing so with a consult like interface seems like it would be nice.

;; TODO: There should be a consult for the "g;" change list.

;; TODO: I think it could be cool to be at a function and then be able
;; to traverse up the call stack. Like, I picture basically
;; constructing a potential call graph/tree and then being able to
;; traverse it. So it would basically show all the places this
;; function gets called and you can pick one and then you can pick
;; another one and go up further etc... Use case was I wanted to get
;; back to a spot in code within a huge file and I knew that it was
;; just a couple jumps up from the function and I thought navigating
;; in this way felt like it would be really fun.

;; TODO: the ripgrep tool (or I suppose the tool it's built on) should
;; remember a history of searches then I can recall them... I suppose
;; I could create some functionality to save every rg I perform and
;; include the search term in the buffer name? Use case was I know I
;; grepped for something in the past and I wanted to jump there. I
;; suppose it's not hard to reconstruct the query too... this seems
;; fun though.

;; TODO: Might be nice to have a "search only visible things". I had a
;; situation where I wanted to search for a number I could see on
;; screen to copy it so I did a search but there were some folded org
;; headings in the way and so it went into them rather than just going
;; straight to the number like I really wanted.

;; TODO: Run a grep on a select number of files. Use case is I wanted
;; to search for the word TODO in some files I had modified in a PR.

;; TODO: I should have a more structured way of filing todos because I
;; KNOW I repeat myself and it would be cool if I could start filing a
;; todo and see which other todo's I've filed and if I see a duplicate
;; then I can increment a count somewhere which will be representative
;; of how much I want the thing.
;; 

(setq org-src-preserve-indentation t)

;; TODO: I should add some embark stuff for copying absolute file
;; names from dired and then pasting it into another location:
;; https://emacs.stackexchange.com/questions/39116/simple-ways-to-copy-paste-files-and-directories-between-dired-buffers

;; TODO: I think it would be cool if the annotations that marginella
;; adds could actually be selected on. Like if I download a file and
;; then go to open it I can search for "min ago" to pull that up.

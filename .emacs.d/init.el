;; My Emacs configuration file. The path to this file is determined by
;; user-init-file which gets dynamically set when emacs starts. I
;; believe that when emacs starts up it does something like "find the
;; first non empty file in the list (~/.emacs ~/.emacs.d/init.el).

;; Need to set GOPATH so tools like goimports will work.
(setenv "GOPATH" (concat (getenv "HOME") "/gocode"))
;; exec-path is like "PATH" but for emacs. When emacs tries to run a
;; binary, it will search through exec-path to find it.
(setq exec-path
      (append (list
	       "/usr/local/bin"
	       (concat (getenv "GOPATH") "/bin"))
	      exec-path))
;; Have PATH be the same as exec-path. We do this because emacs will
;; invoke a shell to run a program and the shell needs PATH set
;; appropriately.
(setenv "PATH" (mapconcat 'identity exec-path path-separator))

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

;; Run goimports when saving a Go file.
(add-hook 'go-mode-hook
	  (lambda ()
	    (setq godoc-command "godoc")
	    (setq godoc-use-completing-read t)
	    (local-set-key (kbd "M-.") #'godef-jump)
	    (setq gofmt-command "goimports")
	    (add-hook 'before-save-hook 'gofmt-before-save)))

;; 2 spaces seems to be used in the example elm tutorials I've seen.
(setq elm-indent-offset 4)

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

;; Start emacs fullscreen
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

;; Highlight whitespace at the end of lines and empty lines at the
;; beginning or end of buffers.
;; [[info:emacs#Useless%20Whitespace][info:emacs#Useless Whitespace]]
(setq whitespace-style '(face empty))
(global-whitespace-mode)

;; I feel like backup files just clutter things and I've never had a
;; use for them as of yet (crosses fingers).
(setq make-backup-files nil)

;; Similar to backup files, these lock files clutter things up. And in
;; this case (at least at the time of this writing) they give no
;; benefit because I'm the only one editing files on my computer.
(setq create-lockfiles nil)

;; Normally auto-saving happens in a different file than the one the
;; buffer is visiting. But that creates more files floating around
;; which I don't care for. So let's have auto-save effect the current
;; file being edited.
(setq auto-save-visited-file-name "auto-save in current file please!")

;; So when buffer names are created, they show up as (concat bufname
;; uniquify-separator dir) instead of (concat bufname "<" dir ">")
;; which is one less character to type.
(setq uniquify-buffer-name-style 'post-forward)
;; (setq uniquify-separator ";")

;; I've never had any use for specific settings for particular files
;; or directories so I figured I'd disable those features.
(setq enable-local-variables nil)
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dockerfile-mode elm-mode restclient yaml-mode markdown-mode go-guru editorconfig go-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Jump to the "alternate" file.
(global-set-key (kbd "C-x C-a") 'ff-get-other-file)

(defun view-help-buffer ()
  "Quickly view the help buffer. This code was copied from `view-echo-area-messages' and modified to work with the help buffer."
  (interactive)
  (with-current-buffer (help-buffer)
    (goto-char (point-max))
    (display-buffer (current-buffer))))
(global-set-key (kbd "C-h H") 'view-help-buffer)

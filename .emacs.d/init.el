;; My Emacs configuration file. The path to this file is determined by
;; user-init-file which gets dynamically set when emacs starts. I
;; believe that when emacs starts up it does something like "find the
;; first non empty file in the list (~/.emacs ~/.emacs.d/init.el).

;; Load packages. As of Emacs 25.1 the (package-initialize) function
;; will actually write a call to (package-initialize) in the init file
;; if such a call does not already exist:
;; [[file:/Applications/Emacs.app/Contents/Resources/lisp/emacs-lisp/package.el.gz::(package--ensure-init-file))]].
;; So it would appear that the elpa package authors prefer it if
;; package loading happens before the init file runs. Doing it this
;; way does feel a bit hacky though. I wonder if they are trying to
;; make loading packages before the init file the standard way of
;; doing things?
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
	    (setq gofmt-command "goimports")
	    (add-hook 'before-save-hook 'gofmt-before-save)))

;; Turn on editorconfig
(editorconfig-mode 1)

;; exec-path is like "PATH" but for emacs. When emacs tries to run a
;; binary, it will search through exec-path to find it.
(setq exec-path
      (append exec-path '("/usr/local/bin" "~/gocode/bin")))

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

;; If there is a key binding for the command just run, let us know
;; more quickly. It will also shorten how long the message is
;; displayed but we can always check *Messages* if we're interested.
(setq suggest-key-bindings 1)

;; Highlight whitespace at the end of lines and empty lines at the
;; beginning or end of buffers.
;; [[info:emacs#Useless%20Whitespace][info:emacs#Useless Whitespace]]
(setq whitespace-style '(face trailing empty))
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (editorconfig go-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

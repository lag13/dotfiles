;; My Emacs configuration file. The path to this file is determined by
;; user-init-file which gets dynamically set when emacs starts. I
;; believe that when emacs starts up it does something like "find the
;; first non empty file in the list (~/.emacs ~/.emacs.d/init.el).

;; Load packages. As of Emacs 25.1 the (package-initialize) function
;; will actually write a call to (package-initialize) in the init file
;; if such a call does not already exist:
;; [[file:/Applications/Emacs.app/Contents/Resources/lisp/emacs-lisp/package.el.gz::(package--ensure-init-file))]].
;; So it would appear that the elpa package authors prefer it if
;; package loading happens before the init file runs.
(package-initialize)

;; package-archives is a list of package archives to search through
;; when running commands like list-packages. The default package
;; archive is more strict to modify so I think more packages end up
;; getting added elsewhere such as http://melpa.org/packages/.
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/"))

;; exec-path is like "PATH" but for emacs. When emacs tries to run a
;; binary, it will search through exec-path to find it.
(setq exec-path
      (append exec-path '("/usr/local/bin" "~/gocode/bin")))

;; Run goimports when saving a Go file.
(add-hook 'go-mode-hook
	  (lambda ()
	    (setq gofmt-command "goimports")
	    (add-hook 'before-save-hook 'gofmt-before-save)))

;; "Interactively Do things" shows possible completions as you type for things like buffer/file switching/finding
;; (setq ido-enable-flex-matching t)
;; (ido-mode 1)

;; Although, from a computing standpoint, two spaces separating
;; sentences probably makes more sense, because then programs like
;; emacs can accurately and easily determine sentence boundaries, one
;; space seems to be the norm.
(setq sentence-end-double-space nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (go-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

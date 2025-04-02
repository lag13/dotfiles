;;; evil-cutlass.el --- When deleting text, do not copy it by default

;; EDIT: The date is 2025-04-02, I have received a new computer,
;; upgraded from emacs 28 to 30, and evil-cutlass has stopped
;; working... The reason is that because of the upgrade, the
;; evil-delete function is now byte compiled and the help page now
;; reads "evil-delete is an interactive byte-code-function in
;; ‘evil-commands.el’ where before it read "evil-delete is an
;; interactive native compiled Lisp function in ‘evil-commands.el’".
;; Apparently advice does not work with byte compiled stuff so here we
;; are:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Advice-and-Byte-Code.html.
;; I guess shit like this is why they advise you to not use advice...

;; Copyright (C) 2022 by Lucas Groenendaal
;; Author: Lucas Groenendaal <groenendaal92@gmail.com>
;; Maintainer: Lucas Groenendaal <groenendaal92@gmail.com>
;; Contributors: Lucas Groenendaal <groenendaal92@gmail.com>
;; URL: http://github.com/lag13/evil-cutlass
;; Package-Version: TODO
;; Package-Commit: TODO
;; Git-Repository: git://github.com/lag13/evil-cutlass.git
;; Created: 2022-01-18
;; Version: TODO
;; Package-Requires: ((emacs "24.1") (evil "1.2.0"))
;; Keywords: convenience tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A port of https://github.com/svermeulen/vim-cutlass (which might
;; have been inspired by https://github.com/nelstrom/vim-cutlass) to
;; emacs.

;; This library defines the available functions which do a "delete"
;; and a "change" but does not overwrite any existing bindings.

;;; Code:

(require 'evil)
;; (require 'nadvice)
(require 'evil-surround)
;; TODO: These are the bindings I'd want to override if I wanted to
;; make a true vim-cutlass sort of thing:

;; (define-key evil-normal-state-map "c" 'evil-change)
;; (define-key evil-normal-state-map "C" 'evil-change-line)
;; (define-key evil-normal-state-map "d" 'evil-delete)
;; (define-key evil-normal-state-map "D" 'evil-delete-line)
;; (define-key evil-normal-state-map "s" 'evil-substitute)
;; (define-key evil-normal-state-map "S" 'evil-change-whole-line)
;; (define-key evil-normal-state-map "x" 'evil-delete-char)
;; (define-key evil-normal-state-map "X" 'evil-delete-backward-char)

;; I think I see 3 options here for making this happen:

;; OPTION 1: I believe all those functions ultimately call out to
;; evil-delete so it feels like the most effective way to override all
;; of those things would be to add a flag to the evil package which
;; changes evil-delete to not cut text and additionally define another
;; operator which explicitly cuts text (i.e. it just does what
;; evil-delete does now). This feels like the best approach because
;; the fact that evil-delete is used within all of those functions is
;; an implementation detail and it feels okay to rely on that detail
;; if you're working within the implementation.

;; OPTION 2: Create cutlass specific commands for ALL of these
;; commands and then rebind all those keys. This seems like a bit of a
;; pain but is probably the right way to go about it if we're making
;; this a dedicated library. ACTUALLY, this might not be the right
;; thing to do (or at least will be more of a pain than I thought)
;; because the "ds" command in evil-surround expects that "d" is bound
;; to evil-delete in order to work (and simlarly for the
;; evil-exchange). I'm not sure on the implementation details there
;; (and why being able to define something like "ds" seems tricky in
;; evil mode) and it seems like we could config around it but it's
;; probably best if we change as little as possible so other packages
;; just keep working. So maybe this option is not so good.

;; OPTION 3: Use the "advice" feature of emacs (which is what this
;; user did in their implementation
;; https://github.com/kisaragi-hiu/evil-cutlass) to redefine the
;; emacs-delete operator so it does not cut text. This is quick to get
;; something working (this file does it currently in fact) but the
;; drawback is that we're relying on an internal implementation
;; detail. If option 2 is a steady consitent version, this is the
;; dynamic powerful one. In general, dynamic'ness feels like a more
;; useful property in sort of "one off" scenarios (like a quick
;; script) that are hopefully scoped to a small number of people (so
;; it can be easily understood). But dynamicness by it's very nature
;; of being, well, dynamic, will only continue to work if the
;; conditions upon which it relies on remain the same. In this case,
;; it would only continue to work if evil-delete remained the function
;; that all others called to delete text. Relying on an internal
;; implementation detail from an external library feels like it has no
;; place in code (at least if you want your code to continue
;; working!). EDIT: I was spot on about the dynamicness thing being
;; delicate in light of the byte compilation breaking the advice...

;; (defvar evil-cutlass-cut-text nil
;;   "Whether the `evil-delete' function should cut text or not.")

;; (evil-define-operator evil-cutlass-delete-advice (evil-delete-fn beg end type register yank-handler)
;;   "Modifies `evil-delete' such that, by default, REGISTER is set
;; to the black hole register and thus the deleted text is not
;; copied. As of 2022-01-09 this ends up, conveniently, modifying
;; all other commands which delete text such as x, s, c, etc...
;; because, under the hood, those commands call out to
;; `evil-delete'."
;;   (interactive "<R><x><y>")
;;   (if evil-cutlass-cut-text
;;       (funcall evil-delete-fn beg end type register yank-handler)
;;     (let ((register (or register ?_)))
;;       (funcall evil-delete-fn beg end type register yank-handler))))

;; (evil-define-operator evil-cutlass-cut (beg end type register yank-handler)
;;   "The same as the original definition of `evil-delete' (i.e. it
;; always cuts text)."
;;   (interactive "<R><x><y>")
;;   (let ((evil-cutlass-cut-text t))
;;     (evil-delete beg end type register yank-handler)))

;; (advice-add 'evil-delete :around #'evil-cutlass-delete-advice)
;; (evil-define-key '(normal visual) 'global "x" #'evil-cutlass-cut)

;; TODO: Get this issue deleted because it doesn't seem to be valid (I
;; was able to advise just fine and not break anything):
;; htts://github.com/emacs-evil/evil/issues/1326

;; (advice-remove 'evil-delete 'evil-cutlass-delete-advice)

(evil-define-operator evil-cutlass-delete (beg end type register yank-handler)
  (interactive "<R><x><y>")
  (let ((register (or register ?_)))
    (evil-delete beg end type register yank-handler)))

(evil-define-operator evil-cutlass-change (beg end type register yank-handler)
  (interactive "<R><x><y>")
  (let ((register (or register ?_)))
    (evil-change beg end type register yank-handler)))

(evil-define-key '(normal visual) 'global "d" #'evil-cutlass-delete)
(evil-define-key '(normal visual) 'global "c" #'evil-cutlass-change)
(evil-define-key '(normal visual) 'global "x" #'evil-delete)

;; Phew, turns out there was a way to just redefine new operators and
;; not break the surround stuff.
(setq evil-surround-operator-alist
      '((evil-change . change)
        (evil-delete . delete)
        (evil-cutlass-delete . delete)
        (evil-cutlass-change . change)))

(provide 'evil-cutlass)

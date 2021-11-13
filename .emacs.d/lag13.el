;;; Starting to mess around with elisp -*- lexical-binding: t; -*-

;; I figure I might be in emacs'land for quite a while so I want to
;; learn some elisp programming but I don't want to clutter up my
;; init.el file so I'll just put this stuff here. Part of this too
;; will probably be me just messing around with programming concepts
;; and doing what I do best: waxing poetic on what I think about
;; various programming concepts.

;; TODO: I don't know how much I should be adding these "require"
;; statements. I guess just as necessary? It's also got me wondering
;; about how libraries declare that they need to rely on other
;; libraries to work such that those dependencies get downloaded too.
;; I don't know a lot about this stuff. It all feels very... global
;; which is probably convenient but also bad? Like I never explicitly
;; installed map.el or ht.el but they both exist. Were they installed
;; from another package? If so then if I remove said package then if
;; I've used that code then my code just breaks one day. I hate that
;; action at a distance stuff. Or maybe some of them were installed by
;; default as part of emacs?
(require 'benchmark)

(defun equal? (o1 o2)
  "Return t if two objects have a similar structure and contents.
This was created because the built in \"equal\" function does not
work for hash tables but I want it to."
  (if (and (ht? o1) (ht? o2))
      (ht-equal? o1 o2)
    (equal o1 o2)))

;; TODO: Apparently a fixed point can be thought of as a "periodic
;; point" (https://en.wikipedia.org/wiki/Periodic_point) with a period
;; of 1. I wonder if making that sort generalization is useful. Heck
;; even a "periodic point" could probably be further generalized as
;; "return the latest point when a function on the orbit of an
;; iterated function returns true". I wonder if THAT generalization is
;; useful at all. Related links:
;; https://en.wikipedia.org/wiki/Iterated_function (so I guess that
;; clojure function "iterate" is well named:
;; https://clojuredocs.org/clojure.core/iterate also, for my
;; knowledge, here is some info about why clojure recommends not
;; passing a function with side effects into iterate:
;; https://www.reddit.com/r/Clojure/comments/orwvmq/iterate_does_f_really_must_be_free_of_sideeffects/.
;; There's also this:
;; https://stuartsierra.com/2015/08/25/clojure-donts-lazy-effects).
;; Just keep it in mind as a way to generalize the idea of controlling
;; iteration!
(defun fixed-point (f x &optional n)
  "Finds the fixed point of F given initial input X. Optionally
provide a max number N of iterations before the computed value is
returned."
  (if (null n)
      ;; Thank you
      ;; https://gigamonkeys.com/book/loop-for-black-belts.html for
      ;; the common lisp loop tutorial!
      (cl-loop for prev = x then next
	       for next = (funcall f prev)
	       until (equal? prev next)
	       finally return prev)
    (cl-loop for prev = x then next
	     repeat n
	     for next = (funcall f prev)
	     until (equal? prev next)
	     finally return prev)))


;; TODO: Returning buffers with data feels kind of weird but also it
;; seems like a pattern with emacs so I guess it's okay. One thing I
;; want to research though is what emacs does with all these random
;; buffers. Do they get cleaned up faster than buffers that get edited
;; normally?

(defun url-retrieve-body-synchronously (url)
  "Returns a buffer filled with just the message body of the http
response from URL"
  (let ((buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      ;; There is an empty line between the header stuff and the
      ;; message body
      (re-search-forward "^$")
      (forward-char)
      (delete-region (point-min) (point))
      buffer)))

(defun url-retrieve-body-synchronously-str (url)
  "A second attempt at writing the function to get just the http
response by using less buffer related logic. I'm so early on in
emacs programming that I don't really know yet if working with
data in buffers is a common way to do things but on first pass it
feels weird so I wanted to see what the code looks like if I just
do normal string manipulation stuff. Unfortunately the base
function which gets the data from a url does store it in a buffer
but it's easy to get that data as a string. Also, I learned that
the function url-http is what ultimately generates the temporary
buffer. Took me a bit to find that. That function is stored in
some sort of property list? Not really sure how it all works
yet."
  (let ((resp (with-current-buffer
		  (url-retrieve-synchronously url)
		(buffer-string))))
    (substring resp (1+ (string-match "^$" resp)))))

(defmacro comment (&rest body)
  "Evaluates the body and yields nil. Borrowed from clojure:
https://clojuredocs.org/clojure.core/comment Useful for adding
code to a file which is illustrative but you don't want
executed." nil)

(defun parse-html-from-url (url)
  "Parses html from the data from a URL. I created this because I
wanted to make sure the temporary buffer gets deleted (I assume
this is a good thing to do?) and I didn't want to write it all
out every time. I just want to be more declarative with my code!"
  (let* ((buffer (url-retrieve-body-synchronously url))
	 (dom 
	  (with-current-buffer
	      buffer
	    ;; TODO: I cannot for the life of me get the "base url"
	    ;; parameter to libxml-parse-html-region working. I
	    ;; assumed that it would slap on the base url to any href
	    ;; attributes which were absolute or relative paths but it
	    ;; doesn't seem to work that way, It doesn't seem to do
	    ;; anything frankly.
	    (libxml-parse-html-region (point-min) (point-max) url))))
    (kill-buffer buffer)
    dom))

;; TODO: Make a naked host url and a host url which just ends in a "/"
;; equivalent.

(defun href-to-absolute-url (href base-urlobj)
  "Turns the string within an anchor tag's 'href' attribute into
an absolute url.

Related links:
- https://url.spec.whatwg.org/"
  (let ((href-urlobj (url-generic-parse-url href)))
    (cond
     ;; Sometimes anchor tags don't have an href attribute hence nil
     ;; could get passed to this function. We could check for nil
     ;; before passing it but it's to accept such a value.
     ((null href)
      nil)
     ((string-match "^/" href)
      (url-recreate-url
       ;; I'm not sure why the creators of url-parse.el decided to get
       ;; rid of the default url constructor in favor of this one
       ;; which only takes positional arguments (there's so many!) but
       ;; here we are! I also learned that constructors that work like
       ;; this one (i.e. they just take positional arguments) are
       ;; called "BOA constructors" lol:
       ;; https://www.gnu.org/software/emacs/manual/html_node/cl/Structures.html
       (url-parse-make-urlobj
	(url-type base-urlobj)
	nil
	nil
	(url-host base-urlobj)
	nil
	(car (url-path-and-query href-urlobj))
	nil
	nil
	t)))
     ((string-match "^\\." href)
      (url-recreate-url
       (url-parse-make-urlobj
	(url-type base-urlobj)
	nil
	nil
	(url-host base-urlobj)
	nil
	(concat (car (url-path-and-query base-urlobj))
		"/"
		(car (url-path-and-query href-urlobj)))
	nil
	nil
	t)))
     ((string-match "^\\(http\\|https\\)://" href)
      (url-recreate-url
       (url-parse-make-urlobj
	(url-type href-urlobj)
	nil
	nil
	(url-host href-urlobj)
	nil
	(car (url-path-and-query href-urlobj))
	nil
	nil
	t)))
     ;; Any other url (like mailto:cool@person.com or
     ;; #element-id-to-jump-to) we don't care about so we're ignoring
     ;; them.
     (t
      nil))))

(defun get-urls-within-same-site (base-url)
  "Finds all urls within BASE-URL which have the same host as
BASE-URL."
  (let ((base-urlobj (url-generic-parse-url base-url))
	;; TODO: I feel like sometimes I just end up fighting with
	;; these threading macros. Like, first I'll write the code in
	;; the "normal" lisp way but then it often feels confusing to
	;; read when there are a lot of functions involved so I start
	;; using threading macros but then sometimes the functions I
	;; write don't conform with the kind of threading macro I
	;; picked or the argument that threads through needs to go in
	;; different places. Maybe I just need to practice using these
	;; more, I wonder if there's a better way though where even a
	;; novice like me doesn't need to fiddle around so much. Like,
	;; would it be so bad to just have a list of single argument
	;; functions and thread a value through that? No need for
	;; macro craziness where it modifies the forms? Maybe what I
	;; need is more curried functions or a more syntactically
	;; friendly way to do curried things! In terms of getting
	;; better at recognizing these threading macros though, it's
	;; probably good to keep in mind the clojure documentation on
	;; these macros, namely, functions that operate on data
	;; structures (getting a value out of a data structure,
	;; updating a data structure, ...) usually have the data
	;; structure as the first argument and functions that operate
	;; on sequences usually have the sequence as their last
	;; argument: https://clojure.org/guides/threading_macros. Not
	;; sure why that convention but I want to remember it and try
	;; to apply it.
	(anchor-tags (-> base-url
			 parse-html-from-url
			 (dom-by-tag 'a))))
    (->> anchor-tags
	 (-map (lambda (anchor-tag) (dom-attr anchor-tag 'href)))
	 (-map (lambda (href) (href-to-absolute-url href base-urlobj)))
	 (-filter (lambda (absolute-href)
		    (and absolute-href
			 (equal (url-host base-urlobj)
				(url-host (url-generic-parse-url absolute-href)))))))))

(defun ht-collect-keys (pred table)
  "Builds a list of all keys from TABLE that satisfy PRED,
which is a function of two arguments: KEY and VALUE."
  (let (results)
    (ht-each (lambda (key val)
	       (when (funcall pred key val)
		 (setq results (cons key results))))
	     table)
    results))

(defun crawl-site-one-level (site-hash-table)
  "Grows the hash table by crawling all urls who's value
indicates it needs to be crawled and, for any newly seen urls,
add them to the hash table so they will be crawled on future
invocations of this function."
  ;; TODO: Could I structure this function to better seperate the side
  ;; effect and non side effect portions? Or isolate the side effect
  ;; stuff to a smaller area? Because right now the
  ;; get-urls-within-same-site function performs IO. I guess I like
  ;; the idea of being able to test this in a functional manner.
  (let* ((urls-to-crawl
	  (ht-collect-keys (lambda (_ value) (eq value :crawl-me)) site-hash-table))
	 (discovered-urls
	  (-map (-compose #'-distinct #'get-urls-within-same-site) urls-to-crawl))
	 ;; Having to do stuff like this (i.e. make a copy of a data
	 ;; structure because otherwise the same one is shared and
	 ;; modified in different places) is what really makes me like
	 ;; functional programming.
	 (site-hash-table (ht-copy site-hash-table)))
    ;; Updates the url keys's values to be the urls that were found on
    ;; those pages.
    (cl-mapc (lambda (url discovered-urls)
	       (ht-set! site-hash-table url discovered-urls))
	     urls-to-crawl
	     discovered-urls)
    ;; Adds the newly discovered urls to the hash table as urls to be
    ;; crawled.
    (cl-mapc (lambda (newly-discovered-url)
	       (unless (ht-contains? site-hash-table newly-discovered-url)
		 (ht-set! site-hash-table newly-discovered-url :crawl-me)))
	     (-mapcat #'identity discovered-urls))
    site-hash-table))

(defun crawl-site (url-or-hash-table &optional depth-limit)
  "Crawls a site starting from a given url. Being able to provide
a single url is sort of a convenience, what the website crawling
function truly operates on is a hash table and that may be passed
in as well which can be handy if you, say, start crawling a
website, save the results so far, and continue crawling at a
later time."
  (if (stringp url-or-hash-table)
      ;; TODO: I'm not sure if I like this :crawl-me keyword getting
      ;; repeated everywhere, how do we fix that? A let binding over
      ;; these two functions?
      (fixed-point #'crawl-site-one-level (ht (url-or-hash-table :crawl-me)) depth-limit)
    (fixed-point #'crawl-site-one-level url-or-hash-table depth-limit)))

;; TODO: Defining a test in elisp is pretty straightforward actually,
;; the remaining work for me is twofold: ONE is that I should probably
;; setup a local webserver for testing instead of hitting an existing
;; one. I bet elisp could do this. TWO is that when comparing hash
;; tables, the ERT output is not at all clear about where the hash
;; tables differ. Turns out that ERT CAN better indicate where the
;; difference is but that has to be set up properly. For example, the
;; function "equal" has this property on it: (get 'equal
;; 'ert-explainer) which points to a function ert--explain-equal. I
;; would need to do something similar for ht-equals? (or my equals?
;; function perhaps) to get that to work properly. I'll read up on the
;; documentation to better understand that.

(ert-deftest crawl-site-test ()
  "Playing around with the elisp testing framework ERT."
  (should (equal? (crawl-site "https://tbaggery.com")
		  #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("https://tbaggery.com" ("https://tbaggery.com/" "https://tbaggery.com/2011/08/08/effortless-ctags-with-git.html" "https://tbaggery.com/2011/02/07/bundle-while-you-git.html" "https://tbaggery.com/2010/10/24/reduce-your-rails-schema-conflicts.html" "https://tbaggery.com/2010/03/04/smack-a-ho-st.html" "https://tbaggery.com/2010/02/28/episode-iv-a-new-pope.html" "https://tbaggery.com/page2") "https://tbaggery.com/" ("https://tbaggery.com/" "https://tbaggery.com/2011/08/08/effortless-ctags-with-git.html" "https://tbaggery.com/2011/02/07/bundle-while-you-git.html" "https://tbaggery.com/2010/10/24/reduce-your-rails-schema-conflicts.html" "https://tbaggery.com/2010/03/04/smack-a-ho-st.html" "https://tbaggery.com/2010/02/28/episode-iv-a-new-pope.html" "https://tbaggery.com/page2") "https://tbaggery.com/2011/08/08/effortless-ctags-with-git.html" ("https://tbaggery.com/") "https://tbaggery.com/2011/02/07/bundle-while-you-git.html" ("https://tbaggery.com/") "https://tbaggery.com/2010/10/24/reduce-your-rails-schema-conflicts.html" ("https://tbaggery.com/") "https://tbaggery.com/2010/03/04/smack-a-ho-st.html" ("https://tbaggery.com/") "https://tbaggery.com/2010/02/28/episode-iv-a-new-pope.html" ("https://tbaggery.com/") "https://tbaggery.com/page2" ("https://tbaggery.com/" "https://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html" "https://tbaggery.com/2008/04/18/example-git-workflows-maintaining-a-long-lived-topic-branch.html" "https://tbaggery.com/2008/04/17/example-git-workflows-merging-a-contributor-via-pull.html" "https://tbaggery.com/2008/04/13/best-practices-for-contributing-to-rails-with-git.html" "https://tbaggery.com/2007/05/03/easy-ruby-examples.html" "https://tbaggery.com/page3") "https://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html" ("https://tbaggery.com/") "https://tbaggery.com/2008/04/18/example-git-workflows-maintaining-a-long-lived-topic-branch.html" ("https://tbaggery.com/" "https://tbaggery.com/2008/04/17/example-git-workflows-merging-a-contributor-via-pull.html") "https://tbaggery.com/2008/04/17/example-git-workflows-merging-a-contributor-via-pull.html" ("https://tbaggery.com/") "https://tbaggery.com/2008/04/13/best-practices-for-contributing-to-rails-with-git.html" ("https://tbaggery.com/") "https://tbaggery.com/2007/05/03/easy-ruby-examples.html" ("https://tbaggery.com/") "https://tbaggery.com/page3" ("https://tbaggery.com/" "https://tbaggery.com/2007/02/11/auto-loading-ruby-code.html" "https://tbaggery.com/page2") "https://tbaggery.com/2007/02/11/auto-loading-ruby-code.html" ("https://tbaggery.com/"))))))

;; TODO: I feel like I wish that every (?) emacs function invocation
;; was timed because when I've been playing around with this,
;; sometimes I'll run a function and after the fact I'll be curious
;; about how long it took to run so I'll have to re-invoke it with a
;; benchmark-elapse which seems silly.

(comment
 (benchmark-elapse (setq tmp (crawl-site "https://tbaggery.com")))
 (benchmark-elapse (crawl-site "http://wyattgorman.com/"))
 (crawl-site-depth-1 (ht ("https://tbaggery.com" :crawl-me)))
 (pp tmp)
 (crawl-site-depth-1 (ht ("https://www.braveclojure.com" :crawl-me)))
 (crawl-site-depth-1 tmp)
 (benchmark-elapse (setq tmp (crawl-site "https://www.braveclojure.com" 5)))
 (length (ht-keys tmp))
 )

;; TODO: I don't think my implementations for lazy stuff is very good
;; or maybe it can't be helped? I say it because we have all these
;; nested lambdas and having those nested invocations will quickly
;; exceed the elisp variable max-lisp-eval-depth and the code will
;; stop. What could we do to fix this? What do other implementations
;; of lazy lists do? Should we modify the list as thunks are
;; evaluated? So part of the list will be non-lazy and part will be
;; and we just adapt lazy-car and lazy-cdr appropriately?
(defmacro lazy-cons (car cdr)
  "Creates a lazy cons cell (i.e. a cons cell wrapped inside a
thunk (i.e. an anonymous function with no args)) with CAR and
CDR. It's worth noting, that one should probably not mix laziness
with code that alters state outside of your program. Usually you
want a \"world\" altering operation to happen at a specific time
and introducing laziness reduces your control over when that side
effect happens. Doing things like modifying internal
state (perhaps a random number generator) or performing IO to
read data is probably fine though.

Related links:

- https://en.wikipedia.org/wiki/Thunk
- https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html#%_sec_3.5
- https://www.classes.cs.uchicago.edu/archive/2017/spring/22300-1/lectures/LazyLists/index.html
- https://stuartsierra.com/2015/08/25/clojure-donts-lazy-effects
- http://blog.thezerobit.com/2012/07/28/lazy-sequences-in-common-lisp.html

An earlier implementation of this function was:

`(cons ,car (thunk-delay ,cdr))

Which I thought felt... I don't know, sort of elegant somehow.
It's actually how SICP implements their \"stream\" data structure
but part of me didn't like it because the very first element of
the list is NOT lazy hence why I scrapped it. Ultimately it
probably doesn't matter (this is just me playing around after
all) but I liked the idea of having a TRULY lazy list.

Before reaching the current implementation I also tried something
like this to achieve a truly lazy list:

`(cons (thunk-delay ,car) (thunk-delay ,cdr))

But I didn't like that since it introduces so many extra thunks
that need to be evaluated."
  `(thunk-delay (cons ,car ,cdr)))

(defun lazy-car (l)
  (car (thunk-force l)))

(defun lazy-cdr (l)
  ;; Check for nil here so lazy-cdr behaves the same as cdr on a nil
  ;; list, namely, it returns nil.
  (if (null l)
      nil
    (cdr (thunk-force l))))

(defun lazy-iterate (f x)
  "Returns a lazy sequence of X, (F X), (F (F X)), etc... In
mathematical terms I believe you would say it returns the
\"orbit\" of X. Implementing this was inspired by the function
\"iterate\" in clojure. 

Related links:

- https://en.wikipedia.org/wiki/Iterated_function
- https://en.wikipedia.org/wiki/Orbit_(dynamics)
- https://clojuredocs.org/clojure.core/iterate"
  (lazy-cons x (lazy-iterate f (funcall f x))))

;; TODO: We have a lot of recursion with these functions. I like
;; writing things like this but a lot of them aren't tail recursive so
;; they'd probably be slow. Maybe I should write a lower level
;; function like "reduce" and have it be more iterative then the rest
;; of these functions can build off that. They'll still be nice but
;; also be more efficient. Or, heck, maybe I just leave it alone!

(defun lazy-nth (n l)
  "Get the Nth element from lazy list L."
  (if (zerop n)
      (lazy-car l)
    (lazy-nth (1- n) (lazy-cdr l))))

(defun lazy-list-helper-fn (l)
  (if (null l)
      'nil
    `(lazy-cons ,(car l) ,(lazy-list-fn (cdr l)))))

(defmacro lazy-list (&rest objects)
  (lazy-list-helper-fn objects))

(defun lazy-take (n l)
  (if (or (<= n 0)
	   (null l))
      nil
    (lazy-cons (lazy-car l) (lazy-take (1- n) (lazy-cdr l)))))

(defun lazy-list-to-list (l)
  "Converts a lazy list to a list."
  (if (null l)
      nil
    (cons (lazy-car l)
	  (lazy-list-to-list (lazy-cdr l)))))

(defun defun-multi-arity-helper-fn (name docstring multi-arity-forms)
  (let ((defun-arg-name (gensym)))
    (cl-flet ((multi-arity-form-to-cl-case-clause
	       ;; Apparently the cl-* forms in elisp allow for
	       ;; destructring of arguments like in clojure, neat:
	       ;; https://www.gnu.org/software/emacs/manual/html_node/cl/Argument-Lists.html
	       ;; It almost makes me just want to use cl-defun all the
	       ;; time instead of defun... hmmm. Ah well! I'll keep it
	       ;; in mind though.
	       ((arglist &rest body))
	       `(,(length arglist)
		 (let ,(-map-indexed (lambda (index arglist-param)
				       (list arglist-param `(nth ,index ,defun-arg-name)))
				     arglist)
		   ,@body))))
      `(defun ,name (&rest ,defun-arg-name)
	 ,docstring
	 (cl-case (length ,defun-arg-name)
	  ,@(-map #'multi-arity-form-to-cl-case-clause multi-arity-forms)
	  (otherwise (error "multi arity function called with a number of arguments for which this function has no definition.")))))))

(defmacro defun-multi-arity (name &optional docstring &rest fn-forms)
  "Creates a function which can accept different numbers of
parameters (i.e. \"arities\") when invoked. It's basically
overloading a function based on the number of arguments. Under
the hood, it dispatches to a different code path depending on how
many arguments were passed into the multi-arity function.

The original motivation for this (as are many other things in my
travels) was clojure:
https://clojure.org/guides/learn/functions#_multi_arity_functions
More specifically, I wanted to make a \"lazy-range\" function and
copy it's implementation from clojure's \"range\" function:
https://github.com/clojure/clojure/blob/38bafca9e76cd6625d8dce5fb6d16b87845c8b9d/src/clj/clojure/core.clj#L3019-L3039
but implementing that with a plain ol' defun plus &optional
parameters felt a bit messy because you have to manually check
how many arguments are passed and work accordingly AND, in this
case, the first parameter has a different meaning when the
function is invoked with more parameters and that made it feel
hard to name the variable properly. For example, when you
call (range 10), the first parameter \"10\" signifies the LAST
number exclusive in the generated sequence but when invoking the
function like (range 10 100), the number \"10\" now means the
FIRST number inclusive in the range. So I wanted to name the
variable something like \"max-or-min\" and that felt weird. Being
able to define my own multi-arity functions fixes these problems.

I also feel like being able to define a multi-arity function is a
potentially cleaner way to define a recursive function which ends
up having to accumulate a result in one of it's parameters. Such
functions often have a clean interface which do NOT expose the
accumulation parameter and then, inside, they define a function
which has that extra accumulation parameter and just call out to
that function."
  (unless (stringp docstring)
    (setq fn-forms (cons docstring fn-forms))
    (setq docstring nil))
  (defun-multi-arity-helper-fn name docstring fn-forms))

(defun-multi-arity lazy-range
  "Basically the same implementation as clojure's \"range\"
function. Other languages have a similarly named function too.
I'm not actually sure if I like this function name much. I say
that because the word \"range\" in mathematics already has a
meaning relating to the values that a function produces. I've
also seen the word as describing the difference between a low and
high value. In the SICP book, they have a function named
\"enumerate-interval\" which I think is more explicit and might
like better.

I'm honestly not sure if I even like this function implementation
because I'm not sure I like it when the same argument position in
a function has a different meaning depending on how many
arguments follow. I think I prefer a little more... consistency?
explicitness? I mean, I guess doing stuff like this effectively
means you get more power out of the same word, but it also opens
the door to a bit of confusion. I think it's the kind of thing
where, for veterans, it's probably powerful but, for newcomers,
it might be a bit confusing at first.

Related links:
- https://clojuredocs.org/clojure.core/range
- https://docs.python.org/3/library/stdtypes.html?highlight=range#range
- https://www.php.net/manual/en/function.range.php
- https://www.mathsisfun.com/definitions/range-statistics-.html
- https://en.wikipedia.org/wiki/Range_of_a_function
- https://www.gnu.org/software/emacs/manual/html_node/elisp/Float-Basics.html"
  (() (lazy-range 1.0e+INF))
  ((end) (lazy-range 0 end))
  ((start end) (lazy-range start end 1))
  ((start end step)
   (if (>= start end)
       nil
     (lazy-cons start (lazy-range (+ start step) end step)))))

(comment
 (lazy-nth 10 (lazy-cons 1 (lazy-cons 2 (lazy-cons 3 ()))))
 (lazy-nth 200 (iterate #'1+ 0))
 ;; My original attempt at writing a lazy list macro. I want to
 ;; understand in more depth why it didn't work. I figured out a
 ;; solution by leaning on a recursive function which generates the
 ;; list and I understand that but I wonder why the macro itself
 ;; couldn't be recursive. Maybe this is helpful:
 ;; https://gist.github.com/nimaai/2f98cc421c9a51930e16#recursion
 (if (null objects)
      ()
    `(lazy-cons ,(car objects) (lazy-list ,(cdr objects))))
 )



;; TODO: How could we put some sort of test in place to make sure the
;; website crawler works as expected? I wonder what testing looks like
;; within emacs and I wonder how we would go about testing something
;; like this.

;; TODO: How to properly handle redirects? Like, if we do
;; https://braveclojure.com then I think it redirects to
;; https://www.braveclojure.com but I wouldn't crawl that because now
;; it's at a different host so my code thinks it's a different site. I
;; guess we need to keep a set of urls hosts which are considered the
;; same site?

;; TODO: Is there a way to inspect the progress of the crawling as it
;; goes? I think that would be cool because then I wouldn't need to
;; have things like print statements.

;; TODO: How do I have a process like this running in the background
;; so I can still do other stuff? If I do that I'd like to see how to
;; inspect said process too!

;; TODO: What happens if a url does not actually exist or something
;; like that? General error handling I guess.

;; TODO: Create a function to download all the pages of a particular
;; website and rewrite any links so that they point to local files.
;; Use case is that sometimes I've read ebook-like things online and
;; sometimes I've wanted to be able to do that when going on an
;; airplane or somewhere else without internet

;; TODO: Another fixed point website crawler but have the evolution
;; function (I think that's a good term for it:
;; https://en.wikipedia.org/wiki/Orbit_(dynamics)) crawl just one site
;; at a time.

;; TODO: A variation on the fixed point idea. Define the function
;; which gets repeatedly applied but have it also accept a number
;; representing the amount of work to do. During it's work the
;; function would reduce this number and pass it back to a new
;; modified fixed-point function. The motivation here is that I didn't
;; like how the fixed-point function I originally defined can only put
;; a limit on the number of times f is called but (especially for
;; website crawling) one invocation of f could take a VERY different
;; amount of time than another invocation depending on how many
;; websites we had to crawl. So, by having this number we could
;; control it in a more fine grained way which feels interesting.

;; TODO: Website crawler function taking a single url and the hash
;; table and does everything, recursion and all.

;; TODO: Website crawler function that just takes the hash table and
;; does it all.

;; TODO: Website crawler function that takes a list of urls instead of
;; just a single string.

;; TODO: Once we write a bunch of website crawling algorithms,
;; parallelize them to see what that looks like.


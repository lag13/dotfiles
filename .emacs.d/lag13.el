;;; Starting to mess around with elisp -*- lexical-binding: t; -*-

;; I figure I might be in emacs'land for quite a while so I want to
;; learn some elisp programming but I don't want to clutter up my
;; init.el file so I'll just put this stuff here. Part of this too
;; will probably be me just messing around and doing what I do best:
;; waxing poetic on what I think about various programming concepts.

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
(require 'thunk)
(require 'dom)

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
returned.

Related links:
- https://github.com/RutledgePaulV/missing/blob/fb1b113ada2be09cf04f42022731ef317287adf6/src/missing/core.clj#L326-L341
- https://gigamonkeys.com/book/loop-for-black-belts.html"
  (if (null n)
      (cl-loop for prev = x then next
	       for next = (funcall f prev)
	       until (equal? prev next)
	       finally return prev)
    (cl-loop for prev = x then next
	     repeat n
	     for next = (funcall f prev)
	     until (equal? prev next)
	     finally return prev)))

;; TODO: Writing this multi-arity defun macro was fun. Do some other
;; macros to like maybe a defun-curried macro for example.

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

(defun bad-defun-curried-helper-fn (name arglist docstring body)
  `(defun ,name (,(car arglist)) ,docstring
	  ,@(-reduce-r-from (lambda (arg acc)
			     (list (append `(lambda (,arg)) acc)))
			    body
			    (cdr arglist))))

(defmacro bad-defun-curried (name arglist &optional docstring &rest body)
  "Used to define a function that is curried. Just made this for
fun, in practice I don't think there's much use for it since
invoking the curried function is so atrocious syntactically. For
example:

Given this function:

(defun-curried lucas-test (x y z)
  (+ x y z))

To finally get a value, we must do:

(funcall (funcall (lucas-test 1) 2) 3)

*shudder*. I get the feeling there is a better way to do define
this curried macro. Like maybe if a function takes 4 arguments we
return a lambda with 4 optional arguments and if 0 get passed we
return the same lambda, if 1 gets passed then we return a lambda
of 3 arguments, if 2 get passed we return a lambda of 2
arguments, etc... Then you could pass in as many values as you'd
like without having to have all these funcall invocations."
  (unless (stringp docstring)
    (setq body (cons docstring body))
    (setq docstring nil))
  (bad-defun-curried-helper-fn name arglist docstring body))

(bad-defun-curried lucas-test (x y z)
  (+ x y z))

(funcall (funcall (lucas-test 1) 2) 3)

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
- https://url.spec.whatwg.org/

TODO: I think a nice addition to this function would be the
ability to \"normalize\" a url. For example google.com/hey and
google.com/hey/../hey are the same url even though they look
different. That makes my crawl site algorithm a little less than
perfect. I think maybe this should be a separate function:
https://en.wikipedia.org/wiki/URI_normalization

TODO: I think another thing this function doesn't check for is if
there is a relative link to another page and it ends in an anchor
tag like hello/there#anchor."
  (let ((href-urlobj (url-generic-parse-url href)))
    (cond
     ;; Sometimes anchor tags don't have an href attribute hence nil
     ;; could get passed to this function. We could check for nil
     ;; before passing it but it's convenient to accept such a value.
     ((null href)
      nil)
     ;; Checking for an absolute path check.
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
	(url-port base-urlobj)
	(car (url-path-and-query href-urlobj))
	nil
	nil
	t)))
     ;; Checking for a relative url. This pattern is probably not all
     ;; encompassing.
     ((string-match "^[a-zA-Z-_./]+$" href)
      (url-recreate-url
       (url-parse-make-urlobj
	(url-type base-urlobj)
	nil
	nil
	(url-host base-urlobj)
	(url-port base-urlobj)
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
	(url-port base-urlobj)
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

(defun reload-webserver (handlers port)
  "A utility function to reload a webserver. Useful for local
testing."
  (ws-stop-all)
  (ws-start handlers port))

(cl-defmacro with-webserver ((server handlers port) &rest body)
  "Starts a web server (binding it to the variable SERVER) with
HANDLERS and PORT, executes BODY, then stops the web server.
Original motivation for writing this was I wanted to unit test a
function which interacts with a web server and wanted to make
sure the web server got spun down after the test ran.

Note that `cl-defmacro' is used over `defmacro' for it's
destructuring capabilities. The structure of this macro is
similar to that of CL's with-open-file:
http://clhs.lisp.se/Body/m_w_open.htm"
  (declare (indent 1) (debug t))
  `(let (,server)
     (unwind-protect
	 (progn
	   (setq ,server (ws-start ,handlers ,port))
	   ,@body)
       (when ,server
	 (ws-stop ,server)))))

(defun array-last-item (arr)
  "Returns the last item of an array. Pretty trivial but I just
wanted it so I can be little more declaritive with my code.
Original motiviation for this function's creation was to find out
the port of a network process created by emacs which appears to
be in the last element of the array generated by the function:

(process-contact process :local)"
  (aref arr (1- (length arr))))

(defun network-process-local-port (network-server-process)
  "Returns the local port of a network process created by
`make-network-process'. Created because I wanted to spin up a web
server during a unit test and have the port be randomly assigned
which means I have to read the port back out during the test.

Looks like I could also maybe use the
`network-lookup-address-info' function here. Probably should...
eh, it's fine."
  (array-last-item (process-contact network-server-process :local)))

(comment
 (reload-webserver '(((:GET . "^/specific/path$") .
		      (lambda (request)
			(with-slots (process headers) request
			  (ws-response-header process 200 '("Content-type" . "text/html"))
			  (process-send-string process (concat "inside of a GET ONE with path " (cdr (assoc :GET headers)))))))
		     ((:GET . "^/wow$") .
		      (lambda (request)
			(with-slots (process headers) request
			  (ws-response-header process 200 '("Content-type" . "text/html"))
			  (process-send-string process (concat "inside of a GET TWO with path " (cdr (assoc :GET headers))))
			  (process-send-string process (pp-to-string (array-last-item (process-contact process :local))))
			  (process-send-string process (pp-to-string (ws-port request)))
			  (comment  (process-send-string process (pp-to-string request))))))
		     ((:GET . "^/request-object$") .
		      (lambda (request)
			(with-slots (process headers) request
			  (ws-response-header process 200 '("Content-type" . "text/html"))
			  (process-send-string process (pp-to-string request)))))
		     ((:POST . ".*") .
		      (lambda (request)
			(with-slots (process headers) request
			  (ws-response-header process 200 '("Content-type" . "text/html"))
			  (process-send-string process "inside of a post!")))))
		   9000)

 (setf (ws-handlers lucas-test) nil)

 (with-webserver (server '(((:GET . "^/specific/path$") .
			    (lambda (request)
			      (with-slots (process headers) request
				(ws-response-header process 200 '("Content-type" . "text/html"))
				(process-send-string process (concat "inside of a GET ONE with path " (cdr (assoc :GET headers)))))))
			   ((:GET . "^/wow$") .
			    (lambda (request)
			      (with-slots (process headers) request
				(ws-response-header process 200 '("Content-type" . "text/html"))
				(process-send-string process (concat "inside of a GET TWO with path " (cdr (assoc :GET headers))))
				(process-send-string process (pp-to-string (array-last-item (process-contact process :local))))
				(process-send-string process (pp-to-string (ws-port request)))
				(comment  (process-send-string process (pp-to-string request))))))
			   ((:GET . "^/request-object$") .
			    (lambda (request)
			      (with-slots (process headers) request
				(ws-response-header process 200 '("Content-type" . "text/html"))
				(process-send-string process (pp-to-string request)))))
			   ((:POST . ".*") .
			    (lambda (request)
			      (with-slots (process headers) request
				(ws-response-header process 200 '("Content-type" . "text/html"))
				(process-send-string process "inside of a post!")))))
			 t)
		 (message (pp-to-string (network-process-local-port (slot-value server :process))))
		 (while t (sit-for 1)))

 )

;; (slot-value (car ws-servers) :port)

;; The built in testing framework for emacs is called ERT. There is a
;; lovely info manual about it so give that a read for more detail.
;; You can run ERT tests by running "M-x ert" and specifying which
;; test to run. If the test fails you can press 'b' to view the
;; backtrace or press 'd' to rerun the test with debugging enabled.

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
  "Test out my function(s) which crawls a website. Motivation for
creating this unit test (besides learning about the unit testing
framework within emacs) was that I wanted to create lots of
different implementations for this website crawler (just to
experiment with different ideas and to explore what elisp can do)
but before doing that I wanted to get this test in place to
ensure that my different implementations were actually
equivalent.

An aside, I think there are two reasons I like writing tests:

1. It gives me confidence that I can refactor or try different
implementations of the code and things will still work.

2. It serves as up-to-date documentation for how to call the
function under test.

TODO: I would be interested in getting emacs to ask for any
arbitrary open port for these tests. Perhaps this could serve as
motivation:
https://github.com/eudoxia0/find-port/blob/master/src/find-port.lisp"
  ;; Defining a couple functions to generate the web server handlers
  ;; from the graph of links. I *think* you could say that the
  ;; webserver and the graph of links are "isomorphic" structures
  ;; because we have a function to transform the graph into the web
  ;; server (these functions) and another function (our crawl-site
  ;; function) to convert the webserver back into the links graph.
  ;; Might be wrong but it sure sounds fun to say:
  ;; https://en.wikipedia.org/wiki/Isomorphism
  (cl-flet* ((generate-html
	      (source-link outgoing-links)
	      (xmlgen `(html
			(body
			 (h1 (concat "Welcome to page: " ,source-link))
			 (a :href "https://google.com" "a link that should not be followed")
			 (p "welcome to the page!")
			 ,@(-map (lambda (outgoing-link)
				   `(div (a :href ,outgoing-link ,outgoing-link)
					 (hr)))
				 outgoing-links)
			 (div (p "bye now")
			      (a :href "https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html" "another link that should not be followed"))))))
	     (generate-handlers
	      (links-graph)
	      (ht-map (lambda (source-link outgoing-links)
			(let ((source-path (car (url-path-and-query (url-generic-parse-url source-link)))))
			  `((:GET . ,(concat "^" source-path "$")) .
			    (lambda (request)
			      (with-slots (process headers) request
				(ws-response-header process 200 '("Content-type" . "text/html"))
				(process-send-string
				 process
				 ,(generate-html source-link outgoing-links)))))))
		      links-graph)))
    ;; Initially there are no handlers for the web server because we
    ;; have to figure out what port the server is on so we can
    ;; generate the graph of links so that we can generate the
    ;; handlers.
    (with-webserver (server nil t)
      (let* ((port (network-process-local-port (ws-process server)))
	     (links-graph (->> '(("/" . ("/one" "/two"))
				 ("/one" . ("/" "/four"))
				 ("/two" . ("/one" "/three"))
				 ("/three" . ("/"))
				 ("/four" . ()))
			       (-tree-map (-partial #'concat "http://localhost:" (number-to-string port)))
			       (ht<-alist))))
	(setf (ws-handlers server) (generate-handlers links-graph))
	(should (equal? (crawl-site (concat "http://localhost:" (number-to-string port) "/"))
			links-graph))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Messing around with implementing lazy lists.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Something which feels like an alternative (and already
;; existing) way to do "infinite" stuff in elisp is this:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Generators.html
;; I'd be super interested in looking at that code too because I think
;; they do continuation stuff and I was reading about that online but
;; it would be cool to see a concrete implementation of it.
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
  "Gets the CAR of a lazy list L. Returns nil if L is nil just
like the car function."
  (if (null l)
      nil
    (car (thunk-force l))))

(defun lazy-cdr (l)
  "Gets the CDR of lazy list L. Returns nil if L is nil just like
the cdr function."
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

(defun lazy-reduce-from (reducing-fn init l)
  "The lazy equivalent of the dash feature's -reduce-from
function."
  (let ((acc init))
    (while l
      (setq acc (funcall reducing-fn acc (lazy-car l)))
      (setq l (lazy-cdr l)))
    acc))

(defun lazy-reduce (reducing-fn l)
  "The lazy equivalent of the dash feature's -reduce function."
  (if l
      (lazy-reduce-from reducing-fn (lazy-car l) (lazy-cdr l))
    (reducing-fn)))

(defun lazy-reduce-r-from (reducing-fn init l)
  "The lazy equivalent of the dash feature's -reduce-r-from
function which does the reduction on the list in reverse. Note
that the reducing function signature has the accumulator
parameter as the second argument instead of the first like the
other reducing function. Strictly speaking, it doesn't have to be
that way but it's done that way in haskell too with it's
\"foldr\" function so I assume folks thought it made more sense.
I suppose maybe they did it because you can envision the
accumulated value coming down from the right side of the list,
heading towards the original invocation of the reducing
function."
  (if l
      (funcall reducing-fn
	       (lazy-car l)
	       (lazy-reduce-r-from reducing-fn init (lazy-cdr l)))
    init))

(defun-multi-arity lazy-reduce-clojure
  "The lazy equivalent of a \"reduce\" function. Operates like
clojure's reduce implementation minus the ability of the reducing
function to call the function \"reduced\" to terminate the
computation early: https://clojuredocs.org/clojure.core/reduce

I remember when I initially saw clojure's reduce function (and
maybe I saw something similar elsewhere?) I thought (as I have
before with multiple arity functions) that the parameters were
confusing. I think it was the fact that the function was new to
me plus the fact that it could take up to 3 parameters and I kept
forgetting which ones went where and it didn't help that the
second parameter was different depending on if a third parameter
was passed. I also thought that the whole business of the
reducing function calling itself with no arguments when the list
is empty just felt weird. Part of me likes it a little more now
that I'm used to it but part of me also wants to keep it simpler.
Still, I just wanted to define this because it feels so cool to
use my home grown defun-multi-arity macro."
  ((reducing-fn l)
   (if l
       (lazy-reduce reducing-fn (lazy-car l) (lazy-cdr l))
     (reducing-fn)))
  ((reducing-fn init l)
   (let ((acc init))
     (while l
       (setq acc (funcall reducing-fn acc (lazy-car l)))
       (setq l (lazy-cdr l)))
     acc)))

;; TODO: This was an initial implementation of the lazy-reduce-from
;; function but it DOES NOT WORK even though I feel like it should.
;; The reason it doesn't work is because it seems that you cannot bind
;; a symbol XYZ to the value of another symbol XYZ in the lexical
;; closure. I feel like it should work though since you can do
;; something like (let ((x 1)) (let ((x (1+ x))) x)). I almost feel
;; like that should be changed. I wonder if I'm right or there's
;; something I'm missing.
(comment
 (defun lazy-reduce-from (reducing-fn init l)
   "The lazy equivalent of the dash feature's -reduce-from
function."
   (cl-loop for acc = init then (funcall reducing-fn acc (lazy-car ll))
	    for l = l then (lazy-cdr l)
	    while l
	    finally return acc)))

(defun lazy-nth (n l)
  "Get the Nth element from lazy list L. I would love to write it
in a tail recursive manner (since I always feel like those
implementations are clean), but unfortunately elisp doesn't do
tail call optimization so, for large lists, it quickly exceeds
the `max-lisp-eval-depth' variable and we'd error out."
  (cl-loop for ll = l then (lazy-cdr ll)
	   while ll
	   repeat n
	   finally return (lazy-car ll)))

(defun lazy-list-helper-fn (l)
  (if (null l)
      'nil
    `(lazy-cons ,(car l) ,(lazy-list-helper-fn (cdr l)))))

(defmacro lazy-list (&rest objects)
  (lazy-list-helper-fn objects))

(defun lazy-take (n l)
  (if (or (<= n 0)
	   (null l))
      nil
    (lazy-cons (lazy-car l) (lazy-take (1- n) (lazy-cdr l)))))

(defun lazy-list-to-list (l)
  "Converts a lazy list to a list."
  (cl-loop for ll = l then (lazy-cdr ll)
	   while ll
	   collect (lazy-car ll)))

(defun lazy-map (f l)
  (if l
      (lazy-cons (funcall f (lazy-car l))
		 (lazy-map f (lazy-cdr l)))
    nil))

(defun lazy-map-with-reduce (f l)
  "Implementing the map function with a reduce just for fun."
  (lazy-reduce-r-from (lambda (x acc) (lazy-cons (funcall f x) acc)) nil l))

(defun lazy-filter (pred l)
  "Filter list L, keeping only values for which PRED returns
true."
  (if l
      (if (funcall pred (lazy-car l))
	  (lazy-cons (lazy-car l) (lazy-filter pred (lazy-cdr l)))
	(lazy-filter pred (lazy-cdr l)))
    nil))

(defun lazy-filter-with-reduce (pred l)
  "Implementing the filter functionality with a reduce just for
fun."
  (lazy-reduce-r-from (lambda (x acc)
			(if (funcall pred x)
			    (lazy-cons x acc)
			  acc))
		      nil
		      l))

(defun complement (f)
  "Returns a function which returns the opposite truth value of
f. Inspired by https://clojuredocs.org/clojure.core/complement.
Turns out this function already existed in the dash library under
the name \"-not\" so I started using that instead. Still kept
this around for fun though.

This function is known as a combinator which is basically a
function that returns another function:
https://en.wikipedia.org/wiki/Combinatory_logic"
  (lambda (&rest args)
    (not (apply f args))))

(defun lazy-remove (pred l)
  "Opposite of `lazy-filter'."
  (lazy-filter (-not pred) l))

(comment
 ;; My original attempt at writing the lazy list macro. I want to
 ;; understand in more depth why it didn't work. I figured out a
 ;; solution by leaning on a recursive function which generates the
 ;; list and I understand that but I wonder why the macro itself
 ;; couldn't be recursive. Maybe this is helpful:
 ;; https://gist.github.com/nimaai/2f98cc421c9a51930e16#recursion
 (if (null objects)
      ()
    `(lazy-cons ,(car objects) (lazy-list ,(cdr objects))))
 )

;; TODO: I'm curious if I can define all this lazy functionality with
;; something like a "generator" instead of the lazy list
;; implementation.

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


;; TODO: In the file backquote.el the ` macro is defined. Originally I
;; thought it was a reader macro but it turns out to be just a
;; defalias: (defalias '\` (symbol-function 'backquote)). I wonder if
;; I can define other things like that. I'm kind of fascinated
;; honestly, using that backtick doesn't feel like calling a normal
;; function and yet it's possible apparently.


(cl-defun daily-amount-to-pay (total-rent num-people num-days-in-month &optional (multiplier 1.0))
  "Used to help let me know how much I have to pay daily
depending on where I'm staying in NYC."
  (* multiplier (/ total-rent num-people num-days-in-month)))

(defmacro if-let-alist (alist then &rest else)
  "I thought the `let-alist' macro was neat and had a situation
where I needed to check if an alist existed before grabbing
values from it. So, I made this macro to make my code a biiiiiet
more concise (but also because writing macros is fun).

An almost equivalent alternative to using this is to use the dash
library functions like `-when-let':

(-when-let ((&alist 'one 'two 'three) '((one . 1) (two . 2) (three . 3)))
  (+ one two three))

The main difference (besides of course having to explicitly
declare which values from the alist you want to use) is that if a
key doesn't exist, then the condition is deemed false:

(-when-let ((&alist 'does-not-exist) '((hello . 1)))
  'will-not-be-reached)"
  (declare (indent 2))
  `(if ,alist
       (let-alist ,alist
	 ,then)
     ,@else))

(defmacro when-let-alist (alist &rest body)
  "These condition+binding things often seem to have an `if' and
`when' variation so here is the `when' one! Funnily there doesn't
seem to be an `unless' variation that I can see (I've checked in
the dash library, clojure, and common lisp), I wonder why that
is. Just not as  "
  (declare (indent 1))
  `(if-let-alist ,alist (progn ,@body)))

(defun how-much-to-pay (start-date end-date person-staying-with)
  (let* ((places-to-stay '(((people . ("Emily" "Evan"))
			    (total-rent . 3400.0)
			    (multiplier . 1.0))
			   ((people . ("Jane" "Tori"))
			    (total-rent . 3000.0)
			    (multiplier . 1.25))
			   ((people . ("Joe"))
			    (total-rent . 2500.0)
			    (multiplier . 1.25))
			   ((people . ("Mom" "Dad"))
			    (total-rent . 0.0)
			    (multiplier . 0.0)))))

    (-when-let ((&alist 'people 'total-rent 'multiplier)
		(-find (lambda (place-to-stay)
			 (-contains? (assoc 'people place-to-stay) person-staying-with))
		       places-to-stay))
      (* multiplier
	 (/ total-rent
	    (1+ (length people))
	    28)))))

(comment
 
 (how-much-to-pay nil nil "Jane")
 (when-let-alist (-find (lambda (place-to-stay)
			     (-contains? (assoc 'people place-to-stay) person-staying-with))
			   places-to-stay)
      (* .multiplier
	 (/ .total-rent
	    (1+ (length .people))
	    28)))

 (decoded-time-add (iso8601-parse "2021-01-31")
		   (make-decoded-time))

 (lazy-list-to-list (lazy-take 31
			       (lazy-map #'decoded-time-day
					 (lazy-iterate (lambda (decoded-time)
							 (decoded-time-add decoded-time
									   (make-decoded-time :day 1)))
						       (iso8601-parse "2021-01-31")))))



 (when-let ((&alist 'people 'total-rent 'multiplier) (-find ....))
   )

 (-when-let ((&alist 'person 'hey) '((person . 100)
				     (hey . 2)))
   hey)



 (calendar-extract-month '(1 2 2021))
 (calendar-extract-day '(1 2 2021))
 (calendar-extract-year '(1 2 2021))

 (calendar-last-day-of-month 1 2021)
 (calendar-leap-year-p 2021)

 (calendar-generate-month 1 2021 0)

 (decoded-time-add (iso8601-parse "2021-01-31")
		   (make-decoded-time :month 1))(0 0 0 28 2 2021 nil nil nil)

 (current-time)(24981 30269 753478 0)

 (insert-date)

 (insert-timestamp)

 (current-time-string)

 (length (decode-time))

 (length (make-decoded-time :month 1))

 (parse-time-string "2021-01-311")
 (let ((decoded-time (iso8601-parse "2021-01-31")))
   (setf (decoded-time-month decoded-time)
	 (+ 1 (decoded-time-month decoded-time)))
   )

 ;; TODO: Given a range of dates and maybe just people I'm staying
 ;; with, it would be cool to calculate just how much I have to pay.

 `((:emily . ,(daily-amount-to-pay 3400.0 3))
   (:joe . ,(daily-amount-to-pay 2000.0 2 1.25)))((:emily . 40.476190476190474) (:joe . 44.642857142857146))
 )

Vim Training
============
Lucas Groenendaal

I think this is more of a reference document or something I would use if I was
giving a talk on vim. For a tutorial, I would recommend vimtutor.
Just type 

```shell
vimtutor
```

on the command line and the tutorial will start. You
could even type

```shell
vimtutor fr
```
if you want the tutorial in French :). Having
said that though, I would still recommend reading through this document. It
has some general vim knowlege which is nice to know and will make learning the
editor a lot more enjoyable.

And of course the internet is full of resources. These are not all tutorials
per se, but I think I've learned something from every one:

- http://www.viemu.com/a-why-vi-vim.html
- http://yannesposito.com/Scratch/en/blog/Learn-Vim-Progressively/
- http://stackoverflow.com/questions/1218390/what-is-your-most-productive-shortcut-with-vim (read the first guy's answer, you can't miss it)
- http://rc3.org/2012/05/12/the-grammar-of-vim/
- http://stackoverflow.com/questions/726894/what-are-the-dark-corners-of-vim-your-mom-never-told-you-about
- https://medium.com/@cole_peters/writing-better-code-with-vim-7c43877ad208
- http://xn--h4hg.ws/2013/12/19/how-to-learn-vim/

Read this if you want to try extending/customizing vim. The first part of it
is super fun and easy to follow. Really though, you should read it.

- http://learnvimscriptthehardway.stevelosh.com/


What is Vim?
------------
Vim is a text editor just like sublime or notepad++. It's first iteration was
called vi which was written in the mid 70s, so it's been around for quite a
while. Vim stands for Vi IMproved.


Vim: Mouseless Text Editing
---------------------------
There are a couple text editors out there, vim being one of them, which allow
and encourage you to ONLY use the keyboard when editing (so the mouse is never
used). The thought here being that you're more efficient when you're hands
stay on the keyboard. So when using vim you can, and are encouraged, to use
only the keyboard when editing.


Vim is a Modal Editor
---------------------
With most other text editors the keys you type show up as text in the
document, with vim this is not the case. Vim has different modes and keys will
do different things depending on which mode you're in. This is not as bad as
it sounds, but then again maybe I'm just used to it.

Three main modes:

        Insert  - Typing text like normal
        Normal  - Moving around your document
        Command - Bigger/Other things (saving and quiting are two of them)

### The Joy of Painting with Bob Ross (and vim)
Many people think vim's modal nature is odd or annoying, which is
understandable. When we type keys in a text editor we expect them to show up
in the document! Having to continually switch from mode to mode might seem
like too much work. I've seen many vim supporters counter this complaint by
comparing vim's way of editing to painting. I rather like that analogy and
since I couldn't say it better myself, I'll take a quote from Drew Neil's wonderful book
"Practical Vim":

    How much time do you reckon artists spend with their paint brushes in
    contact with the canvas? No doubt it would vary from artist to artist, but
    I'd be surprised if it counted for as much as half of the time painters
    spend at work.

    Think of all of the things that painters do besides paint. They study
    their subject, adjust the lighting, and mix paints into new hues. And when
    it comes to applying paint to the canvas, who says they have to use
    brushes? A painter might switch to a palette knife to achieve a different
    texture or use a cotton swab to touch up the paint that's already been
    applied.

    The painter does not rest with a brush on the canvas. And so it is with
    Vim.  Normal mode is the natural resting state. The clue is in the name,
    really.

    Just as painters spend a fraction of their time applying paint,
    programmers spend a fraction of their time composing code. More time is
    spent thinking, reading, and navigating from one part of a codebase to
    another. And when we do want to make a change, who says we have to switch
    to Insert mode? We can reformat existing code, duplicate it, move it
    around, or delete it. From Normal mode, we have many tools at our
    disposal.

Whether or not you agree with the above analogy doesn't much matter. The
important thing is to appreciate the vim way of doing things. Understanding
this "vim mindset" will make the editor easier to learn.

On a side note, a painter I know agrees with the above quotation! She says
that she spends more time planning and thinking rather than actually putting
brush to canvas.

### Vim Editing in a Nutshell

1. Get to where you want in normal mode.
2. Enter insert mode to make the changes.
3. Go back to normal mode and repeat.

               +--> Normal -+
              /              \
        ESC  |                |  a,A,i,I,...
              \              /
               +-- Insert <-+


Starting/Saving/Exiting Vim
---------------------------
### Start Editing

From the command line:

```shell
# Starts vim with the file <file_name> loaded
vim <file_name>
```    

Within vim:

```vim
" Opens a file inside of vim. Can be shortened to just :e <file_name>
:edit <file_name>
```

### Saving
```vim
" Saves the current file. Can be shortened to just :w
:write
```

### Exiting

```vim
" Exits vim. Can be shortened to just :q
:quit
" Shorthand for saving then quiting out of vim
:wq
```

Entering Insert Mode (Brush to the canvas)
-----------------------------------------
These commands enter insert mode from normal mode.

    i - Enter insert mode at cursor
    I - Enter insert mode at the first non-blank character of the line
    a - Enter insert mode after the cursor
    A - Enter insert mode at the end of the line
    o - Open a line below the cursor and enter insert mode
    O - Open a line above the cursor and enter insert mode


Entering Normal Mode (Picking up the brush)
-------------------------------------------
These commands enter normal mode from insert mode.

    <ESC>
    <C-[> (Control-[)
    <C-c> (Control-c)


Moving Around the Document (inside normal mode)
-----------------------------------------------
### Arrow Key Equivalents
        k 
        ^ 
    h <   > l
        v 
        j 
Never have to leave home row!
The reason for using hjkl instead of jkl; (where our hands normally rest on
the keyboard) is historic:
    http://www.catonmat.net/blog/why-vim-uses-hs-arrow-keys/

### Bigger Motions (Basic)
    w - Forward to the beginning of the next word
    W - Forward to the beginning of the next WORD
    b - Backward to the beginning of a word
    B - Backward to the beginning of a WORD
    e - Forward to the next end of word
    E - Forward to the next end of WORD

word = a squence of letters, digits, and underscores (think your average
       variable name)
       
WORD = a squence of non whitespace characters

### Cooler Motions
    f<c> - Forward to character <c>
    F<c> - Backward to character <c>
    t<c> - Forward until character <c> (so it stops one character before <c>)
    T<c> - Backward until character <c>
    ;    - Repeats the last of these commands
    ,    - Repeats the last of these commands in the opposite direction

### Searching
    /  - Searches forward
    ?  - Searches backward
    *  - Searches forward for the EXACT word under the cursor
    #  - Searches backward for the EXACT word under the cursor
    g* - Searches forward for the word under the cursor
    g# - Searches backward for the word under the cursor
    n  - Repeats the last search
    N  - Repeats the last search in the opposite direction

### Scrolling
All the above commands directly move the cursor. These scroll the screen:

    <C-d> - Scroll the window down half a page
    <C-f> - Scroll the window down a full a page
    <C-u> - Scroll the window up half a page
    <C-b> - Scroll the window up a full page


Undoing/Redoing Changes
---------------
    u     - Undo the last change
    <C-r> - Redo the last undo


Core Editing Commands (Copy/Paste/Delete)
-----------------------------------------
    d<motion>  - Deletes (technically cuts) text
    y<motion>  - Copies (yanks) text
    c<motion>  - Deletes text and goes into insert mode (i.e Changes text)
    v<motions> - Visually selects an area (like highlighting) and allows you
                 to act on it.
    p          - Pastes text after cursor
    P          - Pastes text before cursor
    x          - Deletes the letter under the cursor
    X          - Deletes the letter before the cursor

    Some shorthands:
    dd        - Deletes the entire line
    yy        - Copies the entire line

\<motion\> = Any of the motions we have discussed! i.e w,W,h,j,k,l,f<c>...


The Beauty Of Vim
-----------------
The commands you type are sort of like a little language unto themselves. What
you type when editing often corresponds to what you're thinking.

### Beauty Level = 1
Most commands can be preceded with a number. The command will then be executed
that many times.

    [n]command - Where n is a number.

So ```10j``` will move you down 10 lines, ```3w``` will move you forward 3 words, ```2ft``` will
move you to the second occurrence of the letter 't', etc... In all honesty this
feature is not so useful in practice but it has it's moments and it's nice
that almost all commands adhere to this pattern. Something pretty neat is that
this also works for commands that put you into insert mode! So:

    40ia<ESC>

(where \<ESC\> means actually hitting the escape character) will insert 40 a's
into the document.

### Beauty Level = 2
The commands ```d```, ```y```, and ```c``` in the "Core Editing Commands" section are
technically called operators. After you type, say ```d```, it will wait for you to
enter a motion. The operator will then act on said motion.  So typing ```dw```
deletes the next word, ```yfx``` yanks from the cursor to the 'x' character including
'x', ```dt.``` deletes until the '.' character (not including the '.' character).
Isn't that cool???

Commands such as this is where the real idea of an editing language comes in.
Just look at a command like:

    dt,

The ```d``` in ```dt,``` serves as the verb and the ```t,``` serves as the noun. If you put
them together it reads: 

    Delete Till the character ,

The above command is terse as well as mnemonic, it's quite elegant. This
command structure is even more powerful when you realize that any operator can
be used with any motion.  So, if instead of deleting, you want to yank (copy)
until the next ',' then the command is ```yt,```. If you want to change the text
until the next ',' then the command is ```ct,```. Realizing this makes learning vim
a lot easier. You're no longer learning a series of disjoint commands, you're
learning a language.


Why Should I Bother?
--------------------

Pretty much everything in this document covers just the basics of vim so you
can *start* editing text. Since you have to learn so much before you can even
start editing you might wonder, "Why would anyone go to the trouble to learn
this editor?". That is a very fair statement. Many vim users claim that you'll
gain speed, which I agree with to some extent. But the reason I use vim is
because it makes text editing more fun/less tedious. [This article](https://wrongsideofmemphis.wordpress.com/2013/03/27/vim-speed-is-not-really-the-point/) sums it up
nicely.

A couple other reasons to learn at least the basics:

1. vim is literally everywhere and chances are you will have to use it at some point.
2. Other programs like 'less' and the 'man' pages use similar keybindings.
3. In general, it's just fun to learn new things and as far as text editing goes, vim is pretty 'new'.


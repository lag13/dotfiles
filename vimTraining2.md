More Vim Topics
===============
Lucas Groenendaal

This document contains a sampling of intermediate/advanced/very useful vim
commands.


The :help Command
-----------------
Vim has extensive documentation built into the editor. Just type the command:

    :help

And you'll be greeted with a table of contents of vim topics. Generally when
you invoke this command you'll be searching for help on a specific topic so
the command will be:

    :help my_awesome_topic

Hopefully, running the above command will get you where you want. Also
remember to use tab completion as you type! Honestly though, most of the time
it will be simpler to just google what you're looking for. Still though,
having the documentation at you're disposal is extremely convenient.

If you add some notation to 'my_awesome_topic' then vim will try to search
specific areas of the help pages. Here are some examples:

    - my_awesome_topic - Just typing what you are looking for will try to look
      for a Normal mode command called my_awesome_topic. For example, typing
      ':help zt' will find help on the normal mode command 'zt'.
    - 'my_awesome_topic' - This will look for a setting, which you can set
      with the ':set' command. For example, :help 'number'.
    - i_my_awesome_topic - Looks for a command in Insert mode called
      my_awesome_topic. For example, :help i_ctrl-r.
    - v_my_awesome_topic - Looks for a command in Visual mode called
      my_awesome_topic. For example, :help v_o.
    - :my_awesome_topic - Looks for a command in Command mode called
      my_awesome_topic. For example, :help :set.

In the help pages you'll often see words surrounded in '|' characters. These
are essentially hyperlinks to other areas of the help documentation. In vim
they are known as tags. For example here is a quote from the help pages:

    |quickref|	Overview of the most common commands you will use

You can utilize those tags by using the commands:

    <C-]> - Goes to where the tag is pointing.
    <C-t> - Jumps back to where you were before taking the tag.

So in the above quote, if I put my cursor anywhere in the text '|quickref|'
and hit <C-]> then I would be taken to the 'quickref' section of the help
pages.


How Does Undo Work?
-------------------
I don't know exactly how other editors determine, "Okay, that was one undo
chunk and now we're starting another". Vim has a rather nice undo mechanism
which arises because of it's modal'ness. In vim, one 'change' is considered to
be one undo. A change is roughly defined as:

    - While remaining in insert mode, ALL text you write will be considered 1
      change.
    - Otherwise, whenever the document changes in any way then that is
      considered one change.

The big thing to keep in mind is that the entire time spent Insert mode will
only count as one change. So if I enter insert mode, type 5 paragraphs, and
then exit insert mode, those 5 paragraphs will be erased by one undo command.
Some people don't like that because it feels like you're undoing too much, but
once you're aware of it, it becomes a rather nice feature. I'll oftentimes
enter Insert mode, type out a sentence or a general thought, and then exit
Insert mode. Now my complete thought is undoable. If I don't like it then I
can just hit 'u' and start writing something else.



The Dot Command
---------------
The '.' command repeats the last change made to the document. A change has the
same definition as the one given in 'How Does Undo Work?'. It doesn't sound
like much, but it's actually a very powerful command.


Macros
------
You can record a series of keystrokes and then replay them. That recorded series of
keystrokes is called a macro. This is very useful when making a series of
repetative edits.


Text Objects
------------
Extremely powerful.


Use / Or ? In Operator-Pending Mode
-----------------------------------
Just something to keep in mind.


The Jump List
-------------
Whenever you do a command that is considered a 'jump' your old position is
kept track of. You can navigate this jump list (so visit old places you've
been) by using the commands:

    <C-o> - Go backwards through the jump list.
    <C-i> - Go forwards through the jump list.


The Change List
---------------
Similar to the jump list, a list of positions where changes were made is kept.
You can navigate this list with the commands:

    g; - Go backwards through the change list.
    g, - Go forwards through the change list.

Another nifty command is the gi command which will put you in Insert mode at
the last place you exited Insert mode.


How Do I Get Around The Document Efficiently?
---------------------------------------------
After coming from an editor which relies on the mouse to move around, using
vim, assuming you don't use the mouse, is going to feel restrictive. Part of
that gets better with time because with time you'll learn more ways to get
around the document. To be honest though, a lot of moving around in vim really
just comes down to searching. If you see a spot on screen that you would like
to reach, search for it.

If you're really missing the mouse, you can enable it in vim (assuming vim was
built with mouse support) by running the command:

    :set mouse=a

By default, highlighting text with the mouse will put you in Visual mode so
typing won't overwrite the selected area as you might expect it to. If you
want to be able to highlight and then overwrite text like other editors run
this command:

    :set selectmode=mouse


Word And Line Completion
------------------------


zz, zt, zb Commands
-------------------
Positioning the screen relative to the cursor is very important. With the
mouse this is simpler to do.

    zz - Moves the screen so the cursor in the center of the screen
    zt - Moves the screen so the cursor is at the top of the screen
    zb - Moves the screen so the cursor is at the bottom of the screen


Substitute Command
------------------
The command:
    
    :s[ubstitute]/old/new/flags

Will substitute text. By default it only substitutes on the current line.


Working With Multiple Files (Buffer Switching)
----------------------------------------------


Working With Windows/Splits
---------------------------


Switch Two Lines (ddp) And Two Characters (xp/dlp)
--------------------------------------------------
The 'd' command technically cuts text. This is often irritating but it
sometimes has some nice consequences. In this case, typing 'ddp' will swap two
lines and typing xp (or equivalently dlp) will swap two characters.


Saving Sessions: The mksession command
--------------------------------------
It seems that most other editors, by default, save the list of files you had
open when you closed the editor. This can be nice because when you open your
editor again, you can pick right up where you left off. Vim doesn't do this by
default. When you quit vim you lose everything you've been working on. The way
to tell vim to save it's current state is with the mksession command. For
example you can run this command:

    :mksession mySession.vim

This will save the current working state of your vim instance into file called
mySession.vim. The next time you want to start your project you can either
launch vim like this:

    vim -S mySession.vim

Or you can start up vim and 'source' that session file like this:

    :source mySession.vim

Now you'll be right back where you left off.


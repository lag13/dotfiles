More Vim Topics
===============
Lucas Groenendaal

This document contains a sampling of intermediate/advanced/very useful vim
commands.

The :help Command
-----------------
Vim has extensive documentation built into the editor. Just type the command:

```vim
:help
```

And you'll be greeted with a table of contents of vim topics. Generally when
you invoke this command you'll be searching for help on a specific topic so
the command will be:

```vim
:help my_awesome_topic
```

Hopefully, running the above command will get you where you want. Also
remember to use tab completion as you type! Honestly though, if you're not sure what you're looking for it is oftentimes simpler to use google. Still though,
having extensive documentation at you're disposal is extremely convenient.

If you add some notation to the argument ```my_awesome_topic``` then vim will try to search
specific areas of the help pages. Here are some examples:

- my_awesome_topic - Just typing what you are looking for will try to look
      for a Normal mode command called my_awesome_topic. For example, typing
      ```:help zt``` will find help on the normal mode command ```zt```.
- 'my_awesome_topic' - This will look for a setting, which you can set
      with the ```:set``` command. For example, ```:help 'number'```.
- i_my_awesome_topic - Looks for a command in Insert mode called
      my_awesome_topic. For example, ```:help i_ctrl-r```.
- v_my_awesome_topic - Looks for a command in Visual mode called
      my_awesome_topic. For example, ```:help v_o```.
- :my_awesome_topic - Looks for a command in Command mode called
      my_awesome_topic. For example, ```:help :set```.

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

- While remaining in insert mode, ALL text you write will be considered one change.
- Otherwise, whenever the document changes in any way then that is considered one change.

The big thing to keep in mind is that the entire time spent Insert mode will
only count as one change. So if I enter insert mode, type 5 paragraphs, and
then exit insert mode, those 5 paragraphs will be erased by one undo command.
Some people don't like that because it feels like you're undoing too much, but
once you're aware of it, it becomes a rather nice feature. I'll oftentimes
enter Insert mode, type out a sentence or a general thought, and then exit
Insert mode. Now my complete thought is undoable. If I don't like it then I
can just hit ```u``` and start writing something else.

The Dot Command
---------------
The ```.``` command repeats the last change made to the document. A change has the
same definition as the one given in 'How Does Undo Work?'. It doesn't sound
like much, but it's actually a very nifty command. For example, say I had these 4 lines of C
code:

```C
int f0 = 0
int f1 = 1
int f2 = f0 + f1
int f3 = f2 + f3
```

Notice that I forgot to add the semicolons at the end of the lines! Say my cursor was on the first line, 
then I could add the semicolon by typing ```A;<ESC>```. Now, adding a semicolon to the end of
the line is considered one change and the ```.``` command will repeat it. So I can
add semicolons to the rest of the lines by typing ```j.j.j.```. Super quick.

Macros
------
You can record a series of keystrokes and then replay them. That recorded series of
keystrokes is called a macro. This is very useful when making a series of
repetative edits. To start recording a macro you type ```q``` followed by the register
you want to record it in. For example ```qa``` will start recording your keystrokes and
save them to the register ```a```. To end the macro press ```q``` again. To replay the macro,
hit ```@``` followed by the name of the register. In the above case it would probably be ```@a```.
For example, say you had a list of people's names in the format "LastName, FirstName" and you
wanted it to say "FirstName LastName":

    Federer, Roger
    Pope, Tim
    Emmanuel, Tommy
    
Put the cursor on "Federer" and execute the command ```qa0dWA <ESC>phDq```. Then you can just
type ```@a``` on the other lines to execute the same sequence of commands which results in the
same change namely switching the first and last names around.

Text Objects
------------
I think text objects are what make people really fall in love with vim. They are a very powerful concept.
In a nutshell, some of vim's commands are called operators (see ```:help operator``` for a list of them).
After typing an operator, such as ```d```, vim will
wait until you enter a motion *or* a text object. The operator will then act on the region defined by
the motion or text object. For example, the motion ```w``` moves to the beginning
of the next word and so typing ```dw``` will delete from the cursor until the start of the next word. But what
if you want to delete the current word the cursor is on regardless of the cursor position? In that case you
could type ```diw``` which should be read as "Delete Inside Word". The ```iw``` in this command is considered
the text object. Some other text objects are ```i"```, ```i(```, and ```i{```. See ```:help text-objects```
for more.

Use / Or ? In Operator-Pending Mode
-----------------------------------
Just something rather nice to keep in mind. You can type an operator followed by a search and the operator
will act from the cursor position to the start of the search match. For example ```d/hello<CR>``` will delete
from the cursor position to the start of the word "hello". Note that ```<CR>``` in the above example
means hitting the return key.

The Jump List
-------------
Whenever you type a command that is considered a 'jump' your old position is
kept track of in something called the "jump list". You can navigate the jump list 
(so visit old places you've been) by using the commands:

    <C-o> - Go backwards through the jump list.
    <C-i> - Go forwards through the jump list.

See ```:help jumplist``` for more information.

The Change List
---------------
Similar to the jump list, a list of positions where changes were made is kept.
You can navigate this list with the commands:

    g; - Go backwards through the change list.
    g, - Go forwards through the change list.

Another nifty command is ```gi``` which will put you in Insert mode at
the last place you exited Insert mode. See ```:help changelist``` for more information.

How Do I Get Around The Document Efficiently?
---------------------------------------------
After coming from an editor which relies on the mouse to move around, using
vim, assuming you don't use the mouse, is going to feel restrictive. Part of
that gets better with time because with time you'll learn more ways to get
around the document. To be honest though, a lot of moving around in vim really
just comes down to searching. If you see a spot on screen that you would like
to reach, search for it.

If you're really missing the mouse, you can enable it in vim (assuming vim was
built with mouse support) by running the command ```:set mouse=a```

By default, highlighting text with the mouse will put you in Visual mode so
typing won't overwrite the selected area as you might expect it to. If you
want to be able to highlight and then overwrite text like other editors run
this command ```:set selectmode=mouse```

Word And Line Completion
------------------------
Type ```<C-p>``` or ```<C-n>``` while typing a word to try and complete that word.
You can scroll up and down the completion options with the ```<C-p>``` and ```<C-n```
commands. Typing ```<C-x><C-l>``` in insert mode will try to complete the entire line!
This is one of those vim features that I think is really cool. If you are typing a line
of code and know that this line of code appears in one of your other open buffers, type
```<C-x><C-l>``` to get that line.

zz, zt, zb Commands
-------------------
Positioning the screen relative to the cursor is very important. It allows
you to focus the screen on what you want to see while keeping the cursor in
the location you want to work.

    zz - Moves the screen so the cursor in the center of the screen
    zt - Moves the screen so the cursor is at the top of the screen
    zb - Moves the screen so the cursor is at the bottom of the screen

Substitute Command
------------------
The command:
    
    :[range]s[ubstitute]/old/new/flags

Substitutes text. By default it only substitutes on the current line. A couple examples:

```vim
" Replaces all instances of 'yes' to 'no' on the current line
:s/yes/no/g
" Replaces all instances of 'false' to 'true' in the whole file
:%s/false/true/g
```

Working With Multiple Files (Buffer Switching)
----------------------------------------------
The ```:buffer <buffer>``` command switches to a buffer. It can be shortened to just
```:b```. It can tab complete on the buffer you're trying to switch to. The command
```<C-^>``` switches to the file previously looked at.

Working With Windows/Splits
---------------------------


Switch Two Lines (ddp) And Two Characters (xp/dlp)
--------------------------------------------------
The ```d``` command technically cuts text. This is often irritating but it
sometimes has some nice consequences. In this case, typing ```ddp``` will swap two
lines and typing ```xp``` (or equivalently ```dlp```) will swap two characters.

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


" Funny Vim Commands:
" bad[d] - Adds a file name to the buffer list without loading it.
" col[der] - Goes to an older quickfix list.
" nun[map] - Removes the mapping for a normal mode command. A quote from the
" help pages: 'can also be used outside of a monastery'

" Mapping Information:
" A mapping enables you to bind a set of Vim **commands** to a sequence of
" keys. I used to think of pre-defined Vim commands as default mappings but
" that is not true. A mapping is something a user makes while something like
" "n" in Normal mode, assuming it has not been mapped, is considered a Vim
" command. You can view mappings by running the command:
"     :map {lhs}
" If {lhs} is given then it will list all mappings that start with it.
" Otherwise it will list all mappings. There's also a function called
" maparg({lhs}) which returns the {rhs} of a mapping given the {lhs}. I've
" also seen the function hasmapto({rhs}) being used. This function returns
" true of there is a mapping which contains {rhs} in it's {rhs}.

" :[noremap]map	    Normal, Visual and Operator-pending
" :v[noremap]map    Visual and Select
" :x[noremap]map    Visual
" :n[noremap]map    Normal
" :o[noremap]map    Operator-pending
" :[noremap]map!    Insert and Command-line
" :i[noremap]map    Insert
" :c[noremap]map    Command-line

" <expr> in a mapping evaluates the {rhs} as an expression. The return value
" of that expression is used as the key sequence to execute. It's a pretty
" interesting option to use.

" Seemed like a nice quick vimscript tutorial
" http://andrewscala.com/vimscript/

" Creating A Text Object:
" 1. Define the operator-pending mapping so you can operate on the text object
" when using operators.
" 2. Define a visual mapping so you can select the text-object in visual mode.
" A thought I just had, visual mode is kind of like a prolonged
" operator-pending mode. The 'normal' execution for operator commands is:
"
"   operator -> operator-pending
"
" But visual sort of turns that sequence on it's head and you can think of it
" as:
"
"   operator-pending/visual -> operator.
"
" 3. If makes sense to do so, define a normal mapping which will move the
" cursor in adherance with the text-object.

" Creating An Operator:
" 1. Define a normal mapping so we can use this mapping from normal mode.
" 2. Define a visual mapping so we can visually select the exact text we want
" and have the operator operate on it.

" Basic Settings {{{

let mapleader = ","
let maplocalleader = '\'
" We want vim not vi!
set nocompatible
" This exists so the .vim/ directory can still be named 'vim' even on a
" windows machine. The 'exists' check is so I can re-source my .vimrc and not
" mess up 'runtimepath'.
if !exists("g:set_windows_rtp")
    if has("win32") || has("win64")
        set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
        let g:set_windows_rtp = 1
    endif
endif
" With pathogen as my package manager, all plugins can be kept in their own
" folders in the .vim/bundle directory.
execute pathogen#infect()
" Just running: 'filetype on' allows for just the detection of different file
" types. That means that every time a file is edited, vim will try to
" recognize the type and set 'filetype' accordingly. Setting 'filetype' will
" then trigger the 'FileType' event. Running the command 'filetype plugin on'
" does the same thing as just 'filetype on' and also enables that when a file
" is edited, it's plugin file is also loaded. I'm unsure why this wouldn't
" happen by default? Wouldn't you always want to load any plugin files? There
" is also a command: 'filetype indent on' which must mean that there are
" indent sort of plugin files you can enable.
filetype plugin indent on
" Normally this is turned off, and the effect it has is that everytime a
" buffer is 'abandon'ed (i.e there is no window on the buffer) the buffer will
" be unloaded. The only effect I know of that that has is that the undo tree
" for that buffer will be lost. By turning this on, vim will handle buffers
" like other editors do.
set hidden
" Let backspace behave 'normally'.
set backspace=indent,eol,start"
" Can't live without it.
set showmode
set ttimeoutlen=30
set modelines=0
" Tabs will appear 4 spaces wide.
set tabstop=4
" The > and < commands will add 4 spaces.
set shiftwidth=4
" > and < operators will round their indent to a multiple of 'shiftwidth'. So
" this setting makes these operators behave just like when you insert a Tab
" character in insert mode.
set shiftround
" When in insert mode <BS> will delete 4 spaces and Tab will insert 4 spaces.
" If expandtab is NOT enabled then inserted Tab characters will be converted
" to an actual Tab character, instead of spaces, when the number of spaces
" equals 'tabstop'. This setting helps make it feel as if we're working with
" actual tabs even if it is only spaces.
set softtabstop=4
" Insert spaces instead of tabs.
set expandtab
" Copies the indent from the previous line when using o, O, or <CR>.
set autoindent
" A must have, highlight the search as you type.
set incsearch
" Also good, necessary in my opinion. It highlights your search results.
set hlsearch
" Numbered lines
set number
" Close all folds when opening a file
set foldlevelstart=0
" Always show the tab line
set showtabline=2
" The status line will always appears in all windows
set laststatus=2
" Configure the status line
set statusline=
set statusline+=[%n]      " Buffer number
set statusline+=\ [%<%f]  " Current file
set statusline+=\ %m      " Modified flag
set statusline+=\ %y      " File type
set statusline+=\ %{&ff}  " File format
set statusline+=\ %l/%L   " Current line num out of total
set statusline+=\ %P      " Top/Bottom and percentage through file
" Characters which are used to fill empty space in the status line,
" vertical/horizontal separators, and folded lines
set fillchars=vert:\|,fold:-,stl:\ 
" TODO: look into setting the 'title' option.
" Memory is cheap, let's bump up the amount recorded commands.
set history=500
" When tab completing, complete the longest possible prefix and display a list
" of possible completions.
set wildmenu
set wildmode=list:full
set wildignore=""
" Briefly jump to matching paren, bracket... when it's closing character is
" inserted. TODO: It seems that when this is enabled vim will beep if we
" insert an unmatched item. Look into this.
set showmatch
" How quickly in tenths of a second the 'showmatch' jump happens.
set matchtime=2
" Makes it so commands that move the cursor up and down (like gg and G) try to
" retain the same column the cursor was originally in.
set nostartofline
" Long lines will wrap rather than run offscreen.
set wrap
" Wrapped text will break on a character in 'breakat' rather than at the last
" character that fits on screen.
set linebreak
" Character to display before each broken line.
set showbreak=...
" When joining a line that ends in a '.', '?' and some others only insert one
" space instead of two.
set nojoinspaces
" Configure completion a bit. In particular, the menu will still show for one
" match, and some information about where the matches are coming from will be
" in the menu.
set completeopt=menuone,preview
" When typing text, comments will automatically wrap, when hitting return
" inside of a comment the 'comments' options wil be inserted at the begginning
" of the next line, I'm not entirely sure what 'q' does I think it might
" format comments separately, and correctly format numbered lists.
" nqrowcb - seems to be happening in the php files.

" TODO: I noticed that on the work computer this was getting set to
" something else when editing my vimrc. Does file-type related code run
" after the normal vimrc stuff?
set formatoptions=crqn
" Highlights the current line of the cursor.
set cursorline
" Don't update the screen while executing macros and a couple other things
set lazyredraw
" Saw it in Steve Losh's .vimrc file. From reading the help doc on this option
" it says it 'Improves the smoothness of redrawing...' and 'Indicates a fast
" terminal connection.' I'm not exactly sure how it works but I know I have a
" fast terminal connection so enabling it shouldn't hurt.
set ttyfast
" Toggle the 'paste' setting.
set pastetoggle=<F10>
" Only create swap files in my home directory. My pessimistic self still wants
" to keep them around but all they've really done is cause slight annoyances.
set directory=~/.vim

" }}}

" Color Scheme and Terminal Specific Settings {{{

" I've used vim for a while and for the first time have started caring more
" about how it looks. I never thought the default colorscheme looked that bad
" (and still don't) so I never bothered to change it. But I have some friends
" who use sublime and man does that editor look nice. It got me thinking about
" altering vim's color scheme, just for a change of pace. This is what I now
" know of colorscheme's in vim after trying to get the 'solarized' colorscheme
" to work. 

" The big reason that many graphical editors look so great is because they
" support millions of colors. Graphical implementations of vim (gvim and
" MacVim being two) are no different. This is sadly not the case when running
" vim through the terminal. The reason for this is that vim relies on the
" terminal emulator for all its display capabilities. So if the terminal only
" supports 8 colors well then too bad, vim will only be able to display 8
" colors.

" At the moment, this what I understand about how terminal colors work. Almost
" all terminal emulators are capable of displaying 16 different colors at the
" same time. However, you can configure those 16 colors to be ANY color you
" want. So in a sense, terminal emulators are capable of displaying any color,
" but only 16 at the same time. Many terminals (probably most) are also
" capable of displaying 256 colors simultaneously. This will, obviously, make
" most colorschemes look nicer because there are more available colors. Unlike
" the 16 base colors for the terminal, I don't believe you are capable of
" remapping any of these 256 colors. The only web page I could find related to
" this question was this page:
" http://stackoverflow.com/questions/25296985/can-i-change-the-256-color-mappings-in-iterm2-or-terminal.
" If the terminal supports 256 colors, I am currently unsure of how that
" affects the 16 ansii colors. Do the 16 ansii colors get ignored and the same
" set of 256 colors are always used (I don't think so)? Are the first 16
" colors the ansii colors and the other 240 are always be the same (I could
" see this being true)?

" Now onto my setting up the 'solarized' colorscheme. This was a colorscheme
" that kept popping up in my searches and it looked great. So imagine my
" confusion when I ran ':colorscheme solarized' and my terminal looked
" horrendous, it was very dissapointing. This is a nice ST answer I found
" online regarding this problem and its solution:
" http://superuser.com/questions/423709/vim-how-to-configure-solarized-colorscheme-in-konsole
" In short, solarized will only 'look good' when you create a terminal theme
" which defines the 16 ansii terminal colors exactly as specified in the
" documentation. At the moment I'm unsure of how to create my own terminal
" themes in an exact way (i.e specifying exact hexadecimal color values). But
" there are lots of theme's online (including ones for solarized). The themes
" for the osx terminal app seem to be an xml file with the extension
" '.terminal'. All you have to do is 'import' one of those files into the
" terminal and you're ready to go. After writing this I found this site which
" seems VERY promising in creating my own themes: http://terminal.sexy/.

" So, to reiterate, making the solarized colorscheme look as intended involved
" creating a terminal theme who's 16 ansii colors matched what the author
" intended. A not as good alternative, which requires much less work, is to
" set the variable g:solarized_termcolors to a value of 256. This tells
" solarized that you have not configured your terminal to use the specified
" palette and it will try to pick colors out of those 256 that are close to
" the originals.

" In conclusion, setting up colorschemes on vim for the terminal can take a
" bit of work. At the easy end of the spectrum all you have to do is download
" the colorscheme and run ':colorscheme yourDownloadedColorscheme'. On the
" harder end of the spectrum, you have to download a new terminal theme along
" with the colorscheme and whenever you want to use that new colorscheme,
" you'll have to use the appropriate terminal theme. It's a bit of a pain but
" that's the price you pay when you rely on another program for your display
" capabilities.
syntax enable

if has('gui_running')
    set guifont=Courier_New:h10:cANSI
    " Make distance between lines as small as possible.
    set linespace=0
    " Consider adding these to remove the menu and toolbar when running gvim
    " set guioptions-=m
    " set guioptions-=T
    set background=dark
    colorscheme solarized
elseif &term ==# 'win32'
    " I'm using Git-Bash for windows and for some reason when I run vim in it
    " it can't find the 'shine' colorscheme and it errors out which is
    " irritating. By adding 'silent!' it won't bother me with that error
    " message.
    silent! colorscheme shine
    set nocursorline
elseif has('unix')
    let uname = system('echo -n "$(uname -s)"')
    set background=light
    if uname !=# "Darwin"
        let g:solarized_termcolors = 256
        set background=dark
    endif
    silent! colorscheme solarized
endif

" }}}

" Autocommands {{{

augroup general_autocommands
    autocmd!
    " Makes it so I'll be able to see what text I was editing previously even if
    " it was inside a fold.
    autocmd VimEnter * normal! zv
    " Normally, when you execute a :source command it will re-highlight the
    " current search item, assuming that 'hlsearch' is turned on. I don't like
    " this. Originally I thought this autocommand would solve my problem:

    " autocmd SourceCmd * nohlsearch

    " But it turns out that SourceCmd is a special event called a Cmd-event.
    " According to the documentation, if I define an autocommand using this
    " event it is expected that my autocommand will do all the sourcing
    " functionality. So this definitely does not work.

    " Saw this on http://www.bestofvim.com/ (wish there was more on that
    " site...) and thought I'd try it out. So now, whenever I save a vim
    " file, it will automatically source it for me.
    " autocmd BufWritePost *.vim,*vimrc* source %
augroup END

" }}}

" Plugin Configuration {{{

" Plugins To Checkout:
" 1. Viewing man pages inside of vim
" 2. ctags - Tags
" 3. NERDtree or vinegar or vimfiler -  File explorer
" 4. clang complete for autocompleting C/C++ code
" 5. paredit http://danmidwood.com/content/2014/11/21/animated-paredit.html.
" 6. https://github.com/tpope/vim-eunuch, seems to have some useful stuff
" 8. https://github.com/tpope/vim-unimpaired - Many mappings starting with '['
" for moving around different lists.
" 9. http://vimawesome.com/plugin/youcompleteme - Code completion
" 10. http://vimawesome.com/plugin/ultisnips-forever-and-always - Snippets
" 11. http://vimawesome.com/plugin/syntastic - Syntax checking
" 12. http://vimawesome.com/plugin/vim-sneak - Searching on 2 letters
" 13. https://github.com/kien/ctrlp.vim/issues/280 - Delete buffers with ctrlp 
" 14. https://github.com/junegunn/vim-easy-align - Aligning text 
" 15. https://github.com/sjl/gundo.vim - Undo tree. I think it requires python
" to run and the vim version must be 7.3, maybe I'll try making my own version
" in just vimscript.
" also a vimcast about this one!
" 17. https://github.com/nelstrom/vim-cutlass - Addressing the issues of vim's
" registers, unfortunately it is not written... but! I could definitely
" implement some of his ideas. There is also a plugin already out there which
" he mentions: https://github.com/svermeulen/vim-easyclip
" 18. https://github.com/Shougo/unite.vim - Possibley like ctrl-p but can be
" used for other things.
" 19. https://github.com/terryma/vim-multiple-cursors - Multiple cursors
" 20. https://github.com/AndrewRadev/switch.vim - Similar to my 'flip' series
" of commands. I was reading through the github page and one thing I was kind
" of bummed about was that the cursor has to be on the item being 'switched'
" for it to work. I would like it it seek for the next thing that could be
" switched. It would be nice to be able to seek forward and backwards as well.
" 21. https://github.com/justinmk/vim-ipmotion - Configure { and } do behave a
" bit more intelligently. Seems like a nice little plugin.

" I want to keep the sneak mappings all as 's', which isn't currenlty
" happening because of the surround plugin. I'm planning on using the 's'
" mappings with sneak so I gotta find new mappings for surround. Here are some
" characters which could possibly used: g m o p q r u x z C M O P Q R U X Z.
" I'm thinking of 'z' as a strong contender, but I want to find a nice
" mnemonic if possible.

" I like using 'I' and 'A' in visual block mode and I don't see myself really
" using that functionality so I'm disabling it.
let g:targets_aiAI = 'ai  '

" Start interactive EasyAlign in visual mode
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object
nmap ga <Plug>(EasyAlign)
let g:easy_align_delimiters = {
            \ '>': {
            \ 'pattern':      '=>\|->',
            \ 'left_margin':  0,
            \ 'right_margin': 0 },
            \ }

" TODO: Maybe a bug with sneak.vim? If I issue to 's' commands in a row, then
" the second 's' command won't add to the jumplist.
map ,, <Plug>SneakPrevious
" Replace 'f' with 1-char Sneak
map f <Plug>Sneak_f
map F <Plug>Sneak_F
" Replace 't' with 1-char Sneak
map t <Plug>Sneak_t
map T <Plug>Sneak_T

" }}}

" Learn more about netrw and what it can do.

" Make <C-w> in command line mode (and maybe insert mode?) delete a '/'. That
" way it will be easier to delete a filepath.

" Change the headings on tabs to display the current directory for the active
" file. Some quick research, see :help setting-tabline. 'tabline' is the text
" that gets displayed across the ENTIRE tab page it uses the same format as
" 'statusline'. 'showtabline' lets us display the tabline at all times if
" desired. Use the function getcwd() to get the current directory, to put it
" in the statusline we'd have to do something like this :set
" tabline=%!getcwd()

" Is there a text object to change a file name? Also could we have a text
" object to change between the delimiters in g:targets_separators?

" Command to only list modified buffers

" When visually selecting text for a text object, the last selected text (that
" you do with the gv command) is changed. Is there any way to stop that from
" happening?

" Seems like a way to grep on all buffers is with these 2 commands:
" 1. call setqflist([]) - Clears the quickfix lists
" 2. bufdo grepadd! regex % - Goes through each buffer and does a grep for
" your regex. The grepadd commands adds to the quickfix list rather than
" replacing it like :grep would do. I just tried running the command but had
" to hit enter for EVERY buffer. Try prepending 'silent!' next time.

" Consider making the command g/ comment out things instead of gc. I kind of
" like that because then in visual mode I could have a text object, gc, for
" actually selecting some commented text.

" Really get usr_29.txt into your head. Seems like it has some useful things.

" I think this could be fun. Create a little mode (or something) where I'll
" enter a keystroke then I'll be in insert mode and whenever I type a variable
" name like thisisavariablename, it will try to make the variable snake case
" or camel case (depending on the mode) as I type. So I suppose it would look
" for whole words and make the snakiness/camelniness happen on those word
" boundaries.

" I'm super curious to check this out more: http://vim.spf13.com/. It is a
" distribution of vim which includes a lot of plugins and a vimrc. I doubt I'd
" use it but I bet there's some good mappings I could take. I found that on
" this site: http://statico.github.io/vim2.html

" Consider making the 'd' command actually delete text (so no messing with the
" default register). Actually when I have time look that this plugin:
" https://github.com/svermeulen/vim-easyclip. Perhaps I'll give it a whirl.

" Could we configure the % command to work on pairs of quotes as well?

" So I remember that this guy made a brainfuck interpreter using only the
" C-preprocessor: https://github.com/orangeduck/CPP_COMPLETE (amazing, I'll
" have to look through that one day). I wonder if something similar could be
" accomplished with recursive mappings in vim? So this mapping expands to this
" then to that etc... and through that process some useful computation is
" being done. Like could I make a recursive vim mapping to keep appending the
" next fibonacci number to the end of the file or something like that?

" Look into mapping alt key chords: help map-which-keys. Also help index.txt
" for some key sequences which are not mapped. Could I make mappings <M-x> and
" <M-a> to add and subtract but look backwards instead of forwards?

" Could we make a visual mapping to repeat the last text-object movement? So
" if I did vi{ and I wanted to repeat i{ I could just hit ',t' or something
" like that. See https://github.com/terryma/vim-expand-region, it might be my
" best bet.

" Make a mapping which will reformat a function call to look like this:
" $this->abortIf(
"     $this->connection->getDatabasePlatform()->getName() != 'mysql',
"     'Migration can only be executed safely on \'mysql\'.'
" );
" So imagine that the above function used to be one giant line, this mapping
" would make it look like it is right now.

" Make text objects to go to the next occurrence of a comment. The same goes
" with code.

" I think I said this before but I don't remember seeing it in my vimrc so
" I'll say it again. Make an operator (or a series of commands) which will
" print out the value of a variable. This will be helpful for debugging
" purposes. Then I could just do something like: ,piw and in the case of php
" it will insert a new line below the current one with the contents:
" var_dump(yanked_string);

" I was having to make an example json string based off columns like this:
 " /**
   "   * @var \DateTime
   "   *
   "   * @ORM\Column(name="most_recent_mass_email_sent", type="datetime", nullable=true)
   "   * @Serializer\Expose
   "   */
   "  private $mostRecentMassEmailSent;

   "  /**
   "   * @var integer
   "   *
   "   * @ORM\Column(name="candidate_emails_sent", type="integer", nullable=true)
   "   * @Serializer\Expose
   "   */
   "  private $candidateEmailsSent;

   "  /**
   "   * @var \DateTime
   "   *
   "   * @ORM\Column(name="most_recent_candidate_email_sent", type="datetime", nullable=true)
   "   * @Serializer\Expose
   "   */
   "  private $mostRecentCandidateEmailSent;

   "  /**
   "   * @var \DateTime
   "   *
   "   * @ORM\Column(name="last_connection", type="datetime", nullable=true)
   "   * @Serializer\Expose
   "   */
   "  private $lastConnection;
" So for the above data, the json string could look like this:
"
" {"most_recent_mass_email_sent":"DATE", "candidate_emails_sent":8, "most_recent_candidate_email_sent":"DATE", "last_connection":"DATE}
"
" I was doing it semi-manually. Think about a way to automate this more. I know
" it can be done, I just don't how to yank multiple things but still use them in
" a meaningful way.

" Think about using <C-l> and <C-h> for window movements and bind other
" Control key chords to switch between tabs.

" Could we create a paste operator? That sounds like a fun challenge. So
" something like: ,pi" will paste from the default register into the i" text
" object. It really probably wouldn't benefit me too much BUT I could make
" this operator not change the default register, which would be nice.
" Consolidating two of my other ideas: Maybe I could have this operator (if
" the * or + registers are used) delete the text, turn on 'paste', and enter
" insert mode. I can also have an autocommand which turns off paste upon
" leaving insert mode.

" Make a series of mappings to insert 'test' data. So I could have a 'phone
" number' mapping which could insert the phone number "123-456-7890" or have
" one that inserts a test email or something like that.

" Create a text object for an IP address

" Create text objects for parts of snake/camel case variables. I currently
" have the CamelCase plugin but I don't really like it overly much and it
" would be fun to create it on my own.

" Create a :source operator which can source just the visually selected lines.

" Add some function to create class diagrams. I could give it the hierarchy in
" some form and it will draw a nicely spaced class diagram.

" I had to do a lot of yanks which were repetative. In particular I kept
" running this yank: "*yi", so yank inside quotes into the clipboard. Turns
" out that you can add a flag 'y' to the 'cpoptions' setting and then the '.'
" command will repeat the last yank!!! Super cool. I don't know if I'd keep it
" on all of the time but I could defeinitely see myself turning it on in
" specific situations like this. See repeat.txt for more information.

" I liked this article:
" http://viming.blogspot.com/2008/09/conditionally-applying-macro-recordings.html
" I think I could make good use of it because sometimes I have a macro that I
" want to execute on a certain number of searchable things. Like I'd do the
" macro on all 'private' variables. So I'd make the change, search for the
" next 'private' and continue. But the problem is that I want the macro to
" stop itself too. Unfortuntely searching won't cause a 'beep' (we'll
" eventually just loop back around to the first 'private' variable and so the
" macro would just keep going. BUT if I used the g command and executed the
" macro on each of those matches that 'g' found then I think my idea could
" totally work. Alternatively, I think setting the 'wrapscan' option would
" help me accomplish what I want.

" Automatically set marks 'w and 'm to match the marks '[ and '] respectively.
" I chose 'w and 'm because those are easier to type and they are sort of
" mirror images of eachother just like '[ and '] are.

" A text object to select a chunk of code stopping before any commented
" sections. So a code text object.

" TODO: A mapping to type out the previously made auto-completion. This would
" probably be an insert mapping.

" TODO: I just started actively using capital letter marks to help with code
" diving expeditions. Maybe write some little plugin which prints the name of
" the next capital mark I have available, it could be printed in the status
" line. OR I could also come up with a mapping to just use the next unused
" capital mark automatically. That way I wouldn't have to worry about it at
" all. The marks available for that wrapper would probably be some subset of
" them all (like J-Q) or something like that. I could also print out which
" marks are used in the current file, i.e if mark 'A is in file 'file1' and
" I'm editing file1 then that mark will be displayed in the status line. I
" could also incorporate this mark stuff in my documenting of source code. I
" could print out a list of all files attached to a capital letter mark as
" well as the line of code attached to that mark or something like that. Hmmm
" yeah, I like that idea.

" TODO: Could we have automatic completion on whole strings? Right now we can
" do words and whole lines and some other things I think but I don't know
" about strings. Because I had a string like this 'Unique ID' in one area of
" the document and I wanted to write it again. It would have been faster to
" just write it again than copy it but it would have been even FASTER if I
" could have autocompleted the string itself.

" TODO: Make a command to reflect a range of lines. I think that would be a
" cool way to make some ascii art. So like if I had some art like this:
"
"     /
"    /
"   /
"  /
"  And we reflected it it, then we would get this:
"     /\
"    /  \
"   /    \
"  /      \
"  In adition make other commands to rotate, resize, etc...

" TODO: Read about how settings are transferred/inherited from window to
" window, buffer to buffer etc...

" TODO: Make it so that when you exit command mode it doesn't add the command
" to the history list. Also try to make it so that any duplicate commands in
" the history list are removed.

" TODO: Make some sort of notification ability. So like maybe I'll remind
" myself to delete something in a file. I'll add the message, associate it
" with a date and when that date comes to pass the notification will trigger.

" TODO: A bit of a weird request but here goes. So that 'sneak' plugin sounds
" really cool. A search on just two characters! That would definitely help
" with moving around. But I just had a situation where I was at the top of the
" candidat.xml file and I wanted to go to <etat1. To jump right there I had to
" do a search which involves a lot of typing. The sneak plugin wouldn't have
" helped me either. So what about a mapping where you can type 3 characters
" and it will do a search for: 'c1c2\S*c3' i.e it searches for the first two
" characters followed by any amount of whitespace and then the 3rd character.
" Running this command and typing <e1 would find my etat1 right away.

" TODO: The 10th user manual talks about visual block mode. I notice that when
" making changes using 'I', I can't delete any space and have that change be
" carried through all lines in the selection. I also notice that the '<'
" operator isn't working even though the '>' works just fine. Look into this.
" Actually, it seems that '<' needs to be on whitespace and then it works?

" TODO: The 10th user manual also says that 'J' in visual mode joins all lines
" and separates them by one space regardless of leading or trailing
" whitespace. But it looks like trailing whitespace IS being retained. Look
" into this.

" Just for fun, redefine how a number in front of the 'R' command works so
" that when you type something like 3R it will allow you to replace 3
" characters then exit replace mode. This will probably involve creating a
" function to do the work and then remapping R to call that function. We can
" check if a count was given with v:count and work based off that.

" Write a command which will do a vertical replacement (R currently does a
" horizontal replacement) sounds like a fun idea.

" Make it so that doing <C-r>/ in Insert mode will NOT include the \<\>
" surrounding the search text. I think I find myself fairly often using the *
" and then want to paste that data somewhere but the \<\> is unecesarry.

" Look up the pattern \%V when searching. Seems pretty cool, we cand search in
" the last visual selection!
" http://vim.wikia.com/wiki/Search_only_over_a_visual_range

" Could we configure vim so every time we leave insert mode we leave a mark?
" I'm picturing an improvement for those gp and gP commands where it will set
" a capital mark (like Z or something) then we could go hunting through
" completely different files, if we want, to find the thing we want to paste
" and the same command will paste it in the correct place. I suppose we could
" just remap all the keys that leave insert mode to set the Z mark before
" doing their normal function. I would probably need a function as well to
" make sure to only set the mark when we're leaving insert mode. Is there an
" autocmd for detecting when we leave insert mode? OHHH yeah check out :h
" autocmd.txt the event 'InsertLeave'

" That code that I wrote to semi-automatically add sertifi email tags was
" nice. I liked the idea of going around a file and appending some text to a
" list which we later operate on. What if we generalized it a bit? I could
" make an operator which will add the selected text to the end of a global
" array. Then I could call function which would use that array to do
" something. Yeah, I like that idea.

" Sort of related to the idea above of having an operator to add a bit of text
" to a global array. I recently had to take a comma delimited string and put
" each element of that string into different xml node attributes. I wish I
" could have: yanked that into some special register, the register would
" detect that it's a comma delimited string, and then everytime I paste from
" that register it would eat up one of the elements (sort of like how if you
" paste from register 1 and then use '.' it will then paste from 2, 3,... Just
" for more clarity here's the situation I faced:
"       <node atr="Prop1,Prop2,Prop3,Prop4,Prop5">
"       <node1 atr="">
"       <node2 atr="">
"       <node3 atr="">
"       <node4 atr="">
"       <node5 atr="">
" I had to put each of the 'Prop' attributes in the nodes below so in the end
" it looked like this:
"       <node atr="">
"       <node1 atr="Prop1">
"       <node2 atr="Prop2">
"       <node3 atr="Prop3">
"       <node4 atr="Prop4">
"       <node5 atr="Prop5">
" In the end I devised a macro to do most of the work. I set mark 'a on <node>
" and mark 'b on <node1> then this was the macro that did it:
"   'aye`bf"pjmb'afPma
" Then the only thing I had to do at the end was delete the node?atr
" attribute. So that macro is nifty but I would like what I mentioned above.

" Is it possible to define a new regex atom? That would be cool. This thought
" was inspired by how I would implement roman numerals into my existing
" increment vim plugin and I had the thought, if I could turn recognizing
" roman numerals into a regex atom then the problem would be solved! (Well
" almost). Actually, I bet with a bit of thinking I could come up with a regex
" for a roman numeral without too much trouble, just something like: [ivcl]*.
" We could even literally have a list of the first 50 odd roman numerals like
" this: (i|ii|iii|iv|...). I'll think about it.

" I've already talked about this in some other list I was keeping track of but
" I'll write it here as well. I want to make a little code analyzer that will
" look at one file of code and help me understand what's going on. I'm
" picturing this tool would grab all the function names and for each function
" name, see what function calls them. Then build a little graph based off that
" knowledge.

" Luceo Stuff {{{

" That would be really cool if, in the CDATA section of the xml files, I could
" display the actual label associated with a node. This section wouldn't
" actually be part of the file, it would just be displayed somehow.

" Look into using the :keepalt command to edit files but not change the
" alternate file.

" Make an autocommand to run translator:generate everytime we save a
" php.en.json file. On a similar note, look into what needs to be done so I
" could call the functions defined in my .bashrc inside of vim. It would be
" nice to be able to call my 'psf' commands.

" ABOUT: Learning vimscript was a recent endeavor and this is my first attempt
" at doing any serious work with it. The goal with this code is to help
" automate the various things we must do when working with the configuration
" files.

" TODO:

" Add something that will add fields in bulk. I just had to add 11 short
" text fields to a candidate and it would have been a lot smoother if I could
" just add them all in one go.

" Right now that 'GetFieldType()' type spits out every possible field. But
" sometimes it isn't necessary. Like when you remove a field, you only need to
" worry about contacts and selects, there is no need to print that whole list
" to the user. Make 'GetFieldType()' accept a list of strings that it will
" accept.

" Should I make a mapping to change a field type?? That could be useful and it
" could be easy to do.

" The go to CDATA from an xml node isn't always working correctly. Tighten
" that up.

" Look into when/why my functions sometimes prompt me to 'Hit Enter to
" continue'. I noticed it happen when I was removing a 'select' field.

" Look into how to retain state when making changes. Like keeping the
" alterrnate buffer the same, keep the view of the screen and all that sort of
" stuff. This is just for aesthetic reasons.

" Make a mapping to insert/remove a state in the candidate workflow

" Make a function which will activate parent nodes as well as all their
" children. The 'naive' way would be extremely simple: go to the parent node,
" run :normal! vat, then run :call ChangeNodeAttribute('actif', 'true').  This
" would only fail in the case that there are multiple nodes on the same line.
" I suppose I could try running a check on each line in the selection to make
" sure that all 'actif' attributes are true, I'll look into it.

" Mapping to configure silo

" Mapping to disable EEO (at least the xml part)

" WEIRD ERROR: I thought I documented this error but it seems like I haven't... That
" weird problem happened again where when using normal o to insert text, the
" text is wrapping to the next line. I noticed it when I tried to configure
" the candidate workflow. For example I wanted to insert this text:
"       <etat1 cle="AFFECTATION_WORKFLOW_ETAT_1" libelle="TXT_AFFECTATION_WORKFLOW_ETAT_1" icone="icones/etat1.png" onenter="" onleave="" libelle-onenter="" libelle-onleave=""/>
" But when I ran the command that text looke something like this:
"       <etat1 cle="AFFECTATION_WORKFLOW_ETAT_1" libelle="TXT_AFFECTATION_WORKFLOW_ETAT_1" 
"       icone="icones/etat1.png" onenter="" onleave="" 
"       libelle-onenter="" libelle-onleave=""/>
" I really have no idea why that would be happening. Maybe I'll try to
" investigate again, I think it has something to do with the presence of a
" horizontal split but I'm unsure. OKAY! So I actually have a better idea of
" what is causing this now. The short and sweet answer is that the lines are
" breaking because of the 'textwidth' option. Now, normally that option is 0
" for xml documents. BUT when we source the .vimrc file it seems that it gets
" set to 78. I'm currently unsure as to why that is, but at least we have
" something to go off of. So it's not some weird bug, it's just this setting.

" Mapping to go from one CDATA section to another for the same node.

" I think using echoerr might not be the best thing? Try to look into printing
" a messages that just stands out to the user so they can see if something
" went wrong.

" Look at the 'Marks' section in motion.txt it might have some ideas
" about things like not changing the jumplist when using marks and stuff like
" that.

" Make a mapping to configure the email apply customization.

" I created a command to call this funtion, the only problem is that commands
" pass their arguments as strings. I don't know how to get around this so I've
" just eval'd all of the arguments.
" function! CalculateQuarterlyBonus(target, company_achievement, individual_achievement, company_bonus_percent, individual_bonus_percent)
function! CalculateQuarterlyBonus(...)
    let company_achievement = 1.0
    let individual_achievement = 1.5
    let target = 1100.0
    let company_bonus_percent = 0.5
    let individual_bonus_percent = 0.5
    if a:0 > 0
        let company_achievement = eval(a:1)
    endif
    if a:0 > 1
        let individual_achievement = eval(a:2)
    endif
    if a:0 > 2
        let target = eval(a:3)
    endif
    if a:0 > 3
        let company_bonus_percent = eval(a:4)
    endif
    if a:0 > 4
        let individual_bonus_percent = eval(a:5)
    endif
    echo 'Company Achievement      = 1.0'
    echo 'Individual Achievement   = 1.5'
    echo 'Target                   = 1100.0'
    echo 'Company Bonus Percent    = 0.5'
    echo 'Individual Bonus Percent = 0.5'
    echo target * company_achievement * company_bonus_percent + target * individual_achievement * individual_bonus_percent
endfunction
command! -nargs=* EchoQuarterlyBonus call CalculateQuarterlyBonus(<f-args>)

" PSF commands {{{
" Runs psf translator:generate.
function! RunTranslatorGenerate()
    !php ../../../psf.php translator:generate
    redraw!
endfunction

" Runs psf translator:generate. I should have the other functions that run
" generate call this function instead.
function! RunTranslatorDedupe()
    silent !php ../../../psf.php translator:dedupe
    redraw!
endfunction

" Runs psf translator:generate. I should have the other functions that run
" generate call this function instead.
function! RunTranslatorMerge()
    silent !php ../../../psf.php translator:merge
    redraw!
endfunction
" }}}

" XML Related functions (mostly helper function sort of stuff) {{{

" Well that function name is a mouthful. I tried to be descriptive as possible. Say
" you had two paths like this:
"   - one/two/three
"   - one/temp/two/three
" This function would return 'two/three' because it removed the longest common
" prefix ('one/') from the first string and returned that.
function! GetPathRemovingCommonPrefix(path1, path2)
    let path1_list = split(a:path1, '/')
    let path2_list = split(a:path2, '/')
    let i = 0 
    while i <# len(path1_list)
        if path1_list[i] !=# path2_list[i]
            break
        endif
        let i += 1
    endwhile
    let return_path = ""
    while i <# len(path1_list)
        let return_path = return_path . '/' . path1_list[i]
        let i += 1
    endwhile
    " Remove the leading '/' before returning
    return strpart(return_path, 1)
endfunction

" Searches for an xml node given it's path.
" This function is in no way perfect but it should ususally work.

" TODO: Thoughts for improvement:
" 3. See if we can make it so this function doesn't change the jump list.
" Right now when we call GetPathToXMLNode() that changes the jump list. So
" maybe I should try messing around with that function to prevent the jump
" list from changing?
function! GoToXMLNode(path_to_node)
    "THOUGHT: This function will loop infinitely if
    "path_to_travel is ''. But as long as a:path_to_node is
    "non empty, I don't think that will ever happen. Keep it
    "in mind though.
    if a:path_to_node ==# ""
        return 0
    endif
    let abs_node_path = a:path_to_node
    " Hack to account for when we're editing poste.xml
    " TODO: This won't work if we're editing poste.xml from a directory that
    " is not xml. That is because '%' contains the full path to the file.
    " Consider changing this so we make use of the ':t' modifier to get JUST
    " the name of the file regardless of the directory we're in.
    if expand('%:t') ==# 'poste.xml'
        let abs_node_path = 'poste/' . abs_node_path
    endif
    let start_pos = getpos('.')
    " Move to the top of the file
    call cursor(1, 1)
    let path_to_travel = abs_node_path
    let not_there_yet = 1
    while not_there_yet
        " Try to go to the node.
        for i in split(path_to_travel, '/')
            while 1
                if search('\v\<' . i . '( |\>|/|	)', 'W')
                    if synIDattr(synID(line('.'), col('.'), 1), "name") !=# 'xmlCommentPart'
                        break
                    endif
                else
                    "Couldn't find the specified path, restore the cursor
                    "position and return a failure value.
                    call setpos('.', start_pos)
                    return 0
                endif
            endwhile
        endfor
        " Check to make sure our current path is correct.
        let cur_path = GetPathToXMLNode()
        if abs_node_path ==# cur_path
            let not_there_yet = 0
        else
            let path_to_travel = GetPathRemovingCommonPrefix(abs_node_path, cur_path)
        endif
    endwhile
    call setpos("''", start_pos)
    return 1
endfunction 

" Technically, all this function does is yank the first double quoted string
" that appears on a line.  It's purpose is to return the path to a node that
" appears in CDATA.
function! GetNodePathFromCDATA(cdata_line_no)
    return substitute(getline(a:cdata_line_no), '[^"]*"\([^"]*\)".*', '\1', '')
endfunction

" Treats the first string on the current line as an xml path and then calls
" "GoToXMLNode()" on it. Meant to be used in the CDATA sections of the xml.
function! LuceoFindNode()
    let path = GetNodePathFromCDATA(line('.'))
    call GoToXMLNode(path)
endfunction

" Gets the name of an xml node. Not perfect by any stretch but I think it will
" work most of the time.
function! GetNodeName()
    let save_pos = getpos('.')
    " Try to go to the opening '<' of the xml node.
    if !search('<', 'bc', line('.'))
        call search('<', 'c', line('.'))
    endif
    " This is, more or less, the regex that matches a valid xml node name.
    let result = matchstr(strpart(getline('.'), col('.')-1), '^</\?\zs[a-zA-Z_:][-a-zA-Z0-9._:]*')
    call setpos('.', save_pos)
    return result
endfunction

" Goes to an xml node's parent. I think this (like many other things it seems)
" also could use some more work but should work just fine most of the time.
function! GoToParentNode()
    " Store current line
    let start_line = line('.')
    " Try to go to parent end tag
    normal! vat
    let parent_close_tag = line('.')
    " Try to go to parent start tag
    normal! o
    let parent_open_tag = line('.')
    " If we never moved, we were on a start/end tag to begin with. We do two
    " 'at' motions to go to the next parent tag (if there is one).
    if start_line ==# parent_close_tag || start_line ==# parent_open_tag 
        " If the motion to move to a parent tag fails then a beep will occur.
        " I don't like this as nothing is really wrong. So we disable it
        " temporarily.
        let temp = &t_vb
        set visualbell t_vb=
        normal! ato
        set novisualbell
        let &t_vb = temp
        " If the above command was unsuccessful then we're as far out as we
        " can go and we'll be in visual mode on the opening tag of the current
        " node. Exit visual mode and return 0 because we couldn't go to our
        " parent.
        if line('.') ==# parent_open_tag
            normal! v
            return 0
        endif
    endif
    execute "normal! \<ESC>"
    return 1
endfunction

" Gets the absolute path to an xml node from the current cursor position. This
" function is also not perfect by any stretch, but should work most of the
" time.
function! GetPathToXMLNode()
    " Save the cursor position. 
    let start_pos = getpos('.')
    let alt_mark_pos = getpos("''")
    let path = [GetNodeName()]
    " Originally this if statement was a simple while loop. I replaced that
    " while loop because it was running a little slower than I liked. Although
    " messier, this if statement is more optimized.
    if GoToParentNode()
        call insert(path, GetNodeName())
        " After the above call to GoToParentNode() we are on a pair of opening
        " tags. This will put us at the closing tag of that pair.
        normal! vat
        let parent_close_tag = line('.')
        while 1
            " Prevent the error bell from sounding.
            let temp = &t_vb
            set visualbell t_vb=
            normal! at
            set novisualbell
            let &t_vb = temp
            " There wasn't a parent node to go to, so we didn't moved and are
            " left in visual mode.
            if parent_close_tag ==# line('.')
                normal! v
                break
            else
                let parent_close_tag = line('.')
                call insert(path, GetNodeName())
            endif
        endwhile
    endif

    call setpos('.', start_pos)
    call setpos("''", alt_mark_pos)
    return join(path, '/')
endfunction

" Goes from an xml node to it's location in CDATA

" It seems to work but there's still some funkiness going on which I think
" might stem from the GoToParentNode() function. I notice that when I pass the
" GetPathToXMLNode() function directly into the search() function, it doesn't
" work. But when I store the result in a variable then it works just fine.

" Another weird thing is that in all .xml files except candidat.xml, doing vat
" enough will grab the root xml nodes as well. My quick hack is if we can't
" find the path then we strip off the first portion and try searching for that
" as well.
function! LuceoFindCDATA()
    let save_pos = getpos('.')
    let path = GetPathToXMLNode() . '\>'
    call cursor(1,1)
    if !search(path)
        if !search(substitute(path, "[^/]*/", "", ""))
            call setpos('.', save_pos)
        else
            call setpos("''", save_pos)
        endif
    else
        call setpos("''", save_pos)
    endif
endfunction

" Leaves the cursor on the quote surrounding an attribute's value.
" Assumes the cursor is on the node who's attribute we're going to.
function! GoToNodeAttribute(attribute)
    " Move cursor to the beginning of the node
    call search('<', 'bc', line('.'))
    " Move to the attribute's value
    if search('\v\s+' . a:attribute . '\s*\=\s*("|' . "')", 'e', line('.'))
        return 1
    endif
    return 0
endfunction

" Changes a node's attribute
" Assumes the cursor is on the line with the xml node to alter
function! ChangeNodeAttribute(attribute, new_val)
    " Go to the attribute's value
    if GoToNodeAttribute(a:attribute)
        let save_unnamed_register = @@
        " Get the character under the cursor (will be a single or double quote)
        normal! yl
        " Change the value
        execute 'normal! ci' . @@ . a:new_val . "\<ESC>"
        let @@ = save_unnamed_register
    endif
endfunction

" Changes a node's attribute
" Assumes the cursor is on the line with the xml node 
function! GetNodeAttribute(attribute)
    " Go to the attribute's value
    if GoToNodeAttribute(a:attribute)
        " Save the register so it can be restored
        let save_unnamed_register = @@
        " Yank the attribute
        normal! yl
        execute 'normal! yi' . @@
        " Store the result to return
        let result = @@
        let @@ = save_unnamed_register
        return result
    endif
    return 0
endfunction

" }}}

" Data Structure of all Fields {{{

" THINGS I HAD TO ADJUST AND WATCH OUT FOR AFTER TAKING DATA FROM THE WIKI:
" So the wiki isn't completely correct with some things. These are things it
" gets wrong: 
" CANDIDAT:
"   1. All the Work Experience nodes should be 'experiences', on the wiki
"   they're listed as 'experience'. You can accomplish this change with this
"   command-> :g/"field_category" : 2,/.+1s:experience:experiences
"   2. These qualification nodes shouldn't have the trailing dash:
"       qualification/liste-editable1-, qualification/liste-editable2-,
"       qualification/liste-editable3-
"   3. The nodes: 'mairie/debut-rech3', 'mairie/debut-rech2', and
"   'mairie/debut-rech' all have the same json constant. For now I'm only
"   keeping the first debut-rech. The same thing is true of their 'fin-rech'
"   counter parts.
"   4. The experiences nodes in the wiki: 'experiences/date-libre1-5' are
"   missing the 'b' in 'liBre'
"   5. The 'profil/langues/langue1-15' fields are listed as secondary
"   dropdowns but I don't really think they are. I'm not really sure what to
"   do with them so I'm just removing them.
"   6. The node 'profil/niveau-etudes' is missing the ending 's' on the wiki.
"   7. The node 'profil/niveau-experience' has a different label in the
"   searchable and regular sections of the candidate form.
"   8. The 'type-contrat' node below 'situation-recherchee' and
"   situation-actuelle' share the same searchable node.
"   9. There are two nodes listed in the 'regular fields' section with a
"   parent of 'Experiences'. These should probably get moved with other
"   'exepriences' nodes but I'm not sure. For now I've just taken them out.
"   10. The node 'mairie/observation-rech' is listed as a short AND long
"   field when it is really just a long text field.
"   11. Just like the 'debut-rech' nodes in 3. the nodes 'poste-envisage1-3'
"   also share the same json constant.
"   12. Just like the 'debut-rech' nodes in 3, the nodes 'taux-rech1-3' share
"   the same json constant.
"   13. situation-recherchee/heures does NOT have an email tag associated with
"   it.
"   14.  DEPARTMENT

" POSTE:
"   1. The 'descriptif/libre-long-lienref1-5' nodes don't have searchable
"   counterparts. I've put them at the end of the list of 'long' nodes.
"   2. There are 'langue' fields in poste.xml as well that are listed as
"   secondary. I don't really know what to do with these either so I'm taking
"   them out.
"   3. There are short text fields 'langues/libre1-9' I wasn't exactly sure
"   what they were used for either so I just took them out.
"   4. The 'mairie/categorie' field has to have it's search path manually
"   added.
"   5. The contact nodes are listed like 'Descriptif/contact2'. That 'D'
"   should be lower case.
"   6. The divers/charge-recrutement node has the wrong search path in the
"   wiki. This needs to be fixed.

" HOW I ADDED THE SEARCHABLE PATHS:
" Adding the searchable paths was tricky because there doesn't seem to be a
" strictly algorithmic way to do it. The trouble is that the names of the
" nodes you add aren't necessarily the same as their searchable counterpart.
" To make matters worse, some nodes have the same name! I was hoping the csv
" files could make this part easier, but unfortunately that is not the case.
" The best way to do it, I believe, is how I was originally doing it which is
" to search for a node's json constant. If we find only one constant in the
" searchable section then that must be the node's searchable counterpart. If
" this search fails then we'll have to do it manually.

" Requires that the candidat.xml or poste.xml file is being edited.
" Returns the search path for a node given the path to the node. If
" candidat.xml was being editied this function will also open a split
" containing the recherche.xml file. If a search path is found, this split
" will remain open. However, if no search path is found then this function
" will close that split.

" Get's the search path for a node added on the candidate form
function! GetSearchPathCand(node_name, json_constant)
    call cursor(1, 1)
    " Count how many times we find the constant
    let matching_lines = []
    "let my_count = 0
    "execute 'silent .,$global /\<' . a:json_constant . '\>/let my_count += 1'
    execute 'silent global /\<' . a:json_constant . '\>/call add(matching_lines, line("."))'
    " We'll be left in the right spot
    "if my_count ==# 1
    if len(matching_lines) ==# 1
        let path = GetPathToXMLNode()
        return path
    else
        " If there's only one node with a matching name, we take that one.
        let num_node_names = 0
        for i in matching_lines
            call cursor(i, 1)
            if GetNodeName() ==# a:node_name
                let num_node_names += 1
                let path = GetPathToXMLNode()
            endif
        endfor
        if num_node_names ==# 1
            return path
        else
            return ""
        endif
    endif
endfunction

" Get's the search path for a node added on the requisition form
function! GetSearchPathPoste(node_name, json_constant)
    call GoToXMLNode("recherche")
    " Count how many times we find the constant
    "let my_count = 0
    let matching_lines = []
    "execute 'silent .,$global /\<' . a:json_constant . '\>/let my_count += 1'
    execute 'silent .,$global /\<' . a:json_constant . '\>/call add(matching_lines, line("."))'
    " We'll be left in the right spot
    "if my_count ==# 1
    if len(matching_lines) ==# 1
        let path = substitute(GetPathToXMLNode(), "[^/]*/", "", "")
        return path
    else
        " If there's only one node with a matching name, we take that one.
        let num_node_names = 0
        for i in matching_lines
            call cursor(i, 1)
            if GetNodeName() ==# a:node_name
                let num_node_names += 1
                let path = substitute(GetPathToXMLNode(), "[^/]*/", "", "")
            endif
        endfor
        if num_node_names ==# 1
            return path
        else
            return ""
        endif
    endif
endfunction

function! GetSearchPathAndOpenSplit2(path_to_node, cand_or_poste)
    call GoToXMLNode(a:path_to_node)
    let node_name = GetNodeName()
    let json_constant = GetNodeAttribute('libelle')
    if a:cand_or_poste
        split recherche.xml
        let result = GetSearchPathCand(node_name, json_constant) 
    else
        " TODO: I added this code which splits to poste.xml 'cause I had other
        " code closing the split of nothing could be found. I used to not do a
        " split poste because it was splitting on the same file. But I
        " actually kind of like it now. For 1 it's consistant with the name of
        " this function AND you'll get to see the change imediately. Even so,
        " put some thought into thinking of another way to do this. Also, look
        " into if this messes anything up.
        split
        let result =  GetSearchPathPoste(node_name, json_constant)
    endif
    " Close the open split if no search path was found.
    if result ==# ""
        quit
    endif
    return result
endfunction

" This is the function I made automate what I could of filling out the
" 'search_node_path' field of the data structures below.
function! HelperWriteSearchNodePath()
    let cur_line = getline('.')
    return substitute(cur_line, '\v.*: "([^,]*)",', '\1', '')
endfunction

function! WriteSearchNodePath(cand_or_poste)
    let all_node_paths = []
    let all_node_search_paths = []
    'y,'zglobal/"node_path"/call add(all_node_paths, HelperWriteSearchNodePath())
    for i in all_node_paths
        "split candidat.xml
        split poste.xml
        let search_path = GetSearchPathAndOpenSplit(i, a:cand_or_poste)
        if search_path !=# ''
            wincmd c
        endif
        wincmd c
        call add(all_node_search_paths, search_path)
    endfor
    let i = 0
    'y,'zglobal/"search_node_path"/execute 's:"":"' . all_node_search_paths[i] . '"' | let i += 1
endfunction

" All (most) of the candidat.xml fields.
" TODO: Add the repeatable sections for the 'concours' and 'poste-envisiage'
" repeatable sections.
" Date fields
"        \ {
"            \ "reusable" : 1,
"            \ "field_category" : 0,
"            \ "node_path" : "mairie/debut-rech2",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 1,
"            \ "field_category" : 0,
"            \ "node_path" : "mairie/debut-rech3",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 1,
"            \ "field_category" : 0,
"            \ "node_path" : "mairie/fin-rech2",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 1,
"            \ "field_category" : 0,
"            \ "node_path" : "mairie/fin-rech3",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },

" Select fields? Wasn't sure what to do with these 'langue' fields because
" I've never really touched them myself.
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue1",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue10",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue11",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue12",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue13",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue14",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue15",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue2",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue3",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue4",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue5",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue6",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue7",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue8",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue9",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langues-liste",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },

" NOT SURE WHERE TO PUT:
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "Experiences/texte",
"            \ "search_node_path" : "zone_texte/suivi-affectation",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "Experiences/dernier-emploi",
"            \ "search_node_path" : "divers/strlibre3",
"            \ "obj" : "",
"        \ },

let g:candidat_nodes =
\ {
    \ "date" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre1",
            \ "search_node_path" : "champ_date/dtlibre1-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre2",
            \ "search_node_path" : "champ_date/dtlibre2-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre3",
            \ "search_node_path" : "champ_date/dtlibre3-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/certificat-travail",
            \ "search_node_path" : "champ_date/certificat-travail-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/debut-contrat",
            \ "search_node_path" : "champ_date/debut-contrat-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/debut-rech",
            \ "search_node_path" : "champ_date/debut-rech-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/demande-mob",
            \ "search_node_path" : "champ_date/demande-mob-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/dispo-debut",
            \ "search_node_path" : "champ_date/dispo-debut-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/dispo-fin",
            \ "search_node_path" : "champ_date/dispo-fin-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/entree",
            \ "search_node_path" : "champ_date/entree-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/fin-contrat",
            \ "search_node_path" : "champ_date/fin-contrat-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/fin-rech",
            \ "search_node_path" : "champ_date/fin-rech-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/visite-med",
            \ "search_node_path" : "champ_date/visite-med-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/date-naissance",
            \ "search_node_path" : "profil/date-naissance",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/date-libre1",
            \ "search_node_path" : "qualification/date-libre1-debut",
            \ "obj" : "dtQualifLibre1@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/date-libre2",
            \ "search_node_path" : "qualification/date-libre2-debut",
            \ "obj" : "dtQualifLibre2@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/date-libre3",
            \ "search_node_path" : "qualification/date-libre3-debut",
            \ "obj" : "dtQualifLibre3@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/date-libre1",
            \ "search_node_path" : "experiences/date-libre1-debut",
            \ "obj" : "dtLibre1_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/date-libre2",
            \ "search_node_path" : "experiences/date-libre2-debut",
            \ "obj" : "dtLibre2_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/date-libre3",
            \ "search_node_path" : "experiences/date-libre3-debut",
            \ "obj" : "dtLibre3_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/date-libre4",
            \ "search_node_path" : "experiences/date-libre4-debut",
            \ "obj" : "dtLibre4_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/date-libre5",
            \ "search_node_path" : "experiences/date-libre5-debut",
            \ "obj" : "dtLibre5_@",
        \ },
    \ ],
    \ "select" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/categorie",
            \ "search_node_path" : "divers/categorie",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/concours",
            \ "search_node_path" : "divers/concours",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/critere-sociaux",
            \ "search_node_path" : "divers/critere-sociaux",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/formation-complementaire",
            \ "search_node_path" : "divers/formation-complementaire",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/function-publique",
            \ "search_node_path" : "divers/formation-complementaire",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/motif-mobilite",
            \ "search_node_path" : "divers/motif-mobilite",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/motif-reintegration",
            \ "search_node_path" : "divers/motif-reintegration",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/nom-diplome",
            \ "search_node_path" : "divers/nom-diplome",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/permis-conduire",
            \ "search_node_path" : "divers/permis-conduire",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/situation-editable",
            \ "search_node_path" : "divers/situation-editable",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/situation-famille",
            \ "search_node_path" : "divers/situation-famille",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/specialite",
            \ "search_node_path" : "divers/specialite",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/statut",
            \ "search_node_path" : "divers/statut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/temps-travail",
            \ "search_node_path" : "divers/temps-travail",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/type-emploi",
            \ "search_node_path" : "profil/type-emploi",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/type-temps-travail",
            \ "search_node_path" : "divers/type-temps-travail",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "situation-actuelle/liste-editable1",
            \ "search_node_path" : "recherche_referentiel/liste-editable1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "situation-actuelle/liste-editable2",
            \ "search_node_path" : "recherche_referentiel/liste-editable2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "situation-actuelle/liste-editable3",
            \ "search_node_path" : "recherche_referentiel/liste-editable3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "situation-recherchee/liste-editable1",
            \ "search_node_path" : "poste-envisage/liste-editable1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "situation-recherchee/liste-editable2",
            \ "search_node_path" : "poste-envisage/liste-editable2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "situation-recherchee/liste-editable3",
            \ "search_node_path" : "poste-envisage/liste-editable3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "divers/disponibilite",
            \ "search_node_path" : "divers/disponibilite",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langue-maternelle",
            \ "search_node_path" : "profil/langue-maternelle",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/niveau-etudes",
            \ "search_node_path" : "profil/niveau-etudes",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/niveau-experience",
            \ "search_node_path" : "profil/niveau-experience",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "situation-actuelle/type-contrat",
            \ "search_node_path" : "recherche_referentiel/type-contrat",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "situation-recherchee/type-contrat",
            \ "search_node_path" : "recherche_referentiel/type-contrat",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/liste-editable1",
            \ "search_node_path" : "qualification/liste-editable1",
            \ "obj" : "lstQualifEditable1@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/liste-editable2",
            \ "search_node_path" : "qualification/liste-editable2",
            \ "obj" : "lstQualifEditable2@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/liste-editable3",
            \ "search_node_path" : "qualification/liste-editable3",
            \ "obj" : "lstQualifEditable3@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/nom-diplome",
            \ "search_node_path" : "qualification/nom-diplome",
            \ "obj" : "lstNomDiplome@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 1,
            \ "node_path" : "qualification/niveau-diplome",
            \ "search_node_path" : "qualification/niveau-diplome",
            \ "obj" : "lstNiveauDiplome@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/liste-editable1",
            \ "search_node_path" : "experiences/liste-editable1",
            \ "obj" : "lstListeEditable1@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/liste-editable2",
            \ "search_node_path" : "experiences/liste-editable2",
            \ "obj" : "lstListeEditable2@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/liste-editable3",
            \ "search_node_path" : "experiences/liste-editable3",
            \ "obj" : "lstListeEditable3@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/liste-editable4",
            \ "search_node_path" : "experiences/liste-editable4",
            \ "obj" : "lstListeEditable4@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/liste-editable5",
            \ "search_node_path" : "experiences/liste-editable5",
            \ "obj" : "lstListeEditable5@",
        \ },
    \ ],
    \ "integer" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre1",
            \ "search_node_path" : "divers/ilibre1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre2",
            \ "search_node_path" : "divers/ilibre2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre3",
            \ "search_node_path" : "divers/ilibre3",
            \ "obj" : "",
        \ },
    \ ],
    \ "long" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/absence",
            \ "search_node_path" : "zone_texte/absence",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/affectation-mob",
            \ "search_node_path" : "zone_texte/affectation-mob",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/bilan",
            \ "search_node_path" : "zone_texte/bilan",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/contre-indication-integr",
            \ "search_node_path" : "zone_texte/contre-indication-integr",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/evaluation",
            \ "search_node_path" : "zone_texte/evaluation",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/motif-demande-mob",
            \ "search_node_path" : "zone_texte/motif-demande-mob",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/observation-rech",
            \ "search_node_path" : "zone_texte/observation-rech",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/suivi-affectation",
            \ "search_node_path" : "zone_texte/suivi-affectation",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/experience",
            \ "search_node_path" : "zone_texte/cantine-experience",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/specialite",
            \ "search_node_path" : "zone_texte/cantine-specialite",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "mairie/restricted-med",
            \ "search_node_path" : "zone_texte/suivi-affectation",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/competences-texte",
            \ "search_node_path" : "zone_texte/competences-texte",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/formation-texte",
            \ "search_node_path" : "zone_texte/formation-texte",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 1,
            \ "node_path" : "qualification/descriptif",
            \ "search_node_path" : "qualification/descriptif",
            \ "obj" : "txtQualifDescriptif@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte1",
            \ "search_node_path" : "",
            \ "obj" : "txtTexte1_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte2",
            \ "search_node_path" : "",
            \ "obj" : "txtTexte2_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte3",
            \ "search_node_path" : "",
            \ "obj" : "txtTexte3_@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 2,
            \ "node_path" : "experiences/descriptif",
            \ "search_node_path" : "experiences/descriptif",
            \ "obj" : "txtDescriptif@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/commentaire",
            \ "search_node_path" : "zone_texte/commentaire",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/motivations",
            \ "search_node_path" : "zone_texte/motivations",
            \ "obj" : "",
        \ },
    \ ],
    \ "short" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/age-enfant",
            \ "search_node_path" : "texte/age-enfant",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/an-mob",
            \ "search_node_path" : "texte/an-mob",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/code-mob",
            \ "search_node_path" : "texte/code-mob",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/competence-acquise",
            \ "search_node_path" : "texte/competence-acquise",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/contre-indication-med",
            \ "search_node_path" : "texte/contre-indication-med",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/contre-indication-tech",
            \ "search_node_path" : "texte/contre-indication-tech",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/dep-naissance",
            \ "search_node_path" : "texte/dep-naissance",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/employeur-actu",
            \ "search_node_path" : "texte/employeur-actu",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/horaire-actu",
            \ "search_node_path" : "texte/horaire-actu",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/horaire-rech",
            \ "search_node_path" : "texte/horaire-rech",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/horaire-rech2",
            \ "search_node_path" : "texte/horaire-rech2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/horaire-rech3",
            \ "search_node_path" : "texte/horaire-rech3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/lieu-naissance",
            \ "search_node_path" : "texte/lieu-naissance",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/nationalite",
            \ "search_node_path" : "texte/nationalite",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/nb-enfant",
            \ "search_node_path" : "texte/nb-enfant",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/nom-patronymique",
            \ "search_node_path" : "texte/nom-patronymique",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/nom-recommandeur",
            \ "search_node_path" : "texte/nom-recommandeur",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/observation-mob",
            \ "search_node_path" : "texte/observation-mob",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/observation-rech2",
            \ "search_node_path" : "texte/observation-rech2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/observation-rech3",
            \ "search_node_path" : "texte/observation-rech3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/poste-envisage",
            \ "search_node_path" : "texte/poste-envisage",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/poste-envisage2",
            \ "search_node_path" : "texte/poste-envisage2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/poste-envisage3",
            \ "search_node_path" : "texte/poste-envisage3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/reclassement-med",
            \ "search_node_path" : "texte/reclassement-med",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/service-accueil",
            \ "search_node_path" : "texte/service-accueil",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/situation",
            \ "search_node_path" : "texte/situation",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/taux-actu",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/taux-rech",
            \ "search_node_path" : "texte/taux-rech",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/taux-rech2",
            \ "search_node_path" : "texte/taux-rech2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/taux-rech3",
            \ "search_node_path" : "texte/taux-rech3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre1",
            \ "search_node_path" : "divers/strlibre1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre2",
            \ "search_node_path" : "divers/strlibre2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre3",
            \ "search_node_path" : "divers/strlibre3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/annee-recrutement",
            \ "search_node_path" : "texte/cantine-annee-recrutement",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/duree",
            \ "search_node_path" : "texte/duree",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "situation-recherchee/heures",
            \ "search_node_path" : "texte/heures-rech",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/adresse1",
            \ "search_node_path" : "identite/adresse1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/adresse2",
            \ "search_node_path" : "identite/adresse2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/code-postal",
            \ "search_node_path" : "identite/code-postal",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/fax",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/mail",
            \ "search_node_path" : "identite/mail",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/nom",
            \ "search_node_path" : "identite/nom",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/pays",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/reference",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/tel-fixe",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/tel-portable",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/tel-pro",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/titre",
            \ "search_node_path" : "identite/titre",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/ville",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/fonction",
            \ "search_node_path" : "texte/cantine-fonction",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/ecole",
            \ "search_node_path" : "profil/ecole",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/formation",
            \ "search_node_path" : "profil/formation",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre1",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre10",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre2",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre3",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre4",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre5",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre6",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre7",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre8",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre9",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "situation-actuelle/remuneration",
            \ "search_node_path" : "texte/remuneration-actu",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "situation-recherchee/remuneration",
            \ "search_node_path" : "texte/remuneration-rech",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/telephones",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/mention",
            \ "search_node_path" : "qualification/mention",
            \ "obj" : "txtMention@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 1,
            \ "node_path" : "qualification/date-obtention",
            \ "search_node_path" : "qualification/date-obtention",
            \ "obj" : "txtDateObtention@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 1,
            \ "node_path" : "qualification/diplome",
            \ "search_node_path" : "qualification/diplome",
            \ "obj" : "txtQualifDiplome@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 1,
            \ "node_path" : "qualification/intitule",
            \ "search_node_path" : "qualification/intitule",
            \ "obj" : "txtIntitule@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 1,
            \ "node_path" : "qualification/specialites",
            \ "search_node_path" : "qualification/specialites",
            \ "obj" : "txtQualifSpecialite@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre1",
            \ "search_node_path" : "experiences/texte-libre1",
            \ "obj" : "txtLibre1_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre10",
            \ "search_node_path" : "experiences/texte-libre10",
            \ "obj" : "txtLibre10_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre2",
            \ "search_node_path" : "experiences/texte-libre2",
            \ "obj" : "txtLibre2_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre3",
            \ "search_node_path" : "experiences/texte-libre3",
            \ "obj" : "txtLibre3_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre4",
            \ "search_node_path" : "experiences/texte-libre4",
            \ "obj" : "txtLibre4_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre5",
            \ "search_node_path" : "experiences/texte-libre5",
            \ "obj" : "txtLibre5_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre6",
            \ "search_node_path" : "experiences/texte-libre6",
            \ "obj" : "txtLibre6_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre7",
            \ "search_node_path" : "experiences/texte-libre7",
            \ "obj" : "txtLibre7_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre8",
            \ "search_node_path" : "experiences/texte-libre8",
            \ "obj" : "txtLibre8_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre9",
            \ "search_node_path" : "experiences/texte-libre9",
            \ "obj" : "txtLibre9_@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 2,
            \ "node_path" : "experiences/date-debut",
            \ "search_node_path" : "experiences/date-debut",
            \ "obj" : "txtDateDebut@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 2,
            \ "node_path" : "experiences/date-fin",
            \ "search_node_path" : "experiences/date-fin",
            \ "obj" : "txtDateFin@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 2,
            \ "node_path" : "experiences/duree",
            \ "search_node_path" : "experiences/duree",
            \ "obj" : "txtSociete@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 2,
            \ "node_path" : "experiences/fonction",
            \ "search_node_path" : "",
            \ "obj" : "txtDuree@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 2,
            \ "node_path" : "experiences/societe",
            \ "search_node_path" : "experiences/societe",
            \ "obj" : "txtFonction@",
        \ },
    \ ],
    \ "yesno" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/demande-reintegration",
            \ "search_node_path" : "divers/demande-reintegration",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/liste-aptitude",
            \ "search_node_path" : "divers/liste-aptitude",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/projet-pro-rech",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/reclassement",
            \ "search_node_path" : "divers/reclassement",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/reintegration",
            \ "search_node_path" : "divers/reintegration",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/statutaire",
            \ "search_node_path" : "divers/statutaire",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/titulaire-concours",
            \ "search_node_path" : "divers/titulaire-concours",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/titulaire-mob",
            \ "search_node_path" : "divers/titulaire-mob",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num1",
            \ "search_node_path" : "divers/num1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num2",
            \ "search_node_path" : "divers/num2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num3",
            \ "search_node_path" : "divers/num3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num4",
            \ "search_node_path" : "divers/num4",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num5",
            \ "search_node_path" : "divers/num5",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num6",
            \ "search_node_path" : "divers/num6",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num7",
            \ "search_node_path" : "divers/num7",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num8",
            \ "search_node_path" : "divers/num8",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num9",
            \ "search_node_path" : "divers/num9",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num10",
            \ "search_node_path" : "divers/num10",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num11",
            \ "search_node_path" : "divers/num11",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/permis-conduire",
            \ "search_node_path" : "divers/permis-conduire-v2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "divers/handicap",
            \ "search_node_path" : "divers/handicap",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "divers/interne",
            \ "search_node_path" : "divers/interne",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "divers/mobilite-interne",
            \ "search_node_path" : "divers/mobilite-interne",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/en-poste",
            \ "search_node_path" : "profil/en-poste",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/acquis",
            \ "search_node_path" : "qualification/acquis",
            \ "obj" : "optAcquis@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/num1",
            \ "search_node_path" : "experiences/num1",
            \ "obj" : "radNum1_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/num2",
            \ "search_node_path" : "experiences/num2",
            \ "obj" : "radNum2_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/num3",
            \ "search_node_path" : "experiences/num3",
            \ "obj" : "radNum3_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/num4",
            \ "search_node_path" : "experiences/num4",
            \ "obj" : "radNum4_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/num5",
            \ "search_node_path" : "experiences/num5",
            \ "obj" : "radNum5_@",
        \ },
    \ ],
\ }

" All (most) of the poste.xml fields.
" SELECT:
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue1",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue10",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue11",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue12",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue13",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue14",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue15",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue2",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue3",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue4",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue5",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue6",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue7",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue8",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue9",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langues-liste",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },

" SHORT:
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre1",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre10",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre2",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre3",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre4",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre5",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre6",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre7",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre8",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre9",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },

let g:poste_nodes =
\ {
    \ "date" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre1",
            \ "search_node_path" : "recherche/supplementaires/dtlibre1-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre2",
            \ "search_node_path" : "recherche/supplementaires/dtlibre2-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre3",
            \ "search_node_path" : "recherche/supplementaires/dtlibre3-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre4",
            \ "search_node_path" : "recherche/supplementaires/dtlibre4-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre5",
            \ "search_node_path" : "recherche/supplementaires/dtlibre5-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre6",
            \ "search_node_path" : "recherche/supplementaires/dtlibre6-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre7",
            \ "search_node_path" : "recherche/supplementaires/dtlibre7-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre8",
            \ "search_node_path" : "recherche/supplementaires/dtlibre8-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre9",
            \ "search_node_path" : "recherche/supplementaires/dtlibre9-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre10",
            \ "search_node_path" : "recherche/supplementaires/dtlibre10-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre11",
            \ "search_node_path" : "recherche/supplementaires/dtlibre11-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre12",
            \ "search_node_path" : "recherche/supplementaires/dtlibre12-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre13",
            \ "search_node_path" : "recherche/supplementaires/dtlibre13-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre14",
            \ "search_node_path" : "recherche/supplementaires/dtlibre14-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre15",
            \ "search_node_path" : "recherche/supplementaires/dtlibre15-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/date_delai",
            \ "search_node_path" : "recherche/date_delai-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/date_emission",
            \ "search_node_path" : "recherche/date_emission-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/signature",
            \ "search_node_path" : "recherche/signature-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/date-deliberation",
            \ "search_node_path" : "recherche/date-deliberation-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/date-prepositionnement",
            \ "search_node_path" : "recherche/date-prepositionnement-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/date-validation1",
            \ "search_node_path" : "recherche/date-validation1-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/date-validation2",
            \ "search_node_path" : "recherche/date-validation2-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/date-validation3",
            \ "search_node_path" : "recherche/date-validation3-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/date_debut",
            \ "search_node_path" : "recherche/divers/date_debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/date_fin",
            \ "search_node_path" : "recherche/divers/date_fin",
            \ "obj" : "",
        \ },
    \ ],
    \ "contact" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/contact",
            \ "search_node_path" : "recherche/descriptif/contact",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/contact2",
            \ "search_node_path" : "recherche/descriptif/contact2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/contact3",
            \ "search_node_path" : "recherche/descriptif/contact3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/contact4",
            \ "search_node_path" : "recherche/descriptif/contact4",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/contact5",
            \ "search_node_path" : "recherche/descriptif/contact5",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/contact-client",
            \ "search_node_path" : "recherche/divers/contact-client",
            \ "obj" : "",
        \ },
    \ ],
    \ "select" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable1",
            \ "search_node_path" : "recherche/descriptif/liste-editable1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable2",
            \ "search_node_path" : "recherche/descriptif/liste-editable2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable3",
            \ "search_node_path" : "recherche/descriptif/liste-editable3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable4",
            \ "search_node_path" : "recherche/descriptif/liste-editable4",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable5",
            \ "search_node_path" : "recherche/descriptif/liste-editable5",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable6",
            \ "search_node_path" : "recherche/descriptif/liste-editable6",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable7",
            \ "search_node_path" : "recherche/descriptif/liste-editable7",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable8",
            \ "search_node_path" : "recherche/descriptif/liste-editable8",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable9",
            \ "search_node_path" : "recherche/descriptif/liste-editable9",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable10",
            \ "search_node_path" : "recherche/descriptif/liste-editable10",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable11",
            \ "search_node_path" : "recherche/descriptif/liste-editable11",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable12",
            \ "search_node_path" : "recherche/descriptif/liste-editable12",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable13",
            \ "search_node_path" : "recherche/descriptif/liste-editable13",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable14",
            \ "search_node_path" : "recherche/descriptif/liste-editable14",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/section-analytique",
            \ "search_node_path" : "recherche/section-analytique",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/statut_salarie",
            \ "search_node_path" : "recherche/divers/statut_salarie",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/budget",
            \ "search_node_path" : "recherche/divers/budget",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/domaine-exp",
            \ "search_node_path" : "recherche/divers/domaine-exp",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/type-emploi",
            \ "search_node_path" : "recherche/descriptif/type-emploi",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/motif-remplacement",
            \ "search_node_path" : "recherche/motif-remplacement",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/niveau_experience",
            \ "search_node_path" : "recherche/divers/niveau_experience",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/type_contrat",
            \ "search_node_path" : "recherche/descriptif/type_contrat",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "mairie/categorie",
            \ "search_node_path" : "recherche/descriptif/categorie",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "qualification/niveau_etude",
            \ "search_node_path" : "recherche/divers/niveau_etude",
            \ "obj" : "",
        \ },
    \ ],
    \ "user" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement2",
            \ "search_node_path" : "recherche/divers/utilisateur2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement3",
            \ "search_node_path" : "recherche/divers/utilisateur3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement4",
            \ "search_node_path" : "recherche/divers/utilisateur4",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement5",
            \ "search_node_path" : "recherche/divers/utilisateur5",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement6",
            \ "search_node_path" : "recherche/divers/utilisateur6",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement7",
            \ "search_node_path" : "recherche/divers/utilisateur7",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement8",
            \ "search_node_path" : "recherche/divers/utilisateur8",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement9",
            \ "search_node_path" : "recherche/divers/utilisateur9",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement10",
            \ "search_node_path" : "recherche/divers/utilisateur10",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/responsable",
            \ "search_node_path" : "recherche/divers/responsable",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement",
            \ "search_node_path" : "recherche/divers/utilisateur",
            \ "obj" : "",
        \ },
    \ ],
    \ "integer" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre1",
            \ "search_node_path" : "recherche/supplementaires/ilibre1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre2",
            \ "search_node_path" : "recherche/supplementaires/ilibre2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre3",
            \ "search_node_path" : "recherche/supplementaires/ilibre3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre4",
            \ "search_node_path" : "recherche/supplementaires/ilibre4",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre5",
            \ "search_node_path" : "recherche/supplementaires/ilibre5",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre6",
            \ "search_node_path" : "recherche/supplementaires/ilibre6",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre7",
            \ "search_node_path" : "recherche/supplementaires/ilibre7",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre8",
            \ "search_node_path" : "recherche/supplementaires/ilibre8",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre9",
            \ "search_node_path" : "recherche/supplementaires/ilibre9",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre10",
            \ "search_node_path" : "recherche/supplementaires/ilibre10",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre11",
            \ "search_node_path" : "recherche/supplementaires/ilibre11",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre12",
            \ "search_node_path" : "recherche/supplementaires/ilibre12",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre13",
            \ "search_node_path" : "recherche/supplementaires/ilibre13",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre14",
            \ "search_node_path" : "recherche/supplementaires/ilibre14",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre15",
            \ "search_node_path" : "recherche/supplementaires/ilibre15",
            \ "obj" : "",
        \ },
    \ ],
    \ "long" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/commentaire1",
            \ "search_node_path" : "recherche/commentaire1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/commentaire2",
            \ "search_node_path" : "recherche/commentaire2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/commentaire3",
            \ "search_node_path" : "recherche/commentaire3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/commentaire4",
            \ "search_node_path" : "recherche/commentaire4",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/commentaire5",
            \ "search_node_path" : "recherche/commentaire5",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/indemnite_divers",
            \ "search_node_path" : "recherche/indemnite_divers",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/indemnite_logement",
            \ "search_node_path" : "recherche/indemnite_logement",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/indemnite_repas",
            \ "search_node_path" : "recherche/indemnite_repas",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/indemnite_transport",
            \ "search_node_path" : "recherche/indemnite_transport",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/motif_recrutement_surcroitactivite",
            \ "search_node_path" : "recherche/motif_recrutement_surcroitactivite",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/responsabilites",
            \ "search_node_path" : "recherche/responsabilites",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/domaine",
            \ "search_node_path" : "recherche/domaine",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/prepos-commentaire",
            \ "search_node_path" : "recherche/prepos-commentaire",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "qualification/commentaires",
            \ "search_node_path" : "recherche/commentaires",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "qualification/competences",
            \ "search_node_path" : "recherche/competences",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "qualification/formation",
            \ "search_node_path" : "recherche/formation",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "qualification/specialites",
            \ "search_node_path" : "recherche/specialites",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "qualification/whyopen",
            \ "search_node_path" : "recherche/whyopen",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-long-lienref1",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-long-lienref2",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-long-lienref3",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-long-lienref4",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-long-lienref5",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/contexte",
            \ "search_node_path" : "recherche/contexte",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/mission",
            \ "search_node_path" : "recherche/mission",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/temoignage",
            \ "search_node_path" : "recherche/temoignage",
            \ "obj" : "",
        \ },
    \ ],
    \ "short" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre1",
            \ "search_node_path" : "recherche/supplementaires/strlibre1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre2",
            \ "search_node_path" : "recherche/supplementaires/strlibre2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre3",
            \ "search_node_path" : "recherche/supplementaires/strlibre3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre4",
            \ "search_node_path" : "recherche/supplementaires/strlibre4",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre5",
            \ "search_node_path" : "recherche/supplementaires/strlibre5",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre6",
            \ "search_node_path" : "recherche/supplementaires/strlibre6",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre7",
            \ "search_node_path" : "recherche/supplementaires/strlibre7",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre8",
            \ "search_node_path" : "recherche/supplementaires/strlibre8",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre9",
            \ "search_node_path" : "recherche/supplementaires/strlibre9",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre10",
            \ "search_node_path" : "recherche/supplementaires/strlibre10",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre11",
            \ "search_node_path" : "recherche/supplementaires/strlibre11",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre12",
            \ "search_node_path" : "recherche/supplementaires/strlibre12",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre13",
            \ "search_node_path" : "recherche/supplementaires/strlibre13",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre14",
            \ "search_node_path" : "recherche/supplementaires/strlibre14",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre15",
            \ "search_node_path" : "recherche/supplementaires/strlibre15",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/probabilite",
            \ "search_node_path" : "recherche/probabilite",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/duree_jour_max",
            \ "search_node_path" : "recherche/duree_jour_max",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/duree_jour_min",
            \ "search_node_path" : "recherche/duree_jour_min",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/duree_mois_max",
            \ "search_node_path" : "recherche/duree_mois_max",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/duree_mois_min",
            \ "search_node_path" : "recherche/duree_mois_min",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/montant",
            \ "search_node_path" : "recherche/montant",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/prix_jour_max",
            \ "search_node_path" : "recherche/prix_jour_max",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/prix_jour_min",
            \ "search_node_path" : "recherche/prix_jour_min",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/heures",
            \ "search_node_path" : "recherche/heures",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/heures-hebdo",
            \ "search_node_path" : "recherche/heures-hebdo",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-court-lienref1",
            \ "search_node_path" : "recherche/descriptif/libre-court-lienref1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-court-lienref2",
            \ "search_node_path" : "recherche/descriptif/libre-court-lienref2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-court-lienref3",
            \ "search_node_path" : "recherche/descriptif/libre-court-lienref3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-court-lienref4",
            \ "search_node_path" : "recherche/descriptif/libre-court-lienref4",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-court-lienref5",
            \ "search_node_path" : "recherche/descriptif/libre-court-lienref5",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/nom_salarie_absent",
            \ "search_node_path" : "recherche/nom_salarie_absent",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/regime_travail",
            \ "search_node_path" : "recherche/regime_travail",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/salaire-brut",
            \ "search_node_path" : "recherche/salaire-brut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/travail-equipe-texte",
            \ "search_node_path" : "recherche/travail-equipe-texte",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/duree",
            \ "search_node_path" : "recherche/duree",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/num-declaration",
            \ "search_node_path" : "recherche/divers/num-declaration",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/coefficient",
            \ "search_node_path" : "recherche/divers/coefficient",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/intitule",
            \ "search_node_path" : "recherche/descriptif/intitule",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/lieu",
            \ "search_node_path" : "recherche/divers/lieu",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/nb_poste",
            \ "search_node_path" : "recherche/nb_poste",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/reference",
            \ "search_node_path" : "recherche/descriptif/reference",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/salaire-negocie",
            \ "search_node_path" : "recherche/salaire-negocie",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "localisation/adresse1",
            \ "search_node_path" : "recherche/localisation/adresse1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "localisation/adresse2",
            \ "search_node_path" : "recherche/localisation/adresse2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "localisation/code-postal",
            \ "search_node_path" : "recherche/localisation/code-postal",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "localisation/latitude",
            \ "search_node_path" : "recherche/localisation/latitude",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "localisation/longitude",
            \ "search_node_path" : "recherche/localisation/longitude",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "localisation/pays",
            \ "search_node_path" : "recherche/localisation/pays",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "localisation/region",
            \ "search_node_path" : "recherche/localisation/region",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "localisation/ville",
            \ "search_node_path" : "recherche/localisation/ville",
            \ "obj" : "",
        \ },
    \ ],
    \ "yesno" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/besoin_avancement",
            \ "search_node_path" : "recherche/besoin_avancement",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/besoin_recurrent",
            \ "search_node_path" : "recherche/besoin_recurrent",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/renouvelable",
            \ "search_node_path" : "recherche/renouvelable",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/clause_nonconcurrence",
            \ "search_node_path" : "recherche/clause_nonconcurrence",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/periode_essai",
            \ "search_node_path" : "recherche/divers/periode-essai",
            \ "obj" : "",
        \ },
    \ ],
\ }

" The above data structures are lists of nodes organized by field type.  Each
" node has properties associeated with it. This function takes a list of nodes
" of and returns a list containing a specific property of all nodes that match
" some condition. In practice, the use of this function will be to return a
" list of node paths or a list of nodes's searchable paths.
function! GetListOfFieldAttributes(list_of_nodes, node_field, condition, field_to_return)
    let result = []
    for node in a:list_of_nodes
        if node[a:node_field] ==# a:condition
            call add(result, node[a:field_to_return])
        endif
    endfor
    return result
endfunction

" Given a node's path, returns the field type of that node.
function! GetNodeFieldType(field_data_structure, node_path)
    for field_type in keys(a:field_data_structure)
        for field in a:field_data_structure[field_type]
            if field['node_path'] ==# a:node_path
                return field_type
            endif
        endfor
    endfor
    return ""
endfunction

" Does a brute force search through the above data structure for the search
" path associated with a node.
function! GetSearchPath(field_data_structure, node_path)
    for field_type in keys(a:field_data_structure)
        for field in a:field_data_structure[field_type]
            if field['node_path'] ==# a:node_path
                return field['search_node_path']
            endif
        endfor
    endfor
    return ""
endfunction

" I'm slowly getting all my functions to do things based off these new field
" data structures. This one replaces the function that I had originally.
function! GetSearchPathAndOpenSplit(path_to_node, cand_or_poste)
    let search_path = ""
    if a:cand_or_poste
        let search_path = GetSearchPath(g:candidat_nodes, a:path_to_node)
        split recherche.xml
    else
        let search_path = GetSearchPath(g:poste_nodes, a:path_to_node)
        split
    endif
    if search_path ==# ""
        quit
    endif
    return search_path
endfunction

" }}}

" Adding/Modifying Fields {{{

" Assumes you are in the json file. Changes the label attached to a constant.

" TODO: Look into running translator:merge if the constant cannot be found.

" Sometimes in the json file, a constant points to another constant, which
" points to another, etc... This makes it tricky to find the REAL label that
" we need to change. My current solution is sort of a hack. I noticed that all
" constants start with TXT. So I make a check, if TXT occurrs 2 times on a
" single line then a constant must be pointing to another constant. This is a
" total hack because if a label actually has the text "TXT" in it then this
" function won't work.
function! ChangeLabel(constant_to_find, new_label)
    " Move to the top of the file
    call cursor(1, 1)
    let not_found = 1
    let to_find = a:constant_to_find
    while not_found
        " Look for the constant
        if search('\<' . to_find . '\>')
            " If we don't see two TXT's then it's safe to change the label
            if getline('.') !~# "TXT.*TXT"
                " Update the label with the new text
                " ERROR: Really weird vim error. If The line below ends with
                " '",' instead of '"' like it is now, the '%' command doesn't
                " work properly.
                let replacement_line = matchstr(getline('.'), '[^"]*"[^"]*"[^"]*') . '"' . a:new_label . '"' . ','
                call setline('.', replacement_line)
                " Break out of the loop
                let not_found = 0
            " Otherwise we grab the other constant and search for THAT
            else
                normal! $
                call search("TXT", 'b', line('.'))
                let save_unnamed_register = @@
                normal! yiw
                let to_find = @@
                let @@ = save_unnamed_register
            endif
        else
            " Let the user know of any errors
            echoerr "Could not find the label: " . to_find
            " Break out of the loop
            let not_found = 0
        endif
    endwhile
endfunction

" Echo's the label for a xml node if you're in the CDATA OR at the node
" itself. Useful if one of us didn't add the label for a node in the CDATA.
" Check out this page
" http://vim.wikia.com/wiki/Avoid_scrolling_when_switch_buffers for more about
" saving and restoring window views. Or see this:
" http://stackoverflow.com/questions/4251533/vim-keep-window-position-when-switching-buffers
function! GetLabelNameWrapper()
    let save_view = winsaveview()
    if getline('.') !~# 'TXT'
        call LuceoFindNode()
    endif
    if getline('.') =~# 'TXT'
        normal! 0
        call search('TXT', '', line('.'))
        let save_unnamed_register = @@
        normal! yiw
        " I have 'hidden' set so this isn't necessary anyway. But I want to
        " test this to make sure it does what I think it should (i.e hide the
        " current buffer and edit the json file. Then when I return to the
        " first file it will unhide it.
        silent hide edit ../tools/translations/php.en.json
        let label = GetLabelName(@@)
        " If I edit a new file AND echo a message, it prompts me to hit enter
        " after the echo message. I think it happens because both actions
        " print something to screen or something like that. For example if I
        " run the command :echo 'no' | echo 'yes' then I will be asked to hit
        " enter, but If I just do :echo 'no' then I am not prompted. Since we
        " don't want to hit enter so we run this command silently. We run the
        " above buffer switch silently for the same reason.
        silent edit #
        if label !=# ''
            echo label
        else
            " We couldn't find a label, print error message.
            echohl ErrorMsg
            echo "Could not find the label: ".@@
            echohl None
        endif
        let @@ = save_unnamed_register
    endif
    call winrestview(save_view)
endfunction

" Does the main work of finding the json label's value. It has the same sort
" of loop structure as the ChangeLabel() function.
function! GetLabelName(label_constant)
    " Move to the top of the file
    call cursor(1, 1)
    let to_find = a:label_constant
    while search('\<' . to_find . '\>')
        " If we don't see two TXT's then we've found our label
        if getline('.') !~# "TXT.*TXT"
            " Return the label to echo out.
            return matchstr(getline('.'), '^[^"]*"[^"]*"[^"]*"\zs[^"]*')
        else
            " Otherwise we grab the other constant and search for THAT
            normal! $
            call search("TXT", 'b', line('.'))
            let save_unnamed_register = @@
            normal! yiw
            let to_find = @@
            let @@ = save_unnamed_register
        endif
    endwhile
    return ''
endfunction

" Returns the proper amount of spacing for the comment string.
function! GetProperIndentationHelper(pattern, offset, line_no)
    call cursor(a:line_no, 1)
    if search(a:pattern, '', line('.'))
        return repeat(' ', virtcol('.') - a:offset - 1)
    else
        return ""
    endif
endfunction

" Returns a string that attempts to be a properly indented comment string with
" respect to neighboring comment strings. The main inspiration for this was to
" get a properly indented comment string when using my mappings which add a
" field, but it could also be used when changing the label in the comment
" strings in menu.xml. 
function! GetProperIndentation(pattern, offset, line_no)
    let save_pos = getpos('.')
    let num_surrounding_lines = 2
    let spacing = ""
    " I do call reverse() because it seems that looking downwards first for
    " matching indentation generally has better results.
    for i in reverse(range(- num_surrounding_lines, num_surrounding_lines))
        let spacing = GetProperIndentationHelper(a:pattern, a:offset, a:line_no + i)
        if spacing !=# ""
            break
        endif
    endfor
    call setpos('.', save_pos)
    return spacing
endfunction

" Adds a field to CDATA on the line below "line_no"
function! AddCDATAField(line_no, contents, label)
    let indent = GetProperIndentation('[', 0, a:line_no)
    let field = indent . '[' . a:contents . ']'
    let comment = GetProperIndentation('<!--', len(field), a:line_no) . '<!-- ' . a:label . ' -->'
    call append(a:line_no, field . comment)
endfunction

" Changes the comment string, like the ones in CDATA to have a new label.
function! ChangeCommentStr(label)
    let pattern = '<!--'
    " Delete the comment and add a new one.
    execute "substitute/\s*" . pattern . ".*//e"
    normal! $
    " TODO: Is there a way to do this 'virtcol()' stuff without having to move
    " the cursor around? Like we could get the current line as a string and
    " there would be a sort of 'strvirtcol()' function. I suppose I couuld
    " just replace all tabs with spaces in the string.
    let comment = GetProperIndentation(pattern, virtcol('.'), line('.')) . '<!-- ' . a:label . ' -->'
    execute "substitute/$/" . escape(comment, '/') . '/e'
endfunction

" Technically this just returns all line numbers where the double quoted
" string 'path' occurs. It's purpose is to return a list of all lines of
" CDATA. This list is sorted highest to lowest.
function! GetCDATALines(path)
    let line_nums = []
    let col_nums = []
    " TODO: Maybe I should just have the a:path argument be the actual CDATA
    " stuff: [champ path...]. That would probably be better.
    execute 'silent global/"' . escape(a:path, '/') . '"/call insert(line_nums, line(".")) | call search("a:path") | call insert(col_nums, col("."))'
    " Removes any line number from line_nums where the pattern occurred in an
    " xml comment.
    let result = []
    let i = 0
    while i <# len(line_nums)
        if synIDattr(synID(line_nums[i], col_nums[i], 1), "name") !=# 'xmlCommentPart'
            call add(result, line_nums[i])
        endif
        let i += 1
    endwhile
    return result
endfunction

" I noticed a pattern whenever I had to make some extra change for a contact
" and select fields so I abstracted that pattern into this function.
function! HandleContactOrSelectChanges(field_type, path, actif_val, new_label)
    if a:field_type ==# "select"
        let attribute = 'type-liste'
        let file = 'menu.xml'
    else
        let attribute = 'type-contact'
        let file = 'correspondance.xml'
    endif
    call GoToXMLNode(a:path)
    let attr_val  = GetNodeAttribute(attribute)
    execute "split " file
    call search('\<' . attr_val . '\>')
    if a:actif_val !=# ""
        call ChangeNodeAttribute("actif", a:actif_val)
    endif
    if a:new_label !=# ""
        call ChangeCommentStr(a:new_label)
    endif
    write
    wincmd p
endfunction

" Changes a node's label
function! ChangeNodeLabel(label, path, cdata_lines, field_type, cand_or_poste)
    " Change the comment string in  all cdata lines in current file.
    for i in a:cdata_lines
        call cursor(i, 1)
        call ChangeCommentStr(a:label)
    endfor
    write
    " Change the label in php.en.json
    call GoToXMLNode(a:path)
    let json_constant = GetNodeAttribute('libelle')
    split ../tools/translations/php.en.json
    call ChangeLabel(json_constant, a:label)
    write
    call RunTranslatorGenerate()
    wincmd p
    " Change the comment string in menu.xml if necessary.
    if a:field_type ==# "select"
        call HandleContactOrSelectChanges(a:field_type, a:path, '', a:label)
    endif
    " Try changing the comment string in the searchable portion.
    let search_path = GetSearchPathAndOpenSplit(a:path, a:cand_or_poste)
    if search_path !=# ""
        let search_cdata_lines = GetCDATALines(search_path)
        if !empty(search_cdata_lines)
            for i in search_cdata_lines
                call cursor(i, 1)
                call ChangeCommentStr(a:label)
            endfor
            write
        else
            " Close the window, there is no searchable portion.
            quit
        endif
    endif
endfunction

" Assumes that the cursor is on a line of CDATA to change.
function! ChangeNodeLabelWrapper()
    let field_type = GetFieldType()
    let label = input("New Label: ")
    let path = GetNodePathFromCDATA(line('.'))
    let cdata_lines = GetCDATALines(path)
    if expand('%:t') ==# "candidat.xml"
        let cand_or_poste = 1
    else
        let cand_or_poste = 0
    endif
    call ChangeNodeLabel(label, path, cdata_lines, field_type, cand_or_poste)
endfunction

" Activates a node, makes it mandatory, and adds it to CDATA
function! ActivateNode(cdata_pos, path, extra_cdata_contents, is_mandatory, label)
    call AddCDATAField(a:cdata_pos, 'champ cle="' . a:path . '"' . a:extra_cdata_contents, a:label)
    call GoToXMLNode(a:path)
    call ChangeNodeAttribute("actif", "true")
    if a:is_mandatory
        call ChangeNodeAttribute("obligatoire", "true")
    endif
    write
endfunction

" Calls the above function to 'activate' the node and also changes the node's label
function! BasicAddField(cdata_pos, path, extra_cdata_contents, is_mandatory, label)
    call ActivateNode(a:cdata_pos, a:path, a:extra_cdata_contents, a:is_mandatory, a:label)
    let json_constant = GetNodeAttribute('libelle')
    split ../tools/translations/php.en.json
    call ChangeLabel(json_constant, a:label)
    write
    call RunTranslatorGenerate()
    wincmd p
endfunction

" Finds a free node if the field is 'regular' (i.e not a dropwdown)
function! FindFreeRegularNode(list_of_paths)
    for path in a:list_of_paths
        " TODO: Check if we were able to even go to such a node.
        call GoToXMLNode(path)
        if GetNodeAttribute('actif') ==# "false"
            return path
        endif
    endfor
    return ""
endfunction

" Another helper for the function to find an available dropdown. It looks at
" all nodes with the specified list type and makes sure that none of them are
" active.
function! FindFreeSelectHelper2(list_type)
    call cursor(1, 1)
    while search('\<' . a:list_type . '\>', 'W')
        if GetNodeAttribute('actif') ==# "true"
            return 0
        endif
        " Move the cursor down one more line
        call cursor(line('.') + 1, 1)
    endwhile
    return 1
endfunction

" A helper for the function to find an available dropdown. It will check
" candidat.xml and poste.xml for any active nodes attached to a list type AND
" will check menu.xml just to make sure it isn't active there either.
function! FindFreeSelectHelper1(list_type)
    " Check candidat.xml and poste.xml
    edit candidat.xml
    let is_available = FindFreeSelectHelper2(a:list_type)
    edit poste.xml
    let is_available2 = FindFreeSelectHelper2(a:list_type)
    " If both files aren't using that list
    if is_available && is_available2
        " Check menu.xml JUST to make sure it's not being used
        edit menu.xml
        call cursor(1, 1)
        call search('\<' . a:list_type . '\>', 'W')
        let actif = GetNodeAttribute('actif')
        if actif ==# "false"
            return 1
        endif
    endif
    return 0
endfunction

" Finds a free menu item.
" TODO: I used to have a cand_or_poste attribute for these functions which I
" removed because I thought I didn't need them. Turns out they were probably
" helping out. Right now I've got it working again with all those 'wincmd'
" statements you see but it feels kind of hacky. Let's try to patch all that
" up.
function! FindFreeSelectNode(list_of_paths)
    " Open up a separate split to do all the searching in.
    split
    let return_path = ""
    for path in a:list_of_paths
        " Search in the buffer we were originally editing (which will be
        " poste.xml or candidat.xml) 
        wincmd p
        call GoToXMLNode(path)
        if GetNodeAttribute('actif') ==# "false"
            let list_type = GetNodeAttribute('type-liste')
            " Go back to the open split to do the searching in
            wincmd p
            if FindFreeSelectHelper1(list_type)
                let return_path = path
                break
            endif
        else
            wincmd p
        endif
    endfor
    " Close the split that was opened
    quit
    return return_path
endfunction

" Returns the path to a node that is available for use
function! FindFreeNode(field_type, list_of_paths)
    if a:field_type ==# "select"
        return FindFreeSelectNode(a:list_of_paths)
    else
        return FindFreeRegularNode(a:list_of_paths)
    endif
endfunction

" Makes a node searchable 
function! MakeFieldSearchable(path_to_node, cand_or_poste, label)
    let search_path = GetSearchPathAndOpenSplit(a:path_to_node, a:cand_or_poste)
    if search_path !=# ""
        let zmark_line_no = line("'z")
        call ActivateNode(zmark_line_no, search_path, "", 0, a:label)
        " Move the 'z mark down one line
        execute (zmark_line_no+1) . "mark z"
        " Go back to previous window
        wincmd p
    else
        echoerr "Could not add the searchable aspect of this field. You will have to do it manually."
    endif
endfunction

" The javascript I wrote to pull the information for me from the wiki:
" var tbody = document.getElementById('lucastest').getElementsByTagName('tbody')[0];
" var rows = tbody.getElementsByTagName('tr');
" var result_arr = [];
" for(var i = 0; i < rows.length; i++) {
"   var columns = rows[i].getElementsByTagName('td');
"   if(columns[3].innerHTML.indexOf("Date") > -1 && columns[6].innerHTML.indexOf("Yes") > -1) {
"     result_arr[result_arr.length] = columns[0].innerHTML.trim();
"   }
" }
" console.log(result_arr);

" This function adds a field. It also tries to make the field searchable.
" TODO: Make the 'is_searchable' argument a string that is the search path of
" the field. The AddWrapper() function will be passing in the search path.
" Let's also do the 'split'ing in this function, lets essentially get rid of
" that MakeFieldSearchable() function altogether.
function! AddField(cand_or_poste, cdata_pos, extra_cdata_contents, field_type, path, is_mandatory, is_searchable, label)
    " Activates the field, makes it mandatory (if necessary), and adds it to
    " CDATA
    call BasicAddField(a:cdata_pos, a:path, a:extra_cdata_contents, a:is_mandatory, a:label)

    " Extra steps must be taken for some types of fields
    call GoToXMLNode(a:path)
    if a:field_type ==# "select"
        call HandleContactOrSelectChanges(a:field_type, a:path, 'true', a:label)
    elseif a:field_type ==# "contact"
        call HandleContactOrSelectChanges(a:field_type, a:path, 'true', '')
    endif

    " Attempt to make the field searchable
    if a:is_searchable
        call MakeFieldSearchable(a:path, a:cand_or_poste, a:label)
    endif
    " Leave the cursor in the cdata where the field was added.
    call cursor(a:cdata_pos + 1, 1)
endfunction

" Gets a valid field type for use when adding/removing fields and such.
function! GetFieldType()
    let valid_types = ["short", "long", "yesno", "date", "select", "contact", "user", "integer"]
    let prompt_str = join(valid_types, "\n")
    let my_count = 0
    let good_input = 0
    while !good_input
        let input = input("The valid field types are:\n" . prompt_str . "\nEnter a field type: ")
        " lowercase the input and strip out leading and trailing whitespace.
        let result = tolower(substitute(input, '^\s*\(\S*\)\s*$', '\1', ''))
        if index(valid_types, result) !=# -1
            let good_input = 1
        else
            if my_count <# 2
                echo "\nNo you fool, that isn't valid. Try again.\n\n"
            else
                echo "\nWhat are you stupid? The list is right there.\n\n"
            endif
            let my_count += 1
        endif
    endwhile
    return result
endfunction

" A wrapper for the function which adds a field. This wrapper will add a
" 'regular' field.
" TODO: Currently this function is BROKEN (sort of). The call to
" GetListOfFieldAttributes() isn't quite right because it will pick up
" repeatable sections as being reusable even though they shouldn't. The calls
" to this function in the AddRepeatableFieldWrapper() aren't quite right
" either because it will pick up fields that aren't reusable.
function! AddFieldWrapper()
    let cdata_line_no = line('.')
    " TODO: Adjust this function so it takes a list of valid types that could
    " be added.
    let field_type = GetFieldType()
    if expand('%:t') ==# "candidat.xml"
        let cand_or_poste = 1
        let available_nodes = GetListOfFieldAttributes(g:candidat_nodes[field_type], 'reusable', 1, 'node_path')
    else
        let cand_or_poste = 0
        let available_nodes = GetListOfFieldAttributes(g:poste_nodes[field_type], 'reusable', 1, 'node_path')
    endif
    let path = FindFreeNode(field_type, available_nodes)
    if path !=# ''
        call AddField(cand_or_poste, cdata_line_no, "", field_type, path, input("Mandatory: "), input("Searchable: "), input("Label: "))
    endif
endfunction

" A wrapper for the function which adds a field. This wrapper adds a
" 'repeatable' field.
function! AddRepeatableFieldWrapper(which_repeatable_section)
    let cdata_line_no = line('.')
    let field_type = GetFieldType()
    if a:which_repeatable_section ==# 0
        let available_paths = GetListOfFieldAttributes(g:candidat_nodes[field_type], 'field_category', 1, 'node_path')
        let available_objs = GetListOfFieldAttributes(g:candidat_nodes[field_type], 'field_category', 1, 'obj')
    else
        let available_paths = GetListOfFieldAttributes(g:candidat_nodes[field_type], 'field_category', 2, 'node_path')
        let available_objs = GetListOfFieldAttributes(g:candidat_nodes[field_type], 'field_category', 2, 'obj')
    endif
    let path = FindFreeNode(field_type, available_paths)
    if path !=# ''
        call AddField(1, cdata_line_no, ' obj="' . available_objs[index(available_paths, path)] . '"', field_type, path, input("Mandatory: "), input("Searchable: "), input("Label: "))
    endif
endfunction

" Assumes the cursor is on the xml node to add and mark 'y has the position in
" the CDATA to add this field.
function! AddSpecificFieldWrapper()
    let path = GetPathToXMLNode()
    " Again the hack where every xml file except candidat includes the root
    " tags in the path. We remove the root tag from the path.
    if expand('%:t') ==# 'candidat.xml'
        let cand_or_poste = 1
        let field_type = GetNodeFieldType(g:candidat_nodes, path)
    else
        let cand_or_poste = 0
        let path = substitute(path, "[^/]*/", "", "")
        let field_type = GetNodeFieldType(g:poste_nodes, path)
    endif
    normal! 'y
    call AddField(cand_or_poste, line("'y"), "", field_type, path, input("Mandatory: "), input("Searchable: "), input("Label: "))
endfunction

" Returns a string representing an available json label. Assumes that the
" php.en.json file is being edited.
function! GetFreeJsonLabel()
    let free_pattern = 'TXT_LIBRE\d\+'
    call cursor(1, 1)
    while search(free_pattern, 'W')
        let save_unnamed_register = @@
        " Go to the constant's value and yank it.
        normal! $F"yi"
        if @@ ==# ""
            " We yank the name of constant
            normal! 0fXyi"
            let return_val = @@
            let @@ = save_unnamed_register
            return return_val
        endif
    endwhile
    return ""
endfunction

" Adds a 'section' tag in the CDATA
function! AddSection(label)
    split ../tools/translations/php.en.json
    let json_constant = GetFreeJsonLabel()
    call ChangeLabel(json_constant, a:label)
    write
    call RunTranslatorGenerate()
    wincmd p
    call AddCDATAField(line('.'), 'section lib="' . json_constant . '" ' . 'width="w3"', a:label)
    write
endfunction

" A wrapper for the above function
function! AddSectionWrapper()
    let label = input('Enter Label: ')
    call AddSection(label)
endfunction

" TODO: Abstract these four functions a little more. We can do this with less
" code I think. Also need to add a mapping for the one below, I want to do
" ,ll but that mapping already renames a field.

" Adds a 'label' tag in the CDATA
function! AddLabel(label)
    split ../tools/translations/php.en.json
    let json_constant = GetFreeJsonLabel()
    call ChangeLabel(json_constant, a:label)
    write
    call RunTranslatorGenerate()
    wincmd p
    call AddCDATAField(line('.'), 'label lib="' . json_constant . '" ' . 'options="titre"', a:label)
    write
endfunction

" A wrapper for the above function
function! AddLabelWrapper()
    let label = input('Enter Label: ')
    call AddLabel(label)
endfunction


" }}}

" Removing Fields {{{

" Removes a 'normal' field by deactivating it and deleting all lines of CDATA
" that reference it.
function! BasicRemoveField(cdata_lines, path)
    " Deactivate the field
    call GoToXMLNode(a:path)
    call ChangeNodeAttribute("actif", "false")
    call ChangeNodeAttribute("obligatoire", "false")
    call ChangeNodeAttribute("liste", "false")
    " Deletes all the CDATA
    for i in a:cdata_lines
        call cursor(i, 1)
        delete
    endfor
    write
endfunction

" Removes a field.
function! RemoveField(cdata_lines, path, field_type, cand_or_poste)
    call BasicRemoveField(a:cdata_lines, a:path)

    " Extra steps must be taken for some types of fields
    call GoToXMLNode(a:path)
    if a:field_type ==# "select"
        call HandleContactOrSelectChanges(a:field_type, a:path, 'false', '')
    elseif a:field_type ==# "contact"
        call HandleContactOrSelectChanges(a:field_type, a:path, 'false', '')
    endif

    let search_path = GetSearchPathAndOpenSplit(a:path, a:cand_or_poste)
    if search_path !=# ""
        let search_cdata_lines = GetCDATALines(search_path)
        if !empty(search_cdata_lines)
            call BasicRemoveField(search_cdata_lines, search_path)
            write
        else
            " Close the split that was opened. No change was needed.
            quit
        endif
    else
        echoerr "Could not remove the searchable aspect of this field (if there is one). You will have to look into it manually."
    endif
endfunction

" Wrapper for the above function.
function! RemoveFieldWrapper()
    " Get the node's path and all lines of CDATA to delete.
    let path = GetNodePathFromCDATA(line('.'))
    let cdata_lines = GetCDATALines(path)
    if expand('%:t') ==# "candidat.xml"
        let field_type = GetNodeFieldType(g:candidat_nodes, path)
        let cand_or_poste = 1
    else
        let field_type = GetNodeFieldType(g:poste_nodes, path)
        let cand_or_poste = 0
    endif
    call RemoveField(cdata_lines, path, field_type, cand_or_poste)
endfunction

" }}}

" Candidate Workflow {{{

function! OrderXMLNodes(str) range
    let num = 0
    for linenum in range(a:firstline, a:lastline)
        let curr_line = getline(linenum)
        if match(curr_line, a:str . '\d\+') !=# -1
            let num += 1
            let replace_line = substitute(curr_line, '\d\+', num, '')
            call setline(linenum, replace_line)
        endif
    endfor
    " Not sure if this is call to "normal!" is necessary, I want to make sure
    " we are on the same line as the <transitions> node so we can change the
    " "nombre" attribute.
    execute "normal! vato\<ESC>"
    let curr_line = getline('.')
    let replace_line = substitute(curr_line, '\d\+', num, '')
    call setline('.', replace_line)
endfunction

" A function that will put the numbers associated with the transition nodes in
" in order.
function! OrderTransitions() range
    let num = 0
    for linenum in range(a:firstline, a:lastline)
        let curr_line = getline(linenum)
        if match(curr_line, '<transition\d\+') !=# -1
            let num += 1
            let replace_line = substitute(curr_line, '\d\+', num, '')
            call setline(linenum, replace_line)
        endif
    endfor
    " Not sure if this is call to "normal!" is necessary, I want to make sure
    " we are on the same line as the <transitions> node so we can change the
    " "nombre" attribute.
    execute "normal! vato\<ESC>"
    let curr_line = getline('.')
    let replace_line = substitute(curr_line, '\d\+', num, '')
    call setline('.', replace_line)
endfunction

" Returns a string of a transition node
function! GenTransitionStr(num, de, vers, icone)
    let trans = "<transition"
    let state = "AFFECTATION_WORKFLOW_ETAT_"
    let rest = 'onaction="" libelle-onaction="" visible=""/>'
    return trans . a:num . ' de="' . state . a:de . '" vers="' . state . a:vers . '" icone="icones/workflow' . a:icone . '.png" ' . rest
endfunction

function! AppendStrHelper(str)
    let line_num = line('.')
    call append(line_num, a:str)
    call cursor(line_num+1, 1)
endfunction

" Right now I assume that state 1 goes to 2, 2 to 3, etc... all the way to the
" accept state. I've only seen it a few times where builds didn't have that
" standard progression, but I'm wondering if I should use a dictionary to
" encode ALL the transitions. That way it could ALWAYS work. But in reality
" it's probably just easier to change the one or two nodes that aren't
" standard. I'll keep this thought in mind though.

" Writes out the transitions for a candidate workflow. 
" skip_logic - A dictionary (i.e. associative array) mapping a state X to a
" list of states that X can go to. You can also specify a scalar instead of a
" list.
function! WriteTransitions(num_states, skip_logic)
    " Constants
    let applied_state = "DEMANDE"
    let sourced_state = "POSITIONNE"
    let accepted_state = "ACCEPTE"
    let reject_state = "NEGATIF"
    let default_up_icon = 6
    let default_skip_icon = 7
    let default_reject_icon = 11
    let trans_num = 1
    " Write all the go standards
    let indent = repeat(' ', indent('.') + &tabstop)
    call AppendStrHelper(indent . '<!-- go standards -->')
    call AppendStrHelper(indent . GenTransitionStr(trans_num, applied_state, 1, default_up_icon))
    let trans_num += 1
    for i in range(1, a:num_states - 1)
        call AppendStrHelper(indent . GenTransitionStr(trans_num, i, i+1, default_up_icon))
        let trans_num += 1
    endfor
    call AppendStrHelper(indent . GenTransitionStr(trans_num, trans_num-1, accepted_state, default_up_icon))
    let trans_num += 1
    call AppendStrHelper('')

    " Write all the skip steps
    call AppendStrHelper(indent . "<!-- saut de puce -->")
    call AppendStrHelper(indent . GenTransitionStr(trans_num, sourced_state, 1, default_up_icon))
    let trans_num += 1
    for key in keys(a:skip_logic)
        if type(a:skip_logic[key]) !=# type([])
            call AppendStrHelper(indent . GenTransitionStr(trans_num, key, a:skip_logic[key], default_skip_icon))
            let trans_num += 1
        else
            for i in a:skip_logic[key]
                call AppendStrHelper(indent . GenTransitionStr(trans_num, key, i, default_skip_icon))
                let trans_num += 1
            endfor
        endif
    endfor
    call AppendStrHelper('')

    " Write all the nogo standards
    call AppendStrHelper(indent . "<!-- nogo standards -->")
    call AppendStrHelper(indent . GenTransitionStr(trans_num, applied_state, reject_state, default_reject_icon))
    let trans_num += 1
    call AppendStrHelper(indent . GenTransitionStr(trans_num, sourced_state, reject_state, default_reject_icon))
    let trans_num += 1
    for i in range(1, a:num_states)
        call AppendStrHelper(indent . GenTransitionStr(trans_num, i, reject_state, default_reject_icon))
        let trans_num += 1
    endfor
    call AppendStrHelper(indent . GenTransitionStr(trans_num, accepted_state, reject_state, default_reject_icon))

    " Change the "nombre" attribute in the transitions node
    execute "normal! vato\<ESC>"
    call ChangeNodeAttribute("nombre", trans_num)
endfunction

" INPUT() RETURNS A STRING WHEN THE FIRST PARAMETER SHOULD BE A NUMBER. BUT
" EVERYTHING IS STILL WORKING FINE FOR SOME REASON?? I'LL HAVE TO LOOK INTO
" IT SOMETIME.

" Wrapper for the above function.
function! WrapperWriteTransitions(num_states)
    execute "normal! diti\<CR>\<ESC>k"
    call WriteTransitions(a:num_states, eval(input("Enter Skip Logic Dictionary: ")))
endfunction

" Returns a string of a workflow state node
function! GenCandWkflStateStr(state, icon_num, onenter_attr)
    let label = "AFFECTATION_WORKFLOW_ETAT_" . a:state
    let cle = ' cle="' . label . '"'
    let libelle = ' libelle="TXT_' . label . '"'
    let icone = ' icone="icones/etat' . a:icon_num . '.png"'
    let onenter = ' onenter="' . a:onenter_attr . '"'
    let onleave = ' onleave=""'
    let libelle_onenter = ' libelle-onenter=""'
    let libelle_onleave = ' libelle-onleave=""'

    return '<etat' . a:state . cle . libelle . icone . onenter . onleave . libelle_onenter . libelle_onleave . '/>'
endfunction

" A function which configures the states in a candidate workflow.
function! WriteCandWkflStates(label_list, icon_dict, onenter_dict)
    " Write the states
    let num_states = len(a:label_list)
    let indent = repeat(' ', indent('.'))
    for i in range(1, num_states)
        call AppendStrHelper(indent . GenCandWkflStateStr(i, get(a:icon_dict, i), get(a:onenter_dict, i, "")))
    endfor

    " Change the attribute of the workflow node
    call GoToParentNode()
    call ChangeNodeAttribute("etats-actifs", "etat" . join(range(1,num_states), ",etat"))
    write

    " Update the labels
    split ../tools/translations/php.en.json
    for i in range(1, num_states)
        call ChangeLabel("TXT_AFFECTATION_WORKFLOW_ETAT_" . i, a:label_list[i-1])
    endfor
    write
    call RunTranslatorGenerate()
    wincmd p
endfunction

" Takes a string representing a color and returns the number of the picture
" that is used for that color. If no match is found, the parameter will
" be returned as is. 
function! MapColorToNumber(color_str)
    let mapping = {
    \ "green" : 1,
    \ "grey" : 2,
    \ "dark blue" : 3,
    \ "yellow" : 4,
    \ "light blue" : 5,
    \ "red" : 6,
    \ "black" : 7,
    \ "purple" : 8,
    \ "orange" : 9,
    \ }
    return get(mapping, a:color_str, a:color_str)
endfunction

" Gets input from the user, modifies it a bit so it is suitable for the
" WriteCandWkflStates() function, and then calls that function.
function! WrapperWriteCandWkflStates(num_states)
    " Get the labels from the user.
    let label_list = []
    for i in range(1, a:num_states)
        call add(label_list, input("Label " . i . ": "))
    endfor

    " Get the icons from the user.
    " The standard icones for states 1-9 are as follows (I think)
    " demande    - Grey       (2)
    " positionne - Yellow     (4)
    " 1          - Green      (1)
    " 2          - Light Blue (5)
    " 3          - Dark Blue  (3)
    " 4          - Purple     (8)
    " 5          - Red        (6)
    " 6          - Orange     (9)
    " 7          - Green      (1)
    " 8          - Light Blue (5)
    " 9          - Dark Blue  (3)
    let standard_icons = {
    \    1: 'green',
    \    2: 'light blue',
    \    3: 'dark blue',
    \    4: 'purple',
    \    5: 'red',
    \    6: 'orange',
    \    7: 'green',
    \    8: 'light blue',
    \    9: 'dark blue',
    \ }
    " Build up nice string to print to the prompt which states what the
    " standard icon colors are.
    let default_icon_str = ""
    for i in keys(standard_icons)
        let default_icon_str = default_icon_str . "\n" . i . '. ' . standard_icons[i]
    endfor
    " TODO: Better check these inputs. Maybe even make a function that will
    " return an empty list or dictionary if no input is given.
    let icon_dict = eval(input("The default icons are:" . default_icon_str . "\nEnter Icon Dictionary: " ))
    " Turn the colors of the standard icon list back to numbers
    for i in keys(standard_icons)
        let standard_icons[i] = MapColorToNumber(standard_icons[i])
    endfor
    " Overwrite any icons that are not standard.
    for i in keys(icon_dict) 
        let standard_icons[i] = MapColorToNumber(icon_dict[i])
    endfor

    " Get the onenter attributes from the user.
    let onenter_dict = eval(input("Enter onenter Dictionary: "))
    " The code below allows the user to enter shorter things like 'popup',
    " 'go1', and 'elink' when entering the onenter dictionary.
    let popupCount = 1
    for i in keys(onenter_dict)
        if onenter_dict[i] ==# 'popup'
            let onenter_dict[i] = "javascript:popupCourrier(popup" . popupCount . ")"
            let popupCount += 1
        elseif onenter_dict[i] ==# 'go1'
            let onenter_dict[i] = "javascript:popupCourrier(go1)"
        elseif onenter_dict[i] ==# 'elink'
            let onenter_dict[i] = "sendDossierCandidat(iIDAffectation, 1, 1, 1, 1, 1, 'go-nogo', '', 1, 0, 'descriptif/contact')"
        endif
    endfor

    call WriteCandWkflStates(label_list, standard_icons, onenter_dict)
endfunction

" Configures the entire candidate workflow in one go. So it configures the
" states as well as the transitions.
function! ConfigureCandWorkflow()
    let num_states = input("Number of States: ")
    " Note that if there are no etat's present then you'll have to go into the
    " workflow node yourself.
    let i = 1
    while GoToXMLNode('workflow/etat' . i)
        delete
        let i += 1
    endwhile
    call cursor(line('.') - 1, 1)
    call WrapperWriteCandWkflStates(num_states)
    " Our cursor should be left on the <workflow_> node. Go to the transitions
    " node and then configure it.
    call GoToXMLNode('workflow/transitions')
    call WrapperWriteTransitions(num_states)
endfunction

" }}}

" Requisition Workflow {{{

" Disables the requisition workflow.
function! DisableReqWorkflow()
    call GoToXMLNode('workflow_validation')
    call ChangeNodeAttribute("actif", "false")
    call ChangeNodeAttribute("niveau", "2")
    call GoToXMLNode('workflow_validation/popups')
    call ChangeNodeAttribute("actif", "false")
    call GoToXMLNode('workflow_etat/etats/validation')
    call ChangeNodeAttribute("actif", "false")
    write
endfunction

" Configures the requisition workflow but in a very primative way. It assumes
" that the req approvers start with contact2 and then progress from there. So
" no user approvers and no 'contact' approver. Even though it assumes a lot, I
" don't think I'll change this function to try and account for those special
" situations; I get the feeling that it would be simpler to just call this
" function to configure it part way and then, if it isn't quite right, make
" the rest of the changes manually.

" TODO: Instead of having a list of labels for the approvers, consider looking
" in the json file for the name of contact2 - contacx. Then just surround that
" found name with 'Pending' and 'Approval'. ALSO, maybe we could have a flag
" for this function to tell it to start from descriptif/contact instead of
" descriptif/contact2 like it does now; yeah I think I like that second idea.
" TODO: Consider making a wrapper for this function which will prompt for the
" list of labels and such.
function! ConfigureReqWorkflow(approver_name_list)
    call GoToXMLNode('workflow_validation')
    let num_approvers = len(a:approver_name_list)
    call ChangeNodeAttribute("actif", "true")
    call ChangeNodeAttribute("niveau", num_approvers + 2)

    " Update all those 'action-niv' nodes.
    for i in range(1, num_approvers)
        call GoToXMLNode('workflow_validation/action-niv' . i)
        call ChangeNodeAttribute("OnGo", "SendPoste(iIDPoste, 'descriptif/contact" . (i+1) . "', 0, 'postego-postenogo')")
        call ChangeNodeAttribute("OnNoGo", "javascript:popupWorkflowPoste(motif-refus)")
    endfor
    call GoToXMLNode('workflow_validation/action-niv' . (num_approvers+1))
    call ChangeNodeAttribute("OnNoGo", "javascript:popupWorkflowPoste(motif-refus)")
    call GoToXMLNode('workflow_validation/popups')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('workflow_etat/etats/validation')
    call ChangeNodeAttribute("actif", "true")

    " Write the labels: 'Pending XYZ Approval'
    split ../tools/translations/php.en.json
    call ChangeLabel("TXT_POSTE_VALIDE_DEMANDEUR", a:approver_name_list[0])
    for i in range(1, num_approvers - 1)
        call ChangeLabel("TXT_POSTE_VALIDE_VALIDEUR" . i, a:approver_name_list[i])
    endfor
    call ChangeLabel("TXT_POSTE_VALIDE_VALIDEUR" . num_approvers, "Validated")
    write
    call RunTranslatorGenerate()
    wincmd p
endfunction

" This function will be used to configure either the candidate or requisition
" workflow depending on which file is being editied. I put this function in
" this fold because I wanted it to be located inside a fold but I didn't want
" to create a whole new fold for just this function.
function! ConfigureWorkflow()
    if expand('%:t') ==# "candidat.xml"
        call ConfigureCandWorkflow()
    elseif expand('%:t') ==# "poste.xml"
        let approver_name_list = eval(input("Enter list of approvers: "))
        call ConfigureReqWorkflow(approver_name_list)
    endif
endfunction

" }}}

" Sertifi, ZeroChaos, PeopleClues oh my! {{{

" Assumes we are editing forms.xml. Leaves us in forms.xml too with the
" assumption that this function will be used by functions that configure
" sertifi and such.
function! ConfigureFormsWkflowTemplates()
    " So, when all is said and done, I want to return to this same window that
    " is holding the forms.xml buffer. I thought it would be enought to store
    " the window number and then use that number to return but it seems that
    " whenever you open new windows, the window number could change. So the
    " way around this (I think) is to store the number of the forms.xml buffer
    " and then return to the window holding that buffer.
    let buf_num = bufnr("%")

    " Activate on forms.xml
    call GoToXMLNode("forms/form")
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode("forms/form/name")
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode("forms/form/template-link")
    call ChangeNodeAttribute("actif", "true")
    write

    " Activate on menu.xml
    split menu.xml
    call GoToXMLNode("menu/referentiels/ref/forms_templates")
    call ChangeNodeAttribute("actif", "true")
    write

    " Activate on poste.xml
    split poste.xml
    call GoToXMLNode("actions/forms")
    call ChangeNodeAttribute("actif", "true")
    write

    " Return to the forms.xml window
    execute "normal! \<C-w>" . bufwinnr(buf_num) . "w"
endfunction

" TODO: Have these JUST configure their respective things i.e take out the
" call to the function that configures FormsWkflowTemplates
" Assumes we are editing forms.xml
" Configures Sertifi
" NOTE: This does NOT add the sertifi webservice to consts_param.php. That
" step must be done manually. 
function! ConfigureSertifi(account_email, api_code)
    let security_token = '605847382303948747'
    call ConfigureFormsWkflowTemplates()

    " Activate sertifi
    call GoToXMLNode('forms/sertifi')
    call ChangeNodeAttribute("actif", "true")
    " Insert the email and api code
    call GoToXMLNode('forms/sertifi/login')
    call ChangeNodeAttribute("value", a:account_email)
    call ChangeNodeAttribute("api_code", a:api_code)
    " Activate all nodes in <document>
    call GoToXMLNode('forms/sertifi/document/nom')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('forms/sertifi/document/type')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('forms/sertifi/document/mini-moteur')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('forms/sertifi/document/mini-moteur/type')
    " Activate lien
    call GoToXMLNode('forms/sertifi/lien')
    call ChangeNodeAttribute("actif", "true")
    " Fill in security token
    call GoToXMLNode('forms/sertifi/soap/security-token')
    call ChangeNodeAttribute("value", security_token)
    write

    " Activate fields on candidat.xml
    split candidat.xml
    call GoToXMLNode('icones/sertifi')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/sertifi')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/sertifi/poste')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/sertifi/document')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/sertifi/date-envoi')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/sertifi/date-signature')
    call ChangeNodeAttribute("actif", "true")
    write

    " We know that ConfigureFormsWkflowTemplates() will have an open split
    " containing the file menu.xml. So instead of opening up ANOTHER window
    " pointing to that same buffer, we'll just revisit that split.
    let menu_xml_win_num = bufwinnr(bufnr('menu.xml'))
    " Activate on menu.xml
    execute "normal! \<C-w>" . menu_xml_win_num . "w"
    call GoToXMLNode('menu/referentiels/ref/document_sertifi')
    call ChangeNodeAttribute("actif", "true")
    write
endfunction

" Wrapper for the above function.
function! ConfigureSertifiWrapper()
    let email = input("Enter Account Email: ")
    let api_code = input("Enter API Code: ")
    call ConfigureSertifi(email, api_code)
endfunction

" Configures ZeroChaos
function! ConfigureZeroChaos(email)
    call ConfigureFormsWkflowTemplates()
    call GoToXMLNode('forms/checkpast')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('forms/checkpast/account/recruiter_email')
    call ChangeNodeAttribute("value", a:email)
    write

    split candidat.xml
    call GoToXMLNode('icones/checkpast')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/checkpast')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/checkpast/poste')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/checkpast/component')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/checkpast/date-demande')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/checkpast/date-maj')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/checkpast/status')
    call ChangeNodeAttribute("actif", "true")
    write
endfunction

" Wrapper for the above function.
function! ConfigureZeroChaosWrapper()
    let email = input("Enter Account Email: ")
    call ConfigureZeroChaos(email)
endfunction

" Configure PeopleClues
function! ConfigurePeopleClues(id)
    call ConfigureFormsWkflowTemplates()
    call GoToXMLNode('forms/assessment')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('forms/assessment/peopleclues')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('forms/assessment/peopleclues/account/id')
    call ChangeNodeAttribute("value", a:id)
    for i in range(0, 10)
        call GoToXMLNode('forms/assessment/peopleclues/packages/package' . i)
        call ChangeNodeAttribute("actif", "true")
    endfor
    write

    let menu_xml_win_num = bufwinnr(bufnr('menu.xml'))
    " Activate on menu.xml
    execute "normal! \<C-w>" . menu_xml_win_num . "w"
    call GoToXMLNode('menu/referentiels/ref/assessment_templates')
    call ChangeNodeAttribute("actif", "true")
    write

    split candidat.xml
    call GoToXMLNode('icones/assessment')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/assessment')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/assessment/poste')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/assessment/document')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/assessment/score')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/assessment/date-envoi')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/assessment/status')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/assessment/date-status')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/assessment/lien')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/assessment/details')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/assessment/date-complete')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/assessment/infos')
    call ChangeNodeAttribute("actif", "true")
    call GoToXMLNode('suivi/assessment/actions')
    call ChangeNodeAttribute("actif", "true")
    write
endfunction

" Wrapper for the above function.
function! ConfigurePeopleCluesWrapper()
    let id = input("Enter Customer ID: ")
    call ConfigurePeopleClues(id)
endfunction

" }}}

" Miscellaneous {{{

" I found myself needing to copy the attributes 'libelle' and ('type-noeud' or
" 'type-liste') a lot, like when putting fields in the filter, so I thought
" I'd make a mapping to do it for me.
function! GetLibelleNoeudListe()
    let @y = GetNodeAttribute('libelle')
    let attr = GetNodeAttribute('type-noeud')
    if attr !=# ''
        let @z = attr
    else
        let @z = GetNodeAttribute('type-liste')
    endif
endfunction

" Made the complement for the above function, still needs some work. It seems
" to be putting a line break in the middle of a node.
function! SetLibelleNoeudListe()
    call ChangeNodeAttribute('libelle', @y)
    let attr = GetNodeAttribute('type-noeud')
    if attr !=# ''
        call ChangeNodeAttribute('type-noeud', @z)
    else
        call ChangeNodeAttribute('type-liste', @z)
    endif
endfunction

" }}}

" Mappings {{{

nnoremap <silent> <leader>ln :call GetLibelleNoeudListe()<CR>
nnoremap <silent> <leader>lN :call SetLibelleNoeudListe()<CR>

" Jumps to a node from its location in CDATA
nnoremap <leader>lf :call LuceoFindNode()<CR>
" Jumps to the node's location in CDATA from the node 
nnoremap <leader>lF :call LuceoFindCDATA()<CR>

" Makes a node mandatory from the CDATA section
nnoremap <leader>lm :call LuceoFindNode()<CR>:call ChangeNodeAttribute("obligatoire", "true")<CR>
" Makes a node non-mandatory from the CDATA section
nnoremap <leader>lM :call LuceoFindNode()<CR>:call ChangeNodeAttribute("obligatoire", "false")<CR>

" Orders the numbers on the 'field' nodes in the build/application form.
nnoremap <leader>lb vit:call OrderXMLNodes('<field')<CR>

" Orders the numbers on the 'transitions' nodes
nnoremap <leader>lo vit:call OrderXMLNodes('<transition')<CR>
" Writes all the candidate workflow transitions from scratch.
nnoremap <leader>lt :call WrapperWriteTransitions(input("Number of States: "))<CR>
" Thought it might be handy to have a mapping that just inserts one transition.
nnoremap <leader>lT :execute 'normal! o' . GenTransitionStr(1, input("From: "), input("To: "), input("Icon Number: "))<CR>
" Writes the candidate workflow states from scratch.
nnoremap <leader>lc :call WrapperWriteCandWkflStates(input("Number of States: "))<CR>
" Configures the candidate or requisition workflow depending on which file is
" being editied.
nnoremap <leader>lw :call ConfigureWorkflow()<CR>
" Disables the requisition workflow
nnoremap <leader>lW :call DisableReqWorkflow()<CR>

" Adds a field
nnoremap <leader>la :call AddFieldWrapper()<CR>
" Adds a repeatable field to the 'qualification' node.
nnoremap <leader>lq :call AddRepeatableFieldWrapper(0)<CR>
" Adds a repeatable field to the 'experiences' node.
nnoremap <leader>le :call AddRepeatableFieldWrapper(1)<CR>
" Adds a 'section' tag to the CDATA.
nnoremap <leader>ls :call AddSectionWrapper()<CR>

" Changes the label attached to a node
nnoremap <leader>ll :call ChangeNodeLabelWrapper()<CR>
" Echo's the label attached to a node
nnoremap <silent><leader>lL :call GetLabelNameWrapper()<CR>

" Removes a field
nnoremap <leader>lr :call RemoveFieldWrapper()<CR>

" Adds a field in the searchable area
"nnoremap <leader>ls :call LuceoAddSearchableField()<CR>

" Calls psf translator:generate
nnoremap <leader>lpg :call RunTranslatorGenerate()<CR>
" Calls psf translator:dedupe
nnoremap <leader>lpd :call RunTranslatorDedupe()<CR>
" Calls psf translator:merge
nnoremap <leader>lpm :call RunTranslatorMerge()<CR>

" }}}

" }}}

" Normal Mappings {{{

" Move by screen lines rather than actual lines.
noremap j gj
noremap k gk
" Maintain the original 'moving by actual lines' behavior which can still be
" useful. One use I had for it was creating a macro.
noremap gj j
noremap gk k
" I like being able to hit dj or yj to delete/yank two whole lines rather than
" part of both lines.
onoremap j j
onoremap k k
" Quickly scroll up and down the file. Sort of in between a 'j/k' and a
" '<C-d>/<C-u>'. The original J command is sometimes useful but I don't use it
" too often and I can easily just use :j[oin]<CR> or remap it. And I've almost
" never used 'K', in fact, I'll usually hit it by accident and scream
" profanities as I wait for the command to complete. Originally I had
" these mappings as 4gj and 4gk respectively but they didn't always work right
" on wrapped lines which I thought odd. This seems to work fine though.
noremap J gjgjgjgj
noremap K gkgkgkgk
" I've had to delete 3 lines before hence these mappings. The reason I don't
" have these mappings delete 5 lines (which would match how they move in
" normal mode) is because I don't trust myself to actually look at 5 lines and
" say 'hey there are 5 lines there to delete', but 3 lines I can definitely
" eyeball.
onoremap J 2j
onoremap K 2k

" Quickly write a file
nnoremap <leader>w :write<CR>

" When you look at it more, pasting in vim is a little odd. For a
" character-wise paste the cursor is placed at the end of the paste, which
" makes sense to me, but for a line-wise paste the cursor is left at the start
" of the paste. That inconsistancy is odd, so I'm going to fix it and see if I
" like it. Now the cursor will always be positioned at the end of the pasted
" text. Also, I don't find gp and gP's functionality useful so I've re-mapped
" them to leave the cursor at the beginning of the pasted text.
nnoremap p p:keepjumps normal! `]<CR>
nnoremap P P:keepjumps normal! `]<CR>
nnoremap gp p:keepjumps normal! `[<CR>:silent! call repeat#set("gp", v:count)<CR>
nnoremap gP P:keepjumps normal! `[<CR>:silent! call repeat#set("gP", v:count)<CR>

" Reselect the last changed/yanked text. I also made gv and gV text objects
" because it looks cool :).
noremap gV :<C-u>normal! `[v`]<CR>
vnoremap gV <NOP>
onoremap gv :<C-u>normal! `<v`><CR>

" This is easier to type than :j<CR>
nnoremap <leader>J J
vnoremap <leader>J J
" So apparently \r in a substitute command will insert an actual newline:
" http://stackoverflow.com/questions/71323/how-to-replace-a-character-for-a-newline-in-vim
" Okay, this is super cool. The atom \%# in a search/substitution will match
" the cursor position. So if I search for this '\%#\S*\zs\s*' then as I move
" the cursor, all the space AFTER the current word I'm on will be highlighted.
" Or, looking at there example, this highlights the current word that the
" cursor is on: '\k*\%#\k*'. Damn that's cool.

" Splits a line at the next occurrence of whitespace or at the cursor position
" if there is no whitespace. So this essentially does the opposite of the J
" command.
function! SplitLine()
    if search('\s', 'c', line('.'))
        normal! "_diw
    endif
    execute "normal! i\<CR>"
endfunction
nnoremap <leader>j :call SplitLine()
            \\| silent! call repeat#set("\<leader>j", v:count)<CR>

" An altered version of g; which adds to the jumplist
nnoremap g: m'g;

" Inspired by this page I made a mapping:
" http://vim.wikia.com/wiki/Repeat_last_command_and_put_cursor_at_start_of_change
nnoremap <leader>. .:keepjumps normal! `[<CR>

" Goes to the next and previous number on the current line
noremap <silent> <leader>d :call search('\v\d+\ze(\D\|$)', '', line('.'))<CR>
noremap <silent> <leader>D :call search('\v\d+\ze(\D\|$)', 'b', line('.'))<CR>

" Mapping to do a literal search (except for the / and ? characters of course)
nnoremap <leader>/ /\V
nnoremap <leader>? ?\V

" Trigger netrw
nnoremap - :Explore<CR>

" Sources the current file
nnoremap <leader>sc :source <C-R>% \| nohlsearch<CR>
" Sources .vimrc
nnoremap <leader>sv :source $MYVIMRC \| nohlsearch<CR>
" Edits the .vimrc file in a vertical split.
nnoremap <leader>eV :vsplit $MYVIMRC<CR>
" Edits the .vimrc file.
nnoremap <leader>ee :edit $MYVIMRC<CR>
" Opens previous buffer in a vertical split
nnoremap <leader>e# :leftabove vsplit #<CR>
" Quickly open a file in the same directory as the current file:
" http://vimcasts.org/episodes/the-edit-command/
nnoremap <leader>ew :e <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>es :sp <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>ev :vsp <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>et :tabe <C-R>=expand("%:p:h") . "/" <CR>

" H now goes to the first non blank character on the current line.
noremap H ^
" L now goes to last character on the current line.
noremap L $
vnoremap L g_
" We maintain the original H and L functionality.
noremap <leader>H H
noremap <leader>L L

" Makes it so the n and N commands always go in the same direction, forward
" and backward respectively, no matter which direction we're actually
" searching.
noremap  <silent> n /<C-r>/<CR>zv
noremap  <silent> N ?<C-r>/<CR>zv
" Goes to the next/previous search match without changing the jumplist.
noremap <silent><leader>n :execute "keepjumps normal! /".@/."\r"<CR>zv
noremap <silent><leader>N :execute "keepjumps normal! ?".@/."\r"<CR>zv
" Do I need to remap these to something that works? I don't think the jumplist
" has the same effect when you're already inside visual mode.
xnoremap <silent><leader>n <NOP>
xnoremap <silent><leader>N <NOP>

" An operator to resize windows. This is nifty because when I
" horizontally split a window, I'll oftentimes use one window for text
" editing and the other to reference a bit of code. Since the
" referenced code is usually a paragraph or other text object I can
" quickly resize that window to encompass what I need to see and
" maximize my screen real estate. TODO: Consider making this also
" adjust horizontal width. There are 3 possibilities I see with this:
" adjust vertical height, adjust horizontal width, or adjust both. I
" wonder how I could reconcile those options or if it's even worth it.
function! ResizeWindowOperator(type, ...)
    " After using an operator, the cursor is put at the
    " start of the operated text, so I think this is okay.
    let start_line = line('.')
    if a:0
        let end_line = line("'>")
    else
        let end_line = line("']")
    endif
    execute "resize ".(end_line-start_line+1)
    normal! zt
endfunction
nnoremap <silent> zS :set operatorfunc=ResizeWindowOperator<CR>g@
vnoremap <silent> zS :<C-u>call ResizeWindowOperator(visualmode(), 1)<CR>

" Also for fun, could I make replace operator so to speak? What it would do is
" replace the text-object with spaces and put me in Replace mode, at the
" beginning of where the text-object was. The inspiration for this idea was
" making this list:
"               1. voyagecare (DEV AND PROD)
"               2. saia       (DEV)
"               3. voyagecare (DEV)
"               4. voyagecare (DEV)
"               5. voyagecare (DEV)
"               6. voyagecare (DEV)
"               7. voyagecare (DEV)
" I was going to update the later 'voyagecare' strings to a different name but
" I'll want to keep the parentheses aligned. If I replaced a 'voyagecare' with
" whitespace and then just started Replace mode, the parentheses will stay
" aligned (assuming that the new name isn't longer than voyagecare). This
" probably wouldn't something terribly useful at all. If I wanted to keep the
" parentheses aligned, I could just use a plugin like Tabular after changing
" all the 'voyagecares'. But I thought this could be fun if anything else.
function! ReplaceOperator(type)
    let start_pos = getpos("'[")
    let end_pos = getpos("']")
    execute 'normal! '.start_pos[1].'G'.start_pos[2].'|v'.end_pos[1].'G'.end_pos[2].'|r '

    " echom a:type
    " How do I exit a function in a different mode?
    " normal! R
endfunction
" nnoremap <silent> gr :set operatorfunc=ReplaceOperator<CR>g@
" vnoremap <silent> gr :<C-u>call ReplaceOperator(visualmode())<CR>
" I'm unsure how to make an operator for the above request but I made this
" super simple solution for visual mode. I'll just stick with this for now
" because it does do what I want. In doing this, I wonder if it would be worth
" it to create an operator to replace a text-object with a specfic character.
vnoremap <silent> gr r<SPACE>R

" Create command to add a space before or after the cursor in insert mode.
"
" Could I make something that moves me 10 lines then 5 then 2... (as I
" continue the command). Feel like that might be a quick way to get around
" sometimes, you start off "coarse" but then get finer until you can just get
" there with a couple of line movements. Could you even do a binary search
" sort of thing??? Like you start in the middle and if no you can choose up or
" down and the process repeats. That would be kind of cool.
"
" Make commands to jump to a function definition given the name.

nnoremap <leader>co :copen<CR>
nnoremap <leader>cc :cclose<CR>
nnoremap <leader>cn :cnext<CR>
nnoremap <leader>cp :cprevious<CR>

" TODO: I'll have to look into that 'switch' plugin, which resides here:
" https://github.com/AndrewRadev/switch.vim. That plugin is exactly what I was
" trying to do with these 'flip' functions. I don't exactly know how it works
" but I bet if I leave the cursor in one place and run the command, it will
" keep switching the same item back and forth. If that is the case, one
" thought I had for a modification is to have it keep track of where the last
" switch was made and, if the second consecutive command after the first
" switch is another switch, then switch the next furthest thing we can find.

" Figure out how to make this work with the '.' command.
" "Flips" a string to a different one
function! FlipStr(str,flipped,flags)
    call search(a:str,a:flags)
    execute "normal! ce" . a:flipped
endfunction
nnoremap <leader>ft :call FlipStr("true", "false", "cW")<CR>
nnoremap <leader>ff :call FlipStr("false", "true", "cW")<CR>

" Mappings to switch between camelCase and snake_case. I got these substitutes
" from the bottom of this page:
" http://vim.wikia.com/wiki/Converting_variables_to_or_from_camel_case. I
" modified the first one slightly and the second was left unchanged. In
" particular I changed the flip camelCase one so that a string like this
" 'clientID' would change to this 'client_id'. So consecutive capitals get put
" in their own underscore section so to speak. I think it could use a little
" improvement. For example maybe I want it to turn 'XMLisFun' into
" 'xml_is_fun', but I'll do that another day.
" Flips case between snake and camel
function! FlipCase()
    let save_unnamed_register = @"
    " I want this to work but it doesn't seem to. I wonder why...
    " let cur_word = matchstr(getline('.'), '\w*\%#\w*')
    normal! yiw
    " It's snake_case
    if match(@", '_') !=# -1
        let @" = substitute(@", '_\(\l\)', '\u\1', 'g')
    " It's camelCase
    else
        let @" = substitute(@", '\v(<\u\l+|\l+)(\u+)', '\l\1_\L\2', 'g')
    endif
    normal! viwp
    let @" = save_unnamed_register
endfunction
nnoremap <silent><leader>fc :call FlipCase()<CR>

" Flips between 'public', 'private', 'protected' access modifiers.
function! FlipAccessModifier()
    let save_unnamed_register = @"
    normal! yiw
    if match(@", 'public') ==# 0
        let @" = 'private'
        normal! viwp
    elseif match(@", 'private') ==# 0
        let @" = 'protected'
        normal! viwp
    elseif match(@", 'protected') ==# 0
        let @" = 'public'
        normal! viwp
    endif
    let @" = save_unnamed_register
endfunction
nnoremap <silent><leader>fm :call FlipAccessModifier()<CR>

" Unfortunately vim can't understand some key combinations like <C-=>. As a
" workaround I'll just use <C-p> because it's close to '='.

" TODO: Consider making some different mappings altogether. Turns out that
" <C-w>w and <C-w>W cycle through the windows backwards and forwards. For most
" cases those could remove the need for some of the <C-hjkl> mappings I've
" made. There's also a <C-w>t and <C-w>b command to go to the top(lowest
" numbered)/bottom(hightest numbered) window. And there's <C-w>r and <C-w>R
" commands which switch a window with it's neighbor. Those might be better to
" map than the <C-w><C-h> mappings I have now.

" TODO: try to find another mapping to get the windows equal size.
nnoremap <C-_> <C-w>_
nnoremap <C-\> <C-w>\|
" I don't think I ever need <C-c> when in normal mode. I guess we'll find out.
nnoremap <C-c> <C-w>c
" Quickly move between windows
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
" This is a bit incosistent with the window movements but I wanted to the
" <C-h> and <C-w> for switching between tabs.
nnoremap gh <C-w>h
nnoremap gl <C-w>l

" Move and maximize a window
nnoremap <C-w><C-h> <C-w>h<C-w><C-\|>
nnoremap <C-w><C-j> <C-w>j<C-w><C-_>
nnoremap <C-w><C-k> <C-w>k<C-w><C-_>
nnoremap <C-w><C-l> <C-w>l<C-w><C-\|>
nnoremap <C-w><C-t> <C-w>t<C-w><C-_>
nnoremap <C-w><C-b> <C-w>b<C-w><C-_>
nnoremap <C-w><C-p> <C-w>p<C-w><C-_>
" Move the windows
nnoremap <C-w>h <C-w>H
nnoremap <C-w>j <C-w>J
nnoremap <C-w>k <C-w>K
nnoremap <C-w>l <C-w>L

" The gt and gT commands aren't very convenient for switching between tabs.
nnoremap <C-h> gT
nnoremap <C-l> gt

" <CR> already does + so lets make <BS> do the opposite
nnoremap <BS> -
" % has always been a little too inconvenient for my taste.
noremap <SPACE> %
" Keep the redrawing screen functionality that <C-l> gives us. I tried to map
" <C-m> but that seems like it might be connected somehow with <CR>?? I picked
" <C-g> because I don't use it much and I couldn't find another readily
" available ctrl mapping.
nnoremap <C-g> :nohlsearch<CR><C-l>

" Puts the top of the paragraph at the top of the screen leaving the cursor
" where it started.
nnoremap z{ {jzt``
" Puts the bottom of the paragraph at the bottom of the screen leaving the cursor
" where it started.
nnoremap z} }kzb``
" TODO: Make the same mappings for z[[ and stuff like that.

" Slightly easier to type and it wasn't being used!
nnoremap q; q:
xnoremap q; q:

" ' is easier to reach and it's nice for it to go to the exact spot.
noremap ' `
noremap ` '

nnoremap <silent><leader>gg :grep! -r '<C-r>/' *<CR>
nnoremap <silent><leader>gf :grep! -r 'function <C-r>/' *<CR>
nnoremap <silent><leader>gd :grep! -r 'define.*<C-r>/' *<CR>
nnoremap <silent><leader>gc :grep! -r 'class <C-r>/' *<CR>

nnoremap <leader>Gg :grep! -r '<C-r>/' *<LEFT>
nnoremap <leader>Gf :grep! -r 'function <C-r>/' *<LEFT>
nnoremap <leader>Gd :grep! -r 'define.*<C-r>/' *<LEFT>
nnoremap <leader>Gc :grep! -r 'class <C-r>/' *<LEFT>

" Normally Y is a synonym for yy. I think this mapping is more logical because
" D and C behave in this fashion.
nnoremap Y y$
" Yank the entire line characterwise
nnoremap yY 0y$

" Sometimes I just want to clear the line but keep the space it took up.
nnoremap dD :call setline('.', '')<CR>
            \:silent! call repeat#set("dD")<CR>

" Inserts a new line above/below the cursor but remains in normal mode. These
" changes are also made repeatable thanks to repeat.vim. TODO: These mappings
" don't repeat correctly with a count, look into this.
nnoremap <silent> <leader>o :call append('.', '')<CR>
            \:silent! call repeat#set("\<leader>o", v:count)<CR>
nnoremap <silent> <leader>O :call append(line('.')-1, '')<CR>
            \:silent! call repeat#set("\<leader>O", v:count)<CR>

" Run the program given by the makeprg option
nnoremap <silent><leader>m :w<CR>:make!<CR>

" A function which opens up a file using the output of the 'tree' command. So
" if I had this:
"
" web/param/src/
" ├── model
"     ├── correspondances
"         └── correspondanceactions.php
"
" And I wanted to open
" web/param/src/model/correspondances/correspondanceactions.php, I can put my
" cursor on that line, invoke the function, and the file will be opened. I
" made this super quick and it is probably full of problems but I'm happy with
" it. It assumes that the tree command's output starts on the first line of
" the file. In reality, it would probably be better to use netrw's 'tree' view
" of files but this was fun to make.
function! TreeGoToFile(open_tab_p)
    let save_unnamed_register = @"
    normal! $F─w
    let path = ''
    while line('.') !=# 1
        normal! y$b
        let path = @".'/'.path
        normal! yl
        while matchstr(@", '\w') ==# ''
            normal! kyl
        endwhile
    endwhile
    " We're at the top of the file, finish off the path to the file to
    " open.
    let path = getline('.') . path[:-2]
    if a:open_tab_p
        execute "tabedit " . path
    else
        execute "edit " . path
    endif
    let @" = save_unnamed_register
endfunction
nnoremap gt :call TreeGoToFile(0)<CR>
nnoremap gT :call TreeGoToFile(1)<CR>

" Operators to put the top/bottom of the screen on a text object.
function! RedrawCursorLineAtTop(type)
    " Whenever you use an operator, your cursor is positioned at the top of
    " the operated area. So this operator is extremely trivial.
    normal! zt
endfunction
nnoremap zT :set operatorfunc=RedrawCursorLineAtTop<CR>g@
function! RedrawCursorLineAtBottom(type, ...)
    if a:0
        let end_line = line("'>")
    else
        let end_line = line("']")
    endif
    call cursor(end_line, 1)
    normal! zb
endfunction
nnoremap <silent> zB :set operatorfunc=RedrawCursorLineAtBottom<CR>g@
vnoremap <silent> zB :<C-u>call RedrawCursorLineAtBottom(visualmode(), 1)<CR>

" }}}

" Insert Mappings {{{

" Another way to get out of insert mode. I cover all my bases by including
" mappings for every capitalization possibility.
inoremap jk <ESC>
inoremap Jk <ESC>
inoremap jK <ESC>
inoremap JK <ESC>

" These are nice because those keys are right under my fingers.
" inoremap <C-j> <C-r><C-p>"
" inoremap <C-l> <C-r><C-p>0
noremap! <C-j> <C-r><C-r>"
noremap! <C-l> <C-r><C-r>0

" TODO: Makes the current/previous word uppercase

" }}}

" Visual Mappings {{{

" Copied the code from visual star search plugin mentioned in 'Practical Vim'.
" Now hitting * or # when visually selecting text will search for the visually
" selected text.
function! VGetSearch(cmdtype, exact)
    let save_unnamed_register = @"
    normal! gvy
    let search_pat = substitute(escape(@", a:cmdtype . '\'), '\n', '\\n', 'g')
    if a:exact
        let search_pat = '\V\<' . search_pat . '\>'
    else
        let search_pat = '\V' . search_pat
    endif
    let @" = save_unnamed_register
    return search_pat
endfunction
xnoremap * :<C-u>execute 'normal! /' . VGetSearch('/', 1) . "\r"<CR>zv
xnoremap # :<C-u>execute 'normal! ?' . VGetSearch('?', 1) . "\r"<CR>zv
xnoremap g* :<C-u>execute 'normal! /' . VGetSearch('/', 0) . "\r"<CR>zv
xnoremap g# :<C-u>execute 'normal! ?' . VGetSearch('?', 0) . "\r"<CR>zv

" In this video at 6:49: https://www.youtube.com/watch?v=zIOOLZJb87U he uses a
" mapping where he visually selects some text and is prompted for a variable
" name. Once he entered the variable name 'tmp', 'tmp' got assigned the value
" of the visual selection and the variable name was pasted where the visual
" selection was. For example say I have this line of code:

"   $this->getDoctrine()->getEntityManager()->remove($row);

" If I highlight this portion: $this->getDoctrine()->getEntityManager(),
" call this mapping and type the variable name $em then the result would be:
"
"   $em = $this->getDoctrine()->getEntityManager();
"   $em->remove($row);
function! CreateVariableFromSelection()
    let save_unnamed_register = @"
    normal! gvy
    let var_name = input("Variable Name: ")
    " Insert new line above cursor position and put: 
    " 'variable_name = yanked_value'
    execute 'normal! O'.var_name." = \<C-r>".'";'
    " Put 'variable_name' in place of the visual selection
    let @" = var_name
    normal! gvp
    let @" = save_unnamed_register
endfunction
xnoremap <leader>v :<C-u>call CreateVariableFromSelection()<CR>

" A mapping/command that surrounds a selection with an ascii text box like
" this:
" Other text SELECTED_TEXT Other text
"            +---------------+
" Other text | SELECTED_TEXT | Other text
"            +---------------+
" Ideas:
" 1. The function currently opens a new line below and above the current line
" to put the bottom of the box. Consider trying to have it use the existing
" line.
" 2. Make it work for muliple selected lines.
function! SurroundWithBox(...) range
    " This is the best we can do to get default values
    let pipe_num_spaces = 1
    let top_bottom_num_spaces = 0
    if a:0 > 0
        let pipe_num_spaces = a:1
    endif
    if a:0 > 1
        let top_bottom_num_spaces = a:2
    endif
    let save_unnamed_register = @"
    normal! gvy
    let @" = substitute(@", "\n", "", "")
    " Create the top and bottom of the box
    let padding = repeat(' ', col("'<")-1)
    let topAndBottom = padding . '+'.repeat('-', len(@")+2*pipe_num_spaces).'+'
    call append('.', topAndBottom)
    call append(line('.')-1, topAndBottom)

    " Add more pipes if they want padding above or below the selected text
    let pipe_padding = repeat(' ', pipe_num_spaces)
    let pipe_string = padding.'|'.pipe_padding.repeat(' ', len(@")).pipe_padding.'|'
    for i in range(1, top_bottom_num_spaces)
        call append('.', pipe_string)
        call append(line('.')-1, pipe_string)
    endfor

    " Add the pipes on either side of the selected text
    let cur_line = getline('.')
    let first_part = strpart(cur_line, 0, col("'<")-1)
    let second_part = strpart(cur_line, col("'>"))
    call setline('.', first_part.'|'.pipe_padding.@".pipe_padding.'|'.second_part)
    let @" = save_unnamed_register
endfunction
command! -nargs=* -range Boxify call SurroundWithBox(<f-args>)

" }}}

" Operator-Pending Mappings {{{

" Goes to next email address. My regex is probably not perfect but that's
" fine.
onoremap i@ :<C-U>execute "normal! /\\S\\+@\\S\\+.com\r:nohlsearch\rvE"<CR>
onoremap a@ :<C-U>execute "normal! /\\S\\+@\\S\\+.com\r:nohlsearch\rvEl"<CR>
" Yanks the next chunck of code surrounded in '{ }'
" onoremap in{ :<C-U>execute "normal! /{\r:nohlsearch\rvi{"<CR>
" onoremap an{ :<C-U>execute "normal! /{\r:nohlsearch\rva{"<CR>

" A text object for the entire buffer
onoremap <silent> ae :<C-u>normal! ggVG<CR>
vnoremap <silent> ae :<C-u>normal! ggVG<CR>
" The InnerBuffer text-object will ignore any leading and trailing whitespace.
function! TextObjInnerBuffer()
    let save_pos = getpos('.')
    call cursor(1, 1)
    let start_line = search('\S', 'c')
    call cursor('$', 1)
    let end_line = search('\S', 'bc')
    if start_line && start_line <=# end_line
        execute 'normal! '.start_line.'GV'.end_line.'G'
    else
        call setpos('.', save_pos)
    endif
endfunction
onoremap <silent> ie :<C-u>call TextObjInnerBuffer()<CR>
vnoremap <silent> ie :<C-u>call TextObjInnerBuffer()<CR>

" Text-object for a search pattern. So I wanted to implement or install the i/
" text-object talked about in Practical Vim. This text object operates on the
" current search pattern. In trying to implement it I learned about the gn
" operator which behaves like i/ and is built into vim version 7.4.110!! So I
" decided to implement my own gn. I was trying to find a way to detect if a
" key sequence is already mapped and I could for user defined mappings but not
" for built in ones. So I check the version to see whether to define this
" operator or not.
function! TextObjSearchMatch(forward_p, visual_mode)
    if search(@/, 'ce' . (a:forward_p ? '' : 'b')) ==# 0
        return 0
    endif
    let end_pos = getpos('.')
    call search(@/, 'cb')
    execute 'normal! '(a:visual_mode ? 'g':'').'v'
    call cursor(end_pos[1], end_pos[2])
    let text_obj = 'gN'
    if a:forward_p
        let text_obj = 'gn'
    endif
    let cmd = v:operator.text_obj.(v:operator ==# 'c' ? "\<C-r>.\<ESC>" : '')
    silent! call repeat#set(cmd, v:count)
endfunction
" I think I could also use the <unique> tag which will only apply the mapping
" if it isn't already defined.
if v:version < 704
    noremap  <silent> gn :<C-u>call TextObjSearchMatch(1, 0)<CR>
    xnoremap <silent> gn :<C-u>call TextObjSearchMatch(1, 1)<CR>
    noremap  <silent> gN :<C-u>call TextObjSearchMatch(0, 0)<CR>
    xnoremap <silent> gN :<C-u>call TextObjSearchMatch(0, 1)<CR>
endif

" Text-object for an rvalue. At some point I could try to make it more robust
" but I'm satisfied for now.
function! TextObjRVal()
    normal! 0
    call search('=\s*\zs')
    let start_pos = getpos('.')
    " For vim files we just go to the end of the line, so this won't work on
    " multi-line assignments.
    if &filetype ==# 'vim'
        normal! $
    else
        " For other files we move to the last character before the ending
        " semicolon.
        call search('.\ze;')
    endif
    " Highlight from the end of the assignment to the start of it.
    execute 'normal! v'
    call cursor(start_pos[1], start_pos[2])
endfunction
" $temp = 'Change me!';
onoremap rv :<C-u>call TextObjRVal()<CR>
xnoremap rv :<C-u>call TextObjRVal()<CR>

" Text object for a heredoc. I don't really see myself using this much, but I
" thought it would be fun to make :).
function! TextObjHereDoc(around_p)
    let save_unnamed_register = @"
    let save_pos = getpos('.')
    let hd_region = GetStartEndHereDocPos(1, a:around_p)
    let old_hd_region = hd_region
    if empty(hd_region[0]) || empty(hd_region[0]) || !(hd_region[0][1] - (a:around_p ? 0:1) <=# save_pos[1] && save_pos[1] <=# hd_region[1][1] + (a:around_p ? 0:1))
        call setpos('.', save_pos)
        let hd_region = GetStartEndHereDocPos(0, a:around_p)
        if empty(hd_region[0])
            let hd_region = old_hd_region
        endif
    endif
    " Mark the visual selection
    call cursor(hd_region[0][1], hd_region[0][2])
    execute 'normal! '(a:around_p ? 'v':'V')
    call cursor(hd_region[1][1], hd_region[1][2])
endfunction
function! GetStartEndHereDocPos(backwards_p, around_p)
    let save_unnamed_register = @"
    if !search('<<<\?', 'cW' . (a:backwards_p ? 'b':''))
        return [[], []]
    endif
    normal! wyw
    if a:around_p
        normal! b
    else
        normal! j0
    endif
    let start_hd_pos = getpos('.')
    if !search('^' . @")
        let @" = save_unnamed_register
        return [[], []]
    endif
    if a:around_p
        normal! g_
    else
        normal! k
    endif
    let end_hd_pos = getpos('.')
    let @" = save_unnamed_register
    return [start_hd_pos, end_hd_pos]
endfunction
onoremap <silent> ihd :<C-u>call TextObjHereDoc(0)<CR>
xnoremap <silent> ihd :<C-u>call TextObjHereDoc(0)<CR>
onoremap <silent> ahd :<C-u>call TextObjHereDoc(1)<CR>
xnoremap <silent> ahd :<C-u>call TextObjHereDoc(1)<CR>

" Text object for a decimal number. I decided to make this because I was
" editing some css and had to change some numbers like: '100px' and the 'iw'
" motion would change too much. TODO: Consider making the visual mappings
" expand the visual region instead of highlighting just the number.
function! TextObjNumber(modifier, visual_mode)
    let regex = '\d\+'
    if a:modifier ==# 'l'
        call search(regex, 'be')
    else
        call search(regex, 'ce')
        if a:modifier ==# 'n'
            call search(regex, 'e')
        endif
    endif
    let num_boundary = getpos('.')
    call search(regex, 'bc')
    normal! v
    call cursor(num_boundary[1], num_boundary[2])
    let cmd = v:operator.'i'.a:modifier.'d'.(v:operator ==# 'c' ? "\<C-r>.\<ESC>" : '')
    silent! call repeat#set(cmd, v:count)
endfunction
"hi 123 are 000 cool 899
for i in ['', 'n', 'l']
    execute "onoremap <silent> i".i."d :<C-u> call TextObjNumber('".i."', 0)<CR>"
    execute "xnoremap <silent> i".i."d :<C-u> call TextObjNumber('".i."', 1)<CR>"
endfor

" }}}

" Command Mappings {{{
" These commands used to scroll the list up and down but did NOT filter the
" list as the Up and Down keys would. So we remap them because I like that
" behavior.
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

" What <expr> does is evaluate the expression (i.e a bit of vimscript,
" probably a function call) and inserts the result of that expression where
" you are typing the command. This is probably very powerful in insert and
" command line modes because you can have a function's return value be placed
" in the document. Like if I wanted to be able to insert the current line
" number in the document, I could make a mapping like this:
"   inoremap <expr> ln line('.')
" Actually... couldn't we just use the '=' register instead? I think that
" could work EVERY time that <expr> flag would. The above line could be
" written as:
"   inormap ln <C-r>=line('.')<CR>
" So maybe that <expr> is just a convenience thing?

" Makes it easier to open files in the same directory as other files.
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h') . '/' : '%%'

" I envision that this command will set a bunch of options or define a bunch
" of mappings to make it easier to create ascii art.
command! -nargs=0 Ascii set virtualedit=all

" Whenever I needed to figure out what some piece of code did, I'd ususally do
" some code diving, keeping all the pertinent files in active buffers. Then I
" would write what each file helped accomplish in a format like this:
"
"     File: fname1
"     File: fname2
"     File: fname3
"
" So this command writes that for me. For more flexibility, I've added the
" 'which_buffers' argument which lets me control exactly what buffers get
" written.
" 0 - All buffers
" 1 - All Active buffers
" 2 - Active buffers in the current tab
function! GetListOfActiveBuffers(which_buffers)
    let buffer_nums = []
    if a:which_buffers ==# 0
        for buffer in range(1, bufnr('$'))
            if buflisted(buffer)
                call add(buffer_nums, buffer)
            endif
        endfor
    elseif a:which_buffers ==# 1
        for tab in range(1, tabpagenr('$'))
            for buffer in tabpagebuflist(tab) 
                if buflisted(buffer)
                    call add(buffer_nums, buffer)
                endif
            endfor
        endfor
    else
        for buffer in tabpagebuflist(tabpagenr())
            if buflisted(buffer)
                call add(buffer_nums, buffer)
            endif
        endfor
    endif
    " Removes duplicate buffer nums
    return filter(buffer_nums, 'index(buffer_nums, v:val, v:key+1) ==# -1')
endfunction
function! WriteActiveBuffers(...)
    let which_buffers = 1
    if a:0
        let which_buffers = a:1
    endif
    let string = join(map(GetListOfActiveBuffers(which_buffers), '"File: ".bufname(v:val)'), "\n")
    execute "normal! o".string
endfunction
command! -nargs=* PutBuffers call WriteActiveBuffers(<f-args>)

" Command to remove any lines with trailing whitespace
command! -nargs=0 ClearTrailingWhitespace %substitute/\s*$//

" }}}

" Insert Abbreviations {{{
" *abbrev are similar to mappings. You type the text and if you type a
" character that is not in 'iskeyword' then the text will expand.
iabbrev teh the
iabbrev taht that
iabbrev waht what
"iabbrev @@ groenendaal92@gmail.com
iabbrev ret return
" }}}

" XML File Settings {{{
augroup filetype_xml
    autocmd! 
    autocmd Filetype xml setlocal iskeyword+=-
    autocmd Filetype xml setlocal breakat-=-
    autocmd Filetype xml noremap <silent> [[ :call GoToParentNode()<CR>
augroup END 
" }}}

" Haskell File Settings {{{
augroup filetype_haskell
    autocmd! 
augroup END 
" }}}

" Markdown File Settings {{{
augroup filetype_markdown
    autocmd!
    autocmd BufNewFile,BufRead *.md setlocal filetype=markdown
    autocmd Filetype markdown setlocal formatoptions=tcqn
    autocmd Filetype markdown setlocal tw=78
    autocmd Filetype markdown onoremap <buffer> ih :<C-U>execute "normal! ?^==\\+$\r:nohlsearch\rkvg_"<CR>
    autocmd Filetype markdown onoremap <buffer> ah :<C-U>execute "normal! ?^==\\+$\r:nohlsearch\rg_vk0"<CR>
    autocmd Filetype markdown onoremap <buffer> ih :<C-U>execute "normal! ?^==\\+$\r:nohlsearch\rkvg_"<CR>
    function! GetMarkdownHeaderRegex(header_num)
        let regex = '^'
        if a:header_num ==# 1
            let regex .= '=='
        elseif a:header_num ==# 2
            let regex .= '--'
        else
            let regex .= repeat('#', a:header_num).'[^#]'
        endif
        return regex
    endfunction
    " Returns a value depending on if the passed line number is on a header:
    " - 0 if not on header
    " - 1 if on 3-4 the header text
    " - 2 if on 1-2 header text
    " - 3 if on the header markers ('=' and '-') for 1 and 2 headers
    function! OnMarkdownHeaderText(line_num)
        let cur_line = getline(a:line_num)
        let next_line = getline(a:line_num+1)
        for i in range(1, 6)
            let regex = GetMarkdownHeaderRegex(i)
            if i ==# 1 || i ==# 2
                if match(cur_line, regex) !=# -1
                    return 3
                elseif match(next_line, regex) !=# -1
                    return 2
                endif
            elseif match(cur_line, regex) !=# -1
                return 1
            endif
        endfor
        return 0
    endfunction
    " Searches for the next markdown header of the specified number.
    function! SearchMarkdownHeader(header_number, flags)
        if OnMarkdownHeaderText(line('.')) ==# 2
            normal! j
        endif
        let ret = search(GetMarkdownHeaderRegex(a:header_number), a:flags)
        if a:header_number ==# 1 || a:header_number ==# 2
            normal! k
        endif
        return ret
    endfunction
    " Adds or Changes a header
    function! MarkdownChangeHeader(header_num)
        let which_header = OnMarkdownHeaderText(line('.'))
        " Remove existing header
        if which_header ==# 2 || which_header ==# 3
            if which_header ==# 3
                normal! k
            endif
            let header_line = line('.')
            normal! jdd
            if line('.') !=# header_line
                normal! k
            endif
        else
            call setline('.', substitute(getline('.'), '^#\+\s*', '', ''))
        endif

        " Create new header
        let cur_line = getline('.')
        if a:header_num ==# 1
            call append('.', repeat('=', strlen(cur_line)))
            normal! 0j
        elseif a:header_num ==# 2
            call append('.', repeat('-', strlen(cur_line)))
            normal! 0j
        else
            call setline('.', repeat('#', a:header_num).' '.cur_line)
        endif
    endfunction
    for i in range(1,6)
        execute 'autocmd Filetype markdown nnoremap <silent><buffer> <localleader>'.i.' :call MarkdownChangeHeader('.i.')<CR>:silent! call repeat#set("\<localleader>'.i.'", v:count)<CR>'
    endfor
    " TODO: Make an operator pending mode mapping called 'ih' (inner header)
    " which selects all the text inside the current header (ignoring any child
    " headers). Then formatting could be easier because I could just do
    " 'gqih'.
    " TODO: I should also make this actually work in visual mode.
    function! MoveToHeader(backwards)
        let save_view = winsaveview()
        if a:backwards
            let header_lines = [0, 0, 0, 0, 0, 0]
        else
            let max_line = line('$')+1
            let header_lines = [max_line, max_line, max_line, max_line, max_line, max_line]
        endif
        let start_line = line('.')

        let flags = 'W' . (a:backwards ? 'b' : '')
        for i in range(1, 6)
            if SearchMarkdownHeader(i, flags)
                let header_lines[i] = line('.')
            endif
            call cursor(save_view['lnum'], 1)
        endfor

        call winrestview(save_view)
        if a:backwards
            let closest_header = max(header_lines)
            if closest_header
                normal! m'
                call cursor(closest_header, 1)
            endif
        else
            let closest_header = min(header_lines)
            if closest_header <# max_line
                normal! m'
                call cursor(closest_header, 1)
            endif
        endif
    endfunction
    " TODO: Make these work with a count, so 3[[ will move us 3 headers back.
    autocmd Filetype markdown noremap <silent><buffer> [[ :<C-u>call MoveToHeader(1)<CR>
    autocmd Filetype markdown noremap <silent><buffer> ]] :<C-u>call MoveToHeader(0)<CR>
    autocmd Filetype markdown onoremap <buffer> ih :<C-U>execute "normal! ?^==\\+$\r:nohlsearch\rkvg_"<CR>

    " Moves to a specific header
    function! MoveToSpecificHeader(header_num, backwards)
        call SearchMarkdownHeader(a:header_num, 'Ws'.(a:backwards ? 'b' : ''))
    endfunction
    for i in range(1, 6)
        execute "autocmd Filetype markdown noremap <silent><buffer>[".i." :<C-u>call MoveToSpecificHeader(".i.", 1)<CR>"
        execute "autocmd Filetype markdown noremap <silent><buffer>]".i." :<C-u>call MoveToSpecificHeader(".i.", 0)<CR>"
    endfor

augroup END

" }}}

" Bash File Settings {{{
augroup filetype_sh
    autocmd!
    " Create a command to type out a variable so it adds the dollar sign and
    " quotes and other things for us. Maybe I could even have a separate
    " command to type an array so I don't have to bother with those [] chars.
augroup END
" }}}

" PHP File Settings {{{
augroup filetype_php
    autocmd!
    autocmd FileType php setlocal commentstring=//\ %s
    " Check out this for a possibly better way to do all this :make stuff.
    " Right now I just do this and then I have a mapping ,m which will write
    " the current file and then call 'php' on it.
    " http://vim.wikia.com/wiki/Runtime_syntax_check_for_php
    autocmd FileType php setlocal makeprg=php\ %
    autocmd FileType php setlocal errorformat=%m\ in\ %f\ on\ line\ %l
    autocmd FileType php iabbrev vd var_dump("TEST1");
    " Does a var_dump of whatever is in the unnamed register.
    autocmd FileType php iabbrev dv var_dump(<C-r>");
    autocmd FileType php setlocal matchpairs-=<:>
    " autocmd Filetype php setlocal iskeyword+=$
augroup END
" }}}

" HTML File Settings {{{
augroup filetype_html
    autocmd!
    autocmd FileType html nnoremap <buffer> <localleader>f Vatzf
augroup END
" }}}

" Common Lisp File Settings {{{
augroup filetype_lisp
    autocmd!
augroup END
" }}}

" C File Settings {{{
augroup filetype_c
    autocmd!
    "autocmd BufNewFile *.c :normal i#include <stdio.h><CR>Hi
    autocmd FileType c :iabbrev <buffer> iff if()<CR>{<CR>}
augroup END
" }}}

" Vimscript File Settings {{{
augroup filetype_vim
    autocmd!
    " Accessing the :help files with 'K' is useful when editing vimscript.
    " autocmd FileType vim setlocal keywordprg=:help
    " When foldmethod is "marker", vim will fold the lines starting at the one
    " containing {{{ and ending with the one containing }}}. See fold-marker
    autocmd FileType vim setlocal foldmethod=marker
    autocmd FileType vim nnoremap <buffer> <localleader>f O" {{{<ESC>}O" }}}<ESC>
    autocmd FileType vim iabbrev fu function!<CR><CR>endfunction
    autocmd FileType vim iabbrev nno nnoremap
    autocmd FileType vim iabbrev vno vnoremap
    autocmd FileType vim iabbrev ino inoremap
    autocmd FileType vim iabbrev iab iabbrev
    autocmd FileType vim iabbrev ono onoremap
    autocmd FileType vim iabbrev aut autocmd
    autocmd FileType vim iabbrev au! autocmd!
    autocmd FileType vim iabbrev aug augroup
    autocmd FileType vim iabbrev auE augroup END
    " How can I get this to actually print out "<leader>"?
    autocmd FileType vim iabbrev le <leader>
augroup END
" }}}

" TXT File Settings {{{
augroup filetype_txt
    autocmd!
    " I noticed that when I edit an xml file, tw=0. If I edit a vim file THEN
    " edit an xml file, tw is still 0. BUT if I edit a text file then open an
    " xml file tw=78. So something weird is going on there.
    setlocal textwidth=78
    " Still need to figure out how spell chck actually works but this will do for
    " now.
    "autocmd BufNewFile,BufRead *.txt setlocal spell
augroup END
" }}}

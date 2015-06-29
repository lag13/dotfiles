" Funny Vim Commands:
" bad[d] - Adds a file name to the buffer list without loading it.
" col[der] - Goes to an older quickfix list.
" lol[der] - Same as above but goes to an older location in the location list.
" nun[map] - Removes the mapping for a normal mode command. A quote from the
"            help pages: 'can also be used outside of a monastery'
" br[ewind] - Goes to the first buffer in the buffer list. You could call it
"             as :brew

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
set backspace=indent,eol,start
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
" Also necessary in my opinion. It highlights your search results.
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
" Memory is cheap, let's bump up the amount recorded commands.
set history=500
" When tab completing, complete the longest possible prefix and display a list
" of possible completions.
set wildmenu
set wildmode=list:full
" Makes it so commands that move the cursor up and down (like gg and G) try to
" retain the same column the cursor was originally in.
set nostartofline
" Long lines will wrap rather than run offscreen.
set wrap
" Wrapped text will break on a character in 'breakat' rather than at the last
" character that fits on screen.
set linebreak
" Character to display before each wrapped line.
set showbreak=...
" When joining a line that ends in a '.', '?' (and some others) only insert
" one space instead of two.
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
" fast terminal connection so enabling it shouldn't hurt and maybe help?
set ttyfast
" Toggle the 'paste' setting.
set pastetoggle=<F10>
" Only create swap files in my home directory. In my experience swap files
" have just been annoyances, but my pessimistic self still wants to keep them
" around just in case something happens.
set directory=~/.vim

" }}}

" Color Scheme and Terminal Specific Settings {{{

" I've used vim for a while and for the first time have started caring more
" about how it looks. I never thought the default colorscheme looked that bad
" (and still don't) so I never bothered to change it. But I have some friends
" who use sublime and man does that editor look nice. It got me thinking about
" altering vim's color scheme, just for a change of pace. This is what I now
" know of colorschemes in vim after trying to get the 'solarized' colorscheme
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
" same time. However, you can configure those 16 colors to be ANY colors you
" want. So in a sense, terminal emulators are capable of displaying any color,
" but only 16 at the same time. Many terminals (probably most) are also
" capable of displaying 256 colors simultaneously. This will, obviously, make
" most colorschemes look nicer because there are more available colors. But
" unlike the 16 base colors for the terminal, I don't believe you are capable
" of remapping any of these 256 colors. The only web page I could find related
" to this question was this page:
" http://stackoverflow.com/questions/25296985/can-i-change-the-256-color-mappings-in-iterm2-or-terminal.
" If the terminal supports 256 colors, I am currently unsure of how that
" affects the 16 ansii colors. Do the 16 ansii colors get ignored completely(I
" don't think so)? Are the first 16 colors out of 256 the ansii colors and the
" other 240 are always be the same (I could see this being true)?

" Now onto my setting up the 'solarized' colorscheme. This was a colorscheme
" that kept popping up in my searches and it looked great. So imagine my
" confusion when, after installing, I ran ':colorscheme solarized' and my
" terminal looked horrendous, it was very dissapointing. This is a nice answer
" I found online regarding this problem and its solution:
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
" I suppose that is the price you pay when you rely on another program for
" your display capabilities.
syntax enable

if has('gui_running')
    set guifont=Courier_New:h10:cANSI
    " Make distance between lines as small as possible.
    set linespace=0
    " No need for the menu or toolbar
    set guioptions-=m
    set guioptions-=T
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

" General Autocommands {{{

augroup general_autocommands
    autocmd!
    " Makes it so I'll be able to see what text I was editing previously even if
    " it was inside a fold.
    autocmd VimEnter * normal! zv

    " Typically filetype detection happens on the BufNewFile and BufRead
    " autocommand events, but for bash scripts this isn't good enough because
    " my bash files usually have no extension. Vim can detect the bash file
    " type just fine on it's own so I added this autocommand on the BufWrite
    " event to re-run :filetype detect if the filetype isn't already set.
    function! DetectScriptFileType()
        " TODO: When editing markdown files it would change the filetype to
        " modula2 upon saving so did_filetype() must not be doing what I
        " expect. This is my fix for now.
        " if !did_filetype()
        if &filetype ==# ''
            filetype detect
        endif
    endfunction
    autocmd BufWrite * call DetectScriptFileType()
    " Reload file automatically when a file's mode is changed.
    autocmd FileChangedShell * if v:fcs_reason ==# 'mode' | let v:fcs_choice = 'reload' | endif

    " Rather nifty I think
    autocmd InsertLeave * set nopaste
augroup END

" }}}

" Plugin Configuration {{{

" Plugins To Checkout:
" 1. Viewing man pages inside of vim
" 2. ctags - Tags
" 3. NERDtree or vinegar or vimfiler or filebeagle or dirvish - File explorer.
" http://www.reddit.com/r/vim/comments/3a7a6z/netrw_nerdtree/
" 4. clang complete for autocompleting C/C++ code
" 5. paredit http://danmidwood.com/content/2014/11/21/animated-paredit.html.
" 6. https://github.com/tpope/vim-eunuch, seems to have some useful stuff
" 8. https://github.com/tpope/vim-unimpaired - Many mappings starting with '['
" for moving around different lists.
" 9. http://vimawesome.com/plugin/youcompleteme - Code completion
" 10. http://vimawesome.com/plugin/ultisnips-forever-and-always - Snippets
" 11. http://vimawesome.com/plugin/syntastic - Syntax checking
" 13. https://github.com/kien/ctrlp.vim/issues/280 - Delete buffers with ctrlp 
" 15. https://github.com/sjl/gundo.vim - Undo tree. I think it requires python
" to run and the vim version must be 7.3, maybe I'll try making my own version
" in just vimscript.
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
" for it to work. I would like it to seek for the next thing that could be
" switched. It would be nice to be able to seek forward and backwards as well.
" And have the ability to switch specific things.
" 21. https://github.com/justinmk/vim-ipmotion - Configure { and } to behave a
" bit more intelligently. Seems like a nice little plugin.

" TODO: In making a PR for sneak.vim I learned about vader.vim, which is a
" testing framework for vim. vader.vim will output information about the test
" data (what failed, what passed). It seems that that output cannot be
" redirected to a file/command though which I feel is a shame especially if a
" lot of test data is being outputted. See if that can be fixed.

" Trigger netrw
nnoremap - :Explore<CR>
" No banner at the top of the netrw buffer
let g:netrw_banner = 0
" Tree directory view
let g:netrw_liststyle = 3

" TODO: It would be nice if targets.vim ignored escaped double quotes.
" TODO: the 'it' text-object doesn't work correctly on something like this:
" <form name="MainForm" id="MainForm" method="post" action="<?php echo $oPage->getUrlNoParams()?>">
"     <p>hello</p>
" </form>

" getchar() in expression mappings don't work below version 704 (technically
" 7.3.338)
if v:version < 704
    let g:targets_aiAI = 'ai  '
endif

" Start interactive EasyAlign in visual mode
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object
nmap ga <Plug>(EasyAlign)
" TODO: Could we configure easy align to format commented text if the ONLY
" text being operated on are comments?
let g:easy_align_delimiters = {
            \ '>': {
            \ 'pattern':      '=>\|->',
            \ 'left_margin':  0,
            \ 'right_margin': 0 },
            \ 'm': {
            \ 'pattern':      '-' },
            \ }

" I prefer that all my sneak mappings use 's' so I changed all the surround
" mappings to use 'z' instead.
let g:surround_no_mappings = 1
nmap dz  <Plug>Dsurround
nmap cz  <Plug>Csurround
nmap cZ  <Plug>CSurround
nmap yz  <Plug>Ysurround
nmap yZ  <Plug>YSurround
nmap yzz <Plug>Yssurround
nmap yZz <Plug>YSsurround
nmap yZZ <Plug>YSzurround
xmap Z   <Plug>VSurround
xmap gZ  <Plug>VgSurround
" Custom surround object 'c' for comments. TODO: It looks like this can only
" be used for the 'ys' operator. Is there any way to make it work with ds and
" cs as well?
let g:surround_99 = "/* \r */"

" TODO: This plugin still makes a '\' mapping and maybe more. Try to configure
" it in such a way that it doesn't create any unwanted mappings.
" TODO: Bug? If I use 't' of 'f' in operator-pending mode and there is no
" character to delete then it still deletes the next character.
let g:sneak#textobject_z = 0
" Make ; and <SPACE> always repeat the sneak in the same direction
nnoremap <silent> ;       :<c-u>call sneak#rpt('',           sneak#state().reverse)<CR>
nnoremap <silent> <SPACE> :<c-u>call sneak#rpt('',           1 - sneak#state().reverse)<CR>
xnoremap <silent> ;       :<c-u>call sneak#rpt(visualmode(), sneak#state().reverse)<CR>
xnoremap <silent> <SPACE> :<c-u>call sneak#rpt(visualmode(), 1 - sneak#state().reverse)<CR>
onoremap <silent> ;       :<c-u>call sneak#rpt(v:operator,   sneak#state().reverse)<CR>
onoremap <silent> <SPACE> :<c-u>call sneak#rpt(v:operator,   1 - sneak#state().reverse)<CR>
xmap S       <Plug>Sneak_S
omap s       <Plug>Sneak_s
omap S       <Plug>Sneak_S
map  f       <Plug>Sneak_f
map  F       <Plug>Sneak_F
map  t       <Plug>Sneak_t
map  T       <Plug>Sneak_T

" TODO: A bug with ctrlp? It says that <C-h> moves the cursor to the left but if
" actually deletes characters.
" TODO: Could using the find command index files faster?
" let g:ctrlp_user_command = 'find %s -type f'
" TODO: Consider activating the search on filename when looking through
" buffers rather than the full path, that would narrow down searches quicker.
" I'm already using <C-p> to switch between tabs/buffers

let g:ctrlp_root_markers = ['web', 'app']
let g:ctrlp_reuse_window = 'netrw\|help'
let g:ctrlp_follow_symlinks = 1
" I used this little bash script to inspect which directories had the most
" files and then set the g:ctrlp_custom_ignore variable accordingly.
" #!/bin/bash
" for i in *
" do
"     printf "%-10s" $i
"     find -L "$i" -type f | wc -l
" done
let g:ctrlp_custom_ignore = 'vendor\|lib\|img'
" To me it makes more sense to search for files based on cwd. <leader>p will
" now do this and <leader>P will try to search for files based on the root for
" the file being edited (just in case we want it).
let g:ctrlp_working_path_mode = 'rw'
let g:ctrlp_map = '<leader>p'
nnoremap <leader>P :CtrlPRoot<CR>
" Always try to work from the cwd
nnoremap <leader>b :CtrlPBuffer<CR>
" I liked the idea of setting g:ctrlp_mruf_relative = 1 because then different
" tab'bed workspaces could feel like they each contained their own files. But
" I also wanted to keep the default functionality of CtrlPMRUFiles as well so
" I made the second mapping. Is this overkill? Possibly.
nnoremap <leader>m :let g:ctrlp_mruf_relative = 1 <BAR> CtrlPMRUFiles<CR>
nnoremap <leader>M :let g:ctrlp_mruf_relative = 0 <BAR> CtrlPMRUFiles<CR>

" TODO: Bug/fix with indentwise? I had this xml in candidat.xml:
"
"             <!-- on cr�e plein de transitions bidon en standard pour �viter que la mise en page des xml clients ne soit cass�e -->
"             <transition24 de="" vers="" icone="" onaction="" libelle-onaction="" visible=""/>
"             <transition25 de="" vers="" icone="" onaction="" libelle-onaction="" visible=""/>
"             <transition26 de="" vers="" icone="" onaction="" libelle-onaction="" visible=""/>
"             <transition27 de="" vers="" icone="" onaction="" libelle-onaction="" visible=""/>
"             <transition28 de="" vers="" icone="" onaction="" libelle-onaction="" visible=""/>
"             <transition29 de="" vers="" icone="" onaction="" libelle-onaction="" visible=""/>
"             <transition30 de="" vers="" icone="" onaction="" libelle-onaction="" visible=""/>
"         </transitions>
" So an empty line followed by all that transition stuff. I had my cursor on
" the empty line and wanted to delete up to </transitions> by typing d]%. But
" that doesn't work. The ]% motion brings me to the second to last line in the
" file. This is probably not a bug, that line has 0 indent and the ]% behaves
" appropriately. But it would make more sense to me that I want ]% to behave
" based on the next non-empty line we find. I will note that I'm getting some
" odd behavior with [%. I would expect it to bring me to the second to last
" line from the top but it is not.

" }}}

" Create the Game of Life and have it trigger at a certain time. Perhaps I can
" trigger it when:
" 1. vim is started with no buffers
" 2. The CursorHold event (I like this one)
" 3. The FocusLost event

" TODO: Look into changing the prompt of the shell used after invoking :sh. I
" could say something like 'VIM SHELL' in the prompt just to emphasize the
" fact that this shell was started from vim.

" Look into running vim as a daemon.
" http://www.reddit.com/r/vim/comments/3ayhdx/a_quick_question_about_vim_server/

" Look into using vim to browse zip folders

" Learn how to better move around/organize split windows.

" Refine the text object which goes to the end of the current sentence. Also
" create a text object which goes to the end of the current paragraph. So like
" { and } but end on the line preceding the empty line that those commands
" would normally take you to.

" Maybe make commands ,u and ,r to go to the oldest and newest undo state of
" the file respectively.

" Always highlight the middle of the screen so I know where 'M' will jump to.

" I should probably not be using 'make' and 'makeprg' to execute files. That
" piece of functionality is, I think, meant to be used with the quickfix list.

" Customize netrw to delete the buffer associated with a file when deleting a
" file.

" TODO: I'm thinking about adding a list of buffers at the top of vim. The
" comments of this reddit post had a plugin which does JUST that, look into
" it. In preparation I've already made the <C-l> and <C-h> commands switch
" between buffers when there are no tabs open.
" http://www.reddit.com/r/vim/comments/382v6q/my_experience_switching_to_buffers/
" https://github.com/ap/vim-buftabline

" Have a visual command to select run the selected text as an Ex command

" If I use :cd in a [No Name] buffer it seems it only cd's for that window
" alone, so cd behaves like lcd. Look into this more. And actually right now
" I'm cd'ing in a different window and the window on my vimrc buffer is
" staying in the same cwd... What is going on there? I think that if I issue
" an :lcd at any time in a window, then that window is no longer taken into
" account when issuing :cd commands?

" Make the % command ignore commented sections or strings. Look at the
" easy-align plugin for how to do that.

" Have a visual mapping or something where we can highlight a number and it
" will tell you how many days, hours etc... make up the number (assuming the
" number is seconds)

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

" Normal Mappings {{{

" Inspired by cutlass.vim, now 'd' actually deletes while 'x' will cut.
nnoremap d "_d
nnoremap D "_D
xnoremap d "_d
nnoremap x d
nnoremap xx dd
nnoremap X D
xnoremap x d
" Similarly, c will not clobber the default register
nnoremap c "_c
xnoremap c "_c

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
" Quickly scroll up and down the file. Originally I had these mappings as 4gj
" and 4gk respectively but they didn't always work right on wrapped lines
" which I thought odd. This seems to work fine though.
noremap J gjgjgjgj
noremap K gkgkgkgk
" I've had to delete 3 lines before hence these mappings. The reason I don't
" have these mappings delete 4 lines (which would match how they move in
" normal mode) is because I don't trust myself to actually look at 5 lines and
" say 'hey there are 5 lines there to delete', but 3 lines I can definitely
" eyeball.
onoremap J 2j
onoremap K 2k

" Quickly write a file
nnoremap <leader>w :write<CR>

" = is hard to type so I'm remapping it to 'go', which doesn't seem
" immediately useful and has a nice mnemonic 'organize'. I also remapped 'go'
" to 'gO' on the offhand chance I want to use it.
nnoremap go =
xnoremap go =
nnoremap goo ==
nnoremap gO go

" When you look at it more, pasting in vim is a little odd. For a
" character-wise paste the cursor is placed at the end of the paste, which
" makes sense to me, but for a line-wise paste the cursor is left at the start
" of the paste. That inconsistancy is odd, so I'm going to fix it and see if I
" like it. Now the cursor will always be positioned at the end of the pasted
" text. Also, I don't find gp and gP's functionality useful so I've re-mapped
" them to leave the cursor at the beginning of the pasted text.
nnoremap <silent> p p:keepjumps normal! `]<CR>
nnoremap <silent> P P:keepjumps normal! `]<CR>
nnoremap <silent> gp p:keepjumps normal! `[<CR>:silent! call repeat#set("gp", v:count)<CR>
nnoremap <silent> gP P:keepjumps normal! `[<CR>:silent! call repeat#set("gP", v:count)<CR>

" Reselect the last changed/yanked text. I also made gv and gV text objects
" because it looks cool :).
noremap gV :<C-u>normal! `[v`]<CR>
vnoremap gV <NOP>
onoremap gv :<C-u>normal! `<v`><CR>

" This is easier to type than :j<CR>
nnoremap <leader>J J
vnoremap <leader>J J

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

" Sources the current file
nnoremap <leader>sc :source % \| nohlsearch<CR>
" Sources .vimrc
nnoremap <leader>sv :source $MYVIMRC \| nohlsearch<CR>

" Edits the .vimrc file in a vertical split.
nnoremap <leader>eV :vsplit $MYVIMRC<CR>
" Edits the .vimrc file.
nnoremap <leader>ee :edit $MYVIMRC<CR>
" Opens previous buffer in a vertical split
nnoremap <leader>e# :leftabove vsplit #<CR>
" Opens previous buffer in a horizontal split
nnoremap <leader>E# :split #<CR>
" Quickly open a file in the same directory as the current file:
" http://vimcasts.org/episodes/the-edit-command/
nnoremap <leader>ew :e <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>es :sp <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>ev :vsp <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>et :tabe <C-R>=expand("%:p:h") . "/" <CR>

" The two notable uses of tabs that I've seen have been for holding specific
" window layouts or working on a separate project by lcd'ing to a different
" area of the filesystem. Inspired by that second use of tabs I made this
" mapping.
nnoremap <leader>t :tabe <BAR> redraw!<CR>:lcd<SPACE>

" Undo's all changes made since opening the file.
nnoremap <silent><leader>u :let b:save_undo_nr = changenr() <BAR> silent! undo 1 <BAR> silent! undo<CR>
" Redo's all changes since the above command was used. I would have tried to
" redo ALL changes but I don't know of a good way to do that.
nnoremap <silent><leader>r :if exists('b:save_undo_nr') <BAR> execute "undo ".b:save_undo_nr <BAR> endif<CR>

" H now goes to the first non blank character on the current line.
noremap H ^
" L now goes to last character on the current line.
noremap L $
vnoremap L g_
" For consistancy
noremap gH g^
noremap gL g$
" We maintain the original H and L functionality.
noremap <leader>H H
noremap <leader>L L

" Makes it so the n and N commands always go in the same direction, forward
" and backward respectively, no matter which direction we're actually
" searching.
noremap  <silent> n /<C-r>/<CR>zv
noremap  <silent> N ?<C-r>/<CR>zv
" So the 'c' command works correctly
onoremap  <silent> n /<C-r>/<CR>
onoremap  <silent> N ?<C-r>/<CR>
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
" probably wouldn't be useful at all, just one of those things it would be
" interesting to implement. If I really wanted to keep the parentheses
" aligned, that job might be better handled by and aligning plugin like
" easy-align. Maybe the only way to do this is with autocommands?
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

" Could I make something that moves me 10 lines then 5 then 2... (as I
" continue the command). Feel like that might be a quick way to get around
" sometimes, you start off "coarse" but then get finer until you can just get
" there with a couple of line movements. Could you even do a binary search
" sort of thing??? Like you start in the middle and if no you can choose up or
" down and the process repeats. That would be kind of cool.

" TODO: Configure this to work with the '.' command.
" "Flips" a string to a different one
function! FlipStr(str,flipped,flags)
    call search(a:str,a:flags)
    execute "normal! ce" . a:flipped
endfunction
nnoremap <leader>ft :call FlipStr("true", "false", "cW")<CR>
nnoremap <leader>ff :call FlipStr("false", "true", "cW")<CR>

" Mappings to switch between camelCase and snake_case.
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

" I don't think I ever needed <C-c> when in normal mode. I guess we'll find
" out.
nnoremap <C-c> <C-w>c
" Move between windows
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
" Move and maximize a window
nnoremap <C-w><C-h> <C-w>h<C-w><BAR>
nnoremap <C-w><C-j> <C-w>j<C-w><C-_>
nnoremap <C-w><C-k> <C-w>k<C-w><C-_>
nnoremap <C-w><C-l> <C-w>l<C-w><BAR>
" Move the windows
nnoremap <C-w>h <C-w>H
nnoremap <C-w>j <C-w>J
nnoremap <C-w>k <C-w>K
nnoremap <C-w>l <C-w>L

" Switch between tabs if there are multiple and otherwise buffers
function! SwitchTabsOrBuffers(next)
    if tabpagenr('$') ==# 1
        if a:next
            bnext
        else
            bprevious
        endif
    else
        if a:next
            tabnext
        else
            tabprevious
        endif
    endif
endfunction
nnoremap <silent> <C-p> :call SwitchTabsOrBuffers(0)<CR>
nnoremap <silent> <C-n> :call SwitchTabsOrBuffers(1)<CR>

" <CR> already does + so lets make <BS> do the opposite
nnoremap <BS> -
" Keep the redrawing screen functionality that <C-l> gives us. I used <C-g>
" because I don't really see the use for it.
nnoremap <C-g> :nohlsearch<CR><C-l>

" Slightly easier to type and it wasn't being used!
nnoremap q; q:
xnoremap q; q:

" ' is easier to reach.
noremap ' `
noremap ` '

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
nnoremap <silent><leader>x :w<CR>:make!<CR>

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
" the file. In reality, it would probably be better to use some sort of file
" explorer but this was fun to make.
function! TreeGoToFile(open_tab_p)
    let save_unnamed_register = @"
    normal! $F─w
    let path = ''
    while line('.') !=# 1
        normal! yWb
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

" Text-object for a search pattern. I originally wanted to implement or
" install the i/ text-object talked about in Practical Vim, but in trying to
" implement it I learned about the gn operator which behaves like i/ and is
" built into vim version 7.4.110!! So I decided to implement my own gn. So I
" check the version to see whether to define this operator or not.
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

" Text object for a number. I decided to make this because I was editing some
" css and had to change some numbers like: '100px' and the 'iw' motion would
" change too much. TODO: Consider making the visual mappings expand the visual
" region instead of highlighting just the number.
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
for i in ['', 'n', 'l']
    execute "onoremap <silent> i".i."d :<C-u> call TextObjNumber('".i."', 0)<CR>"
    execute "xnoremap <silent> i".i."d :<C-u> call TextObjNumber('".i."', 1)<CR>"
endfor

" Text object for an xml attribute.
function! TextObjXmlAttr(around)
    " TODO: This does not work if there are single quotes within double quotes
    " or vice versa. Perhaps a quick fix would be to search for the = sign and
    " find the nearest quote.
    let end_of_attr_regex = '\v(''|")(\s|/|\>)'
    call search(end_of_attr_regex, 'c', line('.'))
    let save_unnamed_register = @@
    normal! yl
    let quote_type = @@
    if a:around
        normal! lyl
        if @@ ==# ' '
            let space_after_attr = 1
        else
            let space_after_attr = 0
            normal! h
        endif
    endif
    normal! v
    " Go to start of attribute value
    call search(quote_type, 'b')
    " Go to assignment operator
    call search('=', 'b')
    if a:around
        let start_of_attr_regex = '\s'.(space_after_attr ? '\zs':'').'\S'
    else
        let start_of_attr_regex = '\s\zs\S'
    endif
    " Go to beginning of attribute
    call search(start_of_attr_regex, 'b')
    let @@ = save_unnamed_register
endfunction
" <concours actif="true" obligatoire="false" liste="true"/>
onoremap <silent> ix :<C-u>call TextObjXmlAttr(0)<CR>
xnoremap <silent> ix :<C-u>call TextObjXmlAttr(0)<CR>
onoremap <silent> ax :<C-u>call TextObjXmlAttr(1)<CR>
xnoremap <silent> ax :<C-u>call TextObjXmlAttr(1)<CR>

" Goes to the end of the current sentence
function! EndOfCurrentSentence(dir, visual_p)
    if a:visual_p
        normal! gv
    endif
    execute "normal! ".a:dir."ge"
endfunction
noremap <leader>( :<C-u>call EndOfCurrentSentence('(', 0)<CR>
noremap <leader>) :<C-u>call EndOfCurrentSentence(')', 0)<CR>
xnoremap <leader>( :<C-u>call EndOfCurrentSentence('(', 1)<CR>
xnoremap <leader>) :<C-u>call EndOfCurrentSentence(')', 1)<CR>

" }}}

" Command Mappings {{{

" Filter the list rather than proceed sequentially through the command
" history.
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

" Makes it easier to open files in the same directory as other files.
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h') . '/' : '%%'

" I envision that this command might someday set a bunch of options or define
" a bunch of mappings to make it easier to create ascii art. For now it just
" does this one thing though.
command! Ascii set virtualedit=all

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

" Command to count the occurrences of the current search term
command! SearchCount %substitute///gn

" Command to make file executable and not.
command! Exeggcute :!chmod u+x %
command! Exeggutor :!chmod u-x %

" At some point I had installed a plugin called easydir
" (https://github.com/duggiefresh/vim-easydir) which created an autocommand on
" BufWritePre,FileWritePre that creates any directories that don't exist. I
" decided I don't like having that autocommand so I created a dedicated
" command which does the same thing.
function! CreateAndSaveDirectory()
  let s:directory = expand('%:p:h')
  if !isdirectory(s:directory)
    call mkdir(s:directory, 'p')
  endif
  write
endfunction
command! Write call CreateAndSaveDirectory()

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
    autocmd FileType sh setlocal makeprg=%:p
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

" Common Lisp File Settings {{{
augroup filetype_lisp
    autocmd!
augroup END
" }}}

" Vimscript File Settings {{{
augroup filetype_vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
augroup END
" }}}


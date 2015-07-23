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
" In case we start vim with the -u option
set nocompatible
" Setting the above options also resets 'fo' to the default of tcq which
" is annoying because everytime I source my vimrc this happens. So I try to
" remedy that here.
set formatoptions=croql
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
set statusline+=[%n]            " Buffer number
set statusline+=\ [%<%f]        " Current file
set statusline+=\ %m            " Modified flag
set statusline+=\ %y            " File type
set statusline+=\ %{&ff}        " File format
set statusline+=\ %l/%L         " Current line num out of total
set statusline+=\ %P            " Top/Bottom and percentage through file
set statusline+=\ [%{getcwd()}] " Current working directory
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
" Put the approprate code in my bashrc and now when I invoke :sh from within
" vim, that shell will have a modified prompt.
let $PS1_VIM = 'VIM SHELL ' . $PS1

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
    " No need for the menu, toolbar, or scrollbars
    set guioptions-=r
    set guioptions-=m
    set guioptions-=T
    set guioptions-=L
    " Use console dialogs instead of popups
    set guioptions+=c
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
    " Reload file automatically when a file's mode is changed. This was made
    " after I made the :Exeggcute command.
    autocmd FileChangedShell * if v:fcs_reason ==# 'mode' | let v:fcs_choice = 'reload' | endif
    " Mark the position where you last left the buffer. Thought it could come
    " in handy, I guess we'll find out.
    autocmd BufLeave * normal! ml
    " Rather nifty I think
    autocmd InsertLeave * set nopaste
augroup END

" }}}

" Plugin and Plugin Related Configuration {{{

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
" 22. https://github.com/haya14busa/incsearch.vim/releases/tag/v2.0.0. Don't
" know too much about it but seems pretty neat. Incrementally highlights ALL
" search matches while typing. It also seems pretty extensible, it looked like
" there was a fuzzy search extension, very cool.
" 23. http://stackoverflow.com/questions/13406751/vim-completion-based-on-buffer-name
" autocompletion based on buffer name.
" 24. Plugins that 'close' programming structures for you:
" https://github.com/rstacruz/vim-closer and
" https://github.com/tpope/vim-endwise.
" 25. Plugins that can shift function arguments:
" https://github.com/PeterRincker/vim-argumentative
" https://github.com/AndrewRadev/sideways.vim
" 26. Highlilght css color codes: https://github.com/ap/vim-css-color
" 27. Highlight 'interesting words'. Seems really nifty for reading through
" code because you can just run <leader>k to highlight words of interest.
" https://github.com/vasconcelloslf/vim-interestingwords

" TODO: In making a PR for sneak.vim I learned about vader.vim, which is a
" testing framework for vim. vader.vim will output information about the test
" data (what failed, what passed). It seems that that output cannot be
" redirected to a file/command though which I feel is a shame especially if a
" lot of test data is being outputted. See if that can be fixed.
" TODO: I think it would be nice if targets.vim only added to the jumplist if
" we are not already inside the text object. So if we are inside the text
" object, don't add to the jump list.
" TODO: Make 'o_v' mappings work for targets.vim
" TODO: It would be nice if the quote text objects would select only 'proper'
" quotes. So it will only operate on acutal pairs of quotes. Right now I'm
" imagaining a setting g:targets_proper option which will only work on quote
" paris. I feel like there was an issue placed about this already but I can't
" find it at the moment.
" TODO: Could we adjust the code in the visual mappings so that when we're in
" visual block mode they just exit out? That way, even if I have a version
" below 704 I can still use the I and A mappings AND be able to use I and A in
" visual block mode. I feel like this is definitely possible.
" TODO: the 'it' text-object doesn't work correctly on something like this:
" <form name="MainForm" id="MainForm" method="post" action="<?php echo $oPage->getUrlNoParams()?>">
"     <p>hello</p>
" </form>

" TODO: Look into how to configure endwise so that it could auto-close tags or
" if that is even possible.

" TODO: See if endwise could also be triggered on the 'o' mapping.

" Customizing closer.vim
augroup custom_closer
    autocmd!
    autocmd FileType php
                \ let b:closer = 1 |
                \ let b:closer_flags = '([{'
    autocmd FileType sh
                \ let b:closer = 1 |
                \ let b:closer_flags = '{'
augroup END

" TODO: NERD, I'm getting errors when I run the default 'gs' and 'gi'
" commands. I have no idea why this is? I'm not actively using the commands
" but I just want to know what's wrong. Could there be something in my vimrc
" which conflicts? Investigate this.

" Use plain characters to display the tree
let g:NERDTreeDirArrows = 0
" Launches and quits the NERDTree. I wrote this code to make nerdtree behave
" more like a 'split explorer' rather than a 'project drawer'. In doing this I
" also created my own nerdtree quit mapping because nerdtree's default wasn't
" able to retain the alternate file. TODO: Ran into this error when I launched
" nerdtree: E716: Key not present in Dictionary:
" /mnt/vault/www/esa-education.luceosolutions.com E15: Invalid expression:
" "keepalt buffer ".g:nerdtrees[getcwd()]["nerd_alt"] Error detected while
" processing function MyNerdTreeToggle. What happened was that I manually
" invoked NERDTree using :edit on a different directory than the cwd then when
" I hit '-' to quit it failed because no entry had been added to g:nerdtrees.
" I'm thinking that to make this more robust I'll need to create an
" autocommand. I also get an error when I launch it from an empty buffer. It
" seems that if you have an empty buffer. Looks like there is one more little
" problem I didn't notice. When opening a file from within nerdtree, the
" alternate file becomes messed up (i.e the alternate file becomes the nerd
" tree). This is, unfortunately, probably not possible to fix but I'll look
" into it anyhow. Maybe I could create my own <CR> mapping which will run
" nerdtree's <CR> mapping then manually restore the alternate file or
" something like that. Also check out this
" https://www.reddit.com/r/vim/comments/3d4cpf/prevent_netrw_or_nerdtree_from_opening_when/
" it uses that 'FileExplorer' thing again. I want to know what that is.
let g:nerdtrees = {}
function! MyNerdTreeToggle(toggle)
    " Launch NERDTree
    if a:toggle
        let cwd = getcwd()
        let cur_bnr = bufnr("%")
        if has_key(g:nerdtrees, cwd)
            let g:nerdtrees[cwd]["nerd_alt"] = cur_bnr
            execute "keepalt buffer ".g:nerdtrees[cwd]["nerd_buf"]
        else
            keepalt edit .
            let g:nerdtrees[cwd] = {"nerd_buf" : bufnr("%"), "nerd_alt" : cur_bnr}
        endif
    " Quit NERDTree
    else
        execute "keepalt buffer ".g:nerdtrees[getcwd()]["nerd_alt"]
    endif
endfunction
function! MyNerdTreeQuit()
    execute "keepalt buffer ".g:nerdtrees[getcwd()]["nerd_alt"]
endfunction
nnoremap - :call MyNerdTreeToggle(1)<CR>
augroup filetype_nerdtree
    autocmd!
    autocmd Filetype nerdtree nnoremap <buffer> - :call MyNerdTreeToggle(0)<CR>
augroup END
let g:NERDTreeMinimalUI = 1
" So the 'C' mapping doesn't hang
let g:NERDTreeMapCWD = 'cD'
" To have similar mappings between nerdtree and ctrlp
let g:NERDTreeMapOpenSplit = 'x'
let g:NERDTreeMapOpenVSplit = 'v'
" TODO: Do not using 'l', we can copy text from the nerd tree and having 'l'
" lets me get closer to the text I want to copy.
" Because 'x' is taken. 'l' stands for 'level' in my mind.
let g:NERDTreeMapCloseDir = 'l'
let g:NERDTreeMapCloseChildren = 'L'
" I don't see myself using NERDTrees 'J' and 'K' mappings and I'd before to
" scroll with my mappings.
let g:NERDTreeMapJumpFirstChild = ''
let g:NERDTreeMapJumpLastChild = ''

" getchar() in expression mappings don't work below version 704 (technically
" 7.3.338)
if v:version < 704
    let g:targets_aiAI = 'ai  '
endif

" Start interactive EasyAlign in visual mode
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object
nmap ga <Plug>(EasyAlign)
" Just in case we want the original functionality
nnoremap gA ga
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
nmap yZZ <Plug>YSsurround
xmap Z   <Plug>VSurround
xmap gZ  <Plug>VgSurround
" TODO: It looks like custom surround objects can 'ys' operator. Is there any
" way to make them work with ds and cs as well?
" 'c' to surround selection in comment characters.
let g:surround_99  = "/* \r */"
" 'x' to more easily make a search on eXact word boundaries
let g:surround_120 = "\\<\r\\>"

" TODO: If I hit <BS> when being prompted to sneak, I think it would be nice
" if it actually just did a <BS> then let me continue typing. As of now it
" stops the sneak.
" TODO: Bug? If I use 't' of 'f' in operator-pending mode and there is no
" character to delete then it still deletes the next character.
" TODO: Bug? I sneaked with f then did some other stuff. I repeated my sneak
" with ';' but the jump list didn't get set. i.e when I hit '' after ; I
" didn't end up where I invoked ;. Actually this was probably because 'f'
" doesn't add to the jump list (I think).
let g:sneak#textobject_z = 0
let g:sneak#absolute_dir = 1
" If I ever want to try out streak mode, I think these labels will not
" conflict with potential actions I want to take.
" let g:sneak#streak = 1
" let g:sneak#target_labels = "sfjkqtun/SFGJKQTUZNM?"
" Giving this a try. It seems nice because ; and : are on the same key just
" like n and N. <SPACE> is now my ':' key
map  :       <Plug>SneakPrevious
xmap S       <Plug>Sneak_S
omap s       <Plug>Sneak_s
omap S       <Plug>Sneak_S
map  f       <Plug>Sneak_f
map  F       <Plug>Sneak_F
map  t       <Plug>Sneak_t
map  T       <Plug>Sneak_T
" Sneak is using : as <Plug>SneakPrevious so I'm trying out using <SPACE> as
" it's replacement.
noremap <SPACE> :

" TODO: A bug with ctrlp? It says that <C-h> moves the cursor to the left but if
" actually deletes characters.
" TODO: Could using the find command index files faster?
" let g:ctrlp_user_command = 'find %s -type f'
" TODO: Consider activating the search on filename when looking through
" buffers rather than the full path, that would narrow down searches quicker.
" I'm already using <C-p> to switch between tabs/buffers
" TODO: Possible bug? I opened 125 files in a folder by executing: 'vim
" migration/*'. When I opened up other files and used my <leader>b mapping, it
" didn't sort my files in most recently used order. I wonder why that was.
" Did I just have too many buffers opened?

let g:ctrlp_root_markers = ['web', 'app']
let g:ctrlp_reuse_window = 'netrw\|help\|nerdtree'
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

" Comment the current line and paste the uncommented line below.
nnoremap <silent> gcp :copy . <BAR> execute "normal! k:Commentary\rj^"<CR>

" }}}

" Look more into swap file configuration. I think that even after setting the
" 'directory' option my swap files are still appearing in other places.

" I think I could make my zS and zB text objects not move the cursor to the
" type of the text object by registering a one time CursorMoved autocommand
" like repeat.vim does. Look into doing this.

" Create a 'file' text object making use of the 'isfname' setting. See this
" post:
" http://stackoverflow.com/questions/23224317/shortcut-to-select-a-file-path-text-object.
" Perhaps I will install kana's text object utility function.

" Add 'execute file' command support for C files:
" http://stackoverflow.com/questions/2627886/how-do-i-run-a-c-program-from-vim.
" I'm kind of picturing that everytime <leader>x gets called then it could
" check for a Makefile if one exists then use it, otherwise just gcc all the C
" fils in the current directory and run a.out. I'll have to look into it.
" Also, consider having the output of whatever program that gets executed get
" piped into a 'scratch' buffer. Alternatively I could run the 'clear' command
" before executing.

" Is there a way to close all windows in the current column or row? Like if I
" have 4 windows, 3 are in column 1 and 1 is in column 2 I want to remove two
" of the windows from column 1 but keep the one I'm focused on.

" Mapping to open the current url. Actually, perhaps I could even have a
" general 'open' operator which could detect the text being operated on and
" try to run that text through the appropriate program or even open a program
" that matches the text. So if it detects a url it could launch chrome, or if
" the text ended in .mp3 it could open it with vlc or the like. I'm pretty
" sure I remember seeing that netrw did offer this sort of functionality in
" later versions when using the 'x' command, I'd like to see how that works.

" Look into foldmethod=syntax. It sounds to me like it would automatically
" create folds based on things like braces and parens. Inspired by:
" https://www.reddit.com/r/vim/comments/3dmths/vim_unfolding_when_writing/

" Is there any way (there probably is) that you can have your search term be
" some text followed by nothing followed by more text? So picture the search
" highlighting getting broken up in the middle.

" Now that I have <SPACE> as my : I noticed an issue. On all of the "Press
" ENTER or type command to continue" prompts (like after using :ls), pressing
" <SPACE> doesn't let me start a command. Reading :help hit-enter, there
" doesn't seem like a way to configure this, but maybe there is? Look into it
" more.

" I had to do some grepping for John, looking for all the clients who have v1,
" v2, and v3 fos. According to Olivier we have a total of 1381 clients (my
" 'filter_clients_by' script returns 1399 but no matter). When I ran my script
" for v1s, v2s, and v3s I got these counts: v1 = 208, v2 = 129, v3 = 511.
" Adding 79 for the 4.0 sites (which my script doesn't acocunt for) gives a
" total client count of 927. So that means around 460 are not accounted for.
" Anyway, in trying to find out who those 460 clients are, I got a list of all
" clients and a list of all clients who have some FO. I did a vim diff on
" those two files and used this command to grab all the clients who were in
" the file of all clients but not in the FO file. Note that 21 is the highlight
" group ID for the DiffAdd hilight group. Just thought it could be useful to
" notate: g/^/if diff_hlID(line('.'), col('.')) == 21 | normal! "Qyy | endif

" I wonder if there is sort of an 'optimal' distance the cursor moves before
" it would be nice to add an entry to the jump list. I'm picturing that the
" start and end coordinates would go through a function and the function would
" spit out some number X. If X is bigger than some magical number Y then we
" want to add to the jump list. I could imagine this mystical function could
" be something like: 3.5*(row-difference) + col-difference. Then maybe this
" magical number Y could be 20 or something. I bet the interplay between X and
" Y could be a lot more complicated to. Like if the ending position fell at
" the end of a line or on some other text object boundary that was 'easy' to
" reach then maybe no entry would be added to the jump list.

" Forget if I wrote a todo for this or not. Make a command which will delete a
" 'statement' but leave the statement's content. So it could delete an if
" statement but leave it's contents. Similarly it would be nice to have a
" command which does the reverse (i.e take a range of lines and insert them
" into a statement).

" Sounds kind of ridiculous but what about a text object for colors? Like the
" word 'red' and 'blue' would be considered text objects. So would hexadecimal
" colors and any other sort you can imagine.

" Not a huge deal but what about making an improvement to the gf family of
" commands where gf will scan ahead until it is on a character that could be
" considered a valid file name. Or does it already do that?

" I'm thinking about using capital letter marks as a way to trace the call
" stack when I'm hunting down a bug or a way to improve a program. So I will
" make consecutive capital letter marks at all the 'key' locations in the code
" and execute a mapping which will loop through each mark and produce a series
" of paragraphs of this form:
"
"   file_name line_num
"   getline(line_num)
"   getline(line_num)
"
" I will edit the second getline(line_num) myself, replacing variable names
" with the actual values. I will also indent each of theses paragraphs
" appropriately to reflect how deep in the call stack they are. On a related
" note look into :help debug-mode it seems that vim might have some built in
" debugging capabilities.

" Is there a simple way in vim to delete from the cursor position to a closing
" brace or any sort of 'end of construct' syntax item while retaining the
" appropriate indent for the syntax item? For braces, a quick way to do it is
" d]} but it doesn't retain indent. Maybe also check out vim-pasta:
" https://github.com/sickill/vim-pasta

" TODO: Make a zZ operator to center cursor in the middle of a text object?

" Text object for inside the <?php ?> tags?

" Create the Game of Life and have it trigger at a certain time. Perhaps I can
" trigger it when:
" 1. vim is started with no buffers
" 2. The CursorHold event (I like this one)
" 3. The FocusLost event

" Look into running vim as a daemon.
" http://www.reddit.com/r/vim/comments/3ayhdx/a_quick_question_about_vim_server/

" Look into using vim to browse zip folders

" Learn how to better move around/organize split windows.

" Refine the text object which goes to the end of the current sentence. Also
" create a text object which goes to the end of the current paragraph. So like
" { and } but end on the line preceding the empty line that those commands
" would normally take you to.

" Always highlight the middle of the screen so I know where 'M' will jump to.
" Is it possible to highlight a specific line in the number column? Then I
" could just highlight that middle number.

" I should probably not be using 'make' and 'makeprg' to execute files. That
" piece of functionality is, I think, meant to be used with the quickfix list.

" TODO: I'm thinking about adding a list of buffers on vim's tabline. The
" comments of this reddit post had a plugin which does JUST that, look into
" it. In preparation I've already made the <C-p> and <C-n> commands switch
" between buffers when there are no tabs open. I'm picturing that if I have
" one tab then I'll display the buffer list, but if I have multiple tabs and
" each tab has a different cwd() then I'll display the cwd() of each tab. Or
" maybe if I have multiple tabs I'll disply the cwd() of each tab (regardless
" whether they have the same cwd() or not).
" http://www.reddit.com/r/vim/comments/382v6q/my_experience_switching_to_buffers/
" https://github.com/ap/vim-buftabline
" Change the headings on tabs to display the current directory for the active
" file. Some quick research, see :help setting-tabline. 'tabline' is the text
" that gets displayed across the ENTIRE tab page it uses the same format as
" 'statusline'. 'showtabline' lets us display the tabline at all times if
" desired. Use the function getcwd() to get the current directory, to put it
" in the statusline we'd have to do something like this :set
" tabline=%!getcwd()

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

" A text object to select a chunk of code stopping before any commented
" sections. So a code text object.

" TODO: A mapping to type out the previously made auto-completion. This would
" probably be an insert mapping.

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

" Inserts one space on either side of the character under the cursor.
" Overcomplicated? Possibly, but I got to play with regex's and have a
" function with a cool name so I say it's worth it. All this function does is
" surround the current character with spaces. TODO: Surround.vim can surround
" a text object with spaces but for some reason when I exexcute 'yzl ' it
" waits for me to enter another character. Until I figure that out, I'll keep
" this.
function! SplitKick()
    let pat = '^\(.\{'.(col('.')-1).'\}\)\(.\)'
    let sub = '\1 \2 '
    call setline('.', substitute(getline('.'), pat, sub, ''))
    normal! l
endfunction
nnoremap <leader><SPACE> :call SplitKick() <BAR> call repeat#set("\<leader> ", v:count)<CR>
" Deletes one character from either side of the character under the cursor.
nnoremap d<SPACE> i<BS><RIGHT><RIGHT><BS><ESC>:call repeat#set("d ", v:count)<CR>

" To get a good understanding for how a piece of code works I'll document the
" important function calls and what they do, essentially writing down the
" important parts of the callstack. In doing that I record file names and line
" numbers. To get the file name quicker I'll run this command :let @@ = @%,
" paste it into my document, and append the line number. I've done that a lot
" so I made this command to do it for me. Mnemonic - Get Buffer name.
nnoremap <silent> gb :let @@ = @% . ' ' . line('.')<CR>

" Inspired by cutlass.vim, now 'd' actually deletes while 'x' will cut. And
" 'c' doesn't cut text either.
nnoremap d "_d
nnoremap D "_D
xnoremap d "_d
nnoremap x d
nnoremap xx dd
nnoremap X D
xnoremap x d
nnoremap c "_c
nnoremap C "_C
xnoremap c "_c

" Tries to delete your typical function call
function! DeleteFuncCall()
    if search(')', 'c')
        keepjumps normal! %
    elseif !search('(', 'c')
        return
    endif
    " On opening parentheses
    let openp_pos = getpos('.')
    keepjumps normal! %
    normal! "_x
    call setpos('.', openp_pos)
    normal! vb"_d
endfunction

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
" Pastes code and auto indents it with =
function! PasteAndIndent(paste_cmd)
    execute "keepjumps normal! ".a:paste_cmd."'[v']="
endfunction
nnoremap gop :call PasteAndIndent('p')<CR>
nnoremap goP :call PasteAndIndent('P')<CR>

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

" Slightly 'smarter' g; command where if the cursor doesn't move after issuing
" a g; then it will issue it again.
function! SmartOlderChange()
    let save_pos = getpos('.')
    normal! g;
    if getpos('.') ==# save_pos
        normal! g;
    endif
endfunction
nnoremap <silent> g; :call SmartOlderChange()<CR>
" An altered version of g; which adds to the jumplist
nnoremap <silent> g: m':call SmartOlderChange()<CR>

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
nnoremap <leader>ex :sp <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>ev :vsp <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>et :tabe <C-R>=expand("%:p:h") . "/" <CR>

" The two notable uses of tabs that I've seen have been for holding specific
" window layouts or working on a separate project by lcd'ing to a different
" area of the filesystem. Inspired by that second use of tabs I made this
" mapping.
nnoremap <leader>t :tabe <BAR> redraw!<CR>:lcd<SPACE>

" Undo's all changes made since opening the file and saves the current undo state 
nnoremap <silent><leader>u :let b:save_undo_nr = changenr() <BAR> silent! undo 1 <BAR> silent! undo<CR>
" Redo's all changes since the above command was used. I would have tried to
" redo ALL changes but I don't know of a good way to do that.
nnoremap <silent><leader>r :if exists('b:save_undo_nr') <BAR> execute "undo ".b:save_undo_nr <BAR> endif<CR>

" TODO: This is all still a work in progress. In starting my two ideas (an
" undo to go to the previous save state and a way to save specific file undo
" states to return to later) I realized this might be a little tricker than I
" originally imagined. I'll work on it though. Have a command to go back to
" the previous save state. I'm picturing that if you save at point A then make
" changes and don't save, running this command will take you back to point A.
" It you save at point A, make changes and save at point B and run this
" command (so the buffer is not modified) it will take you back to point A.
" Maybe even make a command to save a save state. For example, if I was making
" a bunch of changes to a file (testing out different ways of implementing the
" same thing for instance) then each time I get something working I can
" remember a save state. In all reality it would probably just be an undo
" number. Then I could scan through this list and pick which save state I'd
" like to go to.

" " Stores the change number on every save.
" augroup save_states
"     autocmd!
"     autocmd BufEnter * if !exists('b:save_change_nums') | let b:save_change_nums = [] | endif
"     autocmd BufWrite * call add(b:save_change_nums, changenr()) | let b:save_change_idx = len(b:save_change_nums - 1)
" augroup END
" function! GoToPreviousSaveState()
"     " Need to add checks to make sure the list is non-empty or the index is
"     " valid.
"     if &modified
"         execute 'undo '.b:save_change_nums[b:save_change_idx]
"         let b:save_change_idx = b:save_change_idx - 1
"     else
"         execute 'undo '.b:save_change_nums[b:save_change_idx-1]
"         let b:save_change_idx = b:save_change_idx - 1
"     endif
" endfunction
" nnoremap U :call GoToPreviousSaveState()<CR>

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
" Redraw the screen and remove any search and/or sneak highlighting.
nnoremap <C-g> :nohlsearch <BAR> silent! call sneak#cancel()<CR><C-l>

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
" Because I can
nnoremap yp yyp
nnoremap yP yyP

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
    let w = winsaveview()
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
    call winrestview(w)
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

" Taken from Steve Losh's vimscript tutorial
function! GrepOperator(type)
    let saved_unnamed_register = @@
    if a:type ==# 'v'
        normal! `<v`>y
    elseif a:type ==# 'char'
        normal! `[v`]y
    else
        return
    endif
    if has("win32") || has("win64")
        let cmd = 'noautocmd vimgrep /\V'.escape(@@, '/\').'/gj **'
    else
        let cmd = 'silent grep! -R '.shellescape(@@).' .'
    endif
    execute cmd
    call histadd("cmd", cmd)
    copen
    redraw!
    let @@ = saved_unnamed_register
endfunction
nnoremap <leader>g :set operatorfunc=GrepOperator<CR>g@
vnoremap <leader>g :<C-U>call GrepOperator(visualmode())<CR>

" }}}

" Insert Mappings and Abbreviations {{{

" Adds another newline but keeps the cursor in it's current position. It's
" basically the <C-o> mapping from emacs. I'm not sure how it works exactly
" but it seems that using \<ESC> in a function which is used in an expression
" mapping doesn't break the undo sequence.
function! NewlineSameCursorPosition()
    return "\<CR>\<ESC>kA"
endfunction
" Originally I had a <S-CR> mapping for this but it doesn't work on terminals
" so I changed it.
inoremap <C-d> <C-r>=NewlineSameCursorPosition()<CR>

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

iabbrev lg Lucas Groenendaal
iabbrev lge Lucas.Groenendaal@careerbuilder.com
iabbrev lgE groenendaal92@gmail.com

" }}}

" Visual Mappings {{{

" Copied the code from visual star search plugin mentioned in 'Practical Vim'.
" Now hitting * or # when visually selecting text will search for the visually
" selected text.
function! VGetSearch(cmdtype)
    let save_unnamed_register = @"
    normal! gvy
    let search_pat = substitute(escape(@", a:cmdtype . '\'), '\n', '\\n', 'g')
    let search_pat = '\V' . search_pat
    let @" = save_unnamed_register
    return search_pat
endfunction
xnoremap * :<C-u>execute 'normal! /' . VGetSearch('/') . "\r"<CR>zv
xnoremap # :<C-u>execute 'normal! ?' . VGetSearch('?') . "\r"<CR>zv

" When configuring I often search for the definition of a constant. So I put
" my cursor of the constant, hit '*', then navigate to that file to search for
" the constant's definition. But it's annoying that when I return to the file
" I hit '*' my cursor typically changes position. This is meant to stop that.
" TODO: Make this command actually highlight the search as if we were
" searching.
function! StarSetSearch()
    let save_unnamed_register = @"
    normal! yiw
    let @/ = @"
    let @" = save_unnamed_register
endfunction
nnoremap <leader>* :call StarSetSearch()<CR>

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

" Quickly run a macro or the '.' command on a range of lines
xnoremap @ :normal @
xnoremap . :normal .<CR>

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
" thought it would be fun to make :). This seeks for the next here doc to
" find, searching forwards first. TODO: Make explicite next and last here doc
" objects. Still a bit of refactoring to do with the boolean logic and there
" could be a bug. I think I notice that if there is no heredoc to find, then
" my repeat command gets clobberred. Is there a way to fix this? Look at
" targets.vim for tips on how to go about making a text object when you could
" be inside a multi-line text object.
function! TextObjHereDoc(around_p)
    let w = winsaveview()
    let hd_region = GetStartEndHereDocPos(1, a:around_p)
    let next_hd_region = GetStartEndHereDocPos(0, a:around_p)
    if NoHereDocFound(hd_region) || CursorNotInsideHereDoc(hd_region, a:around_p)
        if NoHereDocFound(hd_region) && NoHereDocFound(next_hd_region)
            call winrestview(w)
            return
        elseif NoHereDocFound(hd_region)
            let hd_region = next_hd_region
        endif
    endif

    " Found a here doc, highlight it.
    call setpos('.', hd_region[0])
    execute 'normal! '(a:around_p ? 'v':'V')
    call setpos('.', hd_region[1])
endfunction
" Convenience function
function! CursorNotInsideHereDoc(hd_textobject, around_p)
    let pos = getpos('.')
    let hd_start_line = a:hd_textobject[0][1] - (a:around_p ? 0:1)
    let hd_end_line = a:hd_textobject[1][1] - (a:around_p ? 0:1)
    return !(hd_start_line <=# pos[1] && pos[1] <=# hd_end_line)
endfunction
" hd_textobject is the start and end position of the heredoc.
function! NoHereDocFound(hd_textobject)
    return empty(a:hd_textobject[0])
endfunction
" Returns a list of two tuples which are the start and end position of the
" heredoc.
function! GetStartEndHereDocPos(backwards_p, around_p)
    let save_unnamed_register = @"
    let save_pos = getpos('.')
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
    call setpos('.', save_pos)
    return [start_hd_pos, end_hd_pos]
endfunction
" TODO: Just ran into an error when trying to select this heredoc. Look into
" what's causing the error.
"  // This would be the correct query if $ACTIVE_STATE was defined. It's different per system and cannot be
"  // defined until we have access to the configuration API. Until then, this will always be reported as null.
" //      $query = <<<EOQ
" //    SELECT COUNT(*) AS numActiveJobPostings
" //    FROM `publication`
" //    AND publication.iActif = 1
" //    AND NOW() >= dtDebut
" //    AND NOW() <= dtFin
" //    AND `publication`.`iSuppr` <> 1
" //EOQ;
" //
" //        return $this->getIntQueryResult($query);

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

" A mapping to delete to the next path seprator.
function! DeleteBackOnePath(is_cmd_line)
    if a:is_cmd_line
        let cur_line = getcmdline()
        let cursor_pos = getcmdpos() - 1
    else
        let cur_line = getline('.')
        let cursor_pos = col('.') - 1
    endif
    if has("win32") || has("win64")
        let path_separator = '\'
    else
        let path_separator = '/'
    endif
    let dels = ""

    if cursor_pos > 1
        if cur_line[cursor_pos - 1] ==# path_separator
            let dels = dels."\<BS>"
            let cursor_pos = cursor_pos - 1
        endif
        while cursor_pos > 0 && cur_line[cursor_pos-1] !=# path_separator
            let dels = dels."\<BS>"
            let cursor_pos = cursor_pos - 1
        endwhile
    endif
    return dels
endfunction
cnoremap <expr> <C-b> DeleteBackOnePath(1)
inoremap <expr> <C-b> DeleteBackOnePath(0)

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
" BufWritePre,FileWritePre that creates any non-existant directories. That way
" you could write a file even if the directory in question didn't exist. I
" decided I don't like having that autocommand so I created a dedicated
" command which does the same thing. TODO: Make this command auto-complete on
" file paths.
function! CreateAndSaveDirectory(...)
    if a:0
        let l:directory = fnamemodify(a:1, ':p:h')
    else
        let l:directory = expand('%:p:h')
    endif
    if !isdirectory(l:directory)
        call mkdir(l:directory, 'p')
    endif
    if a:0
        execute "write ".a:1
    else
        write
    endif
endfunction
command! -nargs=? Write call CreateAndSaveDirectory(<f-args>)

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


" Miscelaneous Notes {{{

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
" of that expression is used as the key sequence to execute.

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

" Macro Insanity:
" On 01/16/16 I felt like making a colorscheme just to get a better sense
" about what that entails. Solarized was one that I liked so I decided to base
" mine off of that one. Solarized is fairly complicated, at least I think so,
" as far as colorschemes go and since I wanted to get a better sense of the
" "essence" of a colorscheme my first task was to make the solarized code look
" more like one of the default colorschemes in $VIMRUNTIME/colors (because
" those are pretty bare bones). So I started and was building up a LOT of
" lines that looked like this:

" highlight Normal cterm=NONE ctermfg=244 ctermbg=234

" Solarized builds those lines mostly using variables and since I got tired of
" repeating those numbers and formats I decided to extract them into variables
" as well. That lead me to put these lines at the top of my file:

" let s:fmt_none = "NONE"
" let s:fmt_bold = "NONE,bold"
" let s:fmt_revr = "NONE,reverse"
" let s:fmt_stnd = "NONE,standout"
" let s:vmode   = "cterm"
" let s:base03  = "234"
" let s:base02  = "235"
" let s:base01  = "239"
" let s:base00  = "240"
" let s:base0   = "244"
" let s:base1   = "245"
" let s:base2   = "187"
" let s:base3   = "230"
" let s:yellow  = "136"
" let s:orange  = "166"
" let s:red     = "124"
" let s:magenta = "125"
" let s:violet  = "61"
" let s:blue    = "33"
" let s:cyan    = "37"
" let s:green   = "64"

" Now of course I had about 30 lines of highlighting code that now needed to
" to be converted to use variables. I did not feel like doing that manually so
" I started recording macros. It took me a long time... and I most definitely
" could have been done sooner if I did it in a more manual fashion but this
" was fun. Here are the macros:

" Register a - The 'main' program
" Iexecute '10@b@c@d@e
" Register b - Replaces the values after equal signs with variables
"  execute "normal! f=lvEygg/\<C-r>0\r^wyiW`<vEp"
" Register c - Puts quotes around the values after equal signs
"  s/=\zs\(\S*\)/'.\1.'/g
" Register d - changes cterm to s:vmode and adds quotes
"  s/cterm/'.s:vmode.'/g
" Register e - Remove the trailing ".'" that appears because of register c
"  s/\.'$//

" Note that I've mapped space to be ':' which is why you see a space preceding
" the commands. Once I built these macros, all I had to do was hit @a on every
" line that needed to be updated. Woo!

" }}}

" Basic Settings {{{

let mapleader = ","
let maplocalleader = '\'
" In case we start vim with the -u option
set nocompatible
" Setting the above options also resets 'fo' to the default of tcq which
" is annoying because everytime I source my vimrc this happens. So I try to
" remedy that here.
set formatoptions=croql
if v:version + has("patch541") >= 704
    set formatoptions+=j
endif
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
" The status line will always appears in all windows
set laststatus=2
" Configure the status line
set statusline=
set statusline+=\ [%t]            " Current file
set statusline+=\ %m              " Modified flag
set statusline+=\ %l/%L           " Current line num out of total
set statusline+=\ %{fugitive#statusline()} " Current working directory
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
set directory=~/.vim//
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
elseif has('unix')
    let g:solarized_termcolors = 256
    set background=dark
    silent! colorscheme solarized
endif

" }}}

" General Autocommands {{{

augroup general_autocommands
    autocmd!
    " Vim returns to the same line when re-opening a file.
    autocmd BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$") |
            \     execute 'normal! g`"zvzz' |
            \ endif
    " Rather nifty I think
    autocmd InsertLeave * set nopaste
augroup END

" }}}

" Plugin and Plugin Related Configuration {{{

" Plugins To Checkout:
" 1. Viewing man pages inside of vim
" 2. ctags and cscope and gutentags:
" https://github.com/ludovicchabant/vim-gutentags - Tags
" 3. NERDtree or vinegar or vimfiler or filebeagle or dirvish - File explorer.
" http://www.reddit.com/r/vim/comments/3a7a6z/netrw_nerdtree/
" 4. clang complete for autocompleting C/C++ code
" 5. paredit http://danmidwood.com/content/2014/11/21/animated-paredit.html.
" 9. http://vimawesome.com/plugin/youcompleteme - Code completion
" 10. http://vimawesome.com/plugin/ultisnips-forever-and-always - Snippets
" 11. http://vimawesome.com/plugin/syntastic - Syntax checking
" 13. https://github.com/kien/ctrlp.vim/issues/280 - Delete buffers with ctrlp
" 17. https://github.com/nelstrom/vim-cutlass - Addressing the issues of vim's
" registers, unfortunately it is not written... but! I could definitely
" implement some of his ideas. There is also a plugin already out there which
" he mentions: https://github.com/svermeulen/vim-easyclip
" 18. https://github.com/Shougo/unite.vim - Possibley like ctrl-p but can be
" used for other things. Also see
" https://www.reddit.com/r/vim/comments/1fpti5/unitevim_the_plugin_you_didnt_know_you_need/
" 19. https://github.com/terryma/vim-multiple-cursors - Multiple cursors
" 20. https://github.com/AndrewRadev/switch.vim - Similar to my 'flip' series
" of commands. I was reading through the github page and one thing I was kind
" of bummed about was that the cursor has to be on the item being 'switched'
" for it to work. I would like it to seek for the next thing that could be
" switched. It would be nice to be able to seek forward and backwards as well.
" And have the ability to switch specific things.
" 22. https://github.com/haya14busa/incsearch.vim/releases/tag/v2.0.0. Don't
" know too much about it but seems pretty neat. Incrementally highlights ALL
" search matches while typing. It also seems pretty extensible, it looked like
" there was a fuzzy search extension, very cool.
" 23. http://stackoverflow.com/questions/13406751/vim-completion-based-on-buffer-name
" autocompletion based on buffer name.
" 25. Plugins that can shift function arguments:
" https://github.com/PeterRincker/vim-argumentative
" https://github.com/AndrewRadev/sideways.vim
" 26. Highlilght css color codes: https://github.com/ap/vim-css-color
" 27. Highlight 'interesting words'. Seems really nifty for reading through
" code because you can just run <leader>k to highlight words of interest.
" https://github.com/vasconcelloslf/vim-interestingwords
" 28. Yank ring: https://github.com/vim-scripts/YankRing.vim
" 29. Put the output of shell commands into a buffer:
" https://github.com/sjl/clam.vim
" 30. Grep operator: https://github.com/inside/vim-grep-operator
" 31. Check out the quick-scope plugin mentioned here and maybe the other
" plugins mentioned:
" https://www.reddit.com/r/vim/comments/3uwm6q/what_are_some_minimalistic_plugins_that_get_the/
" I think it would be really cool if I could combine the functionalities of
" the quici-scope plugin and the sneak plugin. That way I could still get
" multi-line fFtT motions while retaining quick-scope's nice highlighting of
" the current line.
" 32. Check out this weather vim plugin someone made :)
" https://www.reddit.com/r/vim/comments/3vs4mn/vimairline_weather_extension/
" It adds a little weather status to the airline status bar. More importantly
" though check out https://github.com/mattn/webapi-vim. Can I use that to send
" requests from within vim? Are there plugins to send requests from within
" vim? I know there's that restclient-mode for emacs:
" https://github.com/pashky/restclient.el. I wonder if there's something
" similar for vim
" 33. The plugins listed by NSNO in this post seems nice
" https://www.reddit.com/r/vim/comments/443fxr/what_am_i_missing_out_on/. In
" particulart I want to check out this plugin
" https://github.com/ajh17/VimCompletesMe. Which makes <tab> try to do the
" appropriate vim native completion depending on context.
" 34. vim-test for quickly running test suites. I first read about it here:
" https://shyp.github.io/2015/07/13/speed-up-your-javascript-tests.html

" TODO: In making a PR for sneak.vim I learned about vader.vim, which is a
" testing framework for vim. vader.vim will output information about the test
" data (what failed, what passed). It seems that that output cannot be
" redirected to a file/command though which I feel is a shame especially if a
" lot of test data is being outputted. See if that can be fixed.
" TODO: Make 'o_v' mappings work for targets.vim
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

" TODO: Add mappings for impaired like the yo and yO mappings but enter insert
" mode with 'paste' set at and after the cursor. Basically a yi and a ya
" mapping but, for obvious reasons, I cannot use those mappings. Maybe add [E
" and ]E mappings to exchange characters? Maybe add [F and ]F mappings to
" rotate between files but remain in the current directory? So they would just
" cycle between files in that directory.

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

nnoremap - :NERDTreeToggle<CR>
let g:NERDTreeMinimalUI = 1
let g:NERDTreeMapHelp = ''

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
            \ '[': {
            \ 'pattern':      ']',
            \ 'left_margin':  0,
            \ 'right_margin': 0 },
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

" TODO: Could we add numbers to the highlighting to represent how many times
" we have to hit ';' to get to that destination? This would in practice be a
" compromise between the default sneak functionality and streak mode. The
" benefit to me seems that if you immediately get to where you want to (which
" should happen more often than not) then you don't have to escape streak mode
" to take action but if you don't get where you want you know exactly how many
" ';' invocations to spam to get to your destination. Just a thought.
" TODO: If I hit <BS> when being prompted to sneak, I think it would be nice
" if it actually just did a <BS> then let me continue typing. As of now it
" stops the sneak.
" TODO: Bug? If I use 't' of 'f' in operator-pending mode and there is no
" character to delete then it still deletes the next character.
" TODO: Bug? I sneaked with f then did some other stuff. I repeated my sneak
" with ';' but the jump list didn't get set. i.e when I hit '' after ; I
" didn't end up where I invoked ;. Actually this was probably because 'f'
" doesn't add to the jump list (I think). Look into this.
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

" TODO: Experiment with g:ctrlp_user_command to try improving the speed of ctrlp
" let g:ctrlp_user_command = 'find -L %s -type f | grep -v "vendor\|lib\|img\|ps2\|cache\|js"'

" TODO: Possible bug? I opened 125 files in a folder by executing: 'vim
" migration/*'. When I opened up other files and used my <leader>b mapping, it
" didn't sort my files in most recently used order. I wonder why that was.
" Did I just have too many buffers opened?

let g:ctrlp_root_markers = ['web', 'app']
let g:ctrlp_reuse_window = 'netrw\|help\|nerdtree'
let g:ctrlp_follow_symlinks = 1
let g:ctrlp_custom_ignore = 'vendor\|img\|ps2'
let g:ctrlp_switch_buffer = 'e'
" To me it makes more sense to search for files based on cwd. <leader>p will
" now do this and <leader>P will try to search for files based on the root for
" the file being edited (just in case we want it).
let g:ctrlp_working_path_mode = 'rw'
let g:ctrlp_map = '<leader>p'
nnoremap <leader>P :CtrlPRoot<CR>
" Always try to work from the cwd
nnoremap <leader>b :CtrlPBuffer<CR>
" Originally made so when there are only 2 files in the same directory you can
" switch between them quickly.
" TODO: Order these files by most recently used? Can that be done?
nnoremap <leader>d :execute "CtrlP ".expand('%:h')<CR>
nnoremap <leader>D :execute "CtrlP ".expand('#:h')<CR>
" I liked the idea of setting g:ctrlp_mruf_relative = 1 because then different
" tab'bed workspaces could feel like they each contained their own files. But
" I also wanted to keep the default functionality of CtrlPMRUFiles as well so
" I made the second mapping.
nnoremap <leader>m :let g:ctrlp_mruf_relative = 1 <BAR> CtrlPMRUFiles<CR>
nnoremap <leader>M :let g:ctrlp_mruf_relative = 0 <BAR> CtrlPMRUFiles<CR>
" Go to the second most recently visited buffer
nmap <C-^> <leader>m<C-k><CR>

" Comment the current line and paste the uncommented line below.
nnoremap <silent> gcp :copy . <BAR> execute "normal! k:Commentary\rj^"<CR>

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

" The default indentwise mappings are a bit inconvenient so I remapped them
" allowing me to use two hands. Note that [s and ]s by default move between
" spelling mistakes.
map [r <Plug>(IndentWisePreviousLesserIndent)
map [s <Plug>(IndentWisePreviousEqualIndent)
map [g <Plug>(IndentWisePreviousGreaterIndent)
map ]r <Plug>(IndentWiseNextLesserIndent)
map ]s <Plug>(IndentWiseNextEqualIndent)
map ]g <Plug>(IndentWiseNextGreaterIndent)
" [b and ]b are mapped by unimpaired to move between buffers which is why I
" load it first before making these new mappings
runtime! plugin/unimpaired.vim
map [b <Plug>(IndentWiseBlockScopeBoundaryBegin)
map ]b <Plug>(IndentWiseBlockScopeBoundaryEnd)

nnoremap gs :Gstatus<CR>

" }}}

" It would be nice if we could have different quickfix "views" so to speak.
" Motivation is that when I'm looking for something in Luceo I do a grep which
" sometimes returns a massive amount of results (some of which comes from the
" vendor directory) it would be neat if I could choose to ignore that vendor
" directory or in general any sub directory. Or maybe I should learn how to
" use tags better...

" I remember seeing a plugin I wanted to checkout where you could highlight
" terms with different colors. So you could do something like highlight every
" word 'two' to be a color. Then the next time you called it on a different
" word (say 'abc') it would be highlighted in a different color. Then you
" could quickly scan for those words in the file. I was thinking about
" something similar. I think it could be neat to just highlight a region of
" text and give it a different color. My motivation was trying to figure out
" how to be able to write test code with feature flags. I was looking at an
" example of someone mocking an external service like that and there were a
" fair number of files involved. So I ended up with like 8 splits looking at
" the path the code took. I thought it could be useful if in each of those
" splits I could highlight the bit of code that actually mattered to me so I
" could better understand what I needed to do. Making the key parts easily
" visible if you will.

" I think I've already talked about wanting a good "flip" or "switch" plugin
" to change words to common alternatives or oppposites. So like flipping true
" to false and vice versa. I think this plugin should also deal with toggling
" punctuation at the end of a line. So it could add a comma, or semicolon or
" period.

" Noticed potential bug with either me or ctrlp. Ctrlp had set the ignorecase
" option and it affected my normal buffers.
" ignorecase
"       Last set from ~/.vim/bundle/ctrlp/autoload/ctrlp.vim

" Smarter text manipulations would be really nice. I guess something like
" pairedit. I just had a situation where I wanted to move a line of code below
" an if statement. That would be cool if I could move a line past a "block" of
" code.

" A plugin to quickly open dotfiles in my home directory (.vimrc, .bashrc,
" .bash_profile, .ssh/config, .inputrc, .tmuxconf) perhaps it can be as simple
" as find all the files starting with '.' and present them to the user.

" https://www.reddit.com/r/vim/comments/4je4oq/vimflow_declarative_devbuildtest_cycles_in_vim/
" This looks like what I might want/need for running one command which will
" run or compile or test my code out. We'll see though. Man, I really need to
" come up with a good workflow for compiling + running the code I create.

" I think I want this vertical movement plugin:
" https://www.reddit.com/r/vim/comments/4j4duz/vim_how_can_i_jump_to_next_line_containing_a/

" I really want more motions for moving around code, smarter motions. I think
" I'd really like a motion which looks for a line "similar" to this one and
" moves to it. Similar is pretty hard to define though. Or maybe bettwer would
" just be more movements among language constructs like:
" 1. Go to a function header
" 2. Go to the parameters of a function
" 3. Go to the return values of a function (useful for golang)
" 4. Go to the next "type" that appears in the text
" 5. Go to the next assignment
" An example of moving to a "similar" line: I had these two bits of notes and
" I was on the first line starting with "Customer" and I wanted to move to the
" other line starting with "Customer". Now wouldn't it be cool if I could just
" say "jump to similar line" and it would have taken me to the other one and
" maybe even retained the same column position?

" Date: 05/19/2016
" Customer: viafone.luceosolutions.com
" ID: 4086
" Platform: EU
" Error: Could not delete DNS

" Date: 05/19/2016
" Customer: iberiaexpress.luceosolutions.com
" ID: 3634
" Platform: EU
" Error: Could not delete DNS


" Can we make autocomplete work even after typing a space? So what I wanted to
" do was autocomplete on a variable name + type that appeared in the function
" header of another function. I typed the variable name then hit <C-p>. Now I
" would like to be able to hit <space> and it keeps trying to autocomplete.
" That would have gotten to my match faster than doing <C-x><C-p> and hitting
" <C-p> a couple times till I got to my match.

" Make one plugin which encompasses the functionality of buftabline and
" cwdtabline. In the plugin inlude the functionality to make the <C-p> and
" <C-n> mappings work correctly depending on whether there are tabs or not.
" Also include in that plugin a mapping which will make only the buffers that
" are below the cwd appear on the tabline. I was also thinking that maybe I
" could have some sort of ability to: start at buffer A, scroll through a
" couple buffers using <C-p>/<C-n> to get to the buffer I want C, once I get
" to this buffer, remember that I actually came from A and make this the
" alternate file. Ideally I'm thinking that the initial invocation of <C-p>/
" <C-n> will do a normal :bnext and then successive calls will do :keepjumps
" bnext. When you do ANYTHING besides <C-p>/C-n then the next one will just do
" another :bnext. But I don't know how to do something like "if you do
" ANYTHING different that <C-p> then make a change something".

" Vim email client. I'd like to see what code it would take to be able to send
" emails from within vim. Perhaps the actual code to send the email would be
" written in go and we just call it from vim? Who knows.

" Add mru buffer mode to buftabline? Then it could only show the buffers
" underneath the cwd?

" map <C-;> to go to first tab? <C-'> to go to last tab? just like with tmux.
" I wonder if those are free/they don't actually represent a different
" character because terminals are weird.

" Work on better integrating this plugin with the buftabline plugin. Consider
" making that update_tabline function global so if people have a mapping to
" change directory they can still make it work. Update github with
" screenshots. Add documentation. Add a couple options, one to always show the
" tabline, one to maybe show the full path of the active tab?

" Had an instance where only one line was indented with 2 spaces and sleuth
" decided to set shiftwidth to 2 when everywhere else it was 4. I wonder why.

" Can we make a better "paste from tmux" command? To paste from tmux is prefix
" C-] but I've had issues where I've accidentally hit that in normal mode
" which of course means that vim started freaking out.

" Can we make a "window undo" command? So if I accidentally close a window I
" could do something like <C-w>u and the window would be restored.

" Can tim pope's Remove command not close the window on the buffer that is
" being removed? That would be nice I think.

" Make bash command to source a file and export any variables defined.
" Normally to do this I have to set -a, . <file_to_source>, set +a

" Update the nerdtree window whenever I switch buffers. The use case is that
" when I do unimpaired's ]f mapping I want nerdtree to update to show which
" file I'm looking at.

" Wrappers around ctrlp where I set the working mode or whatever that variable
" is called before calling the command. The only use case I want this for is
" MRU file mode. So when I do ,m It will filter based off the cwd always.
" Another thing I could do with this is make my ,d mapping actually just run
" the MRU command with the appropriate root set. Or maybe do the mix of MRU
" and files? Not really sure but I think there's something there.

" Could we make a ctrlp extension to quickly bring back tabs that we've
" closed? My use case: I tend to like having one tab for each different
" project I'm working on even if I'm just referencing a bit of code. Depending
" on what I'm working on, this can cause the number of tabs to grow pretty
" quickly. At some point, the tabs are not needed anymore so I close them to
" reduce clutter. But maybe I'd like to quickly bring that tab back from the
" dead. So a ctrlp plugin to do this might be nice. So maybe I'd save the tab
" and the windows that were loaded in that tab.

" Make a command which goes back to the most recently changed file? It could
" be like a file wide change list I suppose? My use case was that I made a
" change in a file (added a var_dump) then navigated around a lot of other
" files for a while. I wanted to quickly return to that file but just couldn't
" remember what file it was... But this functionality could be nice to have
" just because you can quickly get back to the file you were editing. Like
" maybe you navigate away to copy some code or just peruse other files for
" ideas and when you're ready to code again boom! You instantly return to the
" file you were working on.

" Plugin ideas which are probably talked about in this document but I'll
" reiterate:
" 1. Improved change list. Will contain my implementation where g; jumps back
" one change further if the initial g; doesn't move the cursor. Keeps track of
" a change list spanning across files so that you could edit a file for a bit,
" look around at other code then quickly jump back to that file you were on.
" Maybe some change movements that go to the last change not visible in the
" current window
" 2. Save undo states. My thought here is that sometimes I have a working
" program want to refactor. So I refactor but maybe want to quickly jump back
" to see how I did it before. If I saved that undo state when I had my code
" working I could get there.
" 3. Ctrlp extension where you assemble a list of "files you're working on".
" You run a command to add the current file to this list and then ctrlp will
" just have to filter it.

" http://howivim.com/2016/damian-conway/ Some useful tidbits. I kind of liked
" his Y mapping actually. Lots of vim resources
" https://www.reddit.com/r/vim/comments/461y3f/collection_of_80_vim_resources/

" Can we have another <C-x><C-f> kind of completion which completes on file
" names relative to the current buffer/file? That would be nice.

" Make <CR> in nerdtree open the file and close the nerdtree split and 'o'
" open the file and keep the nerdtree split open. Or vice versa.

" In insert mode I can type <C-x> then <C-e> or <C-y> to scroll up and down.
" Could I make it so that after typing <C-x> I can switch between tabs with
" <C-n> and <C-p> or maybe even go to the alternate buffer with <C-6>?

" A course to look into the vim source code!
" https://www.reddit.com/r/vim/comments/44uqon/learning_vim_from_the_inside_source_code/

" Parinfer has been ported to vimscript!!!!
" https://www.reddit.com/r/vim/comments/44u67c/parinfer_has_been_ported_to_vimscript/.
" Check it out and perhaps install it. Heck, I think I'd be a little curious
" how parinfer works so maybe I could look at the original source code and
" make my own port. Another day though. Actually it looks like there's another
" implementation mentioned on the parinfer website:
" https://shaunlebron.github.io/parinfer/#editor-plugins

" Normally & repeats the last substitution. Normally an equivalent to that
" would be @: because chances are the last command you ran was the
" substitution. So why don't we make & do @:? The only scenario where we would
" lose functionality is when you: do a substitution, run a different command,
" then you want to do the substitution again but like I said I can't say I've
" ever run into that scenario as of yet. I suppose I could also map $ % or ^
" since I'm using those under different mappings, hmm yeah I think I like that
" better actually.

" Could we configure the sneak plugin so that when an operator pending motion
" is completed it doesn't change what the last motion used was? So if I did
" f:dt' I want ; to still take me to the next : instead of the '.

" When writing a file to a directory that doesn't exist doesn't it usually
" give me some sort of error? Right now I'm doing :w<cr> but it just sort of
" fails silently then I do my :Write command to make the necessary
" directories.

" Sometimes I'll do an undo-redo by accident. Is there a way to easily get
" back to where I just was? Or would I have to set a mark? Do mark's get
" undone? I feel like I remember reading somewhere that they did.

" Make a function to turn this sort of thing:
" $emailBody = $this->buildEmailBody($emails, $unableToSave, $postingLocations->getProblemReqs(), $categoryCodes->getProblemReqs());
" Into this:
"        $emailBody = $this->buildEmailBody(
"            $emails,
"            $unableToSave,
"            $postingLocations->getProblemReqs(),
"            $categoryCodes->getProblemReqs()
"        );


" I hope this thread takes off:
" https://www.reddit.com/r/vim/comments/41wgqf/do_you_regularly_use_manual_marks_if_yes_how_do/
" I too think like marks could be very useful but I just haven't made good use
" of them as of yet. Usually I'll edit something and after the fact I think to
" myself, 'man that could have been accomplished better with marks'. Maybe I
" should somehow show the active marks in the file? I wonder how I would do
" that... I could put a string in the status bar with a list of marks? Then
" maybe I could highlight characters on the screen where the mark is located?
" I could use a different color for different marks perhaps? And even if I
" don't know what mark corresponds to what color, I can at least make use of
" the [' and ]' commands to move between nearby marks.

" Might have found a bug with the exchange lines mappings ([e and ]e) in
" unimpaired. If I am inside an open fold and issue those commands then the
" lines do get switched but then the fold closes. Very odd.

" Woa! I was able to do `gf` on a line like this:
" $VIMRUNTIME/syntax/syntax.vim. I wonder if that applies to all of those
" variables beginning with a dollar sign?

" Check out the vim libraries this guy mentions:
" http://stackoverflow.com/a/26314537. vital.vim seems pretty damn cool!

" Make it so the 'join' command will not add a space to the join lines when
" the first one ends in a special character or if the one below starts with a
" special character.

" Make it so '.' in operator pending mode repeats the last text object used.

" Make a website. One thing I could do on that website is give a more in depth
" look at some popular vim plugins. How they work, what they accomplish, and
" all that sort of stuff. Seems like it would be a fun thing to do.

" Check out the plugins mentioned in this reddit post.
" https://www.reddit.com/r/vim/comments/3w90sv/taking_advantage_of_sessions_in_vim/
" Also checkout the ctrlspace plugin mentioned in the README of the
" 'promiscuous' plugin.

" Make a plugin to change the tabline to display a list of cwd's instead of
" file names. Also make it so other tabs with the cwd get shortened and put
" right next to another tab with the same cwd. So something like this:
" ~/Code/first_game * *
" So the ~/Code/first_game bit is the directory and the '*'s are two other
" tabs which are both in the same directory as that first one. Note that I'm
" assuming that all windows in a single tab will have the same cwd. And
" actually lets shorten that path to be '~/C/first_game' so it will display
" the first letter of each directory along the path and the full directory
" name when we get to the cwd. Each time a new tab is added, even though it
" might not be added next to a tab in the same directory, display it next to
" all the tabs with the same cwd. Make mappings which move between tabs
" reflect the display of the tab line rather than what they are internally. On
" a related note, maybe we could leverage this tabline to fake vim having
" separate projects. In my mind a project is a collection of tabs. Tabs are to
" windows as projects are to tabs. So we could have mappings to switch our
" current project but really what it would do would be to give us a different
" tabline. I guess each project would really be just a list of tab numbers.
" Then a plugin would only choose to display the tabs associated with those
" numbers.

" Make a ctrlp extension for 'special files'. I figure that whenever we do
" some programming task we might only be working on a couple files but we'll
" need to reference many other files in the mean time. Those couple files
" we're editing would be considered 'special'. We'd add them to some list
" whenever we wanted and when we run the plugin, we just point ctrlp at that
" list of files. Then we can quickly get back to the file(s) we were working
" on.

" Check out mosh, a replacement for ssh which is supposed to be a little
" nicer:
" http://stackoverflow.com/questions/10152402/vim-scrolling-slow-on-tmux-over-ssh

" The next set of plugins/functionality I would like is:
" 3. Try out the YankRing plugin so I don't have to use registers as much:
" https://github.com/vim-scripts/YankRing.vim. If I try the yankring plugin
" and like it, I suspect that it wouldn't work when using the replace with
" register plugin. It would be nice if those two plugins could have be
" integrated with eachother.

" Text object for all search terms on the screen? Then I could do things like
" gc{this_text_object} and it could comment all the highlighted search terms.
" Like I could search for "echom" and then run this and it will comment them
" out. A better/different alternative to this might be multiple cursors.

" See this for improving speed when working on large files:
" https://www.reddit.com/r/vim/comments/3gpfo7/thank_you_vim/

" Can I do a diff on a range of text? Like diff two visually selected areas?

" Make my source command only work for vim files. Maybe even think of using
" <localleader>x to source vim stuff just so it will be consistant with other
" languages.

" Try playing with the 'scrolljump' option sometime. I remember that when I
" sat next to that emacs coder on the train when his cursor would scroll
" offscreen it would jump a certain number of lines down. Seemed like it could
" be nice to have.

" Right now 'cursorline' seems to be the culprit in making vim slow when
" scrolling through code and so for the time being I've turned it off. But I
" was playing around with it and found that setting the 'lazyredraw' option
" ALSO seems to fix the issue. But I had turned option off not too long ago so
" operating on code with the 'sneak' motion and the repeat operator would work
" correctly. If this option was turned on, those plugins would look like they
" are waiting for input when in actuality the screen is just being lazy and
" not redrawing. I kind of like having the cursorline option set so maybe the
" only option would be turning lazyredraw back on? But try to see if there's
" anything else I can do to make ths scrolling faster. Speaking of fater
" cursor movement I wonder if disabling 'matchparen' which is a standard
" plugin which highlights a matching parentheses will speed up performance?
" OR! Consider turning off syntax highlighting altogether. That would
" definitely speed up performance and I'd get to keep my cursorline!
" http://www.linusakesson.net/programming/syntaxhighlighting/

" Looks like steve losh makes use of an auto-html closing function and created
" a mapping using it:
" https://bitbucket.org/sjl/dotfiles/src/d607caaf596b951d14f58d0a8342d2c2462372f6/vim/bundle/closetags-custom/ftplugin/html/closetags.vim?at=default

" Look at this for more ideas for configuring vim
" https://www.reddit.com/r/vim/comments/3ecu46/large_codebase_editing_in_vim/
" https://www.reddit.com/r/vim/comments/3egaqw/spacebar_backspace_and_enter_are_all_fairly/

" Create a 'file' text object making use of the 'isfname' setting. See this
" post:
" http://stackoverflow.com/questions/23224317/shortcut-to-select-a-file-path-text-object.
" Perhaps I will install kana's text object utility function.

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

" url text object. And related to that, maybe make an 'open' operator which
" would run the 'open' command. That operator + text object would allow me to
" open up a url from within vim. On a related note maybe make a nerdtree
" extension to open a file with it's default program.

" Look into foldmethod=syntax. It sounds to me like it would automatically
" create folds based on things like braces and parens. Inspired by:
" https://www.reddit.com/r/vim/comments/3dmths/vim_unfolding_when_writing/

" Now that I have <SPACE> as my : I noticed an issue. On all of the "Press
" ENTER or type command to continue" prompts (like after using :ls), pressing
" <SPACE> doesn't let me start a command. Reading :help hit-enter, there
" doesn't seem like a way to configure this, but maybe there is? Look into it
" more.

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

" Create the Game of Life and have it trigger at a certain time. Perhaps I can
" trigger it when:
" 1. vim is started with no buffers
" 2. The CursorHold event (I like this one)
" 3. The FocusLost event

" Look into running vim as a daemon.
" http://www.reddit.com/r/vim/comments/3ayhdx/a_quick_question_about_vim_server/

" Look into using vim to browse zip folders

" Create a text object which goes to the end of the current paragraph. So like
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

" I think I said this before but I don't remember seeing it in my vimrc so
" I'll say it again. Make an operator (or a series of commands) which will
" print out the value of a variable. This will be helpful for debugging
" purposes. Then I could just do something like: ,piw and in the case of php
" it will insert a new line below the current one with the contents:
" var_dump(yanked_string); See this as well:
" https://www.reddit.com/r/vim/comments/3i11ie/i_made_my_first_vim_plugin_consolationvim/

" Make a series of mappings to insert 'test' data. So I could have a 'phone
" number' mapping which could insert the phone number "123-456-7890" or have
" one that inserts a test email or something like that.

" Create a text object for an IP address

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

" TODO: Make it so that when you exit command mode it doesn't add the command
" to the history list. Also try to make it so that any duplicate commands in
" the history list are removed.

" TODO: Make some sort of notification ability. So like maybe I'll remind
" myself to delete something in a file. I'll add the message, associate it
" with a date and when that date comes to pass the notification will trigger.

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

" A documentation plugin for documenting the code constructs of different
" languages. I'm actually imagining something qutite simple. In the .vim
" directory we'll have a directory languages/. Inside that directory will be
" directories who's names are that of file types as vim identifies them (c,
" go, vim, etc...). In each of those language directories I'll create files
" for specific language constructs so I might have a slice.go or a map.vim or
" a map.c or a loop.go. Then when editing a specific langauge and I think to
" myself, "man how do I write a loop in go again?" I can just search in
" ~/.vim/languages/go/loop.go to see examples. I would probably create a ctrlp
" extension to quickly search through this directory. In general I could store
" entire chunks of code in their which I've found useful making it really easy
" to look it up again and copy it when needed. Some of these issues of "how do
" I write this construct in this language" could be solved by snippets and
" maybe I'll do that but this has some greater potential in that these
" documentation files will probably have comments explaining things and stuff
" like that.
let g:documentation_location = "~/personal_documentation"
function! GetDocsRoot()
    return expand(g:documentation_location)
endfunction
function! GetLangDocsDir()
    return GetDocsRoot()."/".&filetype
endfunction
function! SearchLangDocumentation()
    let langDocs = GetLangDocsDir()
    if !isdirectory(langDocs)
        call mkdir(langDocs, "p")
    endif
    execute "CtrlP ".langDocs
endfunction
nnoremap <leader>L :call SearchLangDocumentation()<CR>


" TODO: Consider making a PR to get these mappings into unimpaired.vim
function! MoveChar(direction)
    if a:direction == 'h' && col('.') == 'l'
        return
    elseif a:direction == 'l' && col('.') == col('$') - 1
        return
    endif
    let saved_unnamed_register = @@
    let keys = "d" . a:direction . (a:direction ==# 'h' ? 'ph' : 'p')
    execute "normal! " . keys
    let @@ = saved_unnamed_register
    let mapping = (a:direction ==# 'h' ? '[' : ']') . 'E'
    echom mapping
    silent! call repeat#set(mapping)
endfunction
nnoremap <silent> [E :call MoveChar('h')<CR>
nnoremap <silent> ]E :call MoveChar('l')<CR>

" Naive jumping to the next comment
function! JumpToComment(direction, visual)
    if a:visual
        normal! gv
    endif
    if a:direction == -1
        let flag = 'b'
        let stopLine = 1
    elseif a:direction == 1
        let flag = ''
        let stopLine = -1
    else
        return
    endif
    let commentStart = matchstr(&commentstring, '\S\ze%s')
    let cur_line = line('.')
    call search(commentStart, 'W' . flag)
    while line('.') == cur_line + a:direction && line('.') != stopLine
        " TODO: Check if we are in a comment before proceding
        " if !synIDattr(synID(line('.'), col('.'), 0), 'name') =~# 'Comment'
        let cur_line = line('.')
        call search(commentStart, 'W' . flag)
    endwhile
endfunction
nnoremap <silent> [C :call JumpToComment(-1, 0)<CR>
nnoremap <silent> ]C :call JumpToComment(1, 0)<CR>
xnoremap <silent> [C :call JumpToComment(-1, 1)<CR>
xnoremap <silent> ]C :call JumpToComment(1, 1)<CR>

function! ToggleCharacter(char)
    let line = getline('.')
    if line[-1:-1] == a:char
        let line = line[0:-2]
    else
        let line = line . a:char
    endif
    call setline('.', line)
endfunction
" Toggle commas and semicolons
nnoremap <silent> <leader>, :call ToggleCharacter(',')<CR>
nnoremap <silent> <leader>; :call ToggleCharacter(';')<CR>
" Change a variable assignment to a return
nnoremap <silent> <leader>r :normal! ^cf=return<ESC>
" Poor man's zoom on a window
nnoremap <silent> <leader>z :tab split<CR>

function! SmartGe(e, visual)
    if a:visual
        normal! gv
    endif
    let col = col('.')
    execute "normal! g" . a:e
    if col('.') == col - 1
        execute "normal! g" . a:e
    endif
endfunction
nnoremap <silent> ge :call SmartGe('e', 0)<CR>
nnoremap <silent> gE :call SmartGe('E', 0)<CR>
xnoremap <silent> ge :<C-u>call SmartGe('e', 1)<CR>
xnoremap <silent> gE :<C-u>call SmartGe('E', 1)<CR>

" Pulled this from the help pages, thought it was nifty.
function! JumpToKeywordChoice()
    let num = input("Which one: ")
    if num !=# ''
        execute "normal! " . num . "[\<C-i>"
    endif
endfunction
nnoremap [I [I:call JumpToKeywordChoice()<CR>
nnoremap ]I ]I:call JumpToKeywordChoice()<CR>
" TODO: It seems there's a visual mode command but like the built in '*' it
" only does it for the keyword under the cursor, consider making a mapping for
" visual mode which actually searches for the entire selected word.

" Trying it out
nnoremap <BS> <C-^>

" <CR> is easier to type than % and <num><CR> is easier to type than <num>G.
" The autocmd stuff just makes it so we get the normal <CR> behavior in the
" quickfix and command line windows.
noremap <silent> <CR> :<C-u>call PercentOrGotoLine(v:count, visualmode())<CR>
function! PercentOrGotoLine(count, visual)
    if a:visual !=# ""
        normal! gv
    endif
    if a:count == 0
        normal! %
    else
        execute "normal! ".a:count."G"
    endif
endfunction
augroup map_return_key
    autocmd!
    autocmd CmdwinEnter * nnoremap <buffer> <CR> <CR>
    autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>
augroup END

" There's a lot of mappings that have capital letter counterparts which do
" something related and I'd like 'u' and 'U' to adhere to that pattern. I also
" map <C-r> to redraw the screen since it's free. Note that I have to manually
" source the autoload/repeat.vim file because the <Plug>(RepeatRedo) mapping
" is not available when my vimrc is being parsed.
runtime! autoload/repeat.vim
nmap U <Plug>(RepeatRedo)
" Redraw the screen and remove any search and/or sneak highlighting.
nnoremap <silent> <C-r> :nohlsearch <BAR> silent! call sneak#cancel()<CR><C-l>

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
nnoremap <silent> gB :let @@ = @% . ' ' . line('.')<CR>
" Dual purpose of quickly doing an ls with the added bonus of being able to
" switch to a particular buffer.
nnoremap gb :ls<CR>:b<SPACE>

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
" Delete 3 lines. The reason I don't have these mappings delete 5 lines (which
" would match how they move in normal mode) is because I don't trust myself to
" actually look at 5 lines and say 'hey there are 5 lines there to delete',
" but 3 lines I can definitely eyeball.
onoremap J 2j
onoremap K 2k
" Delete 4 lines which I think I'm also capable of eyeballing
onoremap <C-j> 3j
onoremap <C-k> 3k

" Quickly write a file
nnoremap <leader>w :write<CR>

" = is hard to type so I'm remapping it to 'go', which doesn't seem
" immediately useful and has a nice mnemonic 'organize'. I also remapped 'go'
" to 'gO' on the offhand chance I want to use it.
nnoremap go =
xnoremap go =
nnoremap goo ==
nnoremap gO go

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
" TODO: Make this command do a 'hard' older change where it moves the cursor
" to a line different than one of the lines that was changed. Maybe we could
" even make a family of these commands. One I was just thinking of was having
" a command which goes to an older change not currently visible onscreen.
nnoremap <silent> g: m':call SmartOlderChange()<CR>

" Sources the current file
nnoremap <leader>sc :source % \| nohlsearch<CR>
" Sources .vimrc
nnoremap <leader>sv :source $MYVIMRC \| nohlsearch<CR>

" Edits the .vimrc file.
nnoremap <leader>ee :edit $MYVIMRC<CR>
" Quickly open a file in the same directory as the current file:
" http://vimcasts.org/episodes/the-edit-command/
nnoremap <leader>ew :e <C-R>=escape(expand("%:.:h"), " \t\n") . "/" <CR>
nnoremap <leader>es :sp <C-R>=escape(expand("%:.:h"), " \t\n") . "/" <CR>
nnoremap <leader>ex :sp <C-R>=escape(expand("%:.:h"), " \t\n") . "/" <CR>
nnoremap <leader>ev :vsp <C-R>=escape(expand("%:.:h"), " \t\n") . "/" <CR>
nnoremap <leader>et :tabe <C-R>=escape(expand("%:.:h"), " \t\n") . "/" <CR>

" The two notable uses of tabs that I've seen have been for holding specific
" window layouts or working on a separate project by lcd'ing to a different
" area of the filesystem. Inspired by that second use of tabs I made this
" mapping.
nnoremap <leader>t :tabe <BAR> redraw!<CR>:lcd<SPACE>

" " Undo's all changes made since opening the file and saves the current undo state
" nnoremap <silent><leader>u :let b:save_undo_nr = changenr() <BAR> silent! undo 1 <BAR> silent! undo<CR>
" " Redo's all changes since the above command was used. I would have tried to
" " redo ALL changes but I don't know of a good way to do that.
" nnoremap <silent><leader>r :if exists('b:save_undo_nr') <BAR> execute "undo ".b:save_undo_nr <BAR> endif<CR>

" TODO: This is all still a work in progress. In starting my two ideas (an
" undo to go to the previous save state and a way to save specific file undo
" states to return to later) I realized this might be a little tricker than I
" originally imagined. I'll work on it though. Have a command to go back to
" the previous save state. I'm picturing that if you save at point A then make
" changes and don't save, running this command will take you back to point A.
" It you save at point A, make changes and save at point B and run this
" command (so the buffer is not modified) it will take you back to point A.
" Maybe even make a command to save a save state, like a mark for a undo
" state. For example, if I was making a bunch of changes to a file (testing
" out different ways of implementing the same thing for instance) then each
" time I get something working I can remember a save state. In all reality it
" would probably just be an undo number. Then I could scan through this list
" and pick which save state I'd like to go to.

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
noremap gh H
noremap gl L

" Makes it so the n and N commands always go in the same direction, forward
" and backward respectively, no matter which direction we're actually
" searching.
noremap <silent> n /<CR>zv
noremap <silent> N ?<CR>zv
" Removing those zv's above so the 'c' operator works correctly
onoremap <silent> n /<CR>
onoremap <silent> N ?<CR>

" TODO: I'm starting to learn about tmux and have experimented with sending
" code from vim to another tmux pane running a repl, like clisp, which is
" pretty damn cool if you ask me. So it seems that with tmux you can make vim
" a bit more IDE like because you can:
"
" - Send code from vim to another pane containing a repl to incrementally test
"   your code
" - Compile code asynchronously
" - Run tests asynchronously
" - Run code in a separate pane
"
" Looking into it more, there seem to be a lot of plugins to choose from which
" maybe accomplish the same thing? More specifically I've seen these plugins
" and threads:
"
" - https://github.com/tpope/vim-dispatch
" - https://github.com/jpalardy/vim-slime
" - https://github.com/christoomey/vim-tmux-runner
" - https://github.com/benmills/vimux
" - https://www.reddit.com/r/vim/comments/27m0ep/vim_repl_for_python/
" - https://github.com/Shougo/vimshell.vim
"
" The first 4 seem to be about sending code to a tmux pane while the reddit
" post lists some plugins which allow you to have a repl right in a vim
" buffer. The last one is something I've been meaning to check out, it's a
" shell implemented in vimscript. Could that sort of thing make tmux
" irrelevant in some ways? Anyway, this TODO is to figure out exactly what
" problems these plugins solve. Could some of the above plugins be used in
" tandem with eachother because they all solve slightly different problems? Or
" not?

" My quickly hacked together operator to send keys to a tmux pane. Now if I
" have a repl in the bottom pane of the current window, I can send code to it
" from within vim. TODO: Sometimes I'll zoom the vim pane to see as much code
" as possible but when I try sending the code to the pane it doesn't work. So
" I need to turn off the zoom state before sending the keys.
function! TmuxSendKeys(type, ...)
    let saved_unnamed_register = @@
    if a:0
        silent execute "normal! `<" . a:type . "`>y"
    elseif a:type == 'line'
        silent execute "normal! '[V']y"
    elseif a:type == 'char'
        silent execute "normal! `[v`]y"
    else
        return
    endif
    let cmd = shellescape(substitute(substitute(@@, "\n$", '', ''), "\n", "\r", 'g') . "\r")
    " It would appear that there is no tmux command to deliberately put a pane
    " into a certain mode which is a shame because that means the best option
    " is be to to use 'send-keys' and send the key  which is bound to the
    " 'cancel' command.
    let is_in_mode = system('tmux display-message -t bottom -p "#{pane_in_mode}"')
    if is_in_mode
        call system('tmux send-keys -t bottom C-c')
    endif
    " Unzoom so we can send to the pane using the 'bottom' target specifier
    " and so we can see the results.
    let is_zoomed = system("tmux display-message -p '#{window_zoomed_flag}'")
    if is_zoomed
        call system('tmux resize-pane -Z')
    endif
    call system('tmux send-keys -t bottom ' . cmd)
    redraw!
    let @@ = saved_unnamed_register
endfunction
" Mnemonic = 'You Repl'
nnoremap yr :set opfunc=TmuxSendKeys<CR>g@
nnoremap yrr V:<C-u>call TmuxSendKeys(visualmode(), 1)<CR>
xnoremap R :<C-u>call TmuxSendKeys(visualmode(), 1)<CR>

" Experimenting with being able to run the current program from within vim. I
" know there are some solutions out there like the 3 mentioned in this reddit
" post:
" https://www.reddit.com/r/vim/comments/3vk1sg/bexec_is_a_vim_plugin_that_allows_the_user_to/
" but I wanted to try building a solution myself just to see what it takes. So
" far my ideas only apply to interpreted languages, haven't tried figuring out
" compiled languages yet. My ideas at the moment about this:

" Simplest Case:
" You might want to execute a single file. If that file has the shebang at the
" top of the file then we just execute that file. I still have to think about
" whether I want to check if the file is executable first and manually alert
" the user or if I should just let if fail and the user will quickly realize
" the error and fix it themselves.

" Slightly More Complicated:
" Still want to execute the current file but this time the file does not have
" a shebang at the top. In that case we'll have use an interpreter to run our
" code. This is slightly more complicated because now we have to pick the
" right interpreter based on the type of the file. So we set up a bunch
" Filetype autocommands and use the intrepreter if the shebang falls through.
" Not too bad but still a bit trickier.

" Just A Tad More Complicated:
" This time we want to execute a specific command. Perhaps we're writing some
" new unix utility and it takes a number of parameters and we're testing a new
" parameter, who knows. I think in these situations where we want to execute
" something other than the current file, the only solution is to specify the
" exact command we want to run. So the solution I thought of is that we'd have
" a bash script sitting in the cwd() which I've named 'vim_run_prg'. All we'd
" do is edit that file so it runs the command we want and then vim can run
" that file for us. I was thinking that perhaps sometimes you'd like

" Problems And Things I Still Need To Figure Out Or At Least Make Better:
" 1. If there is no program output, perhaps we'd like to switch back to the
" vim instance?
" 2. When we get problems when trying to run the program inside vagrant. For
" example. If we just had a plain old php file with the shebang at the top
" that we wanted to run. Vim will generate the path to that file on the local
" system when it should be passing the path to the file on the VM.
" 3. When I'm sending keys to the window pane on the virtual box (C-c clear
" C-m) the first 'c' in 'clear' does not get sent. Not sure why.
" 4. Maybe I need to open an external application to test like opening a web
" browser to display html.
" 5. Maybe I need to make some sort of POST or GET request to test out the
" code.

let g:run_program_script_name = 'vim_run_prg'
augroup tmux_testing
    autocmd!
    autocmd Filetype python let b:program_runner = 'python3'
    autocmd Filetype php let b:program_runner = 'php'
    autocmd Filetype ruby let b:program_runner = 'ruby'
augroup END
" TODO: Have 2 new settings. One controls how to call the command (send it to
" tmux, just use vim) and the other will be the pane to target (assuming we're
" going with the tmux approach. That way on the fly I could change where these
" commands are getting sent to.
function! ExecuteCommand(cmd)
    write
    call system('tmux send-keys -t 1.0 C-c clear C-m ' . shellescape(a:cmd) . ' C-m')
    call system('tmux select-window -t 1')
endfunction
function! GetExecuteCommand(exec_current_filep)
    let runner = findfile(g:run_program_script_name, getcwd())
    if !a:exec_current_filep && runner !=# ''
        let cmd = './' . runner
    else
        if getline(1) =~ '^#!'
            let cmd = expand("%:p")
        else
            let cmd = b:program_runner . ' ' . expand("%:p")
        endif
    endif
    return cmd
endfunction
function! ExecuteProgram(exec_current_filep)
    let cmd = GetExecuteCommand(a:exec_current_filep)
    call ExecuteCommand(cmd)
endfunction
nnoremap gx :call ExecuteProgram(0)<CR>
nnoremap gX :call ExecuteProgram(1)<CR>

" An operator to horizontally resize windows. TODO: Consider making this also
" adjust horizontal width. There are 3 possibilities I see with this: adjust
" vertical height, adjust horizontal width, or adjust both. I wonder how I
" could reconcile those options or if it's even worth it.
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

" Quickly close windows
function! SmartCloseWindow()
    let closeWindow1 = char2nr('y')
    let closeWindow2 = char2nr("\<CR>")
    let choice = closeWindow1
    if len(tabpagebuflist()) == 1
        echo 'Are you sure you want to close the window? [y/n]: '
        let choice = getchar()
    endif
    if choice ==? closeWindow1 || choice ==? closeWindow2
        " TODO: If we are trying to close the very last window then just quit
        " vim.
        execute "normal! \<C-w>c"
    endif
endfunction
nnoremap <C-c> :call SmartCloseWindow()<CR>
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

" Quickly switch between tabs or buffers
function! TabOrBufferSwitch(next)
    if tabpagenr('$') > 1
        if a:next
            tabnext
        else
            tabprevious
        endif
    else
        if a:next
            bnext
        else
            bprevious
        endif
    endif
endfunction
nnoremap <silent> <C-n> :call TabOrBufferSwitch(1)<CR>
nnoremap <silent> <C-p> :call TabOrBufferSwitch(0)<CR>

" Slightly easier to type and who uses Q anyway.
nnoremap Q q:

" ' is easier to reach.
noremap ' `
noremap ` '

" Make Y behave like D and C
nnoremap Y y$
" Because I can
nnoremap yp yyp
nnoremap yP yyP

" Run/Compile the program using the makeprg option
nnoremap <silent> <localleader>x :w<CR>:silent! make<CR>:copen <BAR> redraw!<CR>

" Operators to put the top/bottom of the screen on a text object. The one time
" CursorMoved autocommand makes it so the cursor remains in the same position
" after the operator is done.
function! RedrawCursorLineAtTop(type)
    " Whenever you use an operator, your cursor is positioned at the top of
    " the operated area. So this operator is extremely trivial.
    normal! zt
    augroup z_operator_position_cursor
        autocmd!
        autocmd CursorMoved <buffer> call setpos('.', g:z_operator_cursorpos) | autocmd! z_operator_position_cursor
    augroup END
endfunction
nnoremap <silent> zT :let g:z_operator_cursorpos = getpos('.') <BAR> set operatorfunc=RedrawCursorLineAtTop<CR>g@
function! RedrawCursorLineAtBottom(type, ...)
    if a:0
        let end_line = line("'>")
    else
        let end_line = line("']")
    endif
    call cursor(end_line, 1)
    normal! zb
    augroup z_operator_position_cursor
        autocmd!
        autocmd CursorMoved <buffer> call setpos('.', g:z_operator_cursorpos) | autocmd! z_operator_position_cursor
    augroup END
endfunction
nnoremap <silent> zB :let g:z_operator_cursorpos = getpos('.') <BAR> set operatorfunc=RedrawCursorLineAtBottom<CR>g@
vnoremap <silent> zB :let g:z_operator_cursorpos = getpos('.') <BAR> <C-u>call RedrawCursorLineAtBottom(visualmode(), 1)<CR>

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

" Another way to get out of insert mode. I cover all my bases by including
" mappings for every capitalization possibility.
inoremap jk <ESC>
inoremap Jk <ESC>
inoremap jK <ESC>
inoremap JK <ESC>

" Quickly paste from the default register in insert mode
inoremap <C-j> <C-r><C-p>"
cnoremap <C-j> <C-r>"

iabbrev lg Lucas Groenendaal
iabbrev lge groenendaal92@gmail.com
iabbrev lgE Lucas.Groenendaal@careerbuilder.com

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
nnoremap <silent> <leader>* :let w = winsaveview()<CR>:silent! keepjumps normal! *<CR>:call winrestview(w)<CR>

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
xnoremap @ :g/^/normal @
xnoremap . :g/^/normal .<CR>

" }}}

" Operator-Pending Mappings {{{

" Email address text object
onoremap i@ :<C-U>execute "normal! /\\S\\+@\\S\\+.com\r:nohlsearch\rvE"<CR>
onoremap a@ :<C-U>execute "normal! /\\S\\+@\\S\\+.com\r:nohlsearch\rvEl"<CR>

" A text object for the entire buffer
onoremap <silent> ae :<C-u>normal! ggVG<CR>
vnoremap <silent> ae :<C-u>normal! ggVG<CR>

" Text-object for a search pattern
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

" Text object for a number
function! TextObjNumber(modifier, visual_mode)
    let regex = '\d\+'
    if a:modifier ==# 'l'
        call search(regex, 'be')
    else
        if a:modifier ==# 'n' && match(getline('.')[col('.')-1], regex) != -1
            call search(regex, '')
        endif
        call search(regex, 'ec')
    endif
    let end_of_num = [line('.'), col('.')]
    call search(regex, 'bc')
    normal! v
    call cursor(end_of_num)
    let cmd = v:operator.'i'.a:modifier.'d'.(v:operator ==# 'c' ? "\<C-r>.\<ESC>" : '')
    silent! call repeat#set(cmd, v:count)
endfunction
for m in ['', 'n', 'l']
    execute "onoremap <silent> i".m."d :<C-u> call TextObjNumber('".m."', 0)<CR>"
    execute "xnoremap <silent> i".m."d :<C-u> call TextObjNumber('".m."', 1)<CR>"
endfor

" Used to operate on a variable for languages whose variables start with '$'
" signs
onoremap <silent><buffer> iv :<C-u>normal! viwoh<CR>
xnoremap <silent><buffer> iv :<C-u>normal! viwoh<CR>

" }}}

" Command Mappings {{{

" Filter the list rather than proceed sequentially through the command
" history.
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

" Makes it easier to open files in the same directory as other files.
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h') . '/' : '%%'

" Command to count the occurrences of the current search term
command! SearchCount %substitute///gn

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
command! -complete=file -nargs=? Write call CreateAndSaveDirectory(<f-args>)

command! JsonPretty %!python -m json.tool

" }}}

" XML File Settings {{{
augroup filetype_xml
    autocmd!
    autocmd Filetype xml setlocal iskeyword+=-
    autocmd Filetype xml setlocal breakat-=-
    autocmd Filetype xml noremap <silent> [[ :call GoToParentNode()<CR>
augroup END
" }}}

" Markdown File Settings {{{
" TODO: That would be cool if for a 1 and 2 header when using the underline
" style, editing the title automatically adjusts the ==='s or ---'s to match
" the length of the title.
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
    " TODO: Make these work in visual mode as well
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
augroup END
" }}}

" C File Settings {{{
augroup filetype_c
    autocmd!
    " Improve 'execute file' command support for C files when needed.:
    " http://stackoverflow.com/questions/2627886/how-do-i-run-a-c-program-from-vim.
    " I'm kind of picturing that everytime <leader>x gets called then it could
    " check for a Makefile if one exists then use it, otherwise just gcc all the C
    " fils in the current directory and run a.out. I'll have to look into it.
    " Also, consider having the output of whatever program that gets executed get
    " piped into a 'scratch' buffer. Alternatively I could run the 'clear' command
    " before executing.
    autocmd FileType c setlocal commentstring=//\ %s
    autocmd FileType c setlocal makeprg=gcc\ %
    function! RunCProgram()
        if &modified
            write
            silent! make
            let qfl = getqflist()
            if len(qfl)
                copen
                wincmd p
                redraw!
            else
                " So we don't accumulate a bunch of empty qf lists
                colder
                cclose
                !./a.out
            endif
        else
            !./a.out
        endif
    endfunction
    autocmd FileType c nnoremap <silent><buffer> <localleader>x :call RunCProgram()<CR>
augroup END
" }}}

" Vimscript File Settings {{{
augroup filetype_vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
augroup END
" }}}

" Go File Settings {{{

" TODO: I found another little bug where I had 3 splits on the same file. I
" saved the file, then if I hit undo the other two non-active splits jump to
" the bottom of the file.

" Runs goimports on the current file.
function! GoImports()
    " TODO: I've already determined that I do not like adding to the undo
    " history unecessarily. In my case I commented out a line, saved, ran the
    " program, went to undo my comment and had to hit undo twice because of
    " gofmt. But I've also determined that using undojoin is a bit
    " dangerous... I commented out a line, saved, undid, saved, now if I redo
    " I cannot redo to where I commented out the line because I've started
    " down a new undo branch. So I think I should only apply the gofmt if the
    " file has actually been changed by gofmt.
    let currentBuffer = getline(1, '$')
    let formattedCode = split(system('goimports ' . expand('%')), "\n")
    " TODO: If there are errors, add them to location list
    if v:shell_error == 0 && currentBuffer != formattedCode
        let w = winsaveview()
        %delete _
        call setline(1, formattedCode)
        call winrestview(w)
    endif
endfunction

augroup filetype_go
    autocmd!
    autocmd BufWritePost,FileWritePost *.go call GoImports()
augroup END

" }}}


" A vimrc file containing what I consider a minamal amount of configuration to
" make vim more usable as well as my notes on what defines a good text editor
" and how vim fits the bill. I created this to step back and re-evaulate what
" I consider to be a 'good text editor' and to see how much configuration it
" takes for vim to get there.

" The Purpose Of A Text Editor:
" 1. Effective file management
" 2. Move efficiently within a file
" 3. Make quick edits

" What Makes A Great Editor:
" 1. Extensibility

" The first three items above define the job of a text editor. Programmers
" work with text much like a painter works with paint and a text editor is the
" interface between our ideas and the code we write. A good text editor means
" we can say what we want to say with less hassle. If a text editor satisfies
" those first three qualities, it should be considered a good text editor. If
" such an editor is also extensible then I would consider it a great editor or
" at least having the potential to be great.

" How Vim Stacks Up:

" File Management:
" Vanilla vim has some nice buffer managment capabilities. We can use splits
" to view multiple files simultaneously. Tabs can maintain different split
" layouts or allow us to work on different projects. Tab completion on
" existing buffers also makes for quick buffer switching. So vim's file
" management is good, but I would say that vim helps little in keeping track
" of your buffers, they just sort of sit there in a big lump. For a small
" number of files this is typically fine, but things get difficult when
" dealing with a large number of buffers. Doing something as simple as opening
" the 3rd file previously looked is a pain at times. I even remember grep'ing
" for certain search terms again just to find the file I was looking for.
" Luckily a wonderful plugin, ctrlp, nicely fills this missing gap in vim's
" capabilities.

" Move Effectively Within A File:
" Vim's default mode is called 'Normal' mode and in that mode most key presses
" represent a different movement. This makes for quick file navigation because
" we can press single keys to move. For people accustomed to mouse based
" movement, using soley the keyboard can feel restrictive, but getting used to
" it is worth it. Oftentimes a quick search gets you right where you need to
" go just as quickly as a mouse would and when you get to your desired
" location you're ready to type. Even when you miss your mark and take longer
" to get to a position than it would have taken with the mouse, that time is
" typically made up for by the fact that your hands never left the keyboard.
" Moving to an arbitrary place close to the cursor can still feel clunky
" because a full on search can feel like too much for such small movement.
" There is, again, another plugin to fill this gap called sneak.vim.

" Make Quick Edits:
" Code has structure and programmers typically have to make some sort of
" structured change to it. Vim's core editing commands have a pattern to them
" which make quick work of such edits. That pattern is:

"     operator +  motion/text object

" An operator is something which will 'operate' on the text in some way.
" Examples are: copy, delete, change (remember vim is modal), comment
" (assuming a plugin is installed), etc... A 'motion' is any sort of motion
" you can perform (like moving by words). A 'text object' is a structured bit
" of text (like inside single quotes). So if you want to copy, 'y', everything
" inside '{' brackets, 'i{', you type: yi{. If you instead want to delete,
" 'd', everything inside '{' then you type: di{. This editing model makes for
" quick changes to any sort of structured text.

" What Vim Is Missing:
" Plain old vanilla vim can stand on it's own (after a touch of configuring),
" but there are a couple pieces of functionality which, when added, make it
" shine a bit more:

" Essentials:
" I would consider these plugins (or something equivalent) on the essential
" side:

" 1. Manage Buffers - CtrlP
" As I said earlier, vim's buffer management leaves a bit to be desired. Two
" useful things this plugin provides is a list of buffers sorted by how
" recently they were viewed as well as a 'most recently used' file list which
" can re-open files from previous vim sessions.

" 2. Comment/Uncomment Code - commentary.vim
" Without a plugin, the most effective way to comment code is to use visual
" block mode, which is a tad clunky. commentary.vim provides an operator to
" comment code making the task quick and easy.

" 3. Package Manager - Pathogen
" You could manually put plugin files into the appropriate directories but
" this quickly gets tedious especially if you are frequently updating your
" list ok plugins. So I consider this part of the 'essentials' list.

" Nice To Have:
" What is considered to be 'nice to have' could of course go on and on and
" on... This is what I have currently:

" 1. More text objects - targets.vim, indent text object, etc...
" More text objects means more code structures we can quickly operate on.

" 2. More ways to move - sneak.vim, indentwise
" Getting where you need to in a file faster is always a plus.

" 3. Surround text with delimiters - surround.vim
" Just damn convenient when you need it.

" 4. Align text - easy-align
" Also convenient when such a thing is needed.

" 5. Repeaing more commands with '.' - repeat.vim
" Kind of a utility plugin, it allows entire mappings to be repeated with the
" '.' command (after some configuring of course).

" 6. Colorschemes - solarized
" Colorschemes are always nice.

" Minmal Vim Config:
" What follows is a minamal vim configuration which makes vim more usable.
let mapleader = ","
" Starting vim with the -u flag will start up vim instead of vi
set nocompatible
filetype plugin indent on
syntax enable
" Keep non-visible buffers loaded
set hidden
" Backspace behaves as you would expect
set backspace=indent,eol,start
" Seems to just be a better number for making files look more aligned.
set tabstop=4
" Highlight searches
set hlsearch
set incsearch
" Show current file in status line
set laststatus=2
set statusline=
set statusline+=%f
" Poor man's buffer management
nnoremap <leader>b :buffers<CR>:b<SPACE>
" A lot more convenient
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

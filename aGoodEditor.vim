" A vimrc file containing varying amounts of configuration. I created this to
" get a better sense of what makes a text editor good and to see how much
" configuration it takes for vim to become a bit more sane. It contains
" different sections for different levels of wh

let config_level = 0
" let config_level = 1
" let config_level = 2
" let config_level = 3
" let config_level = 4
" Better safe than sorry
set nocompatible
" Hide buffers when not visible
set hidden
" Backspace behaves as you would expect
set backspace=indent,eol,start"
" Seems to just be a better number for making files look more aligned.
set tabstop=4
" Highlight searches
set hlsearch
set incsearch
" Show file in status line
set laststatus=2
set statusline=
set statusline+=%f

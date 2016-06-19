" cwdtabline.vim - The cwd of each tab's active window in the tabline
" Author: Lucas Groenendaal <groenendaal92@gmail.com>

if exists("g:loaded_cwdtabline") || &cp || v:version < 700
    finish
endif
let g:loaded_cwdtabline = 1

function s:getlabel(cwd, prev_label)
    let last_label = a:prev_label
    if a:cwd ==# '/'
        let label = a:cwd
    else
        if fnamemodify(a:cwd, ':~') ==# '~/'
            let label = '~'
        else
            let label = fnamemodify(a:cwd, ':t')
        endif
    endif
    if label ==# last_label
        let last_label = label
        let label = '"'
    else
        let last_label = label
    endif
    return [label, last_label]
endfunction

function! cwdtabline#updatetabline()
    let t:cwdtabline = getcwd()
    if tabpagenr('$') <= 1
        return
    endif
    let cur_tab = tabpagenr()
    let tabs = []
    let prev_label = ""
    for t in range(1, tabpagenr('$'))
        let [label, prev_label] = s:getlabel(gettabvar(t, 'cwdtabline'), prev_label)
        let tab = { 'label': ' ' . label . ' ' }
        let tab.highlight = cur_tab == t ? '%#TabLineSel#' : '%#TabLine#'
        let tabs += [tab]
    endfor
    return join(map(tabs,'printf("%s%s", v:val.highlight, v:val.label)'), '') . '%#TabLineFill#'
endfunction

let t:cwdtabline = getcwd()
set tabline=%!cwdtabline#updatetabline()

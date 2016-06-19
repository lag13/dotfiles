" createvariable.vim - Store selected text in a variable
" Author: Lucas Groenendaal <groenendaal92@gmail.com>

" if exists("g:loaded_createvariable") || &cp || v:version < 700
"     finish
" endif
" let g:loaded_createvariable = 1

augroup createvariable
    autocmd!
    autocmd FileType vim let b:createvariable_aprefix = 'let '
    autocmd FileType php
                \ let b:createvariable_aprefix = '$' |
                \ let b:createvariable_prefix = '$'
    autocmd FileType sh
                \ let b:createvariable_amiddle = '=' |
                \ let b:createvariable_prefix = '"$' |
                \ let b:createvariable_end = '"'
    autocmd FileType c,cpp,cs,java,javascript,php let b:createvariable_aend = ';'
    autocmd FileType c,cpp,java let b:createvariable_remove_var_type = 1
    autocmd FileType go let b:createvariable_amiddle = ' := '
augroup END

function! s:get_setting(setting, default)
    return get(b:, a:setting, get(g:, a:setting, a:default))
endfunction

function! s:get_rval(type, visual)
    let saved_unnamed_register = @@
    if a:type ==# 'v'
        normal! `<v`>y
    elseif a:type ==# 'char'
        normal! `[v`]y
    else
        let start = line("'" . (a:visual ? '<' : '['))
        let end = line("'" . (a:visual ? '>' : ']'))
        let @@ = substitute(join(getline(start, end), "\n"), '^\s*', '', '')
    endif
    let rval = @@
    let @@ = saved_unnamed_register
    return split(rval, "\n")
endfunction

" In some languages you have to specify a type when initially creating a
" variable. This function aims to remove the type and just return the variable
" name.
function! s:remove_var_type(remove_typep, left_assignment)
    if a:remove_typep
        return matchstr(a:left_assignment, '\S*$')
    else
        return a:left_assignment
    endif
endfunction

function! s:build_assignment(indent, aprefix, left_side, amiddle, rval, aend)
    let rval = copy(a:rval)
    let rval[0] = a:indent . a:aprefix . a:left_side . a:amiddle . rval[0]
    let rval[len(rval)-1] .= a:aend
    return rval
endfunction

function! s:find_last_line_to_change(replace_multiplep, start_line)
    if a:replace_multiplep
        let start_indent = indent(a:start_line)
        let line_num = a:start_line + 1
        while start_indent <= indent(line_num)
            let line_num += 1
        endwhile
        return line_num - 1
    else
        return a:start_line
    endif
endfunction

function! s:create_variable(type, ...)
    let aprefix = s:get_setting("createvariable_aprefix", '')
    let aend = s:get_setting("createvariable_aend", '')
    let amiddle = s:get_setting("createvariable_amiddle", ' = ')
    let prefix = s:get_setting("createvariable_prefix", '')
    let end = s:get_setting("createvariable_end", '')
    let replace_multiple = s:get_setting('createvariable_replace_multiple', 0)
    let remove_var_type = s:get_setting('createvariable_remove_var_type', 0)

    let rval = s:get_rval(a:type, a:0)
    let left_side = input("Variable Name: ")
    if left_side !=# ''
        let raw_lval = s:remove_var_type(remove_var_type, left_side)
        let lval = prefix.raw_lval.end
        let indent = matchstr(getline(line('.')), '^\s*')
        let assignment = s:build_assignment(indent, aprefix, left_side, amiddle, rval, aend)
        call append(line('.') - 1, assignment)

        let start_line = line('.')
        let end_line = s:find_last_line_to_change(replace_multiple, start_line)
        let rval_str = substitute(escape(join(rval, "\n"), '/\'), '\n', '\\n', 'g')
        execute start_line.','.end_line.'substitute/\V'.rval_str.'/'.lval.'/g'
    endif
endfunction

nnoremap <silent> <Plug>Createvariable :<C-u>set operatorfunc=<SID>create_variable<CR>g@
xnoremap <silent> <Plug>Createvariable :<C-u>call <SID>create_variable(visualmode(), 1)<CR>
if !hasmapto('<Plug>Createvariable', 'n')
    if maparg('gC', 'n') ==# ''
        nmap gC <Plug>Createvariable
    endif
endif
if !hasmapto('<Plug>Createvariable', 'v')
    if maparg('gC', 'x') ==# ''
        xmap gC <Plug>Createvariable
    endif
endif


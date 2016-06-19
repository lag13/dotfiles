" TODO: Maybe make the spiral destroy the code currently on the page. So copy
" the current page of code to the new buffer and then run the spiral on top of
" it.
function! s:initializeUzumaki()
    for y in range(1, &lines-1)
        call setline(y, repeat(' ', &columns))
    endfor
endfunction

" which_arm | meaning
" ----------+--------
"     0     | top arm
"     1     | right arm
"     2     | bottom arm
"     3     | left arm
function! s:getSpiralPath(max_t, columns, lines, dx, dy, start_col, start_line)
    let my_t = 1
    let which_arm = 0
    let cur_arm_len = a:columns
    let next_arm_len = a:lines-1
    let cur_delta = a:dx
    let next_delta = a:dy
    let col = a:start_col
    let line = a:start_line
    let spiral_path = [[line, col]]

    for i in range(1, a:max_t)
        if my_t < cur_arm_len
            let [new_line, new_col] = s:nextSpiralPos(which_arm, line, col, my_t, 0)
            let my_t += 1
        else
            let [new_line, new_col] = s:nextSpiralPos(which_arm, line, col, cur_arm_len-1, 1)
            let which_arm = (which_arm + 1) % 4
            let cur_arm_len = cur_arm_len - cur_delta
            let temp = cur_arm_len
            let cur_arm_len = next_arm_len
            let next_arm_len = temp
            let temp = cur_delta
            let cur_delta = next_delta
            let next_delta = temp
            let my_t = 1
            let [line, col] = [new_line, new_col]
        endif
        call add(spiral_path, [new_line, new_col])
    endfor
    return spiral_path
endfunction

function! s:nextSpiralPos(which_arm, line, col, change, nudge)
    if a:which_arm == 0
        return [a:line+a:nudge, a:col+a:change]
    elseif a:which_arm == 1
        return [a:line+a:change, a:col-a:nudge]
    elseif a:which_arm == 2
        return [a:line-a:nudge, a:col-a:change]
    else
        return [a:line-a:change, a:col+a:nudge]
    endif
endfunction

function! s:maxTvalue(columns, lines, dx, dy)
    return s:maxTvalueHelper(a:columns, a:lines-1, a:dx, a:dy, -1)
endfunction

function! s:maxTvalueHelper(cur_arm_len, next_arm_len, cur_delta, prev_delta, max_t)
    if a:cur_arm_len < a:cur_delta
        return a:max_t + a:cur_arm_len
    else
        return s:maxTvalueHelper(a:next_arm_len, a:cur_arm_len-a:cur_delta, a:prev_delta, a:cur_delta, a:max_t+a:cur_arm_len)
    endif
endfunction

function! s:drawSpiral(spiral_path, char, ms)
    for pos in a:spiral_path
        if getchar(0)
            return 1
        endif
        call cursor(pos)
        execute "normal! r".a:char
        redraw
        if a:ms > 0
            execute "sleep ".a:ms."m"
        endif
    endfor
    return 0
endfunction

function! s:uzumakiLoop()
    let width = &columns
    let height = &lines - 1
    let start_col = 1
    let start_line = 1
    let dx = 3
    let dy = 2
    let sleep_len = 3
    let spiral_path = s:getSpiralPath(s:maxTvalue(width, height, dx, dy), width, height, dx, dy, start_col, start_line)
    let rev_sp = reverse(copy(spiral_path))
    while 1
        if s:drawSpiral(spiral_path, '#', sleep_len)
            break
        endif
        if s:drawSpiral(spiral_path, ' ', sleep_len)
            break
        endif
        if s:drawSpiral(rev_sp, '#', sleep_len)
            break
        endif
        if s:drawSpiral(rev_sp, ' ', sleep_len)
            break
        endif
    endwhile
endfunction

function! screensavers#uzumaki#uzumaki()
    call screensaver#initializeScreenSaver()
    call s:initializeUzumaki()
    call s:uzumakiLoop()
    call screensaver#quitScreenSaver()
endfunction

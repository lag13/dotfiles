" TODO: Do profiling on this to see where speedups can be made. I
" suspect/wonder if copying the previous generations down on s:draw_buffer
" when the picture fills the screen is slow. If it is slow, I wonder if we can
" make some sort of circular list structure to fix it.
function! s:buildLookupTable(rule_number)
    let rule_num = a:rule_number
    let s:lookup_table = repeat([0], 8)
    for i in range(0, 7)
        let rem = rule_num % 2
        let rule_num = rule_num / 2
        let s:lookup_table[i] = rem
    endfor
endfunction

function! s:getNextState(lc, mc, rc)
    let l = 4 * a:lc
    let c = 2 * a:mc
    let m =     a:rc
    return s:lookup_table[l + c + m]
endfunction

function! s:initializeElementary(rule_num, init_random)
    " Add 2 for the two rows of padding
    let s:width = &columns + 2
    " Subtract 1 for the command line window
    let s:height = &lines - 1
    let s:update_buffer = []
    let s:board = []
    let s:draw_buffer = []
    let s:generation_num = 1
    let s:lookup_table = []

    call s:buildLookupTable(a:rule_num)

    " Create an empty draw buffer
    for y in range(0, s:height-1)
        call add(s:draw_buffer, repeat(' ', s:width-2))
    endfor

    " Create an empty board an initialize it
    let s:board = repeat([0], s:width)
    let s:update_buffer = repeat([0], s:width)
    if a:init_random
        call screensaver#seedRNG(localtime())
        for x in range(1, s:width-2)
            let s:board[x] = screensaver#getRand() % 2
        endfor
    else
        let s:board[s:width / 2] = 1
    endif
    call s:updateDrawBuffer(s:generation_num)
endfunction

function! s:updateDrawBuffer(generation_num)
    let line = ''
    for x in range(1, s:width-2)
        let cell = ' '
        if s:board[x]
            let cell = '#'
        endif
        let line = line . cell
    endfor
    let s:draw_buffer[a:generation_num-1] = line
endfunction

function! s:displayBoard()
    for y in range(0, s:height-1)
        call setline(y+1, s:draw_buffer[y])
    endfor
    redraw
endfunction

function! s:updateBoard()
    " TODO: See if doing a copy on each row would be more efficient or not
    " Be sure to only display the non-padded part of the data structure
    " TODO: It would be interesting to see what the performance would be if
    " each row of the board is the same ECA but just one generation forward.
    " Then we would never need to copy rows.
    for x in range(1, s:width-2)
        let s:update_buffer[x] = s:getNextState(s:board[x-1], s:board[x], s:board[x+1])
    endfor
    let s:board = copy(s:update_buffer)

    " Update the draw buffer
    if s:generation_num == s:height
        for g in range(0, s:height-2)
            let s:draw_buffer[g] = s:draw_buffer[g+1]
        endfor
    else
        let s:generation_num += 1
    endif
    call s:updateDrawBuffer(s:generation_num)
endfunction

function! s:gameLoop()
    while 1
        " Quit if any character is pressed
        if getchar(0)
            break
        endif
        call s:displayBoard()
        call s:updateBoard()
        sleep 100m
    endwhile
endfunction

function! screensavers#eca#elementary(...)
    " I like ECA 90
    let rule_num = 90
    let init_random = 0
    if a:0
        if len(a:1)
            let rule_num = a:1[0]
        endif
        if len(a:1) == 2
            let init_random = 1
        endif
    endif
    " TODO: It seems there's a little issue/bug happening where before the
    " function call below, showtabline=2 and &lines = 61. After the call to
    " InitializeScreenSaver(), showtabline=0 and &lines should be 63 but it's
    " still 61. It seems that all the other screen savers are experiencing
    " this issue, look into what is going on.
    call screensaver#initializeScreenSaver()
    call s:initializeElementary(rule_num, init_random)
    call s:gameLoop()
    call screensaver#quitScreenSaver()
endfunction

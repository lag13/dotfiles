" dinosaur = ##
"            ##
"
" cactus = \| 
"           |/
"           |
" and it's opposite and possibly that could be the smaller version and there
" could be bigger cacti.

function! dinosaur#dinosaur()
    call games#seedRNG(localtime())
    call games#clearBuffer()
    call s:gameLoop()
    " call games#quitGame()
endfunction

function! s:gameLoop()
    " 2/3 down the screen looks nice.
    let height = 2 * (&lines - 1) / 3
    let width = &columns
    " 1/4 of the way across the screen looks nice.
    let player_pos = [1, width / 4]
    " Used to detect if we can accept more user input
    let is_on_ground = 1
    let is_ascending = 0
    let is_descending = 0
    let jump_height = 5

    call s:drawGame(width, height, player_pos)
    while 1
        " If on_ground get input
        " Otherwise
        "   If ascending
        "     increase height by 1
        "     check if at max yet and if so un-flag ascending
        "   else " must be descending
        "     decrease height by 1
        "     check if at min yet and if so, flag on_ground and clear input buffer
        if is_on_ground
            let c = getchar(0)
        else
            let c = 0
        endif

        if c == 113 " q
            break
        elseif c
            let is_ascending = 1
            let is_on_ground = 0
        endif

        if is_ascending
            let player_pos[0] = player_pos[0] + 1
            if player_pos[0] == jump_height
                let is_ascending = 0
                let is_descending = 1
            endif
        elseif is_descending
            let player_pos[0] = player_pos[0] - 1
            if player_pos[0] == 1
                let is_descending = 0
                let is_on_ground = 1
            endif
        endif
        call s:drawGame(width, height, player_pos)
        redraw
        sleep 100ms
    endwhile
endfunction

function! s:drawGame(width, height, player_pos)
    let ground = repeat('@', a:width)
    let spaces = repeat(' ', a:height)
    let player = repeat(' ', a:player_pos[1]-1) . '#' . repeat(' ', a:width-a:player_pos[1]+1)
    for l in range(1, a:height-1)
        call setline(l, spaces)
    endfor
    call setline(a:height, ground)
    echom a:player_pos[0]
    call setline(a:height - a:player_pos[0], player)
endfunction


function! games#snake#snake()
    call games#seedRNG(localtime())
    call games#clearBuffer()
    call s:gameLoop()
    call games#quitGame()
endfunction

function! s:gameLoop()
    let height = min([20, &lines-1])
    let width = min([50, &columns])
    let snake_body = [[1, 1]]
    let cur_dir = [1, 0]
    let food_pos = s:generateFoodPos(snake_body, height, width)
    call s:drawBoard(height, width, snake_body, food_pos)
    while 1
        let [snake_body, cur_dir, food_pos] = s:updateSnake(snake_body, cur_dir, food_pos, height, width)
        if snake_body ==# [[0, 0]]
            break
        endif
        call s:drawBoard(height, width, snake_body, food_pos)
        sleep 70ms
    endwhile
endfunction

function! s:gameOver(height, width, snake_body)
    let head_pos = a:snake_body[0]
    if index(s:tail(a:snake_body), head_pos) != -1 || head_pos[0] < 1 || head_pos[1] < 1 || head_pos[0] > a:height || head_pos[1] > a:width
        return 1
    endif
    return 0
endfunction

function! s:tail(lst)
    let new_lst = deepcopy(a:lst)
    call remove(new_lst, 0)
    return new_lst
endfunction

function! s:getNewDir(cur_dir)
    let new_dir = [0, 0]
    " For the most part this game was working fine but occasionally I would
    " run into an issue where my input didn't seem to register with the game.
    " For example, I would hit 'j' then 'l' in quick succession and the 'j'
    " would register but the 'l' wouldn't. I'm not sure how other games do it
    " (perhaps they have a dedicated thread which continuously reads user
    " input) but this solution seems to work more often than not. What I do is
    " keep consuming a character from the input until I get something
    " 'meaningful' or until it is exhausted (at which point I just return the
    " current direction of travel). 'Meaningful' in this context is a
    " direction perpendicular to the current direction of travel. I still
    " sometimes run into problems where it seems my keystrokes don't register.
    " Perhaps sometimes vim can't register any new input when it is doing some
    " sort of computation? That would definitely explain it. I bet that's what
    " it is.
    while 1
        let input = s:getInput()
        if input ==# ''
            return a:cur_dir
        endif
        if input ==# 'h'
            let new_dir = [0, -1]
        elseif input ==# 'j'
            let new_dir = [1, 0]
        elseif input ==# 'k'
            let new_dir = [-1, 0]
        elseif input ==# 'l'
            let new_dir = [0, 1]
        elseif input ==? 'q'
            return [0, 0]
        endif
        " Keep consuming input if the user tried to move in the same or
        " opposite direction.
        if s:addVector(new_dir, a:cur_dir) !=# [0, 0] && new_dir !=# a:cur_dir && new_dir !=# [0, 0]
            return new_dir
        endif
    endwhile
endfunction

function! s:updateSnake(snake_body, cur_dir, food_pos, height, width)
    let new_dir = s:getNewDir(a:cur_dir)
    if new_dir ==# [0, 0]
        return [[[0, 0]], [0, 0], [0, 0]]
    endif

    let new_snake_body = a:snake_body
    if s:gameOver(a:height, a:width, a:snake_body)
        return [[[0, 0]], [0, 0], [0, 0]]
    endif
    let new_food_pos = a:food_pos
    let should_gen_food = 0
    if new_snake_body[0] == a:food_pos
        call add(new_snake_body, [0, 0])
        let should_gen_food = 1
    endif
    let i = len(new_snake_body)-1
    while i > 0
        let new_snake_body[i] = new_snake_body[i-1]
        let i = i - 1
    endwhile
    let new_snake_body[0] = s:addVector(new_snake_body[0], new_dir)
    if should_gen_food
        let new_food_pos = s:generateFoodPos(new_snake_body, a:height, a:width)
    endif

    return [new_snake_body, new_dir, new_food_pos]
endfunction

function! s:getInput()
    let c = getchar(0)
    if c == 0
        return ''
    else
        return nr2char(c)
    endif
endfunction

function! s:clearBoard(height, width)
    let right_border = ''
    let lower_right_corner = ''
    if a:width < &columns
        let right_border = '|'
        let lower_right_corner = '+'
    endif
    let spaces = repeat(' ', a:width)
    for y in range(1, a:height)
        call setline(y, spaces . right_border)
    endfor
    if a:height < &lines-1
        call setline(a:height+1, repeat('-', a:width) . lower_right_corner)
    endif
endfunction

function! s:drawChar(char_to_draw, pos)
    call cursor(a:pos)
    execute "normal r".a:char_to_draw
endfunction

function! s:drawBoard(height, width, snake_body, food_pos)
    let char_to_draw = '#'
    call s:clearBoard(a:height, a:width)
    for s in a:snake_body
        call s:drawChar(char_to_draw, s)
    endfor
    call s:drawChar(char_to_draw, a:food_pos)
    redraw
endfunction

" TODO: Should we put the code in here which detects if it is or is not
" possible to generate more food? Or somewhere else? Remember, when you can't
" generate anymore food then you've won.
" TODO: Improve this code in the future. I feel this is kind of brute force
" right now (i.e just keep generating until it works). We could build a list
" of available places for food and then randomly generate an index into that
" list.
function! s:generateFoodPos(snake_body, height, width)
    let pos = s:genRandomPos(a:height, a:width)
    while 1
        if index(a:snake_body, pos) == -1
            return pos
        endif
        let pos = s:genRandomPos(a:height, a:width)
    endwhile
endfunction

function! s:genRandomPos(height, width)
    let y = games#rand() % a:height + 1
    let x = games#rand() % a:width + 1
    return [y, x]
endfunction

function! s:addVector(v1, v2)
    let v3 = []
    let len1 = len(a:v1)-1
    let len2 = len(a:v2)-1
    for i in range(0, len1 < len2 ? len1 : len2)
        call add(v3, a:v1[i] + a:v2[i])
    endfor
    return v3
endfunction


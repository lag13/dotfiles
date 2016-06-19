function! s:initializeAlphabet()
    let letters = {}

    let zero = []
    call add(zero, " 0000000 ")
    call add(zero, "0       0")
    call add(zero, "0       0")
    call add(zero, "0       0")
    call add(zero, "0       0")
    call add(zero, "0       0")
    call add(zero, "0       0")
    call add(zero, "0       0")
    call add(zero, " 0000000 ")

    let one = []
    call add(one, "    1    ")
    call add(one, "    1    ")
    call add(one, "    1    ")
    call add(one, "    1    ")
    call add(one, "    1    ")
    call add(one, "    1    ")
    call add(one, "    1    ")
    call add(one, "    1    ")
    call add(one, "    1    ")

    let two = []
    call add(two, " 2222222 ")
    call add(two, "        2")
    call add(two, "        2")
    call add(two, "        2")
    call add(two, " 2222222 ")
    call add(two, "2        ")
    call add(two, "2        ")
    call add(two, "2        ")
    call add(two, " 2222222 ")

    let three = []
    call add(three, " 3333333 ")
    call add(three, "        3")
    call add(three, "        3")
    call add(three, "        3")
    call add(three, " 3333333 ")
    call add(three, "        3")
    call add(three, "        3")
    call add(three, "        3")
    call add(three, " 3333333 ")

    let four = []
    call add(four, "4       4")
    call add(four, "4       4")
    call add(four, "4       4")
    call add(four, "4       4")
    call add(four, " 4444444 ")
    call add(four, "        4")
    call add(four, "        4")
    call add(four, "        4")
    call add(four, "        4")

    let five = []
    call add(five, " 5555555 ")
    call add(five, "5        ")
    call add(five, "5        ")
    call add(five, "5        ")
    call add(five, " 5555555 ")
    call add(five, "        5")
    call add(five, "        5")
    call add(five, "        5")
    call add(five, " 5555555 ")

    let six = []
    call add(six, " 6666666 ")
    call add(six, "6        ")
    call add(six, "6        ")
    call add(six, "6        ")
    call add(six, " 6666666 ")
    call add(six, "6       6")
    call add(six, "6       6")
    call add(six, "6       6")
    call add(six, " 6666666 ")

    let seven = []
    call add(seven, "77777777 ")
    call add(seven, "        7")
    call add(seven, "        7")
    call add(seven, "        7")
    call add(seven, "        7")
    call add(seven, "        7")
    call add(seven, "        7")
    call add(seven, "        7")
    call add(seven, "        7")

    let eight = []
    call add(eight, " 8888888 ")
    call add(eight, "8       8")
    call add(eight, "8       8")
    call add(eight, "8       8")
    call add(eight, " 8888888 ")
    call add(eight, "8       8")
    call add(eight, "8       8")
    call add(eight, "8       8")
    call add(eight, " 8888888 ")

    let nine = []
    call add(nine, " 9999999 ")
    call add(nine, "9       9")
    call add(nine, "9       9")
    call add(nine, "9       9")
    call add(nine, " 9999999 ")
    call add(nine, "        9")
    call add(nine, "        9")
    call add(nine, "        9")
    call add(nine, " 9999999 ")

    let colon = []
    call add(colon, "         ")
    call add(colon, "   :::   ")
    call add(colon, "   :::   ")
    call add(colon, "   :::   ")
    call add(colon, "         ")
    call add(colon, "   :::   ")
    call add(colon, "   :::   ")
    call add(colon, "   :::   ")
    call add(colon, "         ")

    let letters['0'] = zero
    let letters['1'] = one
    let letters['2'] = two
    let letters['3'] = three
    let letters['4'] = four
    let letters['5'] = five
    let letters['6'] = six
    let letters['7'] = seven
    let letters['8'] = eight
    let letters['9'] = nine
    let letters[':'] = colon
    return letters
endfunction

function! s:clockLoop()
    let letter_height = 9
    let letter_width = 9
    let letter_definitions = s:initializeAlphabet()
    let num_spaces_between_letters = 1
    let dy = 1
    let dx = 1
    let line = 1
    let col = 1
    while 1
        if getchar(0)
            break
        endif
        let cur_time = s:getCurTime()
        call s:drawClock(s:getClock(cur_time, letter_definitions, num_spaces_between_letters, letter_height), [line, col], letter_height)
        redraw
        sleep 100m
        let [dy, dx] = s:computeDirection([line, col], letter_height, s:getStrLen(cur_time, num_spaces_between_letters, letter_width), dy, dx)
        let line = line + dy
        let col = col + dx
    endwhile
endfunction

function! s:getStrLen(str, num_spaces_between_letters, letter_width)
    let str_len = strlen(a:str)
    return str_len*a:letter_width+a:num_spaces_between_letters*(str_len-1)
endfunction

" Alters the direction of travel if we've hit an edge
function! s:computeDirection(upper_left_corner, height, width, dy, dx)
    let corner_line = a:upper_left_corner[0]
    let corner_col = a:upper_left_corner[1]
    let line_lower_limit = 1
    let line_upper_limit = &lines-1
    let col_lower_limit = 1
    let col_upper_limit = &columns
    let new_dy = a:dy
    let new_dx = a:dx

    if corner_line + a:dy < line_lower_limit
        let new_dy = -a:dy
    elseif corner_line + a:height + a:dy > line_upper_limit
        let new_dy = -a:dy
    endif
    if corner_col + a:dx < col_lower_limit
        let new_dx = -a:dx
    elseif corner_col + a:width + a:dx > col_upper_limit
        let new_dx = -a:dx
    endif

    return [new_dy, new_dx]
endfunction

function! s:drawClock(clock, upper_left_corner, letter_height)
    let corner_line = a:upper_left_corner[0]
    let corner_col = a:upper_left_corner[1]
    let empty_line = repeat(' ', &columns)
    for l in range(1, corner_line-1)
        call setline(l, empty_line)
    endfor
    for l in range(corner_line, corner_line + a:letter_height - 1)
        let line = repeat(' ', corner_col-1) .  a:clock[l-corner_line]
        call setline(l, line)
    endfor
    for l in range(corner_line + a:letter_height, &lines-1)
        call setline(l, empty_line)
    endfor
endfunction

function! s:getClock(str, letter_definitions, num_spaces_between_letters, letter_height)
    let spaces = repeat(' ', a:num_spaces_between_letters)
    let result = []
    for i in range(0, a:letter_height-1)
        let line = ""
        for j in range(0, strlen(a:str)-1)
            let line = line . get(a:letter_definitions, a:str[j])[i] . spaces
        endfor
        let line = strpart(line, 0, strlen(line) - a:num_spaces_between_letters)
        call add(result, line)
    endfor
    return result
endfunction

function! s:getCurTime()
    return strftime("%H:%M:%S")
endfunction

function! screensavers#clock#clock()
    call screensaver#initializeScreenSaver()
    call s:clockLoop()
    call screensaver#quitScreenSaver()
endfunction


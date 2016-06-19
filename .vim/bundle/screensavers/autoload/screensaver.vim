function! screensaver#seedRNG(seed)
    let g:seed = a:seed % 509
endfunction

" TODO: Try to configure the cursor so it is not displayed. For terminal vim I
" believe the terminal itself would have to be configured from within vim. Or
" perhaps we could create highlighting to just conceal the cursor altogether?
" Looking at sneak.vim it seems that he has to do something to prevent that
" from happening so logically I should be able to make it happen.
" TODO: Consider using randomness from the system to generate random numbers:
" http://stackoverflow.com/questions/20430493/how-to-generate-random-numbers-in-the-buffer
" http://stackoverflow.com/questions/3062746/special-simple-random-number-generator
function! screensaver#getRand()
    let a = 35
    let c = 1
    let m = 509
    let g:seed = (a * g:seed + c) % m
    return g:seed
endfunction

function! screensaver#initializeScreenSaver()
    " Open up a blank buffer
    -tabedit
    " Maximize screen space
    setlocal nonumber
    setlocal nocursorline
    setlocal nocursorcolumn
    setlocal nowrap
    let g:save_laststatus = &laststatus
    let g:save_showtabline = &showtabline
    set laststatus=0
    set showtabline=0
    " Generally, the screen will be filled with spaces as filler characters,
    " but we might want to draw a single colored square. Doing this lets us
    " draw that square using the tab character.
    setlocal noexpandtab
    setlocal tabstop=1
endfunction

function! screensaver#quitScreenSaver()
    let &laststatus = g:save_laststatus
    let &showtabline = g:save_showtabline
    bdelete!
endfunction

function! screensaver#runScreenSaver(ss_name, ...)
    if has_key(g:screensaver_default_screensavers, a:ss_name)
        let Fn = function(g:screensaver_default_screensavers[a:ss_name])
    else
        echohl ErrorMsg
        echom "No screensaver defined under that name"
        echohl NONE
        return
    endif

    if a:0
        call Fn(a:000)
    else
        call Fn()
    endif
endfunction

" TODO: Some screensavers (like eca) can take one or more arguments. Try to
" make this completion smarter. Perhaps a solution would be to associate a
" function with each screensaver which would modify completion. That could
" have the added benefit of letting the user create their own screensavers
" using custom completion.
function! screensaver#filterScreenSaverList(arg_lead, cmd_line_unused, cursor_pos_unused)
    return filter(keys(g:screensaver_default_screensavers), 'stridx(v:val, a:arg_lead) == 0')
endfun

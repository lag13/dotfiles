let g:screensaver_default_screensavers = {
            \ 'gol':     'screensavers#gol#gameOfLife',
            \ 'eca':     'screensavers#eca#elementary',
            \ 'uzumaki': 'screensavers#uzumaki#uzumaki',
            \ 'clock':   'screensavers#clock#clock',
            \ }

command! -nargs=* -complete=customlist,screensaver#filterScreenSaverList ScreenSaver call screensaver#runScreenSaver(<f-args>)

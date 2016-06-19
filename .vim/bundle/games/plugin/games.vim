let g:game_default_games = {
            \ 'snake':     'games#snake#snake',
            \ }

command! -nargs=* -complete=customlist,games#filterGameList Game call games#runGame(<f-args>)


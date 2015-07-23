" Some vimscript I wrote to help configure luceo systems.
" By Lucas Groenendaal

" Check out this post for setting up vim as more of an IDE when writing php
" code: http://blog.joncairns.com/2012/05/using-vim-as-a-php-ide/

" TODO: Seems like I might not have nodes in the data structure for <agence>
" and some others in that hierarchy.

" TODO: Make mapping to go directly to a CDATA/xml node's json label.

" TODO: Add highlighting for CDATA lines which are sections. Make a command to
" automatically add commented strings to all lines in CDATA. This would negate
" the need for the GetLabelNameWrapper() function.

" That would be really cool if, in the CDATA section of the xml files, I could
" display the actual label associated with a node. This section wouldn't
" actually be part of the file, it would just be displayed somehow.

" Look into using the :keepalt command to edit files but not change the
" alternate file.

" Make an autocommand to run translator:generate everytime we save a
" php.en.json file. On a similar note, look into what needs to be done so I
" could call the functions defined in my .bashrc inside of vim. It would be
" nice to be able to call my 'psf' commands.

" ABOUT: Learning vimscript was a recent endeavor and this is my first attempt
" at doing any serious work with it. The goal with this code is to help
" automate the various things we must do when working with the configuration
" files.

" TODO:

" Add something that will add fields in bulk. I just had to add 11 short
" text fields to a candidate and it would have been a lot smoother if I could
" just add them all in one go.

" Right now that 'GetFieldType()' type spits out every possible field. But
" sometimes it isn't necessary. Like when you remove a field, you only need to
" worry about contacts and selects, there is no need to print that whole list
" to the user. Make 'GetFieldType()' accept a list of strings that it will
" accept.

" Should I make a mapping to change a field type?? That could be useful and it
" could be easy to do.

" The go to CDATA from an xml node isn't always working correctly. Tighten
" that up.

" Look into when/why my functions sometimes prompt me to 'Hit Enter to
" continue'. I noticed it happen when I was removing a 'select' field.

" Look into how to retain state when making changes. Like keeping the
" alterrnate buffer the same, keep the view of the screen and all that sort of
" stuff. This is just for aesthetic reasons.

" Make a mapping to insert/remove a state in the candidate workflow

" Make a function which will activate parent nodes as well as all their
" children. The 'naive' way would be extremely simple: go to the parent node,
" run :normal! vat, then run :call ChangeNodeAttribute('actif', 'true').  This
" would only fail in the case that there are multiple nodes on the same line.
" I suppose I could try running a check on each line in the selection to make
" sure that all 'actif' attributes are true, I'll look into it.

" Mapping to configure silo

" Mapping to disable EEO (at least the xml part)

" WEIRD ERROR: I thought I documented this error but it seems like I haven't... That
" weird problem happened again where when using normal o to insert text, the
" text is wrapping to the next line. I noticed it when I tried to configure
" the candidate workflow. For example I wanted to insert this text:
"       <etat1 cle="AFFECTATION_WORKFLOW_ETAT_1" libelle="TXT_AFFECTATION_WORKFLOW_ETAT_1" icone="icones/etat1.png" onenter="" onleave="" libelle-onenter="" libelle-onleave=""/>
" But when I ran the command that text looke something like this:
"       <etat1 cle="AFFECTATION_WORKFLOW_ETAT_1" libelle="TXT_AFFECTATION_WORKFLOW_ETAT_1" 
"       icone="icones/etat1.png" onenter="" onleave="" 
"       libelle-onenter="" libelle-onleave=""/>
" I really have no idea why that would be happening. Maybe I'll try to
" investigate again, I think it has something to do with the presence of a
" horizontal split but I'm unsure. OKAY! So I actually have a better idea of
" what is causing this now. The short and sweet answer is that the lines are
" breaking because of the 'textwidth' option. Now, normally that option is 0
" for xml documents. BUT when we source the .vimrc file it seems that it gets
" set to 78. I'm currently unsure as to why that is, but at least we have
" something to go off of. So it's not some weird bug, it's just this setting.

" Mapping to go from one CDATA section to another for the same node.

" I think using echoerr might not be the best thing? Try to look into printing
" a messages that just stands out to the user so they can see if something
" went wrong.

" Look at the 'Marks' section in motion.txt it might have some ideas
" about things like not changing the jumplist when using marks and stuff like
" that.

" Make a mapping to configure the email apply customization.

" I created a command to call this funtion, the only problem is that commands
" pass their arguments as strings. I don't know how to get around this so I've
" just eval'd all of the arguments.
" function! CalculateQuarterlyBonus(target, company_achievement, individual_achievement, company_bonus_percent, individual_bonus_percent)
function! CalculateQuarterlyBonus(...)
    let company_achievement = 1.0
    let individual_achievement = 1.5
    let target = 1100.0
    let company_bonus_percent = 0.5
    let individual_bonus_percent = 0.5
    if a:0 > 0
        let company_achievement = eval(a:1)
    endif
    if a:0 > 1
        let individual_achievement = eval(a:2)
    endif
    if a:0 > 2
        let target = eval(a:3)
    endif
    if a:0 > 3
        let company_bonus_percent = eval(a:4)
    endif
    if a:0 > 4
        let individual_bonus_percent = eval(a:5)
    endif
    echo 'Company Achievement      = 1.0'
    echo 'Individual Achievement   = 1.5'
    echo 'Target                   = 1100.0'
    echo 'Company Bonus Percent    = 0.5'
    echo 'Individual Bonus Percent = 0.5'
    echo target * company_achievement * company_bonus_percent + target * individual_achievement * individual_bonus_percent
endfunction
command! -nargs=* EchoQuarterlyBonus call CalculateQuarterlyBonus(<f-args>)

" Helper code to return the file path to configuration files {{{
" Gets the absolute path the client's root. Remember that all clients have a
" 'web' folder in their home directory.
function! GetPathToClientRoot()
    let path_to_file_arr = split(expand('%:p:h'), '/')
    while len(path_to_file_arr)
        if isdirectory('/'.join(path_to_file_arr, '/').'/web')
            return '/'.join(path_to_file_arr, '/').'/'
        endif
        call remove(path_to_file_arr, len(path_to_file_arr)-1)
    endwhile
endfunction
" Returns 1 or 0 if the client's system has a 4.0 directory structure or not.
function! Is40DirectoryStructure()
    if isdirectory(getcwd().'/config')
        return 1
    else
        return 0
    endif
endfunction
function! GetXmlFileLocation()
    let path = 'web/param/xml/'
    if Is40DirectoryStructure()
        let path = 'config/xml/'
    endif
    return GetPathToClientRoot().path
endfunction
function! GetTranslationsFileLocation()
    let path = 'web/param/tools/translations/'
    if Is40DirectoryStructure()
        let path = 'custom/tools/translations/'
    endif
    return GetPathToClientRoot().path
endfunction
function! GetPsfFile()
    let path = 'psf.php'
    if Is40DirectoryStructure()
        let path = 'bin/console'
    endif
    return GetPathToClientRoot().path
endfunction
" }}}

" PSF commands {{{
" Runs psf translator:generate.
function! RunTranslatorGenerate()
    execute "!php ".GetPsfFile()." translator:generate"
    redraw!
endfunction

" Runs psf translator:dedupe.
" TODO: So I needed to change a label which was not in the json file. I ran
" merge and the label appeared. Then I changed the label and ran this command
" below. After doing that, the label I changed disappeared?!?! I have no idea
" why this is happening. If I start a shell in vim and manually run the
" command it works as expected. Actually I just did this again and didn't run
" into any issue. Maybe I just forgot to save? I also noticed some weirdness
" with dedupe and merge. When I ran dedupe it deleted the TXT_AGENCE constant
" even though it was changed.
function! RunTranslatorDedupe()
    execute "silent !php ".GetPsfFile()." translator:dedupe"
    redraw!
endfunction

" Runs psf translator:merge.
function! RunTranslatorMerge()
    execute "silent !php ".GetPsfFile()." translator:merge"
    redraw!
endfunction
" }}}

" XML Related functions (mostly helper function sort of stuff) {{{

" Well that function name is a mouthful. I tried to be descriptive as possible. Say
" you had two paths like this:
"   - one/two/three
"   - one/temp/two/three
" This function would return 'two/three' because it removed the longest common
" prefix ('one/') from the first string and returned that.
function! GetPathRemovingCommonPrefix(path1, path2)
    let path1_list = split(a:path1, '/')
    let path2_list = split(a:path2, '/')
    let i = 0 
    while i <# len(path1_list)
        if path1_list[i] !=# path2_list[i]
            break
        endif
        let i += 1
    endwhile
    let return_path = ""
    while i <# len(path1_list)
        let return_path = return_path . '/' . path1_list[i]
        let i += 1
    endwhile
    " Remove the leading '/' before returning
    return strpart(return_path, 1)
endfunction

" Searches for an xml node given it's path.
" This function is in no way perfect but it should ususally work.

" TODO: Thoughts for improvement:
" 3. See if we can make it so this function doesn't change the jump list.
" Right now when we call GetPathToXMLNode() that changes the jump list. So
" maybe I should try messing around with that function to prevent the jump
" list from changing?
function! GoToXMLNode(path_to_node)
    "THOUGHT: This function will loop infinitely if
    "path_to_travel is ''. But as long as a:path_to_node is
    "non empty, I don't think that will ever happen. Keep it
    "in mind though.
    if a:path_to_node ==# ""
        return 0
    endif
    let abs_node_path = a:path_to_node
    let start_pos = getpos('.')
    " Move to the top of the file
    call cursor(1, 1)
    let path_to_travel = abs_node_path
    let not_there_yet = 1
    while not_there_yet
        " Go to the node by searching for each successive node
        for i in split(path_to_travel, '/')
            " Search for the next node in the path ignoring any comments
            while 1
                if search('\v\<' . i . '( |\>|/|	)', 'W')
                    if synIDattr(synID(line('.'), col('.'), 1), "name") !=# 'xmlCommentPart'
                        break
                    endif
                else
                    "Couldn't find the specified path, restore the cursor
                    "position and return a failure value.
                    call setpos('.', start_pos)
                    return 0
                endif
            endwhile
        endfor
        " We've landed on every node but maybe they aren't the actual nodes we
        " were trying to get to. We check to make sure our location is
        " correct.
        let cur_path = GetPathToXMLNode()
        if abs_node_path ==# cur_path
            let not_there_yet = 0
        else
            let path_to_travel = GetPathRemovingCommonPrefix(abs_node_path, cur_path)
        endif
    endwhile
    call setpos("''", start_pos)
    return 1
endfunction 

" Technically, all this function does is yank the first double quoted string
" that appears on a line.  It's purpose is to return the path to a node that
" appears in CDATA.
function! GetNodePathFromCDATA(cdata_line_no)
    return substitute(getline(a:cdata_line_no), '[^"]*"\([^"]*\)".*', '\1', '')
endfunction

" Treats the first string on the current line as an xml path and then calls
" "GoToXMLNode()" on it. Meant to be used in the CDATA sections of the xml.
function! LuceoFindNode(path_to_find)
    let path = a:path_to_find
    " For some reason, some files return a full path (all the way up the root
    " node) when using GetPathToXMLNode() while others don't. These files do
    " and we check for them so this function always works.
    let files = ['embauche.xml', 'poste.xml']
    let index = index(files, expand('%:t'))
    if index !=# -1
        let path = substitute(files[index], '\([^.]*\).*', '\1', '').'/'.path
    endif
    call GoToXMLNode(path)
endfunction

" Gets the name of an xml node. Not perfect by any stretch but I think it will
" work most of the time.
function! GetNodeName()
    let save_pos = getpos('.')
    " Try to go to the opening '<' of the xml node.
    if !search('<', 'bc', line('.'))
        call search('<', 'c', line('.'))
    endif
    " This is, more or less, the regex that matches a valid xml node name.
    let result = matchstr(strpart(getline('.'), col('.')-1), '^</\?\zs[a-zA-Z_:][-a-zA-Z0-9._:]*')
    call setpos('.', save_pos)
    return result
endfunction

" Goes to an xml node's parent. I think this (like many other things it seems)
" also could use some more work but should work just fine most of the time.
function! GoToParentNode()
    " Store current line
    let start_line = line('.')
    " Try to go to parent end tag
    normal! vat
    let parent_close_tag = line('.')
    " Try to go to parent start tag
    normal! o
    let parent_open_tag = line('.')
    " If we never moved, we were on a start/end tag to begin with. We do two
    " 'at' motions to go to the next parent tag (if there is one).
    if start_line ==# parent_close_tag || start_line ==# parent_open_tag 
        " If the motion to move to a parent tag fails then a beep will occur.
        " I don't like this as nothing is really wrong. So we disable it
        " temporarily.
        let temp = &t_vb
        set visualbell t_vb=
        normal! ato
        set novisualbell
        let &t_vb = temp
        " If the above command was unsuccessful then we're as far out as we
        " can go and we'll be in visual mode on the opening tag of the current
        " node. Exit visual mode and return 0 because we couldn't go to our
        " parent.
        if line('.') ==# parent_open_tag
            normal! v
            return 0
        endif
    endif
    execute "normal! \<ESC>"
    return 1
endfunction

" Gets the absolute path to an xml node from the current cursor position. This
" function is also not perfect by any stretch, but should work most of the
" time.
function! GetPathToXMLNode()
    " Save the cursor position. 
    let start_pos = getpos('.')
    let alt_mark_pos = getpos("''")
    let path = [GetNodeName()]
    " Originally this if statement was a simple while loop. I replaced that
    " while loop because it was running a little slower than I liked. Although
    " messier, this if statement is more optimized.
    if GoToParentNode()
        call insert(path, GetNodeName())
        " After the above call to GoToParentNode() we are on a pair of opening
        " tags. This will put us at the closing tag of that pair.
        normal! vat
        let parent_close_tag = line('.')
        while 1
            " Prevent the error bell from sounding.
            let temp = &t_vb
            set visualbell t_vb=
            normal! at
            set novisualbell
            let &t_vb = temp
            " There wasn't a parent node to go to, so we didn't moved and are
            " left in visual mode.
            if parent_close_tag ==# line('.')
                normal! v
                break
            else
                let parent_close_tag = line('.')
                call insert(path, GetNodeName())
            endif
        endwhile
    endif

    call setpos('.', start_pos)
    call setpos("''", alt_mark_pos)
    return join(path, '/')
endfunction

" Goes from an xml node to it's location in CDATA

" It seems to work but there's still some funkiness going on which I think
" might stem from the GoToParentNode() function. I notice that when I pass the
" GetPathToXMLNode() function directly into the search() function, it doesn't
" work. But when I store the result in a variable then it works just fine.

" Another weird thing is that in all .xml files except candidat.xml, doing vat
" enough will grab the root xml nodes as well. My quick hack is if we can't
" find the path then we strip off the first portion and try searching for that
" as well.
function! LuceoFindCDATA()
    let save_pos = getpos('.')
    let path = GetPathToXMLNode() . '\>'
    call cursor(1,1)
    if !search(path)
        if !search(substitute(path, "[^/]*/", "", ""))
            call setpos('.', save_pos)
        else
            call setpos("''", save_pos)
        endif
    else
        call setpos("''", save_pos)
    endif
endfunction

" Leaves the cursor on the quote surrounding an attribute's value.
" Assumes the cursor is on the node who's attribute we're going to.
function! GoToNodeAttribute(attribute)
    " Move cursor to the beginning of the node
    call search('<', 'bc', line('.'))
    " Move to the attribute's value
    if search('\v\s+' . a:attribute . '\s*\=\s*("|' . "')", 'e', line('.'))
        return 1
    endif
    return 0
endfunction

" Changes a node's attribute
" Assumes the cursor is on the line with the xml node to alter
function! ChangeNodeAttribute(attribute, new_val)
    " Go to the attribute's value
    if GoToNodeAttribute(a:attribute)
        let save_unnamed_register = @@
        " Get the character under the cursor (will be a single or double quote)
        normal! yl
        " Change the value
        execute 'normal! ci' . @@ . a:new_val . "\<ESC>"
        let @@ = save_unnamed_register
    endif
endfunction

" Changes a node's attribute
" Assumes the cursor is on the line with the xml node 
function! GetNodeAttribute(attribute)
    " Go to the attribute's value
    if GoToNodeAttribute(a:attribute)
        " Save the register so it can be restored
        let save_unnamed_register = @@
        " Yank the attribute
        normal! yl
        execute 'normal! yi' . @@
        " Store the result to return
        let result = @@
        let @@ = save_unnamed_register
        return result
    endif
    return 0
endfunction

" }}}

" Data Structure of all Fields {{{

" THINGS I HAD TO ADJUST AND WATCH OUT FOR AFTER TAKING DATA FROM THE WIKI:
" So the wiki isn't completely correct with some things. These are things it
" gets wrong: 
" CANDIDAT:
"   1. All the Work Experience nodes should be 'experiences', on the wiki
"   they're listed as 'experience'. You can accomplish this change with this
"   command-> :g/"field_category" : 2,/.+1s:experience:experiences
"   2. These qualification nodes shouldn't have the trailing dash:
"       qualification/liste-editable1-, qualification/liste-editable2-,
"       qualification/liste-editable3-
"   3. The nodes: 'mairie/debut-rech3', 'mairie/debut-rech2', and
"   'mairie/debut-rech' all have the same json constant. For now I'm only
"   keeping the first debut-rech. The same thing is true of their 'fin-rech'
"   counter parts.
"   4. The experiences nodes in the wiki: 'experiences/date-libre1-5' are
"   missing the 'b' in 'liBre'
"   5. The 'profil/langues/langue1-15' fields are listed as secondary
"   dropdowns but I don't really think they are. I'm not really sure what to
"   do with them so I'm just removing them.
"   6. The node 'profil/niveau-etudes' is missing the ending 's' on the wiki.
"   7. The node 'profil/niveau-experience' has a different label in the
"   searchable and regular sections of the candidate form.
"   8. The 'type-contrat' node below 'situation-recherchee' and
"   situation-actuelle' share the same searchable node.
"   9. There are two nodes listed in the 'regular fields' section with a
"   parent of 'Experiences'. These should probably get moved with other
"   'exepriences' nodes but I'm not sure. For now I've just taken them out.
"   10. The node 'mairie/observation-rech' is listed as a short AND long
"   field when it is really just a long text field.
"   11. Just like the 'debut-rech' nodes in 3. the nodes 'poste-envisage1-3'
"   also share the same json constant.
"   12. Just like the 'debut-rech' nodes in 3, the nodes 'taux-rech1-3' share
"   the same json constant.
"   13. situation-recherchee/heures does NOT have an email tag associated with
"   it.
"   14.  DEPARTMENT

" POSTE:
"   1. The 'descriptif/libre-long-lienref1-5' nodes don't have searchable
"   counterparts. I've put them at the end of the list of 'long' nodes.
"   2. There are 'langue' fields in poste.xml as well that are listed as
"   secondary. I don't really know what to do with these either so I'm taking
"   them out.
"   3. There are short text fields 'langues/libre1-9' I wasn't exactly sure
"   what they were used for either so I just took them out.
"   4. The 'mairie/categorie' field has to have it's search path manually
"   added.
"   5. The contact nodes are listed like 'Descriptif/contact2'. That 'D'
"   should be lower case.
"   6. The divers/charge-recrutement node has the wrong search path in the
"   wiki. This needs to be fixed.

" HOW I ADDED THE SEARCHABLE PATHS:
" Adding the searchable paths was tricky because there doesn't seem to be a
" strictly algorithmic way to do it. The trouble is that the names of the
" nodes you add aren't necessarily the same as their searchable counterpart.
" To make matters worse, some nodes have the same name! I was hoping the csv
" files could make this part easier, but unfortunately that is not the case.
" The best way to do it, I believe, is how I was originally doing it which is
" to search for a node's json constant. If we find only one constant in the
" searchable section then that must be the node's searchable counterpart. If
" this search fails then we'll have to do it manually. I believe that all the
" functions in this section, before the data structure, were strictly used to
" help build this data structure.

" Requires that the candidat.xml or poste.xml file is being edited.
" Returns the search path for a node given the path to the node. If
" candidat.xml was being editied this function will also open a split
" containing the recherche.xml file. If a search path is found, this split
" will remain open. However, if no search path is found then this function
" will close that split.

" Get's the search path for a node added on the candidate form
function! GetSearchPathCand(node_name, json_constant)
    call cursor(1, 1)
    " Count how many times we find the constant
    let matching_lines = []
    "let my_count = 0
    "execute 'silent .,$global /\<' . a:json_constant . '\>/let my_count += 1'
    execute 'silent global /\<' . a:json_constant . '\>/call add(matching_lines, line("."))'
    " We'll be left in the right spot
    "if my_count ==# 1
    if len(matching_lines) ==# 1
        let path = GetPathToXMLNode()
        return path
    else
        " If there's only one node with a matching name, we take that one.
        let num_node_names = 0
        for i in matching_lines
            call cursor(i, 1)
            if GetNodeName() ==# a:node_name
                let num_node_names += 1
                let path = GetPathToXMLNode()
            endif
        endfor
        if num_node_names ==# 1
            return path
        else
            return ""
        endif
    endif
endfunction

" Get's the search path for a node added on the requisition form
function! GetSearchPathPoste(node_name, json_constant)
    call LuceoFindNode("recherche")
    " Count how many times we find the constant
    "let my_count = 0
    let matching_lines = []
    "execute 'silent .,$global /\<' . a:json_constant . '\>/let my_count += 1'
    execute 'silent .,$global /\<' . a:json_constant . '\>/call add(matching_lines, line("."))'
    " We'll be left in the right spot
    "if my_count ==# 1
    if len(matching_lines) ==# 1
        let path = substitute(GetPathToXMLNode(), "[^/]*/", "", "")
        return path
    else
        " If there's only one node with a matching name, we take that one.
        let num_node_names = 0
        for i in matching_lines
            call cursor(i, 1)
            if GetNodeName() ==# a:node_name
                let num_node_names += 1
                let path = substitute(GetPathToXMLNode(), "[^/]*/", "", "")
            endif
        endfor
        if num_node_names ==# 1
            return path
        else
            return ""
        endif
    endif
endfunction

function! GetSearchPathAndOpenSplit2(path_to_node, cand_or_poste)
    call LuceoFindNode(a:path_to_node)
    let node_name = GetNodeName()
    let json_constant = GetNodeAttribute('libelle')
    if a:cand_or_poste
        execute "split ".GetXmlFileLocation()."recherche.xml"
        let result = GetSearchPathCand(node_name, json_constant) 
    else
        " TODO: I added this code which splits to poste.xml 'cause I had other
        " code closing the split of nothing could be found. I used to not do a
        " split poste because it was splitting on the same file. But I
        " actually kind of like it now. For 1 it's consistant with the name of
        " this function AND you'll get to see the change imediately. Even so,
        " put some thought into thinking of another way to do this. Also, look
        " into if this messes anything up.
        split
        let result =  GetSearchPathPoste(node_name, json_constant)
    endif
    " Close the open split if no search path was found.
    if result ==# ""
        quit
    endif
    return result
endfunction

" This is the function I made automate what I could of filling out the
" 'search_node_path' field of the data structures below.
function! HelperWriteSearchNodePath()
    let cur_line = getline('.')
    return substitute(cur_line, '\v.*: "([^,]*)",', '\1', '')
endfunction

function! WriteSearchNodePath(cand_or_poste)
    let all_node_paths = []
    let all_node_search_paths = []
    'y,'zglobal/"node_path"/call add(all_node_paths, HelperWriteSearchNodePath())
    for i in all_node_paths
        "split candidat.xml
        split poste.xml
        let search_path = GetSearchPathAndOpenSplit(i, a:cand_or_poste)
        if search_path !=# ''
            wincmd c
        endif
        wincmd c
        call add(all_node_search_paths, search_path)
    endfor
    let i = 0
    'y,'zglobal/"search_node_path"/execute 's:"":"' . all_node_search_paths[i] . '"' | let i += 1
endfunction

" All (most) of the candidat.xml fields.
" TODO: Add the repeatable sections for the 'concours' and 'poste-envisiage'
" repeatable sections.
" Date fields
"        \ {
"            \ "reusable" : 1,
"            \ "field_category" : 0,
"            \ "node_path" : "mairie/debut-rech2",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 1,
"            \ "field_category" : 0,
"            \ "node_path" : "mairie/debut-rech3",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 1,
"            \ "field_category" : 0,
"            \ "node_path" : "mairie/fin-rech2",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 1,
"            \ "field_category" : 0,
"            \ "node_path" : "mairie/fin-rech3",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },

" Select fields? Wasn't sure what to do with these 'langue' fields because
" I've never really touched them myself.
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue1",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue10",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue11",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue12",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue13",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue14",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue15",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue2",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue3",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue4",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue5",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue6",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue7",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue8",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langue9",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "profil/langues/langues-liste",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },

" NOT SURE WHERE TO PUT:
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "Experiences/texte",
"            \ "search_node_path" : "zone_texte/suivi-affectation",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "Experiences/dernier-emploi",
"            \ "search_node_path" : "divers/strlibre3",
"            \ "obj" : "",
"        \ },

let g:candidat_nodes =
\ {
    \ "date" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre1",
            \ "search_node_path" : "champ_date/dtlibre1-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre2",
            \ "search_node_path" : "champ_date/dtlibre2-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre3",
            \ "search_node_path" : "champ_date/dtlibre3-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/certificat-travail",
            \ "search_node_path" : "champ_date/certificat-travail-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/debut-contrat",
            \ "search_node_path" : "champ_date/debut-contrat-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/debut-rech",
            \ "search_node_path" : "champ_date/debut-rech-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/demande-mob",
            \ "search_node_path" : "champ_date/demande-mob-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/dispo-debut",
            \ "search_node_path" : "champ_date/dispo-debut-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/dispo-fin",
            \ "search_node_path" : "champ_date/dispo-fin-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/entree",
            \ "search_node_path" : "champ_date/entree-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/fin-contrat",
            \ "search_node_path" : "champ_date/fin-contrat-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/fin-rech",
            \ "search_node_path" : "champ_date/fin-rech-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/visite-med",
            \ "search_node_path" : "champ_date/visite-med-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/date-naissance",
            \ "search_node_path" : "profil/date-naissance",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/date-libre1",
            \ "search_node_path" : "qualification/date-libre1-debut",
            \ "obj" : "dtQualifLibre1@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/date-libre2",
            \ "search_node_path" : "qualification/date-libre2-debut",
            \ "obj" : "dtQualifLibre2@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/date-libre3",
            \ "search_node_path" : "qualification/date-libre3-debut",
            \ "obj" : "dtQualifLibre3@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/date-libre1",
            \ "search_node_path" : "experiences/date-libre1-debut",
            \ "obj" : "dtLibre1_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/date-libre2",
            \ "search_node_path" : "experiences/date-libre2-debut",
            \ "obj" : "dtLibre2_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/date-libre3",
            \ "search_node_path" : "experiences/date-libre3-debut",
            \ "obj" : "dtLibre3_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/date-libre4",
            \ "search_node_path" : "experiences/date-libre4-debut",
            \ "obj" : "dtLibre4_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/date-libre5",
            \ "search_node_path" : "experiences/date-libre5-debut",
            \ "obj" : "dtLibre5_@",
        \ },
    \ ],
    \ "select" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/categorie",
            \ "search_node_path" : "divers/categorie",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/concours",
            \ "search_node_path" : "divers/concours",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/critere-sociaux",
            \ "search_node_path" : "divers/critere-sociaux",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/formation-complementaire",
            \ "search_node_path" : "divers/formation-complementaire",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/function-publique",
            \ "search_node_path" : "divers/formation-complementaire",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/motif-mobilite",
            \ "search_node_path" : "divers/motif-mobilite",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/motif-reintegration",
            \ "search_node_path" : "divers/motif-reintegration",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/nom-diplome",
            \ "search_node_path" : "divers/nom-diplome",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/permis-conduire",
            \ "search_node_path" : "divers/permis-conduire",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/situation-editable",
            \ "search_node_path" : "divers/situation-editable",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/situation-famille",
            \ "search_node_path" : "divers/situation-famille",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/specialite",
            \ "search_node_path" : "divers/specialite",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/statut",
            \ "search_node_path" : "divers/statut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/temps-travail",
            \ "search_node_path" : "divers/temps-travail",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/type-emploi",
            \ "search_node_path" : "profil/type-emploi",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/type-temps-travail",
            \ "search_node_path" : "divers/type-temps-travail",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "situation-actuelle/liste-editable1",
            \ "search_node_path" : "recherche_referentiel/liste-editable1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "situation-actuelle/liste-editable2",
            \ "search_node_path" : "recherche_referentiel/liste-editable2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "situation-actuelle/liste-editable3",
            \ "search_node_path" : "recherche_referentiel/liste-editable3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "situation-recherchee/liste-editable1",
            \ "search_node_path" : "poste-envisage/liste-editable1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "situation-recherchee/liste-editable2",
            \ "search_node_path" : "poste-envisage/liste-editable2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "situation-recherchee/liste-editable3",
            \ "search_node_path" : "poste-envisage/liste-editable3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "divers/disponibilite",
            \ "search_node_path" : "divers/disponibilite",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langue-maternelle",
            \ "search_node_path" : "profil/langue-maternelle",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/niveau-etudes",
            \ "search_node_path" : "profil/niveau-etudes",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/niveau-experience",
            \ "search_node_path" : "profil/niveau-experience",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "situation-actuelle/type-contrat",
            \ "search_node_path" : "recherche_referentiel/type-contrat",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "situation-recherchee/type-contrat",
            \ "search_node_path" : "recherche_referentiel/type-contrat",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/liste-editable1",
            \ "search_node_path" : "qualification/liste-editable1",
            \ "obj" : "lstQualifEditable1@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/liste-editable2",
            \ "search_node_path" : "qualification/liste-editable2",
            \ "obj" : "lstQualifEditable2@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/liste-editable3",
            \ "search_node_path" : "qualification/liste-editable3",
            \ "obj" : "lstQualifEditable3@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/nom-diplome",
            \ "search_node_path" : "qualification/nom-diplome",
            \ "obj" : "lstNomDiplome@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 1,
            \ "node_path" : "qualification/niveau-diplome",
            \ "search_node_path" : "qualification/niveau-diplome",
            \ "obj" : "lstNiveauDiplome@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/liste-editable1",
            \ "search_node_path" : "experiences/liste-editable1",
            \ "obj" : "lstListeEditable1@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/liste-editable2",
            \ "search_node_path" : "experiences/liste-editable2",
            \ "obj" : "lstListeEditable2@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/liste-editable3",
            \ "search_node_path" : "experiences/liste-editable3",
            \ "obj" : "lstListeEditable3@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/liste-editable4",
            \ "search_node_path" : "experiences/liste-editable4",
            \ "obj" : "lstListeEditable4@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/liste-editable5",
            \ "search_node_path" : "experiences/liste-editable5",
            \ "obj" : "lstListeEditable5@",
        \ },
    \ ],
    \ "integer" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre1",
            \ "search_node_path" : "divers/ilibre1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre2",
            \ "search_node_path" : "divers/ilibre2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre3",
            \ "search_node_path" : "divers/ilibre3",
            \ "obj" : "",
        \ },
    \ ],
    \ "long" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/absence",
            \ "search_node_path" : "zone_texte/absence",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/affectation-mob",
            \ "search_node_path" : "zone_texte/affectation-mob",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/bilan",
            \ "search_node_path" : "zone_texte/bilan",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/contre-indication-integr",
            \ "search_node_path" : "zone_texte/contre-indication-integr",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/evaluation",
            \ "search_node_path" : "zone_texte/evaluation",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/motif-demande-mob",
            \ "search_node_path" : "zone_texte/motif-demande-mob",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/observation-rech",
            \ "search_node_path" : "zone_texte/observation-rech",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/suivi-affectation",
            \ "search_node_path" : "zone_texte/suivi-affectation",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/experience",
            \ "search_node_path" : "zone_texte/cantine-experience",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/specialite",
            \ "search_node_path" : "zone_texte/cantine-specialite",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "mairie/restricted-med",
            \ "search_node_path" : "zone_texte/suivi-affectation",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/competences-texte",
            \ "search_node_path" : "zone_texte/competences-texte",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/formation-texte",
            \ "search_node_path" : "zone_texte/formation-texte",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 1,
            \ "node_path" : "qualification/descriptif",
            \ "search_node_path" : "qualification/descriptif",
            \ "obj" : "txtQualifDescriptif@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte1",
            \ "search_node_path" : "",
            \ "obj" : "txtTexte1_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte2",
            \ "search_node_path" : "",
            \ "obj" : "txtTexte2_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte3",
            \ "search_node_path" : "",
            \ "obj" : "txtTexte3_@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 2,
            \ "node_path" : "experiences/descriptif",
            \ "search_node_path" : "experiences/descriptif",
            \ "obj" : "txtDescriptif@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/commentaire",
            \ "search_node_path" : "zone_texte/commentaire",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/motivations",
            \ "search_node_path" : "zone_texte/motivations",
            \ "obj" : "",
        \ },
    \ ],
    \ "short" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/age-enfant",
            \ "search_node_path" : "texte/age-enfant",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/an-mob",
            \ "search_node_path" : "texte/an-mob",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/code-mob",
            \ "search_node_path" : "texte/code-mob",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/competence-acquise",
            \ "search_node_path" : "texte/competence-acquise",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/contre-indication-med",
            \ "search_node_path" : "texte/contre-indication-med",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/contre-indication-tech",
            \ "search_node_path" : "texte/contre-indication-tech",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/dep-naissance",
            \ "search_node_path" : "texte/dep-naissance",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/employeur-actu",
            \ "search_node_path" : "texte/employeur-actu",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/horaire-actu",
            \ "search_node_path" : "texte/horaire-actu",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/horaire-rech",
            \ "search_node_path" : "texte/horaire-rech",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/horaire-rech2",
            \ "search_node_path" : "texte/horaire-rech2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/horaire-rech3",
            \ "search_node_path" : "texte/horaire-rech3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/lieu-naissance",
            \ "search_node_path" : "texte/lieu-naissance",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/nationalite",
            \ "search_node_path" : "texte/nationalite",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/nb-enfant",
            \ "search_node_path" : "texte/nb-enfant",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/nom-patronymique",
            \ "search_node_path" : "texte/nom-patronymique",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/nom-recommandeur",
            \ "search_node_path" : "texte/nom-recommandeur",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/observation-mob",
            \ "search_node_path" : "texte/observation-mob",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/observation-rech2",
            \ "search_node_path" : "texte/observation-rech2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/observation-rech3",
            \ "search_node_path" : "texte/observation-rech3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/poste-envisage",
            \ "search_node_path" : "texte/poste-envisage",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/poste-envisage2",
            \ "search_node_path" : "texte/poste-envisage2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/poste-envisage3",
            \ "search_node_path" : "texte/poste-envisage3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/reclassement-med",
            \ "search_node_path" : "texte/reclassement-med",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/service-accueil",
            \ "search_node_path" : "texte/service-accueil",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/situation",
            \ "search_node_path" : "texte/situation",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/taux-actu",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/taux-rech",
            \ "search_node_path" : "texte/taux-rech",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/taux-rech2",
            \ "search_node_path" : "texte/taux-rech2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/taux-rech3",
            \ "search_node_path" : "texte/taux-rech3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre1",
            \ "search_node_path" : "divers/strlibre1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre2",
            \ "search_node_path" : "divers/strlibre2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre3",
            \ "search_node_path" : "divers/strlibre3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/annee-recrutement",
            \ "search_node_path" : "texte/cantine-annee-recrutement",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/duree",
            \ "search_node_path" : "texte/duree",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "situation-recherchee/heures",
            \ "search_node_path" : "texte/heures-rech",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/adresse1",
            \ "search_node_path" : "identite/adresse1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/adresse2",
            \ "search_node_path" : "identite/adresse2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/code-postal",
            \ "search_node_path" : "identite/code-postal",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/fax",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/mail",
            \ "search_node_path" : "identite/mail",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/nom",
            \ "search_node_path" : "identite/nom",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/pays",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/reference",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/tel-fixe",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/tel-portable",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/tel-pro",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/titre",
            \ "search_node_path" : "identite/titre",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/ville",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/fonction",
            \ "search_node_path" : "texte/cantine-fonction",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/ecole",
            \ "search_node_path" : "profil/ecole",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/formation",
            \ "search_node_path" : "profil/formation",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre1",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre10",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre2",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre3",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre4",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre5",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre6",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre7",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre8",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/langues/libre9",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "situation-actuelle/remuneration",
            \ "search_node_path" : "texte/remuneration-actu",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "situation-recherchee/remuneration",
            \ "search_node_path" : "texte/remuneration-rech",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "identite/telephones",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/mention",
            \ "search_node_path" : "qualification/mention",
            \ "obj" : "txtMention@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 1,
            \ "node_path" : "qualification/date-obtention",
            \ "search_node_path" : "qualification/date-obtention",
            \ "obj" : "txtDateObtention@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 1,
            \ "node_path" : "qualification/diplome",
            \ "search_node_path" : "qualification/diplome",
            \ "obj" : "txtQualifDiplome@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 1,
            \ "node_path" : "qualification/intitule",
            \ "search_node_path" : "qualification/intitule",
            \ "obj" : "txtIntitule@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 1,
            \ "node_path" : "qualification/specialites",
            \ "search_node_path" : "qualification/specialites",
            \ "obj" : "txtQualifSpecialite@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre1",
            \ "search_node_path" : "experiences/texte-libre1",
            \ "obj" : "txtLibre1_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre10",
            \ "search_node_path" : "experiences/texte-libre10",
            \ "obj" : "txtLibre10_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre2",
            \ "search_node_path" : "experiences/texte-libre2",
            \ "obj" : "txtLibre2_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre3",
            \ "search_node_path" : "experiences/texte-libre3",
            \ "obj" : "txtLibre3_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre4",
            \ "search_node_path" : "experiences/texte-libre4",
            \ "obj" : "txtLibre4_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre5",
            \ "search_node_path" : "experiences/texte-libre5",
            \ "obj" : "txtLibre5_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre6",
            \ "search_node_path" : "experiences/texte-libre6",
            \ "obj" : "txtLibre6_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre7",
            \ "search_node_path" : "experiences/texte-libre7",
            \ "obj" : "txtLibre7_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre8",
            \ "search_node_path" : "experiences/texte-libre8",
            \ "obj" : "txtLibre8_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/texte-libre9",
            \ "search_node_path" : "experiences/texte-libre9",
            \ "obj" : "txtLibre9_@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 2,
            \ "node_path" : "experiences/date-debut",
            \ "search_node_path" : "experiences/date-debut",
            \ "obj" : "txtDateDebut@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 2,
            \ "node_path" : "experiences/date-fin",
            \ "search_node_path" : "experiences/date-fin",
            \ "obj" : "txtDateFin@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 2,
            \ "node_path" : "experiences/duree",
            \ "search_node_path" : "experiences/duree",
            \ "obj" : "txtSociete@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 2,
            \ "node_path" : "experiences/fonction",
            \ "search_node_path" : "",
            \ "obj" : "txtDuree@",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 2,
            \ "node_path" : "experiences/societe",
            \ "search_node_path" : "experiences/societe",
            \ "obj" : "txtFonction@",
        \ },
    \ ],
    \ "yesno" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/demande-reintegration",
            \ "search_node_path" : "divers/demande-reintegration",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/liste-aptitude",
            \ "search_node_path" : "divers/liste-aptitude",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/projet-pro-rech",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/reclassement",
            \ "search_node_path" : "divers/reclassement",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/reintegration",
            \ "search_node_path" : "divers/reintegration",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/statutaire",
            \ "search_node_path" : "divers/statutaire",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/titulaire-concours",
            \ "search_node_path" : "divers/titulaire-concours",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/titulaire-mob",
            \ "search_node_path" : "divers/titulaire-mob",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num1",
            \ "search_node_path" : "divers/num1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num2",
            \ "search_node_path" : "divers/num2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num3",
            \ "search_node_path" : "divers/num3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num4",
            \ "search_node_path" : "divers/num4",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num5",
            \ "search_node_path" : "divers/num5",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num6",
            \ "search_node_path" : "divers/num6",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num7",
            \ "search_node_path" : "divers/num7",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num8",
            \ "search_node_path" : "divers/num8",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num9",
            \ "search_node_path" : "divers/num9",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num10",
            \ "search_node_path" : "divers/num10",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/cantine/num11",
            \ "search_node_path" : "divers/num11",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/permis-conduire",
            \ "search_node_path" : "divers/permis-conduire-v2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "divers/handicap",
            \ "search_node_path" : "divers/handicap",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "divers/interne",
            \ "search_node_path" : "divers/interne",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "divers/mobilite-interne",
            \ "search_node_path" : "divers/mobilite-interne",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "profil/en-poste",
            \ "search_node_path" : "profil/en-poste",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 1,
            \ "node_path" : "qualification/acquis",
            \ "search_node_path" : "qualification/acquis",
            \ "obj" : "optAcquis@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/num1",
            \ "search_node_path" : "experiences/num1",
            \ "obj" : "radNum1_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/num2",
            \ "search_node_path" : "experiences/num2",
            \ "obj" : "radNum2_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/num3",
            \ "search_node_path" : "experiences/num3",
            \ "obj" : "radNum3_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/num4",
            \ "search_node_path" : "experiences/num4",
            \ "obj" : "radNum4_@",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 2,
            \ "node_path" : "experiences/num5",
            \ "search_node_path" : "experiences/num5",
            \ "obj" : "radNum5_@",
        \ },
    \ ],
\ }

" All (most) of the poste.xml fields.
" SELECT:
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue1",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue10",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue11",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue12",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue13",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue14",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue15",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue2",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue3",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue4",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue5",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue6",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue7",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue8",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langue9",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/langues-liste",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },

" SHORT:
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre1",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre10",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre2",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre3",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre4",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre5",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre6",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre7",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre8",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },
"        \ {
"            \ "reusable" : 0,
"            \ "field_category" : 0,
"            \ "node_path" : "langues/libre9",
"            \ "search_node_path" : "",
"            \ "obj" : "",
"        \ },

" TODO: I don't have any entries here for referentiel nodes. Like descriptif/departement_geo. Add those in somehow.

let g:poste_nodes =
\ {
    \ "date" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre1",
            \ "search_node_path" : "recherche/supplementaires/dtlibre1-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre2",
            \ "search_node_path" : "recherche/supplementaires/dtlibre2-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre3",
            \ "search_node_path" : "recherche/supplementaires/dtlibre3-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre4",
            \ "search_node_path" : "recherche/supplementaires/dtlibre4-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre5",
            \ "search_node_path" : "recherche/supplementaires/dtlibre5-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre6",
            \ "search_node_path" : "recherche/supplementaires/dtlibre6-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre7",
            \ "search_node_path" : "recherche/supplementaires/dtlibre7-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre8",
            \ "search_node_path" : "recherche/supplementaires/dtlibre8-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre9",
            \ "search_node_path" : "recherche/supplementaires/dtlibre9-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre10",
            \ "search_node_path" : "recherche/supplementaires/dtlibre10-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre11",
            \ "search_node_path" : "recherche/supplementaires/dtlibre11-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre12",
            \ "search_node_path" : "recherche/supplementaires/dtlibre12-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre13",
            \ "search_node_path" : "recherche/supplementaires/dtlibre13-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre14",
            \ "search_node_path" : "recherche/supplementaires/dtlibre14-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/dtlibre15",
            \ "search_node_path" : "recherche/supplementaires/dtlibre15-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/date_delai",
            \ "search_node_path" : "recherche/date_delai-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/date_emission",
            \ "search_node_path" : "recherche/date_emission-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/signature",
            \ "search_node_path" : "recherche/signature-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/date-deliberation",
            \ "search_node_path" : "recherche/date-deliberation-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/date-prepositionnement",
            \ "search_node_path" : "recherche/date-prepositionnement-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/date-validation1",
            \ "search_node_path" : "recherche/date-validation1-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/date-validation2",
            \ "search_node_path" : "recherche/date-validation2-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/date-validation3",
            \ "search_node_path" : "recherche/date-validation3-debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/date_debut",
            \ "search_node_path" : "recherche/divers/date_debut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/date_fin",
            \ "search_node_path" : "recherche/divers/date_fin",
            \ "obj" : "",
        \ },
    \ ],
    \ "contact" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/contact",
            \ "search_node_path" : "recherche/descriptif/contact",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/contact2",
            \ "search_node_path" : "recherche/descriptif/contact2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/contact3",
            \ "search_node_path" : "recherche/descriptif/contact3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/contact4",
            \ "search_node_path" : "recherche/descriptif/contact4",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/contact5",
            \ "search_node_path" : "recherche/descriptif/contact5",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/contact-client",
            \ "search_node_path" : "recherche/divers/contact-client",
            \ "obj" : "",
        \ },
    \ ],
    \ "select" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable1",
            \ "search_node_path" : "recherche/descriptif/liste-editable1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable2",
            \ "search_node_path" : "recherche/descriptif/liste-editable2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable3",
            \ "search_node_path" : "recherche/descriptif/liste-editable3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable4",
            \ "search_node_path" : "recherche/descriptif/liste-editable4",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable5",
            \ "search_node_path" : "recherche/descriptif/liste-editable5",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable6",
            \ "search_node_path" : "recherche/descriptif/liste-editable6",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable7",
            \ "search_node_path" : "recherche/descriptif/liste-editable7",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable8",
            \ "search_node_path" : "recherche/descriptif/liste-editable8",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable9",
            \ "search_node_path" : "recherche/descriptif/liste-editable9",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable10",
            \ "search_node_path" : "recherche/descriptif/liste-editable10",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable11",
            \ "search_node_path" : "recherche/descriptif/liste-editable11",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable12",
            \ "search_node_path" : "recherche/descriptif/liste-editable12",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable13",
            \ "search_node_path" : "recherche/descriptif/liste-editable13",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/liste-editable14",
            \ "search_node_path" : "recherche/descriptif/liste-editable14",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/section-analytique",
            \ "search_node_path" : "recherche/section-analytique",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/statut_salarie",
            \ "search_node_path" : "recherche/divers/statut_salarie",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/budget",
            \ "search_node_path" : "recherche/divers/budget",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/domaine-exp",
            \ "search_node_path" : "recherche/divers/domaine-exp",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/type-emploi",
            \ "search_node_path" : "recherche/descriptif/type-emploi",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/motif-remplacement",
            \ "search_node_path" : "recherche/motif-remplacement",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/niveau_experience",
            \ "search_node_path" : "recherche/divers/niveau_experience",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/type_contrat",
            \ "search_node_path" : "recherche/descriptif/type_contrat",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "mairie/categorie",
            \ "search_node_path" : "recherche/descriptif/categorie",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "qualification/niveau_etude",
            \ "search_node_path" : "recherche/divers/niveau_etude",
            \ "obj" : "",
        \ },
    \ ],
    \ "user" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement2",
            \ "search_node_path" : "recherche/divers/utilisateur2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement3",
            \ "search_node_path" : "recherche/divers/utilisateur3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement4",
            \ "search_node_path" : "recherche/divers/utilisateur4",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement5",
            \ "search_node_path" : "recherche/divers/utilisateur5",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement6",
            \ "search_node_path" : "recherche/divers/utilisateur6",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement7",
            \ "search_node_path" : "recherche/divers/utilisateur7",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement8",
            \ "search_node_path" : "recherche/divers/utilisateur8",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement9",
            \ "search_node_path" : "recherche/divers/utilisateur9",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement10",
            \ "search_node_path" : "recherche/divers/utilisateur10",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/responsable",
            \ "search_node_path" : "recherche/divers/responsable",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "divers/charge-recrutement",
            \ "search_node_path" : "recherche/divers/utilisateur",
            \ "obj" : "",
        \ },
    \ ],
    \ "integer" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre1",
            \ "search_node_path" : "recherche/supplementaires/ilibre1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre2",
            \ "search_node_path" : "recherche/supplementaires/ilibre2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre3",
            \ "search_node_path" : "recherche/supplementaires/ilibre3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre4",
            \ "search_node_path" : "recherche/supplementaires/ilibre4",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre5",
            \ "search_node_path" : "recherche/supplementaires/ilibre5",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre6",
            \ "search_node_path" : "recherche/supplementaires/ilibre6",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre7",
            \ "search_node_path" : "recherche/supplementaires/ilibre7",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre8",
            \ "search_node_path" : "recherche/supplementaires/ilibre8",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre9",
            \ "search_node_path" : "recherche/supplementaires/ilibre9",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre10",
            \ "search_node_path" : "recherche/supplementaires/ilibre10",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre11",
            \ "search_node_path" : "recherche/supplementaires/ilibre11",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre12",
            \ "search_node_path" : "recherche/supplementaires/ilibre12",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre13",
            \ "search_node_path" : "recherche/supplementaires/ilibre13",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre14",
            \ "search_node_path" : "recherche/supplementaires/ilibre14",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/ilibre15",
            \ "search_node_path" : "recherche/supplementaires/ilibre15",
            \ "obj" : "",
        \ },
    \ ],
    \ "long" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/commentaire1",
            \ "search_node_path" : "recherche/commentaire1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/commentaire2",
            \ "search_node_path" : "recherche/commentaire2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/commentaire3",
            \ "search_node_path" : "recherche/commentaire3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/commentaire4",
            \ "search_node_path" : "recherche/commentaire4",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "divers/commentaire5",
            \ "search_node_path" : "recherche/commentaire5",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/indemnite_divers",
            \ "search_node_path" : "recherche/indemnite_divers",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/indemnite_logement",
            \ "search_node_path" : "recherche/indemnite_logement",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/indemnite_repas",
            \ "search_node_path" : "recherche/indemnite_repas",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/indemnite_transport",
            \ "search_node_path" : "recherche/indemnite_transport",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/motif_recrutement_surcroitactivite",
            \ "search_node_path" : "recherche/motif_recrutement_surcroitactivite",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/responsabilites",
            \ "search_node_path" : "recherche/responsabilites",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/domaine",
            \ "search_node_path" : "recherche/domaine",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/prepos-commentaire",
            \ "search_node_path" : "recherche/prepos-commentaire",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "qualification/commentaires",
            \ "search_node_path" : "recherche/commentaires",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "qualification/competences",
            \ "search_node_path" : "recherche/competences",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "qualification/formation",
            \ "search_node_path" : "recherche/formation",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "qualification/specialites",
            \ "search_node_path" : "recherche/specialites",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "qualification/whyopen",
            \ "search_node_path" : "recherche/whyopen",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-long-lienref1",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-long-lienref2",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-long-lienref3",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-long-lienref4",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-long-lienref5",
            \ "search_node_path" : "",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/contexte",
            \ "search_node_path" : "recherche/contexte",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/mission",
            \ "search_node_path" : "recherche/mission",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/temoignage",
            \ "search_node_path" : "recherche/temoignage",
            \ "obj" : "",
        \ },
    \ ],
    \ "short" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre1",
            \ "search_node_path" : "recherche/supplementaires/strlibre1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre2",
            \ "search_node_path" : "recherche/supplementaires/strlibre2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre3",
            \ "search_node_path" : "recherche/supplementaires/strlibre3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre4",
            \ "search_node_path" : "recherche/supplementaires/strlibre4",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre5",
            \ "search_node_path" : "recherche/supplementaires/strlibre5",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre6",
            \ "search_node_path" : "recherche/supplementaires/strlibre6",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre7",
            \ "search_node_path" : "recherche/supplementaires/strlibre7",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre8",
            \ "search_node_path" : "recherche/supplementaires/strlibre8",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre9",
            \ "search_node_path" : "recherche/supplementaires/strlibre9",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre10",
            \ "search_node_path" : "recherche/supplementaires/strlibre10",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre11",
            \ "search_node_path" : "recherche/supplementaires/strlibre11",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre12",
            \ "search_node_path" : "recherche/supplementaires/strlibre12",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre13",
            \ "search_node_path" : "recherche/supplementaires/strlibre13",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre14",
            \ "search_node_path" : "recherche/supplementaires/strlibre14",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "supplementaires/strlibre15",
            \ "search_node_path" : "recherche/supplementaires/strlibre15",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/probabilite",
            \ "search_node_path" : "recherche/probabilite",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/duree_jour_max",
            \ "search_node_path" : "recherche/duree_jour_max",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/duree_jour_min",
            \ "search_node_path" : "recherche/duree_jour_min",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/duree_mois_max",
            \ "search_node_path" : "recherche/duree_mois_max",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/duree_mois_min",
            \ "search_node_path" : "recherche/duree_mois_min",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/montant",
            \ "search_node_path" : "recherche/montant",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/prix_jour_max",
            \ "search_node_path" : "recherche/prix_jour_max",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/prix_jour_min",
            \ "search_node_path" : "recherche/prix_jour_min",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/heures",
            \ "search_node_path" : "recherche/heures",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/heures-hebdo",
            \ "search_node_path" : "recherche/heures-hebdo",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-court-lienref1",
            \ "search_node_path" : "recherche/descriptif/libre-court-lienref1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-court-lienref2",
            \ "search_node_path" : "recherche/descriptif/libre-court-lienref2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-court-lienref3",
            \ "search_node_path" : "recherche/descriptif/libre-court-lienref3",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-court-lienref4",
            \ "search_node_path" : "recherche/descriptif/libre-court-lienref4",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/libre-court-lienref5",
            \ "search_node_path" : "recherche/descriptif/libre-court-lienref5",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/nom_salarie_absent",
            \ "search_node_path" : "recherche/nom_salarie_absent",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/regime_travail",
            \ "search_node_path" : "recherche/regime_travail",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/salaire-brut",
            \ "search_node_path" : "recherche/salaire-brut",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/travail-equipe-texte",
            \ "search_node_path" : "recherche/travail-equipe-texte",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/duree",
            \ "search_node_path" : "recherche/duree",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "mairie/num-declaration",
            \ "search_node_path" : "recherche/divers/num-declaration",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/coefficient",
            \ "search_node_path" : "recherche/divers/coefficient",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/intitule",
            \ "search_node_path" : "recherche/descriptif/intitule",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/lieu",
            \ "search_node_path" : "recherche/divers/lieu",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/nb_poste",
            \ "search_node_path" : "recherche/nb_poste",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/reference",
            \ "search_node_path" : "recherche/descriptif/reference",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/salaire-negocie",
            \ "search_node_path" : "recherche/salaire-negocie",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "localisation/adresse1",
            \ "search_node_path" : "recherche/localisation/adresse1",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "localisation/adresse2",
            \ "search_node_path" : "recherche/localisation/adresse2",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "localisation/code-postal",
            \ "search_node_path" : "recherche/localisation/code-postal",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "localisation/latitude",
            \ "search_node_path" : "recherche/localisation/latitude",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "localisation/longitude",
            \ "search_node_path" : "recherche/localisation/longitude",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "localisation/pays",
            \ "search_node_path" : "recherche/localisation/pays",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "localisation/region",
            \ "search_node_path" : "recherche/localisation/region",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 0,
            \ "field_category" : 0,
            \ "node_path" : "localisation/ville",
            \ "search_node_path" : "recherche/localisation/ville",
            \ "obj" : "",
        \ },
    \ ],
    \ "yesno" :
    \ [
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/besoin_avancement",
            \ "search_node_path" : "recherche/besoin_avancement",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/besoin_recurrent",
            \ "search_node_path" : "recherche/besoin_recurrent",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "contexte_commercial/renouvelable",
            \ "search_node_path" : "recherche/renouvelable",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/clause_nonconcurrence",
            \ "search_node_path" : "recherche/clause_nonconcurrence",
            \ "obj" : "",
        \ },
        \ {
            \ "reusable" : 1,
            \ "field_category" : 0,
            \ "node_path" : "descriptif/periode_essai",
            \ "search_node_path" : "recherche/divers/periode-essai",
            \ "obj" : "",
        \ },
    \ ],
\ }

" The above data structures are lists of nodes organized by field type.  Each
" node has properties associeated with it. This function takes a list of nodes
" of and returns a list containing a specific property of all nodes that match
" some condition. In practice, the use of this function will be to return a
" list of node paths or a list of nodes's searchable paths.
function! GetListOfFieldAttributes(list_of_nodes, node_field, condition, field_to_return)
    let result = []
    for node in a:list_of_nodes
        if node[a:node_field] ==# a:condition
            call add(result, node[a:field_to_return])
        endif
    endfor
    return result
endfunction

" Given a node's path, returns the field type of that node.
function! GetNodeFieldType(field_data_structure, node_path)
    for field_type in keys(a:field_data_structure)
        for field in a:field_data_structure[field_type]
            if field['node_path'] ==# a:node_path
                return field_type
            endif
        endfor
    endfor
    return ""
endfunction

" Does a brute force search through the above data structure for the search
" path associated with a node.
function! GetSearchPath(field_data_structure, node_path)
    for field_type in keys(a:field_data_structure)
        for field in a:field_data_structure[field_type]
            if field['node_path'] ==# a:node_path
                return field['search_node_path']
            endif
        endfor
    endfor
    return ""
endfunction

" I'm slowly getting all my functions to do things based off these new field
" data structures. This one replaces the function that I had originally.
function! GetSearchPathAndOpenSplit(path_to_node, cand_or_poste)
    let search_path = ""
    if a:cand_or_poste
        let search_path = GetSearchPath(g:candidat_nodes, a:path_to_node)
        execute "split ".GetXmlFileLocation()."recherche.xml"
    else
        let search_path = GetSearchPath(g:poste_nodes, a:path_to_node)
        split
    endif
    if search_path ==# ""
        quit
    endif
    return search_path
endfunction

" }}}

" Adding/Modifying Fields {{{

" Assumes you are in the json file. Changes the label attached to a constant.

" TODO: Look into running translator:merge if the constant cannot be found.

" Sometimes in the json file, a constant points to another constant, which
" points to another, etc... This makes it tricky to find the REAL label that
" we need to change. My current solution is sort of a hack. I noticed that all
" constants start with TXT. So I make a check, if TXT occurrs 2 times on a
" single line then a constant must be pointing to another constant. This is a
" total hack because if a label actually has the text "TXT" in it then this
" function won't work.
function! ChangeLabel(constant_to_find, new_label)
    " Move to the top of the file
    call cursor(1, 1)
    let not_found = 1
    let to_find = a:constant_to_find
    while not_found
        " Look for the constant
        if search('\<' . to_find . '\>')
            " If we don't see two TXT's then it's safe to change the label
            if getline('.') !~# "TXT.*TXT"
                " Update the label with the new text
                " ERROR: Really weird vim error. If The line below ends with
                " '",' instead of '"' like it is now, the '%' command doesn't
                " work properly.
                let replacement_line = matchstr(getline('.'), '[^"]*"[^"]*"[^"]*') . '"' . a:new_label . '"' . ','
                call setline('.', replacement_line)
                " Break out of the loop
                let not_found = 0
            " Otherwise we grab the other constant and search for THAT
            else
                normal! $
                call search("TXT", 'b', line('.'))
                let save_unnamed_register = @@
                normal! yiw
                let to_find = @@
                let @@ = save_unnamed_register
            endif
        else
            " Let the user know of any errors
            echoerr "Could not find the label: " . to_find
            " Break out of the loop
            let not_found = 0
        endif
    endwhile
endfunction

" Echo's the label for a xml node if you're in the CDATA OR at the node
" itself. Useful if one of us didn't add the label for a node in the CDATA.
" Check out this page
" http://vim.wikia.com/wiki/Avoid_scrolling_when_switch_buffers for more about
" saving and restoring window views. Or see this:
" http://stackoverflow.com/questions/4251533/vim-keep-window-position-when-switching-buffers
function! GetLabelNameWrapper()
    let save_view = winsaveview()
    if getline('.') !~# 'TXT'
        call LuceoFindNode(GetNodePathFromCDATA(line('.')))
    endif
    if getline('.') =~# 'TXT'
        normal! 0
        call search('TXT', '', line('.'))
        let save_unnamed_register = @@
        normal! yiw
        " I have 'hidden' set so this isn't necessary anyway. But I want to
        " test this to make sure it does what I think it should (i.e hide the
        " current buffer and edit the json file. Then when I return to the
        " first file it will unhide it.
        execute "silent hide edit ".GetTranslationsFileLocation()."php.en.json"
        let label = GetLabelName(@@)
        " If I edit a new file AND echo a message, it prompts me to hit enter
        " after the echo message. I think it happens because both actions
        " print something to screen or something like that. For example if I
        " run the command :echo 'no' | echo 'yes' then I will be asked to hit
        " enter, but If I just do :echo 'no' then I am not prompted. Since we
        " don't want to hit enter so we run this command silently. We run the
        " above buffer switch silently for the same reason.
        silent edit #
        if label !=# ''
            echo label
        else
            " We couldn't find a label, print error message.
            echohl ErrorMsg
            echo "Could not find the label: ".@@
            echohl None
        endif
        let @@ = save_unnamed_register
    endif
    call winrestview(save_view)
endfunction

" Does the main work of finding the json label's value. It has the same sort
" of loop structure as the ChangeLabel() function.
function! GetLabelName(label_constant)
    " Move to the top of the file
    call cursor(1, 1)
    let to_find = a:label_constant
    while search('\<' . to_find . '\>')
        " If we don't see two TXT's then we've found our label
        if getline('.') !~# "TXT.*TXT"
            " Return the label to echo out.
            return matchstr(getline('.'), '^[^"]*"[^"]*"[^"]*"\zs[^"]*')
        else
            " Otherwise we grab the other constant and search for THAT
            normal! $
            call search("TXT", 'b', line('.'))
            let save_unnamed_register = @@
            normal! yiw
            let to_find = @@
            let @@ = save_unnamed_register
        endif
    endwhile
    return ''
endfunction

" Returns the proper amount of spacing for the comment string.
function! GetProperIndentationHelper(pattern, offset, line_no)
    call cursor(a:line_no, 1)
    if search(a:pattern, '', line('.'))
        return repeat(' ', virtcol('.') - a:offset - 1)
    else
        return ""
    endif
endfunction

" Returns a string that attempts to be a properly indented comment string with
" respect to neighboring comment strings. The main inspiration for this was to
" get a properly indented comment string when using my mappings which add a
" field, but it could also be used when changing the label in the comment
" strings in menu.xml. 
function! GetProperIndentation(pattern, offset, line_no)
    let save_pos = getpos('.')
    let num_surrounding_lines = 2
    let spacing = ""
    " I do call reverse() because it seems that looking downwards first for
    " matching indentation generally has better results.
    for i in reverse(range(- num_surrounding_lines, num_surrounding_lines))
        let spacing = GetProperIndentationHelper(a:pattern, a:offset, a:line_no + i)
        if spacing !=# ""
            break
        endif
    endfor
    call setpos('.', save_pos)
    return spacing
endfunction

" Adds a field to CDATA on the line below "line_no"
function! AddCDATAField(line_no, contents, label)
    let indent = GetProperIndentation('[', 0, a:line_no)
    let field = indent . '[' . a:contents . ']'
    let comment = GetProperIndentation('<!--', len(field), a:line_no) . '<!-- ' . a:label . ' -->'
    call append(a:line_no, field . comment)
endfunction

" Changes the comment string, like the ones in CDATA to have a new label.
function! ChangeCommentStr(label)
    let pattern = '<!--'
    " Delete the comment and add a new one.
    execute "substitute/\s*" . pattern . ".*//e"
    normal! $
    " TODO: Is there a way to do this 'virtcol()' stuff without having to move
    " the cursor around? Like we could get the current line as a string and
    " there would be a sort of 'strvirtcol()' function. I suppose I couuld
    " just replace all tabs with spaces in the string.
    let comment = GetProperIndentation(pattern, virtcol('.'), line('.')) . '<!-- ' . a:label . ' -->'
    execute "substitute/$/" . escape(comment, '/') . '/e'
endfunction

" Technically this just returns all line numbers where the double quoted
" string 'path' occurs. It's purpose is to return a list of all lines of
" CDATA. This list is sorted highest to lowest.
function! GetCDATALines(path)
    let line_nums = []
    let col_nums = []
    " TODO: Maybe I should just have the a:path argument be the actual CDATA
    " stuff: [champ path...]. That would probably be better.
    execute 'silent global/"' . escape(a:path, '/') . '"/call insert(line_nums, line(".")) | call search("a:path") | call insert(col_nums, col("."))'
    " Removes any line number from line_nums where the pattern occurred in an
    " xml comment.
    let result = []
    let i = 0
    while i <# len(line_nums)
        if synIDattr(synID(line_nums[i], col_nums[i], 1), "name") !=# 'xmlCommentPart'
            call add(result, line_nums[i])
        endif
        let i += 1
    endwhile
    return result
endfunction

" I noticed a pattern whenever I had to make some extra change for a contact
" and select fields so I abstracted that pattern into this function.
function! HandleContactOrSelectChanges(field_type, path, actif_val, new_label)
    if a:field_type ==# "select"
        let attribute = 'type-liste'
        let file = 'menu.xml'
    else
        let attribute = 'type-contact'
        let file = 'correspondance.xml'
    endif
    let file = GetXmlFileLocation().file
    call LuceoFindNode(a:path)
    let attr_val  = GetNodeAttribute(attribute)
    execute "split ".file
    call search('\<' . attr_val . '\>')
    if a:actif_val !=# ""
        call ChangeNodeAttribute("actif", a:actif_val)
    endif
    if a:new_label !=# ""
        call ChangeCommentStr(a:new_label)
    endif
    write
    wincmd p
endfunction

" Changes a node's label
function! ChangeNodeLabel(label, path, cdata_lines, field_type, cand_or_poste)
    " Change the comment string in  all cdata lines in current file.
    for i in a:cdata_lines
        call cursor(i, 1)
        call ChangeCommentStr(a:label)
    endfor
    write
    " Change the label
    call LuceoFindNode(a:path)
    let json_constant = GetNodeAttribute('libelle')
    execute "split ".GetTranslationsFileLocation()."php.en.json"
    call ChangeLabel(json_constant, a:label)
    write
    call RunTranslatorGenerate()
    wincmd p
    " Change the comment string in menu.xml if necessary.
    if a:field_type ==# "select"
        call HandleContactOrSelectChanges(a:field_type, a:path, '', a:label)
    endif
    " Try changing the comment string in the searchable portion.
    let search_path = GetSearchPathAndOpenSplit(a:path, a:cand_or_poste)
    if search_path !=# ""
        let search_cdata_lines = GetCDATALines(search_path)
        if !empty(search_cdata_lines)
            for i in search_cdata_lines
                call cursor(i, 1)
                call ChangeCommentStr(a:label)
            endfor
            write
        else
            " Close the window, there is no searchable portion.
            quit
        endif
    endif
endfunction

" Assumes that the cursor is on a line of CDATA to change.
function! ChangeNodeLabelWrapper()
    let field_type = GetFieldType()
    let label = input("New Label: ")
    let path = GetNodePathFromCDATA(line('.'))
    let cdata_lines = GetCDATALines(path)
    if expand('%:t') ==# "candidat.xml"
        let cand_or_poste = 1
    else
        let cand_or_poste = 0
    endif
    call ChangeNodeLabel(label, path, cdata_lines, field_type, cand_or_poste)
endfunction

" Activates a node, makes it mandatory, and adds it to CDATA
function! ActivateNode(cdata_pos, path, extra_cdata_contents, is_mandatory, label)
    call AddCDATAField(a:cdata_pos, 'champ cle="' . a:path . '"' . a:extra_cdata_contents, a:label)
    call LuceoFindNode(a:path)
    call ChangeNodeAttribute("actif", "true")
    if a:is_mandatory
        call ChangeNodeAttribute("obligatoire", "true")
    endif
    write
endfunction

" Calls the above function to 'activate' the node and also changes the node's label
function! BasicAddField(cdata_pos, path, extra_cdata_contents, is_mandatory, label)
    call ActivateNode(a:cdata_pos, a:path, a:extra_cdata_contents, a:is_mandatory, a:label)
    let json_constant = GetNodeAttribute('libelle')
    execute "split ".GetTranslationsFileLocation()."php.en.json"
    call ChangeLabel(json_constant, a:label)
    write
    call RunTranslatorGenerate()
    wincmd p
endfunction

" Finds a free node if the field is 'regular' (i.e not a dropwdown)
function! FindFreeRegularNode(list_of_paths)
    for path in a:list_of_paths
        " TODO: Check if we were able to even go to such a node.
        call LuceoFindNode(path)
        if GetNodeAttribute('actif') ==# "false"
            return path
        endif
    endfor
    return ""
endfunction

" Another helper for the function to find an available dropdown. It looks at
" all nodes with the specified list type and makes sure that none of them are
" active.
function! FindFreeSelectHelper2(list_type)
    call cursor(1, 1)
    while search('\<' . a:list_type . '\>', 'W')
        if GetNodeAttribute('actif') ==# "true"
            return 0
        endif
        " Move the cursor down one more line
        call cursor(line('.') + 1, 1)
    endwhile
    return 1
endfunction

" A helper for the function to find an available dropdown. It will check
" candidat.xml and poste.xml for any active nodes attached to a list type AND
" will check menu.xml just to make sure it isn't active there either.
function! FindFreeSelectHelper1(list_type)
    " Check candidat.xml and poste.xml
    execute "edit ".GetXmlFileLocation()."candidat.xml"
    let is_available = FindFreeSelectHelper2(a:list_type)
    execute "edit ".GetXmlFileLocation()."poste.xml"
    let is_available2 = FindFreeSelectHelper2(a:list_type)
    " If both files aren't using that list
    if is_available && is_available2
        " Check menu.xml JUST to make sure it's not being used
        execute "edit ".GetXmlFileLocation()."menu.xml"
        call cursor(1, 1)
        call search('\<' . a:list_type . '\>', 'W')
        let actif = GetNodeAttribute('actif')
        if actif ==# "false"
            return 1
        endif
    endif
    return 0
endfunction

" Finds a free menu item.
" TODO: I used to have a cand_or_poste attribute for these functions which I
" removed because I thought I didn't need them. Turns out they were probably
" helping out. Right now I've got it working again with all those 'wincmd'
" statements you see but it feels kind of hacky. Let's try to patch all that
" up.
function! FindFreeSelectNode(list_of_paths)
    " Open up a separate split to do all the searching in.
    split
    let return_path = ""
    for path in a:list_of_paths
        " Search in the buffer we were originally editing (which will be
        " poste.xml or candidat.xml) 
        wincmd p
        call LuceoFindNode(path)
        if GetNodeAttribute('actif') ==# "false"
            let list_type = GetNodeAttribute('type-liste')
            " Go back to the open split to do the searching in
            wincmd p
            if FindFreeSelectHelper1(list_type)
                let return_path = path
                break
            endif
        else
            wincmd p
        endif
    endfor
    " Close the split that was opened
    quit
    return return_path
endfunction

" Returns the path to a node that is available for use
function! FindFreeNode(field_type, list_of_paths)
    if a:field_type ==# "select"
        return FindFreeSelectNode(a:list_of_paths)
    else
        return FindFreeRegularNode(a:list_of_paths)
    endif
endfunction

" Makes a node searchable 
function! MakeFieldSearchable(path_to_node, cand_or_poste, label)
    let search_path = GetSearchPathAndOpenSplit(a:path_to_node, a:cand_or_poste)
    if search_path !=# ""
        let zmark_line_no = line("'z")
        call ActivateNode(zmark_line_no, search_path, "", 0, a:label)
        " Move the 'z mark down one line
        execute (zmark_line_no+1) . "mark z"
        " Go back to previous window
        wincmd p
    else
        echoerr "Could not add the searchable aspect of this field. You will have to do it manually."
    endif
endfunction

" The javascript I wrote to pull the information for me from the wiki:
" var tbody = document.getElementById('lucastest').getElementsByTagName('tbody')[0];
" var rows = tbody.getElementsByTagName('tr');
" var result_arr = [];
" for(var i = 0; i < rows.length; i++) {
"   var columns = rows[i].getElementsByTagName('td');
"   if(columns[3].innerHTML.indexOf("Date") > -1 && columns[6].innerHTML.indexOf("Yes") > -1) {
"     result_arr[result_arr.length] = columns[0].innerHTML.trim();
"   }
" }
" console.log(result_arr);

" This function adds a field. It also tries to make the field searchable.
" TODO: Make the 'is_searchable' argument a string that is the search path of
" the field. The AddWrapper() function will be passing in the search path.
" Let's also do the 'split'ing in this function, lets essentially get rid of
" that MakeFieldSearchable() function altogether.
function! AddField(cand_or_poste, cdata_pos, extra_cdata_contents, field_type, path, is_mandatory, is_searchable, label)
    " Activates the field, makes it mandatory (if necessary), and adds it to
    " CDATA
    call BasicAddField(a:cdata_pos, a:path, a:extra_cdata_contents, a:is_mandatory, a:label)

    " Extra steps must be taken for some types of fields
    call LuceoFindNode(a:path)
    if a:field_type ==# "select"
        call HandleContactOrSelectChanges(a:field_type, a:path, 'true', a:label)
    elseif a:field_type ==# "contact"
        call HandleContactOrSelectChanges(a:field_type, a:path, 'true', '')
    endif

    " Attempt to make the field searchable
    if a:is_searchable
        call MakeFieldSearchable(a:path, a:cand_or_poste, a:label)
    endif
    " Leave the cursor in the cdata where the field was added.
    call cursor(a:cdata_pos + 1, 1)
endfunction

" Gets a valid field type for use when adding/removing fields and such.
function! GetFieldType()
    let valid_types = ["short", "long", "yesno", "date", "select", "contact", "user", "integer"]
    let prompt_str = join(valid_types, "\n")
    let my_count = 0
    let good_input = 0
    while !good_input
        let input = input("The valid field types are:\n" . prompt_str . "\nEnter a field type: ")
        " lowercase the input and strip out leading and trailing whitespace.
        let result = tolower(substitute(input, '^\s*\(\S*\)\s*$', '\1', ''))
        if index(valid_types, result) !=# -1
            let good_input = 1
        else
            if my_count <# 2
                echo "\nNo you fool, that isn't valid. Try again.\n\n"
            else
                echo "\nWhat are you stupid? The list is right there.\n\n"
            endif
            let my_count += 1
        endif
    endwhile
    return result
endfunction

" A wrapper for the function which adds a field. This wrapper will add a
" 'regular' field.
" TODO: Currently this function is BROKEN (sort of). The call to
" GetListOfFieldAttributes() isn't quite right because it will pick up
" repeatable sections as being reusable even though they shouldn't. The calls
" to this function in the AddRepeatableFieldWrapper() aren't quite right
" either because it will pick up fields that aren't reusable.
function! AddFieldWrapper()
    let cdata_line_no = line('.')
    " TODO: Adjust this function so it takes a list of valid types that could
    " be added.
    let field_type = GetFieldType()
    if expand('%:t') ==# "candidat.xml"
        let cand_or_poste = 1
        let available_nodes = GetListOfFieldAttributes(g:candidat_nodes[field_type], 'reusable', 1, 'node_path')
    else
        let cand_or_poste = 0
        let available_nodes = GetListOfFieldAttributes(g:poste_nodes[field_type], 'reusable', 1, 'node_path')
    endif
    let path = FindFreeNode(field_type, available_nodes)
    if path !=# ''
        call AddField(cand_or_poste, cdata_line_no, "", field_type, path, input("Mandatory: "), input("Searchable: "), input("Label: "))
    endif
endfunction

" A wrapper for the function which adds a field. This wrapper adds a
" 'repeatable' field.
function! AddRepeatableFieldWrapper(which_repeatable_section)
    let cdata_line_no = line('.')
    let field_type = GetFieldType()
    if a:which_repeatable_section ==# 0
        let available_paths = GetListOfFieldAttributes(g:candidat_nodes[field_type], 'field_category', 1, 'node_path')
        let available_objs = GetListOfFieldAttributes(g:candidat_nodes[field_type], 'field_category', 1, 'obj')
    else
        let available_paths = GetListOfFieldAttributes(g:candidat_nodes[field_type], 'field_category', 2, 'node_path')
        let available_objs = GetListOfFieldAttributes(g:candidat_nodes[field_type], 'field_category', 2, 'obj')
    endif
    let path = FindFreeNode(field_type, available_paths)
    if path !=# ''
        call AddField(1, cdata_line_no, ' obj="' . available_objs[index(available_paths, path)] . '"', field_type, path, input("Mandatory: "), input("Searchable: "), input("Label: "))
    endif
endfunction

" Assumes the cursor is on the xml node to add and mark 'y has the position in
" the CDATA to add this field.
function! AddSpecificFieldWrapper()
    let path = GetPathToXMLNode()
    " Again the hack where every xml file except candidat includes the root
    " tags in the path. We remove the root tag from the path.
    if expand('%:t') ==# 'candidat.xml'
        let cand_or_poste = 1
        let field_type = GetNodeFieldType(g:candidat_nodes, path)
    else
        let cand_or_poste = 0
        let path = substitute(path, "[^/]*/", "", "")
        let field_type = GetNodeFieldType(g:poste_nodes, path)
    endif
    normal! 'y
    call AddField(cand_or_poste, line("'y"), "", field_type, path, input("Mandatory: "), input("Searchable: "), input("Label: "))
endfunction

" Returns a string representing an available json label. Assumes that the
" php.en.json file is being edited.
function! GetFreeJsonLabel()
    let free_pattern = 'TXT_LIBRE\d\+'
    call cursor(1, 1)
    while search(free_pattern, 'W')
        let save_unnamed_register = @@
        " Go to the constant's value and yank it.
        normal! $F"yi"
        if @@ ==# ""
            " We yank the name of constant
            normal! 0fXyi"
            let return_val = @@
            let @@ = save_unnamed_register
            return return_val
        endif
    endwhile
    return ""
endfunction

" Adds a 'section' tag in the CDATA
function! AddSection(label)
    execute "split ".GetTranslationsFileLocation()."php.en.json"
    let json_constant = GetFreeJsonLabel()
    call ChangeLabel(json_constant, a:label)
    write
    call RunTranslatorGenerate()
    wincmd p
    call AddCDATAField(line('.'), 'section lib="' . json_constant . '" ' . 'width="w3"', a:label)
    write
endfunction

" A wrapper for the above function
function! AddSectionWrapper()
    let label = input('Enter Label: ')
    call AddSection(label)
endfunction

" TODO: Abstract these four functions a little more. We can do this with less
" code I think. Also need to add a mapping for the one below, I want to do
" ,ll but that mapping already renames a field.

" Adds a 'label' tag in the CDATA
function! AddLabel(label)
    execute "split ".GetTranslationsFileLocation()."php.en.json"
    let json_constant = GetFreeJsonLabel()
    call ChangeLabel(json_constant, a:label)
    write
    call RunTranslatorGenerate()
    wincmd p
    call AddCDATAField(line('.'), 'label lib="' . json_constant . '" ' . 'options="titre"', a:label)
    write
endfunction

" A wrapper for the above function
function! AddLabelWrapper()
    let label = input('Enter Label: ')
    call AddLabel(label)
endfunction


" }}}

" Removing Fields {{{

" Removes a 'normal' field by deactivating it and deleting all lines of CDATA
" that reference it.
function! BasicRemoveField(cdata_lines, path)
    " Deactivate the field
    call LuceoFindNode(a:path)
    call ChangeNodeAttribute("actif", "false")
    call ChangeNodeAttribute("obligatoire", "false")
    call ChangeNodeAttribute("liste", "false")
    " Deletes all the CDATA
    for i in a:cdata_lines
        call cursor(i, 1)
        delete
    endfor
    write
endfunction

" Removes a field.
function! RemoveField(cdata_lines, path, field_type, cand_or_poste)
    call BasicRemoveField(a:cdata_lines, a:path)

    " Extra steps must be taken for some types of fields
    call LuceoFindNode(a:path)
    if a:field_type ==# "select"
        call HandleContactOrSelectChanges(a:field_type, a:path, 'false', '')
    elseif a:field_type ==# "contact"
        call HandleContactOrSelectChanges(a:field_type, a:path, 'false', '')
    endif

    let search_path = GetSearchPathAndOpenSplit(a:path, a:cand_or_poste)
    if search_path !=# ""
        let search_cdata_lines = GetCDATALines(search_path)
        if !empty(search_cdata_lines)
            call BasicRemoveField(search_cdata_lines, search_path)
            write
        else
            " Close the split that was opened. No change was needed.
            quit
        endif
    else
        echoerr "Could not remove the searchable aspect of this field (if there is one). You will have to look into it manually."
    endif
endfunction

" Wrapper for the above function.
function! RemoveFieldWrapper()
    " Get the node's path and all lines of CDATA to delete.
    let path = GetNodePathFromCDATA(line('.'))
    let cdata_lines = GetCDATALines(path)
    if expand('%:t') ==# "candidat.xml"
        let field_type = GetNodeFieldType(g:candidat_nodes, path)
        let cand_or_poste = 1
    else
        let field_type = GetNodeFieldType(g:poste_nodes, path)
        let cand_or_poste = 0
    endif
    call RemoveField(cdata_lines, path, field_type, cand_or_poste)
endfunction

" }}}

" Candidate Workflow {{{

function! OrderXMLNodes(str) range
    let num = 0
    for linenum in range(a:firstline, a:lastline)
        let curr_line = getline(linenum)
        if match(curr_line, a:str . '\d\+') !=# -1
            let num += 1
            let replace_line = substitute(curr_line, '\d\+', num, '')
            call setline(linenum, replace_line)
        endif
    endfor
    " Not sure if this is call to "normal!" is necessary, I want to make sure
    " we are on the same line as the <transitions> node so we can change the
    " "nombre" attribute.
    execute "normal! vato\<ESC>"
    let curr_line = getline('.')
    let replace_line = substitute(curr_line, '\d\+', num, '')
    call setline('.', replace_line)
endfunction

" A function that will put the numbers associated with the transition nodes in
" in order.
function! OrderTransitions() range
    let num = 0
    for linenum in range(a:firstline, a:lastline)
        let curr_line = getline(linenum)
        if match(curr_line, '<transition\d\+') !=# -1
            let num += 1
            let replace_line = substitute(curr_line, '\d\+', num, '')
            call setline(linenum, replace_line)
        endif
    endfor
    " Not sure if this is call to "normal!" is necessary, I want to make sure
    " we are on the same line as the <transitions> node so we can change the
    " "nombre" attribute.
    execute "normal! vato\<ESC>"
    let curr_line = getline('.')
    let replace_line = substitute(curr_line, '\d\+', num, '')
    call setline('.', replace_line)
endfunction

" Returns a string of a transition node
function! GenTransitionStr(num, de, vers, icone)
    let trans = "<transition"
    let state = "AFFECTATION_WORKFLOW_ETAT_"
    let rest = 'onaction="" libelle-onaction="" visible=""/>'
    return trans . a:num . ' de="' . state . a:de . '" vers="' . state . a:vers . '" icone="icones/workflow' . a:icone . '.png" ' . rest
endfunction

function! AppendStrHelper(str)
    let line_num = line('.')
    call append(line_num, a:str)
    call cursor(line_num+1, 1)
endfunction

" Right now I assume that state 1 goes to 2, 2 to 3, etc... all the way to the
" accept state. I've only seen it a few times where builds didn't have that
" standard progression, but I'm wondering if I should use a dictionary to
" encode ALL the transitions. That way it could ALWAYS work. But in reality
" it's probably just easier to change the one or two nodes that aren't
" standard. I'll keep this thought in mind though.

" Writes out the transitions for a candidate workflow. 
" skip_logic - A dictionary (i.e. associative array) mapping a state X to a
" list of states that X can go to. You can also specify a scalar instead of a
" list.
function! WriteTransitions(num_states, skip_logic)
    " Constants
    let applied_state = "DEMANDE"
    let sourced_state = "POSITIONNE"
    let accepted_state = "ACCEPTE"
    let reject_state = "NEGATIF"
    let default_up_icon = 6
    let default_skip_icon = 7
    let default_reject_icon = 11
    let trans_num = 1
    " Write all the go standards
    let indent = repeat(' ', indent('.') + &tabstop)
    call AppendStrHelper(indent . '<!-- go standards -->')
    call AppendStrHelper(indent . GenTransitionStr(trans_num, applied_state, 1, default_up_icon))
    let trans_num += 1
    for i in range(1, a:num_states - 1)
        call AppendStrHelper(indent . GenTransitionStr(trans_num, i, i+1, default_up_icon))
        let trans_num += 1
    endfor
    call AppendStrHelper(indent . GenTransitionStr(trans_num, trans_num-1, accepted_state, default_up_icon))
    let trans_num += 1
    call AppendStrHelper('')

    " Write all the skip steps
    call AppendStrHelper(indent . "<!-- saut de puce -->")
    call AppendStrHelper(indent . GenTransitionStr(trans_num, sourced_state, 1, default_up_icon))
    let trans_num += 1
    for key in keys(a:skip_logic)
        if type(a:skip_logic[key]) !=# type([])
            call AppendStrHelper(indent . GenTransitionStr(trans_num, key, a:skip_logic[key], default_skip_icon))
            let trans_num += 1
        else
            for i in a:skip_logic[key]
                call AppendStrHelper(indent . GenTransitionStr(trans_num, key, i, default_skip_icon))
                let trans_num += 1
            endfor
        endif
    endfor
    call AppendStrHelper('')

    " Write all the nogo standards
    call AppendStrHelper(indent . "<!-- nogo standards -->")
    call AppendStrHelper(indent . GenTransitionStr(trans_num, applied_state, reject_state, default_reject_icon))
    let trans_num += 1
    call AppendStrHelper(indent . GenTransitionStr(trans_num, sourced_state, reject_state, default_reject_icon))
    let trans_num += 1
    for i in range(1, a:num_states)
        call AppendStrHelper(indent . GenTransitionStr(trans_num, i, reject_state, default_reject_icon))
        let trans_num += 1
    endfor
    call AppendStrHelper(indent . GenTransitionStr(trans_num, accepted_state, reject_state, default_reject_icon))

    " Change the "nombre" attribute in the transitions node
    execute "normal! vato\<ESC>"
    call ChangeNodeAttribute("nombre", trans_num)
endfunction

" INPUT() RETURNS A STRING WHEN THE FIRST PARAMETER SHOULD BE A NUMBER. BUT
" EVERYTHING IS STILL WORKING FINE FOR SOME REASON?? I'LL HAVE TO LOOK INTO
" IT SOMETIME.

" Wrapper for the above function.
function! WrapperWriteTransitions(num_states)
    execute "normal! diti\<CR>\<ESC>k"
    call WriteTransitions(a:num_states, eval(input("Enter Skip Logic Dictionary: ")))
endfunction

" Returns a string of a workflow state node
function! GenCandWkflStateStr(state, icon_num, onenter_attr)
    let label = "AFFECTATION_WORKFLOW_ETAT_" . a:state
    let cle = ' cle="' . label . '"'
    let libelle = ' libelle="TXT_' . label . '"'
    let icone = ' icone="icones/etat' . a:icon_num . '.png"'
    let onenter = ' onenter="' . a:onenter_attr . '"'
    let onleave = ' onleave=""'
    let libelle_onenter = ' libelle-onenter=""'
    let libelle_onleave = ' libelle-onleave=""'

    return '<etat' . a:state . cle . libelle . icone . onenter . onleave . libelle_onenter . libelle_onleave . '/>'
endfunction

" A function which configures the states in a candidate workflow.
function! WriteCandWkflStates(label_list, icon_dict, onenter_dict)
    " Write the states
    let num_states = len(a:label_list)
    let indent = repeat(' ', indent('.'))
    for i in range(1, num_states)
        call AppendStrHelper(indent . GenCandWkflStateStr(i, get(a:icon_dict, i), get(a:onenter_dict, i, "")))
    endfor

    " Change the attribute of the workflow node
    call GoToParentNode()
    call ChangeNodeAttribute("etats-actifs", "etat" . join(range(1,num_states), ",etat"))
    write

    " Update the labels
    execute "split ".GetTranslationsFileLocation()."php.en.json"
    for i in range(1, num_states)
        call ChangeLabel("TXT_AFFECTATION_WORKFLOW_ETAT_" . i, a:label_list[i-1])
    endfor
    write
    call RunTranslatorGenerate()
    wincmd p
endfunction

" Takes a string representing a color and returns the number of the picture
" that is used for that color. If no match is found, the parameter will
" be returned as is. 
function! MapColorToNumber(color_str)
    let mapping = {
    \ "green" : 1,
    \ "grey" : 2,
    \ "dark blue" : 3,
    \ "yellow" : 4,
    \ "light blue" : 5,
    \ "red" : 6,
    \ "black" : 7,
    \ "purple" : 8,
    \ "orange" : 9,
    \ }
    return get(mapping, a:color_str, a:color_str)
endfunction

" Gets input from the user, modifies it a bit so it is suitable for the
" WriteCandWkflStates() function, and then calls that function.
function! WrapperWriteCandWkflStates(num_states)
    " Get the labels from the user.
    let label_list = []
    for i in range(1, a:num_states)
        call add(label_list, input("Label " . i . ": "))
    endfor

    " Get the icons from the user.
    " The standard icones for states 1-9 are as follows (I think)
    " demande    - Grey       (2)
    " positionne - Yellow     (4)
    " 1          - Green      (1)
    " 2          - Light Blue (5)
    " 3          - Dark Blue  (3)
    " 4          - Purple     (8)
    " 5          - Red        (6)
    " 6          - Orange     (9)
    " 7          - Green      (1)
    " 8          - Light Blue (5)
    " 9          - Dark Blue  (3)
    let standard_icons = {
    \    1: 'green',
    \    2: 'light blue',
    \    3: 'dark blue',
    \    4: 'purple',
    \    5: 'red',
    \    6: 'orange',
    \    7: 'green',
    \    8: 'light blue',
    \    9: 'dark blue',
    \ }
    " Build up nice string to print to the prompt which states what the
    " standard icon colors are.
    let default_icon_str = ""
    for i in keys(standard_icons)
        let default_icon_str = default_icon_str . "\n" . i . '. ' . standard_icons[i]
    endfor
    " TODO: Better check these inputs. Maybe even make a function that will
    " return an empty list or dictionary if no input is given.
    let icon_dict = eval(input("The default icons are:" . default_icon_str . "\nEnter Icon Dictionary: " ))
    " Turn the colors of the standard icon list back to numbers
    for i in keys(standard_icons)
        let standard_icons[i] = MapColorToNumber(standard_icons[i])
    endfor
    " Overwrite any icons that are not standard.
    for i in keys(icon_dict) 
        let standard_icons[i] = MapColorToNumber(icon_dict[i])
    endfor

    " Get the onenter attributes from the user.
    let onenter_dict = eval(input("Enter onenter Dictionary: "))
    " The code below allows the user to enter shorter things like 'popup',
    " 'go1', and 'elink' when entering the onenter dictionary.
    let popupCount = 1
    for i in keys(onenter_dict)
        if onenter_dict[i] ==# 'popup'
            let onenter_dict[i] = "javascript:popupCourrier(popup" . popupCount . ")"
            let popupCount += 1
        elseif onenter_dict[i] ==# 'go1'
            let onenter_dict[i] = "javascript:popupCourrier(go1)"
        elseif onenter_dict[i] ==# 'elink'
            let onenter_dict[i] = "sendDossierCandidat(iIDAffectation, 1, 1, 1, 1, 1, 'go-nogo', '', 1, 0, 'descriptif/contact')"
        endif
    endfor

    call WriteCandWkflStates(label_list, standard_icons, onenter_dict)
endfunction

" Configures the entire candidate workflow in one go. So it configures the
" states as well as the transitions.
function! ConfigureCandWorkflow()
    let num_states = input("Number of States: ")
    " Note that if there are no etat's present then you'll have to go into the
    " workflow node yourself.
    let i = 1
    while LuceoFindNode('workflow/etat' . i)
        delete
        let i += 1
    endwhile
    call cursor(line('.') - 1, 1)
    call WrapperWriteCandWkflStates(num_states)
    " Our cursor should be left on the <workflow_> node. Go to the transitions
    " node and then configure it.
    call LuceoFindNode('workflow/transitions')
    call WrapperWriteTransitions(num_states)
endfunction

" }}}

" Requisition Workflow {{{

" Disables the requisition workflow.
function! DisableReqWorkflow()
    call LuceoFindNode('workflow_validation')
    call ChangeNodeAttribute("actif", "false")
    call ChangeNodeAttribute("niveau", "2")
    call LuceoFindNode('workflow_validation/popups')
    call ChangeNodeAttribute("actif", "false")
    call LuceoFindNode('workflow_etat/etats/validation')
    call ChangeNodeAttribute("actif", "false")
    write
endfunction

" Configures the requisition workflow but in a very primative way. It assumes
" that the req approvers start with contact2 and then progress from there. So
" no user approvers and no 'contact' approver. Even though it assumes a lot, I
" don't think I'll change this function to try and account for those special
" situations; I get the feeling that it would be simpler to just call this
" function to configure it part way and then, if it isn't quite right, make
" the rest of the changes manually.

" TODO: Instead of having a list of labels for the approvers, consider looking
" in the json file for the name of contact2 - contacx. Then just surround that
" found name with 'Pending' and 'Approval'. ALSO, maybe we could have a flag
" for this function to tell it to start from descriptif/contact instead of
" descriptif/contact2 like it does now; yeah I think I like that second idea.
" TODO: Consider making a wrapper for this function which will prompt for the
" list of labels and such.
function! ConfigureReqWorkflow(approver_name_list)
    call LuceoFindNode('workflow_validation')
    let num_approvers = len(a:approver_name_list)
    call ChangeNodeAttribute("actif", "true")
    call ChangeNodeAttribute("niveau", num_approvers + 2)

    " Update all those 'action-niv' nodes.
    for i in range(1, num_approvers)
        call LuceoFindNode('workflow_validation/action-niv' . i)
        call ChangeNodeAttribute("OnGo", "SendPoste(iIDPoste, 'descriptif/contact" . (i+1) . "', 0, 'postego-postenogo')")
        call ChangeNodeAttribute("OnNoGo", "javascript:popupWorkflowPoste(motif-refus)")
    endfor
    call LuceoFindNode('workflow_validation/action-niv' . (num_approvers+1))
    call ChangeNodeAttribute("OnNoGo", "javascript:popupWorkflowPoste(motif-refus)")
    call LuceoFindNode('workflow_validation/popups')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('workflow_etat/etats/validation')
    call ChangeNodeAttribute("actif", "true")

    " Write the labels: 'Pending XYZ Approval'
    execute "split ".GetTranslationsFileLocation()."php.en.json"
    call ChangeLabel("TXT_POSTE_VALIDE_DEMANDEUR", a:approver_name_list[0])
    for i in range(1, num_approvers - 1)
        call ChangeLabel("TXT_POSTE_VALIDE_VALIDEUR" . i, a:approver_name_list[i])
    endfor
    call ChangeLabel("TXT_POSTE_VALIDE_VALIDEUR" . num_approvers, "Validated")
    write
    call RunTranslatorGenerate()
    wincmd p
endfunction

" This function will be used to configure either the candidate or requisition
" workflow depending on which file is being editied. I put this function in
" this fold because I wanted it to be located inside a fold but I didn't want
" to create a whole new fold for just this function.
function! ConfigureWorkflow()
    if expand('%:t') ==# "candidat.xml"
        call ConfigureCandWorkflow()
    elseif expand('%:t') ==# "poste.xml"
        let approver_name_list = eval(input("Enter list of approvers: "))
        call ConfigureReqWorkflow(approver_name_list)
    endif
endfunction

" }}}

" Sertifi, ZeroChaos, PeopleClues oh my! {{{

" Assumes we are editing forms.xml. Leaves us in forms.xml too with the
" assumption that this function will be used by functions that configure
" sertifi and such.
function! ConfigureFormsWkflowTemplates()
    " So, when all is said and done, I want to return to this same window that
    " is holding the forms.xml buffer. I thought it would be enought to store
    " the window number and then use that number to return but it seems that
    " whenever you open new windows, the window number could change. So the
    " way around this (I think) is to store the number of the forms.xml buffer
    " and then return to the window holding that buffer.
    let buf_num = bufnr("%")

    " Activate on forms.xml
    call LuceoFindNode("forms/form")
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode("forms/form/name")
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode("forms/form/template-link")
    call ChangeNodeAttribute("actif", "true")
    write

    " Activate on menu.xml
    execute "split ".GetXmlFileLocation()."menu.xml"
    call LuceoFindNode("menu/referentiels/ref/forms_templates")
    call ChangeNodeAttribute("actif", "true")
    write

    " Activate on poste.xml
    execute "split ".GetXmlFileLocation()."poste.xml"
    call LuceoFindNode("actions/forms")
    call ChangeNodeAttribute("actif", "true")
    write

    " Return to the forms.xml window
    execute "normal! \<C-w>" . bufwinnr(buf_num) . "w"
endfunction

" TODO: Have these JUST configure their respective things i.e take out the
" call to the function that configures FormsWkflowTemplates
" Assumes we are editing forms.xml
" Configures Sertifi
" NOTE: This does NOT add the sertifi webservice to consts_param.php. That
" step must be done manually. 
function! ConfigureSertifi(account_email, api_code)
    let security_token = '605847382303948747'
    call ConfigureFormsWkflowTemplates()

    " Activate sertifi
    call LuceoFindNode('forms/sertifi')
    call ChangeNodeAttribute("actif", "true")
    " Insert the email and api code
    call LuceoFindNode('forms/sertifi/login')
    call ChangeNodeAttribute("value", a:account_email)
    call ChangeNodeAttribute("api_code", a:api_code)
    " Activate all nodes in <document>
    call LuceoFindNode('forms/sertifi/document/nom')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('forms/sertifi/document/type')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('forms/sertifi/document/mini-moteur')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('forms/sertifi/document/mini-moteur/type')
    " Activate lien
    call LuceoFindNode('forms/sertifi/lien')
    call ChangeNodeAttribute("actif", "true")
    " Fill in security token
    call LuceoFindNode('forms/sertifi/soap/security-token')
    call ChangeNodeAttribute("value", security_token)
    write

    " Activate fields on candidat.xml
    execute "split ".GetXmlFileLocation()."candidat.xml"
    call LuceoFindNode('icones/sertifi')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/sertifi')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/sertifi/poste')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/sertifi/document')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/sertifi/date-envoi')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/sertifi/date-signature')
    call ChangeNodeAttribute("actif", "true")
    write

    " We know that ConfigureFormsWkflowTemplates() will have an open split
    " containing the file menu.xml. So instead of opening up ANOTHER window
    " pointing to that same buffer, we'll just revisit that split.
    let menu_xml_win_num = bufwinnr(bufnr('menu.xml'))
    " Activate on menu.xml
    execute "normal! \<C-w>" . menu_xml_win_num . "w"
    call LuceoFindNode('menu/referentiels/ref/document_sertifi')
    call ChangeNodeAttribute("actif", "true")
    write
endfunction

" Wrapper for the above function.
function! ConfigureSertifiWrapper()
    let email = input("Enter Account Email: ")
    let api_code = input("Enter API Code: ")
    call ConfigureSertifi(email, api_code)
endfunction

" Configures ZeroChaos
function! ConfigureZeroChaos(email)
    call ConfigureFormsWkflowTemplates()
    call LuceoFindNode('forms/checkpast')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('forms/checkpast/account/recruiter_email')
    call ChangeNodeAttribute("value", a:email)
    write

    execute "split ".GetXmlFileLocation()."candidat.xml"
    call LuceoFindNode('icones/checkpast')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/checkpast')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/checkpast/poste')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/checkpast/component')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/checkpast/date-demande')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/checkpast/date-maj')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/checkpast/status')
    call ChangeNodeAttribute("actif", "true")
    write
endfunction

" Wrapper for the above function.
function! ConfigureZeroChaosWrapper()
    let email = input("Enter Account Email: ")
    call ConfigureZeroChaos(email)
endfunction

" Configure PeopleClues
function! ConfigurePeopleClues(id)
    call ConfigureFormsWkflowTemplates()
    call LuceoFindNode('forms/assessment')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('forms/assessment/peopleclues')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('forms/assessment/peopleclues/account/id')
    call ChangeNodeAttribute("value", a:id)
    for i in range(0, 10)
        call LuceoFindNode('forms/assessment/peopleclues/packages/package' . i)
        call ChangeNodeAttribute("actif", "true")
    endfor
    write

    let menu_xml_win_num = bufwinnr(bufnr('menu.xml'))
    " Activate on menu.xml
    execute "normal! \<C-w>" . menu_xml_win_num . "w"
    call LuceoFindNode('menu/referentiels/ref/assessment_templates')
    call ChangeNodeAttribute("actif", "true")
    write

    execute "split ".GetXmlFileLocation()."candidat.xml"
    call LuceoFindNode('icones/assessment')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/assessment')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/assessment/poste')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/assessment/document')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/assessment/score')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/assessment/date-envoi')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/assessment/status')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/assessment/date-status')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/assessment/lien')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/assessment/details')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/assessment/date-complete')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/assessment/infos')
    call ChangeNodeAttribute("actif", "true")
    call LuceoFindNode('suivi/assessment/actions')
    call ChangeNodeAttribute("actif", "true")
    write
endfunction

" Wrapper for the above function.
function! ConfigurePeopleCluesWrapper()
    let id = input("Enter Customer ID: ")
    call ConfigurePeopleClues(id)
endfunction

" }}}

" Miscellaneous {{{

" TODO: Also add: set path+=src/profilsoft to the path and configure gf in
" such a way that 'use' statements like this:
"       use Profilsearch\Enum\CandidateTransferPart;
" Actually go to the file in question. If I cchanged that use statement to
" look like this:
"       use Profilsearch/Enum/CandidateTransferPart.php;
" Then it works.

" Sometimes the source code directly references files in this directory
set path+=src/view

" I found myself needing to copy the attributes 'libelle' and ('type-noeud' or
" 'type-liste') a lot, like when putting fields in the filter, so I thought
" I'd make a mapping to do it for me.
function! GetLibelleNoeudListe()
    let @y = GetNodeAttribute('libelle')
    let attr = GetNodeAttribute('type-noeud')
    if attr !=# ''
        let @z = attr
    else
        let @z = GetNodeAttribute('type-liste')
    endif
endfunction

" Made the complement for the above function, still needs some work. It seems
" to be putting a line break in the middle of a node.
function! SetLibelleNoeudListe()
    call ChangeNodeAttribute('libelle', @y)
    let attr = GetNodeAttribute('type-noeud')
    if attr !=# ''
        call ChangeNodeAttribute('type-noeud', @z)
    else
        call ChangeNodeAttribute('type-liste', @z)
    endif
endfunction

" Highlight the section boundaries a little differently
autocmd BufWinEnter *.xml match Underlined '\[section.*'

" }}}

" Mappings {{{

nnoremap <silent> <leader>ln :call GetLibelleNoeudListe()<CR>
nnoremap <silent> <leader>lN :call SetLibelleNoeudListe()<CR>

" Jumps to a node from its location in CDATA
nnoremap <leader>lf :call LuceoFindNode(GetNodePathFromCDATA(line('.')))<CR>
" Jumps to the node's location in CDATA from the node 
nnoremap <leader>lF :call LuceoFindCDATA()<CR>

" Makes a node mandatory from the CDATA section
nnoremap <leader>lm :call LuceoFindNode(GetNodePathFromCDATA(line('.')))<CR>:call ChangeNodeAttribute("obligatoire", "true")<CR>
" Makes a node non-mandatory from the CDATA section
nnoremap <leader>lM :call LuceoFindNode(GetNodePathFromCDATA(line('.')))<CR>:call ChangeNodeAttribute("obligatoire", "false")<CR>

" Orders the numbers on the 'field' nodes in the build/application form.
nnoremap <leader>lb vit:call OrderXMLNodes('<field')<CR>

" Orders the numbers on the 'transitions' nodes
nnoremap <leader>lo vit:call OrderXMLNodes('<transition')<CR>
" Writes all the candidate workflow transitions from scratch.
nnoremap <leader>lt :call WrapperWriteTransitions(input("Number of States: "))<CR>
" Thought it might be handy to have a mapping that just inserts one transition.
nnoremap <leader>lT :execute 'normal! o' . GenTransitionStr(1, input("From: "), input("To: "), input("Icon Number: "))<CR>
" Writes the candidate workflow states from scratch.
nnoremap <leader>lc :call WrapperWriteCandWkflStates(input("Number of States: "))<CR>
" Configures the candidate or requisition workflow depending on which file is
" being editied.
nnoremap <leader>lw :call ConfigureWorkflow()<CR>
" Disables the requisition workflow
nnoremap <leader>lW :call DisableReqWorkflow()<CR>

" Adds a field
nnoremap <leader>la :call AddFieldWrapper()<CR>
" Adds a repeatable field to the 'qualification' node.
nnoremap <leader>lq :call AddRepeatableFieldWrapper(0)<CR>
" Adds a repeatable field to the 'experiences' node.
nnoremap <leader>le :call AddRepeatableFieldWrapper(1)<CR>
" Adds a 'section' tag to the CDATA.
nnoremap <leader>ls :call AddSectionWrapper()<CR>

" Changes the label attached to a node
nnoremap <leader>ll :call ChangeNodeLabelWrapper()<CR>
" Echo's the label attached to a node
nnoremap <silent><leader>lL :call GetLabelNameWrapper()<CR>

" Removes a field
nnoremap <leader>lr :call RemoveFieldWrapper()<CR>

" Adds a field in the searchable area
"nnoremap <leader>ls :call LuceoAddSearchableField()<CR>

" Calls psf translator:generate
nnoremap <leader>lpg :call RunTranslatorGenerate()<CR>
" Calls psf translator:dedupe
nnoremap <leader>lpd :call RunTranslatorDedupe()<CR>
" Calls psf translator:merge
nnoremap <leader>lpm :call RunTranslatorMerge()<CR>

" }}}


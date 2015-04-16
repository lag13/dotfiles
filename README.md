My Vim Configuration
====================

I started working on multiple machines and wanted to share my vim
configuration. I saw this video on vimcasts and decided to give it a shot:
http://vimcasts.org/episodes/synchronizing-plugins-with-git-submodules-and-pathogen/

Installation:
-------------
    
### Clone The Repository

    git clone https://github.com/lag13/vimconfig.git ~/.vim

### Create The symlink

    ln -s ~/.vim/vimrc ~/.vimrc

### Initialize The git-submodules


Adding New Plugins
------------------

    cd ~/.vim
    git submodule add GIT_URL path/to/plugin



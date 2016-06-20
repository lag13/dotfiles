dotfiles
========

I'm using the strategy outlined here to manage my dotfiles:
https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/

New Machine Setup
-----------------

Run these commands to set up a new machine. Note the `dotgit` command is an
alias in `.bash_profile`:

```
git clone --bare git@github.com:lag13/dotfiles.git $HOME/.dotgit
dotgit config --local status.showUntrackedFiles no
dotgit checkout
```

Update Dotfiles
---------------

The dotfiles are now in the home directory where they should be. Use the
`dotgit` to check if any files have changed and commit/push/... just like
normal. New files will not be shown in the output of `dotgit status` because
of the `showUntrackedFiles` option. This is to reduce clutter.

Large File Storage (LFS)
========================

https://git-lfs.github.com/
https://www.atlassian.com/git/tutorials/git-lfs

From atlassian:

      Git is a distributed version control system, meaning the entire history
      of the repository is transferred to the client during the cloning
      process. For projects containing large files, particularly large files
      that are modified regularly, this initial clone can take a huge amount
      of time, as every version of every file has to be downloaded by the
      client. Git LFS (Large File Storage) is a Git extension developed by
      Atlassian, GitHub, and a few other open source contributors, that
      reduces the impact of large files in your repository by downloading the
      relevant versions of them lazily. Specifically, large files are
      downloaded during the checkout process rather than during cloning or
      fetching.

Git lfs is just a new subcommand for git which, uses hooks and such, to get
the job done. You tell lfs to manage certain files. When you push those files,
the hooks lfs set up will convert that file to a "pointer" file which has
enough information for lfs to download it when needed and that "pointer" file
gets pushed to github. The file itself is pushed to an lfs server. You never
see these "pointer" files though (even on github), it all happens
transparently.

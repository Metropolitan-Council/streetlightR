## Contributing to streetlightR

This project uses a [feature-branch](https://deepsource.io/blog/git-branch-naming-conventions/) naming convention and workflow.

`main` is the main branch (not `master`), base your work off of `main`.
Contribute to the project by making changes to your own feature branch and issuing pull-requests when you're ready to integrate into the `main` branch:

* Pull the `main` branch; `git pull`, and if necessary `git checkout main` to switch to `main`
* Create a feature branch and check out your branch, e.g., `git checkout -b bug-fix-1`
  * You can use your initials to prefix a feature branch, e.g.,
  `aa-bug-fix-1`.
  * Your feature branch should do one thing only, for example: 
    * create a new function,
    * create a new custom variable,  
    * fix an issue - [please name your branch with the issue number](https://deepsource.io/blog/git-branch-naming-conventions/)
* Commit changes related to your feature and push them to GitHub.
* You can push changes to your feature branch at any time.
* When you're ready to have your work reviewed you create a pull-request on GitHub.
* You can issue a pull-request and request a review of work-in-progress if you want guidance on code or content.
* Make changes or respond to comments in your pull-request reviews.
  * New commits pushed to your branch will update the pull-request.
* When your pull request is approved, the reviewer will merge your branch into main and may delete your branch from GitHub.
  * To remove deleted feature branches from your local repository run `git remote prune origin`.
  * Do not attempt to push additional commits to a merged pull-request.
  Instead, start a new feature branch and issue a new pull request.
* Remember to update and branch off of `main` whenever you start a new feature, e.g., `git checkout main; git pull origin main; git checkout -b a-new-feature`.

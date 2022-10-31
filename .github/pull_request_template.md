# Description

Please include a summary of the change and which issue is fixed. 

Fixes # (issue)

## Type of change

Please delete options that are not relevant.

- [ ] Bug fix 
- [ ] New feature 

# Package development checklist

- [ ] `usethis::use_latest_dependencies()`
- [ ] `usethis::use_tidy_description()`
- [ ] `devtools::test()`
- [ ] `devtools::document(roclets = c('rd', 'collate', 'namespace'))`
- [ ] Update `DESCRIPTION` with new contributors, if neccessary.
- [ ] Double check your documentation
- [ ] `styler:::style_active_pkg()`
- [ ] `rcmdcheck::rcmdcheck()`
  - fix errors
  - fix simple warnings and notes


## Courtesy checklist

- [ ] My code follows the style guidelines of this project
- [ ] I have performed a self-review of my own code
- [ ] I have commented my code, particularly in hard-to-understand areas
- [ ] I have made corresponding changes to the documentation
- [ ] I have added tests that prove my fix is effective or that my feature works
- [ ] I have tagged this PR appropriatley

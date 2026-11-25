library(tidyverse)
#testing changes for git push
library(dslabs)

install.packages(c("usethis", "gitcreds"))
usethis::create_github_token()
gitcreds::gitcreds_set()
usethis::git_sitrep()



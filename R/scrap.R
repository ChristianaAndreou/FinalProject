# Scrap code

# This code might be useful to avoid repeatedly adding your
# username and long PAT token (the long password)

library(usethis)
# Set your Git user name and email
use_git_config(user.name  = "ADD-YOUR-GIT-USERNAME",
               user.email = "ADD-YOUR-EMAIL")

# install.packages("gitcreds") if needed, probably already installed
library(gitcreds)
# Save your token in the OS credential store
gitcreds_set()
# You will be prompted for your PAT token (the long password)
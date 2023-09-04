

# Tutorial: https://beatrizmilz.github.io/slidesR/git_rstudio/11-2021-ENCE.html#22

if (!require(pacman)) install.packages("pacman")

pacman::p_load(usethis, rmarkdown)

# install.packages("usethis")
# install.packages("rmarkdown")

usethis::use_git_config(# Seu nome
  user.name = "Fernando Bispo", 
  # Seu email
  user.email = "fobispo@outlook.com")

# Gerando um token
usethis::create_github_token()


usethis::edit_r_environ()




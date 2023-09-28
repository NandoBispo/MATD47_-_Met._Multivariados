

# Tutorial:
# https://beatrizmilz.github.io/slidesR/git_rstudio/11-2021-ENCE.html#22

# Pacotes ----
if (!require(pacman)) install.packages("pacman")

pacman::p_load(usethis, rmarkdown)

# install.packages("usethis")
# install.packages("rmarkdown")
# ______________________________________________________

usethis::use_git_config(# Seu nome
  user.name = "Fernando Bispo", 
  # Seu email
  user.email = "fobispo@outlook.com")

# Gerando um token
usethis::create_github_token()

# Armazenando o token no local correto.
gitcreds::gitcreds_set()
# https://analisemacro.com.br/data-science/conectando-git-github-e-rstudio-em-3-passos/

# Abrindo o documento reviron para adicionar o token.
usethis::edit_r_environ()

# Checando se deu tudo certo
usethis::git_sitrep()


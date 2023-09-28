# Atividades IRIS

pacman::p_load(janitor, tidyverse, mvoutlier)

# mvoutlier: Possibilita o calculo da distância generalizada com o comando chisq.plot

dados_iris <- janitor::clean_names(iris)


## Item a ----
dados_iris|>
  select(petal_width, petal_length, species)|>
  # dplyr::filter(species == "setosa")|>
  summarytools::descr(
    stats = c("min", "max", "mean", "sd"),
    transpose = T,
    justify = "c",
    style = "grid"
  )

dados_iris|>
  select(petal_width, petal_length, species)|>
  dplyr::filter(species == "versicolor")|>
  summarytools::descr(
    stats = c("min", "max", "mean", "sd"),
    transpose = T,
    justify = "c",
    style = "grid"
  )

dados_iris|>
  select(petal_width, petal_length, species)|>
  dplyr::filter(species == "virginica")|>
  summarytools::descr(
    stats = c("min", "max", "mean", "sd"),
    transpose = T,
    justify = "c",
    style = "grid"
  )

dados_iris|>
  select(petal_width, petal_length, species)|>
  dplyr::filter(species == "setosa")|>
  summarytools::descr(
    stats = c("min", "max", "mean", "sd"),
    transpose = T,
    justify = "c",
    style = "grid"
  )


covariancia <- dados_iris|>
  select(petal_length, petal_width)|>
  stats::cov()

solve(covariancia)

n <- dim(dados_iris)[1]
p = 2

medias <- dados_iris|>
  select(petal_length, petal_width)|>
  colMeans()|>
  as.matrix()


dados_iris|>
  select(petal_length, petal_width)|>
  mvoutlier::chisq.plot(ask = T)


## Item b ----






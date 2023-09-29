# ******************************
# Atividade pacote IRIS
# ______________________________

# PACOTES ----
pacman::p_load(janitor, tidyverse, mvoutlier, kableExtra)

# mvoutlier: Possibilita o cálculo da distância generalizada com o comando chisq.plot

# DADOS ----
dados_iris <- janitor::clean_names(iris)
dplyr::glimpse(dados_iris)

# ITENS ----
## Item a ----
summarytools::st_options(lang = "pt")

### AnaDesc por variável ----
dados_iris|>
  dplyr::select(petal_width, petal_length, species)|>
  summarytools::descr(
    stats = c("min", "q1", "med", "mean","q3", "max",  "sd"),
    transpose = T,
    justify = "c",
    headings = F, 
    # display.labels = F,
    style = "simple" #"jira" # "rmarkdown"# "grid"
  )

### AnaDesc por grupo ----
dados_iris|>
  select(petal_width, petal_length, species)|>
  group_by(species)|>
  summarytools::descr(
    stats = c("min", "q1", "med", "mean","q3", "max",  "sd"),
    transpose = T,
    justify = "c",
    headings = F, 
    # display.labels = F,
    style = "simple" #"jira" # "rmarkdown"# "grid"
  )

### d(i)² ----
# Matriz de variância e covariância
covariancia <- dados_iris|>
  select(petal_length, petal_width)|>
  stats::cov()

covariancia

# Matriz de covariância inversa
solve(covariancia)

n <- dim(dados_iris)[1]
p = 2

medias <- dados_iris|>
  select(petal_length, petal_width)|>
  colMeans()|>
  as.matrix()

# Calculando a Distância Generalizada (Mahalanobis)
distancias <- array(data=1:n, dim=c(n,1))
gl <- base::array(1:n, dim=c(n,1))
perc <- base::array(1:n, dim=c(n,1))

# Vetor com as distâncias generalizadas.
for (i in 1:n) {
  distancias[i,1] <- (dados_iris[i,3:4]-medias)|>as.matrix()%*%solve(covariancia)%*%t(dados_iris[i,3:4]-medias)|>as.matrix()
  gl[i] <- (i-0.5)/n
  perc[i] <- stats::qchisq(gl[i],p)
}

distancias|>
  round(4)|>
  head()

# Ordenando o vetor de distâncias.
dist_order <- base::sort(distancias[,1],na.last = F, decreasing = F)
dist_order|>round(4)

# Grafico
plot(dist_order,perc, col="red", pch=16, ylab="q(i)", xlab="d(i)²", main="Grafico dos pares")
# lines(x = distancias, y = perc)

# Utilizando o pacote mvoutlier
dados_iris|>
  select(petal_length, petal_width)|>
  mvoutlier::chisq.plot(ask = T)

range(distancias)
range(perc)


## Item b ----






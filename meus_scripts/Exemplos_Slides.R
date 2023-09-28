# Pacotes ----
pacman::p_load(matlib)


# Slide 3 ----
## Exemplo 1 ----
### Item a ----
# a <- c(3, 5, 5, 7, 7, 7, 8, 9, 10, 11)
# b <- c(2.3, 1.9, 1, 0.7, 0.3, 1, 1.05, 0.45, 0.7, 0.3)
m1 <- base::cbind(c(3, 5, 5, 7, 7, 7, 8, 9, 10, 11), c(2.3, 1.9, 1, 0.7, 0.3, 1, 1.05, 0.45, 0.7, 0.3))

# m1 <- base::cbind(a, b)
# |>as.data.frame()

n=dim(m1)[1]
p=dim(m1)[2]

# Matriz de Variâncias e Covariâncias
cov_m1 <- stats::cov(m1)
cov_m1|>round(3)

# Vetor de Médias
# x_barra1 <- colMeans(m1)[[1]]
# x_barra2 <- colMeans(m1)[[2]]
medias <- colMeans(m1)|>as.matrix()

# Obtendo a Matriz Inversa da Covariância
solve(cov_m1)|>round(3)

# Calculando a Distância Generalizada (Mahalanobis)
distancias <- array(1:n, dim=c(n,1))

# Vetor com as distâncias generalizadas.
for (i in 1:n) {
  distancias[i,1] <- t(m1[i,]-medias)%*%solve(cov_m1)%*%(m1[i,]-medias)
}

distancias|>round(4)|>t()

# Ordenando o vetor de distâncias.
dist_order <- base::sort(distancias[,1],na.last = F, decreasing = F)
dist_order|>round(4)


# _____________________________________________________________
  t(m1[2,]-medias)%*%solve(cov_m1)|>round(3)
  
  t(m1[2,]-medias)%*%solve(cov_m1)%*%(m1[2,]-medias)

  (m1[,2]-medias[2])
# _____________________________________________________________
  
### Item b ----

gl <- base::array(1:n, dim=c(n,1))
perc <- base::array(1:n, dim=c(n,1))

for (i in 1:n) {
  gl[i] <- (i-0.5)/n
  perc[i] <- stats::qchisq(gl[i],p)
}

gl|>round(3)|>t()
perc|>round(3)|>t()
# qchisq(gl,p,0)|>round(3)|>t()

qchisq(0.5,2)

### Item c ----

# Grafico
plot(dist_order,perc, col="red", pch=19, ylab="q(i)", xlab="d(i)", main="Grafico dos pares")


  
  
  
  
  
  
  


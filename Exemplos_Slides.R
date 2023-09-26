# Pacotes ----
pacman::p_load(matlib)


# Slide 3 ----
## Exemplo 1 ----
### Item a ----
a <- c(3, 5, 5, 7, 7, 7, 8, 9, 10, 11)
b <- c(2.3, 1.9, 1, 0.7, 0.3, 1, 1.05, 0.45, 0.7, 0.3)

m1 <- base::cbind(a, b)
# |>as.data.frame()

n=dim(m1)[1]
p=dim(m1)[2]

cov_m1 <- stats::cov(m1)
cov_m1|>round(3)

# Obtendo o Vetor de Médias
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

# _____________________________________________________________
  t(m1[2,]-medias)%*%solve(cov_m1)|>round(3)
  
  t(m1[2,]-medias)%*%solve(cov_m1)%*%(m1[2,]-medias)

  (m1[,2]-medias[2])
# _____________________________________________________________
  
# adjoint(cov_m1)
  
### Item b ----
# Ordenando o vetor de distâncias.
dist_order <- base::sort(distancias[,1],na.last = F, decreasing = F)
  
dist_order|>round(4)

gl = ((n-0.5)/n)

for (i in 1:n) {
  gl[i] <- (i-0.5)/n
  perc[i] <- stats::dchisq(gl[i],p)
}

gl
perc

dchisq(0.95,2)
plot(dist_order,perc)
  
  
  
  
  
  
  
  


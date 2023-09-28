# _______________________________________________________________________

if (!require(pacman)) install.packages("pacman")

pacman::p_load(readxl, summarytools)


library(readxl)
# pardais <- read_excel("C:/MATD47_20232/aula2_pardais/R_pardais/pardais.xlsx")
pardais <- readxl::read_excel("dados/pardais.xlsx")
View(pardais)
dados<-pardais
#require(stats)

#library(help="stats")

#require(MASS)

summary(dados[2:6])
graphics::pairs(dados[2:6]) # ou
plot(dados[2:6])

summarytools::st_options(lang = "pt")

dados|>
  dplyr::select(-Pássaro)|>
  summarytools::descr(stats = c("min", "q1", "med", "mean","q3", "max",  "sd"), # , "cv", "Skewness", "Kurtosis"
                    # round.digits = 3,
                    justify = "c",
                    style = "rmarkdown",
                    # headings = T,
                    # split.tables = 0.3,
                    transpose = F
)


## Selecionando subconjuntos ----

sobrev<-subset(dados, dados[1] <=21)
sobrev

n_sobrev<-subset(dados, dados[1] >21)
n_sobrev

summary(sobrev[2:6])
summary(n_sobrev[2:6]) 


# _______________________________________________________________________
## Covariância ----

R<-c(1,-1,0,0,0)
R<-base::as.matrix(R,5,1)
# R<-as.matrix(data=R, nrow=5, ncol=1)
dim(R)
medias<-base::colMeans(dados[2:6])
medias<-base::as.matrix(medias,5,1)
dim(medias)


dados1<-(dados[2:6]) # Gera um novo cj de dados para facilitar os indices
dados1[1,]
n<-dim(dados)[1]
um<-as.matrix(rep(1,n),n,1)
medias[1,1]*um

## Variância ----
d1<-matrix(0,5,1)
for(j in 1:5){
  d1[j,]<-sum((dados1[,j]-(medias[j,1]*um))^2)  
}
d1/n
diag(cov(dados1))

## Covariância x1 e x2 ----

d12<-sum((dados1[,1]-(medias[1,1]*um))*(dados1[,2]-(medias[2,1]*um)))
d12/n
cov(dados1)[1,2]

#Inferência para combinações lineares do vetor de médias:
#por exemplo: mu1-mu2
quantil<-qt(0.975,n-1)
covariancia<-cov(dados[2:6])

LI<-(t(R)%*%medias)-quantil*sqrt((1/n)*t(R)%*%covariancia%*%R)
LI

LS<-(t(R)%*%medias)+quantil*sqrt((1/n)*t(R)%*%covariancia%*%R)
LS

cat("Intervalo de confiança para mu[1]-mu[2]:", LI, LS)

boxplot(dados[2:3])

## Verificando normalidade (distâncias) ----

d2_G<-array(1:n, dim=c(n,1))
perc_G<-array(1:n, dim=c(n,1))
for(j in 1:n) {
  d2_G[j,1]<-t(t(dados1[j,]-medias))%*%solve(covariancia)%*%t(dados1[j,] -medias)
  gl<-(j-0.5)/n
  perc_G[j,1]<-qchisq(gl,5)
}

gl<-48.5/49
qchisq(gl,5,0)

ord_d2_G<-sort(d2_G) # ordenando distancias
plot(ord_d2_G,perc_G)

## Normalidade Multivariada ----

require(mvnormalTest)
mardia(dados[2:6])
mvnTest(dados[2:6], B = 1000, pct = c(0.01, 0.99))


## Matriz de Correlações ----

library(corrplot)
res <- cor(dados[2:6],method="pearson") # matriz de correlação
round(res, 2)
corrplot(cor(dados[2:6]), method = "circle")
corrplot(cor(dados[2:6]), method = "square")

library(PerformanceAnalytics)
chart.Correlation(dados[2:6], pch=19)

library(GGally)
ggpairs(dados[2:6])

library(psych)
pairs.panels(dados[2:6])

cortest.bartlett(res,n=49) 
cortest.bartlett(dados[2:6]) #apresenta mensagem de erro, mas faz a conta

corre=cor(dados[2:6], method = "pearson")
library(qgraph)
qgraph(corre, shape="circle", 
       posCol="darkgreen", 
       negCol="darkred", layout="groups", vsize=10)

## Teste de hipótese para matriz de correlação ----
# testa matriz identidade
autovalor<-eigen(cor(dados1))$values
est_test<--(n-(1/6)*(2*5+11))*sum(log(autovalor))

quant<-0.5*5*4
qui<-qchisq(0.05,0.5*5*4,ncp=0)

if(est_test>qui) cat("conclusão: rejeita-se H0","\n") else cat("conclusão: não se rejeita H0","\n")


## Estatistica T2 de Hotelling ----
require(DescTools)
#selecionando subconjuntos

HotellingsT2Test(dados[2:6])

### Conferindo ----

p<-dim(dados1)[2]
mu0<-matrix(0,p,1)
T2<-n*t(medias-mu0)%*%solve(covariancia)%*%(medias-mu0)
T2

valorF<-(n-p)*T2/(p*(n-1))
valorF  #estatística

valor<-qf(0.05, p, n-p,  lower.tail = FALSE)
if( T2>valor) cat("Conclusão: Rejeita-se H0","\n") else cat("Conclusão: Não se rejeita H0","\n")

## IC para mu1 ----
LI<-medias[1,1]-sqrt(covariancia[1,1]/n)*sqrt(p*(n-1)*qf(0.05, p, n-p,  lower.tail = FALSE)/(n-p))

LS<-medias[1,1]+sqrt(covariancia[1,1]/n)*sqrt(p*(n-1)*qf(0.05, p, n-p,  lower.tail = FALSE)/(n-p))

cat("Intervalo de confiança para mu[1]:", LI,LS)


# _______________________________________________________________________

require(mvoutlier)
chisq.plot(dados1,ask=TRUE)   #ask (lógica):  fazer interação

res<-chisq.plot(dados1,ask=TRUE)
res$outliers

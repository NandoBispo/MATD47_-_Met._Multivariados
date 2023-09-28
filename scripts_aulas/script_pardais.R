# PACOTES ----
# library(readxl)

if (!require(pacman)) install.packages("pacman")
pacman::p_load(readxl,  janitor, tidyverse)

# DADOS ----

# pardais <- read_excel("C:/MATD47_20232/aula2_pardais/R_pardais/pardais.xlsx")
pardais <- readxl::read_excel("dados/pardais.xlsx")
View(pardais)
dados<-pardais

#require(stats)

#library(help="stats")

#require(MASS)

summary(dados[2:6])
pairs(dados[2:6]) #ou
plot(dados[2:6])

# _____________________________________________________________
dados1=dados|>
  janitor::clean_names()|>
  dplyr::rename(
    "comp_total"=x1, "ext_alar"=x2, "comp_bico_kbca"=x3, "comp_umero"=x4, "comp_quilha_esterno"=x5)

# dados1|>
#   dplyr::select(-passaro)|>
  sobrev|>
  summarytools::descr(
    stats = c("min", "q1", "med", "mean","q3", "max",  "sd"),
    # round.digits = 3,
    justify = "c",
    style = "rmarkdown", #"grid", #"jira", #"simple",
    headings = F,
    # split.tables = 1, 
    rescale.weights = T,
    transpose = F
  )
# _____________________________________________________________

## Selecionando subconjuntos ----

sobrev<-base::subset(dados, dados[1]<=21)
sobrev

n_sobrev<-base::subset(dados, dados[1]>21)
n_sobrev

summary(sobrev[2:6])
summary(n_sobrev[2:6]) 


pairs(sobrev[2:6])
pairs(n_sobrev[2:6])
pairs(~X1+X2+X3+X4+X5,data=dados)

plot(dados[2:6])


### Gráfico de dispersão - por grupo ----

plot(sobrev$X1, sobrev$X2,type = "p",xlab="comprimento total (mm)", ylab="extensão alar (mm)",
     xlim=c(150,170),ylim=c(225,255),main = "")
points(n_sobrev$X1,n_sobrev$X2, col = "red")
legend("topright", cex=0.4,c("Sobreviventes", "Não-sobreviventes"),text.col=c("black","red"))

# _____________________________________________________________

## Gráficos de perfis ----

media_sob<-apply(sobrev[2:6], 2,mean)
media_Nsob<-apply(n_sobrev[2:6], 2,mean)
plot(media_sob,type = "b",xlab="variáveis", ylab="média",main = "",col = "green")
lines(media_Nsob,type = "b", col = "red")
legend("topright", cex=0.4,c("Sobreviventes", "Não-sobreviventes"),text.col=c("green","red"))

sd_sob<-apply(sobrev[2:6], 2,sd)
sd_Nsob<-apply(n_sobrev[2:6], 2,sd)
plot(sd_sob,type = "b",xlab="variáveis", ylab="desvio padrão",main = "",col = "green",ylim=c(0,7))
lines(sd_Nsob,type = "b", col = "red")
legend("topright", c("Sobreviventes", "Não-sobreviventes"),cex=0.4,text.col=c("green","red"))

## BOXPLOT ----
boxplot(as.data.frame(dados[2:6]))

par(mfrow = c(2, 3))        

boxplot(dados$X1~dados$Sobrevivente)
legend("topright", c("S", "NS"),cex=0.4)
boxplot(dados$X2~dados$Sobrevivente)
boxplot(dados$X3~dados$Sobrevivente)
boxplot(dados$X4~dados$Sobrevivente)
boxplot(dados$X5~dados$Sobrevivente)

par(mfrow = c(1, 2))

boxplot(as.data.frame(sobrev[2:6]))
boxplot(as.data.frame(n_sobrev[2:6]))


rb <- boxplot(dados$X1~dados$Sobrevivente, col="bisque")
title("Comparando boxplot e intervalo da média +/- SD")

mn.t <- tapply(dados$X1, dados$Sobrevivente, mean)
sd.t <- tapply(dados$X1, dados$Sobrevivente, sd)
xi <- 0.3 + seq(rb$n)
points(xi, mn.t, col = "orange", pch = 18)
arrows(xi, mn.t - sd.t, xi, mn.t + sd.t,
       code = 3, col = "pink", angle = 75, length = .1)


# _____________________________________________________________

## Gráfico de 3 dimensões ----
require(lattice)
cloud(dados$X3~dados$X2+dados$X1,xlab="extensão alar",ylab="comprimento total",zlab="comp do bico e da cabeça" )

fator_sobre<-as.factor(dados$Sobrevivente)
cloud(dados$X3~dados$X2+dados$X1,groups=fator_sobre,col=c("red","blue"),xlab="extensão alar",ylab="comprimento total",zlab="comp do bico e da cabeça" )

par(mfrow = c(2, 2))
require(scatterplot3d)
scatterplot3d(dados$X3,dados$X2,dados$X1)
scatterplot3d(dados$X3,dados$X2,dados$X1,angle=80)
scatterplot3d(dados$X3,dados$X2,dados$X1,angle=40,type='h')

# _____________________________________________________________

# Matriz de Covariância ----

cor(dados[2:6], use = "all.obs",
    method = "pearson")              #paramétrico

cor(dados[2:6], use = "all.obs",
    method = "kendall")

cor(dados[2:6], use = "all.obs",
    method = "spearman")


### QQ plot ----
par(mfrow = c(2, 3))  

x1<-seq(-3,3,0.01)
y1<-x1

# Padronização
xx<-(dados$X1-mean(dados$X1))/sqrt(var(dados$X1))
qqnorm(xx)
lines(x1,y1,col="red")

xx2<-(dados$X2-mean(dados$X2))/sqrt(var(dados$X2))
qqnorm(xx2)
lines(x1,y1,col="red")

xx3<-(dados$X3-mean(dados$X3))/sqrt(var(dados$X3))
qqnorm(xx3)
lines(x1,y1,col="red")

xx4<-(dados$X4-mean(dados$X4))/sqrt(var(dados$X4))
qqnorm(xx4)
lines(x1,y1,col="red")

xx5<-(dados$X5-mean(dados$X5))/sqrt(var(dados$X5))
qqnorm(xx5)
lines(x1,y1,col="red")

# Testes de Hipóteses - comparação de médias ----

t.test(X1 ~ Sobrevivente, data = dados)
t.test(X2 ~ Sobrevivente, data = dados)
t.test(X3 ~ Sobrevivente, data = dados)
t.test(X4 ~ Sobrevivente, data = dados)
t.test(X5 ~ Sobrevivente, data = dados)

### Normalidade Multivariada ----

require(mvnormalTest)
mvnormalTest::mardia(dados[2:6])
mvnormalTest::mvnTest(dados[2:6], B = 1000, pct = c(0.01, 0.99))


### Matriz de Correlações ----

library(corrplot)
res <- cor(dados[2:6],method="pearson") # Corr matrix
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


### Estatistica T2 de Hotelling ----
require(DescTools) 
#selecionando subconjuntos

dados_sobrev<-subset(dados, dados[1] <=21,select=X1:X5)

dados_Nsobrev<-subset(dados, dados[1] >21,select=X1:X5)

HotellingsT2Test(dados_sobrev,dados_Nsobrev)

#####Conferindo
dados_sobrev<-as.matrix(dados_sobrev)
dados_Nsobrev<-as.matrix(dados_Nsobrev)

media_S<-apply(dados_sobrev, 2,mean)
media_NS<-apply(dados_Nsobrev, 2,mean)

t(media_S-media_NS )

n1<-dim(dados_sobrev)[1]  #tamanho da amostra 1
n2<-dim(dados_Nsobrev)[1]  #tamanho da amostra 2
p<-dim(dados_Nsobrev)[2]  # variáveis

S1<-cov(dados_sobrev)
S1

S2<-cov(dados_Nsobrev)
S2

S<-((n1-1)*S1+(n2-1)*S2)/(n1+n2-2)

T2<-((n1*n2)/(n1+n2))*t(media_S-media_NS)%*%solve(S)%*%(media_S-media_NS)
T2

valorF<-(n1+n2-p-1)*T2/((n1+n2-2)*p)
valorF  #estatística

valor<-((n1+n2-2)*p)*qf(0.05, p, (n1+n2-p-1),  lower.tail = FALSE)/ (n1+n2-p-1)
if( T2>valor) cat("conclusão: rejeita-se H0","\n") else cat("conclusão: não se rejeita H0","\n")
T2

######M de Box
n1<-dim(dados_sobrev)[1]  #tamanho da amostra 1
n2<-dim(dados_Nsobrev)[1]  #tamanho da amostra 2
p<-dim(dados_Nsobrev)[2]  # variáveis

S1<-cov(dados_sobrev)
S1

S2<-cov(dados_Nsobrev)
S2



W<-((n1-1)*S1+(n2-1)*S2)/(n1+n2-2)
W

d1<-det(S1)
d2<-det(S2)

n<-n1+n2

g<-2

a1<-p*(p+1)*(g-1)/2
nv<-1/(n1-1)+1/(n2-1)
c1<-(2*p^2+3*p-1)*(nv-(1/(n-g)))/(6*(p+1)*(g-1))
c1^2
nv2<-1/((n1-1)^2)+1/((n2-1)^2)
c2<-(p-1)*(p+2)*(nv2-(1/((n-g)^2)))/(6*(g-1))
c2
a2<-(a1+2)/(c2-c1^2)
b<-(1-c1-a1/a2)/a1

lM<-(-b)*((n1-1)*log(d1)+(n2-1)*log(d2)-
            (n-g)*log(det(W)))

lM

valorF<-qf(0.1, v1, v2,  lower.tail = FALSE) #percentil da distribuição F
valorF

valorP<-pf(lM, v1, v2,  lower.tail = FALSE) #percentil da distribuição F
valorP

if( lM>valorF) cat("conclusão: rejeita-se H0","\n") else cat("conclusão: não se rejeita H0","\n")
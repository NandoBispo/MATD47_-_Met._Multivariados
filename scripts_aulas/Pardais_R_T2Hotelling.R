library(readxl)
pardais <- read_excel("C:/MATD47_20232/aula2_pardais/R_pardais/pardais.xlsx")
View(pardais)
dados<-pardais

#####Estatistica T2 de Hotelling 

require(DescTools) 
#selecionando subconjuntos

dados_sobrev<-subset(dados, dados[1] <=21,select=X1:X5)

dados_Nsobrev<-subset(dados, dados[1] >21,select=X1:X5)

teste0<-HotellingsT2Test(dados_sobrev,dados_Nsobrev)

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

#Para conferir
teste0

valorF<-(n1+n2-p-1)*T2/((n1+n2-2)*p)
valorF  #estatística

valor<-((n1+n2-2)*p)*qf(0.05, p, (n1+n2-p-1),  lower.tail = FALSE)/ (n1+n2-p-1)
if( T2>valor) cat("conclusão: rejeita-se H0","\n") else cat("conclusão: não se rejeita H0","\n")

#############
install.packages("Hotelling")
require(Hotelling)
?hotelling.test

teste1<-hotelling.test(dados_sobrev,dados_Nsobrev, shrinkage = FALSE, var.equal = TRUE)
#shrinkage = TRUE
#estimador de Strimmer James-Stein shrinkage 
#é usado para calcular as matrizes de covariâncias 

teste2<-hotelling.test(dados_sobrev,dados_Nsobrev, shrinkage = FALSE, var.equal = FALSE)

#Conferindo
T2<-t(media_S-media_NS)%*%solve((1/n1)*S1+(1/n2)*S2)%*%(media_S-media_NS)
T2

#####
require(ICSNP)
HotellingsT2(dados_sobrev,dados_Nsobrev,test = "chi") 
teste1

HotellingsT2(dados_sobrev,dados_Nsobrev,test = "f") 
teste0



# Dados ----
dados<-read.table("dados/cranio.txt",header=TRUE)

X<-cbind(dados$x1,dados$x2,dados$x3,dados$x4)

grupo <- factor(gl(5,30), labels=c("1", "2","3","4","5"))

result<-stats::manova(X~grupo)

result2<-stats::summary.manova(result)

resultW<-summary.manova(result,test="Wilks")

resultR<-summary.manova(result,test="Roy")

resultP<-summary.manova(result,test="Pillai")

resultHL<-summary.manova(result,test="Hotelling-Lawley")

resultW

resultR

resultP

resultHL

# _____________________________________________________________

# Selecionando subconjuntos ----

pop1<-subset(dados[1:4], dados[5] ==1)
pop1

pop2<-subset(dados[1:4], dados[5] ==2)
pop2

pop3<-subset(dados[1:4], dados[5] ==3)
pop3

pop4<-subset(dados[1:4], dados[5] ==4)
pop4

pop5<-subset(dados[1:4], dados[5] ==5)
pop5

pop1<-as.matrix(pop1)
pop2<-as.matrix(pop2)
pop3<-as.matrix(pop3)
pop4<-as.matrix(pop4)
pop5<-as.matrix(pop5)


S1<-cov(pop1)
S2<-cov(pop2)
S3<-cov(pop3)
S4<-cov(pop4)
S5<-cov(pop5)

n1<-dim(pop1)[1]
n2<-dim(pop2)[1]
n3<-dim(pop3)[1]
n4<-dim(pop4)[1]
n5<-dim(pop5)[1]

W<-(n1-1)*S1+(n2-1)*S2+(n3-1)*S3+(n4-1)*S4+(n5-1)*S5
W


media_pop1<-apply(pop1, 2,mean)
media_pop2<-apply(pop2, 2,mean)
media_pop3<-apply(pop3, 2,mean)
media_pop4<-apply(pop4, 2,mean)
media_pop5<-apply(pop5, 2,mean)


media<-(n1*media_pop1+n2*media_pop2+n3*media_pop3+n4*media_pop4+n5*media_pop5 )/(n1+n2+n3+n4+n5)
media


B<-n1*(media_pop1-media)%*%t(media_pop1-media) +n2*(media_pop2-media)%*%t(media_pop2-media)+
n3*(media_pop3-media)%*%t(media_pop3-media)+n4*(media_pop4-media)%*%t(media_pop4-media)+
n5*(media_pop5-media)%*%t(media_pop5-media)
B

## Lambda de Wilks -----
lambda<-det(W)/det(W+B)
lambda
n<-n1+n2+n3+n4+n5
p<-dim(dados[1:4])[2]
g<-5
w<- n-1-((p+g)/2)
gl1<-p*(g-1)
gl1
ta<-sqrt((gl1^2-4)/(p^2+(g-1)^2-5))
gl2<-w*ta-(gl1/2)+1

valorF<-((1-lambda^(1/ta))/(lambda^(1/ta)))*(gl2/gl1)
valorF

pvalor<-pf(valorF, gl1, gl2,  lower.tail = FALSE)
pvalor

# _______________
WB<-solve(W)%*%B
# _______________


## Roy ----

gl1<-max(p,g-1)
gl2<-n-d-1

roy<-eigen(WB)$values[1] # calculando o autovalor
roy

valorF<-roy*(gl2/gl1)
valorF

pvalor<-pf(valorF, gl1, gl2,  lower.tail = FALSE)
pvalor


## Pillai ----

sa<-min(p,g-1)
gl2<-sa*(n-g-p+sa)
gl1<-sa*d

pilai<-eigen(WB)$values[1]/(1+eigen(WB)$values[1]) +
eigen(WB)$values[2]/(1+eigen(WB)$values[2])+
eigen(WB)$values[3]/(1+eigen(WB)$values[3])+
eigen(WB)$values[4]/(1+eigen(WB)$values[4])
pilai

valorF<-(pilai/(sa-pilai))*((n-g-p+sa)/d)
valorF

pvalor<-pf(valorF, gl1, gl2,  lower.tail = FALSE)
pvalor


## Lawley ----

U<-eigen(WB)$values[1] +eigen(WB)$values[2]+eigen(WB)$values[3]+
eigen(WB)$values[4]
U

A<-(abs(g-p-1)-1)/2
B<-(n-g-p-1)/2
gl1<-sa*(2*A+sa+1)
gl2<-2*(sa*B+1)

valorF<-(gl2*U)/(sa*gl1)
valorF

pvalor<-pf(valorF, gl1, gl2,  lower.tail = FALSE)
pvalor

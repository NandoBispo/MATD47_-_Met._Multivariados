

# Questão 1 ----
i_rad_sol=c(190, 118, 149, 313, 299, 99, 256, 290, 274, 65, 334, 307, 78, 322, 44, 8, 320, 25, 92, 13)

v_vento=c(7.4, 8, 12.6, 11.5, 8.6, 13.8, 9.7, 9.2, 10.9, 13.2, 11.5, 12, 18.4, 11.5, 9.7, 9.7, 16.6, 9.7, 12, 12)

temp=c(67, 72, 74, 62, 65, 59, 69, 66, 68, 58, 64, 66, 57, 68, 62, 59, 73, 61, 61, 67)

ozonio=c(41, 36, 12, 18, 23, 29, 16, 11, 14, 18, 14, 34, 6, 30, 11, 1, 11, 4, 32, 23)

quali_ar = base::cbind(i_rad_sol, v_vento, temp, ozonio)|>base::as.data.frame()

## Item a. ----

base::apply(X = quali_ar, MARGIN =  2, FUN = mean)
stats::cov(x = quali_ar, use = "all.obs", method = "pearson")|>round(2)

## Item b. ----
stats::cor(x = quali_ar, use = "all.obs", method = "pearson")|>round(2)


## Item c. ----

y1 = quali_ar$i_rad_sol*0.1 + quali_ar$ozonio
y2 = quali_ar$v_vento*2 - quali_ar$temp*5
y3 = quali_ar$v_vento*0.5 + quali_ar$v_vento*1.5 + quali_ar$temp - quali_ar$ozonio*2
y4 = quali_ar$i_rad_sol/120 + quali_ar$v_vento/2.7 + quali_ar$temp/5 - quali_ar$ozonio/15.5

Y = base::cbind(y1, y2, y3, y4)|>as.data.frame()

stats::cov(x = Y, use = "all.obs", method = "pearson")|>round(2)

stats::cor(x = Y, use = "all.obs", method = "pearson")|>round(2)

# Questão 2 ----







# Questão 3 ----









# Questão 4 ----

















# Questão 5 ----















# Questão 6 ----


















# Questão 7 ----











# Questão 15 ----
if (!require(pacman)) install.packages("pacman")
pacman::p_load(readxl,  janitor, tidyverse)

## Dados ----

notas <- readxl::read_excel("dados/dados_notas_lista1.xlsx")
View(notas)

dados <- janitor::clean_names(notas)
View(dados)


# Pacotes para teste de T2 de Hotelling ______________________
pacman::p_load(ICSNP,  rrcov, DescTools)

ICSNP::HotellingsT2(dados, mu=c(500,50,30),test = "chi")
ICSNP::HotellingsT2(dados, mu=c(500,50,30),test = "f")

rrcov::T2.test(dados, method = "c",mu=c(500,50,30),conf.int=T)

DescTools::HotellingsT2Test(dados,mu=c(500,50,30))
# ____________________________________________________________

# Obtendo o vetor de médias.
media=apply(dados,2,mean) # O comando colMeans obtêm o mesmo resultado mais rapidamente.

?colMeans
media=base::colMeans(x=dados,na.rm = F) 

media=matrix(media,3,1) # Transformando em vetor coluna.

S=stats::cov(dados) # Matriz de covariancia amostral.

S^{-1}

base::dim(dados)[1]
















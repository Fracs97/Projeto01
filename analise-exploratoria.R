library(data.table)
library(dplyr)
dados = fread(file='train_sample.csv')

head(dados)

#A coluna ip não é útil
dados[,'ip'] = NULL

#Verificando os tipos dos dados
str(dados)

#Todos os inteiros são na verdade fatores
dados = dados %>% mutate_if(is.integer,as.factor)

#Analisando cada variável, separando por classe
testa_var = function(variavel){
  sprintf('Verificando a variável %s',variavel)
  tapply(dados[,variavel],dados$is_attributed,function(x){prop.table(table(x))})
  chisq.test(dados[,variavel],dados$is_attributed)
  }



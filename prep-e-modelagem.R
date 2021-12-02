library(data.table)
library(dplyr)

#DATA MUNGING
dados = fread(file='dados_final.csv')

str(dados)

#Os inteiros são na verdade fatores
dados = dados %>% mutate_if(is.integer,as.factor)

#Verificando se há valores NA
sapply(dados,function(x)sum(is.na(x)))
#Não tem nenhum

#Verificando se há valores duplicados
sum(duplicated(dados))
#Quase tudo são valores duplicados. Já que todas as variáveis são categóricas,
#isso é aceitável
#Como essas repetições já foram analisadas na análise exploratória, posso remover 
#esses valores

#Mantendo só os valores únicos
dados = distinct(dados)
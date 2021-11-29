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

#Alterando os fatores da saída para facilitar a interpretação
levels(dados$is_attributed) = c('Não baixou','Baixou')

prop.table(table(dados$is_attributed))
#Os dados estão extremamente desbalanceados

#Função responsável por gerar um dataframe que compara as proporções das variáveis
#entre as classes
df_compara = function(agrupados,variavel){
  df1 = as.data.frame(agrupados$`Não baixou`)
  df2 = as.data.frame(agrupados$Baixou)$Freq
  colnames(df1) = c(variavel,'Freq_nao_baixou')
  compara = cbind(df1,Freq_baixou=df2)
}

#Analisando cada variável, separando por classe
#APP
agrupados = tapply(dados$app,dados$is_attributed,function(x){
  round(prop.table(table(x))*100,3)})

compara_app = df_compara(agrupados,'app')

#Testando a associação entre app e a saída
chisq.test(dados$app,dados$is_attributed)
#VERIFICAR SE HÁ RELAÇÃO ENTRE AS VARIÁVEIS

#DEVICE
agrupados = tapply(dados$device,dados$is_attributed,function(x){
  round(prop.table(table(x))*100,3)})

compara_device = df_compara(agrupados,'device')

#Testando a associação entre device e a saída
chisq.test(dados$device,dados$is_attributed)
#VERIFICAR SE HÁ RELAÇÃO ENTRE AS VARIÁVEIS

#COMENTAR SOBRE AS TABELAS










library(data.table)
library(dplyr)

#As coluna ip  e attributed_time não são úteis
#O número de observações no dataset teve de ser reduzido devido à limitações
#de memória ram, leitura original:
#dados = fread(file='train.csv',nrows=50000000,drop=c('ip','attributed_time'))

dados = fread(file='train_reduzido.csv')
head(dados)

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

#Analisando a proporção de cada variável, separando por classe

#APP
agrupados = tapply(dados$app,dados$is_attributed,function(x){
  round(prop.table(table(x))*100,3)})

compara_app = df_compara(agrupados,'app')
#Os cliques que se converteram em download se concentram nos apps 19,35,29,10 e 5
#E os que não resultaram em download se concentram nos apps 3,12,2,15,18,9 e 14

#Testando a associação entre app e is_attributed
chisq.test(dados$app,dados$is_attributed)
#p-value <2.2e-16, ou seja, há indícios de associação entre as variáveis

#DEVICE
agrupados = tapply(dados$device,dados$is_attributed,function(x){
  round(prop.table(table(x))*100,3)})

compara_device = df_compara(agrupados,'device')
#94% dos cliques sem download vieram do dispositivo 1
#Dos que baixaram, 71% vieram do dispositivo 1 e 19% do dispositivo 0

#Testando a associação entre device e a saída
chisq.test(dados$device,dados$is_attributed)
#p-value <2.2e-16, ou seja, há indícios de associação entre as variáveis

#OS
agrupados = tapply(dados$os,dados$is_attributed,function(x){
  round(prop.table(table(x))*100,3)})

compara_os = df_compara(agrupados,'os')
#Os que não baixaram possuem em maioria os os 19,13,17,18
#Os que baixaram têm os parecidos, sendo majoritariamente 19,13,24 e 0, isso pode 
#indicar que essa variável não é boa em separar os dados

chisq.test(dados$os,dados$is_attributed)
#Apesar da conclusão acima, o p-value é <2.2e-16, ou seja, há indícios de associação 
#entre as variáveis

#CHANNEL
agrupados = tapply(dados$channel,dados$is_attributed,function(x){
  round(prop.table(table(x))*100,3)})

compara_channel = df_compara(agrupados,'channel')
#No grupo dos que não baixaram o app, esta variável não se concentra em channels
#específicos, as maiores proporções são 7.5%, 5.2% e 5.2% para 280,245,107 respectivamente
#Já para os que baixaram, os dados se concentram em 213,113,21,274 e 101

chisq.test(dados$channel,dados$is_attributed)
#p-value <2.2e-16, ou seja, há indícios de associação entre as variáveis

#CLICK_TIME
#Extraindo o dia do mês, mês, dia da semana e o horário
dados %>% mutate(mes=month(click_time),dia=format(click_time,'%d'),dia_semana=weekdays(click_time)) %>% View()
#Não posso considerar o mês e dia, pois só aparecem os dias 6 e 7 do mês 11, pois
#isso levaria a conclusões enviesadas

unique(format(dados[c(1:20000000),'click_time'],'%m'))

library(data.table)
library(dplyr)
library(caret)
#DATA MUNGING
dados = fread(file='dados_final.csv')

str(dados)

#Os inteiros são na verdade fatores
dados = dados %>% mutate_if(is.integer,as.factor)

#Convertendo a classe em fator
dados$is_attributed = as.factor(dados$is_attributed)

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

#Nova proporção entre as classes
prop.table(table(dados$is_attributed))
#14.19% Baixaram e 85.8% Não baixaram

#MODELAGEM
#Separação em treino e teste
tt = createDataPartition(dados$is_attributed,p=0.7,list = F)
treino = dados[tt,]
teste = dados[-tt,]

#Trocando a posição da coluna classe pra facilitar na hora de chamar o predict
teste = teste[,c('is_attributed','app','device','os','channel')]

dim(treino);dim(teste)

#Testando modelos
modelo = train(is_attributed~.,treino,method='C5.0')
confusionMatrix(predict(modelo,teste[,-1]))

#rpart
modelo_rpart = rpart(is_attributed~.,data=treino)
previsoes = predict(modelo_rpart,teste[,-1],type='class')
confusionMatrix(as.data.frame(previsoes)$previsoes,teste$is_attributed)


length(predict(modelo_rpart,teste[,-1],type='class'))








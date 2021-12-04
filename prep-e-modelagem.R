library(data.table)
library(dplyr)
library(caret)
library(C50)
library(naivebayes)
library(class)
library(ROSE)
library(rpart)

#DATA MUNGING
dados = fread(file='dados_final.csv')

str(dados)

#A coluna click_time não é mais necessária
dados$click_time = NULL

#Ajustando o tipo das variáveis
dados = dados %>% mutate_at(c('app','device','os','channel','is_attributed'),
                            as.factor)

modelo = C5.0(is_attributed~.,data=dados)
C5imp(modelo)
#Um modelo do C5.0 mostrou que a relevância da variável minuto é muito baixa,além
#de deixar o modelo pouco generalizável
dados$minuto = NULL

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
teste = teste[,c('is_attributed','app','device','os','channel','hora')]
treino = treino[,c('is_attributed','app','device','os','channel','hora')]

dim(treino);dim(teste)

#Nesse estudo foi considerado que os Falsos Positivos (Quando o algoritmo diz que
#baixou o app, mas na verdade não baixou) são mais relevantes que os Falsos negativos
#Pois esse primeiro abre uma brecha para as fraudes

#rpart
modelo_rpart = rpart(is_attributed~.,data=treino)
previsoes_rpart = predict(modelo_rpart,teste[,-1],type='class')
confusionMatrix(as.data.frame(previsoes_rpart)[,1],teste$is_attributed)
#0% de acurácia na classe Baixou e 100% na classe Não baixou

#C5.0
custo = matrix(c(0,5,1,0),nr=2,dimnames = list(c('Baixou','Não baixou'),
                                               c('Baixou','Não baixou')))
modelo_C5.0 = C5.0(is_attributed~.,data=treino,costs=custo)
previsoes_C5.0 = predict(modelo_C5.0,teste[,-1],type='class')
confusionMatrix(as.data.frame(previsoes_C5.0)[,1],teste$is_attributed)
#82% acurácia na classe Baixou e 88% na classe Não baixou
roc.curve(teste$is_attributed,as.data.frame(previsoes_C5.0)[,1])

#Naive Bayes
modelo_nb = naive_bayes(is_attributed~.,data=treino,laplace=1)
previsoes_nb = predict(modelo_nb,teste[,-1])
confusionMatrix(as.data.frame(previsoes_nb)[,1],teste$is_attributed)
#52% acurácia na classe Baixou e 92% na classe Não baixou

#KNN
modelo_knn = knn(treino[,-1],teste[,-1],treino$is_attributed,k=3)
confusionMatrix(modelo_knn,teste$is_attributed)
#16% acurácia na classe Baixou e 97% na classe Não baixou

#BALANCEAMENTO
treino_b = ROSE(is_attributed~.,data=treino,p=0.4)$data
prop.table(table(treino_b$is_attributed))

#Treinando novamente, com os dados novos

#rpart
modelo_rpart = rpart(is_attributed~.,data=treino_b)
previsoes_rpart = predict(modelo_rpart,teste[,-1],type='class')
confusionMatrix(as.data.frame(previsoes_rpart)[,1],teste$is_attributed)
#74% de acurácia na classe Baixou e 88% na classe Não baixou
#Balanceamento com p=0.3

#C5.0
modelo_C5.0 = C5.0(is_attributed~.,data=treino_b)
previsoes_C5.0 = predict(modelo_C5.0,teste[,-1],type='class')
confusionMatrix(as.data.frame(previsoes_C5.0)[,1],teste$is_attributed)
#85% de acurácia na classe Baixou e 84% na classe Não baixou

#Naive Bayes
modelo_nb = naive_bayes(is_attributed~.,data=treino_b,laplace=1)
previsoes_nb = predict(modelo_nb,teste[,-1])
confusionMatrix(previsoes_nb,teste$is_attributed)
#69% de acurácia na classe Baixou e 88% na classe Não baixou

#KNN
modelo_knn = knn(treino_b[,-1],teste[,-1],treino_b$is_attributed,k=3)
confusionMatrix(modelo_knn,teste$is_attributed)
#68% de acurácia na classe Baixou e 82% na classe Não baixou

#Balanceamento não melhorou o modelo

#FEATURE SELECTION
summary(modelo_C5.0)
custo = matrix(c(0,5,1,0),nr=2,dimnames = list(c('Baixou','Não baixou'),
                                               c('Baixou','Não baixou')))
modelo_C5.0 = C5.0(is_attributed~.,data=treino,costs=custo)
previsoes_C5.0 = predict(modelo_C5.0,teste[,-1],type='class')
confusionMatrix(as.data.frame(previsoes_C5.0)[,1],teste$is_attributed)

#Feature Selection não melhorou o modelo

#TUNING
modelo_C5.0 = C5.0(is_attributed~.,data=treino,costs=custo,trials=20)
previsoes_C5.0 = predict(modelo_C5.0,teste[,-1],type='class')
confusionMatrix(as.data.frame(previsoes_C5.0)[,1],teste$is_attributed)



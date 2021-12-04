library(data.table)
library(dplyr)
library(caret)
library(C50)
library(naivebayes)
library(class)
library(ROSE)
library(rpart)
library(kernlab)
library(randomForest)

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
treino = treino[,c('is_attributed','app','device','os','channel')]

dim(treino);dim(teste)

#Nesse estudo foi considerado que os Falsos Positivos (Quando o algoritmo diz que
#baixou o app, mas na verdade não baixou) são mais relevantes que os Falsos negativos
#Pois esse primeiro abre uma brecha para as fraudes

#rpart
modelo_rpart = rpart(is_attributed~.,data=treino)
previsoes_rpart = predict(modelo_rpart,teste[,-1],type='class')
confusionMatrix(as.data.frame(previsoes_rpart)[,1],teste[,1])
#98.4% de acurácia na classe Baixou e 1.9% na classe Não baixou

#C5.0
custo = matrix(c(0,3,1,0),nr=2,dimnames = list(c('Baixou','Não baixou'),
                                               c('Baixou','Não baixou')))
modelo_C5.0 = C5.0(is_attributed~.,data=treino,cost=custo)
previsoes_C5.0 = predict(modelo_C5.0,teste[,-1],type='class')
confusionMatrix(as.data.frame(previsoes_C5.0)[,1],teste$is_attributed)
#79% acurácia na classe Baixou e 75.7% na classe Não baixou
roc.curve(teste$is_attributed,as.data.frame(previsoes_C5.0)[,1])

#Naive Bayes
modelo_nb = naive_bayes(is_attributed~.,data=treino,laplace=1)
previsoes_nb = predict(modelo_nb,teste[,-1])
confusionMatrix(as.data.frame(previsoes_nb)[,1],teste$is_attributed)
#47.5% acurácia na classe Baixou e 85.4% na classe Não baixou

#SVM
modelo_svm = ksvm(is_attributed~.,data=treino,type='C-svc',kernel='rbfdot',scaled=F)
previsoes_svm = predict(modelo_svm,teste[,-1])
confusionMatrix(previsoes_svm,teste$is_attributed)

#KNN
modelo_knn = knn(treino[,-1],teste[,-1],treino$is_attributed,k=3)
confusionMatrix(modelo_knn,teste$is_attributed)
#12.5% acurácia na classe Baixou e 91.6% na classe Não baixou

#BALANCEAMENTO
treino_b = ROSE(is_attributed~.,data=treino,p=0.3)$data
prop.table(table(treino_b$is_attributed))

#Treinando novamente, com os dados novos

#rpart
modelo_rpart = rpart(is_attributed~.,data=treino_b)
previsoes_rpart = predict(modelo_rpart,teste[,-1],type='class')
confusionMatrix(as.data.frame(previsoes_rpart)[,1],teste$is_attributed)
#82% de acurácia na classe Baixou e 71% na classe Não baixou
#Balanceamento com p=0.5

#C5.0
custo = matrix(c(0,2.5,1.5,0),nr=2,dimnames = list(c('Baixou','Não baixou'),
                                               c('Baixou','Não baixou')))
modelo_C5.0 = C5.0(is_attributed~.,data=treino_b,cost=custo)
previsoes_C5.0 = predict(modelo_C5.0,teste[,-1],type='class')
confusionMatrix(as.data.frame(previsoes_C5.0)[,1],teste$is_attributed)
#73.5% de acurácia na classe Baixou e 78% na classe Não baixou

#Naive Bayes
modelo_nb = naive_bayes(is_attributed~.,data=treino_b,laplace=1)
previsoes_nb = predict(modelo_nb,teste[,-1])
confusionMatrix(previsoes_nb,teste$is_attributed)
#67.9% de acurácia na classe Baixou e 77.9


#KNN
modelo_knn = knn(treino_b[,-1],teste[,-1],treino_b$is_attributed,k=3)
confusionMatrix(modelo_knn,teste$is_attributed)
#74.8% de acurácia na classe Baixou e 62.9% na classe Não baixou

#FEATURE SELECTION
summary(modelo_C5.0)
custo = matrix(c(0,3,1,0),nr=2,dimnames = list(c('Baixou','Não baixou'),
                                               c('Baixou','Não baixou')))
modelo_C5.0 = C5.0(is_attributed~app+os+channel+device,data=treino,cost=custo)
previsoes_C5.0 = predict(modelo_C5.0,teste[,-1],type='class')
confusionMatrix(as.data.frame(previsoes_C5.0)[,1],teste$is_attributed)
#79% acurácia na classe Baixou e 75.7% na classe Não baixou
roc.curve(teste$is_attributed,as.data.frame(previsoes_C5.0)[,1])

horas = fread(file='horas.csv')
dados = cbind(dados,horas)


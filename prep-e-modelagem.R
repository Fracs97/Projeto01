library(data.table)
library(dplyr)
library(caret)
library(C50)
library(naivebayes)
library(kernlab)
library(class)
library(ROSE)
library(rpart)

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

#rpart
modelo_rpart = rpart(is_attributed~.,data=treino)
previsoes_rpart = predict(modelo_rpart,teste[,-1],type='class')
confusionMatrix(as.data.frame(previsoes_rpart)[,1],teste$is_attributed)
#98.4% de acurácia na classe Baixou e 1.9% na classe Não baixou

#C5.0
custo = matrix(c(0,3,1,0),nr=2,dimnames = list(c('Baixou','Não baixou'),
                                               c('Baixou','Não baixou')))
modelo_C5.0 = C5.0(is_attributed~.,data=treino,cost=custo)
previsoes_C5.0 = predict(modelo_C5.0,teste[,-1],type='class')
confusionMatrix(as.data.frame(previsoes_C5.0)[,1],teste$is_attributed)
#79% acurácia na classe Baixou e 75.7% na classe Não baixou

#Naive Bayes
modelo_nb = naive_bayes(is_attributed~.,data=treino,laplace=1)
previsoes_nb = predict(modelo_nb,teste[,-1])
confusionMatrix(as.data.frame(previsoes_nb)[,1],teste$is_attributed)
#47.5% acurácia na classe Baixou e 85.4% na classe Não baixou

#Regressão logística
modelo_glm = glmtree(x=treino[,-1],y=treino$is_attributed)
previsoes_glm = predict(modelo_glm,teste[,-1])
confusionMatrix(previsoes_glm,teste$is_attributed)

#SVM
modelo_svm = ksvm(is_attributed~.,data=treino,type='C-svc',kernel='rbfdot',scaled=F)
previsoes_svm = predict(modelo_svm,teste[,-1])
confusionMatrix(previsoes_svm,teste$is_attributed)

#KNN
modelo_knn = knn(treino[,-1],teste[,-1],treino$is_attributed,k=3)
confusionMatrix(modelo_knn,teste$is_attributed)
#12.5% acurácia na classe Baixou e 91.6% na classe Não baixou

#BALANCEAMENTO
treino_b = ROSE(is_attributed~.,data=treino,p=0.5)$data
prop.table(table(treino_b$is_attributed))

#Treinando novamente, com os dados novos

#rpart
modelo_rpart = rpart(is_attributed~.,data=treino_b)
previsoes_rpart = predict(modelo_rpart,teste[,-1],type='class')
confusionMatrix(as.data.frame(previsoes_rpart)[,1],teste$is_attributed)
#91.5% de acurácia na classe Baixou e 60% na classe Não baixou

#C5.0
custo = matrix(c(0,0,3,0),nr=2,dimnames = list(c('Baixou','Não baixou'),
                                               c('Baixou','Não baixou')))
modelo_C5.0 = C5.0(is_attributed~.,data=treino_b,cost=custo)
previsoes_C5.0 = predict(modelo_C5.0,teste[,-1],type='class')
confusionMatrix(as.data.frame(previsoes_C5.0)[,1],teste$is_attributed)
#84% acurácia na classe Baixou e 71% na classe Não baixou

#Naive Bayes
modelo_nb = naive_bayes(is_attributed~.,data=treino_b,laplace=1)
previsoes_nb = predict(modelo_nb,teste[,-1])
confusionMatrix(as.data.frame(previsoes_nb)[,1],teste$is_attributed)
#47.5% acurácia na classe Baixou e 85.4% na classe Não baixou









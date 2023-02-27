#Modelo 5
#cargamos librerias
library(pacman)
p_load(tidyverse, rio,skimr,dplyr, caret, glmnet,smotefamily,ROSE, themis, AER, tidymodels)
p_load(AER, tidyverse, caret, MLmetrics, tidymodels, themis, smotefamily)

#cargamos nuestras bases
dvf_test <- readRDS("~/Desktop/git hut repositorios/Problem_Set_2/3. STORE/dvf_test.rds")
test <- dvf_test
dvf_train <- readRDS("~/Desktop/git hut repositorios/Problem_Set_2/3. STORE/dvf_train.rds")
train<- dvf_train
#platamos nuestra semilla
set.seed(123) 

# Crear matriz de predictores y vector de respuesta para el conjunto de entrenamiento
x_train <- dvf_train[, -which(names(dvf_train) == "Pobre.1")]
y_train <- dvf_train$Pobre.1
x_test <- dvf_test

#logit con Elatic Net
modelo5<- train(x =x_train,
                y= y_train,
                preProcess=NULL,
                method = "glmnet")
modelo5

#predecimos 
insample<- predict(modelo5, dvf_train)
outsample<- predict(modelo5, dvf_test)

#analisamos su acc
acc_insample <- Accuracy(y_pred= insample, y_true= train$Pobre.1)
acc_insample 

#analisamos precision
pre_insample <- Precision(y_pred= insample, y_true= train$Pobre.1, positive = 1)
pre_insample 

#analisamos recall
rec_insample <- Recall(y_pred= insample, y_true= train$Pobre.1, positive = 1)
rec_insample 

#analisamos f1
f1_insample <- F1_Score(y_pred= insample, y_true= train$Pobre.1, positive = 1)
f1_insample

metricas_insample1 <- data.frame(Modelo = "Regresión logistica Elastic Net", 
                                 "Muestreo" = NA, 
                                 "Evaluación" = "Dentro de muestra",
                                 "Accuracy" = acc_insample,
                                 "Precision - PPV" = pre_insample,
                                 "Recall - TPR - Sensitivity" = rec_insample,
                                 "F1" = f1_insample)

#########
#balanceamos la muestra 
names(dvf_train) <- make.names(names(dvf_train))
dvf_train_balanced <- ROSE(Pobre.1 ~ ., data = dvf_train, seed = 123, p = 0.4)$data

###dejamos como factores
dvf_train$Pobre.1 = as.factor(dvf_train$Pobre.1)
dvf_train_balanced$Pobre.1 = as.factor(dvf_train_balanced$Pobre.1)

#analisis para construccion de la variable dependiente ahora es balanceada
prop.table(table(dvf_train_balanced$Pobre.1))*100

#Modelo con la base balanceada 
x_trainb <- dvf_train_balanced[, -which(names(dvf_train_balanced) == "Pobre.1")]
y_trainb <- dvf_train_balanced$Pobre.1
x_testb <- dvf_test

modelo6<- train(x =x_trainb,
                y= y_trainb,
                preProcess=NULL,
                method = "glmnet")
modelo6
#predecimos 
insampleb<- predict(modelo6, dvf_train)

#analisamos su acc
acc_insampleb <- Accuracy(y_pred= insampleb, y_true= train$Pobre.1)
acc_insampleb 

#analisamos precision
pre_insampleb <- Precision(y_pred= insampleb, y_true= train$Pobre.1, positive = 1)
pre_insampleb 

#analisamos recall
rec_insampleb <- Recall(y_pred= insampleb, y_true= train$Pobre.1, positive = 1)
rec_insampleb 

#analisamos f1
f1_insampleb <- F1_Score(y_pred= insampleb, y_true= train$Pobre.1, positive = 1)
f1_insampleb 

metricas_insample2 <- data.frame(Modelo = "Regresión logistica Elastic Net", 
                                 "Muestreo" = NA, 
                                 "Evaluación" = "Dentro de muestra",
                                 "Accuracy" = acc_insampleb,
                                 "Precision - PPV" = pre_insampleb,
                                 "Recall - TPR - Sensitivity" = rec_insampleb,
                                 "F1" = f1_insampleb)


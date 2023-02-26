#Modelo Arbol
#llamamos librerias
library(pacman)
p_load(tidyverse, rio,skimr,dplyr, caret, glmnet,smotefamily,ROSE,rattle, MLmetrics,rattle, ranger)
#cargamos nuestas bases 

dvf_test <- readRDS("~/Desktop/git hut repositorios/Problem_Set_2/3. STORE/dvf_test.rds")
test<- dvf_test
dvf_train <- readRDS("~/Desktop/git hut repositorios/Problem_Set_2/3. STORE/dvf_train.rds")
train <- dvf_train
#plantamos nuestra semillas
set.seed(123) 

#participaciones para cross validation
cv5 <- trainControl(number = 5, method = "cv")
cv3 <- trainControl(number = 3, method = "cv")

# Crear matriz de predictores y vector de respuesta para el conjunto de entrenamiento
x_train <- dvf_train[, -which(names(dvf_train) == "Pobre.1")]
y_train <- dvf_train$Pobre.1
x_test <- dvf_test

#deajmos que se escoja el mejor modelo posible 
clasifiction_a5 <- train(as.factor(Pobre.1) ~ .,
data = dvf_train, 
method = "rpart", 
trControl = cv5)

#grafico arbol de decisión
fancyRpartPlot(modelo1$finalModel)

#predecimos 
insample<- predict(clasifiction_a5 , dvf_train)
outsample<- predict(clasifiction_a5 , dvf_test)

#calulamos las metricas fuera y dentro de muestra

MAE(y_pred = y_hat_insample1, y_true = dvf_train$Pobre.1)
MAPE(y_pred = y_hat_insample1, y_true = dvf_train$Pobre.1)
MAE(y_pred = y_hat_outsample1, y_true = dvftest$Pobre.1)
MAPE(y_pred = y_hat_outsample1, y_true = dvftest$Pobre.1)


#realizamos ramdom forest

# Creamos una grilla

tunegrid_rf <- expand.grid(mtry = c(3, 5, 10), 
                           min.node.size = c(10, 30, 50,
                                             70, 100),
                           splitrule = "variance")

clasifiction_a6  <- train(y_train ~ .,
                 data = dvf_train, 
                 method = "ranger", 
                 trControl = cv5,
                 metric = 'RMSE', 
                 tuneGrid = tunegrid_rf)





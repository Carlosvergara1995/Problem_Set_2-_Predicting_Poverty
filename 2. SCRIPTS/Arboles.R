#Modelo Arbol
#llamamos librerias
library(pacman)
p_load(tidyverse, rio,skimr,dplyr, caret, glmnet,smotefamily,ROSE,rattle, MLmetrics)
#cargamos nuestas bases 

dvf_test <- readRDS("~/Desktop/git hut repositorios/Problem_Set_2/3. STORE/dvf_test.rds")
dvf_train <- readRDS("~/Desktop/git hut repositorios/Problem_Set_2/3. STORE/dvf_train.rds")

#plantamos nuestra semillas
set.seed(123) 

#deajmos que se escoja el mejor modelo posible 
clasifiction_a5 <- train(Pobre.1 ~ .,
data = dvf_train, 
method = "rpart", 
trControl = cv5)

#grafico arbol de decisión
fancyRpartPlot(modelo1$finalModel)

#predecimos 
insample<- predict(clasifiction_a5 , dvf_train)
outsample<- predict(clasifiction_a5 , dvf_test)

#calulamos las metricas fuera y dentro de muestra

AE(y_pred = y_hat_insample1, y_true = train_df$wage)












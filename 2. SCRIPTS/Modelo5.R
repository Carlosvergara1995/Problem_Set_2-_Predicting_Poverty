#Modelo 5
#cargamos librerias
library(pacman)
p_load(tidyverse, rio,skimr,dplyr, caret, glmnet,smotefamily,ROSE)

set.seed(123) 

#logit con lasticnet REVISAR NOMBRWE
modelo5<- train(x = select(db_train, -Pobre)
                y= as.factor(db_train$Pobre),
                preProcess=NULL,
                method = "glmnet")
modelo5

#predecimos 
insample<- predict(modelo5, db_train)
outsample<- predict(modelo5, db_test)
#Cargamos nuestras librearias 
require(pacman)
p_load(tidyverse, rio,skimr,dplyr, caret, glmnet,smotefamily,ROSE,caTools)
update.packages("caret")

#Cargamos la bases ensambladas finales de datos
setwd('data')

#Cargamos datos test y train
df_test<- import("nv_test_sinna.rds") 
df_train<- import("nv_training_sinna.rds") 


#analisis para construccion de la variable dependiente
prop.table(table(df_train$Pobre))*100
#un 20% es levemente desbalanceada

# Eliminar las columnas que no se necesitan para el analisis
df_train  <- subset(df_train, select = -c(Clase,id,Depto, Dominio))
df_test  <-  subset(df_test,  select = -c(Clase,id,Depto, Dominio))

###Cambiamos factores
df_train$P5090 <- factor(df_train$P5090)
df_test$P5090 <- factor(df_test$P5090)



df_train$Pobre <- factor(df_train$Pobre)


###Creamos las dummies - - - Dummyficamos 
dumificador <- dummyVars(formula = ~ ., data = df_train, fullRank = T)
db_train <- predict(dumificador, newdata = df_train)
db_train <- as.data.frame(db_train)

dumificador <- dummyVars(formula = ~ ., data = df_test, fullRank = T)
db_test <- predict(dumificador, newdata = df_test)
db_test <- as.data.frame(db_test)



# Eliminar la variable con desviación estándar igual a cero
db_train <- db_train[, apply(db_train, 2, sd) != 0]

#####Eliminamos columna por multicolinealidad######
matriz_cor <- cor(db_train)
columnas_correlacionadas <- findCorrelation(matriz_cor, cutoff = 0.8)
db_train  <- subset(db_train, select = -columnas_correlacionadas)

# Seleccionar las columnas que están en ambos datasets y la columna Pobre
cols <- c(intersect(names(db_train), names(db_test)), "Pobre.1")
db_train <- db_train[, cols]
cols <- c(intersect(names(db_train), names(db_test)))
db_test <- db_test[, cols]





####Modelo de clasifiacion 1  con data sin balancear########


###########

set.seed(123) # Establecer una semilla para reproducibilidad
train_index <- sample.split(db_train, SplitRatio = 0.7) # Generar vector lógico
train <- db_train[train_index, ] # Conjunto de entrenamiento
test <- db_train[!train_index, ] # Conjunto de prueba

# Crear matriz de predictores y vector de respuesta para el conjunto de entrenamiento
set.seed(666) # Fijar semilla para reproducibilidad
x_train <- train[, -which(names(db_train) == "Pobre.1")]
y_train <- train$Pobre.1

x_test <- test[, -which(names(db_train) == "Pobre.1")]
y_test <- test$Pobre.1




###Con rose balanceamos la data de train
names(train) <- make.names(names(train))
db_train_balanced <- ROSE(Pobre.1 ~ ., data = train, seed = 123, p = 0.4)$data

###dejamos como factores
db_train$Pobre.1 = as.factor(db_train$Pobre.1)
db_train_balanced$Pobre.1 = as.factor(db_train_balanced$Pobre.1)

#analisis para construccion de la variable dependiente ahora es balanceada
prop.table(table(db_train_balanced$Pobre.1))*100



####Balanceada
x_train_b <- db_train_balanced[, -which(names(db_train_balanced) == "Pobre.1")]
y_train_b <- db_train_balanced$Pobre.1

# Check levels of outcome variable
y_train <- make.names(y_train)
y_train_b <- make.names(y_train_b)

# Crear modelo con regresión logística y Lasso

tune_grid <- expand.grid(alpha = seq(0, 1, length = 5),
                         lambda = seq(0.001, 1, length = 10))
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...)) 
ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats, 
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

set.seed(1410)
##########MODELO DESBALANCEADO ############
modelo_1 <- train(x_train, y_train, 
                                method = "glmnet",
                                trControl = ctrl,
                                family = "binomial", 
                                metric = "ROC",
                                tuneGrid = tune_grid, 
                                preProcess = c("center", "scale")
)
print(modelo_1)

###########MODELO BALANCEADO ############
modelo_balanceado <- train(x_train_b, y_train_b, 
                  method = "glmnet",
                  trControl = ctrl,
                  family = "binomial", 
                  metric = "ROC",
                  tuneGrid = tune_grid, 
                  preProcess = c("center", "scale")
)
print(modelo_balanceado)


# Seleccionar el mejor modelo basados en la mejor ROC

parametros_des_ROC <- modelo_1$results[which.max(modelo_1$results$ROC),]
parametros_des_Accuracy <- modelo_1$results[which.max(modelo_1$results$Accuracy),]
parametros_des_Sens <- modelo_1$results[which.max(modelo_1$results$Sens),]

parametros_bal_ROC <- modelo_balanceado$results[which.max(modelo_balanceado$results$ROC),]
parametros_bal_Accuracy <- modelo_balanceado$results[which.max(modelo_balanceado$results$Accuracy),]
parametros_bal_Sens <- modelo_balanceado$results[which.max(modelo_balanceado$results$Sens),]

########### Hacer la predicción #################
# Crear el modelo de clasificación final utilizando el mejor valor de alpha y lambda
modelo_des_ROC <-      glmnet(x_train, y_train, alpha = parametros_des_ROC$alpha, lambda = parametros_des_ROC$lambda, family = "binomial")
modelo_des_Accuracy <- glmnet(x_train, y_train, alpha = parametros_des_Accuracy$alpha, lambda = parametros_des_Accuracy$lambda, family = "binomial")
modelo_des_Sens <-        glmnet(x_train, y_train, alpha = parametros_des_Sens$alpha, lambda = parametros_des_Sens$lambda, family = "binomial")

modelo_bal_ROC <-      glmnet(x_train_b, y_train_b, alpha = parametros_bal_ROC$alpha, lambda = parametros_bal_ROC$lambda, family = "binomial")
modelo_bal_Accuracy <- glmnet(x_train_b, y_train_b, alpha = parametros_bal_Accuracy$alpha, lambda = parametros_bal_Accuracy$lambda, family = "binomial")
modelo_bal_Sens <-     glmnet(x_train_b, y_train_b, alpha = parametros_bal_Sens$alpha, lambda = parametros_bal_Sens$lambda, family = "binomial")




# Hacer las predicciones con el modelo 
Pobre_des_ROC <- ifelse(predict(modelo_des_ROC, newx = as.matrix(x_test), type = "response") >= 0.5, 1, 0)
Pobre_des_Accuracy <- ifelse(predict(modelo_des_Accuracy, newx = as.matrix(x_test), type = "response") >= 0.5, 1, 0)
Pobre_des_Sens <- ifelse(predict(modelo_des_Sens, newx = as.matrix(x_test), type = "response") >= 0.5, 1, 0)

Pobre_bal_ROC <- ifelse(predict(modelo_bal_ROC, newx = as.matrix(x_test), type = "response") >= 0.5, 1, 0)
Pobre_bal_Accuracy <- ifelse(predict(modelo_bal_Accuracy, newx = as.matrix(x_test), type = "response") >= 0.5, 1, 0)
Pobre_bal_Sens <- ifelse(predict(modelo_bal_Sens, newx = as.matrix(x_test), type = "response") >= 0.5, 1, 0)

#########Calculamos la matriz de confusion
Pobre_des_ROC <- as.factor(Pobre_des_ROC)
Pobre_des_Accuracy <- as.factor(Pobre_des_Accuracy)
Pobre_des_Sens <- as.factor(Pobre_des_Sens)

Pobre_bal_ROC <- as.factor(Pobre_bal_ROC)
Pobre_bal_Accuracy <- as.factor(Pobre_bal_Accuracy)
Pobre_bal_Sens <- as.factor(Pobre_bal_Sens)
y_test <- as.factor(y_test)

#Desbalanceada

#####ROC
confusion_des_ROC     <- confusionMatrix(Pobre_des_ROC, y_test)
accurracy_des_ROC <- sum(diag(confusion_des_ROC$table)) / sum(confusion_des_ROC$table)
sensibilidad_des_ROC <-sensitivity(confusion_des_ROC$table, confusion_des_ROC$positive)

###acurracy
confusion_des_Accuracy     <- confusionMatrix(Pobre_des_Accuracy, y_test)
accurracy_des_Accuracy <- sum(diag(confusion_des_Accuracy$table)) / sum(confusion_des_Accuracy$table)
sensibilidad_des_Accuracy <-sensitivity(confusion_des_Accuracy$table, confusion_des_Accuracy$positive)

####sens
confusion_des_Sens     <- confusionMatrix(Pobre_des_Sens, y_test)
accurracy_des_Sens <- sum(diag(confusion_des_Sens$table)) / sum(confusion_des_Sens$table)
sensibilidad_des_Sens <-sensitivity(confusion_des_Sens$table, confusion_des_Sens$positive)

################BALANCEADA
#####ROC
confusion_bal_ROC     <- confusionMatrix(Pobre_bal_ROC, y_test)
accurracy_bal_ROC <- sum(diag(confusion_bal_ROC$table)) / sum(confusion_bal_ROC$table)
sensibilidad_bal_ROC <-sensitivity(confusion_bal_ROC$table, confusion_bal_ROC$positive)

###acurracy
confusion_bal_Accuracy     <- confusionMatrix(Pobre_bal_Accuracy, y_test)
accurracy_bal_Accuracy <- sum(diag(confusion_bal_Accuracy$table)) / sum(confusion_bal_Accuracy$table)
sensibilidad_bal_Accuracy <-sensitivity(confusion_bal_Accuracy$table, confusion_bal_Accuracy$positive)

####sens
confusion_bal_Sens     <- confusionMatrix(Pobre_bal_Sens, y_test)
accurracy_bal_Sens <- sum(diag(confusion_bal_Sens$table)) / sum(confusion_bal_Sens$table)
sensibilidad_bal_Sens <-sensitivity(confusion_bal_Sens$table, confusion_bal_Sens$positive)


###############Por fin resultados

Modelo <- c("maximo Roc desbalanceado","maximo Accuracy desbalanceado","maximo sens desbalanceado",
            "maximo Roc balanceado","maximo Accuracy balanceado","maximo sens balanceado")
Accuracy <- c(accurracy_des_ROC,accurracy_des_Accuracy,accurracy_des_Sens,
              accurracy_bal_ROC,accurracy_bal_Accuracy,accurracy_bal_Sens)
Sensibilidad <- c(sensibilidad_des_ROC,sensibilidad_des_Accuracy,sensibilidad_des_Sens,
                  sensibilidad_bal_ROC,sensibilidad_bal_Accuracy,sensibilidad_bal_Sens)

resultado <- data.frame(Modelo, Accuracy, Sensibilidad)
resultado


###########Seleccionamos el modelo con mayor accuracy desbalanceada y balanceada

########### Hacer la predicción #################


# Hacer las predicciones con el modelo 
x_test <- db_test
Pobre_des <- ifelse(predict(modelo_des_ROC, newx = as.matrix(x_test), type = "response") >= 0.5, 1, 0)
Pobre_bal <- ifelse(predict(modelo_bal_ROC, newx = as.matrix(x_test), type = "response") >= 0.5, 1, 0)

########Importamos
test<- import("test_sinna.rds") 
Modelo1_con_grid_search <- data.frame(id = test$id, Pobre = Pobre_des)
Modelo1_con_grid_search_bal <- data.frame(id = test$id, Pobre = Pobre_bal)

#### vemos porcentajes
print("SIN BALANCEAR")
tabla_frecuencia <- table(Modelo1_con_grid_search$s0)
prop.table(tabla_frecuencia)

print(" BALANCEADO")
tabla_frecuencia <- table(Modelo1_con_grid_search_bal$s0)
prop.table(tabla_frecuencia)

###exportamos
write.csv(Modelo1_con_grid_search, "Modelo1_con_grid_search.csv", row.names = FALSE)
write.csv(Modelo1_con_grid_search_bal, "Modelo1_con_grid_search_bal.csv", row.names = FALSE)




resultados_ordenados <- modelo_1$results %>%
  arrange(desc(Sens))

resultados_ordenados

resultados_ordenados <- modelo_balanceado$results %>%
  arrange(desc(Sens))
resultados_ordenados






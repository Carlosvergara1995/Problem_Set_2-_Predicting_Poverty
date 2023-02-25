#Cargamos nuestras librearias 
require(pacman)
p_load(tidyverse, rio,skimr,dplyr, caret, glmnet,smotefamily,ROSE)

#Cargamos la bases ensambladas finales de datos
setwd('data')

#Cargamos datos test y train
df_test<- import("nv_test_sinna.rds") 
df_train<- import("nv_training_sinna.rds") 


#analisis para construccion de la variable dependiente
prop.table(table(df_train$Pobre))*100
#un 20% es levemente desbalanceada

# Eliminar las columnas que no se necesitan para el analisis
df_train  <- subset(df_train, select = -c(Clase,id,Depto))
df_test  <-  subset(df_test,  select = -c(Clase,id,Depto))

###Cambiamos factores
df_train$P5090 <- factor(df_train$P5090)
df_test$P5090 <- factor(df_test$P5090)

df_train$Dominio <- factor(df_train$Dominio)
df_test$Dominio <- factor(df_test$Dominio)

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

###Con rose balanceamos la data
names(db_train) <- make.names(names(db_train))
db_train_balanced <- ROSE(Pobre.1 ~ ., data = db_train, seed = 123, p = 0.4)$data

###dejamos como factores
db_train$Pobre.1 = as.factor(db_train$Pobre.1)
db_train_balanced$Pobre.1 = as.factor(db_train_balanced$Pobre.1)

#analisis para construccion de la variable dependiente ahora es balanceada
prop.table(table(db_train_balanced$Pobre.1))*100



####Modelo de clasifiacion 1  con data sin balancear########



# Crear matriz de predictores y vector de respuesta para el conjunto de entrenamiento
set.seed(666) # Fijar semilla para reproducibilidad
x_train <- db_train[, -which(names(db_train) == "Pobre.1")]
y_train <- db_train$Pobre.1
x_test <- db_test

####Balanceada
x_train_b <- db_train_balanced[, -which(names(db_train_balanced) == "Pobre.1")]
y_train_b <- db_train_balanced$Pobre.1

# Check levels of outcome variable
y_train <- make.names(y_train)
y_train_b <- make.names(y_train_b)

# Crear modelo con regresión logística y Lasso

tune_grid <- expand.grid(alpha = 0:1,
                         lambda = seq(0.001, 1, length = 15))
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

modelo_T1 <- modelo_1$results[which.max(modelo_1$results$ROC),]
modelo_bal <- modelo_balanceado$results[which.max(modelo_balanceado$results$ROC),]

########### Hacer la predicción #################
# Crear el modelo de clasificación final utilizando el mejor valor de alpha y lambda
modelo_pre1 <- glmnet(x_train, y_train, alpha = modelo_T1$alpha, lambda = modelo_T1$lambda, family = "binomial")
modelo_bal <- glmnet(x_train_b, y_train_b, alpha = modelo_bal$alpha, lambda = modelo_bal$lambda, family = "binomial")

# Hacer las predicciones con el modelo 
Pobre_des <- ifelse(predict(modelo_pre1, newx = as.matrix(x_test), type = "response") >= 0.5, 1, 0)
Pobre_bal <- ifelse(predict(modelo_bal, newx = as.matrix(x_test), type = "response") >= 0.5, 1, 0)

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









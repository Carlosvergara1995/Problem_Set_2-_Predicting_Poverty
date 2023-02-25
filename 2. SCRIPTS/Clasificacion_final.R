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
db_test <- db_train[, cols]

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


# Crear modelo con regresión logística y Lasso

tune_grid <- expand.grid(alpha = 0:1,
                         lambda = seq(0.001, 1, length = 10))

modelo_1 <- train(x_train, y_train,
               method = "glmnet",
               trControl = trainControl(method = "cv", number = 5),
               family = "binomial", 
               tuneGrid = tune_grid,
               metric = "Spec ",
               reProcess = c("center", "scale"))

print(modelo_1)
model$results

# Seleccionar el mejor modelo basado en la menor tasa de error de clasificación en el conjunto de prueba

mejor_modelo <- modelo_1$results[which.max(modelo_1$results$Accuracy),]


########### Hacer la predicción #################
# Crear el modelo de clasificación final utilizando el mejor valor de alpha y lambda

modelo_final <- glmnet(x_train, y_train, alpha = mejor_modelo$alpha, lambda = mejor_modelo$lambda, family = "binomial")


# Hacer las predicciones con el modelo ajustado
pobre1 <- ifelse(predict(modelo_final, newx = as.matrix(x_test), type = "response") >= 0.5, 1, 0)

test<- import("test_sinna.rds") 

Respuesta_lasso_grid_serch <- data.frame(id = test$id, pobre = pobre1)

#### vemos porcentajes

tabla_frecuencia <- table(Respuesta_lasso_grid_serch$s0)
prop.table(tabla_frecuencia)


###exportamos

write.csv(Respuesta_lasso_grid_serch, "Respuesta_lasso_grid_search.csv", row.names = FALSE)









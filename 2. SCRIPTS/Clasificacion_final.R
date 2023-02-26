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



############ REGRESION LINEAL ############



#######prueba


# Separar los datos en variables predictoras (X) y variable respuesta (y)
df_train_lineal <- df_train[, -which(names(df_train) == "Pobre")]


# Convertir las variables categóricas en variables dummy

dumificador <- dummyVars(formula = ~ ., data = df_train_lineal, fullRank = T)
df_train_lineal <- predict(dumificador, newdata = df_train_lineal)
df_train_lineal <- as.data.frame(df_train_lineal)

# Dividir los datos en entrenamiento y validación
set.seed(123)
train_idx <- sample(nrow(df_train_lineal), 0.8*nrow(df_train_lineal))
df_train_lineal_train <- df_train_lineal[train_idx,]
df_train_lineal_test <- df_train_lineal[-train_idx,]


# Definir la secuencia de valores de lambda
lambda_seq <- 10^seq(10, -2, length = 20)

# Ajustar modelos para cada valor de lambda y calcular el AIC correspondiente
# Obtener nombres de variables numéricas





AIC_values <- numeric(length = length(lambda_seq))
for (i in seq_along(lambda_seq)) {
  model <- glmnet(df_train_lineal_train[, -which(names(df_train_lineal_train) == "Lp")], df_train_lineal_train$Lp, alpha = 0.5, lambda = lambda_seq[i])
  model_glm <- coef(model, s = "lambda.min")[-1]
  AIC_values[i] <- AIC(glm(Lp ~ ., data = df_train_lineal, family = "gaussian", weights = NULL, model = model_glm))
}




# Seleccionar el valor de lambda con el menor AIC
best_lambda <- lambda_seq[which.min(AIC_values)]

# Obtener los coeficientes del modelo con el mejor valor de lambda
best_model <- glmnet(X_train, y_train, alpha = 0.5, lambda = best_lambda)

# Obtener las predicciones en el conjunto de validación
pred <- predict(best_model, newx = df_train_lineal_test)

# Calcular el AIC del modelo
AIC <- AIC(best_model)



set.seed(123)
fitControl <- trainControl(## 5-fold CV, 10 better
  method = "cv",
  number = 5)

fmla<-formula(Lp~.)
EN<-train(fmla,
          data=df_train_lineal_train,
          method = 'glmnet', 
          trControl = fitControl,
          tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1), #grilla de alpha
                                 lambda = seq(0.001,0.02,by = 0.001)),
          preProcess = c("center", "scale")
) 


EN$bestTune

coef_EN<-coef(EN$finalModel, EN$bestTune$lambda)
coef_EN



resultados_ordenados <- modelo_1$results %>%
  arrange(desc(ROC))

resultados_ordenados

resultados_ordenados <- modelo_balanceado$results %>%
  arrange(desc(roc))

resultados_ordenados
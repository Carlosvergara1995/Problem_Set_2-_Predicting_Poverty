#Cargamos nuestras librearias 
require(pacman)
p_load(tidyverse, rio,skimr,dplyr, caret, glmnet,smotefamily,ROSE)

#Cargamos la bases ensambladas finales de datos
setwd('data')

#base training 
training<- import("df_training_hogares_VF.rds") 
training_1<- training


# Calcular la cantidad de valores NA en cada columna
na_count <- colSums(is.na(training))

# Calcular el porcentaje de valores NA en cada columna
na_percentage <- na_count / nrow(training) * 100

# Imprimir solo las columnas con valores NA mayores a cero
if(any(na_count > 0)) {

  print(na_percentage[na_count > 0])
} else {
  print("No hay valores NA en el dataframe")
}

######Eliminamos las columnas que tengan mas del 20% en na

training <- training[, colMeans(is.na(training)) < 0.2]

#base test
test <- import("df_test_hogares_VF.rds")
test_1 <- test

#analisis de estructura de nuestra base
skim(training)
summary(training)

#analisis para construccion de la variable dependiente
table(training$Pobre, useNA = "always")
table(training$Indigente, useNA = "always")

# Analisis na para training####
attach(training)
# Analisis de na por variable
na_count<-colSums(is.na(training))
na_cols <- na_count[na_count > 0]
na_cols

#encontramos que las variables que contienne na son P5100, P5130,  P5140, cuota_amortizacion, arriendo, edu_promedio, horas_trabajadas_promedio,
# las cuales corresponden a Â¿CuÃ¡nto pagan mensualmente por cuota de amortizaciÃ³n?; Si tuviera que pagar arriendo por esta vivienda,
# Â¿cuÃ¡nto estima que tendrÃ­a que pagar mensualmente?; Â¿ CuÃ¡nto pagan mensualmente por arriendo ?; cuota de amortizaciÃ³n; 
#analizamos el porcentaje de nas presente en cada variable 

# Calcular el porcentaje de valores NA por columna
porcentaje_nulos_por_columna <- na_count / nrow(training) * 100

# Seleccionar solo las variables con NA
variables_con_na <- names(porcentaje_nulos_por_columna[porcentaje_nulos_por_columna > 0])
#resultados
print(paste("Variables con NA:", paste(variables_con_na, collapse = ", ")))
print(paste("Porcentaje de NA por variable:"))
print(porcentaje_nulos_por_columna[porcentaje_nulos_por_columna > 0])

#encontramos que la variable P5100 y cuota_amortizaciÃ³n se ecnuentran casi completanmente vacias con un por centaje de na del 96%
# por otro lado las demas variables con na tambien presentan valores relativamente grandes de nas eceptuando a horas_trabajadas_promedio y edu_promedio
# dados los resultados anteriores y las demas variables presentes en la base procederemos a realizar nuestro analisis sin ellas 
# reemplazandolas con otras varibales, menos en el caso de edu_promedio que solo presenta 1 na el cual sera reemplazado por su promedio

#codificamos nuestra variable edu_promedio dado que solo tiene un na lo reemplazaresmos con el promedio y eliminamos las variabbles con altos porcentajes de nas
#calculamos el promedio de la variable
media_edu_promedio <- mean(training$edu_promedio, na.rm = TRUE)


#se realiza el mismo procedimiento en test
attach(test)
na_count_test<-colSums(is.na(test))
na_cols_test <- na_count_test[na_count_test > 0]
na_cols_test
#encontramos que las mismas variables presentan nas aunque con diferentes cifras a los de training

# Calcular el porcentaje de valores NA por columna
porcentaje_nulos_por_columna_t <- na_count_test / nrow(test) * 100

# Seleccionar solo las variables con NA
variables_con_na_t <- names(porcentaje_nulos_por_columna_t[porcentaje_nulos_por_columna_t > 0])
#resultados
print(paste("Variables con NA:", paste(variables_con_na_t, collapse = ", ")))
print(paste("Porcentaje de NA por variable:"))
print(porcentaje_nulos_por_columna_t[porcentaje_nulos_por_columna_t > 0])

#codificamos nuestra variable edu_promedio dado que solo tiene un na lo reemplazaresmos con el promedio y eliminamos las variabbles con altos porcentajes de nas
#calculamos el promedio de la variable

###OJO ACA SE HACE CON EL TRAIN###############

media_edu_promediot <- mean(training$edu_promedio, na.rm = TRUE)
variables_eliminart <- c('P5100', 'P5130',  'P5140', 'cuota_amortizacion', 'arriendo', 'horas_trabajadas_promedio')
test<- test %>%
  mutate( edu_promedio = ifelse(is.na(edu_promedio), media_edu_promediot, edu_promedio))%>%
  select(-variables_eliminar)

#creamos nuestra variable dependiente para predecir la probleza en los hogares ####

training$pobreza<- ifelse(training$Pobre == 1 | training$Indigente == 1, 1, 0)

skim(training$pobreza)

pobrezatabla <- table(training$pobreza, useNA = "always")
pobrezatabla

#encontramos que de las  164960 observaciones 33024 hogares se encuentran en estado de pobreza o indigencia 

saveRDS(test, "test_sinna.rds")
saveRDS(training, "training_sinna.rds")


test<- import("test_sinna.rds") 
training<- import("training_sinna.rds")
df_test <- test
df_train <-na.omit(training)

####Vemos los datos de pobreza

prop.table(table(df_train$pobreza))*100

prop.table(table(df_train$Pobre))*100

##################POBREZA Y POBRE SON LO MISMO

df_train$Pobre <- NULL

##Dominio y pobreza es categorica

df_train <-  df_train %>%
        mutate(Dominio=factor(Dominio))

df_test <-  df_test %>%
  mutate(Dominio=factor(Dominio))

df_train$pobreza <- factor(df_train$pobreza, levels = c(0, 1))

#un 20% es levemente desbalanceada

######### Seleccionamos las columnas que esten en train y test #########

cols_to_keep <- c(names(df_test), "pobreza")
df_train <- df_train[, intersect(cols_to_keep, names(df_train))]

###Seleccioanamos algunas variables
df_train2 <- df_train[, c("Dominio","Lp","Nro_personas_trabajo_formal","Nro_personas_empleo_propio","Nro_personas_pensiones","Nro_cuartos","Nro_personas_ocupadas","tipo_vivienda","Nro_personas_subsidio_familiar","Nro_hijos","pobreza")]
# Obtener los nombres de las columnas comunes a df_train2 y df_test
cols_comunes <- intersect(names(df_test), names(df_train2))

# Seleccionar únicamente las columnas de df_test que están presentes en df_train2
df_test2 <- subset(df_test, select = cols_comunes)

#Definimos factores y enteros
df_train2$Nro_personas_trabajo_formal <- as.integer(df_train2$Nro_personas_trabajo_formal)
df_train2$Nro_personas_pensiones <- as.integer(df_train2$Nro_personas_pensiones)
df_train2$Nro_hijos <- as.integer(df_train2$Nro_hijos)

df_test2$Nro_personas_trabajo_formal <- as.integer(df_test2$Nro_personas_trabajo_formal)
df_test2$Nro_personas_pensiones <- as.integer(df_test2$Nro_personas_pensiones)
df_test2$Nro_hijos <- as.integer(df_test2$Nro_hijos)
str(df_test2)
str(df_train2)



# Dummyficamos 
dumificador <- dummyVars(formula = ~ ., data = df_train2, fullRank = T)
db_train <- predict(dumificador, newdata = df_train2)
db_train <- as.data.frame(db_train)

dumificador <- dummyVars(formula = ~ ., data = df_test2, fullRank = T)
db_test <- predict(dumificador, newdata = df_test2)
db_test <- as.data.frame(db_test)
# Eliminamos columna por multicolinealidad

# Calcular la matriz de correlación
matriz_cor <- cor(db_train)

# Identificar las columnas altamente correlacionadas
columnas_correlacionadas <- findCorrelation(matriz_cor, cutoff = 0.8)

# Eliminar las columnas identificadas
db_train  <- subset(db_train, select = -columnas_correlacionadas)



##Escaladas
# Estandarizamos DESPUÉS de partir la base en train/test
glimpse(df_train_con_dummies)

df_train_con_dummies_s <- df_train_con_dummies
df_test_con_dummies_s <- df_test_con_dummies

df_train_con_dummies_s <- as_tibble(df_train_con_dummies_s)
df_test_con_dummies_s <- as_tibble(df_test_con_dummies_s)

variables_numericas <- c("Ingtotug", "Ingtotugarr",
                         "Ingpcug", "Li", "Lp","Fex_c","edad_promedio","Ingtotob_hogar")
escalador <- preProcess(df_train_con_dummies[, variables_numericas],
                        method = c("center", "scale"))
df_train_con_dummies_s[, variables_numericas] <- predict(escalador, df_train_con_dummies[, variables_numericas])
df_test_con_dummies_s[, variables_numericas] <- predict(escalador, df_test_con_dummies[, variables_numericas])


####Modelo 1 sin hacer nada###########


############# prueba #####################






set.seed(123) # Fijar semilla para reproducibilidad



db_train$pobreza <- factor(db_train$pobreza, levels = c(0, 1))
# Separar en conjunto de entrenamiento y prueba
train <- na.omit(db_train) 
train <- subset(train, select = -pobreza.1) 
test <- na.omit(db_test)

# Crear matriz de predictores y vector de respuesta para el conjunto de entrenamiento
x_train <- train[, -which(names(train) == "pobreza")]
y_train <- train$pobreza
x_test <- test

##Dejamos las mismas columnas
train_cols <- intersect(colnames(x_train), colnames(x_test))
x_train <- x_train[, train_cols]


# Crear modelo con regresión logística y Lasso

tune_grid <- expand.grid(alpha = 0:1,
                         lambda = seq(0.001, 1, length = 10))

model <- train(x_train, y_train,
               method = "glmnet",
               trControl = trainControl(method = "cv", number = 5),
               family = "binomial", 
               tuneGrid = tune_grid,
               metric = "Spec ",
               reProcess = c("center", "scale"))

print(model)
model$results

# Seleccionar el mejor modelo basado en la menor tasa de error de clasificación en el conjunto de prueba

mejor_modelo <- model$results[which.max(model$results$Accuracy),]


# Hacer la predicción


# Crear el modelo de clasificación final utilizando el mejor valor de alpha y lambda
y_train <- as.factor(y_train)
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







# Obtener los niveles de la variable de respuesta
levels_y <- levels(y_train)

# Convertir los niveles a nombres válidos en R
levels_y_validos <- make.names(levels_y)

# Reemplazar los niveles antiguos con los nuevos en y_train
y_train <- factor(y_train, levels = levels_y, labels = levels_y_validos)



lambda_grid <- 10^seq(-4, 0.01, length = 10) #en la practica se suele usar una grilla de 200 o 300
tune_grid <- expand.grid(alpha = 0:1,
                         lambda = seq(0.001, 1, length = 10))

set.seed(1410)
modelo_grid_search_ROC <- train(x_train, y_train, 
                           method = "glmnet",
                           trControl = ctrl,
                           family = "binomial", 
                           metric = "ROC",
                           tuneGrid = tune_grid, 
                           preProcess = c("center", "scale")
)
modelo_grid_search_ROC



# Seleccionar el mejor modelo basado en la menor tasa de error de clasificación en el conjunto de prueba

mejor_modelo_2 <- modelo_grid_search_ROC$results[which.max(modelo_grid_search_ROC$results$ROC),]

# Hacer la predicción


# Crear el modelo de clasificación final utilizando el mejor valor de alpha y lambda
y_train <- as.factor(y_train)
modelo_final_2<- glmnet(x_train, y_train, alpha = mejor_modelo_2$alpha, lambda = mejor_modelo_2$lambda, family = "binomial")


# Hacer las predicciones con el modelo ajustado
pobre2 <- ifelse(predict(modelo_final_2, newx = as.matrix(x_test), type = "response") >= 0.5, 1, 0)

test<- import("test_sinna.rds") 

Respuesta_lasso_grid_serch_ROC <- data.frame(id = test$id, pobre = pobre2)

#### vemos porcentajes

tabla_frecuencia <- table(Respuesta_lasso_grid_serch_ROC$s0)
prop.table(tabla_frecuencia)


###exportamos

write.csv(Respuesta_lasso_grid_serch_ROC, "Respuesta_lasso_grid_serch_ROC_05.csv", row.names = FALSE)









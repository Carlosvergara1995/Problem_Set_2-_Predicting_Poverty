#regresion lienal 1

#cargamos los datos 
dvf_train <- readRDS("~/Desktop/git hut repositorios/Problem_Set_2/3. STORE/dvf_train.rds")
training_sinna <- readRDS("~/Desktop/git hut repositorios/Problem_Set_2/3. STORE/training_sinna.rds")
dvf_test <- readRDS("~/Desktop/git hut repositorios/Problem_Set_2/3. STORE/dvf_test.rds")

#agregamos la variable de ingresos totales 
#variables dominio
train_ing <- cbind(dvf_train, Ingtotug= training_sinna$Ingtotug, Lp=training_sinna$Lp)
train_ing <- select(train_ing, -Nro_personas_inactivas, -starts_with("Dominio"))

#divimos nuestras datos en train, validacion 
inTrain <- createDataPartition(
  y = train_ing$Ingtotug,## La variable dependiente u objetivo 
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)


train <- train_ing[inTrain,]
valit <- train_ing[-inTrain,]

train<- select(train, -Pobre.1)

modelo_ingreso <- lm(Ingtotug ~ ., data = train)
summary(modelo_ingreso)

ingresos_prueba <- predict(modelo_ingreso, newdata = valit)

# Convertir los ingresos predichos en una variable objetivo binaria (pobreza)
corte <- median(valit$Lp)
pobreza_predicha <- ifelse(ingresos_prueba < corte, 1, 0)

#evaluamos la prediccion del modelo 

# Calcular la exactitud
exactitud <- mean(pobreza_predicha == valit$Pobre.1)
exactitud 

#####
#regresion lienal 2


modelo_ingreso_2 <- lm(Ingtotug ~ ., data = train)
summary(modelo_ingreso_2)

ingresos_prueba_2 <- predict(modelo_ingreso_2, newdata = valit)

# Convertir los ingresos predichos en una variable objetivo binaria (pobreza)
corte_2 <- mean(valit$Lp)
pobreza_predicha_2 <- ifelse(ingresos_prueba_2 < corte, 1, 0)

#evaluamos la prediccion del modelo 
# Calcular la exactitud
exactitud_2 <- mean(pobreza_predicha_2 == valit$Pobre.1)
exactitud_2

# agregamos la columna Lp segun su dominio
dvf_test$

#para exportar los resultados
ingresos_prueba_2 <- predict(modelo_ingreso_2, newdata = dvf_test)

# Convertir los ingresos predichos en una variable objetivo binaria (pobreza)
corte_2 <- mean(valit$Lp)
pobreza_predicha_2 <- ifelse(ingresos_prueba_2 < corte, 1, 0)

#exportamos los resultados 
write.csv(, "Modelo1_con_grid_search.csv", row.names = FALSE)
write.csv(, "Modelo1_con_grid_search_bal.csv", row.names = FALSE)



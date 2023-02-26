#regresion lienal 1

#cargamos los datos 
dvf_train <- readRDS("~/Desktop/git hut repositorios/Problem_Set_2/3. STORE/dvf_train.rds")
training_sinna <- readRDS("~/Desktop/git hut repositorios/Problem_Set_2/3. STORE/training_sinna.rds")

#agragamos la variable de ingresos totales 
train_ing <- cbind(dvf_train, Ingtotug= training_sinna$Ingtotug, Lp=training_sinna$lp)
train_ing <- select(train_ing, -Nro_personas_inactivas)

modelo_ingreso <- lm(Ingtotug ~ ., data = train_ing)
summary(modelo_ingreso)

ingresos_prueba <- predict(modelo_ingreso, newdata = train_ing)

# Convertir los ingresos predichos en una variable objetivo binaria (pobreza)
corte <- median(train_ing$Lp)
pobreza_predicha <- ifelse(ingresos_prueba < corte, 1, 0)

#evaluamos la prediccion del modelo 

# Calcular la exactitud
exactitud <- mean(pobreza_predicha == dvf_train$Pobre.1)

# Imprimir la exactitud
cat("Exactitud del modelo:", exactitud)

#####
#regresion lienal 2


modelo_ingreso_2 <- lm(Ingtotug ~ ., data = train_ing)
summary(modelo_ingreso_2)

ingresos_prueba_2 <- predict(modelo_ingreso_2, newdata = train_ing)

# Convertir los ingresos predichos en una variable objetivo binaria (pobreza)
corte_2 <- mean(train_ing$Lp)
pobreza_predicha_2 <- ifelse(ingresos_prueba_2 < corte, 1, 0)

#evaluamos la prediccion del modelo 

# Calcular la exactitud
exactitud_2 <- mean(pobreza_predicha_2 == dvf_train$Pobre.1)

# Imprimir la exactitud
cat("Exactitud del modelo:", exactitud_2)



write.csv(Modelo1_con_grid_search, "Modelo1_con_grid_search.csv", row.names = FALSE)
write.csv(Modelo1_con_grid_search_bal, "Modelo1_con_grid_search_bal.csv", row.names = FALSE)



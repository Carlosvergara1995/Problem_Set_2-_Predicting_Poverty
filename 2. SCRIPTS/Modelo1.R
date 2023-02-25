#Modelo 1 
require(pacman)
p_load(tidyverse,rio,skimr,dplyr, caret, magrittr, glmnet,smotefamily,ROSE)

#cargamos nuestras bases directamente desde store
nv_test_sinna <- readRDS("~/Desktop/git hut repositorios/Problem_Set_2/3. STORE/nv_test_sinna.rds")
test <- nv_test_sinna

nv_training_sinna <- readRDS("~/Desktop/git hut repositorios/Problem_Set_2/3. STORE/nv_training_sinna.rds")
training<- nv_training_sinna

#ANALISIS DE VARIABLES####
# comparar los nombres de las variables
diff_variables <- setdiff(names(training), names(test))

# imprimir las variables diferentes
print(diff_variables)
#ambas bases presentan las mimas variables eceptuando pobre 


#analisis de los datos####
glimpse(training)
glimpse(test)
 
#analizamos la distribución de la pobreza en nuestra base training
table(training$Pobre)
prop.table(table(training$Pobre))
#encontramos un desvalance moderado de la base con un porcentaje del 20% de la clase minoritaria (pobre)


#seeleccionamos nuestras variables categoricas ####
#para training
training<- training %>% 
  mutate(P5090 = factor(P5090),
    Dominio = factor(Dominio),
    Clase = ifelse(Clase == 2, 0, Clase),
    Depto = factor(Depto),
    tipo_vivienda= factor(tipo_vivienda))
#para test
test<- test %>% 
  mutate(P5090 = factor(P5090),
         Dominio = factor(Dominio),
         Clase = ifelse(Clase == 2, 0, Clase),
         Depto = factor(Depto),
         tipo_vivienda= factor(tipo_vivienda))


# Eliminar las columnas que no se necesitan para el analisis####
df_train  <- select(training, -c(Clase, id, Depto))
df_test  <-  subset(test,  select = -c(Clase,id,Depto))

#Cambiamos factores####
df_train$P5090 <- factor(df_train$P5090)
df_test$P5090 <- factor(df_test$P5090)

df_train$Dominio <- factor(df_train$Dominio)
df_test$Dominio <- factor(df_test$Dominio)

df_train$Pobre <- factor(df_train$Pobre)
# Dummyficamos ####

###Cambiamos factores
df_train$P5090 <- factor(df_train$P5090)
df_test$P5090 <- factor(df_test$P5090)

df_train$Dominio <- factor(df_train$Dominio)
df_test$Dominio <- factor(df_test$Dominio)

df_train$Pobre <- factor(df_train$Pobre)


##Creamos las dummies - - - Dummyficamos ####
dumificador <- dummyVars(formula = ~ ., data = df_train, fullRank = T)
db_train <- predict(dumificador, newdata = df_train)
db_train <- as.data.frame(db_train)

dumificador <- dummyVars(formula = ~ ., data = df_test, fullRank = T)
db_test <- predict(dumificador, newdata = df_test)
db_test <- as.data.frame(db_test)


# Eliminar la variable con desviaci?n est?ndar igual a cero
db_train <- db_train[, apply(db_train, 2, sd) != 0]

# Eliminamos columna por multicolinealidad####

# Calcular la matriz de correlaci?n
matriz_cor <- cor(db_train)

# Identificar las columnas altamente correlacionadas
columnas_correlacionadas <- findCorrelation(matriz_cor, cutoff = 0.8)


# Eliminar las columnas identificadas
db_train  <- subset(db_train, select = -columnas_correlacionadas)
db_test  <- subset(db_test, select = -columnas_correlacionadas)

##Escaladas####
# Estandarizamos DESPU?S de partir la base en train/test
glimpse(db_train)
glimpse(db_test)

df_train_con_dummies_s <- db_train 
df_test_con_dummies_s <- db_test

df_train_con_dummies_s <- as_tibble(df_train_con_dummies_s)
df_test_con_dummies_s <- as_tibble(df_test_con_dummies_s)


# Seleccionar las columnas que est?n en ambos datasets y la columna Pobre
cols <- c(intersect(names(df_train_con_dummies_s), names(df_test_con_dummies_s)), "Pobre.1")
df_train_con_dummies_s <- df_train_con_dummies_s[, cols]
cols <- c(intersect(names(df_train_con_dummies_s), names(df_test_con_dummies_s)))
df_test_con_dummies_s <- df_test_con_dummies_s[, cols]

#verificamos
diff_variables <- setdiff(names(df_train_con_dummies_s), names(df_test_con_dummies_s))


#saveRDS(df_test_con_dummies_s,"dvf_test.rds")
#saveRDS(df_train_con_dummies_s,"dvf_train.rds")




#Modelo 1 
require(pacman)
p_load(tidyverse,rio,skimr,dplyr, caret, magrittr)

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


#seeleccionamos nuestras variables categoricas y dummificamos####
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


# Dummyficamos ####
dumificador <- dummyVars(formula = ~ ., data = training, fullRank = T)
db_train <- predict(dumificador, newdata = training)
db_train <- as.data.frame(db_train)

dumificador <- dummyVars(formula = ~ ., data = test, fullRank = T)
db_test <- predict(dumificador, newdata = test)
db_test <- as.data.frame(db_test)

# Eliminamos columna por multicolinealidad####

# Calcular la matriz de correlaci?n
matriz_cor <- cor(db_train)

# Identificar las columnas altamente correlacionadas
columnas_correlacionadas <- findCorrelation(matriz_cor, cutoff = 0.8)

# Eliminar las columnas identificadas
db_train  <- subset(db_train, select = -columnas_correlacionadas)


##Escaladas####
# Estandarizamos DESPU?S de partir la base en train/test
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


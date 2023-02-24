#Cargamos nuestras librearias 
require(pacman)
p_load(tidyverse,rio,skimr,dplyr, caret)

#Cargamos la bases ensambladas finales de datos
setwd('~/Desktop/git hut repositorios/Problem_Set_2/3. STORE')

#base training 
training<- import("nv_training_hogares_VF") 
training_1<- training
#base test
test <- import("df_test_hogares_VF.rds")
test_1 <- test

#analisis de estructura de nuestra base
skim(train)
summary(train)

#analisis para construccion de la variable dependiente
table(train$Pobre, useNA = "always")

# Analisis na para training####
attach(training)
# Analisis de na por variable
na_count<-colSums(is.na(training))
na_cols <- na_count[na_count > 0]
na_cols

#encontramos que las variables que contienne na son P5100, P5130,  P5140, cuota_amortizacion, arriendo, edu_promedio, horas_trabajadas_promedio,
# las cuales corresponden a ¿Cuánto pagan mensualmente por cuota de amortización?; Si tuviera que pagar arriendo por esta vivienda,
# ¿cuánto estima que tendría que pagar mensualmente?; ¿ Cuánto pagan mensualmente por arriendo ?; cuota de amortización; 
#analizamos el porcentaje de nas presente en cada variable 

# Calcular el porcentaje de valores NA por columna
porcentaje_nulos_por_columna <- na_count / nrow(training) * 100

# Seleccionar solo las variables con NA
variables_con_na <- names(porcentaje_nulos_por_columna[porcentaje_nulos_por_columna > 0])
#resultados
print(paste("Variables con NA:", paste(variables_con_na, collapse = ", ")))
print(paste("Porcentaje de NA por variable:"))
print(porcentaje_nulos_por_columna[porcentaje_nulos_por_columna > 0])

#encontramos que la variable P5100 y cuota_amortización se ecnuentran casi completanmente vacias con un por centaje de na del 96%, ademas
#presentan multicolinealidad procedemos a eliminar p5100 y a codificar arriendo reemplazando los na con 0 dado que suponemos que las personas 
# que no respondieron no pagan arriendo 
# por otro lado las demas variables con na tambien presentan valores relativamente grandes de nas ec

#codificamos nuestra variable edu_promedio dado que solo tiene un na lo reemplazaresmos con el promedio y eliminamos las variabbles con altos porcentajes de nas
#asuminmos que los na en horas promedio trabajadas es debido a que no cuantan con un trabajo

#calculamos el promedio de las variable
media_P5130 <- mean(training$P5130, na.rm = TRUE)
media_edu_promedio <- mean(training$edu_promedio, na.rm = TRUE)

training <- training %>%
  mutate(edu_promedio = ifelse(is.na(edu_promedio), media_edu_promedio, edu_promedio)) %>%
  mutate(arriendo = ifelse(is.na(arriendo), 0, arriendo)) %>%
  mutate(horas_trabajadas_promedio = ifelse(is.na(horas_trabajadas_promedio), 0, arriendo)) %>%
  mutate(P5130 = ifelse(Nro_personas_arriendos == 1 & is.na(P5130), P5130, ifelse(Nro_personas_arriendos == 0 & is.na(P5130), media_P5130, P5130)))

training$arriendo <- ifelse(is.na(training$arriendo), 0, training$arriendo)

training$horas_trabajadas_promedio <- ifelse(is.na(training$horas_trabajadas_promedio), 0, training$horas_trabajadas_promedio)

training$P5130 <- ifelse(training$Nro_personas_arriendos == 1 & is.na(training$P5130), training$P5130, ifelse(training$Nro_personas_arriendos == 0 & is.na(training$P5130), media_P5130, training$P5130))

training <- training[, !(names(training) %in% c("P5100", "P5140", "cuota_amortizacion"))]

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


#calculamos el promedio de las variable
media_P5130_t <- mean(test$P5130, na.rm = TRUE)
media_edu_promedio_t <- mean(test$edu_promedio, na.rm = TRUE)
#variables a eliminar por multicolinealidad y por altas porcentajes de na

#codificamos nuestras variables 
test <- test %>%
  mutate(edu_promedio = ifelse(is.na(edu_promedio), media_edu_promedio_t, edu_promedio)) %>%
  mutate(arriendo = ifelse(is.na(arriendo), 0, arriendo)) %>%
  mutate(horas_trabajadas_promedio = ifelse(is.na(horas_trabajadas_promedio), 0, arriendo)) %>%
  mutate(P5130 = ifelse(Nro_personas_arriendos == 1 & is.na(P5130), P5130, ifelse(Nro_personas_arriendos == 0 & is.na(P5130), media_P5130, P5130)))

test$arriendo <- ifelse(is.na(test$arriendo), 0, test$arriendo)

test$horas_trabajadas_promedio <- ifelse(is.na(test$horas_trabajadas_promedio), 0, test$horas_trabajadas_promedio)

test$P5130 <- ifelse(test$Nro_personas_arriendos == 1 & is.na(test$P5130), test$P5130, ifelse(test$Nro_personas_arriendos == 0 & is.na(test$P5130), media_P5130_t, test$P5130))

test <- test[, !(names(test) %in% c("P5100", "P5140", "cuota_amortizacion"))]


saveRDS(test, "nv_test_sinna.rds")
saveRDS(training, "nv_training_sinna.rds")


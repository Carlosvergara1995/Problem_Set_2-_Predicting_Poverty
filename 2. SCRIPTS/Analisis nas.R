#Cargamos nuestras librearias 
require(pacman)
p_load(tidyverse, rio,skimr,dplyr)

#Cargamos la bases ensambladas finales de datos
setwd('~/Desktop/git hut repositorios/Problem_Set_2/3. STORE')

#base training 
training<- import("df_training_hogares_VF.rds") 
training_1<- training
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

#encontramos que la variable P5100 y cuota_amortización se ecnuentran casi completanmente vacias con un por centaje de na del 96%
# por otro lado las demas variables con na tambien presentan valores relativamente grandes de nas eceptuando a horas_trabajadas_promedio y edu_promedio
# dados los resultados anteriores y las demas variables presentes en la base procederemos a realizar nuestro analisis sin ellas 
# reemplazandolas con otras varibales, menos en el caso de edu_promedio que solo presenta 1 na el cual sera reemplazado por su promedio

#codificamos nuestra variable edu_promedio dado que solo tiene un na lo reemplazaresmos con el promedio y eliminamos las variabbles con altos porcentajes de nas
#calculamos el promedio de la variable
media_edu_promedio <- mean(training$edu_promedio, na.rm = TRUE)
variables_eliminar <- c('P5100', 'P5130',  'P5140', 'cuota_amortizacion', 'arriendo', 'horas_trabajadas_promedio')

training<- training %>%
  mutate( edu_promedio = ifelse(is.na(edu_promedio), media_edu_promedio, edu_promedio))%>%
  select(-variables_eliminar)

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

media_edu_promediot <- mean(test$edu_promedio, na.rm = TRUE)
variables_eliminart <- c('P5100', 'P5130',  'P5140', 'cuota_amortizacion', 'arriendo', 'horas_trabajadas_promedio')
test<- test %>%
  mutate( edu_promedio = ifelse(is.na(edu_promedio), media_edu_promediot, edu_promedio))%>%
  select(-variables_eliminar)


#saveRDS(test, "test_sinna.rds")
#saveRDS(training, "training_sinna.rds")


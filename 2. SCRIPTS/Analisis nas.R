#Cargamos nuestras librearias 
require(pacman)
p_load(tidyverse, rio)

#Cargamos la bases ensambladas finales de datos
setwd('~/Desktop/git hut repositorios/Problem_Set_2/3. STORE')

#base training 
training<- import("df_training_hogares_VF.rds") 
training_1<- training
#base test
test <- import("df_test_hogares_VF.rds")
test_1 <- test

############ Analisis na

#Analisis de na por variable
colSums(is.na(data))

#Calculamos el porcentaje de los datos diferentes de NA
sum(data$y_ingLab_m_ha > 0 & !is.na(data$y_ingLab_m_ha) )/length(data$y_ingLab_m_ha)
# se infiere que el 43% de la base presenta datos 

#eliminamos todas las filas con un valor faltante en la columna de nuestra valiable dependiente (y_ingLab_m_ha)
df <- data[!is.na(data$y_ingLab_m_ha), ] %>% as.data.frame()

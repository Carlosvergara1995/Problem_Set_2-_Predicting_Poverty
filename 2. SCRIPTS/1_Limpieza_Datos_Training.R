#Problmem_Set_2
# Limpieza de datos Trainign
#Carlos Vergara, Alexandra Rizo, Danna Bolaños, Héctor Tacumán
_______________________________________________________________

#######Preparación del espacio########

## Se llaman los paquetes para su uso en el Script:
rm(list=ls())
install.packages("pacman")
require(pacman)
p_load(tidyverse,rvest,writexl,rio,skimr,pastecs,PerformanceAnalytics,naniar,gtsummary)

## Se llaman las bases de datos de Training: 
<<<<<<< Updated upstream
rm(list=ls())
setwd("~/OneDrive - Universidad de los Andes/2023/2023-1/BIG DATA/TALLERES GRUPALES/TALLER No. 2/Problem_Set_2-_Predicting_Poverty/3. STORE")

df_training_hogares <- import("train_hogares.csv")
=======
setwd("data")
df_training_hogares <- import("df_training_hogares.rds") 
df_training_personas <- import("df_training_personas.rds")
>>>>>>> Stashed changes

df_training_personas <- import("train_personas.csv")

colnames(df_training_hogares)
colnames (df_training_personas)

summary(df_training_hogares)
summary(df_training_personas)

#######Modificación de las bases de datos########

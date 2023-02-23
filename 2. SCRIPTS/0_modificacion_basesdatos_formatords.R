#Problmem_Set_2
# Transformación de bases de datos a formato Rds.
#Carlos Vergara, Alexandra Rizo, Danna Bolaños, Héctor Tacumán


#######Preparación del espacio########
rm(list=ls())
## Se llaman los paquetes para su uso en el Script:
install.packages("pacman")
require(pacman)
p_load(tidyverse,rvest,writexl,rio,skimr,pastecs,PerformanceAnalytics,naniar,gtsummary)

## Se llaman las bases de datos: 
setwd("~/OneDrive - Universidad de los Andes/uniandes-bdml-20231-ps2 2") #se cargan los archivos de la carpeta store directamente

df_training_hogares <- import("train_hogares.csv") 
df_training_personas <- import("train_personas.csv")
df_test_hogares <- import("test_hogares.csv") 
df_test_personas <- import("test_personas.csv")
df_sample_submission <- import("sample_submission.csv")

## Se salvan las bases de datos como rds.

saveRDS(df_training_hogares,"df_training_hogares.rds")
saveRDS(df_training_personas,"df_training_personas.rds")
saveRDS(df_test_hogares,"df_test_hogares.rds")
saveRDS(df_test_personas,"df_test_personas.rds")
saveRDS(df_sample_submission,"df_sample_submission.rds")

##Se cargan las bases en formanto rds. en la carpeta STORES de manera manual. 
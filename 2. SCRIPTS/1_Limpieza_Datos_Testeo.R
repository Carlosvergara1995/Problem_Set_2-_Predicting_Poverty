#Problmem_Set_2
# Limpieza de datos testeo y unificación de bases
#Carlos Vergara, Alexandra Rizo, Danna Bolaños, Héctor Tacumán
_______________________________________________________________

####### Preparación del espacio ########
## Se llaman los paquetes para su uso en el Script:
install.packages("pacman")
require(pacman)
p_load(tidyverse,rvest,writexl,rio,skimr,pastecs,PerformanceAnalytics,naniar,gtsummary)

## Se llaman las bases de datos de Training: 
rm(list=ls())
setwd("~/OneDrive - Universidad de los Andes/2023/2023-1/BIG DATA/TALLERES GRUPALES/TALLER No. 2/Problem_Set_2-_Predicting_Poverty/3. STORE")

df_test_hogares <- import("df_test_hogares.rds") 
df_test_personas <- import("df_test_personas.rds")
#Problmem_Set_2
# Estadisticas descriptivas
#Carlos Vergara, Alexandra Rizo, Danna Bolaños, Héctor Tacumán

#######Preparación del espacio########

## Se llaman los paquetes para su uso en el Script:
install.packages("pacman")
require(pacman)
p_load(tidyverse,rvest,writexl,rio,skimr,pastecs,PerformanceAnalytics,naniar,gtsummary)

## Se llaman las bases de datos: 

dvf_test<- readRDS("dvf_test.rds")
dvf_train<- readRDS("dvf_train.rds")

####### Estadisticas descriptivas para la base de datos training ########

dim(dvf_train)
colnames(dvf_train)
Pobres <- prop.table(table(dvf_train$Pobre.1))
p_load(officer)
library(officer)
doc <- read_docx()
print (doc, Pobres= "Tablas_pobres")
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
Pobres <- prop.table(table(dvf_train$Pobre.1))
colnames(dvf_train)

## Se seleccionan variables de interes:

df_1 <- dvf_train %>% select(c("Nper","Pobre.1","Nro_cuartos", "Nro_personas_cuartos","arriendo","Nro_mujeres","edad_promedio","jefe_hogar_mujer","Nro_hijos","Nro_personas_trabajo_formal","edu_promedio","Nro_personas_subsidio_familiar","horas_trabajadas_promedio","Nro_personas_empleo_propio","Nro_personas_segundo_trabajo","Nro_personas_arriendos","Nro_personas_pensiones","Nro_personas_pension_alimenticia","Nro_personas_otros_ingresos","Nro_personas_otros_ingresos_pais","Nro_personas_otros_ingresos_otros_paises","Nro_personas_otros_ingresos_instituciones","Nro_personas_otras_ganancias","Nro_personas_PET","Nro_personas_ocupadas","Nro_personas_desempleadas","Nro_personas_inactivas"))

### Estadísticas descriptivas ###

summary(df_1)

# estadísticas descriptivas variables de interes

require(gtsummary) #llamado librería

# estadísiticas descriptivas generales datos:

stat.desc(df_1)
descriptivas_trainig <- stat.desc(df_1)
descriptivas_trainig$Estadisticas <- row.names(descriptivas_trainig) 
descriptivas_trainig <- descriptivas_trainig %>% select(Estadisticas, everything())  
write_xlsx(descriptivas_trainig, "descriptivas_trainig.xlsx")

# Tablas descriptivas
tbl_summary(df_1, statistic = list (all_continuous()~"{mean} ({sd})")) # generales
tbl_summary(df_1, by= Pobre.1, statistic = list (all_continuous()~"{mean} ({sd})")) # por clasificación

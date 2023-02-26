#Problmem_Set_2
# Estadisticas descriptivas
#Carlos Vergara, Alexandra Rizo, Danna Bolaños, Héctor Tacumán

#######Preparación del espacio########

## Se llaman los paquetes para su uso en el Script:
install.packages("pacman")
require(pacman)
p_load(tidyverse,stargazer,rvest,writexl,rio,skimr,pastecs,PerformanceAnalytics,naniar,gtsummary,flextable,officer)

## Se llaman las bases de datos: 

dvf_test<- readRDS("dvf_test.rds")
dvf_train<- readRDS("dvf_train.rds")

####### Estadisticas descriptivas para la base de datos training ########

dim(dvf_train)
prop.table(table(dvf_train$Pobre.1))
colnames(dvf_train)

## Se seleccionan variables de interes:

df_1 <- dvf_train %>% select(c("tipo_vivienda.3","tipo_vivienda.5","Nro_cuartos","Nro_mujeres","jefe_hogar_mujer","Nro_hijos","Nro_personas_trabajo_formal","edu_promedio", "Nro_personas_subsidio_familiar","horas_trabajadas_promedio","Nro_personas_segundo_trabajo","Nro_personas_pensiones","Nro_personas_pension_alimenticia","Nro_personas_otros_ingresos","Nro_personas_otros_ingresos_pais","Nro_personas_otros_ingresos_otros_paises","Nro_personas_otros_ingresos_instituciones","Nro_personas_otras_ganancias","Nro_personas_PET","Nro_personas_ocupadas","Nro_personas_desempleadas","Nro_personas_inactivas","Pobre.1"))

### Estadísticas descriptivas ###

summary(df_1)

# estadísticas descriptivas variables de interes

tbl_summary(df_1, statistic = list (all_continuous()~"{mean} ({sd})")) # generales
tbl_summary(df_1, by= Pobre.1, statistic = list (all_continuous()~"{mean} ({sd})")) # por clasificación

df_2 <-tbl_summary(df_1, statistic = list (all_continuous()~"{mean} ({sd})")) # generales
df_3 <- tbl_summary(df_1, by= Pobre.1, statistic = list (all_continuous()~"{mean} ({sd})")) # por clasificación

#Se convierte la tabla por clasificación en formato word:

Tabla_est_desc_train <- as_tibble(df_3)
tabla_flex <- flextable(Tabla_est_desc_train)
doc <- read_docx()
doc <- doc %>% 
  body_add_flextable(tabla_flex)
print(doc, target = "Tabla_est_desc_train.docx")

####### Estadisticas descriptivas para la base de datos testeo ########

dvf_test<- readRDS("dvf_test")
dim(dvf_test)
colnames(dvf_test)

## Se seleccionan variables de interes
df_4 <- dvf_test %>% select(c("tipo_vivienda.3","tipo_vivienda.5","Nro_cuartos","Nro_mujeres","jefe_hogar_mujer","Nro_hijos","Nro_personas_trabajo_formal","edu_promedio", "Nro_personas_subsidio_familiar","horas_trabajadas_promedio","Nro_personas_segundo_trabajo","Nro_personas_pensiones","Nro_personas_pension_alimenticia","Nro_personas_otros_ingresos","Nro_personas_otros_ingresos_pais","Nro_personas_otros_ingresos_otros_paises","Nro_personas_otros_ingresos_instituciones","Nro_personas_otras_ganancias","Nro_personas_PET","Nro_personas_ocupadas","Nro_personas_desempleadas","Nro_personas_inactivas","Pobre.1"))

summary(df_4)

# estadísticas descriptivas variables de interes

require(gtsummary) #llamado librería

# estadísiticas descriptivas generales datos

tbl_summary(df_4, statistic = list (all_continuous()~"{mean} ({sd})")) # generales

  
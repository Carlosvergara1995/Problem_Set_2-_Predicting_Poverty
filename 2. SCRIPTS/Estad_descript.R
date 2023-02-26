#Problmem_Set_2
# Estadisticas descriptivas
#Carlos Vergara, Alexandra Rizo, Danna Bolaños, Héctor Tacumán

#######Preparación del espacio########

## Se llaman los paquetes para su uso en el Script:
install.packages("pacman")
require(pacman)
p_load(tidyverse,stargazer,rvest,writexl,rio,skimr,pastecs,PerformanceAnalytics,naniar,gtsummary,flextable,officer)

## Se llaman las bases de datos unificadas y sin NAs: 

nv_test_sinna<- readRDS("nv_test_sinna.rds")
nv_training_sinna<- readRDS("nv_training_sinna.rds")

####### Estadisticas descriptivas para la base de datos training ########

dim(nv_training_sinna)
prop.table(table(nv_training_sinna$Pobre.1))
colnames(nv_training_sinna)

## Se seleccionan variables de interes:

df_1 <- nv_training_sinna %>% select(c("Nper","Npersug","Pobre","Depto","tipo_vivienda","Nro_cuartos","Nro_personas_cuartos","arriendo","Nro_mujeres","edad_promedio","jefe_hogar_mujer","Nro_hijos","Nro_personas_trabajo_formal","edu_promedio","Nro_personas_subsidio_familiar","horas_trabajadas_promedio","Nro_personas_empleo_propio","Nro_personas_segundo_trabajo","Nro_personas_arriendos","Nro_personas_pensiones","Nro_personas_pension_alimenticia","Nro_personas_otros_ingresos","Nro_personas_otros_ingresos_pais","Nro_personas_otros_ingresos_otros_paises","Nro_personas_otros_ingresos_instituciones","Nro_personas_otras_ganancias","Nro_personas_PET","Nro_personas_ocupadas","Nro_personas_desempleadas","Nro_personas_inactivas")) 

### Estadísticas descriptivas ###

summary(df_1)

# estadísticas descriptivas variables de interes

tbl_summary(df_1, statistic = list (all_continuous()~"{mean} ({sd})")) # generales
tbl_summary(df_1, by= Pobre, statistic = list (all_continuous()~"{mean} ({sd})")) # por clasificación

df_2 <-tbl_summary(df_1, statistic = list (all_continuous()~"{mean} ({sd})")) # generales
df_3 <- tbl_summary(df_1, by= Pobre, statistic = list (all_continuous()~"{mean} ({sd})")) # por clasificación

#Se convierte la tabla por clasificación en formato word:

Tabla_est_train_1 <- as_tibble(df_2)
tabla_flex <- flextable(Tabla_est_train_1)
doc_1 <- read_docx()
doc_1 <- doc_1 %>% 
  body_add_flextable(tabla_flex)
print(doc_1, target = "Tabla_est_train_1.docx")

Tabla_est_train_2 <- as_tibble(df_3)
tabla_flex_1 <- flextable(Tabla_est_train_2)
doc_2 <- read_docx()
doc_2 <- doc_2 %>% 
  body_add_flextable(tabla_flex_1)
print(doc_2, target = "Tabla_est_train_2.docx")

####### Estadisticas descriptivas para la base de datos testeo ########

nv_test_sinna<- readRDS("nv_test_sinna".rds)
dim(nv_test_sinna)
colnames(nv_test_sinna)

## Se seleccionan variables de interes
df_4 <- nv_test_sinna %>% select(c("Nper","Npersug","Depto","tipo_vivienda","Nro_cuartos","Nro_personas_cuartos","arriendo","Nro_mujeres","edad_promedio","jefe_hogar_mujer","Nro_hijos","Nro_personas_trabajo_formal","edu_promedio","Nro_personas_subsidio_familiar","horas_trabajadas_promedio","Nro_personas_empleo_propio","Nro_personas_segundo_trabajo","Nro_personas_arriendos","Nro_personas_pensiones","Nro_personas_pension_alimenticia","Nro_personas_otros_ingresos","Nro_personas_otros_ingresos_pais","Nro_personas_otros_ingresos_otros_paises","Nro_personas_otros_ingresos_instituciones","Nro_personas_otras_ganancias","Nro_personas_PET","Nro_personas_ocupadas","Nro_personas_desempleadas","Nro_personas_inactivas")) 
summary(df_4)

# estadísiticas descriptivas generales datos

tbl_summary(df_4, statistic = list (all_continuous()~"{mean} ({sd})")) # generales

Tabla_est_desc_test <- as_tibble(df_4)
tabla_flex_3 <- flextable(Tabla_est_desc_test)
doc_3 <- read_docx()
doc_3 <- doc_3 %>% 
  body_add_flextable(tabla_flex_3)
print(doc_3, target = "Tabla_est_desc_test.docx")

  
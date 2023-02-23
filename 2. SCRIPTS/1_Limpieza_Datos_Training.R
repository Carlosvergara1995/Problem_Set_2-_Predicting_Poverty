#Problmem_Set_2
# Limpieza de datos Trainign y unificación de bases
#Carlos Vergara, Alexandra Rizo, Danna Bolaños, Héctor Tacumán


####### Preparación del espacio ########
## Se llaman los paquetes para su uso en el Script:
install.packages("pacman")
require(pacman)
p_load(tidyverse,rvest,writexl,rio,skimr,pastecs,PerformanceAnalytics,naniar,gtsummary)

## Se llaman las bases de datos de Training: 
rm(list=ls())
setwd("~/OneDrive - Universidad de los Andes/2023/2023-1/BIG DATA/TALLERES GRUPALES/TALLER No. 2/Problem_Set_2-_Predicting_Poverty/3. STORE")

df_training_hogares <- import("df_training_hogares.rds") 
df_training_personas <- import("df_training_personas.rds")

####### Modificación de las bases de datos y creación de variables ########

###Revisando los documentos de soporte de las bases de datos "Colombia - Medicion de Pobreza Monetaroia y Desigualdad 2018" se revisan las variables objeto de estudio. 

##Se identifican las variables y se procede a hacer las correspondientes modificaciones, a saber:

# Frente a las variables de sexo, jefe de hogar y nivel educativo alcanzado, se realizan los siguientes cambios: 

df_training_personas <- df_training_personas %>% mutate(mujer = ifelse(P6020 == 2, 1, 0))
df_training_personas <- df_training_personas %>% mutate(jefe_hogar = ifelse(P6050 == 1, 1, 0))
df_training_personas <- df_training_personas %>% mutate(hijo = ifelse(P6050 == 3, 1, 0))
df_training_personas <- df_training_personas %>% mutate(jefe_hogar_mujer = jefe_hogar*mujer)

aggregate(df_training_personas$P6210s1, by = list(df_training_personas$P6210), mean, na.rm = TRUE)
aggregate(df_training_personas$P6210s1, by = list(df_training_personas$P6210), min, na.rm = TRUE)
aggregate(df_training_personas$P6210s1, by = list(df_training_personas$P6210), max, na.rm = TRUE)

df_training_personas %>% subset(P6210 == 2) %>% select(P6210s1) %>% table()
df_training_personas %>% subset(P6210 == 3) %>% select(P6210s1) %>% table()
df_training_personas %>% subset(P6210 == 4) %>% select(P6210s1) %>% table()
df_training_personas %>% subset(P6210 == 5) %>% select(P6210s1) %>% table()
df_training_personas %>% subset(P6210 == 6) %>% select(P6210s1) %>% table()

df_training_personas <- df_training_personas %>% mutate(P6210s1 = ifelse(P6210 == 4 & P6210s1 == 0, 5, P6210s1))
df_training_personas <- df_training_personas %>% mutate(edu = case_when(P6210 == 1 ~ 0,P6210 == 2 ~ P6210s1,P6210 == 3 ~ P6210s1 + 1,P6210 == 4 ~ P6210s1 + 1,P6210 == 5 ~ P6210s1 + 1,P6210 == 6 ~ P6210s1 + 1, P6210 == 7 ~ P6210s1 + 1,P6210 == 8 ~ P6210s1 + 1,P6210 == 9 ~ P6210s1 + 1,P6210 == 10 ~ P6210s1 + 1))

#Frente a las variables de trabajo u otros ingresos, se realizan los siguientes cambios: 

df_training_personas <- df_training_personas %>% mutate(trabajo_formal = ifelse(P6920 == 1, 1, 0))
df_training_personas <- df_training_personas %>% mutate(segundo_trabajo = ifelse(P7040 == 1, 1, 0))
df_training_personas <- df_training_personas %>% mutate(arriendos = ifelse(P7495 == 1, 1, 0))
df_training_personas <- df_training_personas %>% mutate(pensiones = ifelse(P7500s2 == 1, 1, 0))
df_training_personas <- df_training_personas %>% mutate(pension_alimenticia = ifelse(P7500s3 == 1, 1, 0))
df_training_personas <- df_training_personas %>% mutate(otros_ingresos = ifelse(P7505 == 1, 1, 0))
df_training_personas <- df_training_personas %>% mutate(otros_ingresos_pais = ifelse(P7510s1 == 1, 1, 0))
df_training_personas <- df_training_personas %>% mutate(otros_ingresos_otros_paises = ifelse(P7510s2 == 1, 1, 0))
df_training_personas <- df_training_personas %>% mutate(otros_ingresos_instituciones = ifelse(P7510s3 == 1, 1, 0))
df_training_personas <- df_training_personas %>% mutate(otras_ganancias = ifelse(P7510s5 == 1, 1, 0))

# Se agrupa la información de la base personas por hogar para unirla con la base de hogar

df_training_hogares1 <-df_training_personas %>% group_by(id) %>% summarize(Nro_mujeres=sum(mujer,na.rm = TRUE),
edad_promedio=mean(P6040,na.rm = TRUE),jefe_hogar_mujer=sum(jefe_hogar_mujer,na.rm = TRUE),Nro_hijos=sum(hijo,na.rm = TRUE),Nro_personas_trabajo_formal=sum(P6090,na.rm = TRUE),
edu_promedio=mean(edu,na.rm = TRUE),Nro_personas_subsidio_familiar=sum(P6585s3,na.rm = TRUE), horas_trabajadas_promedio=mean(P6800,na.rm = TRUE),
Nro_personas_empleo_propio=sum(P6870,na.rm = TRUE),Nro_personas_trabajo_formal=sum(trabajo_formal,na.rm = TRUE),Nro_personas_segundo_trabajo=sum(segundo_trabajo,na.rm = TRUE),
Nro_personas_arriendos=sum(arriendos,na.rm = TRUE),Nro_personas_pensiones=sum(pensiones,na.rm = TRUE),
Nro_personas_pension_alimenticia=sum(pension_alimenticia,na.rm = TRUE),Nro_personas_otros_ingresos=sum(otros_ingresos,na.rm = TRUE),
Nro_personas_otros_ingresos_pais=sum(otros_ingresos_pais,na.rm = TRUE),Nro_personas_otros_ingresos_otros_paises=sum(otros_ingresos_otros_paises,na.rm = TRUE),
Nro_personas_otros_ingresos_instituciones=sum(otros_ingresos_instituciones,na.rm = TRUE), Nro_personas_otras_ganancias=sum(otras_ganancias,na.rm = TRUE),
Nro_personas_PET=sum(Pet,na.rm = TRUE),Nro_personas_ocupadas=sum(Oc,na.rm = TRUE), Nro_personas_desempleadas=sum(Des,na.rm = TRUE),
Nro_personas_inactivas=sum(Ina,na.rm = TRUE),Ingtot_hogar=sum(Ingtot,na.rm = TRUE))

# Se generan las modificaciones a la base de datos de hogares de entrenamiento con la adición de variables: 

df_training_hogares <-df_training_hogares %>% mutate(Pobre=factor(Pobre,levels=c(1,0)))
df_training_hogares <-df_training_hogares %>% mutate(Indigente=factor(Indigente,levels=c(1,0)))
df_training_hogares <-df_training_hogares %>% mutate(tipo_vivienda=factor(P5090,levels=c(1, 2, 3, 4, 5, 6)))

df_training_hogares <- df_training_hogares %>% mutate(Nro_cuartos = P5000)
df_training_hogares <- df_training_hogares %>% mutate(Nro_personas_cuartos = Nper/P5010)
df_training_hogares <- df_training_hogares %>% mutate(cuota_amortizacion = P5100)
df_training_hogares <- df_training_hogares %>% mutate(arriendo = P5140)

## Se unen las bases de datos:

df_training_hogares_VF <- left_join(df_training_hogares, df_training_hogares1)

saveRDS(df_training_hogares_VF, file = "df_training_hogares_VF.rds")

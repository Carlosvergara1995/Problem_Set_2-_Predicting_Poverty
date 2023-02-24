#Problmem_Set_2
# Limpieza de datos Trainign
#Carlos Vergara, Alexandra Rizo, Danna Bolaños, Héctor Tacumán
_______________________________________________________________

#######Preparación del espacio########

## Se llaman los paquetes para su uso en el Script:
install.packages("pacman")
require(pacman)
p_load(tidyverse,rvest,writexl,rio,skimr,pastecs,PerformanceAnalytics,naniar,gtsummary)

## Se llaman las bases de datos de Training modificadas: 
rm(list=ls())
setwd("~/OneDrive - Universidad de los Andes/2023/2023-1/BIG DATA/TALLERES GRUPALES/TALLER No. 2/Problem_Set_2-_Predicting_Poverty/3. STORE")

nv_training_hogares <- import("nv_train_hogares.csv")
nv_training_personas <- import("nv_train_personas.csv")

colnames(nv_training_hogares)
colnames (nv_training_personas)

summary(nv_training_hogares)
summary(nv_training_personas)

#######Modificación de las bases de datos########
## Se modifica la base de datos de hogares

#variable categorica tipo de vivienda
nv_training_hogares <- nv_training_hogares %>% mutate (tipo_vivienda=factor(P5090,levels=c(1, 2, 3, 4, 5, 6)))

#variable número de cuarto                                                                   
nv_training_hogares <- nv_training_hogares %>% mutate (Nro_cuartos = P5000)

#variable número de personas por cuartos
nv_training_hogares <- nv_training_hogares %>% mutate (Nro_personas_cuartos = Nper/P5010)

#variable cuota ¿cuanto paga de cuota de amortización mesualmente?
nv_training_hogares <- nv_training_hogares %>% mutate (cuota_amortizacion = P5100)

#variable ¿cuanto paga de arriendo mesualmente?
nv_training_hogares <- nv_training_hogares %>% mutate (arriendo = P5140)

# Frente a la base de datos de personas y sus  variables de sexo, jefe de hogar y nivel educativo alcanzado, se realizan los siguientes cambios: 

#variable mujer 1 si es mujer 0 en otros caos 
nv_training_personas <- nv_training_personas %>% mutate(mujer = ifelse(P6020 == 0, 1, 0))

#variable categorica, parenteszo jefe del hogar 
nv_training_personas <- nv_training_personas %>% mutate(jefe_hogar = ifelse(P6050 == 1, 1, 0))

nv_training_personas <- nv_training_personas %>% mutate(hijo = ifelse(P6050 == 3, 1, 0))

#variable si el jefe del hogar es mujer 
nv_training_personas <- nv_training_personas %>% mutate(jefe_hogar_mujer = jefe_hogar*mujer)

# Variable categórica de educación:

aggregate(nv_training_personas$P6210s1, by = list(nv_training_personas$P6210), mean, na.rm = TRUE)
aggregate(nv_training_personas$P6210s1, by = list(nv_training_personas$P6210), min, na.rm = TRUE)
aggregate(nv_training_personas$P6210s1, by = list(nv_training_personas$P6210), max, na.rm = TRUE)

nv_training_personas %>% subset(P6210 == 2) %>% select(P6210s1) %>% table()
nv_training_personas %>% subset(P6210 == 3) %>% select(P6210s1) %>% table()
nv_training_personas %>% subset(P6210 == 4) %>% select(P6210s1) %>% table()
nv_training_personas %>% subset(P6210 == 5) %>% select(P6210s1) %>% table()
nv_training_personas %>% subset(P6210 == 6) %>% select(P6210s1) %>% table()

nv_training_personas <- nv_training_personas %>% mutate(P6210s1 = ifelse(P6210 == 4 & P6210s1 == 0, 5, P6210s1))
nv_training_personas <- nv_training_personas %>% mutate(edu = case_when(P6210 == 1 ~ 0,P6210 == 2 ~ P6210s1,P6210 == 3 ~ P6210s1 + 1,P6210 == 4 ~ P6210s1 + 1,P6210 == 5 ~ P6210s1 + 1,P6210 == 6 ~ P6210s1 + 1, P6210 == 7 ~ P6210s1 + 1,P6210 == 8 ~ P6210s1 + 1,P6210 == 9 ~ P6210s1 + 1,P6210 == 10 ~ P6210s1 + 1))

#Frente a las variables de trabajo u otros ingresos, se realizan los siguientes cambios: 

#Variable categorica trabajo_formal
nv_training_personas <- nv_training_personas %>% mutate(trabajo_formal = ifelse(P6920 == 1, 1, 0))

# Variable categórica de segundo trabajo 1 si 0 no
nv_training_personas <- nv_training_personas %>% mutate(segundo_trabajo = ifelse(P7040 == 1, 1, 0))

# Variable categórica de pagos por arriendo 1 si 0 no
nv_training_personas <- nv_training_personas %>% mutate(arriendos = ifelse(P7495 == 1, 1, 0))

# Variable categórica de pagos por pensión 1 si 0 no
nv_training_personas <- nv_training_personas %>% mutate(pensiones = ifelse(P7500s2 == 1, 1, 0))

# Variable categórica de pagos por pensión alimentaria 1 si 0 no
nv_training_personas <- nv_training_personas %>% mutate(pension_alimenticia = ifelse(P7500s3 == 1, 1, 0))

# Variable categórica de pagos por otros ingresos 1 si 0 no
nv_training_personas <- nv_training_personas %>% mutate(otros_ingresos = ifelse(P7505 == 1, 1, 0))

# Variable categórica de pagos por envios de dinero dentro del país 1 si 0 no
nv_training_personas <- nv_training_personas %>% mutate(otros_ingresos_pais = ifelse(P7510s1 == 1, 1, 0))

# Variable categórica de pagos por concepto de remesas 1 si 0 no
nv_training_personas <- nv_training_personas %>% mutate(otros_ingresos_otros_paises = ifelse(P7510s2 == 1, 1, 0))

# Variable categórica de pagos por otros ingresos provenientes de otras instituciones 1 si 0 no
nv_training_personas <- nv_training_personas %>% mutate(otros_ingresos_instituciones = ifelse(P7510s3 == 1, 1, 0))

# Variable categórica de pagos por otras ganancias 1 si 0 no
nv_training_personas <- nv_training_personas %>% mutate(otras_ganancias = ifelse(P7510s5 == 1, 1, 0))

# Se agrupa la información de la base personas por hogar para unirla con la base de hogar

nv_training_hogares1 <-nv_training_personas %>% group_by(id) %>% summarize(Nro_mujeres=sum(mujer,na.rm = TRUE),
edad_promedio=mean(P6040,na.rm = TRUE),jefe_hogar_mujer=sum(jefe_hogar_mujer,na.rm = TRUE),
Nro_hijos=sum(hijo,na.rm = TRUE),Nro_personas_trabajo_formal=sum(P6090,na.rm = TRUE),
edu_promedio=mean(edu,na.rm = TRUE),Nro_personas_subsidio_familiar=sum(P6585s3,na.rm = TRUE), 
horas_trabajadas_promedio=mean(P6800,na.rm = TRUE),Nro_personas_empleo_propio=sum(P6870,na.rm = TRUE),
Nro_personas_trabajo_formal=sum(trabajo_formal,na.rm = TRUE),Nro_personas_segundo_trabajo=sum(segundo_trabajo,na.rm = TRUE),
Nro_personas_arriendos=sum(arriendos,na.rm = TRUE),Nro_personas_pensiones=sum(pensiones,na.rm = TRUE),
Nro_personas_pension_alimenticia=sum(pension_alimenticia,na.rm = TRUE),Nro_personas_otros_ingresos=sum(otros_ingresos,na.rm = TRUE),
Nro_personas_otros_ingresos_pais=sum(otros_ingresos_pais,na.rm = TRUE),Nro_personas_otros_ingresos_otros_paises=sum(otros_ingresos_otros_paises,na.rm = TRUE),
Nro_personas_otros_ingresos_instituciones=sum(otros_ingresos_instituciones,na.rm = TRUE), Nro_personas_otras_ganancias=sum(otras_ganancias,na.rm = TRUE),
Nro_personas_PET=sum(Pet,na.rm = TRUE),Nro_personas_ocupadas=sum(Oc,na.rm = TRUE), Nro_personas_desempleadas=sum(Des,na.rm = TRUE),
Nro_personas_inactivas=sum(Ina,na.rm = TRUE))

summary(nv_training_hogares)
summary(nv_training_personas)

## Se unen las bases de datos:

nv_training_hogares_VF <- left_join(nv_training_hogares, nv_training_hogares1)

saveRDS(nv_training_hogares_VF, file = "nv_training_hogares_VF.rds")

colnames (nv_training_hogares_VF)

summary(nv_training_hogares_VF)

str(nv_training_hogares_VF)


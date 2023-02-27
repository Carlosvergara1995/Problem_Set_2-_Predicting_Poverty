#..

install.packages("gtsummary")
install.packages("pastecs")
library(data.table)
library(dplyr)
library(R.utils)
library(readr)
library(pastecs)
library(gtsummary)
library(ggplot2)

setwd("H:/DIAN/A?o 2023/Alexandra/Taller 2")

training.personas = readRDS(gunzip("df_training_personas.gz"))
table(training.personas$Dominio)
View(training.personas)
training.hogares = readRDS(gunzip("df_training_hogares.gz"))
View(training.hogares)
table(training.hogares$Dominio)

#####Haciendo merge#####

df.training = merge(training.personas, training.hogares, by = "id", all.x = T)
names(df.training)
View(df.training)

str(df.training)
summary(df.training)

####Creando variable sexo####

df.training <- df.training %>%
  mutate(Sexo = recode(P6020, "1" = "Hombre",
                          "2" = "Mujer",
                          )
  )
table(df.training$Sexo)
table(df.training$Pobre)

####Base training 

## Se seleccionan variables de interes

df.training.f <- df.training %>% select(c("Dominio.x", "Sexo", "P6040","Nper", "Npersug", "Ingtotug", "Ingtotugarr", "Ingpcug",  "Li", "Lp", "Pobre", "Indigente", "Npobres", "Nindigentes", "Oc", "Des", "Ina", "Impa", "Isa", "Ie",  "Imdi", "Iof1", "Iof2", "Iof3h", "Iof3i", "Iof6", "Cclasnr2", "Cclasnr3", "Cclasnr4", "Impaes", "Isaes",  "Iees", "Imdies", "Iof1es", "Iof2es", "Iof3hes", "Iof3ies", "Iof6es", "Ingtotob", "Ingtotes", "Ingtot"))
View(df.training)
names(df.training)

####Estad?sticas 

tbl_summary(df.training.f, by= Pobre, statistic = list (all_continuous()~"{mean} ({sd})"))

####Gr?fico de dispersi?n ingresos

ggplot(data = df.training.f, mapping = aes(x = P6040 , y = Ingtot/1000000)) +
  labs(x = "Edad", y = "Ingreso total") +
  geom_point(aes(colour = factor(Pobre)), size = 3) 

####Creanco ingreso en millones de $

df.training.f$ingtenmillon = df.training.f$Ingtot/1000000

####Gr?fico por grupos

options(scipen = 999)
mosaicplot(Sexo~Pobre,data=df.training.f, col=c("Blue","Red"))
ggplot(df.training.f, aes(x = ingtenmillon, fill = factor(Pobre))) + 
  geom_histogram(alpha = 0.5, position = "dodge")+labs(x = "Ingreso en millones de $", y = "Conteo")


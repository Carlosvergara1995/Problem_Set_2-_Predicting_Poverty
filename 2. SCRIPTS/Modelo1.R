#Modelo 1 
#revisamos las variables de cada training y test
colnames(test)
colnames(training)

#ANALISIS DE VARIABLES####
# comparar los nombres de las variables
diff_variables <- setdiff(names(training), names(test))

# imprimir las variables diferentes
print(diff_variables)

glimpse(training)
glimpse(test)

#dummyficamos las variables categoricas 
dumificadortraining <- dummyVars(formula = ~ . + I(age^2) + I(yearsmarried^2) - 
                           affairs - 1, data = Affairs, fullRank = T)
db <- predict(dumificadortraining, newdata = training)
db <- as.data.frame(db)

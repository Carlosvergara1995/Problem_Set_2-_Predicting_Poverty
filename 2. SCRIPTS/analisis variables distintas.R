###################################################################################################################################################################
#analizamos las variables presentes en ambas bases 
#para hogares 
colnames(df_test_hogares)
colnames(df_training_hogares)

# comparar los nombres de las variables
diff_variables <- setdiff(names(df_training_hogares), names(df_test_hogares))
diff_variables <- diff_variables[diff_variables != "Pobre"]
# imprimir las variables diferentes
print(diff_variables) #variables que estan en training y no en test

#analizamos las variables presentes en ambas bases 
#para hogares 
colnames(df_test_personas)
colnames(df_training_personas)

# comparar los nombres de las variables menos la variable pobre
diff_variables_p <- setdiff(names(df_training_personas), names(df_test_personas))

# imprimir las variables diferentes
print(diff_variables_p) #variables que estan en training y no en test

#creamos una nueva base de training menos las variables diferentes 
nv_training_hogares <- df_training_hogares%>%select(-all_of(diff_variables))

#realizamos el mismo proceso para la base de training de personas 
nv_training_personas<- df_training_personas %>% select(-all_of(diff_variables_p))


#en este punto tenemos las mismas variables en ambas bases tanto para training como para test personas y hogares
saveRDS(nv_training_hogares, file = "nv_training_hogares.rds")
saveRDS(nv_training_personas, file = "nv_training_personas.rds")


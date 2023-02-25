
#------------------------------------------------------------------------------#
#
#                           4. FINAL MODEL
#
#------------------------------------------------------------------------------#

#Lectura de datos Test para el modelo final

  test_hogares      <- read.csv("test_hogares.csv")
  test_personas     <- read.csv("test_personas.csv")



#Generación del Modelo de Entrega-----------------------------------------------

  
  best_model <- 
  
  poverty <- predict(best_model, test_hogares)
  modelo_final <- poverty %>% select(id, Pobre_1)
  write.csv(modelo_final, "modelo_final.csv")



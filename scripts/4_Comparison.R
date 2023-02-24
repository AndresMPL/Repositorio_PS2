
#------------------------------------------------------------------------------#
#
#                           4. MODEL COMPARISON
#
#------------------------------------------------------------------------------#

#Imágenes en el Problem Set

  Imagen_1

#Comparación resultados de los modelos------------------------------------------




#Generación del Modelo de Entrega-----------------------------------------------
#Ejemplo Archivo "sample_submission.csv"
  
  
  best_model <- 
  
  poverty <- predict(best_model, test_hogares)
  modelo_final <- poverty %>% select(id, Pobre_1)
  write.csv(modelo_final, "modelo_final.csv")



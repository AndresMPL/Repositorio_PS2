
#------------------------------------------------------------------------------#
#
#                       3. REGRESSION INCOMES
#
#------------------------------------------------------------------------------#

#Modelos de Regresión-----------------------------------------------------------

#Predicción de ingresos - Intento de predecir primero los ingresos y 
#luego predecir indirectamente 0 (no pobre) y 1 (pobre).

  #Generamos grilla de lambdas
  
  #TrainControl

  
##Regresión lineal----

  #Generamos el modelo
  
  #Evaluamos el modelo
  
    #In Sample
  
    #Out of sample
  
    #Comparamos las evaluaciones del modelo

  #Métricas dentro y fuera de muestra


##Ridge-------------------------------------------------------------------------

  #Generamos el modelo - BD Train
  
  #Evaluamos el cambio del modelo con diferentes lambdas y generamos la gráfica
  
    #Put coefficients in a data frame, except the intercept
  
    #Add the lambda grid to to data frame
  
    #ggplot 
  
  #Generamos las predicciones con los lambdas - BD Test
  
  #Cada predicción se evalúa - BD Test
  
    #Gráfica RMSE
  
    #Gráfica R2
  
  #Guardamos el mejor Ridge
  
  #Evaluamos con el mejor Ridge dentro y fuera de muestra
  
  #Métricas dentro y fuera de muestra
  
  #Guardamos el desempeño
  
  #Juntamos resultados con el modelo anterior


##Lasso-------------------------------------------------------------------------

  #Generamos el modelo - BD Train
  
  #Evaluamos el cambio del modelo con diferentes lambdas y generamos la gráfica
  
  #Generamos las predicciones con los lambdas - BD Test
  
  #Cada predicción se evalúa - BD Test
  
    #Gráfica RMSE
  
    #Gráfica R2
  
  #Guardamos el mejor Lasso
  
  #Evaluamos con el mejor Lasso dentro y fuera de muestra
  
  #Métricas dentro y fuera de muestra
  
  #Guardamos el desempeño
  
  #Juntamos resultados con el modelo anterior
  

##EN----


  #Generamos el modelo - BD Train
  
    #Secuencia Lambda EN - lambda = seq(0.001,0.02,by = 0.001)
  
  #Evaluamos el cambio del modelo con diferentes lambdas y generamos la gráfica
  
  #Generamos las predicciones con los lambdas - BD Test
  
  #Cada predicción se evalúa - BD Test
  
    #Gráfica RMSE
  
    #Gráfica R2
  
  #Guardamos el mejor Lasso
  
  #Evaluamos con el mejor Lasso dentro y fuera de muestra
  
  #Métricas dentro y fuera de muestra
  
  #Guardamos el desempeño
  
  #Juntamos resultados con el modelo anterior
  

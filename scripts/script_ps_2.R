
#------------------------------------------------------------------------------#
#
#                                PROBLEM SET 2
#
#   Grupo 5:  Isabella Mendez Pedraza.
#             Manuela Ojeda Ojeda.
#             Juan Sebastian Tellez Melo.
#             Andres Mauricio Palacio Lugo.
#
#------------------------------------------------------------------------------#

#Pasos previos------------------------------------------------------------------

  rm(list=ls())

#Cargar librerías

  require(pacman)
  p_load(tidyverse,caret, skimr, stargazer)
  

#Generamos una semilla

    set.seed(010101)

    
#Leer los datos

  setwd("C:/Users/User/Documents/Big_Data/BD_Taller 2")

  sample_sub        <- read.csv("sample_submission.csv")
  test_hogares      <- read.csv("test_hogares.csv")
  test_personas     <- read.csv("test_personas.csv")
  train_hogares     <- read.csv("train_hogares.csv")
  train_personas    <- read.csv("train_personas.csv")

#Revisión de las BD
  
  stargazer(test_hogares, header=FALSE, type='text',title="Tabla Test Hogares")
  stargazer(test_personas, header=FALSE, type='text',title="Tabla Test Personas")
  stargazer(train_hogares, header=FALSE, type='text',title="Tabla Train Hogares")
  stargazer(train_personas, header=FALSE, type='text',title="Tabla Train Personas")

  #Ingreso = (Ingpcug) Ingreso percápita de la unidad de gasto con imputación de arriendo a propietarios y usufructuarios
  
  #Variable Pobre = train_hogares$Pobre
  #Variable Ingresos = train_hogares$Ingpcug
  
  str(train_hogares$Pobre)
  table(train_hogares$Pobre)
  prop.table(table(train_hogares$Pobre))
  
  
#Limpieza BD----------------------------------------------------------------------

  #Evaluamos los NA de la base
  
  #Generamos y transformamos variables
  
  #Seleccionamos las variables relevantes
  
  #Arreglamos las variables dicótomas para que sean 1 y 0
  
  #Definimos las variables categoricas
  
  #Dividimos la BD en Train y Test
  
      #y_train
      #X_train
      #y_test
      #X_test
  
  #Procedemos a generar dummys
  
  #Revisamos la distribución de nuestra variable a predecir


#Modelo de Clasificación--------------------------------------------------------
  #Enfoque de clasificación - Intento de predecir directamente 0 (no pobre) y 1 (pobre)

  #Mutamos factores
  
  #create Data Partition

  
##Evaluación Desbalance de clases-----

  #Evaluación
  
  #Selección de variables
  
  #Dummyficamos
  
  #Dividimos train/test (70/30)
  
  #Estandarizamos
  
  #Evaluamos modelos In Sample - Out of Sample
  
      #Normal
  
      #Upsampling
  
      #Oversampling
  
      #Downsampling
  
      #Threshold óptimo
  
      #Cambiar función de costo
  

##Logit----

    #fiveStats 
  
    #trainControl
  
    #mylogit_caret - TEST
  
    #predictTest_logit - TRAIN
  
  
##LDA - Caret----

  #Generamos modelo - TRAIN
  
  #Generamos predicciones - TEST
  

##ROC performance----

  #In-sample (Logit, LDA)
  
  #Out-of-sample (Logit, LDA)


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
  

#Comparación resultados de los pronósticos--------------------------------------

  
  
  
#Generación del Modelo de Entrega-----------------------------------------------

  
  
  
#Ejemplo Archivo "sample_submission.csv"
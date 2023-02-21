
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


#Definición de variables a trabajar---------------------------------------------
  
  

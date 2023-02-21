
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

  #install.packages("kableExtra")
  library(pacman, kableExtra)
  p_load(tidyverse,caret, skimr, stargazer, dplyr)


#Generamos una semilla para trabajar en todo el procesamiento

  set.seed(010101)


#Leer los datos - 

  setwd("C:/Users/User/Documents/Big_Data/BD_Taller 2") #Por tamaño de los archivos, seleecionar el directorio local
  
  test_hogares      <- read.csv("test_hogares.csv")
  test_personas     <- read.csv("test_personas.csv")
  train_hogares     <- read.csv("train_hogares.csv")
  train_personas    <- read.csv("train_personas.csv")

#Organización de las BD

  #test_hogares      <- para la prueba final del modelo seleccionado
  #test_personas     <- para la prueba final del modelo seleccionado
  
  #train_hogares     <- para la generación del modelo la dividimos en dos
  #train_h       <- BD inicial de Train Hogares para -train- del modelo
  #test_h        <- BD inicial de Train Hogares para -test - del modelo
  
  #train_personas 

#Limpieza BD y selección de variables--------------------------------------------

  #Depuramos las variables que trabajaremos y evaluamos NA

  train_h <- train_hogares %>% select(all_of(colnames(test_hogares)))    #Dejamos las mismas variables de los archivos de Test
  train_p <- train_personas %>% select(all_of(colnames(test_personas)))
  
  glimpse(train_h)
  glimpse(train_p)
  
  factores <- colnames(select(train_p, -id, -Orden, -Clase, -Dominio, -P6040, -Fex_c, -Fex_dpto, -P6426, -P6800))
  
  for (v in factores) 
  {train_p[, v] <- as.factor(train_p[, v, drop = T])}
  
  glimpse(train_p) 
  
  sapply(train_h, function(x) sum(is.na(x)))
  sapply(train_p, function(x) sum(is.na(x)))



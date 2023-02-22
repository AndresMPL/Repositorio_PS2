
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

  library(pacman)
  p_load(tidyverse,caret, skimr, stargazer, dplyr, kableExtra)


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
  
  #train_personas    <- para la generación del modelo la dividimos en dos
  #train_p       <- BD inicial de Train Personas para -train- del modelo
  #test_p        <- BD inicial de Train Personas para -test - del modelo


#Limpieza BD y selección de variables--------------------------------------------

#Depuramos las variables que trabajaremos y evaluamos NA

  train_h <- train_hogares %>% select(all_of(colnames(test_hogares)))    #Dejamos las mismas variables de los archivos de Test
  train_p <- train_personas %>% select(all_of(colnames(test_personas)))
  
  glimpse(test_hogares)
  glimpse(test_personas)
  glimpse(train_h)
  glimpse(train_p)
  
  factores <- colnames(select(train_p, -id, -Orden, -Clase, -Dominio, -P6040, -Fex_c, -Fex_dpto, -P6426, -P6800, -Pet, -Oc, -Des, -Ina, -P7045, -Depto, -Oficio, -P6210s1))
  
  for (v in factores) 
  {train_p[, v] <- as.factor(train_p[, v, drop = T])}
  
  glimpse(train_p) 

#NA

  missing_percentage <-sapply(train_p, function(y) sum(length(which(is.na(y))))/length(train_p$id))
  data_x <- as.data.frame(missing_percentage)
  var <- cbind(Var_name = rownames(data_x), data_x)
  rownames(var) <- 1:nrow(var)
  var_delete <- var[var$missing_percentage>=0.5,]
  var_keep <- var[var$missing_percentage<0.5,]
  count(var) # Contamos cuantas variables tenemos en total 
  count(var_keep) # Contamos cuantas variables tienen % missing menor o igual a 50%
  count(var_delete) # Contamos cuantas variables tienen % missing mayor a 50% 
  
  View(var_keep)
  
  conteo_variables <- data.frame( total_var = nrow(var),
                                  keep_bar = nrow(var_keep),
                                  delete_var = nrow(var_delete))
  
  conteo_variables #comprobamos el número de variables
  vector_var <- as.vector(var_keep[1]) #Llevamos las variables que trabajaremos a un vector

#Limpiamos los datos de NA´s

  sapply(train_h, function(x) sum(is.na(x))) %>% as.data.frame()
  sapply(train_p, function(x) sum(is.na(x))) %>% as.data.frame()
  
  train_p <- train_p %>% filter(P6100 != "NA")   
  
  sapply(train_p, function(x) sum(is.na(x))) %>% as.data.frame() 


#Seleccionamos las variables que cumplen con el requisito y generamos y validamos las variables que necesitamos

  train_p <- train_p %>% select(all_of(vector_var$Var_name))

#Generamos dummys en Train_Personas

  dmy <- dummyVars("~.", data = train_p)
  head(dmy)
  train_p <- data.frame(predict(dmy, newdata = train_p))
  
  glimpse(train_p)


#Generamos las variables de Train_Personas y las llevamos a Train_Hogares




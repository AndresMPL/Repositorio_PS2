
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

#Pasos previos

  rm(list=ls())


#Cargar librerías
  
  install.packages("fastDummies")
  library(pacman)
  p_load(tidyverse,caret, glmnet, skimr, stargazer, dplyr, kableExtra, AER, MLmetrics, tidymodels, themis, smotefamily, ROSE, fastDummies)

  
#Leer los datos - 

  setwd("C:/Users/User/Documents/Big_Data/BD_Taller 2") #Por tamaño de los archivos, seleecionar el directorio local
  
  test_hogares      <- read.csv("test_hogares.csv")
  test_personas     <- read.csv("test_personas.csv")
  train_hogares     <- read.csv("train_hogares.csv")
  train_personas    <- read.csv("train_personas.csv")

  prop.table(table(train_hogares$Pobre))
  
  
#Organización de las BD

    #test_hogares      <- para la prueba final del modelo seleccionado
    #test_personas     <- para la prueba final del modelo seleccionado
    
    #train_hogares     <- BD Original
       #train_h        <- BD ajustada para trabajar
          #train_hh    <- 70% Train hogares - para entreamiento | Las BD estabndarizadas tendrán (s) al final
          #test_hh     <- 20% Train hogares - para pruebas      |
          #eval_hh     <- 10% Train hogares - para evaluacion   | 
    
    #train_personas    <- BD Original
       #train_p        <- BD ajustada para trabajar
          #train_pp    <- 70% Train personas - para entreamiento
          #test_pp     <- 20% Train personas - para pruebas
          #eval_pp     <- 10% Train personas - para evaluacion  
  
#Selección de Variables de interés
  
  train_p <- train_personas %>% 
    select(id, Clase, Orden, P6020, P6040, P6050, P6090, P6100, P6210, Oc, Des, Ina, Ingtot)
  
  train_h <- train_hogares %>% 
    select(Pobre, id, Clase, P5000, P5010, P5090, Nper, Npersug, Lp, Ingtotugarr, Ingpcug)
  
  glimpse(test_hogares)
  glimpse(test_personas)
  glimpse(train_h)
  glimpse(train_p)
  
  
#Crear variables de interes  
  
  train_p$female <- ifelse(train_p$P6020 == 2, 1, 0) %>% as.numeric()
  train_p$Menores_edad <- if_else(train_p$P6040<=14, 1, 0 , missing = NULL)
  train_p$adulto_mayor <- if_else(train_p$P6040>=65, 1, 0 , missing = NULL)
  train_p <- train_p %>%
                    mutate(Desempleado = replace_na(Des, 0),
                    Inactivo = replace_na(Ina, 0),
                    Ocupado = replace_na(Oc, 0))
  
  train_p$no_ingresos <- if_else(train_p$P6040>=65, 1, 0 , missing = NULL)
  
  
  train_personas_hog <- train_p %>%
                        group_by(id) %>%
                        summarize(edad_jefe_hogar = (P6040)[P6050==1], 
                        sexo_jefe_hogar = (female)[P6050==1],
                        nivel_edu_jefe_hogar= (P6210)[P6050==1],
                        num_Menores_edad = sum(Menores_edad, na.rm = TRUE),
                        num_adulto_mayor = sum(adulto_mayor, na.rm = TRUE),
                        jefe_hogar_des = (Desempleado)[P6050==1],
                        jefe_hogar_ina = (Inactivo)[P6050==1],
                        jefe_hogar_oc = (Ocupado)[P6050==1],
                        ing_tot_hogar = sum(Ingtot, na.rm = TRUE))
  
#Uniendo bases
  
  train_h <- left_join(train_h,train_personas_hog)
  colnames(train_h) 

  
#Variables con NA´s

  #Personas
    missing_percentage <-sapply(train_p, function(y) sum(length(which(is.na(y))))/length(train_p$id))
    data_x <- as.data.frame(missing_percentage)
    var <- cbind(Var_name = rownames(data_x), data_x)
    rownames(var) <- 1:nrow(var)
    var_delete <- var[var$missing_percentage>0.5,]
    var_keep <- var[var$missing_percentage<=0.5,]
    count(var) # Contamos cuantas variables tenemos en total 
    count(var_keep) # Contamos cuantas variables tienen % missing menor o igual a 50%
    count(var_delete) # Contamos cuantas variables tienen % missing mayor a 50% 
    
    conteo_variables <- data.frame( total_var = nrow(var),
                                    keep_bar = nrow(var_keep),
                                    delete_var = nrow(var_delete))
    
    conteo_variables #Tabla de clasificación de variables
   
    vector_var <- as.vector(var_keep[1]) #Llevamos las variables que trabajaremos a un vector
    
    train_p <- train_p %>% select(all_of(vector_var$Var_name))
    
    sapply(train_p, function(x) sum(is.na(x))) %>% as.data.frame()
    
    #Hogares
    missing_percentage2 <-sapply(train_h, function(y) sum(length(which(is.na(y))))/length(train_h$id))
    data_x2 <- as.data.frame(missing_percentage2)
    var2 <- cbind(Var_name = rownames(data_x2), data_x2)
    rownames(var2) <- 1:nrow(var2)
    var_delete2 <- var2[var2$missing_percentage2>0.5,]
    var_keep2 <- var2[var2$missing_percentage2<=0.5,]
    count(var2) # Contamos cuantas variables tenemos en total 
    count(var_keep2) # Contamos cuantas variables tienen % missing menor o igual a 50%
    count(var_delete2) # Contamos cuantas variables tienen % missing mayor a 50% 
    
    conteo_variables2 <- data.frame( total_var = nrow(var2),
                                    keep_bar = nrow(var_keep2),
                                    delete_var = nrow(var_delete2))
    
    conteo_variables2 #Tabla de clasificación de variables
    
    vector_var2 <- as.vector(var_keep2[1]) #Llevamos las variables que trabajaremos a un vector
    
    train_h <- train_h %>% select(all_of(vector_var2$Var_name))
    
    sapply(train_h, function(x) sum(is.na(x))) %>% as.data.frame() #no se excluyen variables
    
#Mutamos factores
    
    names(train_p)
    names(train_h)
    
  #Factores de Personas
      
      factoresp <- c("P6050", "P6210", "Clase")
      
      for (v in factoresp) {
        train_p[, v] <- as.factor(train_p[, v, drop = T])
        }

  #Factores de Hogares

      factoresh <- c("P5090", "Clase", "nivel_edu_jefe_hogar")
      
      for (v in factoresh) {
        train_h[, v] <- as.factor(train_h[, v, drop = T])
        }

#Generamos dummys en Train_Personas
  
      train_p <- dummy_cols(train_p, 
                            select_columns = c("P6050", "P6210"), 
                            remove_selected_columns = TRUE)
      
      glimpse(train_p)
    
#Generamos dummys en Train_Hogares
      
      train_h <- dummy_cols(train_h, 
                            select_columns = c("P5090", "nivel_edu_jefe_hogar"), 
                            remove_selected_columns = TRUE)
     
      glimpse(train_h)
      
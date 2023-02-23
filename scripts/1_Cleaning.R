
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
  p_load(tidyverse,caret, skimr, stargazer, dplyr, kableExtra, AER, MLmetrics, tidymodels, themis, smotefamily)


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
       #train_h        <- BD para ajustada para trabajar
          #train_hh    <- 70% Train hogares - para entreamiento | Las BD estabndarizadas tendrán (s) al final
          #test_hh     <- 20% Train hogares - para pruebas      |
          #eval_hh     <- 10% Train hogares - para evaluacion   | 
    
    #train_personas    <- BD Original
       #train_p        <- BD para ajustada para trabajar
          #train_pp    <- 70% Train personas - para entreamiento
          #test_pp     <- 20% Train personas - para pruebas
          #eval_pp     <- 10% Train personas - para evaluacion  
  
#Limpieza BD y selección de variables--------------------------------------------

  #Igualamemos las variables con las BD de Test

  train_h <- train_hogares %>% select(id, Clase, Dominio,P5000,P5010, P5090, P5100, P5130, P5140, Nper, Npersug, Li, 
                                      Lp, Fex_c, Depto, Fex_dpto, Pobre) #Dejamos las mismas variables de los archivos de Test y "Pobre"
  
  train_p <- train_personas %>% select(all_of(colnames(test_personas))) #Agregar Ingresos
  
  glimpse(test_hogares)
  glimpse(test_personas)
  glimpse(train_h)
  glimpse(train_p)
  
  
  #Mutamos factores 
  
    #Personas
    factoresp <- colnames(select(train_p, -id, -Orden, -Clase, -Dominio, -P6040, -Fex_c, -Fex_dpto, -P6426, -P6800, -Pet, -Oc, -Des, -Ina, -P7045, -Depto, -Oficio, -P6210s1))
    
    for (v in factoresp) 
      {train_p[, v] <- as.factor(train_p[, v, drop = T])}
    
    glimpse(train_p) 
    
    #Hogares
    factoresh <- colnames(select(train_h,P5090, Clase))
    
    for (v in factoresh) 
      {train_h[, v] <- as.factor(train_h[, v, drop = T])}
    
    glimpse(train_h) 
  
  
  #Eliminamos NA´s
  
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
    
    train_p <- train_p %>% filter(P6100 != "NA")   
        
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
    
    sapply(train_h, function(x) sum(is.na(x))) %>% as.data.frame()
    
    train_h <- train_h %>% filter(P5130 != "NA")   
    
    sapply(train_h, function(x) sum(is.na(x))) %>% as.data.frame() 
    
  
  #Generamos dummys en Train_Personas
  
    dmyp <- dummyVars("~.", data = train_p)
    head(dmyp)
    train_p <- data.frame(predict(dmyp, newdata = train_p))
    
    glimpse(train_p)

    
  #Generamos dummys en Train_Hogares
  
    dmyh <- dummyVars("~.", data = train_h)
    head(dmyh)
    train_h <- data.frame(predict(dmyh, newdata = train_h))
    
    glimpse(train_h)
  
    
  #Generamos las variables de Train_Personas y las llevamos a Train_Hogares
  
  
  

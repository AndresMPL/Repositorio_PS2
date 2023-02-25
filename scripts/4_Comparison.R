
#------------------------------------------------------------------------------#
#
#                           4. FINAL MODEL
#
#------------------------------------------------------------------------------#

metricas_inicial <- metricas

print(xtable(metricas), include.rownames = FALSE)

#Lectura de datos Test para el modelo final

  test_hogares      <- read.csv("test_hogares.csv")
  test_personas     <- read.csv("test_personas.csv")

  
#Selección de Variables
  
  test_p <- test_personas %>% 
    select(id, Clase, Orden, P6020, P6040, P6050, P6090, P6100, P6210, Pet, Oc, Des, Ina, Ingtot)
  
  test_h <- test_hogares %>% 
    select(Pobre, id, Clase, P5000, P5010, P5090, Nper, Npersug, Lp, Ingtotugarr, Ingpcug)
  
  glimpse(test_h)
  glimpse(test_p)
  
  
#Creación de variables
  
  test_p$Genero <- ifelse(test_p$P6020 == 2, 1, 0) %>% as.numeric()
  test_p$Menores_edad <- if_else(test_p$P6040<=14, 1, 0 , missing = NULL)
  test_p$adulto_mayor <- if_else(test_p$P6040>=65, 1, 0 , missing = NULL)
  test_p <- test_p %>%
    mutate(Desempleado = replace_na(Des, 0),
           Inactivo = replace_na(Ina, 0),
           Ocupado = replace_na(Oc, 0),
           Pet = replace_na(Pet,0))
  
  test_p$no_ingresos <- if_else(test_p$P6040>=65, 1, 0 , missing = NULL)
  test_p$Clase <- if_else(test_p$Clase== 2, 1, 0 , missing = NULL)
  test_h$Clase <- if_else(test_h$Clase== 2, 1, 0 , missing = NULL)

  test_personas_hog <- test_p %>%
                        group_by(id) %>%
                        summarize(edad_jefe_hogar = (P6040)[P6050==1], 
                                  sexo_jefe_hogar = (Genero)[P6050==1],
                                  nivel_edu_jefe_hogar= (P6210)[P6050==1],
                                  num_Menores_edad = sum(Menores_edad, na.rm = TRUE),
                                  num_adulto_mayor = sum(adulto_mayor, na.rm = TRUE),
                                  jefe_hogar_des = (Desempleado)[P6050==1],
                                  jefe_hogar_ina = (Inactivo)[P6050==1],
                                  jefe_hogar_oc = (Ocupado)[P6050==1],
                                  num_oc_hogar = sum(Ocupado))
  
  test_h <- test_h %>% rename(num_cuartos = P5000, num_cuartos_dormir = P5010) #se renombran variables
  test_p <- test_p %>% rename(edad = P6040) #se renombran variables
  
  #Uniendo bases
  
  test_h <- left_join(test_h,test_personas_hog)
  colnames(test_h) 
  
  #Creacion de variables 
  
  test_h$jefe_hogar_ina2 <- if_else(test_h$edad_jefe_hogar==11, as.integer(1), test_h$jefe_hogar_ina) ##observacion de 11 años que no clasifica como inactivo, desempleado, ocupado se asigna como inactiva
  test_h <- test_h %>% select(-jefe_hogar_ina)# se retira la variable antigua
  test_h <- test_h %>% rename(jefe_hogar_ina = jefe_hogar_ina2)#se renombra la nueva
  
  test_h <- test_h %>% mutate(Numper_por_dor= Nper/num_cuartos_dormir,
                                Hacinamiento = if_else(Numper_por_dor>3, 1 , 0),
                                Ocupados_por_perhog = if_else(num_oc_hogar>0, Npersug/num_oc_hogar, as.double(Npersug)))
  
  #Variables con NA´s
  
  #Personas
  missing_percentage <-sapply(test_p, function(y) sum(length(which(is.na(y))))/length(test_p$id))
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
  
  test_p <- test_p %>% select(all_of(vector_var$Var_name))
  
  sapply(test_p, function(x) sum(is.na(x))) %>% as.data.frame()
  
  #Hogares
  missing_percentage2 <-sapply(test_h, function(y) sum(length(which(is.na(y))))/length(test_h$id))
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
  
  test_h <- test_h %>% select(all_of(vector_var2$Var_name))
  
  sapply(test_h, function(x) sum(is.na(x))) %>% as.data.frame() #no se excluyen variables
  
  #Mutamos factores
  
  names(test_p)
  names(test_h)
  
  #Factores de Personas
  
  test_p <- test_p %>% mutate(Clase=factor(Clase,levels=c(0,1),labels=c("Urbano","Rural")),
                                Genero=factor(Genero,levels=c(0,1),labels=c("Hombre","Mujer ")),
                                Parentesco_con_jefe=factor(P6050,levels=c(1,2,3,4,5,6,7,8,9),labels=c("jefe", "pareja", "Hijo/hija", "nieto", "otro", "emplead_servicio", "pensionista", "trabajador", "otro_no_pariente")),
                                nivel_edu=factor(P6210,levels=c(1,2,3,4,5,6,9),labels=c("Ninguno", "Preescolar", "Basica_primaria", "Basica_secundaria", "Media", "Superior", "No_Saber")),
                                Desempleado=factor(Desempleado,levels=c(0,1),labels=c("No","Si")),
                                Inactivo=factor(Inactivo,levels=c(0,1),labels=c("No","Si")),
                                Ocupado=factor(Ocupado,levels=c(0,1),labels=c("No","Si")),
                                Pet=factor(Pet,levels=c(0,1),labels=c("No","Si")),
                                no_ingresos=factor(no_ingresos,levels=c(0,1),labels=c("No","Si")))
  
  test_p <- test_p %>% select(-P6020, -P6050, -P6210) # se creo una nueva variable con factores y se elimino la anterior
  
  head(test_p)
  
  
  #Factores de Hogares
  
  test_h <- test_h %>% mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No_Pobre","Pobre")),
                                Clase=factor(Clase,levels=c(0,1),labels=c("Urbano","Rural")),
                                Vivienda=factor(P5090,levels=c(1,2,3,4,5,6),labels=c("Propia_paga","Propia_No_Paga", "Arriendo","Usufructo", "Ocupante_No_Dueño", "Otra")),
                                sexo_jefe_hogar=factor(sexo_jefe_hogar,levels=c(0,1),labels=c("Hombre","Mujer")),
                                nivel_edu_jefe_hogar=factor(nivel_edu_jefe_hogar,levels=c(1,2,3,4,5,6,9),labels=c("Ninguno", "Preescolar", "Basica_primaria", "Basica_secundaria", "Media", "Superior", "No_Saber")),
                                jefe_hogar_des=factor(jefe_hogar_des,levels=c(0,1),labels=c("No","Si")),
                                jefe_hogar_ina=factor(jefe_hogar_ina,levels=c(0,1),labels=c("No","Si")),
                                jefe_hogar_oc=factor(jefe_hogar_oc,levels=c(0,1),labels=c("No","Si")),
                                Hacinamiento = factor(Hacinamiento, levels = c(0,1), labels = c("No","Si")))
  
  test_h <- test_h %>% select(-P5090) # se creo una nueva variable con factores y se elimino la anterior
  
  glimpse(test_h)
  colnames(test_h)
  
  db <- dummyVars(~.,test_h) #Ver cuáles tienen solo dos factores para no dummyficar

  test_hd <- dummy_cols(test_h, 
                         select_columns = c("nivel_edu_jefe_hogar", "Vivienda"), 
                         remove_selected_columns = TRUE)
  
  test_hd <- test_hd %>% select(-id, -Nper, -Lp,-Ingtotugarr, -Ingpcug, -num_oc_hogar, -Vivienda_Propia_paga, -nivel_edu_jefe_hogar_Ninguno)
  
  glimpse(test_hd)
  test_h <- test_hd

#Calculamos con modelo seleccionado de todo el proceso 
  
  best_model <- 
  
  y_hat_test <- predict(best_model, newdata = test_h)
    
  
  
  
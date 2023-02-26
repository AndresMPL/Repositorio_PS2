
#------------------------------------------------------------------------------#
#
#                           4. FINAL MODEL
#
#------------------------------------------------------------------------------#

  metricas_total <- metricas
  
  metricas_eval <- metricas %>% filter(Evaluación == "Eval") %>% as.data.frame()
  View(metricas_eval)
  print(xtable(metricas_eval), include.rownames = FALSE)
  
  metricas_test <- metricas %>% filter(Evaluación == "Test") %>% as.data.frame()
  View(metricas_test)
  print(xtable(metricas_test), include.rownames = FALSE)

#Lectura de datos Test para el modelo final

  test_hogares      <- read.csv("test_hogares.csv")
  test_personas     <- read.csv("test_personas.csv")
  sample            <- read.csv("sample_submission.csv")
  
#Selección de Variables
  
  test_p <- test_personas %>% 
    select(id, Clase, Orden, P6020, P6040, P6050, P6090, P6100, P6210, Pet, Oc, Des, Ina)
  
  test_h <- test_hogares %>% 
    select(id, Clase, P5000, P5010, P5090, Nper, Npersug, Lp)
  
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
  missing_percentage3 <-sapply(test_p, function(y) sum(length(which(is.na(y))))/length(test_p$id))
  data_x3 <- as.data.frame(missing_percentage3)
  var3 <- cbind(Var_name = rownames(data_x3), data_x3)
  rownames(var3) <- 1:nrow(var3)
  var_delete3 <- var3[var3$missing_percentage3>0.5,]
  var_keep3 <- var3[var3$missing_percentage3<=0.5,]
  count(var3) # Contamos cuantas variables tenemos en total 
  count(var_keep3) # Contamos cuantas variables tienen % missing menor o igual a 50%
  count(var_delete3) # Contamos cuantas variables tienen % missing mayor a 50% 
  
  conteo_variables3 <- data.frame( total_var3 = nrow(var3),
                                  keep_bar3 = nrow(var_keep3),
                                  delete_var3 = nrow(var_delete3))
  
  conteo_variables3 #Tabla de clasificación de variables
  
  vector_var3 <- as.vector(var_keep3[1]) #Llevamos las variables que trabajaremos a un vector
  
  test_p <- test_p %>% select(all_of(vector_var3$Var_name))
  
  sapply(test_p, function(x) sum(is.na(x))) %>% as.data.frame()
  
  #Hogares
  missing_percentage4 <-sapply(test_h, function(y) sum(length(which(is.na(y))))/length(test_h$id))
  data_x4 <- as.data.frame(missing_percentage4)
  var4 <- cbind(Var_name = rownames(data_x4), data_x4)
  rownames(var4) <- 1:nrow(var4)
  var_delete4 <- var4[var4$missing_percentage4>0.5,]
  var_keep4 <- var4[var4$missing_percentage4<=0.5,]
  count(var4) # Contamos cuantas variables tenemos en total 
  count(var_keep4) # Contamos cuantas variables tienen % missing menor o igual a 50%
  count(var_delete4) # Contamos cuantas variables tienen % missing mayor a 50% 
  
  conteo_variables4 <- data.frame( total_var = nrow(var4),
                                   keep_bar = nrow(var_keep4),
                                   delete_var = nrow(var_delete4))
  
  conteo_variables4 #Tabla de clasificación de variables
  
  vector_var4 <- as.vector(var_keep4[1]) #Llevamos las variables que trabajaremos a un vector
  
  test_h <- test_h %>% select(all_of(vector_var4$Var_name))
  
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
  
  test_h <- test_h %>% mutate(Clase=factor(Clase,levels=c(0,1),labels=c("Urbano","Rural")),
                                Vivienda=factor(P5090,levels=c(1,2,3,4,5,6),labels=c("Propia_paga","Propia_No_Paga", "Arriendo","Usufructo", "Ocupante_No_Dueño", "Otra")),
                                sexo_jefe_hogar=factor(sexo_jefe_hogar,levels=c(0,1),labels=c("Hombre","Mujer")),
                                nivel_edu_jefe_hogar=factor(nivel_edu_jefe_hogar,levels=c(1,2,3,4,5,6,9),labels=c("Ninguno", "Preescolar", "Basica_primaria", "Basica_secundaria", "Media", "Superior", "No_Saber")),
                                jefe_hogar_des=factor(jefe_hogar_des,levels=c(0,1),labels=c("No","Si")),
                                jefe_hogar_ina=factor(jefe_hogar_ina,levels=c(0,1),labels=c("No","Si")),
                                jefe_hogar_oc=factor(jefe_hogar_oc,levels=c(0,1),labels=c("No","Si")),
                                Hacinamiento = factor(Hacinamiento, levels = c(0,1), labels = c("No","Si")))
  
  test_h <- test_h %>% select(-P5090) # se creo una nueva variable con factores y se elimino la anterior
  test_h$N_personas_hog <- test_h$Npersug
  glimpse(test_h)
  
  #Copia de la BD ajustada - Train Hogares
  backup <- test_h
  backup -> test_h
  
  #Generamos dummys en Train_Personas
  
  test_p <- dummy_cols(test_p, 
                        select_columns = c("Clase", "Pet", "Genero", "Desempleado", "Inactivo", "Ocupado", "Parentesco_con_jefe", "nivel_edu", "no_ingresos"), 
                        remove_selected_columns = TRUE)
  
  glimpse(test_p)
  
  #Generamos dummys en Train_Hogares
  
  test_h <- dummy_cols(test_h, 
                        select_columns = c("Clase", "Vivienda", "sexo_jefe_hogar", "nivel_edu_jefe_hogar", "jefe_hogar_des", "jefe_hogar_oc", "jefe_hogar_ina", "Hacinamiento"), 
                        remove_selected_columns = TRUE)
  
  glimpse(test_h)

  
  #Estandarizacion 
  
  test_h2  <- test_h
  
  variables_numericas <- c("num_cuartos", "num_cuartos_dormir", "Npersug",
                           "edad_jefe_hogar", "num_Menores_edad", "num_adulto_mayor", 
                           "Numper_por_dor", "Ocupados_por_perhog")
  
  
  
  escalador_test <- preProcess(test_h2[, variables_numericas],
                          method = c("center", "scale"))
  
  test_h2[, variables_numericas] <- predict(escalador_test, test_h2[, variables_numericas])
  
  
#Predicción sobre el modelo seleccionado
  
  final <- modelo11
  test_h$y_hat_final <- predict(final, newdata = test_h)
  
#Modelo seleccionado
  
  
  final <- modelo11
  
  coefs <- coef(final$finalModel) %>% as.data.frame()
  colnames(coefs)<-c("Modelo")
  round(coefs,4)
  coefs
  print(xtable(coefs), include.rownames = FALSE)

  
#Archivo de Kaggle
  
  exportar <- test_h %>% select(id, prediccion) %>% rename("pobre" = prediccion)
  write.csv(exportar, "modelo_kaggle.csv", row.names = FALSE)

  
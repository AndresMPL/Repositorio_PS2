
#------------------------------------------------------------------------------#
#
#                         2. POVERTY CLASSIFICATION
#
#------------------------------------------------------------------------------#

#Enfoque de clasificación - Intento de predecir directamente 0 (no pobre) y 1 (pobre)

#Pasos previos ----------------------------------------------------------------

  train_hd <- dummy_cols(train_h, 
                         select_columns = c("nivel_edu_jefe_hogar", "Vivienda"), 
                         remove_selected_columns = TRUE)
                
  prop.table(table(train_hd$Pobre)) #1-Pobre, 0-No Pobre
  prop.table(table(train_hd$Clase))
                
  train_hd <- train_hd %>% select(-id)
  #train_hd$Pobre <- as.factor(train_hd$Pobre)
                
  glimpse(train_hd)
                
  train_h <- train_hd
  
  glimpse(train_h)
  
#Evaluación desbalance
  
  prop.table(table(train_h$Pobre)) #Grado de desbalance Moderado
  
  Imagen_1 <- ggplot(train_h, aes(x = Pobre)) +
              geom_bar(fill = "#B5B5B5") +
              theme_bw() +
              scale_y_continuous(labels = label_number()) +
              labs(title = "Distribución de la Clasificación de Pobreza por hogares",
                   y = "Número de hogares",
                   x = "Clasificación")
  
  Imagen_1
  
#Dividimos train/test/eval (70/20/10) - BD Hogares

  set.seed(10110)
  index_1 <- createDataPartition(train_h$Pobre, p = 0.7)[[1]]
  train_hh  <- train_hd[index_1,]
  other     <- train_hd[-index_1,]

  set.seed(10110)
  index2  <- createDataPartition(other$Pobre, p = 1/3)[[1]]
  test_hh <- other[-index2,]
  eval_hh <- other[ index2,]

  dim(train_hh)   
  dim(test_hh)
  dim(eval_hh)
  
  dim(train_h)[1] - dim(train_hh)[1] - dim(test_hh)[1] - dim(eval_hh)[1] #Cero para verificar que las particiones hayan quedado bien
  
  
  prop.table(table(train_h$Pobre))    #Verificamos que las particiones conserven las mismas proporciones
  prop.table(table(train_hh$Pobre))
  prop.table(table(test_hh$Pobre))
  prop.table(table(eval_hh$Pobre))  

  
#Estandarizamos
  
  train_hhs <- train_hh #Guardamos las tres BD Originales aparte
  test_hhs  <- test_hh  
  eval_hhs  <- eval_hh

  colnames(train_hhs)
  glimpse(train_hhs)
  
  names <- data.frame(vars = colnames(train_hhs)) %>% 
                      filter(vars != "Pobre") 
  
  variables_numericas <- as.vector(names$vars)
  
  escalador <- preProcess(train_hh[, variables_numericas],
                          method = c("center", "scale"))
  
  train_hhs[, variables_numericas] <- predict(escalador, train_hh[, variables_numericas])
  test_hhs[, variables_numericas] <- predict(escalador, test_hh[, variables_numericas])
  eval_hhs[, variables_numericas] <- predict(escalador, eval_hh[, variables_numericas])  

  train_hhs <- train_hhs %>%  mutate(Pobre = factor(train_hhs$Pobre, 
                                                    levels = c(1, 0),
                                                    labels = c("Pobre", "No_Pobre"))) #Pobre=1, No Pobre=0
  
  test_hhs <- test_hhs %>%  mutate(Pobre = factor(test_hhs$Pobre, 
                                                      levels = c(1, 0),
                                                      labels = c("Pobre", "No_Pobre"))) #Pobre=1, No Pobre=0
  
  eval_hhs <- eval_hhs %>%  mutate(Pobre = factor(eval_hhs$Pobre, 
                                                      levels = c(1, 0),
                                                      labels = c("Pobre", "No_Pobre"))) #Pobre=1, No Pobre=0
  

  
#Control------------------------------------------------------------------------
  
  train_hhs$Pobre <- factor(train_hhs$Pobre)

  grilla <- 10^seq(10, -1, length = 100)
    
  fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
  
  control <- trainControl(method = "cv",
                       number = 5,
                       summaryFunction = fiveStats,
                       classProbs = TRUE,
                       verbose=FALSE,
                       savePredictions = T)

  
#1 - Logit sin regularizar ---------------------------------------------------------

  modelo1 <-   train(Pobre~., 
                     data = train_hhs,
                     method = "glmnet",
                     family = "binomial",
                     preProcess = NULL,
                     metric = 'Accuracy')
  modelo1
  
  y_hat_train1 <- predict(modelo1, train_hhs)
  y_hat_test1  <- predict(modelo1, test_hhs)
  y_hat_eval1  <- predict(modelo1, eval_hhs)
  
  probs_train1 <- predict(modelo1, train_hhs, type = "prob")
  probs_test1  <- predict(modelo1, test_hhs, type = "prob")
  probs_eval1  <- predict(modelo1, eval_hhs, type = "prob")
  
  acc_train1  <- Accuracy(y_pred = y_hat_train1, y_true = train_hhs$Pobre)
  acc_test1   <- Accuracy(y_pred = y_hat_test1, y_true = test_hhs$Pobre)
  acc_eval1   <- Accuracy(y_pred = y_hat_eval1, y_true = eval_hhs$Pobre)
  
  
  metricas_train1 <- data.frame(Modelo = "Logit", 
                                "Muestreo" = NA, 
                                "Evaluación" = "Entrenamiento",
                                "Accuracy" = acc_train1)
  
  metricas_test1 <- data.frame(Modelo = "Logit", 
                                "Muestreo" = NA, 
                                "Evaluación" = "Test",
                                "Accuracy" = acc_test1)
  
  metricas_eval1 <- data.frame(Modelo = "Logit", 
                                "Muestreo" = NA, 
                                "Evaluación" = "Evaluación",
                                "Accuracy" = acc_eval1)
  
  
  metricas1 <- bind_rows(metricas_train1, metricas_test1, metricas_eval1)
  metricas1 %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  
  ###1.1 Logit - Upsampling ----
  
  
  ###1.2 Logit - Oversampling ----
  
  
  ###1.3 Logit - Downsampling ----
  
  
  ###1.4 Logit - Threshold óptimo ----
  
  
  ###1.5 Logit - Cambiar función de costo ----
  
  
#2 - Logit con Lasso (1)------------------------------------------------------------

  set.seed(1010)
  
  modelo2 <- train(Pobre~P5000+P5010+Nper+Npersug+Lp+Ingtotugarr+Ingpcug+P5090_1+
                     P5090_2+P5090_3+P5090_4+P5090_5+P5090_6+Clase_1, 
                   data = train_hhs,
                   method = "glmnet",
                   trControl = control,
                   family = "binomial",
                   preProcess = NULL,
                   metric = 'Accuracy',
                   tuneGrid = expand.grid(alpha = 1,lambda=grilla))
  modelo2
  
  y_hat_train2 <- predict(modelo2, train_hhs)
  y_hat_test2  <- predict(modelo2, test_hhs)
  y_hat_eval2  <- predict(modelo2, eval_hhs)
  
  acc_train2  <- Accuracy(y_pred = y_hat_train2, y_true = train_hhs$Pobre)
  acc_test2   <- Accuracy(y_pred = y_hat_test2, y_true = test_hhs$Pobre)
  acc_eval2   <- Accuracy(y_pred = y_hat_eval2, y_true = eval_hhs$Pobre)
  
  
  metricas_train2 <- data.frame(Modelo = "Logit - Lasso", 
                                "Muestreo" = NA, 
                                "Evaluación" = "Entrenamiento",
                                "Accuracy" = acc_train2)
  
  metricas_test2 <- data.frame(Modelo = "Logit - Lasso", 
                               "Muestreo" = NA, 
                               "Evaluación" = "Test",
                               "Accuracy" = acc_test2)
  
  metricas_eval2 <- data.frame(Modelo = "Logit - Lasso", 
                               "Muestreo" = NA, 
                               "Evaluación" = "Evaluación",
                               "Accuracy" = acc_eval2)
  
  
  metricas2 <- bind_rows(metricas_train2, metricas_test2, metricas_eval2)
  metricas <- bind_rows(metricas1, metricas2)
  
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  
  ###2.1 Logit - Upsampling ----
  
  
  ###2.2 Logit - Oversampling ----
  
  
  ###2.3 Logit - Downsampling ----
  
  
  ###2.4 Logit - Threshold óptimo ----
  
  
  ###2.5 Logit - Cambiar función de costo ----
  
#3 - Logit con Ridge (0)------------------------------------------------------------
  
  modelo3 <- train(Pobre~P5000+P5010+Nper+Npersug+Lp+Ingtotugarr+Ingpcug+P5090_1+
                     P5090_2+P5090_3+P5090_4+P5090_5+P5090_6+Clase_1, 
                   data = train_hhs,
                   method = "glmnet",
                   trControl = control,
                   family = "binomial",
                   preProcess = NULL,
                   metric = 'Accuracy',
                   tuneGrid = expand.grid(alpha = 0,lambda=grilla))
  
  modelo3
  
  y_hat_train3 <- predict(modelo3, train_hhs)
  y_hat_test3  <- predict(modelo3, test_hhs)
  y_hat_eval3  <- predict(modelo3, eval_hhs)
  
  acc_train3  <- Accuracy(y_pred = y_hat_train3, y_true = train_hhs$Pobre)
  acc_test3   <- Accuracy(y_pred = y_hat_test3, y_true = test_hhs$Pobre)
  acc_eval3   <- Accuracy(y_pred = y_hat_eval3, y_true = eval_hhs$Pobre)
  
  
  metricas_train3 <- data.frame(Modelo = "Logit - Ridge", 
                                "Muestreo" = NA, 
                                "Evaluación" = "Entrenamiento",
                                "Accuracy" = acc_train3)
  
  metricas_test3 <- data.frame(Modelo = "Logit - Ridge", 
                               "Muestreo" = NA, 
                               "Evaluación" = "Test",
                               "Accuracy" = acc_test3)
  
  metricas_eval3 <- data.frame(Modelo = "Logit - Ridge", 
                               "Muestreo" = NA, 
                               "Evaluación" = "Evaluación",
                               "Accuracy" = acc_eval3)
  
  
  metricas3 <- bind_rows(metricas_train3, metricas_test3, metricas_eval3)
  metricas <- bind_rows(metricas, metricas3)
  
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  
  
  ###3.1 Logit - Upsampling ----
  
  
  ###3.2 Logit - Oversampling ----
  
  
  ###3.3 Logit - Downsampling ----
  
  
  ###3.4 Logit - Threshold óptimo ----
  
  
  ###3.5 Logit - Cambiar función de costo ----
  
  
  
#4 - Logit con EN-------------------------------------------------------------------
  
  modelo4 <- train(Pobre~P5000+P5010+Nper+Npersug+Lp+Ingtotugarr+Ingpcug+P5090_1+
                     P5090_2+P5090_3+P5090_4+P5090_5+P5090_6+Clase_1, 
                   data = train_hhs,
                   method = "glmnet",
                   trControl = control,
                   family = "binomial",
                   preProcess = NULL,
                   metric = 'Accuracy',
                   tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda=grilla))

  modelo4
  
  y_hat_train4 <- predict(modelo4, train_hhs)
  y_hat_test4  <- predict(modelo4, test_hhs)
  y_hat_eval4  <- predict(modelo4, eval_hhs)
  
  acc_train4  <- Accuracy(y_pred = y_hat_train4, y_true = train_hhs$Pobre)
  acc_test4   <- Accuracy(y_pred = y_hat_test4, y_true = test_hhs$Pobre)
  acc_eval4   <- Accuracy(y_pred = y_hat_eval4, y_true = eval_hhs$Pobre)
  
  
  metricas_train4 <- data.frame(Modelo = "Logit - EN", 
                                "Muestreo" = NA, 
                                "Evaluación" = "Entrenamiento",
                                "Accuracy" = acc_train3)
  
  metricas_test4 <- data.frame(Modelo = "Logit - EN", 
                               "Muestreo" = NA, 
                               "Evaluación" = "Test",
                               "Accuracy" = acc_test3)
  
  metricas_eval4 <- data.frame(Modelo = "Logit - EN", 
                               "Muestreo" = NA, 
                               "Evaluación" = "Evaluación",
                               "Accuracy" = acc_eval3)
  
  
  metricas4 <- bind_rows(metricas_train4, metricas_test4, metricas_eval4)
  metricas <- bind_rows(metricas, metricas4)
  
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  
  
  ###4.1 Logit - Upsampling ----
  
  
  ###4.2 Logit - Oversampling ----
  
  
  ###4.3 Logit - Downsampling ----
  
  
  ###4.4 Logit - Threshold óptimo ----
  
  
  ###4.5 Logit - Cambiar función de costo ----
  
  
  ## Up sampling-------------------------------------------------------------------

  upSampledTrain_h <- upSample(y = as.factor(train_hhs$Pobre),
                               x = select(train_hhs, -id, -Pobre),
                               yname = "Pobre")
  dim(train_hhs)
  
  dim(upSampledTrain_h)
  
  table(upSampledTrain_h$Pobre)
  
  
  #lambda_grid_h <- 10^seq(-4, 0.01, length = 300) #en la practica se suele usar una grilla de 200 o 300
  
  
 # modelo2 <- train(y = as.factor(upSampledTrain_h$Pobre),x = select(upSampledTrain_h),method = "glmnet",
                  # trControl = ctrl,
                  # family = "binomial", 
                  # metric = 'Accuracy',
                  # tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid_h))
  
  #modelo2

#------------------------------------------------------------------------------#
#
#                         2. POVERTY CLASSIFICATION
#
#------------------------------------------------------------------------------#

#Enfoque de clasificación - Intento de predecir directamente 0 (no pobre) y 1 (pobre)

#Pasos previos ----------------------------------------------------------------

  train_h <- train_h %>% mutate(Pobre=factor(Pobre_Pobre,levels=c(0,1),labels=c("No_Pobre","Pobre")))
  train_h <- train_h %>% select(-id, -Nper, -Lp,-Ingtotugarr, -Ingpcug, -num_oc_hogar, -Pobre_No_Pobre, -Pobre_Pobre, -Clase_Urbano, -Vivienda_Propia_paga, -sexo_jefe_hogar_Hombre, -nivel_edu_jefe_hogar_Ninguno, -jefe_hogar_des_No, -jefe_hogar_oc_No, -jefe_hogar_ina_No, -Hacinamiento_No, -jefe_hogar_oc_Si)
  
  glimpse(train_h)
  

#Evaluación desbalance

  prop.table(table(train_h$Pobre)) #1-Pobre, 0-No Pobre
  #Grado de desbalance Moderado
  
  
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
  index_1 <- createDataPartition(y=train_h$Pobre, p = 0.7, list = FALSE)
  train_hh  <- train_h[index_1,]
  other     <- train_h[-index_1,]
  
  set.seed(10110)
  index2  <- createDataPartition(other$Pobre, p = 1/3, list = FALSE)
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

  glimpse(train_hhs)

#names <- data.frame(vars = colnames(train_hhs)) %>% 
#filter(vars != "Pobre") 

  variables_numericas <- c("num_cuartos", "num_cuartos_dormir", "Npersug",
                           "edad_jefe_hogar", "num_Menores_edad", "num_adulto_mayor", 
                           "Numper_por_dor", "Ocupados_por_perhog")
  
  escalador <- preProcess(train_hh[, variables_numericas],
                          method = c("center", "scale"))
  
  train_hhs[, variables_numericas] <- predict(escalador, train_hh[, variables_numericas])
  test_hhs[, variables_numericas] <- predict(escalador, test_hh[, variables_numericas])
  eval_hhs[, variables_numericas] <- predict(escalador, eval_hh[, variables_numericas])  


#Control------------------------------------------------------------------------

  grilla <- 10^seq(10, -1, length = 100)
  
  fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
  
  control <- trainControl(method = "cv",
                          number = 5,
                          summaryFunction = fiveStats,
                          classProbs = TRUE,
                          verbose=FALSE,
                          savePredictions = T)


#1 - Logit sin regularizar ---------------------------------------------------------

  set.seed(10110)
  modelo1 <-   train(Pobre ~ . , 
                     data = train_hhs,
                     method = "glm",
                     trControl = control,
                     family = "binomial",
                     preProcess = NULL,
                     metric = 'ROC')
  modelo1
  
  y_hat_train1 <- predict(modelo1, newdata = train_hhs)
  y_hat_test1  <- predict(modelo1, newdata = test_hhs)
  y_hat_eval1  <- predict(modelo1, newdata = eval_hhs)
  
  confusionMatrix(y_hat_eval1, eval_hhs$Pobre)
  
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

  train_hhs$Pobre <- factor(train_hhs$Pobre)
  
  set.seed(10110)
  train_hhs11 <- upSample(x = train_hhs, 
                          y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs11$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs11) 
  
  glimpse(train_hhs11) 
  
  set.seed(10110)
  modelo11 <- train(Pobre~., 
                    data = train_hhs11,
                    method = "glm",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy')
  
  probs_train11  <- predict(modelo11, newdata = train_hhs, type = "prob")[, 1, drop = T]
  #probs_train11[probs_train11 < 0] <- 0
  #probs_train11[probs_train11 > 1] <- 1
  
  probs_test11   <- predict(modelo11, newdata = test_hhs, type = "prob")[, 1, drop = T]
  #probs_test11[probs_train11 < 0] <- 0
  #probs_test11[probs_train11 > 1] <- 1
  
  probs_eval11   <- predict(modelo11, newdata = eval_hhs, type = "prob")[, 1, drop = T]
  #probs_eval11[probs_train11 < 0] <- 0
  #probs_eval11[probs_train11 > 1] <- 1
  
  y_hat_train11  <- as.numeric(probs_train11 > 0.5)
  y_hat_test11   <- as.numeric(probs_test11 > 0.5)
  y_hat_eval11   <- as.numeric(probs_eval11 > 0.5)
  
  acc_train11  <- Accuracy(y_pred = y_hat_train11, y_true = as.numeric(train_hhs$Pobre))
  acc_test11   <- Accuracy(y_pred = y_hat_test11, y_true = as.numeric(test_hhs$Pobre))
  acc_eval11   <- Accuracy(y_pred = y_hat_eval11, y_true = as.numeric(eval_hhs$Pobre))
  
  metricas_train11 <- data.frame(Modelo = "Logit - Up", 
                                 "Muestreo" = "Upsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Accuracy" = acc_train11)
  
  metricas_test11 <- data.frame(Modelo = "Logit - Up", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Test",
                                "Accuracy" = acc_test11)
  
  metricas_eval11 <- data.frame(Modelo = "Logit - Up", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Evaluación",
                                "Accuracy" = acc_eval11)
  
  metricas11 <- bind_rows(metricas_train11, metricas_test11, metricas_eval11)
  metricas <- bind_rows(metricas1, metricas11)
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  
  

###1.2 Logit - Downsampling ----




#2 - Logit con Lasso (1)------------------------------------------------------------

  set.seed(1010)
  modelo2 <- train(Pobre ~ . , 
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
  
  confusionMatrix(y_hat_eval2, eval_hhs$Pobre)
  
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
  
  train_hhs$Pobre <- factor(train_hhs$Pobre)
  
  set.seed(10110)
  train_hhs21 <- upSample(x = train_hhs, 
                          y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs21$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs21) 
  
  glimpse(train_hhs21) 
  
  set.seed(10110)
  modelo21 <- train(Pobre ~ . , 
                    data = train_hhs, 
                    method = "glmnet",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy',
                    tuneGrid = expand.grid(alpha = 1,lambda=grilla))
  
  probs_train21  <- predict(modelo21, newdata = train_hhs, type = "prob")[, 1, drop = T]
  #probs_train21[probs_train21 < 0] <- 0
  #probs_train21[probs_train21 > 1] <- 1
  
  probs_test21   <- predict(modelo21, newdata = test_hhs, type = "prob")[, 1, drop = T]
  #probs_test21[probs_train21 < 0] <- 0
  #probs_test21[probs_train21 > 1] <- 1
  
  probs_eval21   <- predict(modelo21, newdata = eval_hhs, type = "prob")[, 1, drop = T]
  #probs_eval21[probs_train21 < 0] <- 0
  #probs_eval21[probs_train21 > 1] <- 1
  
  y_hat_train21  <- as.numeric(probs_train21 > 0.5)
  y_hat_test21   <- as.numeric(probs_test21 > 0.5)
  y_hat_eval21   <- as.numeric(probs_eval21 > 0.5)
  
  acc_train21  <- Accuracy(y_pred = y_hat_train21, y_true = as.numeric(train_hhs$Pobre))
  acc_test21   <- Accuracy(y_pred = y_hat_test21, y_true = as.numeric(test_hhs$Pobre))
  acc_eval21   <- Accuracy(y_pred = y_hat_eval21, y_true = as.numeric(eval_hhs$Pobre))
  
  metricas_train21 <- data.frame(Modelo = "Logit - Up", 
                                 "Muestreo" = "Upsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Accuracy" = acc_train21)
  
  metricas_test21 <- data.frame(Modelo = "Logit - Up", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Test",
                                "Accuracy" = acc_test21)
  
  metricas_eval21 <- data.frame(Modelo = "Logit - Up", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Evaluación",
                                "Accuracy" = acc_eval21)
  
  metricas21 <- bind_rows(metricas_train21, metricas_test21, metricas_eval21)
  metricas1 <- bind_rows(metricas1, metricas11)
  metricas1 %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  
  
  
  
  ###2.3 Logit - Downsampling ----
  
  

#3 - Logit con Ridge (0)------------------------------------------------------------

  set.seed(10110)
  modelo3 <- train(Pobre ~ . , 
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
  
  confusionMatrix(y_hat_eval3, eval_hhs$Pobre)
  
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
  
  
  
  
  train_hhs$Pobre <- factor(train_hhs$Pobre)
  
  set.seed(10110)
  train_hhs11 <- upSample(x = train_hhs, 
                          y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs11$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs11) 
  
  glimpse(train_hhs11) 
  
  set.seed(10110)
  modelo11 <- train(Pobre~., 
                    data = train_hhs11,
                    method = "glm",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy')
  
  probs_train11  <- predict(modelo11, newdata = train_hhs, type = "prob")[, 1, drop = T]
  #probs_train11[probs_train11 < 0] <- 0
  #probs_train11[probs_train11 > 1] <- 1
  
  probs_test11   <- predict(modelo11, newdata = test_hhs, type = "prob")[, 1, drop = T]
  #probs_test11[probs_train11 < 0] <- 0
  #probs_test11[probs_train11 > 1] <- 1
  
  probs_eval11   <- predict(modelo11, newdata = eval_hhs, type = "prob")[, 1, drop = T]
  #probs_eval11[probs_train11 < 0] <- 0
  #probs_eval11[probs_train11 > 1] <- 1
  
  y_hat_train11  <- as.numeric(probs_train11 > 0.5)
  y_hat_test11   <- as.numeric(probs_test11 > 0.5)
  y_hat_eval11   <- as.numeric(probs_eval11 > 0.5)
  
  acc_train11  <- Accuracy(y_pred = y_hat_train11, y_true = as.numeric(train_hhs$Pobre))
  acc_test11   <- Accuracy(y_pred = y_hat_test11, y_true = as.numeric(test_hhs$Pobre))
  acc_eval11   <- Accuracy(y_pred = y_hat_eval11, y_true = as.numeric(eval_hhs$Pobre))
  
  metricas_train11 <- data.frame(Modelo = "Logit - Up", 
                                 "Muestreo" = "Upsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Accuracy" = acc_train11)
  
  metricas_test11 <- data.frame(Modelo = "Logit - Up", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Test",
                                "Accuracy" = acc_test11)
  
  metricas_eval11 <- data.frame(Modelo = "Logit - Up", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Evaluación",
                                "Accuracy" = acc_eval11)
  
  metricas11 <- bind_rows(metricas_train11, metricas_test11, metricas_eval11)
  metricas1 <- bind_rows(metricas1, metricas11)
  metricas1 %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  
  
  
  
  ###3.2 Logit - Downsampling ----
  
  

#4 - Logit con EN-------------------------------------------------------------------

  set.seed(10110)
  modelo4 <- train(Pobre ~ . , 
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
  
  confusionMatrix(y_hat_eval4, eval_hhs$Pobre)
  
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
  
  
  
  
  train_hhs$Pobre <- factor(train_hhs$Pobre)
  
  set.seed(10110)
  train_hhs11 <- upSample(x = train_hhs, 
                          y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs11$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs11) 
  
  glimpse(train_hhs11) 
  
  set.seed(10110)
  modelo11 <- train(Pobre~., 
                    data = train_hhs11,
                    method = "glm",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy')
  
  probs_train11  <- predict(modelo11, newdata = train_hhs, type = "prob")[, 1, drop = T]
  #probs_train11[probs_train11 < 0] <- 0
  #probs_train11[probs_train11 > 1] <- 1
  
  probs_test11   <- predict(modelo11, newdata = test_hhs, type = "prob")[, 1, drop = T]
  #probs_test11[probs_train11 < 0] <- 0
  #probs_test11[probs_train11 > 1] <- 1
  
  probs_eval11   <- predict(modelo11, newdata = eval_hhs, type = "prob")[, 1, drop = T]
  #probs_eval11[probs_train11 < 0] <- 0
  #probs_eval11[probs_train11 > 1] <- 1
  
  y_hat_train11  <- as.numeric(probs_train11 > 0.5)
  y_hat_test11   <- as.numeric(probs_test11 > 0.5)
  y_hat_eval11   <- as.numeric(probs_eval11 > 0.5)
  
  acc_train11  <- Accuracy(y_pred = y_hat_train11, y_true = as.numeric(train_hhs$Pobre))
  acc_test11   <- Accuracy(y_pred = y_hat_test11, y_true = as.numeric(test_hhs$Pobre))
  acc_eval11   <- Accuracy(y_pred = y_hat_eval11, y_true = as.numeric(eval_hhs$Pobre))
  
  metricas_train11 <- data.frame(Modelo = "Logit - Up", 
                                 "Muestreo" = "Upsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Accuracy" = acc_train11)
  
  metricas_test11 <- data.frame(Modelo = "Logit - Up", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Test",
                                "Accuracy" = acc_test11)
  
  metricas_eval11 <- data.frame(Modelo = "Logit - Up", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Evaluación",
                                "Accuracy" = acc_eval11)
  
  metricas11 <- bind_rows(metricas_train11, metricas_test11, metricas_eval11)
  metricas1 <- bind_rows(metricas1, metricas11)
  metricas1 %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  
  
  
  
  ###4.2 Logit - Downsampling ----
  
  
  
  
  
  
## Up sampling-------------------------------------------------------------------

#upSampledTrain_h <- upSample(y = as.factor(train_hhs$Pobre),
                             #x = select(train_hhs, -id, -Pobre),
                             #yname = "Pobre")
#dim(train_hhs)

#dim(upSampledTrain_h)

#table(upSampledTrain_h$Pobre)


#lambda_grid_h <- 10^seq(-4, 0.01, length = 300) #en la practica se suele usar una grilla de 200 o 300


# modelo2 <- train(y = as.factor(upSampledTrain_h$Pobre),x = select(upSampledTrain_h),method = "glmnet",
# trControl = ctrl,
# family = "binomial", 
# metric = 'Accuracy',
# tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid_h))

#modelo2
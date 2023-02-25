
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


#1 - Logit ---------------------------------------------------------

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
                                "Muestreo" = "---", 
                                "Evaluación" = "Entrenamiento",
                                "Accuracy" = acc_train1)
  
  metricas_test1 <- data.frame(Modelo = "Logit", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Test",
                               "Accuracy" = acc_test1)
  
  metricas_eval1 <- data.frame(Modelo = "Logit", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Evaluación",
                               "Accuracy" = acc_eval1)
  
  
  metricas1 <- bind_rows(metricas_train1, metricas_test1, metricas_eval1)
  metricas1 %>% kbl(digits = 2) %>% kable_styling(full_width = T)

###1.1 Logit - Upsampling ----
  
  set.seed(10110)
  train_hhs11 <- upSample(x = select(train_hhs, -Pobre),
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
  
  metricas_train11 <- data.frame(Modelo = "Logit", 
                                 "Muestreo" = "Upsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Accuracy" = acc_train11)
  
  metricas_test11 <- data.frame(Modelo = "Logit", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Test",
                                "Accuracy" = acc_test11)
  
  metricas_eval11 <- data.frame(Modelo = "Logit", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Evaluación",
                                "Accuracy" = acc_eval11)
  
  metricas11 <- bind_rows(metricas_train11, metricas_test11, metricas_eval11)
  metricas <- bind_rows(metricas1, metricas11)
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  
  
###1.2 Logit - Downsampling ----

  set.seed(10110)
  train_hhs12 <- downSample(x = select(train_hhs, -Pobre), 
                            y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs12$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs12) 
  
  modelo12 <- train(Pobre~., 
                    data = train_hhs12,
                    method = "glmnet",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy')
  
  y_hat_train12  <- predict(modelo12, train_hhs)
  y_hat_test12   <- predict(modelo12, test_hhs)
  y_hat_eval12   <- predict(modelo12, eval_hhs)
  
  acc_train12  <- Accuracy(y_pred = y_hat_train12, y_true = train_hhs$Pobre)
  acc_test12   <- Accuracy(y_pred = y_hat_test12, y_true = test_hhs$Pobre)
  acc_eval12   <- Accuracy(y_pred = y_hat_eval12, y_true = eval_hhs$Pobre)
  
  metricas_train12 <- data.frame(Modelo = "Logit", 
                                 "Muestreo" = "Downsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Accuracy" = acc_train12)
  
  metricas_test12 <- data.frame(Modelo = "Logit", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Test",
                                "Accuracy" = acc_test12)
  
  metricas_eval12 <- data.frame(Modelo = "Logit", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Evaluación",
                                "Accuracy" = acc_eval12)
  
  metricas12 <- bind_rows(metricas_train12, metricas_test12, metricas_eval12)
  metricas <- bind_rows(metricas, metricas12)
  
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  

#2 - Logit - Lasso (1)------------------------------------------------------------

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
                                "Muestreo" = "---", 
                                "Evaluación" = "Entrenamiento",
                                "Accuracy" = acc_train2)
  
  metricas_test2 <- data.frame(Modelo = "Logit - Lasso", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Test",
                               "Accuracy" = acc_test2)
  
  metricas_eval2 <- data.frame(Modelo = "Logit - Lasso", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Evaluación",
                               "Accuracy" = acc_eval2)
  
  
  metricas2 <- bind_rows(metricas_train2, metricas_test2, metricas_eval2)
  metricas <- bind_rows(metricas, metricas2)
  
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)

  
  ###2.1 Logit - Lasso - Upsampling ----
  
  set.seed(10110)
  train_hhs21 <- upSample(x = select(train_hhs, -Pobre), 
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
  #probs_test21[probs_test21 < 0] <- 0
  #probs_test21[probs_test21 > 1] <- 1
  
  probs_eval21   <- predict(modelo21, newdata = eval_hhs, type = "prob")[, 1, drop = T]
  #probs_eval21[probs_eval21 < 0] <- 0
  #probs_eval21[probs_eval21 > 1] <- 1
  
  y_hat_train21  <- as.numeric(probs_train21 > 0.5)
  y_hat_test21   <- as.numeric(probs_test21 > 0.5)
  y_hat_eval21   <- as.numeric(probs_eval21 > 0.5)
  
  acc_train21  <- Accuracy(y_pred = y_hat_train21, y_true = as.numeric(train_hhs$Pobre))
  acc_test21   <- Accuracy(y_pred = y_hat_test21, y_true = as.numeric(test_hhs$Pobre))
  acc_eval21   <- Accuracy(y_pred = y_hat_eval21, y_true = as.numeric(eval_hhs$Pobre))
  
  metricas_train21 <- data.frame(Modelo = "Logit - Lasso", 
                                 "Muestreo" = "Upsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Accuracy" = acc_train21)
  
  metricas_test21 <- data.frame(Modelo = "Logit - Lasso", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Test",
                                "Accuracy" = acc_test21)
  
  metricas_eval21 <- data.frame(Modelo = "Logit - Lasso", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Evaluación",
                                "Accuracy" = acc_eval21)
  
  metricas21 <- bind_rows(metricas_train21, metricas_test21, metricas_eval21)
  metricas <- bind_rows(metricas, metricas21)
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  
  
  ###2.2 Logit - Lasso - Downsampling ----
  
  set.seed(10110)
  train_hhs22 <- downSample(x = select(train_hhs, -Pobre), 
                            y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs22$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs22) 
  
  #train_hhs22 <- data.frame(sapply(train_hhs22, as.numeric))
  
  modelo22 <- train(Pobre~., 
                    data = train_hhs22,
                    method = "glmnet",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy')
  
  y_hat_train22  <- predict(modelo22, train_hhs)
  y_hat_test22   <- predict(modelo22, test_hhs)
  y_hat_eval22   <- predict(modelo22, eval_hhs)
  
  acc_train22  <- Accuracy(y_pred = y_hat_train22, y_true = train_hhs$Pobre)
  acc_test22   <- Accuracy(y_pred = y_hat_test22, y_true = test_hhs$Pobre)
  acc_eval22   <- Accuracy(y_pred = y_hat_eval22, y_true = eval_hhs$Pobre)
  
  metricas_train22 <- data.frame(Modelo = "Logit - Lasso", 
                                 "Muestreo" = "Downsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Accuracy" = acc_train22)
  
  metricas_test22 <- data.frame(Modelo = "Logit - Lasso", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Test",
                                "Accuracy" = acc_test22)
  
  metricas_eval22 <- data.frame(Modelo = "Logit - Lasso", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Evaluación",
                                "Accuracy" = acc_eval22)
  
  metricas22 <- bind_rows(metricas_train22, metricas_test22, metricas_eval22)
  metricas <- bind_rows(metricas, metricas22)
  
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  

#3 - Logit - Ridge (0)------------------------------------------------------------

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
                                "Muestreo" = "---", 
                                "Evaluación" = "Entrenamiento",
                                "Accuracy" = acc_train3)
  
  metricas_test3 <- data.frame(Modelo = "Logit - Ridge", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Test",
                               "Accuracy" = acc_test3)
  
  metricas_eval3 <- data.frame(Modelo = "Logit - Ridge", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Evaluación",
                               "Accuracy" = acc_eval3)
  
  
  metricas3 <- bind_rows(metricas_train3, metricas_test3, metricas_eval3)
  metricas <- bind_rows(metricas, metricas3)
  
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)

  
  ###3.1 Logit - Ridge - Upsampling ----
  
  set.seed(10110)
  train_hhs31 <- upSample(x = select(train_hhs, -Pobre), 
                          y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs31$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs31) 
  
  glimpse(train_hhs31) 
  
  set.seed(10110)
  modelo31 <- train(Pobre ~ . , 
                    data = train_hhs31,
                    method = "glmnet",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy',
                    tuneGrid = expand.grid(alpha = 0,lambda=grilla))
  
  probs_train31  <- predict(modelo31, newdata = train_hhs, type = "prob")[, 1, drop = T]
  #probs_train31[probs_train31 < 0] <- 0
  #probs_train31[probs_train31 > 1] <- 1
  
  probs_test31   <- predict(modelo31, newdata = test_hhs, type = "prob")[, 1, drop = T]
  #probs_test31[probs_test31 < 0] <- 0
  #probs_test31[probs_test31 > 1] <- 1
  
  probs_eval31   <- predict(modelo31, newdata = eval_hhs, type = "prob")[, 1, drop = T]
  #probs_eval31[probs_eval31 < 0] <- 0
  #probs_eval31[probs_eval31 > 1] <- 1
  
  y_hat_train31  <- as.numeric(probs_train31 > 0.5)
  y_hat_test31   <- as.numeric(probs_test31 > 0.5)
  y_hat_eval31   <- as.numeric(probs_eval31 > 0.5)
  
  acc_train31  <- Accuracy(y_pred = y_hat_train31, y_true = as.numeric(train_hhs$Pobre))
  acc_test31   <- Accuracy(y_pred = y_hat_test31, y_true = as.numeric(test_hhs$Pobre))
  acc_eval31   <- Accuracy(y_pred = y_hat_eval31, y_true = as.numeric(eval_hhs$Pobre))
  
  metricas_train31 <- data.frame(Modelo = "Logit - Ridge", 
                                 "Muestreo" = "Upsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Accuracy" = acc_train31)
  
  metricas_test31 <- data.frame(Modelo = "Logit - Ridge", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Test",
                                "Accuracy" = acc_test31)
  
  metricas_eval31 <- data.frame(Modelo = "Logit - Ridge", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Evaluación",
                                "Accuracy" = acc_eval31)
  
  metricas31 <- bind_rows(metricas_train31, metricas_test31, metricas_eval31)
  metricas <- bind_rows(metricas, metricas31)
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  
  
  ###3.2 Logit - Ridge - Downsampling ----
  
  set.seed(10110)
  train_hhs32 <- downSample(x = select(train_hhs, -Pobre), 
                            y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs32$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs32) 
  
  #train_hhs32 <- data.frame(sapply(train_hhs32, as.numeric))
  
  modelo32 <- train(Pobre~., 
                    data = train_hhs32,
                    method = "glmnet",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy')
  
  y_hat_train32  <- predict(modelo32, train_hhs)
  y_hat_test32   <- predict(modelo32, test_hhs)
  y_hat_eval32   <- predict(modelo32, eval_hhs)
  
  acc_train32  <- Accuracy(y_pred = y_hat_train32, y_true = train_hhs$Pobre)
  acc_test32   <- Accuracy(y_pred = y_hat_test32, y_true = test_hhs$Pobre)
  acc_eval32   <- Accuracy(y_pred = y_hat_eval32, y_true = eval_hhs$Pobre)
  
  metricas_train32 <- data.frame(Modelo = "Logit - Ridge", 
                                 "Muestreo" = "Downsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Accuracy" = acc_train32)
  
  metricas_test32 <- data.frame(Modelo = "Logit - Ridge", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Test",
                                "Accuracy" = acc_test32)
  
  metricas_eval32 <- data.frame(Modelo = "Logit - Ridge", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Evaluación",
                                "Accuracy" = acc_eval32)
  
  metricas32 <- bind_rows(metricas_train32, metricas_test32, metricas_eval32)
  metricas <- bind_rows(metricas, metricas32)
  
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)


  #4 - Logit - EN-------------------------------------------------------------------

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
                                "Muestreo" = "---", 
                                "Evaluación" = "Entrenamiento",
                                "Accuracy" = acc_train4)
  
  metricas_test4 <- data.frame(Modelo = "Logit - EN", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Test",
                               "Accuracy" = acc_test4)
  
  metricas_eval4 <- data.frame(Modelo = "Logit - EN", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Evaluación",
                               "Accuracy" = acc_eval4)
  
  metricas4 <- bind_rows(metricas_train4, metricas_test4, metricas_eval4)
  metricas <- bind_rows(metricas, metricas4)
  
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)

  
  ###4.1 Logit - EN - Upsampling ----
  
  set.seed(10110)
  train_hhs41 <- upSample(x = select(train_hhs, -Pobre), 
                          y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs41$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs41) 
  
  glimpse(train_hhs41) 
  
  set.seed(10110)
  modelo41 <- train(Pobre ~ . , 
                    data = train_hhs41,
                    method = "glmnet",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy',
                    tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda=grilla))
  
  probs_train41  <- predict(modelo41, newdata = train_hhs, type = "prob")[, 1, drop = T]
  #probs_train41[probs_train41 < 0] <- 0
  #probs_train41[probs_train41 > 1] <- 1
  
  probs_test41   <- predict(modelo41, newdata = test_hhs, type = "prob")[, 1, drop = T]
  #probs_test41[probs_test41 < 0] <- 0
  #probs_test41[probs_test41 > 1] <- 1
  
  probs_eval41   <- predict(modelo41, newdata = eval_hhs, type = "prob")[, 1, drop = T]
  #probs_eval41[probs_eval41 < 0] <- 0
  #probs_eval41[probs_eval41 > 1] <- 1
  
  y_hat_train41  <- as.numeric(probs_train41 > 0.5)
  y_hat_test41   <- as.numeric(probs_test41 > 0.5)
  y_hat_eval41   <- as.numeric(probs_eval41 > 0.5)
  
  acc_train41  <- Accuracy(y_pred = y_hat_train41, y_true = as.numeric(train_hhs$Pobre))
  acc_test41   <- Accuracy(y_pred = y_hat_test41, y_true = as.numeric(test_hhs$Pobre))
  acc_eval41   <- Accuracy(y_pred = y_hat_eval41, y_true = as.numeric(eval_hhs$Pobre))
  
  metricas_train41 <- data.frame(Modelo = "Logit - EN", 
                                 "Muestreo" = "Upsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Accuracy" = acc_train41)
  
  metricas_test41 <- data.frame(Modelo = "Logit - EN", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Test",
                                "Accuracy" = acc_test41)
  
  metricas_eval41 <- data.frame(Modelo = "Logit - EN", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Evaluación",
                                "Accuracy" = acc_eval41)
  
  metricas41 <- bind_rows(metricas_train41, metricas_test41, metricas_eval41)
  metricas <- bind_rows(metricas, metricas41)
  
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  
  
  ###4.2 Logit - EN - Downsampling ----
  
  set.seed(10110)
  train_hhs42 <- downSample(x = select(train_hhs, -Pobre), 
                            y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs42$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs42) 
  
  #train_hhs42 <- data.frame(sapply(train_hhs42, as.numeric))
  
  modelo42 <- train(Pobre~., 
                    data = train_hhs42,
                    method = "glmnet",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy',
                    tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda=grilla))
  
  y_hat_train42  <- predict(modelo42, train_hhs)
  y_hat_test42   <- predict(modelo42, test_hhs)
  y_hat_eval42   <- predict(modelo42, eval_hhs)
  
  acc_train42  <- Accuracy(y_pred = y_hat_train42, y_true = train_hhs$Pobre)
  acc_test42   <- Accuracy(y_pred = y_hat_test42, y_true = test_hhs$Pobre)
  acc_eval42   <- Accuracy(y_pred = y_hat_eval42, y_true = eval_hhs$Pobre)
  
  metricas_train42 <- data.frame(Modelo = "Logit - EN", 
                                 "Muestreo" = "Downsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Accuracy" = acc_train42)
  
  metricas_test42 <- data.frame(Modelo = "Logit - EN", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Test",
                                "Accuracy" = acc_test42)
  
  metricas_eval42 <- data.frame(Modelo = "Logit - EN", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Evaluación",
                                "Accuracy" = acc_eval42)
  
  metricas42 <- bind_rows(metricas_train42, metricas_test42, metricas_eval42)
  metricas <- bind_rows(metricas, metricas42)
  
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  
  #5 - LDA ---------------------------------------------------------------------
  
  set.seed(10110)
  modelo5 <- train(Pobre ~ . , 
                   data = train_hhs,
                   method = "lda",
                   trControl = control,
                   family = "binomial",
                   preProcess = NULL,
                   metric = 'Accuracy')
  
  modelo5
  
  y_hat_train5 <- predict(modelo5, train_hhs)
  y_hat_test5  <- predict(modelo5, test_hhs)
  y_hat_eval5  <- predict(modelo5, eval_hhs)
  
  confusionMatrix(y_hat_eval5, eval_hhs$Pobre)
  
  acc_train5  <- Accuracy(y_pred = y_hat_train5, y_true = train_hhs$Pobre)
  acc_test5   <- Accuracy(y_pred = y_hat_test5, y_true = test_hhs$Pobre)
  acc_eval5   <- Accuracy(y_pred = y_hat_eval5, y_true = eval_hhs$Pobre)
  
  
  metricas_train5 <- data.frame(Modelo = "LDA", 
                                "Muestreo" = "---", 
                                "Evaluación" = "Entrenamiento",
                                "Accuracy" = acc_train5)
  
  metricas_test5 <- data.frame(Modelo = "LDA", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Test",
                               "Accuracy" = acc_test5)
  
  metricas_eval5 <- data.frame(Modelo = "LDA", 
                               "Muestreo" = "---", 
                               "Evaluación" = "Evaluación",
                               "Accuracy" = acc_eval5)
  
  metricas5 <- bind_rows(metricas_train5, metricas_test5, metricas_eval5)
  metricas <- bind_rows(metricas, metricas5)
  
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  
  
  ###5.1 LDA - Upsampling ----
  
  set.seed(10110)
  train_hhs51 <- upSample(x = select(train_hhs, -Pobre), 
                          y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs51$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs51) 
  
  glimpse(train_hhs51) 
  
  set.seed(10110)
  modelo51 <- train(Pobre ~ . , 
                    data = train_hhs51,
                    method = "lda",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy')
  
  probs_train51  <- predict(modelo51, newdata = train_hhs, type = "prob")[, 1, drop = T]
  #probs_train51[probs_train51 < 0] <- 0
  #probs_train51[probs_train51 > 1] <- 1
  
  probs_test51   <- predict(modelo51, newdata = test_hhs, type = "prob")[, 1, drop = T]
  #probs_test51[probs_test51 < 0] <- 0
  #probs_test51[probs_test51 > 1] <- 1
  
  probs_eval51   <- predict(modelo51, newdata = eval_hhs, type = "prob")[, 1, drop = T]
  #probs_eval51[probs_eval51 < 0] <- 0
  #probs_eval51[probs_eval51 > 1] <- 1
  
  y_hat_train51  <- as.numeric(probs_train51 > 0.5)
  y_hat_test51   <- as.numeric(probs_test51 > 0.5)
  y_hat_eval51   <- as.numeric(probs_eval51 > 0.5)
  
  acc_train51  <- Accuracy(y_pred = y_hat_train51, y_true = as.numeric(train_hhs$Pobre))
  acc_test51   <- Accuracy(y_pred = y_hat_test51, y_true = as.numeric(test_hhs$Pobre))
  acc_eval51   <- Accuracy(y_pred = y_hat_eval51, y_true = as.numeric(eval_hhs$Pobre))
  
  metricas_train51 <- data.frame(Modelo = "LDA", 
                                 "Muestreo" = "Upsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Accuracy" = acc_train51)
  
  metricas_test51 <- data.frame(Modelo = "LDA", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Test",
                                "Accuracy" = acc_test51)
  
  metricas_eval51 <- data.frame(Modelo = "LDA", 
                                "Muestreo" = "Upsampling", 
                                "Evaluación" = "Evaluación",
                                "Accuracy" = acc_eval51)
  
  metricas51 <- bind_rows(metricas_train51, metricas_test51, metricas_eval51)
  metricas <- bind_rows(metricas, metricas51)
  
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  
  
  ###5.2 LDA - Downsampling ----
  
  
  set.seed(10110)
  train_hhs52 <- downSample(x = select(train_hhs, -Pobre), 
                            y = train_hhs$Pobre, yname = "Pobre")
  
  prop.table(table(train_hhs$Pobre)) #BD inicial
  nrow(train_hhs) 
  
  prop.table(table(train_hhs52$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
  nrow(train_hhs52) 
  
  #train_hhs52 <- data.frame(sapply(train_hhs52, as.numeric))
  
  modelo52 <- train(Pobre~., 
                    data = train_hhs52,
                    method = "glmnet",
                    trControl = control,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy')
  
  y_hat_train52  <- predict(modelo52, train_hhs)
  y_hat_test52   <- predict(modelo52, test_hhs)
  y_hat_eval52   <- predict(modelo52, eval_hhs)
  
  acc_train52  <- Accuracy(y_pred = y_hat_train52, y_true = train_hhs$Pobre)
  acc_test52   <- Accuracy(y_pred = y_hat_test52, y_true = test_hhs$Pobre)
  acc_eval52   <- Accuracy(y_pred = y_hat_eval52, y_true = eval_hhs$Pobre)
  
  metricas_train52 <- data.frame(Modelo = "LDA", 
                                 "Muestreo" = "Downsampling", 
                                 "Evaluación" = "Entrenamiento",
                                 "Accuracy" = acc_train52)
  
  metricas_test52 <- data.frame(Modelo = "LDA", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Test",
                                "Accuracy" = acc_test52)
  
  metricas_eval52 <- data.frame(Modelo = "LDA", 
                                "Muestreo" = "Downsampling", 
                                "Evaluación" = "Evaluación",
                                "Accuracy" = acc_eval52)
  
  metricas52 <- bind_rows(metricas_train52, metricas_test52, metricas_eval52)
  metricas <- bind_rows(metricas, metricas52)
  
  metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)
  
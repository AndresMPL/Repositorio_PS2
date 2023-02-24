
#------------------------------------------------------------------------------#
#
#                         2. POVERTY CLASSIFICATION
#
#------------------------------------------------------------------------------#

#Enfoque de clasificación - Intento de predecir directamente 0 (no pobre) y 1 (pobre)

#Pasos previos ----------------------------------------------------------------

  diccionario = c("No Pobre", "Pobre")
  
  train_h <- train_h %>%    mutate(Pobre = factor(train_h$Pobre, 
                            levels = c(0, 1),
                            labels = diccionario)) #Pobre=1, No Pobre=0
  
  
#Evaluación desbalance
  
  prop.table(table(train_h$Pobre)) #Grado de desbalance Moderado
  
  Imagen_1 <- ggplot(train_h, aes(y = Pobre)) +
              geom_bar(fill = "#53868B") +
              theme_bw() +
              labs(title = "Distribución de hogares Clasificación de Pobreza",
                   y = "",
                   x = "Número de hogares")
  
  Imagen_1

#Dividimos train/test/eval (70/20/10) - BD Hogares

  set.seed(10110)
  index_1 <- createDataPartition(train_h$Pobre, p = 0.7)[[1]]
  train_hh  <- train_h[index_1,]
  other     <- train_h[-index_1,]

  set.seed(10110)
  index2  <- createDataPartition(other$Pobre, p = 1/3)[[1]]
  test_hh <- other[-index2,]
  eval_hh <- other[ index2,]

  dim(train_h)   
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
    
  variables_numericas <- c("P5000", "P5010", "P5130", "Nper", 
                           "Npersug", "Li", "Lp", "Fex_c", "Fex_dpto")
  
  as.vector(colnames(train_hhs))
  as.vector(train_hhs[1])
  
  escalador <- preProcess(train_hh[, variables_numericas],
                          method = c("center", "scale"))
  
  train_hhs[, variables_numericas] <- predict(escalador, train_hh[, variables_numericas])
  test_hhs[, variables_numericas] <- predict(escalador, test_hh[, variables_numericas])
  eval_hhs[, variables_numericas] <- predict(escalador, eval_hh[, variables_numericas])  

#Control------------------------------------------------------------------------
  
  Y <- train_hhs$Pobre
  X <- model.matrix(~, train_hhs)
  
  lgrid <- 10^seq(-5, 0.01, length = 100)
    
  fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
  
  control <- trainControl(method = "cv",
                          number = 10,
                          summaryFunction = fiveStats,
                          classProbs = TRUE,
                          verbose=FALSE,
                          savePredictions = T)
  
  
#1 - Logit sin regularizar ---------------------------------------------------------
  
  modelo1 <- train(y = Y,
                   x = X,
                   data = train_hhs, 
                   method = "glm",
                   trControl = ctrl,
                   family = "binomial", 
                   metric = 'Accuracy')
  
  modelo1
  
  
  y_hat_train1 <- predict(modelo1, train_hhs)
  y_hat_test1  <- predict(modelo1, test_hhs)
  y_hat_eval1  <- predict(modelo1, eval_hhs)
  
  probs_train1 <- predict(modelo1, train_hhs, type = "prob")[, "1", drop = T]
  probs_test1  <- predict(modelo1, test_hhs, type = "prob")[, "1", drop = T]
  probs_eval1  <- predict(modelo1, eval_hhs, type = "prob")[, "1", drop = T]
  
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
  
  
  
#2 - Logit con Lasso (1)------------------------------------------------------------
  
  modelo2 <- train(y = y,
                   x = X,
                   data = train_hhs, 
                   method = "glm",
                   trControl = ctrl,
                   family = "binomial", 
                   metric = 'Accuracy',
                   tuneGrid = expand.grid(alpha = 1,lambda=lgrid),
                   preProcess = c("center","scale"))
  
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
  
  
#3 - Logit con Ridge (0)------------------------------------------------------------
  
  modelo3 <- train(y = Y,
                   x = X,
                   data = train_hhs, 
                   method = "glm",
                   trControl = ctrl,
                   family = "binomial", 
                   metric = 'Accuracy',
                   tuneGrid = expand.grid(alpha = 0,lambda=lgrid),
                   preProcess = c("center","scale"))
  
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
  
  
#4 - Logit con EN-------------------------------------------------------------------
  
  modelo4 <- train(y = Y,
                   x = X,
                   data = train_hhs, 
                   method = "glm",
                   trControl = ctrl,
                   family = "binomial", 
                   metric = 'Accuracy',
                   tuneGrid = expand.grid(alpha = seq(from = 0,to = 1,by = 0.1),lambda=lgrid),
                   preProcess = c("center","scale"))

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
  
  

##1.1 Logit sin regularizar - Upsampling ----

      train_hhs$Pobre <- factor(train_hhs$Pobre)
      
      set.seed(10110)
      train_hhs11 <- upSample(x = train_hhs, 
                              y = train_hhs$Pobre, yname = "Pobre")
      
      prop.table(table(train_hhs$Pobre)) #BD inicial
      nrow(train_hhs) 
      
      prop.table(table(train_hhs11$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
      nrow(train_hhs11) 
      
      glimpse(train_hhs11) 
      
      #train_hhs11$Pobre <- as.numeric(train_hhs$Pobre)
      #train_hhs11$Pobre <- as.numeric(train_hhs$Pobre)
      #train_hhs11$Pobre <- as.numeric(train_hhs$Pobre)
      
      set.seed(10110)
      modelo11 <- train(Pobre~., 
                        data = train_hhs11,
                        method = "glmnet",
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
      
      

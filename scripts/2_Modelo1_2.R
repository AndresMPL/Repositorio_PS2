
##1.2 Logit sin regularizar - ROSE Oversampling ----

filtro <- sapply(train_hhs, function(variable) length(unique(variable)) == 2)

var_categoricas <- names(train_hhs)[filtro]

train_hhs12 <- train_hhs %>% mutate(across(.cols = var_categoricas, .fns = factor))
test_hhs12 <- test_hhs %>% mutate(across(.cols = var_categoricas, .fns = factor))
eval_hhs12 <- eval_hhs %>% mutate(across(.cols = var_categoricas, .fns = factor))

table(train_hhs12$Pobre)

rose_train <- ROSE(Pobre ~ ., data = train_hhs12, N = nrow(train_hhs12) + 69239, p = 0.5)$data 

prop.table(table(train_hhs$Pobre))
nrow(train_hhs)

prop.table(table(rose_train$Pobre)) #Verificamos proporciones de cada clase
nrow(rose_train)

rose_train <- data.frame(sapply(rose_train, as.numeric))

   modelo12 <- train(Pobre~., 
                     data = rose_train,
                     method = "glmnet",
                     family = "binomial",
                     preProcess = NULL)

   backup_train_hhs <- train_hhs 
   backup_test_hhs <- test_hhs
   backup_eval_hhs <- eval_hhs
   
   train_hhs <- data.frame(sapply(train_hhs, as.numeric))
   test_hhs  <- data.frame(sapply(test_hhs, as.numeric))
   eval_hhs  <- data.frame(sapply(eval_hhs, as.numeric))

y_hat_train_rose <- predict(modelo12, newdata = train_hhs)
y_hat_test_rose  <- predict(modelo12, newdata = test_hhs)
y_hat_eval_rose  <- predict(modelo12, newdata = eval_hhs)

acc_train_rose <- Accuracy(y_pred = y_hat_train_rose, y_true = train_hhs$Pobre)
acc_test_rose  <- Accuracy(y_pred = y_hat_test_rose, y_true = test_hhs$Pobre)
acc_eval_rose  <- Accuracy(y_pred = y_hat_eval_rose, y_true = eval_hhs$Pobre)

metricas_train12 <- data.frame(Modelo = "Logit - ROSE", 
                               "Muestreo" = "Oversampling", 
                               "Evaluación" = "Entrenamiento",
                               "Accuracy" = acc_train_rose)

metricas_test12 <- data.frame(Modelo = "Logit - ROSE", 
                              "Muestreo" = "Oversampling", 
                              "Evaluación" = "Test",
                              "Accuracy" = acc_test_rose)

metricas_eval12 <- data.frame(Modelo = "Logit - ROSE", 
                              "Muestreo" = "Oversampling", 
                              "Evaluación" = "Evaluación",
                              "Accuracy" = acc_eval_rose)

metricas12 <- bind_rows(metricas_train12, metricas_test12, metricas_eval12)
metricas <- bind_rows(metricas, metricas12)
metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)

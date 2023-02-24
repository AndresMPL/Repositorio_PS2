
##1.3 Logit sin regularizar - Downsampling ----


train_hhs$Pobre <- factor(train_hhs$Pobre)

train_hhs13 <- downSample(x = select(train_hhs, -Pobre), 
                          y = train_hhs$Pobre, list = F, yname = "Pobre")

prop.table(table(train_hhs$Pobre)) #BD inicial
nrow(train_hhs) 

prop.table(table(train_hhs13$Pobre)) #BD remuestreo - Verificamos proporciones de cada clase
nrow(train_hhs13) 

train_hhs13 <- data.frame(sapply(train_hhs13, as.numeric))

modelo13 <- train(Pobre~., 
                  data = train_hhs13,
                  method = "glmnet",
                  family = "binomial",
                  preProcess = NULL,
                  metric = 'Accuracy')

y_hat_train13  <- predict(modelo13, train_hhs)
y_hat_test13   <- predict(modelo13, test_hhs)
y_hat_eval13   <- predict(modelo13, eval_hhs)

acc_train13  <- Accuracy(y_pred = y_hat_train13, y_true = train_hhs$Pobre)
acc_test13   <- Accuracy(y_pred = y_hat_test13, y_true = test_hhs$Pobre)
acc_eval13   <- Accuracy(y_pred = y_hat_eval13, y_true = eval_hhs$Pobre)

metricas_train13 <- data.frame(Modelo = "Logit - Down", 
                               "Muestreo" = "Downsampling", 
                               "Evaluación" = "Entrenamiento",
                               "Accuracy" = acc_train13)

metricas_test13 <- data.frame(Modelo = "Logit - Down", 
                              "Muestreo" = "Downsampling", 
                              "Evaluación" = "Test",
                              "Accuracy" = acc_test13)

metricas_eval13 <- data.frame(Modelo = "Logit - Down", 
                              "Muestreo" = "Downsampling", 
                              "Evaluación" = "Evaluación",
                              "Accuracy" = acc_eval13)

metricas13 <- bind_rows(metricas_train13, metricas_test13, metricas_eval13)
metricas <- bind_rows(metricas, metricas13)
metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)




##1.5 Logit sin regularizar - Cambiar función de costo ----

pesos <- as.numeric(train_hhs$Pobre)
pesos[train_hhs$Pobre == 1] <- 3
pesos[train_hhs$Pobre == 0] <- 1

X15 <- select(train_hhs, -Pobre)
Y15 <- as.factor(train_hhs$Pobre - 1)

modelo_pesos <- train(x = X15,
                      y = Y15,
                      method = "glmnet",
                      weights = pesos)

y_hat_train15  <- predict(modelo_pesos, train_hhs)
y_hat_test15   <- predict(modelo_pesos, test_hhs)
y_hat_eval155  <- predict(modelo_pesos, eval_hhs)

acc_train15 <- Accuracy(y_pred = y_hat_train15, y_true = train_hhs$Pobre)
acc_test15 <- Accuracy(y_pred = y_hat_test15, y_true = train_hhs$Pobre)
acc_eval15 <- Accuracy(y_pred = y_hat_eval155, y_true = train_hhs$Pobre)


metricas_train15 <- data.frame(Modelo = "Logit", 
                               "Muestreo" = "Cambiar función de costo", 
                               "Evaluación" = "Entrenamiento",
                               "Accuracy" = acc_train15)

metricas_test15 <- data.frame(Modelo = "Logit", 
                              "Muestreo" = "Cambiar función de costo", 
                              "Evaluación" = "Test",
                              "Accuracy" = acc_test15)

metricas_eval15 <- data.frame(Modelo = "Logit", 
                              "Muestreo" = "Cambiar función de costo", 
                              "Evaluación" = "Evaluación",
                              "Accuracy" = acc_eval15)


metricas15 <- bind_rows(metricas_train15, metricas_test15, metricas_eval15)
metricas <- bind_rows(metricas, metricas15)
metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)

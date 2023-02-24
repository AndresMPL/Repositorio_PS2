
##1.4 Logit sin regularizar - Threshold óptimo ----

thresholds <- seq(0, 1, length.out = 100)

opt_t <- data.frame()

for (t in thresholds) {
                      y_pred_t <- as.numeric(probs_train1 > t)
                      f1_t <- F1_Score(y_true = train_hhs$Pobre, 
                      y_pred = y_pred_t,
                      positive = 1)
                      fila <- data.frame(t = t, F1 = f1_t)
                      opt_t <- bind_rows(opt_t, fila)
                      }

mejor_t <-  opt_t$t[which(opt_t$F1 == max(opt_t$F1, na.rm = T))]


ggplot(opt_t, aes(x = t, y = F1)) +
        geom_point(size = 0.7) +
        geom_line() +
        theme_bw() +
        geom_vline(xintercept = mejor_t, linetype = "dashed", 
                   color = "green") +
        labs(x = "Threshold")

# Convertimos la probabilidad en una predicción
y_hat_train14 <- as.numeric(probs_train1 > mejor_t)
y_hat_test14  <- as.numeric(probs_test1 > mejor_t)
y_hat_eval14  <- as.numeric(probs_eval1 > mejor_t)

acc_train14 <- Accuracy(y_pred = y_hat_train14, y_true = train_hhs$infielTRUE)
acc_test14 <- Accuracy(y_pred = y_hat_test14, y_true = test_hhs$infielTRUE)
acc_eval14 <- Accuracy(y_pred = y_hat_eval14, y_true = eval_hhs$infielTRUE)


metricas_train14 <- data.frame(Modelo = "Logit - Threshold óptimo", 
                               "Muestreo" = NA, 
                               "Evaluación" = "Entrenamiento",
                               "Accuracy" = acc_train14)

metricas_test14 <- data.frame(Modelo = "Logit - Threshold óptimo", 
                              "Muestreo" = NA, 
                              "Evaluación" = "Test",
                              "Accuracy" = acc_test14)

metricas_eval14 <- data.frame(Modelo = "Logit - Threshold óptimo", 
                              "Muestreo" = NA, 
                              "Evaluación" = "Evaluación",
                              "Accuracy" = acc_eval14)


metricas14 <- bind_rows(metricas_train14, metricas_test14, metricas_eval14)
metricas <- bind_rows(metricas, metricas14)
metricas %>% kbl(digits = 2) %>% kable_styling(full_width = T)



#------------------------------------------------------------------------------#
#
#                       3. REGRESSION INCOMES
#
#------------------------------------------------------------------------------#

#Predicción de ingresos - Intento de predecir primero los ingresos

         #PROPUESTA ESTRUCTURA PRELIMINAR
  
#Regresión lineal---------------------------------------------------------------



#Control------------------------------------------------------------------------

grilla2 <- 10^seq(10, -1, length = 100)

fiveStats2 <- function(...) c(twoClassSummary(...), defaultSummary(...))

control2 <- trainControl(method = "cv",
                         number = 5,
                         summaryFunction = fiveStats2,
                         classProbs = TRUE,
                         verbose=FALSE,
                         savePredictions = T)



#Ridge--------------------------------------------------------------------------

ridge_incomes <- train(Pobre_1~P5000+P5010+Nper+Npersug+Lp+Ingtotugarr+Ingpcug+P5090_1+
                         P5090_2+P5090_3+P5090_4+P5090_5+P5090_6+Clase_1, 
                       data = train_hhs,
                       method = "glmnet",
                       trControl = control2,
                       family = "binomial",
                       preProcess = NULL,
                       metric = 'Accuracy',
                       tuneGrid = expand.grid(alpha = 0,lambda=grilla2))

ridge_incomes


#Lasso--------------------------------------------------------------------------

lasso_incomes <- train(Pobre_1~P5000+P5010+Nper+Npersug+Lp+Ingtotugarr+Ingpcug+P5090_1+
                         P5090_2+P5090_3+P5090_4+P5090_5+P5090_6+Clase_1, 
                       data = train_hhs,
                       method = "glmnet",
                       trControl = control2,
                       family = "binomial",
                       preProcess = NULL,
                       metric = 'Accuracy',
                       tuneGrid = expand.grid(alpha = 1,lambda=grilla2))

lasso_incomes



#EN-----------------------------------------------------------------------------

EN_incomes <- train(Pobre_1~P5000+P5010+Nper+Npersug+Lp+Ingtotugarr+Ingpcug+P5090_1+
                      P5090_2+P5090_3+P5090_4+P5090_5+P5090_6+Clase_1, 
                    data = train_hhs,
                    method = "glmnet",
                    trControl = control2,
                    family = "binomial",
                    preProcess = NULL,
                    metric = 'Accuracy',
                    tuneGrid = tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda=grilla2))

EN_incomes

 


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

-----------------------------------------------------------------------------
  
  #Dividimos train/test/eval (70/20/10) - BD Hogares
  
  set.seed(10110)
index_3 <- createDataPartition(y = train_h$Ingpcug , p = 0.7)[[1]]
trainhh2<- train_h[index_3,]
other2 <- train_h[-index_3,]


set.seed(10110)
index_4<- createDataPartition(y = other2$Ingpcug , p = 1/3)[[1]]
testhh2 <- other2[index_4,]
evalhh2 <- other2[-index_4,]


dim(train_h)   
dim(trainhh2)
dim(testhh2)
dim(evalhh2)

dim(train_h)[1] - dim(trainhh2)[1] - dim(testhh2)[1] - dim(evalhh2)[1]

##Estandarizamos

train_hhs2 <- trainhh2 #Guardamos las tres BD Originales aparte
test_hhs2  <- testhh2  
eval_hhs2  <- evalhh2
glimpse(train_hhs2)

variables_numericas <- c("num_cuartos", "num_cuartos_dormir", "Npersug",
                         "edad_jefe_hogar", "num_Menores_edad", "num_adulto_mayor", 
                         "Numper_por_dor", "Ocupados_por_perhog")

escalador <- preProcess(train_hhs2[, variables_numericas],
                        method = c("center", "scale"))

train_hhs2[, variables_numericas] <- predict(escalador, trainhh2[, variables_numericas])
test_hhs2[, variables_numericas] <- predict(escalador, testhh2[, variables_numericas])
eval_hhs2[, variables_numericas] <- predict(escalador, evalhh2[, variables_numericas])  

#Control------------------------------------------------------------------------

grilla2 <- 10^seq(10, -1, length = 100)

fiveStats2 <- function(...) c(twoClassSummary(...), defaultSummary(...))

control2 <- trainControl(method = "cv",
                         number = 5,
                         summaryFunction = fiveStats2,
                         classProbs = TRUE,
                         verbose=FALSE,
                         savePredictions = T)
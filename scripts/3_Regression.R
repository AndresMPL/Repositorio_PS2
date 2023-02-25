
#------------------------------------------------------------------------------#
#
#                       3. REGRESSION INCOMES
#
#------------------------------------------------------------------------------#

#Predicción de ingresos - Intento de predecir primero los ingresos

         #PROPUESTA ESTRUCTURA PRELIMINAR
  
train_hp3 <- train_hp3 %>% select(-id, -Ingtotugarr, -num_oc_hogar, -Pobre_No_Pobre, -Pobre_Pobre, -Clase_Urbano, -Vivienda_Propia_paga, -sexo_jefe_hogar_Hombre, -nivel_edu_jefe_hogar_Ninguno, -jefe_hogar_des_No, -jefe_hogar_oc_No, -jefe_hogar_ina_No, -Hacinamiento_No, -jefe_hogar_oc_Si)


#Dividimos train/test/eval (70/20/10) - BD Hogares

set.seed(10110)
index_3 <- createDataPartition(y = train_hp3$Ingpcug , p = 0.7)[[1]]
trainhh2<- train_hp3[index_3,]
other2 <- train_hp3[-index_3,]


set.seed(10110)
index_4<- createDataPartition(y = other2$Ingpcug , p = 1/3)[[1]]
testhh2 <- other2[index_4,]
evalhh2 <- other2[-index_4,]


dim(train_hp3)   
dim(trainhh2)
dim(testhh2)
dim(evalhh2)

dim(train_hp3)[1] - dim(trainhh2)[1] - dim(testhh2)[1] - dim(evalhh2)[1]

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
                         number = 10)

###############################################################################################

glimpse(train_hhs2)
colnames(train_hhs2)
set.seed(10110)
lineal_p3<-   train(Ingpcug ~ ., 
                    data = train_hhs2,
                    method = "lm",
                    trControl = control2,
                    preProcess = NULL)
lineal_p3

##RIDGE
ridge_p3<-train(Ingpcug ~ .,
                data=train_hhs2,
                method = 'glmnet', 
                trControl = control2,
                tuneGrid = expand.grid(alpha = 0, lambda = grilla2)) 
ridge_p3

coef_ridge_p3 <- coef(ridge_p3$finalModel, ridge_p3$bestTune$lambda)
coef_ridge_p3

##LASSO

set.seed(1010)
lasso_p3<-train(Ingpcug ~ .,
                data=train_hhs2,
                method = 'glmnet', 
                trControl = control2,
                tuneGrid = expand.grid(alpha = 1, lambda = grilla2)) 

lasso_p3

coef_lasso_p3 <- coef(lasso_p3$finalModel, lasso_p3$bestTune$lambda)
coef_lasso_p3

##ELASTIC NET

set.seed(10110)
en_p3 <- train(Ingpcug ~ . , 
               data = train_hhs2,
               method = "glmnet",
               trControl = control2,
               preProcess = NULL,
               tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1),lambda=grilla2))

en_p3

coef_en_p3 <- coef(en_p3$finalModel, en_p3$bestTune$lambda)
coef_en_p3
##---------------------

coefs_df_p3<-cbind(coef(lineal_p3$finalModel),as.matrix(coef_ridge_p3),as.matrix(coef_lasso_p3),as.matrix(coef_en_p3))
colnames(coefs_df_p3)<-c("OLS","RIDGE","LASSO","ELASTIC_NET")
round(coefs_df_p3,4)

####----

RMSE_df_p3<-cbind(lineal_p3$results$RMSE,ridge_p3$results$RMSE[which.min(ridge_p3$results$lambda)],lasso_p3$results$RMSE[which.min(lasso_p3$results$
                                                                                                                                     lambda)],en_p3$results$
                    RMSE[which.min(en_p3$results$
                                     lambda)])
colnames(RMSE_df_p3)<-c("OLS","RIDGE","LASSO","EN")
RMSE_df_p3
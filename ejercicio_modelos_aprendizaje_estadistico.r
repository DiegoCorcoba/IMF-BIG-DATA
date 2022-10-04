library(tidyverse)
library(here)
library(xray)
library(gridExtra)
library(missForest)
library(forcats)
library(FSelector)
library(glmnet)
library(caret)
library(knitr)
library(kableExtra)
library(broom)
library(influence.ME)
library(ISLR)
library(MASS)
library(dplyr)
library(vcd)
library(ggplot2)

##########3 ejercicio 1
#leer archivo
datos <- read.csv(file = 'crx.data', sep = ',')
#Cambiar nombres de las columnas
colnames(datos) <- c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15','A16')
#Eliminar interrogantes por NA (valor defecto R)
datos <- datos %>%
  na_if("?")
#Cambiar la clase de las columnas que no estaban bien
datos[c("A2","A14")] <- sapply(datos[c("A2","A14")], as.numeric)
datos[c("A1", "A4", "A5", "A6", "A7","A9","A10","A12","A13")] <- lapply(datos[c("A1", "A4", "A5", "A6", "A7","A9","A10","A12","A13")], as.factor)
#Cambiar +- por 0,1
datos <- datos %>% 
  mutate(A16 = as.factor(if_else(A16 == "-", 0, 1)))
#Visualizar todas las variables frente a A16(aprobacion del credito)

for (i in 1:ncol(datos)) {
  if (is.factor(datos[,i])){
     plot <- ggplot(data = datos, mapping = aes(x=datos[,i])) + geom_bar(aes(fill = A16),position = "dodge") +
             xlab(i)
  }
     else {
    plot <- ggplot(data = datos, mapping = aes(y=datos[,i],x=A16)) + geom_boxplot(aes(fill = A16)) + xlab(i)
  }
   x11()
   plot <- print(plot)
  
}


# Las variables con mas influencia en la aprobación del crédito son la A9, A10, A11 y A15.

########### ejercicio 2#####################
#utilizar missforest para completar los valores de NA
datos_completos <- missForest(datos)$ximp
############ ejercicio 3 ##################
#dividir el dataframe en train y test
train <- datos_completos[1:590,]
test <- datos_completos[591:690, ]
############  ejercicio 4 #################
#seleccionar las vairalbes train y test para x e y
x_train <- data.matrix(train[,1:15])
y_train <- data.matrix(train$A16)
x_test <- data.matrix(test[,1:15])
y_test <- data.matrix(test$A16)

# establecimiento de 3 particiones para el cross validation
n_folds <- 3
#fijando semilla inicial
set.seed(123)
#realizando la cross validation de ridge
cv_ridge <- cv.glmnet(x_train, y_train, family='binomial', alpha=0, 
                      parallel=TRUE, standardize=TRUE, type.measure='auc',
                      nfolds = n_folds)

#realizando la cross validation de lasso
cv_lasso <- cv.glmnet(x_train, y_train, family='binomial', alpha=1, 
                      parallel=TRUE, standardize=TRUE, type.measure='auc',
                      nfolds = n_folds)

# estimacion del mejor AUC en cada modelo
auc_ridge <- max(cv_ridge$cvm)
auc_lasso <- max(cv_lasso$cvm)

# Estimacion del error estandar del mejor AUC
se_auc_ridge <- cv_ridge$cvsd[which.max(cv_ridge$cvm)]/sqrt(n_folds) 
se_auc_lasso <- cv_lasso$cvsd[which.max(cv_lasso$cvm)]/sqrt(n_folds)

resultados_auc <- data.frame("AUC" = c("V. esperado", "Error"),
                             "Ridge" = round(c(auc_ridge, se_auc_ridge), 3), 
                             "LASSO" = round(c(auc_lasso, se_auc_lasso), 3)
)
#metricas del resultado del mejor AUC
resultados_auc

#Como se ve en la tabla los valores son muy parecidos respecto al AUC. Por tanto se puede usar uno u otro de forma indistinta. 
#Para este caso, se utilizarÃ¡ lasso al tener una aun mayor.

#modelo con lasso para sacar la matriz de confusiÃ³n
modelo_glm_lasso <- as.numeric(predict.glmnet(cv_lasso$glmnet.fit, newx=x_test, s=cv_lasso$lambda.min)>.5)
conf_matrix <- confusionMatrix(as.factor(modelo_glm_lasso),as.factor(y_test), mode = "everything", positive = "1")

################## ejercicio 5 ####################3

logodds <- tidy(coef(cv_lasso, s = cv_lasso$lambda.min))[c(1, 3)]
names(logodds) <- c("Variable", "Beta")
logodds$Beta <- round(logodds$Beta, 4)

logodds

################## ejercicio 6 ########################
conf_matrix
#COmo se observa en la matriz de confusiÃ³n, segun el modelo se predice que:
# a 85 personas se les deniega el crÃ©dito y en la realidad se le deniega (verdadero negativo)
# a 1 persona se le concede el crÃ©dito cuando en realidad se le deniega (falso positivo)
# a 8 personas se le deniega el credito cuando en realidad se le concede (falso negativo)
# a 5 personas se le concede cuando en realidad se le concede. (verdadero positivo)
#Como se puede observar en la matriz de confusión el modelo tiene una precision alta y es debido a que
# consigue acertar los negativos con mucha eficacia (98.88%) pero no es bueno acertando las concesiones de credito (38,46)%
# dicho esto, la ganancia sería: 5*100- 1*20, es decir 480 um.


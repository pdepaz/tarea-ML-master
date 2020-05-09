########################################################
#
# Prueba de Evaluacion Machine Learning
# Máster en Big Data & Business Analytics (2019/20)
# Alumno: Pablo de Paz Carbajo
# Realizado en abril de 2020
#
########################################################


getwd()

# Fijar el directorio en el que trabajaré
setwd('/Users/pablodepaz/Google Drive/A Master/9. ML con R y Python/Tarea (18 de junio)/TrabajoML')

# Cargar funciones desde otros archivos .R (archivo R usado en otro curso)
source("FuncionesML_R.R")

# Librerias usadas
paquetes(c('nnet','MASS','caret','pROC', 
           'ggplot2', 'plyr', 'h2o', 'parallel', 'doParallel'))


formula <- "income ~ `marital-status.Married-civ-spouse` + `occupation.Blue-Collar` + `capital-gain` + `hours-per-week` + occupation.Services + age + `education.HS-Graduate` + `education.High School` +`education.Elem-Middle-School` + education.Associates + `relationship.Own-child` + education.Bachelors + `occupation.Exec-managerial` + education.Masters + `relationship.Other-relative` + `occupation.Adm-clerical` + relationship.Wife + sex.Female + `workclass.Self-employed` + `marital-status.Never-married` + fnlwgt + `marital-status.Married-AF-spouse` + `native-country.United-States` + occupation.Sales + `relationship.Not-in-family` + `native-country.Western-Europe` + `capital-loss` + race.Black + `native-country.South-America` + `race.Amer-Indian-Eskimo` + `native-country.SE-Asia` +  `native-country.Eastern-Europe` + `workclass.Not-Working`"



########################################################
# Evaluación de algoritmo: 1) Redes Neuronales
#*******************************************************

# Uso el dataset con las variables elegidas anteriormente (adult_final_use).
# Aplico Validación Cruzada Repetida a AvNNet.


GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # Numero de cores del Mac (4). Por convencion, se deja 1 para el OS
registerDoParallel(cluster) # Registro del procesamiento paralelo

set.seed(12346)

# Control (con caret)
control <- trainControl(method = "repeatedcv", number=4, repeats=5, # 4 folds (divir dataframe en 4 partes). Repetir esto 5 veces.
                        savePredictions = "all", classProbs=TRUE) # classProbs=TRUE: guardar las probabilidades

# Parametros a tunear (rejilla)
avnnetgrid <- expand.grid(size=c(5,10,15,20), # Number of Hidden Units (size, numeric)
                          decay=c(0.01,0.1,0.001), bag=FALSE) # Weight Decay (decay, numeric). # Bagging (bag, logical)

# Modelo
redavnnet <- train(formula, data=adult_final_use, 
                   method = "avNNet", linout = FALSE, maxit = 100, # 100 iteraciones del proceso de optimizacion
                   trControl=control, tuneGrid = avnnetgrid, repeats = 5) # 5 repeticiones de las redes

stopCluster(cluster) # Apagar el cluster 
registerDoSEQ(); #Se fuerza a R a volver a un hilo de procesamiento
GS_T1 <- Sys.time()
GS_T1-GS_T0

redavnnet











########################################################
# Evaluación de algoritmo: 2) Bagging
#*******************************************************









########################################################
# Evaluación de algoritmo: 3) Random Forest
#*******************************************************







########################################################
#
# FIN
#
########################################################
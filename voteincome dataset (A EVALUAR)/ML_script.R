########################################################
#
# Prueba de Evaluacion Machine Learning
# Máster en Big Data & Business Analytics (2019/20)
# Alumno: Pablo de Paz Carbajo
# Realizado en abril de 2020
#
########################################################



########################################################
# Preparar espacio de trabajo e importar librerias
#*******************************************************

getwd()

# Fijar el directorio en el que trabajaré
setwd('/Users/pablodepaz/Google Drive/A Master/9. ML con R y Python/Tarea (18 de junio)/TrabajoML_voteincome')

# Cargar funciones desde otros archivos .R (archivo R usado en otro curso)
source("FuncionesML_R.R")



# Librerias
paquetes(c('nnet','dummies','MASS','reshape','pROC', 
           'caret', 'ggplot2', 'plyr', 'naniar', 'rpart',
           'rpart.plot', 'rattle', 'dplyr', 'h2o', 'questionr', 
           'psych', 'outliers', 'corrplot', 'parallel', 'doParallel',
           'randomForest'))




########################################################
# Cargar los datos y analizar el dataset
#*******************************************************
voteincome <- read.table("../voteincome/voteincome.csv", header=TRUE, sep=",", strip.white=TRUE)
voteincome[,1] <- NULL # 1a columna: numeracion de filas. No es util!

str(voteincome)
summary(voteincome)

# La variable "year" no aporta ningun valor pues toma el mismo valor (2000) siempre. La elimino
voteincome$year <- NULL
str(voteincome)

# Pasar de integer a numeric todas las variables (util para posteriores calculos)
voteincome[2:6] <- lapply(voteincome[2:6], as.numeric)
str(voteincome)

# Considero que "education" tiene pocos valores posibles (1 a 4) y puede ser un factor
voteincome$education <- factor(voteincome$education)

# Para la variable "female", voy a crear una nueva que sea "gender" (factor) con 2 niveles: Male o Female.
voteincome$gender <- as.factor(ifelse(voteincome$female == 1, "Female", "Male"))
voteincome$female <- NULL
str(voteincome)

# La variable "income" es un factor con los niveles siguientes
voteincome$income <- factor(voteincome$income)
voteincome$income <- revalue(voteincome$income, c("4"="Less-$5K"))
voteincome$income <- revalue(voteincome$income, c("5"="$5K-$7,49K"))
voteincome$income <- revalue(voteincome$income, c("6"="$7,5K-$9,9K"))
voteincome$income <- revalue(voteincome$income, c("7"="$10K-$12,49K"))
voteincome$income <- revalue(voteincome$income, c("8"="$12,5K-$14,9K"))
voteincome$income <- revalue(voteincome$income, c("9"="$15K-$19,9K"))
voteincome$income <- revalue(voteincome$income, c("10"="$20K-$24,9K"))
voteincome$income <- revalue(voteincome$income, c("11"="$25K-$29,9K"))
voteincome$income <- revalue(voteincome$income, c("12"="$30K-$34,9K"))
voteincome$income <- revalue(voteincome$income, c("13"="$35K-$39,9K"))
voteincome$income <- revalue(voteincome$income, c("14"="$40K-$49,9K"))
voteincome$income <- revalue(voteincome$income, c("15"="$50K-$59,9K"))
voteincome$income <- revalue(voteincome$income, c("16"="$60K-$74,9K"))
voteincome$income <- revalue(voteincome$income, c("17"="More-$75K"))
levels(voteincome$income)


# Establezco listas con las variables
listconti <- c("age")
listclass <- c("state", "income", "education", "gender")
vardep <- c("vote")


# Inspección visual de los datos
str(voteincome)
summary(voteincome)

# Para variables numéricas
hist(voteincome$age) 
psych::describe(Filter(is.numeric, voteincome))

# Para variables categóricas (factores)
questionr::freq(voteincome$state) 
levels(voteincome$state)
questionr::freq(voteincome$income) 
plot(voteincome$income)
levels(voteincome$income)
questionr::freq(voteincome$education)
plot(voteincome$education)
levels(voteincome$education)
questionr::freq(voteincome$gender)
plot(voteincome$gender)
levels(voteincome$gender)

ggplot(as.data.frame(voteincome$vote), aes(y = voteincome$vote)) + geom_bar(fill = "#0073C2FF") # Frecuencia de "vote"
# La variable a predecir no se encuentra balanceada en el dataset
questionr::freq(voteincome$vote)

# Resumen grafico de todas las variables
par(mfrow=c(3,3))
dfplot(as.data.frame(voteincome))

# Correlacion entre variables
par(mfrow=c(1,1))
corrplot(cor(cbind(voteincome$vote, 
                   Filter(is.numeric, voteincome)), use="pairwise", method="pearson"), method = "ellipse",type = "upper")
# Ninguna correlacion es muy significativa por lo que no voy a eliminar ninguna variable






########################################################
# Correción de errores. Tratamiento de outliers y missings
#*******************************************************

# Guardo en una variable el estado actual de "voteincome" como Backup
backup1 <- voteincome
#voteincome <- backup1


# No hay valores mal codificados ni NAs (al menos a simple vista)


# Tratamiento de outliers
summary(select_if(voteincome, is.numeric))
boxplot(voteincome$age, horizontal=TRUE, main="age", boxwex=0.1) # No hay outliers

# Tratamiento de missings
apply(is.na(voteincome), 2, sum); print(""); apply(is.na(voteincome), 2, mean) # No hay missings en ninguna variable






########################################################
# Normalizacion (var. continuas) y creacion de dummies (var. categoricas)
#*******************************************************

# Obtener dummies
if (listclass != c("")){
  voteincome_dummy <- voteincome[,c(vardep,listconti,listclass)]
  voteincome_dummy <- dummy.data.frame(voteincome_dummy, listclass, sep = ".")
} else {
  voteincome_dummy <- voteincome[,c(vardep,listconti)]
}


# Escalar
means <- apply(as.data.frame(voteincome_dummy[,listconti]), 2, mean)
sds <- sapply(as.data.frame(voteincome_dummy[,listconti]), sd)

age <- scale(voteincome_dummy[,listconti], center = means, scale = sds)

numerocont <- which(colnames(voteincome_dummy) %in% listconti) # "age" is in index 2
voteincome_dummy_sc <- cbind(age, voteincome_dummy[, -numerocont, drop=FALSE])

voteincome_dummy_sc[3:24] <- lapply(voteincome_dummy_sc[3:24], as.numeric)
str(voteincome_dummy_sc)

# Elimino variables auxiliares antes de continuar
rm(voteincome_dummy); rm(age); rm(means); rm(sds); rm(numerocont)






########################################################
# Variables más importantes y selección de variables
#*******************************************************

# Antes de continuar, creo 2 variables aleatorias que me servirán como "variables de control"
voteincome_dummy_sc$aleatorio <- runif(nrow(voteincome_dummy_sc))
voteincome_dummy_sc$aleatorio2 <- runif(nrow(voteincome_dummy_sc))



# Backup
backup2 <- voteincome_dummy_sc
#voteincome_dummy_sc <- backup2

# Guardo los nombres de las columnas y las variables continuas (incl. dummies)
colnames <- colnames(voteincome_dummy_sc)
listconti <- colnames(voteincome_dummy_sc[,-2]) # "vote" is in index 2
rm(listclass) # Ya no hay variables input cualitativas

# Seleccion de variables clasica (stepwise)
full<-lm(vote~., data=voteincome_dummy_sc)
null<-lm(vote~1, data=voteincome_dummy_sc)

seleccion1_AIC <- step(null, scope=list(lower=null, upper=full), direction="both", trace=FALSE) # No muestra trazas
summary(seleccion1_AIC)
seleccion1_BIC <- step(null, scope=list(lower=null, upper=full), direction="both", k=log(nrow(voteincome_dummy_sc)), trace=FALSE) # No muestra trazas
summary(seleccion1_BIC)

seleccion1_AIC$rank # 12
seleccion1_BIC$rank # 6

# Obtener la lista de variables que entran en el modelo y pegarla en la función de validación cruzada. 
# HAY QUE BORRAR LA CONSTANTE (Intercept) DEL MODELO
dput(names(seleccion1_AIC$coefficients))
dput(names(seleccion1_BIC$coefficients))

# En versión formula...
formula(seleccion1_AIC)
formula(seleccion1_BIC)



# Seleccion de variables repetida con submuestras

# Se carga la función "steprepetidobinaria" permite realizar el proceso training test varias veces obteniendo el modelo
# por stepwise sobre datos train y la tabla de frecuencias de los modelos escogidos.
source("funcion steprepetido binaria.R")

lista_AIC <- steprepetidobinaria(data=voteincome_dummy_sc, vardep=c("vote"), listconti=listconti,
                                 sinicio=12345, sfinal=12355, porcen=0.8, criterio="AIC") # Tarda 30 seg

tabla_AIC <- lista_AIC[[1]]
View(tabla_AIC)

dput(lista_AIC[[2]][[1]])
dput(lista_AIC[[2]][[2]])
dput(lista_AIC[[2]][[3]])
# ...
dput(lista_AIC[[2]][[10]])



lista_BIC <- steprepetidobinaria(data=voteincome_dummy_sc, vardep=c("vote"), listconti=listconti,
                                 sinicio=12345, sfinal=12355, porcen=0.8, criterio="BIC") # Tarda 20 seg

tabla_BIC <- lista_BIC[[1]]
View(tabla_BIC)

dput(lista_BIC[[2]][[1]])
dput(lista_BIC[[2]][[2]])
dput(lista_BIC[[2]][[3]])
# ...
dput(lista_BIC[[2]][[9]])



# Creo diferentes "sets" de variables
set1 <- names(seleccion1_AIC$coefficients)[-1] #11 variables
set2 <- names(seleccion1_BIC$coefficients)[-1] #5 variables
source("sets_vars.R") # El resto de sets (lo incluyo asi por limpieza del codigo)


# Una vez se tienen las listas de variables ("sets" anteriores), comparar con Regresion Logistica para la selección de variables definitiva.

# Podemos probar validación cruzada repetida para comparar vía sesgo-varianza
source("cruzadas avnnet y log binaria.R")

# Renombro la variable dependiente binaria (1=Yes, 0=No)
voteincome_dummy_sc$vote <- as.factor(voteincome_dummy_sc$vote)
voteincome_dummy_sc$vote <- mapvalues(voteincome_dummy_sc$vote, c("0", "1"), c("No", "Yes"))
str(voteincome_dummy_sc)


start_time <- Sys.time()

medias1 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set1, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias1$modelo= "modelo1"

medias2 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set2, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias2$modelo="modelo2"

medias3 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set3, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias3$modelo="modelo3"

medias4 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set4, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias4$modelo="modelo4"

medias5 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set5, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias5$modelo="modelo5"

medias6 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set6, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias6$modelo="modelo6"

medias7 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set7, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias7$modelo="modelo7"

medias8 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set8, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias8$modelo="modelo8"

medias9 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set9, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias9$modelo="modelo9"

medias10 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set10, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias10$modelo="modelo10"

medias11 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set11, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias11$modelo="modelo11"

medias12 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set12, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias12$modelo="modelo12"

medias13 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set13, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias13$modelo="modelo13"

medias14 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set14, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias14$modelo="modelo14"

medias15 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set15, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias15$modelo="modelo15"

medias16 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set16, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias16$modelo="modelo16"

medias17 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set17, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias17$modelo="modelo17"

medias18 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set18, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias18$modelo="modelo18"

medias19 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set19, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias19$modelo="modelo19"

medias20 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set20, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias20$modelo="modelo20"

medias21 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set21, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias21$modelo="modelo21"

medias22 <- cruzadalogistica(data=voteincome_dummy_sc, vardep=vardep, listconti=set22, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias22$modelo="modelo22"

end_time <- Sys.time()
print(end_time - start_time) # 1 min

rm(start_time); rm(end_time)

union <- rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8,medias9,medias10,medias11,medias12,
               medias13,medias14,medias15,medias16,medias17,medias18,medias19,medias20,medias21,medias22)

par(cex.axis=0.5)
boxplot(data=union, col="pink", tasa~modelo, main="Tasa de Fallos")
boxplot(data=union, col="pink", auc~modelo, main="AUC")

# La tasa de fallos es muy similar entre todos los modelos (con diferencias de centesimas o milesimas)
# La AUC varia un poco más, entre 0.56 y 0.66.

# Voy a seleccionar los mejores modelos para visualizarlo mejor, por tasa de fallos (los que sea menor) y por AUC (los que sea mayor)
union_fallos <- rbind(medias22, medias14, medias20, medias21, medias1, medias17)
boxplot(data=union_fallos, col="pink", tasa~modelo, main="Tasa de Fallos (modelos seleccionados)")

union_auc <- rbind(medias6, medias1, medias10, medias9, medias13, medias8, medias7, medias11)
boxplot(data=union_auc, col="pink", auc~modelo, main="AUC (modelos seleccionados)")

# Teniendo en cuenta los boxplots anteriores y a través de un "Estudio sesgo-varianza".
# - La diferencia en "Tasa de Fallos" entre los modelos seleccionados es minima. Cualquiera me valdria.
# - modelo1 y modelo6 tienen el mejor AUC. De ello, solo modelo1 aparece entre los mejores seleccionados anteriormente.
# Por otro lado, considero el número de variables de cada set (ronda entre 9 y 12).

# Con todo ello, elegiré el set de variables includas en el modelo1 


set1_1 <- gsub(pattern = '`', x = set1, replacement = "") # 11

voteincome_final <- select(voteincome_dummy_sc, set1_1)
voteincome_final <- cbind(voteincome_final, voteincome_dummy_sc$vote) # 12 variables en total
names(voteincome_final)[names(voteincome_final) == "voteincome_dummy_sc$vote"] <- "vote"

# Backup
backup3 <- voteincome_final
#voteincome_final <- backup3

formula <- formula(seleccion1_AIC)


# Exporto este dataframe creado para evaluar algoritmos
write.csv(voteincome_final,"voteincome_final.csv", row.names = TRUE)







########################################################
# Evaluación de algoritmo: 1.A) Redes Neuronales (NNet)
#*******************************************************

# Uso el dataset con las variables elegidas anteriormente (voteincome_final).

GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # Numero de cores del Mac (4). Por convencion, se deja 1 para el OS
registerDoParallel(cluster) # Registro del procesamiento paralelo

set.seed(12346)

# Control (con caret)
control <- trainControl(method = "repeatedcv", number=4, repeats=5, # 4 folds (divir dataframe en 4 partes). Repetir esto 5 veces.
                        savePredictions = "all", classProbs=TRUE) # classProbs=TRUE: guardar las probabilidades

# Parametros a tunear (rejilla)
nnetgrid <- expand.grid(size=c(5,10,15,20), # Number of Hidden Units (size, numeric)
                          decay=c(0.01,0.1,0.001)) # Weight Decay (decay, numeric). # Bagging (bag, logical)

# Modelo
rednnet <- train(formula, data=voteincome_final, 
                   method = "nnet", linout = FALSE, maxit = 100, # 100 iteraciones del proceso de optimizacion
                   trControl=control, tuneGrid = nnetgrid, repeats = 5) # 5 repeticiones de las redes

stopCluster(cluster) # Apagar el cluster 
registerDoSEQ(); #Se fuerza a R a volver a un hilo de procesamiento
GS_T1 <- Sys.time()
GS_T1-GS_T0 # Entorno a 30 seg

rednnet





########################################################
# Evaluación de algoritmo: 1.B) Redes Neuronales (AvNNet)
#*******************************************************

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
redavnnet <- train(formula, data=voteincome_final, 
                   method = "avNNet", linout = FALSE, maxit = 100, # 100 iteraciones del proceso de optimizacion
                   trControl=control, tuneGrid = avnnetgrid, repeats = 5) # 5 repeticiones de las redes

stopCluster(cluster) # Apagar el cluster 
registerDoSEQ(); #Se fuerza a R a volver a un hilo de procesamiento
GS_T1 <- Sys.time()
GS_T1-GS_T0 # Entorno a 2 min

redavnnet

# Podemos manipular el numero maximo de iteraciones, modificando el parámetro maxit en la función cruzadaavnnetbin. 
# Como en caret no se permite tuneado de maxit, en este caso lo hacemos con CV Repetida y gráficos.

source ("cruzadas avnnet y log binaria.R")

GS_T0 <- Sys.time()
m1_avnnet <- cruzadaavnnetbin(data=voteincome_final, vardep="vote", 
                              listconti=set1_1, listclass=c(""), grupos=4, sinicio=12346, repe=5,
                              size=c(15), decay=c(0.001), repeticiones=5, itera=50)

m1_avnnet$modelo="avnnet50"

m2_avnnet <- cruzadaavnnetbin(data=voteincome_final, vardep="vote", 
                              listconti=set1_1, listclass=c(""), grupos=4, sinicio=12346, repe=5,
                              size=c(15), decay=c(0.001), repeticiones=5, itera=100)

m2_avnnet$modelo="avnnet100"

m3_avnnet <- cruzadaavnnetbin(data=voteincome_final, vardep="vote", 
                              listconti=set1_1, listclass=c(""), grupos=4, sinicio=12346, repe=5,
                              size=c(15), decay=c(0.001), repeticiones=5, itera=150)

m3_avnnet$modelo="avnnet150"

m4_avnnet <- cruzadaavnnetbin(data=voteincome_final, vardep="vote", 
                              listconti=set1_1, listclass=c(""), grupos=4, sinicio=12346, repe=5,
                              size=c(15), decay=c(0.001), repeticiones=5, itera=200)

m4_avnnet$modelo="avnnet200"

m5_avnnet <- cruzadaavnnetbin(data=voteincome_final, vardep="vote", 
                              listconti=set1_1, listclass=c(""), grupos=4, sinicio=12346, repe=5,
                              size=c(15), decay=c(0.001), repeticiones=5, itera=250)

m5_avnnet$modelo="avnnet250"

m6_avnnet <- cruzadaavnnetbin(data=voteincome_final, vardep="vote", 
                              listconti=set1_1, listclass=c(""), grupos=4, sinicio=12346, repe=5,
                              size=c(15), decay=c(0.001), repeticiones=5, itera=300)

m6_avnnet$modelo="avnnet300"

m7_avnnet <- cruzadaavnnetbin(data=voteincome_final, vardep="vote", 
                              listconti=set1_1, listclass=c(""), grupos=4, sinicio=12346, repe=5,
                              size=c(15), decay=c(0.001), repeticiones=5, itera=400)

m7_avnnet$modelo="avnnet400"

m8_avnnet <- cruzadaavnnetbin(data=voteincome_final, vardep="vote", 
                              listconti=set1_1, listclass=c(""), grupos=4, sinicio=12346, repe=5,
                              size=c(15), decay=c(0.001), repeticiones=5, itera=500)

m8_avnnet$modelo="avnnet500"


GS_T1 <- Sys.time()
GS_T1-GS_T0 # Entorno a 10 min

union_mx_avnnet <- rbind(m1_avnnet, m2_avnnet, m3_avnnet, m4_avnnet, m5_avnnet, m6_avnnet, m7_avnnet, m8_avnnet)

par(cex.axis=0.8)
boxplot(data=union_mx_avnnet, auc~modelo, main="AUC", col="pink")

uni_mx_avnnet <- union_mx_avnnet
uni_mx_avnnet$modelo <- with(uni_mx_avnnet,reorder(modelo, auc, mean))

par(cex.axis=0.8, las=2)
boxplot(data=uni_mx_avnnet, auc~modelo, main="AUC ordenada", col="pink")

# A medida que aumentan las iteraciones, la AUC también aumenta, así como la varianza. 
# Como la diferencia de AUC entre 150 y 500 iteraciones no es tan grande y ambas tienen una varianza pequeña
# comparado con el resto de las iteraciones entre medias, elegiré 150 iteraciones por la menor potencia computacional necesaria.






########################################################
# Evaluación de algoritmo: 2) Bagging
#*******************************************************

GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # Numero de cores del Mac (4). Por convencion, se deja 1 para el OS
registerDoParallel(cluster) # Registro del procesamiento paralelo

bagginggrid <- expand.grid(mtry=c(11)) # mtry: numero de variables independientes en el modelo

set.seed(12346)

control <- trainControl(method = "repeatedcv", number=4, repeats=5,
                        savePredictions = "all", classProbs=TRUE)

bagging <- train(data=voteincome_final, formula,
           method="rf", trControl=control, tuneGrid=bagginggrid,
           linout = FALSE, ntree=1000, nodesize=150, replace=TRUE) # ntree: numero iteraciones/arboles. # nodesize: tamaño maximo nodos finales

stopCluster(cluster) # Apagar el cluster 
registerDoSEQ(); #Se fuerza a R a volver a un hilo de procesamiento
GS_T1 <- Sys.time()
GS_T1-GS_T0 # Entorno a 20 segs

bagging

# Podemos manipular el tamaño muestral para observar su efecto sobre el modelo bagging, incorporando el parámetro sampsize en la función cruzadarfbin. 
# Como en caret no se permite tuneado de sampsize, en este caso lo hacemos con CV Repetida y gráficos.

# Sampsize debe ser menor que el número de observaciones training 
# Al usar Validación Cruzada de 4 grupos, sampsize debe ser menor que 0.75*n = 1.125 (en este caso).

# Recordatorio: sampsize maximo 1125.

source ("cruzada rf binaria.R")

GS_T0 <- Sys.time()
m1 <- cruzadarfbin(data=voteincome_final, vardep="vote",
                   listconti=set1_1, listclass=c(""),
                   grupos=10, sinicio=12346, repe=20, nodesize=150,
                   mtry=11, ntree=1000, replace=TRUE, sampsize=100)

m1$modelo="bagging100"

m2 <- cruzadarfbin(data=voteincome_final, vardep="vote",
                   listconti=set1_1, listclass=c(""),
                   grupos=10, sinicio=12346, repe=20, nodesize=150,
                   mtry=11, ntree=1000, replace=TRUE, sampsize=250)

m2$modelo="bagging250"

m3 <- cruzadarfbin(data=voteincome_final, vardep="vote",
                   listconti=set1_1, listclass=c(""),
                   grupos=10, sinicio=12346, repe=20, nodesize=150,
                   mtry=11, ntree=1000, replace=TRUE, sampsize=500)

m3$modelo="bagging500"

m4 <- cruzadarfbin(data=voteincome_final, vardep="vote",
                   listconti=set1_1, listclass=c(""),
                   grupos=10, sinicio=12346, repe=20, nodesize=150,
                   mtry=11, ntree=1000, replace=TRUE, sampsize=750)

m4$modelo="bagging750"

m5 <- cruzadarfbin(data=voteincome_final, vardep="vote",
                   listconti=set1_1, listclass=c(""),
                   grupos=10, sinicio=12346, repe=20, nodesize=150,
                   mtry=11, ntree=1000, replace=TRUE, sampsize=1000)

m5$modelo="bagging1000"

m6 <- cruzadarfbin(data=voteincome_final, vardep="vote",
                   listconti=set1_1, listclass=c(""),
                   grupos=10, sinicio=12346, repe=20, nodesize=150,
                   mtry=11, ntree=1000, replace=TRUE)

m6$modelo="bagging1125_BASE"

GS_T1 <- Sys.time()
GS_T1-GS_T0 # Entorno a 14 min

union_mx <- rbind(m1, m2, m3, m4, m5, m6)

par(cex.axis=0.8)
boxplot(data=union_mx, auc~modelo, main="AUC", col="pink")

uni_mx <- union_mx
uni_mx$modelo <- with(uni_mx,reorder(modelo, auc, mean))

par(cex.axis=0.8, las=2)
boxplot(data=uni_mx, auc~modelo, main="AUC ordenada", col="pink")

# Se aprecia que los modelos con mejor comportamiento son, en orden descendiente, aquellos cuyo sampsize es 100, 1125, 1000.
# En un principio, elegiré aquel cuyo sampsize=100.





########################################################
# Evaluación de algoritmo: 3) Random Forest
#*******************************************************

# Como se ha estudiado, el RF es una variacion del Bagging que incorpora alteatoriedad en la construccion de arboles

GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # Numero de cores del Mac (4). Por convencion, se deja 1 para el OS
registerDoParallel(cluster) # Registro del procesamiento paralelo

rfgrid <- expand.grid(mtry=c(3,4,5,6,7,8,9,10,11))

set.seed(12346)
control <- trainControl(method = "repeatedcv", number=4, repeats=5,
                        savePredictions = "all", classProbs=TRUE)

rf <- train(data=voteincome_final, formula,
            method="rf", trControl=control, tuneGrid=rfgrid,
            linout = FALSE, ntree=1000, nodesize=150, replace=TRUE, importance=TRUE) 

stopCluster(cluster) # Apagar el cluster 
registerDoSEQ(); #Se fuerza a R a volver a un hilo de procesamiento
GS_T1 <- Sys.time()
GS_T1-GS_T0 # Entorno a 2 min

rf 
# mtry recomendado: 3. A pesar de la recomendacion que hace Caret, obtendriamos el mismo resultado con cualquier valor de mtry 
# Esto significa que en cada árbol creado sortearíamos en cada nodo 3 variables de las 11 variables input y,
# de esas 3 elegiríamos la mejor.


# RF permite ver la Importancia de las Variables
# Con esto, podriamos elegir un punto a partir de la cual desechar el resto de variables

rf_final <- rf$finalModel

rf_tabla <- as.data.frame(importance(rf_final))
rf_tabla <- rf_tabla[order(-rf_tabla$MeanDecreaseAccuracy),]
rf_tabla

barplot(rf_tabla$MeanDecreaseAccuracy,names.arg=rownames(rf_tabla))


# Recordatorio: sampsize maximo 1125.

source ("cruzada rf binaria.R")

GS_T0 <- Sys.time()
m1 <- cruzadarfbin(data=voteincome_final, vardep="vote",
                   listconti=set1_1, listclass=c(""),
                   grupos=10, sinicio=12346, repe=20, nodesize=150,
                   mtry=3, ntree=1000, replace=TRUE, sampsize=100)

m1$modelo="rf100"

m2 <- cruzadarfbin(data=voteincome_final, vardep="vote",
                   listconti=set1_1, listclass=c(""),
                   grupos=10, sinicio=12346, repe=20, nodesize=150,
                   mtry=3, ntree=1000, replace=TRUE, sampsize=250)

m2$modelo="rf250"

m3 <- cruzadarfbin(data=voteincome_final, vardep="vote",
                   listconti=set1_1, listclass=c(""),
                   grupos=10, sinicio=12346, repe=20, nodesize=150,
                   mtry=3, ntree=1000, replace=TRUE, sampsize=500)

m3$modelo="rf500"

m4 <- cruzadarfbin(data=voteincome_final, vardep="vote",
                   listconti=set1_1, listclass=c(""),
                   grupos=10, sinicio=12346, repe=20, nodesize=150,
                   mtry=3, ntree=1000, replace=TRUE, sampsize=750)

m4$modelo="rf750"

m5 <- cruzadarfbin(data=voteincome_final, vardep="vote",
                   listconti=set1_1, listclass=c(""),
                   grupos=10, sinicio=12346, repe=20, nodesize=150,
                   mtry=3, ntree=1000, replace=TRUE, sampsize=1000)

m5$modelo="rf1000"

m6 <- cruzadarfbin(data=voteincome_final, vardep="vote",
                   listconti=set1_1, listclass=c(""),
                   grupos=10, sinicio=12346, repe=20, nodesize=150,
                   mtry=3, ntree=1000, replace=TRUE)

m6$modelo="rf1125_BASE"

GS_T1 <- Sys.time()
GS_T1-GS_T0 # Entorno a 14 min

union_mx <- rbind(m1, m2, m3, m4, m5, m6)

par(cex.axis=0.8)
boxplot(data=union_mx, auc~modelo, main="AUC", col="pink")

uni_mx <- union_mx
uni_mx$modelo <- with(uni_mx,reorder(modelo, auc, mean))

par(cex.axis=0.8, las=2)
boxplot(data=uni_mx, auc~modelo, main="AUC ordenada", col="pink")

# Se aprecia que el modelo con mejor comportamiento es aquel cuyo sampsize es 100. Ese elijo.





########################################################
# Evaluación de algoritmo: 4) Gradient Boosting
#*******************************************************

# Notese que los parametros a utilizar en GB son interdependientes

# Caret permite tunear:
# - shrinkage (parámetro v de regularización, mide la velocidad de ajuste, a menor v, más lento y necesita más iteraciones, pero es más fino en el ajuste)
# - n.minobsinnode= tamaño máximo de nodos finales (el principal parámetro que mide la complejidad)
# - n.trees = el número de iteraciones (árboles)
# - interaction.depth (2 para árboles binarios)

GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # Numero de cores del Mac (4). Por convencion, se deja 1 para el OS
registerDoParallel(cluster) # Registro del procesamiento paralelo

gbmgrid <- expand.grid(shrinkage=c(0.1,0.05,0.03,0.01,0.001),
                     n.minobsinnode=c(10,20,50,100,150),
                     n.trees=c(100,500,1000,5000),
                     interaction.depth=c(2))

set.seed(12346)
control <- trainControl(method = "repeatedcv", number=4, savePredictions = "all", classProbs=TRUE) 

gbm <- train(data=voteincome_final, formula,
             method="gbm", trControl=control, tuneGrid=gbmgrid,
             distribution="bernoulli", bag.fraction=1, verbose=FALSE) 

stopCluster(cluster) # Apagar el cluster 
registerDoSEQ(); #Se fuerza a R a volver a un hilo de procesamiento
GS_T1 <- Sys.time()
GS_T1-GS_T0 # Entorno a 2 min

gbm

# Veamos en una gráfica el comportamiento del algoritmo
plot(gbm)

# Probamos a fijar shrinkage=0.05, n.minobsinnode=100


# Estudio de Early Stopping
# Probamos a fijar algunos parámetros para ver como evoluciona en función de las iteraciones
GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # Numero de cores del Mac (4). Por convencion, se deja 1 para el OS
registerDoParallel(cluster) # Registro del procesamiento paralelo

gbmgrid <- expand.grid(shrinkage=c(0.05),
                     n.minobsinnode=c(100),
                     n.trees=c(50,100,300,500,800,1000,1200,1500,2000),
                     interaction.depth=c(2))

set.seed(12346)
control <- trainControl(method = "repeatedcv", number=4, savePredictions = "all", classProbs=TRUE)  


gbm <- train(data=voteincome_final, formula,
             method="gbm", trControl=control, tuneGrid=gbmgrid,
             distribution="bernoulli", bag.fraction=1, verbose=FALSE) 

stopCluster(cluster) # Apagar el cluster 
registerDoSEQ(); #Se fuerza a R a volver a un hilo de procesamiento
GS_T1 <- Sys.time()
GS_T1-GS_T0 # Entorno a 10 segs

gbm
plot(gbm) # Parece ser que con 1500 arboles es suficiente






########################################################
# Evaluación de algoritmo: 4.A) XgBoost
#*******************************************************

# Caret permite tunear:

#  
# - nrounds (# Boosting Iterations) = número de iteraciones
# - max_depth (Max Tree Depth) = profundida máxima de los árboles
# - eta (Shrinkage) = parámetro v gradient boosting
# - gamma (Minimum Loss Reduction) = gamma
# - cte regularización. Dejar a 0 por defecto
# - colsample_bytree (Subsample Ratio of Columns) = % Sorteo variables antes de cada árbol. Dejar a 1 por defecto.
# - min_child_weight (Minimum Sum of Instance Weight) = observaciones mínimas en el nodo final. Similar al minobsinnode del gbm.
# - # subsample (Subsample Percentage) = % Sorteo de observaciones antes de cada árbol. Dejar a 1 por defecto.

GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # Numero de cores del Mac (4). Por convencion, se deja 1 para el OS
registerDoParallel(cluster) # Registro del procesamiento paralelo

xgbmgrid <- expand.grid(min_child_weight=c(10,20,50,100,150),
                        eta=c(0.1,0.05,0.03,0.01,0.001),
                        nrounds=c(100,500,1000,2500,5000),
                        max_depth=6, gamma=0, colsample_bytree=1, subsample=1)

set.seed(12346)
control <- trainControl(method = "repeatedcv", number=4, savePredictions = "all", classProbs=TRUE)

xgbm <- train(data=voteincome_final, formula,
              method="xgbTree", trControl=control, tuneGrid=xgbmgrid, verbose=FALSE) 

stopCluster(cluster) # Apagar el cluster 
registerDoSEQ(); #Se fuerza a R a volver a un hilo de procesamiento
GS_T1 <- Sys.time()
GS_T1-GS_T0 # Entorno a 4min

xgbm

# Veamos en una gráfica el comportamiento del algoritmo
plot(xgbm)

# Probamos a fijar nrounds=500, max_depth=6, eta=0.03, min_child_weight=10, colsample_bytree=1, subsample=1, gamma=0.


# Estudio de Early Stopping
# Probamos a fijar algunos parámetros para ver como evoluciona en función de las iteraciones
GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # Numero de cores del Mac (4). Por convencion, se deja 1 para el OS
registerDoParallel(cluster) # Registro del procesamiento paralelo

xgbmgrid <- expand.grid(min_child_weight=c(10),
                        eta=c(0.03),
                        nrounds=c(50,100,500,1000,2000,3000,4000,5000),
                        max_depth=6, gamma=0, colsample_bytree=1, subsample=1)

set.seed(12345)
control <- trainControl(method = "repeatedcv", number=4, savePredictions = "all", classProbs=TRUE) 

xgbm <- train(data=voteincome_final, formula,
              method="xgbTree", trControl=control, tuneGrid=xgbmgrid, verbose=FALSE) 

stopCluster(cluster) # Apagar el cluster 
registerDoSEQ(); #Se fuerza a R a volver a un hilo de procesamiento
GS_T1 <- Sys.time()
GS_T1-GS_T0 # Entorno a 30 segs

xgbm
plot(xgbm) # Parece ser que con nrounds=500 es suficiente





########################################################
# Evaluación de algoritmo: 5.A) SVM (Lineal)
#*******************************************************

# Tuneado: solo parámetro C (mayor C, menor sesgo y mayor sobreajuste)

GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # Numero de cores del Mac (4). Por convencion, se deja 1 para el OS
registerDoParallel(cluster) # Registro del procesamiento paralelo

SVMgrid <- expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10))

set.seed(12345)
control <- trainControl(method = "repeatedcv", number=4, savePredictions = "all") 

SVM_lin <- train(data=voteincome_final, formula, method="svmLinear", trControl=control,
            tuneGrid=SVMgrid, verbose=FALSE)

stopCluster(cluster) # Apagar el cluster 
registerDoSEQ(); #Se fuerza a R a volver a un hilo de procesamiento
GS_T1 <- Sys.time()
GS_T1-GS_T0 # Entorno a 10 segs

SVM_lin$results
plot(SVM_lin$results$C, SVM_lin$results$Accuracy) # Accuracy es igual con cualquier C (no varía)

# Eligo C=0.5 (un valor no muy alto pero no muy bajo)






########################################################
# Evaluación de algoritmo: 5.B) SVM (Polinomial)
#*******************************************************

# Tuneado: C, degree y scale

GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # Numero de cores del Mac (4). Por convencion, se deja 1 para el OS
registerDoParallel(cluster) # Registro del procesamiento paralelo

SVMgrid <- expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10),
                      degree=c(2,3),
                      scale=c(0.1,0.5,1,2,5))

set.seed(12345)
control <- trainControl(method = "repeatedcv", number=4, savePredictions = "all") 

SVM_poli <- train(data=voteincome_final, formula, method="svmPoly", trControl=control,
                  tuneGrid=SVMgrid, verbose=FALSE)

stopCluster(cluster) # Apagar el cluster 
registerDoSEQ(); #Se fuerza a R a volver a un hilo de procesamiento
GS_T1 <- Sys.time()
GS_T1-GS_T0 # Entorno a 35 min

SVM_poli

SVM_poli$results


SVM_poli_results <- as.data.frame(SVM_poli$results)
library(ggplot2)

# Plot de dos variables categoricas, una continua
ggplot(SVM_poli_results, aes(x=factor(C), y=Accuracy, color=factor(degree), pch=factor(scale))) +
  geom_point(position=position_dodge(width=0.5), size=3)

# Solo DEGREE=2 ya que la Accuracy es más alta
SVM_poli_results2 <- SVM_poli_results[SVM_poli_results$degree==2,]  

ggplot(SVM_poli_results2, aes(x=factor(C), y=Accuracy, colour=factor(scale))) +
  geom_point(position=position_dodge(width=0.5),size=3)

# Caret me sugiere "degree = 2, scale = 0.1 and C = 0.01". 
# En vista de la grafica, degree=2, scale=0.1 y C podría ser 0.01, 0.05, 0.1 o 0.2.








########################################################
# Evaluación de algoritmo: 5.C) SVM (RBF)
#*******************************************************

# Tuneado: C, sigma

GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # Numero de cores del Mac (4). Por convencion, se deja 1 para el OS
registerDoParallel(cluster) # Registro del procesamiento paralelo

SVMgrid <- expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10,30),
                       sigma=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10,30))

set.seed(12345)
control <- trainControl(method = "repeatedcv", number=4, savePredictions = "all") 

SVM_radial <- train(data=voteincome_final, formula, method="svmRadial", trControl=control,
             tuneGrid=SVMgrid, verbose=FALSE)

stopCluster(cluster) # Apagar el cluster 
registerDoSEQ(); #Se fuerza a R a volver a un hilo de procesamiento
GS_T1 <- Sys.time()
GS_T1-GS_T0 # Entorno a 30 segs

SVM_radial

SVM_radial_results <- as.data.frame(SVM_radial$results)

ggplot(SVM_radial_results, aes(x=factor(C), y=Accuracy, color=factor(sigma)))+ 
  geom_point(position=position_dodge(width=0.5),size=3)

# Best values at sigma = 30 and C = 1






########################################################
# Ensamblado y comparacion entre modelos
#*******************************************************

# Una vez llegados a este punto del codigo, podemos realizar pruebas de ensamblado con los distintos algoritmos usados.

# 1) Leer las cruzadas de Ensamblado
source("cruzadas ensamblado binaria fuente.R")

# 2) Obtener datos de CV repetida para cada algoritmo y procesar el resultado
repe <- 5

ensamblado_medias0 <- cruzadalogistica(data=voteincome_final, vardep=vardep, 
                            listconti=set1, listclass=c(""), grupos=4, sinicio=1234, repe=repe)
ensamblado_medias0bis <- as.data.frame(ensamblado_medias0[1])
ensamblado_medias0bis$modelo <- "Logistica"
ensamblado_predi0 <- as.data.frame(ensamblado_medias0[2])
ensamblado_predi0$logi <- ensamblado_predi0$Yes


ensamblado_medias1 <- cruzadaavnnetbin(data=voteincome_final, vardep=vardep, 
                                       listconti=set1_1, listclass=c(""), grupos=4, sinicio=1234, repe=repe,
                                       size=c(15), decay=c(0.001), repeticiones=5, itera=150)
ensamblado_medias1bis <- as.data.frame(ensamblado_medias1[1])
ensamblado_medias1bis$modelo <- "AvNNet"
ensamblado_predi1 <- as.data.frame(ensamblado_medias1[2])
ensamblado_predi1$avnnet <- ensamblado_predi1$Yes


ensamblado_medias2 <- cruzadarfbin(data=voteincome_final, vardep=vardep, 
                                   listconti=set1_1, listclass=c(""), grupos=4, sinicio=1234, repe=repe,
                                   mtry=11,ntree=1000,nodesize=150,replace=TRUE,sampsize=100)
ensamblado_medias2bis <- as.data.frame(ensamblado_medias2[1])
ensamblado_medias2bis$modelo <- "bagging"
ensamblado_predi2 <- as.data.frame(ensamblado_medias2[2])
ensamblado_predi2$bagging <- ensamblado_predi2$Yes


ensamblado_medias3 <- cruzadarfbin(data=voteincome_final, vardep=vardep, 
                                   listconti=set1_1, listclass=c(""), grupos=4, sinicio=1234, repe=repe,
                                   mtry=3,ntree=1000,nodesize=150,replace=TRUE,sampsize=100)
ensamblado_medias3bis <- as.data.frame(ensamblado_medias3[1])
ensamblado_medias3bis$modelo <- "rf"
ensamblado_predi3 <- as.data.frame(ensamblado_medias3[2])
ensamblado_predi3$rf <- ensamblado_predi3$Yes


ensamblado_medias4 <- cruzadagbmbin(data=voteincome_final, vardep=vardep, 
                                    listconti=set1_1, listclass=c(""), grupos=4, sinicio=1234, repe=repe,
                                    n.minobsinnode=100, shrinkage=0.05, n.trees=1500, interaction.depth=2)
ensamblado_medias4bis <- as.data.frame(ensamblado_medias4[1])
ensamblado_medias4bis$modelo <- "gbm"
ensamblado_predi4 <- as.data.frame(ensamblado_medias4[2])
ensamblado_predi4$gbm <- ensamblado_predi4$Yes


ensamblado_medias5 <- cruzadaxgbmbin(data=voteincome_final, vardep=vardep, 
                                     listconti=set1_1, listclass=c(""), grupos=4, sinicio=1234, repe=repe,
                                     min_child_weight=10, eta=0.03, nrounds=500, max_depth=6, gamma=0, 
                                     colsample_bytree=1, subsample=1, alpha=0, lambda=0, lambda_bias=0)
ensamblado_medias5bis <- as.data.frame(ensamblado_medias5[1])
ensamblado_medias5bis$modelo <- "xgbm"
ensamblado_predi5 <- as.data.frame(ensamblado_medias5[2])
ensamblado_predi5$xgbm <- ensamblado_predi5$Yes


ensamblado_medias6 <- cruzadaSVMbin(data=voteincome_final, vardep=vardep, 
                                     listconti=set1_1, listclass=c(""), grupos=4, sinicio=1234, repe=repe,
                                     C=0.5)
ensamblado_medias6bis <- as.data.frame(ensamblado_medias6[1])
ensamblado_medias6bis$modelo <- "svmLinear"
ensamblado_predi6 <- as.data.frame(ensamblado_medias6[2])
ensamblado_predi6$svmLinear <- ensamblado_predi6$Yes


ensamblado_medias7 <- cruzadaSVMbinPoly(data=voteincome_final, vardep=vardep, 
                                    listconti=set1_1, listclass=c(""), grupos=4, sinicio=1234, repe=repe,
                                    C=0.1, degree=2, scale=0.1)
ensamblado_medias7bis <- as.data.frame(ensamblado_medias7[1])
ensamblado_medias7bis$modelo <- "svmPoly"
ensamblado_predi7 <- as.data.frame(ensamblado_medias7[2])
ensamblado_predi7$svmPoly <- ensamblado_predi7$Yes


ensamblado_medias8 <- cruzadaSVMbinRBF(data=voteincome_final, vardep=vardep, 
                                        listconti=set1_1, listclass=c(""), grupos=4, sinicio=1234, repe=repe,
                                        C=1, sigma=30)
ensamblado_medias8bis <- as.data.frame(ensamblado_medias8[1])
ensamblado_medias8bis$modelo <- "svmRadial"
ensamblado_predi8 <- as.data.frame(ensamblado_medias8[2])
ensamblado_predi8$svmRadial <- ensamblado_predi8$Yes


ensamblado_union <- rbind(ensamblado_medias0bis, ensamblado_medias1bis, ensamblado_medias2bis, ensamblado_medias3bis,
                          ensamblado_medias4bis, ensamblado_medias5bis, ensamblado_medias6bis, ensamblado_medias7bis,
                          ensamblado_medias8bis)

par(cex.axis=0.8)
boxplot(data=ensamblado_union, tasa~modelo, col="pink", main='Tasa de Fallos')
boxplot(data=ensamblado_union, auc~modelo, col="pink", main='AUC') # Me fijo más en este parametro (independiente del punto de corte)



# 3) Construccion de los ensamblados a partir de predicciones

unipredi <- cbind(ensamblado_predi0, ensamblado_predi1, ensamblado_predi2, ensamblado_predi3,
                  ensamblado_predi4, ensamblado_predi5, ensamblado_predi6, ensamblado_predi7,
                  ensamblado_predi8)

# Eliminar columnas duplicadas
unipredi<- unipredi[, !duplicated(colnames(unipredi))]

# Construccion de ensamblados
unipredi$ensamblado_predi9<-(unipredi$logi+unipredi$avnnet)/2
unipredi$ensamblado_predi10a<-(unipredi$logi+unipredi$bagging)/2
unipredi$ensamblado_predi10b<-(unipredi$logi+unipredi$rf)/2
unipredi$ensamblado_predi11<-(unipredi$logi+unipredi$gbm)/2
unipredi$ensamblado_predi12<-(unipredi$logi+unipredi$xgbm)/2
unipredi$ensamblado_predi13<-(unipredi$logi+unipredi$svmLinear)/2
unipredi$ensamblado_predi14<-(unipredi$logi+unipredi$svmPoly)/2
unipredi$ensamblado_predi15<-(unipredi$logi+unipredi$svmRadial)/2

unipredi$ensamblado_predi16a<-(unipredi$avnnet+unipredi$bagging)/2
unipredi$ensamblado_predi16b<-(unipredi$avnnet+unipredi$rf)/2
unipredi$ensamblado_predi17<-(unipredi$avnnet+unipredi$gbm)/2
unipredi$ensamblado_predi18<-(unipredi$avnnet+unipredi$xgbm)/2
unipredi$ensamblado_predi19<-(unipredi$avnnet+unipredi$svmLinear)/2
unipredi$ensamblado_predi20<-(unipredi$avnnet+unipredi$svmPoly)/2
unipredi$ensamblado_predi21<-(unipredi$avnnet+unipredi$svmRadial)/2

unipredi$ensamblado_predi22<-(unipredi$bagging+unipredi$rf)/2
unipredi$ensamblado_predi23<-(unipredi$bagging+unipredi$gbm)/2
unipredi$ensamblado_predi24<-(unipredi$bagging+unipredi$xgbm)/2
unipredi$ensamblado_predi25<-(unipredi$bagging+unipredi$svmLinear)/2
unipredi$ensamblado_predi26<-(unipredi$bagging+unipredi$svmPoly)/2
unipredi$ensamblado_predi27<-(unipredi$bagging+unipredi$svmRadial)/2

unipredi$ensamblado_predi28<-(unipredi$rf+unipredi$gbm)/2
unipredi$ensamblado_predi29<-(unipredi$rf+unipredi$xgbm)/2
unipredi$ensamblado_predi30<-(unipredi$rf+unipredi$svmLinear)/2
unipredi$ensamblado_predi31<-(unipredi$rf+unipredi$svmPoly)/2
unipredi$ensamblado_predi32<-(unipredi$rf+unipredi$svmRadial)/2

unipredi$ensamblado_predi33<-(unipredi$gbm+unipredi$xgbm)/2
unipredi$ensamblado_predi34<-(unipredi$gbm+unipredi$svmLinear)/2
unipredi$ensamblado_predi35<-(unipredi$gbm+unipredi$svmPoly)/2
unipredi$ensamblado_predi36<-(unipredi$gbm+unipredi$svmRadial)/2

unipredi$ensamblado_predi37<-(unipredi$xgbm+unipredi$svmLinear)/2
unipredi$ensamblado_predi38<-(unipredi$xgbm+unipredi$svmPoly)/2
unipredi$ensamblado_predi39<-(unipredi$xgbm+unipredi$svmRadial)/2

unipredi$ensamblado_predi40<-(unipredi$svmLinear+unipredi$svmPoly)/2
unipredi$ensamblado_predi41<-(unipredi$svmLinear+unipredi$svmRadial)/2

unipredi$ensamblado_predi42<-(unipredi$svmPoly+unipredi$svmRadial)/2


unipredi$ensamblado_predi43<-(unipredi$logi+unipredi$avnnet+unipredi$bagging)/3
unipredi$ensamblado_predi44<-(unipredi$logi+unipredi$avnnet+unipredi$rf)/3
unipredi$ensamblado_predi45<-(unipredi$logi+unipredi$avnnet+unipredi$gbm)/3
unipredi$ensamblado_predi46<-(unipredi$logi+unipredi$avnnet+unipredi$xgbm)/3
unipredi$ensamblado_predi47<-(unipredi$logi+unipredi$avnnet+unipredi$svmLinear)/3
unipredi$ensamblado_predi48<-(unipredi$logi+unipredi$avnnet+unipredi$svmPoly)/3
unipredi$ensamblado_predi49<-(unipredi$logi+unipredi$avnnet+unipredi$svmRadial)/3
unipredi$ensamblado_predi50<-(unipredi$logi+unipredi$bagging+unipredi$rf)/3
unipredi$ensamblado_predi51<-(unipredi$logi+unipredi$bagging+unipredi$gbm)/3
unipredi$ensamblado_predi52<-(unipredi$logi+unipredi$bagging+unipredi$xgbm)/3
unipredi$ensamblado_predi53<-(unipredi$logi+unipredi$bagging+unipredi$svmLinear)/3
unipredi$ensamblado_predi54<-(unipredi$logi+unipredi$bagging+unipredi$svmPoly)/3
unipredi$ensamblado_predi55<-(unipredi$logi+unipredi$bagging+unipredi$svmRadial)/3
unipredi$ensamblado_predi56<-(unipredi$logi+unipredi$rf+unipredi$gbm)/3
unipredi$ensamblado_predi57<-(unipredi$logi+unipredi$rf+unipredi$xgbm)/3
unipredi$ensamblado_predi58<-(unipredi$logi+unipredi$rf+unipredi$svmLinear)/3
unipredi$ensamblado_predi59<-(unipredi$logi+unipredi$rf+unipredi$svmPoly)/3
unipredi$ensamblado_predi60<-(unipredi$logi+unipredi$rf+unipredi$svmRadial)/3
unipredi$ensamblado_predi61<-(unipredi$logi+unipredi$gbm+unipredi$xgbm)/3
unipredi$ensamblado_predi62<-(unipredi$logi+unipredi$gbm+unipredi$svmLinear)/3
unipredi$ensamblado_predi63<-(unipredi$logi+unipredi$gbm+unipredi$svmPoly)/3
unipredi$ensamblado_predi64<-(unipredi$logi+unipredi$gbm+unipredi$svmRadial)/3
unipredi$ensamblado_predi65<-(unipredi$logi+unipredi$xgbm+unipredi$svmLinear)/3
unipredi$ensamblado_predi66<-(unipredi$logi+unipredi$xgbm+unipredi$svmPoly)/3
unipredi$ensamblado_predi67<-(unipredi$logi+unipredi$xgbm+unipredi$svmRadial)/3
unipredi$ensamblado_predi68<-(unipredi$logi+unipredi$svmLinear+unipredi$svmPoly)/3
unipredi$ensamblado_predi69<-(unipredi$logi+unipredi$svmLinear+unipredi$svmRadial)/3
unipredi$ensamblado_predi70<-(unipredi$logi+unipredi$svmPoly+unipredi$svmRadial)/3

# Lo anterior, se puede hacer de manera similar con todos los algoritmos.
# Por simplicidad, a continuación, voy a realizarlo para algunos seleccionados.
unipredi$ensamblado_predi71<-(unipredi$avnnet+unipredi$rf+unipredi$gbm)/3
unipredi$ensamblado_predi71<-(unipredi$avnnet+unipredi$rf+unipredi$xgbm)/3
unipredi$ensamblado_predi71<-(unipredi$avnnet+unipredi$rf+unipredi$svmLinear)/3
unipredi$ensamblado_predi72<-(unipredi$avnnet+unipredi$rf+unipredi$svmPoly)/3
unipredi$ensamblado_predi73<-(unipredi$avnnet+unipredi$rf+unipredi$svmRadial)/3
unipredi$ensamblado_predi71<-(unipredi$avnnet+unipredi$gbm+unipredi$svmLinear)/3
unipredi$ensamblado_predi72<-(unipredi$avnnet+unipredi$gbm+unipredi$svmPoly)/3
unipredi$ensamblado_predi73<-(unipredi$avnnet+unipredi$gbm+unipredi$svmRadial)/3
unipredi$ensamblado_predi74<-(unipredi$avnnet+unipredi$xgbm+unipredi$svmLinear)/3
unipredi$ensamblado_predi75<-(unipredi$avnnet+unipredi$xgbm+unipredi$svmPoly)/3
unipredi$ensamblado_predi76<-(unipredi$avnnet+unipredi$xgbm+unipredi$svmRadial)/3

unipredi$ensamblado_predi77<-(unipredi$rf+unipredi$gbm+unipredi$svmLinear)/3
unipredi$ensamblado_predi78<-(unipredi$rf+unipredi$gbm+unipredi$svmPoly)/3
unipredi$ensamblado_predi79<-(unipredi$rf+unipredi$gbm+unipredi$svmRadial)/3
unipredi$ensamblado_predi80<-(unipredi$rf+unipredi$xgbm+unipredi$svmLinear)/3
unipredi$ensamblado_predi81<-(unipredi$rf+unipredi$xgbm+unipredi$svmPoly)/3
unipredi$ensamblado_predi82<-(unipredi$rf+unipredi$xgbm+unipredi$svmRadial)/3

unipredi$ensamblado_predi83<-(unipredi$logi+unipredi$rf+unipredi$gbm+unipredi$avnnet)/4
unipredi$ensamblado_predi84<-(unipredi$logi+unipredi$rf+unipredi$gbm+unipredi$svmLinear)/4
unipredi$ensamblado_predi85<-(unipredi$logi+unipredi$rf+unipredi$gbm+unipredi$svmPoly)/4
unipredi$ensamblado_predi86<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$avnnet)/4
unipredi$ensamblado_predi87<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$svmLinear)/4
unipredi$ensamblado_predi88<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$svmPoly)/4

unipredi$ensamblado_predi89<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$avnnet+unipredi$svmLinear)/5
unipredi$ensamblado_predi90<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$avnnet+unipredi$svmPoly)/5
unipredi$ensamblado_predi91<-(unipredi$logi+unipredi$rf+unipredi$xgbm+unipredi$avnnet+unipredi$svmRadial)/5



# 4) Procesado de los ensamblados
# Listado de modelos a considerar
dput(names(unipredi))

listado<-c("logi", "avnnet", "bagging",
           "rf","gbm",  "xgbm", "svmLinear",  "svmPoly", 
           "svmRadial","ensamblado_predi9", "ensamblado_predi10a", "ensamblado_predi10b", "ensamblado_predi11", "ensamblado_predi12", 
           "ensamblado_predi13", "ensamblado_predi14", "ensamblado_predi15", "ensamblado_predi16a", "ensamblado_predi16b", "ensamblado_predi17", "ensamblado_predi18", 
           "ensamblado_predi19", "ensamblado_predi20", "ensamblado_predi21", "ensamblado_predi22", "ensamblado_predi23", "ensamblado_predi24", 
           "ensamblado_predi25", "ensamblado_predi26", "ensamblado_predi27", "ensamblado_predi28", "ensamblado_predi29", "ensamblado_predi30", 
           "ensamblado_predi31", "ensamblado_predi32", "ensamblado_predi33", "ensamblado_predi34", "ensamblado_predi35", "ensamblado_predi36", 
           "ensamblado_predi37", "ensamblado_predi38", "ensamblado_predi39", "ensamblado_predi40", "ensamblado_predi41", "ensamblado_predi42", 
           "ensamblado_predi43", "ensamblado_predi44", "ensamblado_predi45", "ensamblado_predi46", "ensamblado_predi47", "ensamblado_predi48", 
           "ensamblado_predi49", "ensamblado_predi50", "ensamblado_predi51", "ensamblado_predi52", "ensamblado_predi53", "ensamblado_predi54", 
           "ensamblado_predi55", "ensamblado_predi56", "ensamblado_predi57", "ensamblado_predi58", "ensamblado_predi59", "ensamblado_predi60", 
           "ensamblado_predi61", "ensamblado_predi62", "ensamblado_predi63", "ensamblado_predi64", "ensamblado_predi65", "ensamblado_predi66", 
           "ensamblado_predi67", "ensamblado_predi68", "ensamblado_predi69", "ensamblado_predi70", "ensamblado_predi71", "ensamblado_predi72",
           "ensamblado_predi73", "ensamblado_predi74", "ensamblado_predi75", "ensamblado_predi76", "ensamblado_predi77", "ensamblado_predi78",
           "ensamblado_predi79", "ensamblado_predi80", "ensamblado_predi81", "ensamblado_predi82", "ensamblado_predi83", "ensamblado_predi84",
           "ensamblado_predi85", "ensamblado_predi86", "ensamblado_predi87", "ensamblado_predi88", "ensamblado_predi89", "ensamblado_predi90",
           "ensamblado_predi91")


# Se obtiene el numero de repeticiones CV y se calculan las medias por repeticion en el data frame medias0
repeticiones <- nlevels(factor(unipredi$Rep))
unipredi$Rep <- as.factor(unipredi$Rep)
unipredi$Rep <- as.numeric(unipredi$Rep)

medias0 <- data.frame(c())

for (prediccion in listado){
  unipredi$proba<-unipredi[,prediccion]
  unipredi[,prediccion]<-ifelse(unipredi[,prediccion]>0.5,"Yes","No")
  
  for (repe in 1:repeticiones){
    paso <- unipredi[(unipredi$Rep==repe),]
    pre<-factor(paso[,prediccion])
    archi<-paso[,c("proba","obs")]
    archi<-archi[order(archi$proba),]
    obs<-paso[,c("obs")]
    tasa=1-tasafallos(pre,obs)
    t<-as.data.frame(tasa)
    t$modelo<-prediccion
    auc<-auc(archi$obs,archi$proba)
    t$auc<-auc
    medias0<-rbind(medias0,t)
  }
}


# 5) Boxplot (resumen)

par(cex.axis=0.5, las=2)
boxplot(data=medias0, tasa~modelo, col="pink", main="Tasa de Fallos")
boxplot(data=medias0, auc~modelo, col="pink", main="AUC") # Me voy a fijar mas en esta metrica puesto que no requiere punto de corte
# Antes de sacar conclusiones, voy a ordenar el boxplot y representarlo también en formato tabla


# 6) Tabla ordenada y boxplot ordenado

# Tabla ordenada por Tasa de Error
tablamedias <- medias0 %>% group_by(modelo) %>% summarize(tasa=mean(tasa))     
tablamedias<-tablamedias[order(tablamedias$tasa),]
View(tablamedias)

# Boxplot ordenado por Tasa de Error
medias0$modelo <- with(medias0,reorder(modelo, tasa, mean))
par(cex.axis=0.7,las=2)
boxplot(data=medias0, tasa~modelo, col="pink", main='TASA FALLOS')


# Tabla ordenada por AUC
tablamedias2<-medias0 %>%group_by(modelo) %>%summarize(auc=mean(auc))     
tablamedias2<-tablamedias2[order(-tablamedias2$auc),]
View(tablamedias2)

# Boxplot ordenado por AUC
medias0$modelo <- with(medias0, reorder(modelo, auc, mean))
par(cex.axis=0.7, las=2)
boxplot(data=medias0, auc~modelo, col="pink", main='AUC')

# A modo resumen: en este caso, hay algunos modelos de Ensamblado que se comportan mejor que los modelos individualmente.
# De manera descendiente según la AUC, son:
# ensamblado_predi91 = avnnet+xgbm+svmRadial
# ensamblado_predi73 = avnnet+rf+svmRadial
# ensamblado_predi21 = avnnet+svmRadial
# ensamblado_predi39 = xgbm+svmRadial
# ensamblado_predi82 = rf+xgbm+svmRadial
# ensamblado_predi49 = logi+avnnet+svmRadial
# ensamblado_predi67 = logi+xgbm+svmRadial
# ensamblado_predi64 = logi+gbm+svmRadial
# ensamblado_predi36 = gbm+svmRadial

# Por lo que parece avnnet, xgbm y svmRadial en conjunto son los que mejores resultados dan. 
# Además, observando la tabla ordenada por AUC, los modelos que mejores resultados dan de manera individual son avnnet, svmRadial y xgbm (a partir de la posicion 37/94)

# La diferencia en AUC entre los modelos con AUC mas alta y avnnet (el primer modelo individual que aparece) es de 0.04 aprox,
# resultado que me hace pensar y elegir un modelo de ensamblado para prediccion antes que uno individual. 







########################################################
# Prediccion
#*******************************************************

# Necesitaríamos otro archivo con una muestra (test)

#datos_test <- read.table("datos_test.csv", header=TRUE, sep=",", strip.white=TRUE)
sample_size = floor(0.3*nrow(datos_test))

# Se crean los índices para train test
set.seed(12345)
indices = sample(seq_len(nrow(datos_test)),size = sample_size)

# Se crean los archivos train test
train = datos_test[indices,]
test = datos_test[-indices,]


# 1) Ya tenemos el mejor modelo resultante del ensamblado: ensamblado_predi91

# 2) Aplicamos el objeto modelo creado sobre datos test
# Lo mejor es crear un data frame con las probabilidades y la clase más probable según corte 0.5

prediccionestodas <- predict(ensamblado_predi91, test, type = "prob") %>% mutate('pred'=names(.)[apply(., 1, which.max)]) 
prediccionestodas

# Se obtienen 3 columnas
# - Las dos primeras son las probabilidades predichas
# - La tercera, la predicción con punto de corte 0.5


# 3) Medidas de performance sobre datos test
# Se utilizará la función confusionMatrix de caret y la función ROC para calcular el AUC

prediccionestodas$pred <- as.factor(prediccionestodas$pred)
test$vote <- as.factor(test$vote)

salconfu <- confusionMatrix(prediccionestodas$pred, test$vote)
salconfu

library(pROC)
curvaroc <- roc(response=test$vote, predictor=prediccionestodas$vote)
auc <- curvaroc$auc

plot(roc(response=test$vote, predictor=prediccionestodas$vote))









########################################################
# EXTRA: AutoML en h2o
#*******************************************************

h2o.init(nthreads=8) 
start_time <- Sys.time()

# Para h2o, lo primero es traducir el archivo a h2o
voteincome_final_h2o <- as.h2o(voteincome_final)

# Nota: en lugar de poner max_models=m, 
# se puede limitar el tiempo de proceso con max_runtime_secs = 60 por ejemplo

# Los modelos que prueba con AutoML por defecto son: 
# Redes (h20.deeplearning)
# Gbm (h20.gbm)
# Random forest (h20.drf)
# Logística o regresión, ambos LASSO (h20.glm)
# Ensamblado (stacking) de varios modelos

aml <- h2o.automl(x = 1:11, y=12, training_frame = voteincome_final_h2o, max_runtime_secs = 180, seed = 1,
                  keep_cross_validation_predictions=TRUE, nfolds=4)

lb <- aml@leaderboard
print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)

aml@leader

end_time <- Sys.time()
end_time - start_time

aml@leader

h2o.shutdown()





########################################################
#
# FIN
#
########################################################
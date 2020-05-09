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
setwd('/Users/pablodepaz/Google Drive/A Master/9. ML con R y Python/Tarea (18 de junio)/TrabajoML')

# Cargar funciones desde otros archivos .R (archivo R usado en otro curso)
source("FuncionesML_R.R")






# Librerias usadas
paquetes(c('nnet','dummies','MASS','reshape','caret','pROC', 
           'caret', 'ggplot2', 'plyr', 'naniar', 'rpart',
           'rpart.plot', 'rattle', 'dplyr', 'h2o', 'questionr', 
           'psych', 'outliers', 'dummies', 'corrplot', 'tidyverse',
           'parallel', 'doParallel'))



########################################################
# Cargar los datos, verificar y analizar el dataset
#*******************************************************
  
adult_train <- read.table("../Adult/adult.data", header=FALSE, sep=",", strip.white=TRUE)
adult_test <- read.table("../Adult/adult.test", header=FALSE, sep=",", strip.white=TRUE, skip=1)

colnames <- c('age', 'workclass', 'fnlwgt', 'education', 
              'education-num', 'marital-status', 'occupation',
              'relationship', 'race', 'sex', 'capital-gain', 
              'capital-loss', 'hours-per-week', 'native-country', 'income')

colnames(adult_train) <- colnames
colnames(adult_test) <- colnames

str(adult_train)
str(adult_test)
# 1) "native-country" tiene diferente numero de niveles en el factor. 
# No hay problema ya que es casualidad que en adult_train hay un pais que no aparece en adult_test.
# 2) "income" tiene un error de codificación en los niveles del factor en adult_test.

# Corrijo el error de codificación de "income" en adult_test
levels(adult_test$income)[levels(adult_test$income)=="<=50K."] <- "<=50K"
levels(adult_test$income)[levels(adult_test$income)==">50K."] <- ">50K"

# Junto en un unico dataset todos los datos que tengo
adult <- rbind(adult_train, adult_test) # Este dataframe es el que usaré posteriormente
str(adult)

listconti <- c("age", "fnlwgt", "education-num", "capital-gain",
               "capital-loss",  "hours-per-week")
listclass <- c('workclass', 'education', 'marital-status', 'occupation',
               'relationship', 'race', 'sex', 'native-country')
vardep <- c("income")


# Inspección visual de los datos
summary(adult)

hist(adult$`hours-per-week`) # Para variables numéricas de manera individual
psych::describe(Filter(is.numeric, adult)) # Para variables numéricas
questionr::freq(adult$`native-country`) # Para variables categóricas (factores)
levels(adult$`native-country`) # Para variables categóricas (factores)

ggplot(as.data.frame(adult$income), aes(x = adult$income)) + geom_bar(fill = "#0073C2FF") # Frecuencia de "income"


par(mfrow=c(2,2))
dfplot(as.data.frame(adult))
# Conclusiones:
# workclass, ocupation, native-country --> es un factor y tiene un valor "?" (error)
# fnlwgt, capital-gain, capital-loss --> viendo la distribución y atentiendo a la mediana, el tercer cuartil y kurtosis, tiene posibles outliers
# capital-gain --> tiene además un posible valor mal codificado (99999)
# education, occupation, native-country --> es un factor y algunos valores se podrían agrupar para tener menos niveles
# native-country dudo de si merece la pena que sea un factor...
# income (variable "y") no está balanceada (76.1% y 23.9%). Puede haber sesgo...


# Variable "income" (var. dependiente binaria) se transforma a "0" (<=50K) and "1" (>50K)
adult$income <- lapply(adult$income, as.character)
adult$income <- replace(adult$income, which(adult$income=='<=50K'), '0')
adult$income <- replace(adult$income, which(adult$income=='>50K'), '1')
adult$income <- as.numeric(adult$income)


# Correlacion entre variables
par(mfrow=c(1,1))
corrplot(cor(cbind(adult$income, 
                   Filter(is.numeric, adult)), use="pairwise", method="pearson"), method = "ellipse",type = "upper")
# Hay una cierta linearidad directa de "education-num" con la variable objetivo "income".




########################################################
# Correción de errores. Tratamiento de outliers y missings
#*******************************************************

# Guardo en una variable el estado actual de "adult" como Backup
adult_backup1 <- adult
#adult <- adult_backup1



# Valor mal codificado. Si pongo NA, luego obtengo errores. 
# Para facilitar el trabajo, pongo un valor más bajo y donde ya hay outliers
adult$`capital-gain` <- replace(adult$`capital-gain`, which(adult$`capital-gain`==99999), 30000) 

# Missings no declarados en variables cualitativas (?)
adult$workclass <- recode.na(adult$workclass,"?")
adult$occupation <- recode.na(adult$occupation,"?")
adult$`native-country` <- recode.na(adult$`native-country`,"?")


# Antes de continuar, me doy cuenta de que "education" y "education-num" representan lo mismo 
# pero una variable en forma nominal y la otra numérica (grado educativo de EE.UU.).
# Más info sobre los grados educativos: https://tuhijoalextranjero.com/wp-content/uploads/2018/04/usaSystem.png 
adult$`education-num` <- NULL
colnames <- c('age', 'workclass', 'fnlwgt', 'education', 
              'marital-status', 'occupation', 'relationship', 
              'race', 'sex', 'capital-gain', 'capital-loss', 
              'hours-per-week', 'native-country', 'income')
listconti <- c("age", "fnlwgt", "capital-gain", "capital-loss",  "hours-per-week")

# Voy a comprobar otra vez la correlacion...
par(mfrow=c(1,1))
corrplot(cor(cbind(adult$income, 
                   Filter(is.numeric, adult)), use="pairwise", method="pearson"), method = "ellipse",type = "upper")


# Agrupar algunos niveles de factores con muchos niveles
levels(adult$workclass)
par(mfrow=c(1,1))
dfplot(as.data.frame(adult$workclass))
adult$workclass <- revalue(adult$workclass, c("Without-pay"="Not-Working", "Never-worked"="Not-Working"))
adult$workclass <- revalue(adult$workclass, c("Self-emp-inc"="Self-employed", "Self-emp-not-inc"="Self-employed"))
adult$workclass <- revalue(adult$workclass, c("Federal-gov"="Government", "Local-gov"="Government", "State-gov"="Government"))
dfplot(as.data.frame(adult$workclass))

levels(adult$education)
par(mfrow=c(1,1))
dfplot(as.data.frame(adult$education))
adult$education <- revalue(adult$education, c("Preschool"="Elem-Middle-School", "1st-4th"="Elem-Middle-School", 
                                              "5th-6th"="Elem-Middle-School", "7th-8th"="Elem-Middle-School"))
adult$education <- revalue(adult$education, c("9th"="High-School", "10th"="High-School", "11th"="High-School", 
                                              "12th"="High-School"))
adult$education <- revalue(adult$education, c("Assoc-acdm"="Associates", "Assoc-voc"="Associates"))
adult$education <- revalue(adult$education, c("HS-grad"="HS-Graduate", "Some-college"="HS-Graduate"))
dfplot(as.data.frame(adult$education))

levels(adult$occupation)
dfplot(as.data.frame(adult$occupation))
sum(adult$occupation=='Armed-Forces') # 15 entries
adult$occupation <- revalue(adult$occupation, c("Armed-Forces"="Services", "Other-service"="Services",
                                                "Priv-house-serv"="Services", "Protective-serv"="Services"))
adult$occupation <- revalue(adult$occupation, c("Craft-repair"="Blue-Collar", "Farming-fishing"="Blue-Collar",
                                                "Handlers-cleaners"="Blue-Collar", "Machine-op-inspct"="Blue-Collar",
                                                "Transport-moving"="Blue-Collar")) # Ejecutantes de tareas manuales y obreros
dfplot(as.data.frame(adult$occupation))

levels(adult$`native-country`)
questionr::freq(adult$`native-country`) # Agrupacion de paises por similar zona geografica y nivel socioeconomico
adult$`native-country` <- revalue(adult$`native-country`, c("Cambodia"="SE-Asia", "Thailand"="SE-Asia", 
                                                            "Vietnam"="SE-Asia", "Laos"="SE-Asia", "Philippines"="SE-Asia"))
adult$`native-country` <- revalue(adult$`native-country`, c("China"="N-Asia", "Hong"="N-Asia", "Japan"="N-Asia", 
                                                            "South"="N-Asia", "Taiwan"="N-Asia"))
adult$`native-country` <- revalue(adult$`native-country`, c("India"="Middle-East", "Iran"="Middle-East"))
adult$`native-country` <- revalue(adult$`native-country`, c("Columbia"="South-America", "Ecuador"="South-America", 
                                                            "Peru"="South-America", "Trinadad&Tobago"="South-America"))
adult$`native-country` <- revalue(adult$`native-country`, c("Cuba"="Central-America", "Dominican-Republic"="Central-America", 
                                                            "El-Salvador"="Central-America", "Guatemala"="Central-America", 
                                                            "Haiti"="Central-America", "Honduras"="Central-America", 
                                                            "Jamaica"="Central-America", "Nicaragua"="Central-America", "Puerto-Rico"="Central-America"))
adult$`native-country` <- revalue(adult$`native-country`, c("Canada"="North-America", "Mexico"="North-America", "Outlying-US(Guam-USVI-etc)"="North-America"))
adult$`native-country` <- revalue(adult$`native-country`, c("England"="Western-Europe", "France"="Western-Europe", "Germany"="Western-Europe",
                                                            "Holand-Netherlands"="Western-Europe", "Ireland"="Western-Europe", 
                                                            "Italy"="Western-Europe", "Portugal"="Western-Europe", "Scotland"="Western-Europe"))
adult$`native-country` <- revalue(adult$`native-country`, c("Greece"="Eastern-Europe", "Hungary"="Eastern-Europe", 
                                                            "Poland"="Eastern-Europe", "Yugoslavia"="Eastern-Europe"))
dfplot(as.data.frame(adult$`native-country`))





#par(mfrow=c(2,2))
#dfplot(as.data.frame(adult))
#par(mfrow=c(1,1))



# Tratamiento de outliers
# Antes de nada, transformo las variables cuantitativas de integer a numeric
adult$age <- as.numeric(adult$age)
adult$fnlwgt <- as.numeric(adult$fnlwgt)
adult$`capital-gain` <- as.numeric(adult$`capital-gain`)
adult$`capital-loss` <- as.numeric(adult$`capital-loss`)
adult$`hours-per-week` <- as.numeric(adult$`hours-per-week`)

#sapply(Filter(is.numeric, adult),function(x) atipicosAmissing(x)[[2]])/nrow(adult) # Proporcion de outliers en cada variable

# Visualizar los outliers en boxplots
par(mfrow=c(2,3))
boxplot(adult$age, horizontal=TRUE, main="age", boxwex=0.1)
boxplot(adult$fnlwgt, horizontal=TRUE, main="fnlwgt", boxwex=0.1)
boxplot(adult$`capital-gain`, horizontal=TRUE, main="capital-gain", boxwex=0.1)
boxplot(adult$`capital-loss`, horizontal=TRUE, main="capital-loss", boxwex=0.1)
boxplot(adult$`hours-per-week`, horizontal=TRUE, main="hours-per-week", boxwex=0.1)

# NO usaré el método comentado aqui debajo ya que es demasiado estricto con valores que puedan superar un poco los bigotes del boxplot.
# Me basaré, más abajo, en la definición de Z-Score: https://www.statisticshowto.com/probability-and-statistics/z-score/
#outlier_values_1 <- boxplot.stats(adult$fnlwgt)$out
#index_outliers_1 <- which(adult$fnlwgt %in% outlier_values_1)

# Remplazar outliers por NAs (he elegido esta estrategia y no eliminar filas)
adult[obtenerOutliersIndex(adult$age), "age"] <- NA
adult[obtenerOutliersIndex(adult$fnlwgt), "fnlwgt"] <- NA
adult[obtenerOutliersIndex(adult$`capital-gain`), "capital-gain"] <- NA
adult[obtenerOutliersIndex(adult$`capital-loss`), "capital-loss"] <- NA
adult[obtenerOutliersIndex(adult$`hours-per-week`), "hours-per-week"] <- NA

summary(select_if(adult, is.numeric))

#length(is_outlier_age[is_outlier_age==TRUE])/nrow(adult)
#table(adult$`capital-gain`[adult$`capital-gain`<1])

# Se podría verificar aquí con los boxplots anteriores...



# Tratamiento de missings

# Llevo a cabo la estrategia de imputación de NAs con valores estadísticos de la media/mediana.
# Hay pocos NAs en comparación con el total de los datos.
apply(is.na(adult), 2, sum); print(""); apply(is.na(adult), 2, mean) # Missings por variable 

adult[,as.vector(which(sapply(adult, class)=="numeric"))] <- 
  sapply(Filter(is.numeric, adult),function(x) ImputacionCuant(x,"media")) # Imputacion variables cuantitativas por la media
# Todas las 6 variables numericas a excepcion de "capital-gain" y "capital-loss" tienen una media muy parecida a la mediana

adult[,as.vector(which(sapply(adult, class)=="factor"))] <- 
  sapply(Filter(is.factor, adult),function(x) ImputacionCuali(x,"aleatorio")) # Imputacion variables cualitativas (aleatoriamente)

adult[,as.vector(which(sapply(adult, class)=="character"))] <- 
  lapply(adult[,as.vector(which(sapply(adult, class)=="character"))] , factor) # Cambiar clase a "factor" porque a veces se cambia a "character"

apply(is.na(adult), 2, sum) # Missings por variable
# Si es necesario, volver a correr la linea de imputacion variables cuantitativas

str(adult)
summary(adult)




########################################################
# Normalizacion (var. continuas) y creacion de dummies (var. categoricas)
#*******************************************************

# Vuelvo a hacer un backup
adult_backup2 <- adult
#adult <- adult_backup2


# Obtener dummies
if (listclass != c("")){
  adult_dummy <- adult[,c(vardep,listconti,listclass)]
  adult_dummy <- dummy.data.frame(adult_dummy, listclass, sep = ".")
} else {
  adult_dummy <- adult[,c(vardep,listconti)]
}

# Escalar
means <- apply(adult_dummy[,listconti], 2, mean)
sds <- sapply(adult_dummy[,listconti], sd)

adult_dummy_bis <- scale(adult_dummy[,listconti], center = means, scale = sds)

numerocont <- which(colnames(adult_dummy) %in% listconti)
adult_dummy_sc <- cbind(adult_dummy_bis,adult_dummy[, -numerocont, drop=FALSE])

adult_dummy_sc[7:54] <- lapply(adult_dummy_sc[7:54], as.numeric)

# Elimino variables auxiliares antes de continuar
rm(adult_dummy); rm(adult_dummy_bis); rm(means); rm(sds); rm(numerocont)



########################################################
# Variables más importantes y selección de variables
#*******************************************************

# Backup
adult_backup3 <- adult_dummy_sc
#adult_dummy_sc <- adult_backup3

# Antes de continuar, creo 2 variables aleatorias que me servirán como "variables de control"
adult_dummy_sc$aleatorio <- runif(nrow(adult_dummy_sc))
adult_dummy_sc$aleatorio2 <- runif(nrow(adult_dummy_sc))

# Actualizo los nombres de las columnas y las variables continuas (incl. dummies)
colnames <- colnames(adult_dummy_sc)
listconti <- colnames(adult_dummy_sc[,-6])

#par(mfrow=c(1,1))
#graficoVcramer(as.data.frame(adult_dummy_sc[,-6]), adult_dummy_sc$income)

# Seleccion de variables clasica (stepwise)
full<-lm(income~., data=adult_dummy_sc)
null<-lm(income~1, data=adult_dummy_sc)

seleccion1_AIC <- step(null, scope=list(lower=null, upper=full), direction="both", trace=FALSE) # Tarda 2-3 minutos y no muestra trazas
summary(seleccion1_AIC)
seleccion1_BIC <- step(null, scope=list(lower=null, upper=full), direction="both", k=log(nrow(adult_dummy_sc)), trace=FALSE) # Tarda 1-2 minutos y no muestra trazas
summary(seleccion1_BIC)

seleccion1_AIC$rank # 33
seleccion1_BIC$rank # 26

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

lista_AIC <- steprepetidobinaria(data=adult_dummy_sc, vardep=c("income"), listconti=listconti,
                                 sinicio=12345, sfinal=12350, porcen=0.8, criterio="AIC") # Tarda +15 min/semilla

tabla_AIC <- lista_AIC[[1]]
dput(lista_AIC[[2]][[1]])
dput(lista_AIC[[2]][[2]])
# ...
dput(lista_AIC[[2]][[6]])

  
lista_BIC <- steprepetidobinaria(data=adult_dummy_sc, vardep=c("income"), listconti=listconti,
                                 sinicio=12345, sfinal=12350, porcen=0.8, criterio="BIC") # Tarda +12 min/semilla

tabla_BIC <- lista_BIC[[1]]
dput(lista_BIC[[2]][[1]])
dput(lista_BIC[[2]][[2]])
dput(lista_BIC[[2]][[3]])
dput(lista_BIC[[2]][[4]])



# Diferentes "sets" de variables
set1 <- dput(names(seleccion1_AIC$coefficients))[-1] #32 variables
set2 <- dput(names(seleccion1_BIC$coefficients))[-1] #25
source("variables_sets.R") # Para el resto de sets (por limpieza de codigo)


# Una vez se tienen las listas de variables ("sets" anteriores), comparar con Regresion Logistica para la selección de variables definitiva.

# Podemos probar validación cruzada repetida para comparar vía sesgo-varianza
source("cruzadas avnnet y log binaria.R")

adult_dummy_sc$income <- as.factor(adult_dummy_sc$income)
adult_dummy_sc$income <- revalue(adult_dummy_sc$income, c("0"="No", "1"="Yes")) # Rename factor levels


start_time <- Sys.time()

medias1 <- cruzadalogistica(data=adult_dummy_sc, vardep=vardep, listconti=set1, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias1$modelo= "modelo1"

medias2 <- cruzadalogistica(data=adult_dummy_sc, vardep=vardep, listconti=set2, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias2$modelo="modelo2"

medias3 <- cruzadalogistica(data=adult_dummy_sc, vardep=vardep, listconti=set3, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias3$modelo="modelo3"

medias4 <- cruzadalogistica(data=adult_dummy_sc, vardep=vardep, listconti=set4, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias4$modelo="modelo4"

medias5 <- cruzadalogistica(data=adult_dummy_sc, vardep=vardep, listconti=set5, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias5$modelo="modelo5"

medias6 <- cruzadalogistica(data=adult_dummy_sc, vardep=vardep, listconti=set6, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias6$modelo="modelo6"

medias7 <- cruzadalogistica(data=adult_dummy_sc, vardep=vardep, listconti=set7, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias7$modelo="modelo7"

medias8 <- cruzadalogistica(data=adult_dummy_sc, vardep=vardep, listconti=set8, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias8$modelo="modelo8"

medias9 <- cruzadalogistica(data=adult_dummy_sc, vardep=vardep, listconti=set9, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias9$modelo="modelo9"

medias10 <- cruzadalogistica(data=adult_dummy_sc, vardep=vardep, listconti=set10, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias10$modelo="modelo10"

medias11 <- cruzadalogistica(data=adult_dummy_sc, vardep=vardep, listconti=set11, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias11$modelo="modelo11"

medias12 <- cruzadalogistica(data=adult_dummy_sc, vardep=vardep, listconti=set12, listclass=c(""), grupos=4, sinicio=1234, repe=30)
medias12$modelo="modelo12"

end_time <- Sys.time()
print(end_time - start_time) # 26 mins en total

rm(start_time); rm(end_time)

union <- rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8,medias9,medias10,medias11,medias12)

par(cex.axis=0.5)
boxplot(data=union, col="pink", tasa~modelo, main="Tasa de Fallos")
boxplot(data=union, col="pink", auc~modelo, main="AUC")

union_fallos <- rbind(medias1, medias3, medias4, medias7)
boxplot(data=union_fallos, col="pink", tasa~modelo, main="Tasa de Fallos (modelos seleccionados)")

union_auc <- rbind(medias3, medias4, medias5, medias7, medias8)
boxplot(data=union_auc, col="pink", auc~modelo, main="AUC (modelos seleccionados)")

# La difernecia entre la tasa de fallos y AUC para los distintos sets de variables es muy pequeña.
# Aún así, teniendo en cuenta los boxplots anteriores y, a través de un "Estudio sesgo-varianza".
# - modelo1 y modelo4 tienen el mejor comportamiento en términos de fallos.
# - modelo3 y modelo4 tienen el mejor comportamiento en términos de AUC.
# Por otro lado, considero el número de variables de cada set (ronda entre 26 y 34).
# Con todo ello, elegiré el set de variables includas en el modelo4. 


set4_1 <- gsub(pattern = '`', x = set4, replacement = "") # 33

adult_final_select <- select(adult_dummy_sc, set4_1)
adult_final_select <- cbind(adult_final_select, adult_dummy_sc$income) # 34 variables en total
names(adult_final_select)[names(adult_final_select) == "adult_dummy_sc$income"] <- "income"

# Backup
adult_backup4 <- adult_final_select
#adult_final_select <- adult_backup4

formula <- "income ~ `marital-status.Married-civ-spouse` + `occupation.Blue-Collar` + `capital-gain` + `hours-per-week` + occupation.Services + age + `education.HS-Graduate` + `education.High School` +`education.Elem-Middle-School` + education.Associates + `relationship.Own-child` + education.Bachelors + `occupation.Exec-managerial` + education.Masters + `relationship.Other-relative` + `occupation.Adm-clerical` + relationship.Wife + sex.Female + `workclass.Self-employed` + `marital-status.Never-married` + fnlwgt + `marital-status.Married-AF-spouse` + `native-country.United-States` + occupation.Sales + `relationship.Not-in-family` + `native-country.Western-Europe` + `capital-loss` + race.Black + `native-country.South-America` + `race.Amer-Indian-Eskimo` + `native-country.SE-Asia` +  `native-country.Eastern-Europe` + `workclass.Not-Working`"


# Antes de comenzar a evaluar algoritmos y teniendo en cuenta la existente limitacion de hardware y de tiempo,
# usare una de las "estrategias avanzadas para tratar con grandes datos, tiempo y recursos limitados" estudiadas.
# Voy a usar para el entrenamiento una parte de mis datos: 25.000 obsevaciones, que garantiza que "# observaciones/# variables" > 500
# Usaré Muestreo Aleatorio Estratificado

prop.table(table(adult_final_select$income))

adult_final_use <- adult_final_select %>%
  group_by(income) %>%
  sample_frac(0.5)

table(adult_final_use$income)
prop.table(table(adult_final_use$income))

adult_final_use <- as.data.frame(adult_final_use)

# Exporto este dataframe creado para evaluar algoritmos
write.csv(adult_final_use,"adult_final_use.csv", row.names = TRUE)






########################################################
#
# CONTINUA EN OTRO ARCHIVO .R
#
########################################################
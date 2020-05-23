# Funcion que carga paquetes y/o los instala
paquetes <- function(x){ 
  for( i in x ){
    if( ! require( i , character.only = TRUE ) ){ 
      # Instala el paquete
      install.packages( i , dependencies = TRUE ) 
      # Lo carga
      require( i , character.only = TRUE )
    }
  } 
}

# Graficos de inspeccion rapida de un dataset. 
dfplot <- function(data.frame){
  df <- data.frame
  ln <- length(names(data.frame))
  for(i in 1:ln){
    if(is.factor(df[,i])){
      plot(df[,i],main=names(df)[i])}
    else{hist(df[,i],main=names(df)[i])
	boxplot(df[,i],main=names(df)[i])}
  }
}



# Cuenta el numero de atipicos y los transforma en missings
atipicosAmissing<-function(varaux){
  if (abs(skew(varaux))<1){
    criterio1<-abs((varaux-mean(varaux,na.rm=T))/sd(varaux,na.rm=T))>3
  } else {
    criterio1<-abs((varaux-median(varaux,na.rm=T))/mad(varaux,na.rm=T))>8
  }
  qnt <- quantile(varaux, probs=c(.25, .75), na.rm = T)
  H <- 3 * IQR(varaux, na.rm = T)
  criterio2<-(varaux<(qnt[1] - H))|(varaux>(qnt[2] + H))
  varaux[criterio1&criterio2]<-NA
  return(list(varaux,sum(criterio1&criterio2,na.rm=T)))
}


# Imputacion variables cuantitativas
ImputacionCuant<-function(vv,tipo){#tipo debe tomar los valores media, mediana o aleatorio
  if (tipo=="media"){
    vv[is.na(vv)]<-round(mean(vv,na.rm=T),4)
  } else if (tipo=="mediana"){
    vv[is.na(vv)]<-round(median(vv,na.rm=T),4)
  } else if (tipo=="aleatorio"){
    dd<-density(vv,na.rm=T,from=min(vv,na.rm = T),to=max(vv,na.rm = T))
    vv[is.na(vv)]<-round(approx(cumsum(dd$y)/sum(dd$y),dd$x,runif(sum(is.na(vv))))$y,4)
  }
  vv
}

# Imputacion variables cualitativas
ImputacionCuali<-function(vv,tipo){#tipo debe tomar los valores moda o aleatorio
  if (tipo=="moda"){
    vv[is.na(vv)]<-names(sort(table(vv),decreasing = T))[1]
  } else if (tipo=="aleatorio"){
    vv[is.na(vv)]<-sample(vv[!is.na(vv)],sum(is.na(vv)),replace = T)
  }
  factor(vv)
}


# Calcula el V de Cramer
Vcramer<-function(v,target){
  if (is.numeric(v)){
    v<-cut(v,5)
  }
  if (is.numeric(target)){
    target<-cut(target,5)
  }
  cramer.v(table(v,target))
}


# Grafico con el V de cramer de todas las variables input para saber su importancia
graficoVcramer<-function(matriz, target){
  salidaVcramer<-sapply(matriz,function(x) Vcramer(x,target))
  barplot(sort(salidaVcramer,decreasing =T),las=2,ylim=c(0,1))
}


# Devuelve el indice de los outliers en una columna
obtenerOutliersIndex <- function(column){
  outlier_scores <- scores(column)
  is_outlier <- outlier_scores > 3 | outlier_scores < -3
  return(is_outlier) # Logic column (TRUE/FALSE)
}


# Calcula la Tasa de Fallos (ensamblado)
tasafallos<-function(x,y) {
  confu<-confusionMatrix(x,y)
  tasa<-confu[[3]][1]
  return(tasa)
}

# Calcula la AUC (ensamblado)
auc<-function(x,y) {
  curvaroc<-roc(response=x,predictor=y)
  auc<-curvaroc$auc
  return(auc)
}

## Función Objetivo
fun_obj<-function(ind,matriz){
  n<-length(ind)
  func<-0
  for(i in 1:(n-1)){
    func<-func+matriz[ind[i],ind[i+1]]
  }
  func<-func+matriz[ind[1],ind[n]]
  return(func)
}


# Selección torneo

torneo<-function(FunObj,muestra){
  tp<-length(FunObj) 
  Pos_Padres<-sample(tp,muestra)
  FunObjPadres<-FunObj[Pos_Padres]
  Pos_Padre_Gana<-which(FunObjPadres==min(FunObjPadres))
  
  # Los padres pueden tener la misma función objetivo
  N_Pos_Padre_Gana<-length(Pos_Padre_Gana)
  if (N_Pos_Padre_Gana > 1){
    selc<-sample(N_Pos_Padre_Gana,1)
    Pos_Padre_Gana<-Pos_Padre_Gana[selc]
  }else{
    Pos_Padre_Gana<-Pos_Padres[Pos_Padre_Gana]
  }
  
  return(Pos_Padre_Gana)
}

# cruzamiento, se intercambia material.

pmx<-function(ind1,ind2){
  n<-length(ind1)
  al1<-sample(n,1)
  al2<-sample(n,1)
  
  h1<-ind1
  h1[al1:al2]<-0
  h1[h1==0]<-ind2[!ind2 %in% h1]
  
  h2<-ind2
  h2[al1:al2]<-0
  h2[h2==0]<-ind1[!ind1 %in% h2]
  
  h12<-rbind(h1,h2)
  return(h12)
}

# Mutación
opt2<-function(ind){
  n<-length(ind)
  al1<-sample(n,1)
  al2<-sample(n,1)

  if (al1 > al2){
    a<-al1
    al1<-al2
    al2<-a
  }
  mut<-ind[al2:al1]
  ind[al1:al2]<-0
  ind[ind==0]<-mut
  return(ind)
}

# Voraz

greedy<-function(matriz){
  n<-ncol(matriz) # Cantidad de ciudades. 
  inicio<-sample(n,1)
  visitadas<-vector(length = 20)
  visitadas[1]<-inicio
  distancias<-matriz[inicio,]
  distancias[inicio]<-Inf
  for ( i in 2:(n)){
  posmindis<-which(distancias==min(distancias))
  lenposmindis<-length(posmindis)
    if(lenposmindis>1){
      posmindis<-posmindis[sample(lenposmindis,1)]
    }
      visitadas[i]<-posmindis
      distancias<-matriz[visitadas[i],]
      distancias[visitadas[1:i]]<-Inf
}
  return(visitadas)
}






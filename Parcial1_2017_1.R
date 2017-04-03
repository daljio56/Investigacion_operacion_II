#Tipo 1
restricciones<-matrix(0,8,8)
#sala 1
restricciones[1,1]<-1
#sala 2
restricciones[2,2]<-1
#sala 3
restricciones[3,1]<-1
restricciones[3,3]<-1
#sala 4
restricciones[4,3]<-1
restricciones[4,4]<-1
#sala 5
restricciones[5,2]<-1
restricciones[5,4]<-1
restricciones[5,5]<-1
restricciones[5,6]<-1
#sala 6
restricciones[6,7]<-1
#sala 7
restricciones[7,7]<-1
restricciones[7,8]<-1
#sala 8
restricciones[8,5]<-1
restricciones[8,8]<-1

funcion<-rep(1,8)
recurso<-rep(1,8)
dir<-rep(">=",8)

solucion<-lp("min", funcion, restricciones, dir, recurso)
solucion$objval
solucion$solution


#Tipo 2
restricciones<-matrix(0,8,8)
#sala 1
restricciones[1,1]<-1
#sala 2
restricciones[2,2]<-1
#sala 3
restricciones[3,1]<-1
restricciones[3,3]<-1
#sala 4
restricciones[4,3]<-1
restricciones[4,4]<-1
#sala 5
restricciones[5,2]<-1
restricciones[5,4]<-1
restricciones[5,5]<-1
restricciones[5,6]<-1
#sala 6
restricciones[6,7]<-1
#sala 7
restricciones[7,7]<-1
restricciones[7,8]<-1
#sala 8
restricciones[8,5]<-1
restricciones[8,8]<-1

funcion<-rep(1,8)
recurso<-rep(1,8)
dir<-rep(">=",8)

solucion<-lp("min", funcion, restricciones, dir, recurso)
solucion$objval
solucion$solution

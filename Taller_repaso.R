# Taller clase 8
# Punto 1


library(lpSolve)
# la lógica que se uso es que si se tiene una 
f.obj <- c(1,1,1,1,1,1,0,0,0,0,0,0)
Y_1<-c(1,1,0,0,0,0,-1,0,0,0,0,0)
Y_2<-c(1,1,0,0,0,1,0,-1,0,0,0,0)
Y_3<-c(0,0,1,1,0,0,0,0,-1,0,0,0)
Y_4<-c(0,0,1,1,1,1,0,0,0,-1,0,0)
Y_5<-c(0,0,0,1,1,1,0,0,0,0,-1,0)
Y_6<-c(0,1,0,1,1,1,0,0,0,0,0,-1)
Y_total<-c(0,0,0,0,0,0,1,1,1,1,1,1)

f.con<-as.matrix(rbind(Y_1,Y_2,Y_3,Y_4,Y_5,Y_6,Y_total))


f.dir <-c(rep(">=",6),"=") 
f.rhs <- c(0,0,0,0,0,0,6)

solucion<-lp("min", f.obj, f.con, f.dir, f.rhs, all.bin=T)
solucion$objval
solucion$solution

##  Subproblema 5

## 11.4-5
library(lpSolve)

f.obj <- c(3,6,6,5,4,3,3,2)
mutua_1<-c(1,1,0,0,0,0,0,0)
mutua_2<-c(0,0,1,1,1,1,0,0)
mutua_3<-c(0,0,0,0,0,0,1,1)
cont_medio_1<-c(-1,0,1,0,0,0,0,0)
cont_medio_2<-c(-1,0,0,1,0,0,0,0)
cont_medio_3<-c(0,-1,0,0,1,0,0,0)
cont_medio_4<-c(0,-1,0,0,0,1,0,0)
cont_final_1<-c(0,0,-1,0,-1,0,1,0)
cont_final_2<-c(0,0,0,-1,0,-1,0,1)
f.con<-as.matrix(rbind(mutua_1,mutua_2,mutua_3,cont_medio_1,cont_medio_2,cont_medio_3,cont_medio_4,cont_final_1,cont_final_2))


f.dir <- c("=","=","=","<=","<=","<=","<=","<=","<=")
f.rhs <- c(1,1,1,0,0,0,0,0,0)

solucion<-lp("min", f.obj, f.con, f.dir, f.rhs, all.bin=T)
solucion$objval
solucion$solution

#david

##  Subproblema 5

## 11.4-5
library(lpSolve)

f.obj <- c(3,6,6,5,4,3,3,2)


cont_inicio_1<-c(1,1,0,0,0,0,0,0)
cont_medio_2<-c(-1,0,1,1,0,0,0,0)
cont_medio_3<-c(0,-1,0,0,1,1,0,0)

cont_final_1<-c(0,0,1,0,1,0,-1,0)
cont_final_2<-c(0,0,0,1,0,1,0,-1)
f.con<-as.matrix(rbind(cont_inicio_1,cont_medio_2,cont_medio_3,cont_final_1,cont_final_2))


#f.dir <- c("=","=","=","<=","<=","<=","<=","<=","<=")
f.dir <- rep("=",5)
f.rhs <- c(1,0,0,0,0)

solucion<-lp("min", f.obj, f.con, f.dir, f.rhs, all.bin=T)
solucion$objval
solucion$solution


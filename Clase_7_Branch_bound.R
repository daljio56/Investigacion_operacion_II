library(lpSolve)
f.obj <- c(8,5)
f.con<-matrix(c(1,9,1,5),2) 
f.dir <- rep("<=", 2)
f.rhs <- c(6,45)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution





##  Subproblema 1

f.obj <- c(8,5)
f.con<-matrix(c(1,9,1,1,5,0),3) 
f.dir <- c("<=","<=",">=")
f.rhs <- c(6,45,4)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution

##  Subproblema 2

f.obj <- c(8,5)
f.con<-matrix(c(1,9,1,1,5,0),3) 
f.dir <- c("<=","<=","<=")
f.rhs <- c(6,45,3)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution

#incumbente

##  Subproblema 3

f.obj <- c(8,5)
f.con<-matrix(c(1,9,1,0,1,5,0,1),4) 
f.dir <- c("<=","<=",">=",">=")
f.rhs <- c(6,45,4,2)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution

#infactible

##  Subproblema 4

f.obj <- c(8,5)
f.con<-matrix(c(1,9,1,0,1,5,0,1),4) 
f.dir <- c("<=","<=",">=","<=")
f.rhs <- c(6,45,4,1)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution

##  Subproblema 5

f.obj <- c(8,5)
f.con<-matrix(c(1,9,1,0,1,5,0,1),4) 
f.dir <- c("<=","<=",">=","<=")
f.rhs <- c(6,45,5,1)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution


##  Subproblema 5

f.obj <- c(8,5)
f.con<-matrix(c(1,9,1,0,1,5,0,1),4) 
f.dir <- c("<=","<=","<=","<=")
f.rhs <- c(6,45,4,1)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution


#####################################################################
#####################################################################
######                                                     ##########
######                  Branch and cut                     ##########
######                                                     ##########
#####################################################################
#####################################################################

library(lpSolve)
f.obj <- c(2,1)
f.con<-matrix(c(1,-1,6,1,1,2),3) 
f.dir <- rep("<=", 3)
f.rhs <- c(5,0,21)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution

##segunda iteracion

library(lpSolve)
f.obj <- c(2,1)
f.con<-matrix(c(1,-1,6,1,1,2),3) 
#branch and cut
f.con.bc<- matrix(c(1,2,0,1),2) 
f.con<-rbind(f.con,f.con.bc)

f.dir <- rep("<=", 5)
f.rhs <- c(5,0,21,2,7)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution


##segunda iteracion

library(lpSolve)
f.obj <- c(2,1)
f.con<-matrix(c(1,-1,6,1,1,2),3) 
#branch and cut
f.con.bc<- matrix(c(1,2,0,1),2) 
f.con<-rbind(f.con,f.con.bc)

f.dir <- rep("<=", 5)
f.dir[4]<-">="
f.rhs <- c(5,0,21,3,7)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution


##segunda iteracion

library(lpSolve)
f.obj <- c(2,1)
f.con<-matrix(c(1,-1,6,1,1,2),3) 
#branch and cut
f.con.bc<- matrix(c(1,2,0,1),2) 
f.con<-rbind(f.con,f.con.bc)

f.dir <- rep("<=", 5)
f.dir[4]<-"="
f.rhs <- c(5,0,21,3,7)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution

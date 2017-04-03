
#####################################################################
#####################################################################
######                                                     ##########
######                 Punto 2 del taller                  ##########
######                                                     ##########
#####################################################################
#####################################################################
## Taller clase 8  

## Punto 2 Branch and bound

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
## Punto 2 Branch and cut
#####################################################################



library(lpSolve)
f.obj <- c(8,5)
f.con<-matrix(c(1,9,1,5),2) 
f.dir <- rep("<=", 2)
f.rhs <- c(6,45)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution


## Primera iteracion 
# se plantea: 
# B and B el corte x1<=3
# Gomory el corte 3x1+2x2<=15
library(lpSolve)
f.obj <- c(8,5)
f.con<-matrix(c(1,9,1,5),2) 
#Nuevas restricciones
f.con.bc<- matrix(c(1,3,0,2),2) 
f.con<-rbind(f.con,f.con.bc)

f.dir <- rep("<=", 4)
f.rhs <- c(6,45,3,15)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution


## Primera iteracion 
# se plantea: 
# B and B el corte x1>=4
# Gomory el corte 3x1+2x2<=15
library(lpSolve)
f.obj <- c(8,5)
f.con<-matrix(c(1,9,1,5),2) 
#Nuevas restricciones
f.con.bc<- matrix(c(1,3,0,2),2) 
f.con<-rbind(f.con,f.con.bc)

f.dir <- rep("<=", 4)
f.dir[3]<-">="
f.rhs <- c(6,45,4,15)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution



#####################################################################
#####################################################################
######                                                     ##########
######                 Punto 3 del taller                  ##########
######                                                     ##########
#####################################################################
#####################################################################
## Taller clase 8  
## Punto 2 Branch and bound

library(lpSolve)
f.obj <- c(3.5,2.5)
f.con<-matrix(c(1,2.5,1,1.5,1,0),3) 
f.dir <- rep("<=", 3)
f.rhs <- c(80,120,70)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution

## primera iteracion

library(lpSolve)
f.obj <- c(3.5,2.5)
f.con<-matrix(c(1,2.5,1,1,1.5,1,0,0),4) 
f.dir <- rep("<=", 4)
f.rhs <- c(80,120,70,36)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution

## primera iteracion

library(lpSolve)
f.obj <- c(3.5,2.5)
f.con<-matrix(c(1,2.5,1,1,1.5,1,0,0),4) 
f.dir <- rep("<=", 4)
f.dir[4]<-">="
f.rhs <- c(80,120,70,37)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution


## segunda iteracion

library(lpSolve)
f.obj <- c(3.5,2.5)
f.con<-matrix(c(1,2.5,1,1,0,1.5,1,0,0,1),5) 
f.dir <- rep("<=", 5)
f.rhs <- c(80,120,70,36,29)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution

## segunda iteracion

library(lpSolve)
f.obj <- c(3.5,2.5)
f.con<-matrix(c(1,2.5,1,1,0,1.5,1,0,0,1),5) 
f.dir <- rep("<=", 5)
f.dir[5]<-">="
f.rhs <- c(80,120,70,36,30)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution
#####################################################################
## Punto 3 Branch and cut
#####################################################################
## Punto 2 Branch and bound

library(lpSolve)
f.obj <- c(3.5,2.5)
f.con<-matrix(c(1,2.5,1,1.5,1,0),3) 
f.dir <- rep("<=", 3)
f.rhs <- c(80,120,70)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution

## primera iteracion
# B and B: x1<=36
# Gomory: 1.5x1+x2<=83

library(lpSolve)
f.obj <- c(3.5,2.5)
f.con<-matrix(c(1,2.5,1,1,1.5,1.5,1,0,0,1),5) 
f.dir <- rep("<=", 5)
f.rhs <- c(80,120,70,36,83)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution

## primera iteracion

library(lpSolve)
f.obj <- c(3.5,2.5)
f.con<-matrix(c(1,2.5,1,1,1.5,1.5,1,0,0,1),5) 
f.dir <- rep("<=", 5)
f.dir[4]<-">="
f.rhs <- c(80,120,70,37,83)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution

# segunda iteracion

library(lpSolve)
f.obj <- c(3.5,2.5)
f.con<-matrix(c(1,2.5,1,1,1.5,0,1.5,1,0,0,1,1),6) 
f.dir <- rep("<=", 6)
f.rhs <- c(80,120,70,36,83,29)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution

# segunda iteracion

library(lpSolve)
f.obj <- c(3.5,2.5)
f.con<-matrix(c(1,2.5,1,1,1.5,0,1.5,1,0,0,1,1),6) 
f.dir <- rep("<=", 6)
f.dir[6]<-">="
f.rhs <- c(80,120,70,36,83,30)
solucion<-lp("max", f.obj, f.con, f.dir, f.rhs)
solucion$objval
solucion$solution


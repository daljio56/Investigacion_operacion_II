library(lpSolve)
f.obj <- c(1, 9, 1)
f.con <- matrix (c(1, 2, 3, 3, 2, 2), nrow=2, byrow=TRUE)
f.dir <- c("<=", "<=")
f.rhs <- c(9, 15)
#
# Now run.
#
lp ("max", f.obj, f.con, f.dir, f.rhs)

solucion<-lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec=c(1,3))


chess.obj <- rep (1, 64)
q8 <- make.q8()
chess.dir <- rep(c("=", "<"), c(16, 26))
chess.rhs <- rep(1, 42)
solution<-lp('max', chess.obj, , chess.dir, chess.rhs, dense.const = q8, all.bin=TRUE, num.bin.solns=3)


f.obj <- c(5,7,3,0,0,0,0)
f.con<-as.matrix(read.csv("Workbook2.csv",header = F))
f.dir <- rep("<=", 9)
f.rhs <- c(7,5,9,0,0,0,2,30,100000039)
solucion<-lp ("max", f.obj, f.con, f.dir, f.rhs, binary.vec=c(4:8))



#11.6 

f.obj <- c(9,5,6,4)
f.con<-matrix(c(6,0,-1,0,3,0,0,-1,5,1,1,0,2,1,0,1),4)
f.dir <- rep("<=", 4)
f.rhs <- c(10,1,0,0)
solucion<-lp ("max", f.obj, f.con, f.dir, f.rhs, binary.vec=c(1:4))
solucion$objval


#11.7

f.obj <- c(4,-2,7,-1)
f.con<-matrix(c(1,1,6,-1,0,1,-5,0,5,-1,0,2,0,0,0,-2),4)
f.dir <- rep("<=", 4)
f.rhs <- c(10,1,0,3)
solucion<-lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec=c(1:3))
solucion$objval
solucion$solution


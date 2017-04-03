library(lpSolve)
## definicion de la variable.
# la variable del modelo es:
# Xsch
# Esta variable toma 1 si se asigna la asignatura s al salon c en la hora h.
Nasignaturas<-9
Nsalones<-3
Nhoras<-4
nombres<-rep(0,(Nasignaturas*Nsalones*Nhoras))
temp<-0
for(k in 1:Nasignaturas)
{
  for(l in 1:Nsalones)
  {
    for(q in 1:Nhoras)
    {
      temp<-temp+1
      nombres[temp]<-paste("X",k,l,q,sep="")
    }
  }
}
matriz<-matrix(c(1,1,1,1),2)

#######################################################
#######################################################
######         Primera restriccion           ##########
#######################################################
#######################################################
#que los profesores tengan el numero de horas que son
## horas de clase
sebastian<-c(3,5,6,8)
david<-c(2,4,7)
JuanMa<-c(1,9)
Profesores<-list(sebastian,david,JuanMa)
restric_profesor<-c()
for(k in 1:length(Profesores))
{
  temp<-(as.numeric(substr(nombres,2,2))%in%Profesores[[k]])*1
  restric_profesor<-rbind(restric_profesor,temp)
}
restric_profesor_desg<-rep("=",3)
restric_profesor_coef<-c(4,3,2)
#######################################################
#######################################################
######         Segunda restriccion           ##########
#######################################################
#######################################################

#que los profesor i no le asignen mas de una hora al mismo tiempo h
NumeroHoras<-4
restric_profesor_horas<-c()
for(k in 1:length(Profesores))
{
  temp<-(as.numeric(substr(nombres,2,2))%in%Profesores[[k]])*1
  for(l in 1:NumeroHoras)
  {
    temp2<-(as.numeric(substr(nombres,4,4))==l)*1 #horas h
    restric_profesor_horas<-rbind(restric_profesor_horas,temp*temp2)
  }
}
restric_profesor_horas_desg<-rep("<=",dim(restric_profesor_horas)[1])
restric_profesor_horas_coef<-rep(1,dim(restric_profesor_horas)[1])

#######################################################
#######################################################
######         tercera restriccion           ##########
#######################################################
#######################################################

#que cada asignatura se asigne una sola vez

NumeroHoras<-4
NumeroAsignatura<-9
restric_asignatura_horas<-c()
for(k in 1:NumeroAsignatura)
{
  restric_asignatura_horas<-rbind(restric_asignatura_horas,(as.numeric(substr(nombres,2,2))==k)*1)
}
restric_asignatura_horas_desg<-rep("=",NumeroAsignatura)
restric_asignatura_horas_coef<-rep(1,NumeroAsignatura)


#######################################################
#######################################################
######         Cuarta  restriccion           ##########
#######################################################
#######################################################

#Que para cada salon y cada hora solo se asigne una asignatura
NumeroHoras<-4
Numerosalones<-3
restric_horas_salon<-c()
for(k in 1:Numerosalones)
{
  temp1<-(as.numeric(substr(nombres,3,3))==k)*1
  for(l in 1:NumeroHoras)
  {
    temp2<-(as.numeric(substr(nombres,4,4))==l)*1
    restric_horas_salon<-rbind(restric_horas_salon,temp1*temp2)
  }
}

restric_horas_salon_desg<-rep("<=",NumeroHoras*Numerosalones)
restric_horas_salon_coef<-rep(1,NumeroHoras*Numerosalones)




#######################################################
#######################################################
######         Total restricciones           ##########
#######################################################
#######################################################
Restric<-rbind(restric_profesor,restric_profesor_horas,restric_asignatura_horas,restric_horas_salon)
desg<-c(restric_profesor_desg,restric_profesor_horas_desg,restric_asignatura_horas_desg,restric_horas_salon_desg)
coeficientesRH<-c(restric_profesor_coef,restric_profesor_horas_coef,restric_asignatura_horas_coef,restric_horas_salon_coef)




#funcionobjetivo

Funcion_obj<-as.numeric(substr(nombres,3,3))*as.numeric(substr(nombres,4,4))

solucion<-lp("min",Funcion_obj,Restric,desg,coeficientesRH,all.bin = T)
solucion_var<-as.logical(solucion$solution)
nombres[solucion_var]

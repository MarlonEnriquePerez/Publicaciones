library(readxl)
library(fastDummies)
library(tidyverse)
ACM_PERRu<- read_excel("C:/Users/k_kep/OneDrive/Escritorio/ACM PERRuño.xlsx")
ACM_PERRu$Tamaño<-factor(ACM_PERRu$Tamaño,  levels=c("1","2","3"), labels=c("pequeño", "mediano", "grande"))
ACM_PERRu$Peso<-factor(ACM_PERRu$Peso,  levels=c("1","2","3"), labels=c("liviano", "moderado", "pesado"))
ACM_PERRu$Velocidad<-factor(ACM_PERRu$Velocidad,  levels=c("1","2","3"), labels=c("lento", "normal", "rapido"))
ACM_PERRu$Inteligencia<-factor(ACM_PERRu$Inteligencia,  levels=c("1","2","3"), labels=c("baja", "moderada", "alta"))
ACM_PERRu$Afeccion<-factor(ACM_PERRu$Afeccion,  levels=c("1","2"), labels=c("afectuoso", "nada afectuoso"))
ACM_PERRu$Agresividad<-factor(ACM_PERRu$Agresividad,  levels=c("1","2"), labels=c("no agresivo", "agresivo"))
ACM_PERRu$Funcion<-factor(ACM_PERRu$Funcion,  levels=c("1","2","3"), labels=c("hogareño", "caza", "guardián"))
PSEUDO_TCC<-ACM_PERRu
View(PSEUDO_TCC)
var_TDC<-names(PSEUDO_TCC)[c(3,4,5,6,7,8,9)]
TCC<-select(PSEUDO_TCC,3:9)
PSEUDO_TDC<-dummy_cols(TCC,var_TDC)
TDC<-select(PSEUDO_TDC,8:26)
col.sums<-apply(TDC, 2, sum)
# matriz en terminos de proporciones
n<-sum(TDC)
P<-TDC/n
# matriz en terminos de proporciones
P<-as.matrix(P)
rr<-margin.table(P,1)
cc<-margin.table(P,2)
S<-diag(rr^(-0.5)) %*% (P-rr %*%t (cc))%*%diag(cc^(-0.5))
S
u<-svd(S)$u
v<-svd(S)$v
Da<-diag(svd(S)$d)
#paso3
FF<-diag(rr^(-0.5))%*% u %*%Da
GG<-diag(cc^(-0.5))%*% v %*%Da
cumsum(svd(S)$d)/sum(svd(S)$d)
#grafico
plot(GG[,1],GG[,2])
points(FF[,1],FF[,2],col="red")
kvar<-c("Tamaño_pequeño","Tamaño_mediano","Tamaño_grande","Peso_liviano","Peso_moderado"
      ,"Peso_pesado","Velocidad_lento","Velocidad_normal","Velocidad_rapido"
      ,"Inteligencia_baja","Inteligencia_moderada","Inteligencia_alta","Afeccion_afectuoso"
      ,"Afeccion_nada afectuoso", "Agresividad_no agresivo","Agresividad_agresivo"
      ,"Funcion_hogareño","Funcion_caza","Funcion_guardián")
#ind<-names(PSEUDO_TCC)[c(2)]
plot(GG[,1],GG[,2], col="green")
text(GG[,1],GG[,2],labels =kvar,cex=0.7, col="class")
points(FF[,1],FF[,2],col="red",aplha="1", size="3")
ind<-PSEUDO_TCC$Raza
text(FF[,1],FF[,2],labels =ind,cex=0.7,col = "green")

library(factoextra)

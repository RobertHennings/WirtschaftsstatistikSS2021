#Wirtschaftsstatistik SS 21
#Vorlseung Nr.9 Ausrüstungen, Preise und Profite
#Beispiel aus Vorlseung in Umsetzung

#Schritt 1: Anlegen der Produktionsmengen
x<-(t(c(100,50,10,80)))
View(x)

#Schritt 2: Anlegen der Matrix der Vorleistungen
vlHolz<-c(0,0,2,0)
vlStahl<-c(40,0,0,0)
vlFahr<-c(20,30,0,0)
vlGetreide<-c(0,10,2,0)

A<-as.data.frame(cbind(vlHolz, vlStahl, vlFahr, vlGetreide));A

#Schritt 3: Bestimmen der Inputkoeffizientenmatrix
#Leer Matrix anlegen und befüllen
Pi<-matrix(rep(NA,16), ncol=4, nrow=4, byrow=TRUE);Pi

for (i in 1: nrow(A)){
  Pi[,i]<-A[,i]/x[i]
}
#Befüllte Matrix als Inputkoeffizientenmatrix
Pi

#Schritt 4: Abziehen der Inputkoeffizientenmatrix von der Einheitsmatrix 
#Einheitsmatrix anlegen
I<-matrix(rep(0,16), ncol=4);I
diag(I)<-c(rep(1,nrow(I)));I

#Abziehen
I-Pi

#Schritt 5: Leontieff Inverse bilden
Leont<-solve(I-Pi);Leont


#Schritt 6: Anlegen der Matrix der Ausrüstung bzw. des fixen Kapitals
kapHolz<-c(0,0,0.01,0)
kapStahl<-c(0.6,0,0.2,0)
kapFahr<-c(1.5,2,0,0)
kapGetreide<-c(1,0.08,0.02,0)

GPhi<-as.data.frame(cbind(kapHolz,kapStahl, kapFahr, kapGetreide));GPhi

#Schritt 7: dominanten Eigenwert bestimmen
Eigenwerte<-matrix(rep(NA,16), ncol=4, nrow=4, byrow=TRUE);Eigenwerte

for (i in 1: nrow(Eigenwerte)){
  Eigenwerte[,i]<-GPhi[,i]*Leont[,i]
}
diag(Eigenwerte)
GPhi
Leont
as.matrix(GPhi)%*%as.matrix(Leont)
diag(as.matrix(GPhi)%*%as.matrix(Leont))




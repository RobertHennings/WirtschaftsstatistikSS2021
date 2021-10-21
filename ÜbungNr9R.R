#Wirtschaftsstatistik SS2021
#Übung Nr.9)

#Aufgabe Nr.1)
#a) Matrix der Inputkoeffizienten ermitteln

#Anlegen der Vorleistungsmatrix
A<-matrix(c(2,4,6,2), ncol=2, byrow = TRUE);A
#Anlegen der Outputmenge
x<-c(20,10);x
#Anlegen der Inputkoeffizienten
Pi<-matrix(c(A[,1]/x[1],A[,2]/x[2]),ncol=2);Pi

#b) Ermitteln der sektorale Arbeitskoeffizienten
b<-c(10,3)

w<-b/x;w

#c) Bilden der Leontieff Matrix
I<-matrix(c(1,0,0,1), ncol=2, byrow=TRUE);I

Leont<-solve(I-Pi);Leont

#f) Bilden der Matrix des fixen Kapitals
Gphi<-matrix(c(0,0.1,0.4,0.2),ncol=2,byrow=TRUE);Gphi
#Outputvektor x muss jedoch noch gewandelt werden 
X<-x*I;X
#Anwenden der Matrixmultiplikation
Gphi%*%X

#g) Bestimmen der reinen Arbeitswerte der beiden Güter
w
W<-w%*%Leont;W
#Reine Arbeitswerte in W enthalten

#h) iii) Ermittlung des numerischen Preisvektors für beide Güter bei einer festgelegten Profitrate
w
#Festlegen der Profitrate rho auf 0.2
rho1<-0.2
#Profitrate ist quasi variabel in der GLeichung
p1<-w%*%solve(I-Pi-rho1*Gphi);p1

#h) iv) Profitrate rho = 0.5
rho2<-0.5
p2<-w%*%solve(I-Pi-rho2*Gphi);p2

#h) v) Prozentuale Veränderungen der Preise bei rho=0.5 im Vergleich zu den reinen Arbeitswerten
#Reine Arbeitswerte in W
W
#In Prozent
(p2/W)-c(rep(1,2))
#51,17% und 56,02% liegen die Preise bei rho=0.5 höher als die reinen Arbeitswerte

#h) vi) Maximale Profitrate ergibt sich 
1/0.7105

#h vii) Numerischer Preisvektor bei rho=1.40746 (maximaler Profitrate, Arbeitswerte sind dann gleich 0)
rho3<-c(1/0.7105)
p3<-w%*%solve(I-Pi-rho3*Gphi);p3


#Formel erstellen mit variabler Profitarte rho
#rho hat Range von 0 bis 1.40746

Preisvektor<-function(rho){
  p<-w%*%solve(I-Pi-rho*Gphi)
  return(p)
}

Preisvektor(rho=0.5)
#Jetzt dynamisches Abspeichern der Werte in neuer Tabelle

rho=seq(from=0, to=c(1/0.7105+0.01), by=0.01)
length(rho)

PreisRho<-as.data.frame(matrix(NA,ncol=3,nrow=length(rho)));PreisRho
colnames(PreisRho)=c("Profitrate", "Preis1", "Preis2");head(PreisRho)
                        
#Jetzt muss der leere Dataframe befüllt werden

for (i in rho){
  for (i in 1:nrow(PreisRho)){
  PreisRho[i,1]<-rho[i]
  PreisRho[i,2]<-Preisvektor(rho[i])[1]
  PreisRho[i,3]<-Preisvektor(rho[i])[2]
  }
}

PreisRho
#Jedoch der Preisvektor bei der maximalen Profitrate nich ganz richtig aber dies resultiert aufgrund der Nachkommastellen die extreme Wirkung haben

Preisvektor(rho=1.4074)

#Auswirkungen darstellen
plot(x=PreisRho$Profitrate,y=PreisRho$Preis1,
     main="Zusammenhang von steigender Profitrate und Preis1",
     xlab="Profitrate",
     ylab="Preis für Gut 1",ylim=c(0,180), type="l")
lines(x=PreisRho$Profitrate,y=PreisRho$Preis2, col="red")

par(mfrow=c(1,2))
plot(x=PreisRho$Profitrate,y=PreisRho$Preis1,
     main="Zusammenhang von steigender Profitrate und Preis1",
     xlab="Profitrate",
     ylab="Preis für Gut 1",ylim=c(0,180), type="l")

plot(x=PreisRho$Profitrate,y=PreisRho$Preis2,
     main="Zusammenhang von steigender Profitrate und Preis2",
     xlab="Profitrate",
     ylab="Preis für Gut 2",ylim=c(0,180), type="l",col="red")

par(mfrow=c(1,1))

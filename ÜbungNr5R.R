#Wirtschaftsstatistik SS 21
#ÜBung Nr.5 Input Output Tabellen
#Aufagbe Nr.7)
#a) Vorleistungsmatrix erstellen
holz<-c(5,4,3)
stahl<-c(40,1,2)
fahr<-c(20,30,1)
#Gesamte matrix
mat<-data.frame(holz,stahl,fahr);mat
colnames(mat)<-NULL
#Final
mat

#b) Matrix der Inputkoeffizienten
mat[,1]/100
mat[,2]/50
mat[,3]/10
mat2<-data.frame((mat[,1]/100),(mat[,2]/50),(mat[,3]/10));mat2
colnames(mat2)<-NULL;mat2
#c) Vektor der eingesetzten Vorleistungen
x<-c(100,50,10);x
mat2
#Matrixmultiplikation anwenden da x die transponierte Version ist, es wird der Spaltenvektor gebraucht
vorleistungen<-as.matrix(mat2)%*%as.matrix(x);vorleistungen
#65,35,6

#d) Berechnung der Nettoproduktion
#Einheitsmatrix 3x3 anlegen
einheit<-matrix(data=0,nrow=3,ncol=3);einheit
for(i in 1:3){
  einheit[i,i]<-1
  
}
einheit

#Abziehen der Inputkoeffizientenmatrix
mat2
as.matrix(x)

y<-x-vorleistungen;y
#35,15,4

#e)
#Einheitsmatrix minus die Inputkoeffizientenmatrix
einheit
mat2<-as.matrix(mat2);mat2

einheit-mat2

#f) Nettoproduktion mit Hilfe der Matrixrechnung
y<-(einheit-mat2)%*%as.matrix(x);y

#g) 
solve(einheit-mat2)

#Aufgabe Nr.8)
#c) Leichtere Berechnung von Vorleistungen höherer Stufen
mat2
#Vorleistungen zweiter Stufe (hoch 2)
mat2%*%mat2
#Vorleistungen dritter Stufe (hoch 3)
mat2%*%mat2%*%mat2

#Nach unendlich vielen Iterationsschritten
solve(einheit-mat2)-einheit

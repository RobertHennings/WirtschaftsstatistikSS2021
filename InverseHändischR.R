#Wirtschaftsstatistik SS21
#Matrixinverse berechnen händisch


#Matrix anlegen
mat<-matrix(seq(1:4),nrow=2,ncol=2,byrow=TRUE);mat

koef<-1/((mat[1,1]*mat[2,2])-(mat[2,1]*mat[1,2]));koef
inver<-koef*matrix(c(mat[2,2],-mat[1,2],-mat[2,1],mat[1,1]),nrow=2,ncol=2,byrow=TRUE);inver




#Test mit Matrix aus der Aufgabe
mat<-matrix(c(0.9,-0.42,-0.38,0.76),nrow=2,ncol=2,byrow=TRUE);mat

koef<-1/((mat[1,1]*mat[2,2])-(mat[2,1]*mat[1,2]));koef
inver<-koef*matrix(c(mat[2,2],-mat[1,2],-mat[2,1],mat[1,1]),nrow=2,ncol=2,byrow=TRUE);inver

#Als schnellere Version
for(i in 1){
  koef<-1/((mat[1,1]*mat[2,2])-(mat[2,1]*mat[1,2]));koef
  inver<-koef*matrix(c(mat[2,2],-mat[1,2],-mat[2,1],mat[1,1]),nrow=2,ncol=2,byrow=TRUE);inver
  print(koef)
  print(inver)
}

#In R implementierte Funktion
solve(mat)

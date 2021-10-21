#Wirtschaftsstatistik SS2021
#Übung Nr.4)

#Aufgabe Nr.6) anlegen der Daten die gegeben sind
Altersklasse<-c(seq(1,4))
nf<-c(100,120,90,60)
ßf<-c(0,0.7,0.6,0)
stf<-c(0.1,0,0.3,1)

d<-as.matrix(cbind(Altersklasse,nf,ßf,stf));d

#a) Fortgeschriebene Bevölkerung ermitteln händisch
dframe<-as.data.frame(d)
#Bestehende Bevölkerung fortschreiben
dframe$t1<-c((1-dframe$stf)*dframe$nf);dframe
#Neugeborene die mit dazu kommen in t+1
dframe$neugebt1<-c(dframe$ßf*dframe$nf);dframe

#Gesamte Bevölkerung im nächsten Jahr
sum(dframe$t1,dframe$neugebt1)
#Ursprüngliche Bevölkerung in t
sum(dframe$nf)

#b) Nun mit der Leslie Matrix überprüfen die zum selben ergebnis kommen muss die 411

#4x4 Matrix anlegen
ft<-matrix(0,nrow=4,ncol=4)
#erste Zeile sind die Geburtenziffern
ft[1,]<-dframe$ßf;ft
#auffüllen mit den Überlebensziffern

for (i in 1:ncol(ft)){
  ifelse(i ==dim(ft)[2],break,ft[i+1,i]<-c(1-dframe$stf[i]))
}
ft

#Mit Matrixmultiplikation fortschreiben
sum(ft%*%dframe$nf)
#Alternatives Aufsummieren
eins<-c(rep(1,4))
#Achtung auf die Richtung vom Multiplizieren
eins%*%(ft%*%dframe$nf)

#Mit Schleife
mat<-ft
#2 mal multiplizieren entspricht der Bevölkerung im Jahr 3 der Fortschreibung
n.mult<-1
for(i in seq(n.mult)){
  mat<-mat%*%ft
  print("Bevölkerungsstand in Schritt:",print(i))
  print(sum(mat))
}
#Noch finales multiplizieren mit der absoluten Anzahl
sum(mat%*%dframe$nf)

sum((ft%*%ft)%*%dframe$nf)


#Tunen
#Mit Schleife
mat<-ft
#2 mal multiplizieren entspricht der Bevölkerung im Jahr 3 der Fortschreibung
n.mult<-2
for(i in seq(n.mult)){
  mat<-mat%*%ft
  print("Bevölkerungsstand in Schritt:",print(i))
  print(sum(mat))
  if(i == n.mult){
    mat<-mat%*%ft
    print(mat%*%dframe$nf)
  }
  
}
#Noch finales multiplizieren mit der absoluten Anzahl
sum(mat%*%dframe$nf)

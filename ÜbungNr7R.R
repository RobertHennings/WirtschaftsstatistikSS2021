#Wirtschaftsstatistik SS2021
#Übung Nr.7)
#Anlegen der gegeben Daten

#Output x
x<-t(c(20,40))

#Eingesetzte Arbeit
b<-t(c(10,5))

#Vorleistungsmatrix

A<-matrix(c(4,12,6,3),ncol=2,nrow=2,byrow=TRUE);A


#a) Berechnung der Inputkoeffizientenmatrix
pi<-matrix(c(A[,1]/x[1],A[,2]/x[2]),ncol=2,nrow=2);pi

#b) Vektor der eingesetzten Vorleistungen
pi%*%t(x)

#c) Vektor der Nettoproduktion 
y<-t(x)-pi%*%t(x);y

#d) Endnachfragemtarix erstellen
y
#Von Getreide (Zeile 1) werden 80% konsumiert und 20% investiert
#Von Zelten (Zeil 2) werden 60% konsumiert und 40% gespart

konsum<-c(0.8,0.6)
invest<-c(0.2,0.4)

end<-matrix(c(y[1]*konsum[1],y[1]*invest[1],y[2]*konsum[2],y[2]*invest[2]),ncol=2,nrow=2,byrow=TRUE);end

#e) Ermitteln: (eins-pi)
#Anlegen der Einheitsmatrix
eins<-matrix(c(1,0,0,1),ncol=2,nrow=2,byrow=TRUE);eins

eins-pi

#f) Nettoproduktion ermitteln mit (eins-pi)
x
y1<-(eins-pi)%*%t(x);y1

y1==y
#Selbes ergebnis berechnet

#g) die Inverse von (eins-pi) zu ermitteln
solve(eins-pi)

#h) 

#i)
#Sektorale Arbeitskoeffizienten berechnen
b
x
w<-b/x;w

#j) Numerisch zu zeigen: G=w*solve(eins-pi)*y
G=w%*%solve(eins-pi)%*%y;G

#k) Berechnung Vorleistungs Beschäftigungsmatrix

Ba1<-matrix(c(w[1],0,0,w[2]),ncol=2,nrow=2,byrow=TRUE);Ba1

#Ba mal die Vorleistungsmatrix
Ba<-Ba%*%A

#l)

#m) Endnachfrage Beschäftigungsmatrix
By<-Ba%*%end;By

#n)

#o)
#Zeilensummen von Ba und By sollen sich zum Beschägtigungsvektor aufsummieren
Ba
By
addmargins(Ba)[c(1:2),3]
addmargins(By)[c(1:2),3]

addmargins(Ba)[c(1:2),3]+addmargins(By)[c(1:2),3]


#p) Ermittlung der Beschäftigungsinverse
Ba1%*%solve(eins-pi)

#q)

#r) Ermittlung der sektoralen Beschäftigungsmultiplikatoren
w%*%solve(eins-pi)

#s)

#t) Beschäftigungsinverse der Endnachfrage Be
Be<-Ba1%*%solve(eins-pi)%*%end;Be

#u)

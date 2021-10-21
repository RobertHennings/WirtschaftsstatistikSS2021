#Wirtschaftsstatistik SS21 Vorlesung
#Vorlesung Nr.4) Demographie 4
#Einführung der altersspezifischen Buchführungsgleichung
#Gesamtheit der Bevölkerung die in einem Jahr t lebt ergibt sich aus der Summe der Menschen in den verschiedenen Altersklassen
#2 Vektoren für Männer und Frauen die 100 Zeilen lang sind für die Altersklassen 1 bis 100
#Beispielhaftes anlegen der Vektoren
#absolute Anzahl der Menschen je Altersklasse
ntm<-sample(seq(50,200),100);ntm

ntf<-sample(seq(50,200),100);ntf

#Gesamte Bevölkerung ergibt sich als Summe beider Vektoren
Bev<-sum(ntm,ntf);Bev
#24692 Menschen am leben

#Konstruktion von altersspezifischen Sterbeziffern
#Benötigt: Anzahl der gestorbenen Personen je Altersklasse
dtm<-sample(seq(0,50),100,replace=TRUE);dtm
dtf<-sample(seq(0,50),100,replace=TRUE);dtf

#Berchnen der altersspezifischen Sterbeziffern dtm/ntm

STdtm<-dtm/ntm
STdtf<-dtf/ntf
#Letzten Eintrag auf 1 setzen, es sterben alle im letzten Alter
STdtf[100]<-1
STdtm[100]<-1
#Konstruktion von altersspezifischen Geburtenziffern
#Benötigt: Anzahl der geborenen Personen je Altersklasse
btm<-sample(seq(0,30),100,replace=TRUE);btm
btf<-sample(seq(0,30),100,replace=TRUE);btf

#Berchnen der altersspezifischen Geburtenziffern, erst alle Geburten durch alle lebende Frauen
GEBalter<-c(sum(btm,btf)/ntf);GEBalter

#Altersspezifische Geburtenziffern
#Männer 
ßtm<-c(0.55*GEBalter*(1-0.20))
ßtf<-c((1-0.55)*GEBalter*(1-0.18))

#Nun soll die Bevölkerung um 1 Jahr fortgeschriben werden zu t+1
#Zunächst alle neugebornen die dazu kommen im Jahr t+1
Frauent1neu<-sum(btf*ntf);Frauent1neu
Männert1neu<-sum(btm*ntm);Männert1neu

#Nun Fortschreibung aller anderen Altersklassen
#Für alle anderen gilt:
Frauent1alt<-c((1-STdtm)*ntm)
Männert1alt<-c((1-STdtf)*ntf)

#Gesamte Bevölerung in t+1 ergibt sich aus der fortgeschriebenen bestehenden Bev und den neugeborenen
Bev1<-sum(Frauent1alt,Frauent1neu,Männert1alt,Männert1neu);Bev1
Bev

#Übertrag in Matrixschreibweise
#Zunächst die Geburtenziffermatrix für Männer und Frauen
ßMatm<-matrix(0,ncol=100,nrow=100)
ßMatf<-matrix(0,ncol=100,nrow=100)

#erste Zeile jeweils ist der 100x1 Vektor der Geburtenziffern, alles andere ist 0
ßMatm[1,]<-t(ßtm)
ßMatf[1,]<-t(ßtf)

#Auf der nicht ganz Hauptdiagonalen werden nun die Überlebensraten (1-STdtm) eingetragen
#Letzte Zeile muss komplett aus 0 en bestehen da das letzte Alter niemand überlebt
#Einträge müssen um eine Zeile nach unten verschoben werden da es sich nicht um die Hauptdiagonale handelt Start bei [2,1]
for (i in 1:ncol(ßMatm)){
  ifelse(i ==dim(ßMatm)[2],break,ßMatm[i+1,i]<-c(1-STdtm[i]))
}
#Für die Frauenmatrix
for (i in 1:ncol(ßMatf)){
  ifelse(i ==dim(ßMatf)[2],break,ßMatf[i+1,i]<-c(1-STdtf[i]))
}

#Leslie Matrix ist jeweils ßMatf und ßMatm

#Fortschreibung der Bevölkerung auch mittels Leslie Matrix möglich
#Leslie Matrix mal die Anzahl der ursprünglichen absoluten Anzahl je Altersklasse

Bev1
#Frauen fortschreiben mit Matrix
Frauent1Mat<-ßMatf%*%ntf
Männert1Mat<-ßMatm%*%ntm

#Aufsummieren
sum(Frauent1Mat,Männert1Mat)
eins<-c(rep(1,100))
eins%*%Frauent1Mat
eins%*%Männert1Mat


#Jetzt soll die Bevölkerung fortgeschrieben werden sodass man die Bev in t+3 hat
#Frauen drei Perioden fortgeschrieben
sum(ßMatf%*%ßMatf%*%ßMatf%*%ntf)
#Männer drei Perioden fortgeschrieben
sum(ßMatm%*%ßMatm%*%ßMatm%*%ntf)


#Gesamte Bevölkerung
Bev3<-sum(sum(ßMatf%*%ßMatf%*%ßMatf%*%ntf),sum(ßMatm%*%ßMatm%*%ßMatm%*%ntf));Bev3

#Mit Schleife
mat<-ßMatf
#2 mal multiplizieren entspricht der Bevölkerung im Jahr 3 der Fortschreibung
n.mult<-2
for(i in seq(n.mult)){
  mat<-mat%*%ßMatf
  print("Bevölkerungsstand in Schritt:",print(i))
  print(sum(mat))
}
sum(mat%*%ntf)
#Achtung im letzten Schritt muss noch ntf zB %*% multipliziert werden
#Dies über mehrere Zeiträume ausführen und die Daten abspeichern

erg<-c()
mat<-ßMatf
#2 mal multiplizieren entspricht der Bevölkerung im Jahr 3 der Fortschreibung
n.mult<-20
for(i in seq(n.mult)){
  mat<-mat%*%ßMatf
  print("Bevölkerungsstand in Schritt:",print(i))
  print(sum(mat))
  erg[i]<-sum(mat)
}
erg

#Jedoch Fehler in meiner händischen Berechnung
#Bev1 ist nicht die richtige Anzahl
#Über die Zeit strebt die Bevölkerung gegen ein Gleichgewicht
#bei konstanten Sterbe und Geburtenziffern also konstanter Fortschreibung der Leslie matrix ergibt sich
Bevt1<-(1+wr)*Bevt
#wr ist die intrinsische Wachstumsrate

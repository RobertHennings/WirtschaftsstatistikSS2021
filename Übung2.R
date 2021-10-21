#Modul Wirtschaftsstatistik SS21
#Übung Skript Nr.2)

#Aufgabe Nr.1)
#Anlegen der Datenbasis
Todesjahr<-c(1,3,6,7,9)
AnzahlTode<-c(1,1,3,2,1)
d<-data.frame(Todesjahr,AnzahlTode);d
#a)
d$Häufig<-c(AnzahlTode/(sum(AnzahlTode)));d
#b) Graphische Darstellung der Häufigkeistverteilung des Alters
plot(d$Häufig,type="h",main="Häufigkeitsverteilung des Alters",
     xlab="Todesjahre bzw. erreichtes Lebensalter",
     ylab="Relative Häufigkeit")
#c) Graphische Darstellung der Verteilungsfunktion des Alters

d$Vert<-cumsum(d$Häufig);d
plot(d$Vert,main="Verteilungsfunktion des Alters",
     xlab="Todesjahre bzw. erreichtes Lebensalter",
     ylab="Relative Häufigkeit",type="s")

plot(d$Vert,main="Verteilungsfunktion des Alters",
     xlab="Todesjahre bzw. erreichtes Lebensalter",
     ylab="Relative Häufigkeit",type="l")
#d) Graphische Darstellung der Survivorfunktion
dim(d)
d$Surv<-c(rep(1,5))

for (i in 2:nrow(d))
  d[i,5]<-1-d[i-1,4]

#Graphische Darstellung
plot(d$Surv,main="Surviviorfunktion des Alters",
     xlab="Todesjahre bzw. erreichtes Lebensalter",
     ylab="Relative Häufigkeit",type="l")

par(mfrow=c(1,2))
plot(d$Vert,main="Verteilungsfunktion des Alters",
     xlab="Todesjahre bzw. erreichtes Lebensalter",
     ylab="Relative Häufigkeit",type="l")
plot(d$Surv,main="Surviviorfunktion des Alters",
     xlab="Todesjahre bzw. erreichtes Lebensalter",
     ylab="Relative Häufigkeit",type="l")
par(mfrow=c(1,1))

#e) Graphische Darstellung der Ratenfuntion (Überlebensrate)
d$Rate<-d$Häufig/d$Surv

plot(d$Rate,main="Ratenfunktion des Alters",
     xlab="Todesjahre bzw. erreichtes Lebensalter",
     ylab="Relative Häufigkeit",type="l")

#f) Berechnung der mittleren Lebenserwartung eines Neugebornene
M<-sum(d$Todesjahr*d$Häufig);M

#g) Berechnung der ferneren Lebenserwartung eines 5 jährigen
c(d[3,1]*d[3,3]+d[4,1]*d[4,3]+d[5,1]*d[5,3])/sum(d[c(3:5),3])-5

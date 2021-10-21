#Wirtschaftsstatistik SS2021
#Übung Nr.2) 

Jahre<-c(0,seq(1:9))
Verstorbene<-c(0,1,0,1,0,0,3,2,0,1)
d<-data.frame(Jahre,Verstorbene);d

#a)
#Relative Häufigkeiten berechnen
d$P<-d$Verstorbene/sum(d$Verstorbene)
d

#b) Häufigkeitsfunktion dartsellen
plot(y=d$P,x=d$Jahre,type="h",lwd=2,
     main="Übersicht über die relative Sterbehäufigkeit",
     xlab="Jahre",
     ylab="Relative Häufigkeit",ylim=c(0,0.4))
#c) Verteilungsfunktion berechnen und darstellen
d$F<-cumsum(d$P);d
plot(y=d$F,x=d$Jahre,type="l",lwd=2,
     main="Verteilungsfuntkion der Sterbehäufigkeit",
     xlab="Jahre",
     ylab="Kumulierte relative Häuifgkeit")

plot(y=d$F,x=d$Jahre,type="S",lwd=2,
     main="Verteilungsfuntkion der Sterbehäufigkeit",
     xlab="Jahre",
     ylab="Kumulierte relative Häuifgkeit")

#d) Survivorfunktion errechnen
d$G<-1-d$F;d
plot(y=d$G,x=d$Jahre,type="S",lwd=2,
     main="Survivorfunktion G",
     xlab="Jahre",
     ylab="Kumulierte relative Häuifgkeit",
     xlim=c(0,9))
#e) Ratenfunktion berechnen
d$r<-d$P/d$G;d
d$r[10]<-1
d
#Graphische Darstellung
#Muss nochmal abgeäbdert werden
plot(y=d$r,x=d$Jahre,type="S",lwd=2,
     main="Survivorfunktion G",
     xlab="Jahre",
     ylab="Kumulierte relative Häuifgkeit",
     xlim=c(0,9))

#f) Berechnung der mittleren Lebenserwartung
mittel<-sum(d$Jahre*d$P);mittel
#5.625

#g) Lebenserwartung eines 5 Jährigen
mittel5<-c(sum(d$Jahre[6:10]*d$P[6:10])/sum(d$P[6:10]));mittel5
mittel5-5

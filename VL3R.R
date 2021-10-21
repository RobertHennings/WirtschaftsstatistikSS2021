#Wirtschaftsstatistik SS2021
#Vorlseung Nr.3) Demographie 3
#Anlegen der benötigten Daten
#maximales alter 100 daher werden 100x1 Vektoren gebraucht

#Bestehende Bevölkerung im Jahr t nach Altersklassen
#Am Anfang des Jahres
#Männer
ntm<-sample(seq(200,300),100,replace=TRUE)
#Frauen
ntf<-sample(seq(200,300),100,replace=TRUE)

#Am Ende des Jahres
ntmEnde<-sample(seq(100,150),100,replace=TRUE)
ntfEnde<-sample(seq(100,150),100,replace=TRUE)

#Geburten je Altersklasse anlegen

a<-c(seq(1:15),seq(from=15, to=1))
geburten<-c(rep(0,15),a,rep(0,55))
geburten

#Allgemeine Geburtenziffer
#Zahl aller Geburten durch die durchschnittlich anwesende Bevölkerung
#Durchschnittliche Bevölkerung
sum(ntm,ntmEnde)/2

sum(ntf,ntfEnde)/2

Bev<-sum(sum(ntf,ntfEnde)/2,sum(ntm,ntmEnde)/2);Bev
#Zahl aller Geburten
AGZ<-sum(geburten)/Bev;AGZ

#Allgemeine Geburtenrate
#Anzahl der Frauen im gebährfähigen Alter: alle Angaben in Zeilen 15 bis 45 aufsummieren
#Aufsummieren unde den Durchschnitt bilden von Zeil 15 bis 45
sum(ntf[1:45],ntfEnde[1:45])/2

AGR<-sum(geburten)/(sum(ntf[1:45],ntfEnde[1:45])/2);AGR
#Vergleich zeigt:
AGR;AGZ

#Weitere Spezialisierung: Altersspezifische Geburtenziffern
geburten
ntf


btf<-geburten/ntf;btf
plot(btf,type="l")
#Modellhaft nachempfunden
plot(density(rnorm(100,mean=31,sd=6)))

#Ableitung von Reproduktionsraten 
#Berechnung als gewichtetem Mittelwert
#altersspezifische Geburtenziffer je Altersklasse aufstellen
geburten
sum(ntf[1:45],ntfEnde[1:45])/2

geburten/ntfEnde

#Gewichte: relativer Anteil der jeweiligen Altersklasse an allen gebährfähigen Frauen
rel<-ntfEnde[1:45]/sum(ntfEnde[1:45]);rel
rel<-c(rep(0,14),ntfEnde[1:45]/sum(ntfEnde[1:45]),rep(0,41));length(rel)

#Total Fertility Rate berechnen:
#Aufsummieren der allgemeinen Geburtenrate der Jahre 15-49
TFR<-sum(geburten/ntf);TFR
#1000 Frauen würden demnach TFR*1000 Kinder gebären
TFR*1000

#Bruttoreproduktionsrate BRR
#sigmaf gibt den Frauenanteil an den Neugeborenen an
sigmaf<-0.487
BRR<-sigmaf*sum(geburten/ntf);BRR

#Nettoreproduktionsrate NRR
#Berücksichtigt die Sterblichkeit mit

sterb<-sample(rnorm(100,mean=0.4,sd=0.02),30,replace=TRUE)
sterb

#Wirtschaftsstatistik SS 21
#ÜBung Nr.8 Input Output Tabellen

#Einlesen der Input Output Tabelle
#Zeilen 1-12 stellen die aggregierten Produktionsbereiche dar
#Importe haben die eigene Zeile 14
#Weiterhin sind Spalten zu Konsum (C) Investitionen (I) und Exporten (Ex) angegebn und Letzte Verwendung (LV) sowie Gesamtverwendung (GV)
da<-read.csv2("/Users/Robert_Hennings/Dokumente/Uni/6.Semester/Wirtschaftsstatitsik/Übungen/Übung 8/io_2015_in.csv")
da
da<-as.data.frame(da)

db<-read.csv2("/Users/Robert_Hennings/Dokumente/Uni/6.Semester/Wirtschaftsstatitsik/Übungen/Übung 8/io_2015_et.csv")
db
#Aufgabe Nr.1) Drei größten Bereiche bezüglich Prodwerte
#a)
max(da[23,c(2:13)])

sort(da[23,c(2:13)], decreasing=TRUE)

#b)
#Höchste Exporte 
#Exporte liegt als Spalte vor

sort(da$Ex[1:12],decreasing = TRUE)

exporte<-as.data.frame(cbind(da$X, da$Ex));exporte[c(1:12),]
sort(exporte$V2, decreasing=TRUE)

#c) höchstes Verhältnis von Exporten zu Produktionswerten
#Neuen Frame mit BPW und EX als Variablen
str(da)
da[23,]
t(da[23,c(2:13)])
ex<-da$Ex[c(1:12)];ex


ex_BPW<-as.data.frame(cbind(t(da[23,c(2:13)]),ex));ex_BPW
colnames(ex_BPW)<-c("BPW","EX");ex_BPW



ex_BPW$EXBPW<-c(ex_BPW$EX/ex_BPW$BPW);ex_BPW
order(ex_BPW$EXBPW)
ex_BPW[order(ex_BPW$EXBPW, decreasing=TRUE),]

#d) Höchsten Nateil der Lähne (Entgelte) an der Bruttowertschöpfung
#BWS als Zeile 22
#Lohn als Zeile 18
t(da[22,c(2:13)])
t(da[18,c(2:13)])

lohnBrutto<-as.data.frame(cbind(t(da[22,c(2:13)]),t(da[18,c(2:13)])));lohnBrutto
colnames(lohnBrutto)<-c("BWS", "Lohn");lohnBrutto
lohnBrutto$LohnanteilanBWS<-c(lohnBrutto$Lohn/lohnBrutto$BWS);lohnBrutto
lohnBrutto[order(lohnBrutto$LohnanteilanBWS, decreasing=TRUE),]

#e) höchsten Anteile von Abschreibungen an der Bruttowertschöpfung
#BWS als Zeile 22
#Abschreibungen als Zeile 20

t(da[22,c(2:13)])
t(da[20,c(2:13)])

AbBrutto<-as.data.frame(cbind(t(da[22,c(2:13)]),t(da[20,c(2:13)])));AbBrutto
colnames(AbBrutto)<-c("BWS", "Abschreibungen");AbBrutto
AbBrutto$AbschreibanBWS<-c(AbBrutto$Abschreibungen/AbBrutto$BWS);AbBrutto
AbBrutto[order(AbBrutto$AbschreibanBWS, decreasing=TRUE),]

#Aufgabe Nr.2)
#Operationalisierung verschiedener Begriffe
#a) Importabhängigkeiten
#b)
#c)
#d) Höchste und niedrigste Arbeitsproduktivität und Kapitalintensität
#Arbeitsproduktivität als Prosuktionswert/Löhne
#Zeile 23 BPW
#Zeile 18 Löhne

t(da[23,c(2:13)])
t(da[18,c(2:13)])

arbprod<-as.data.frame(cbind(t(da[23,c(2:13)]), t(da[18,c(2:13)])));arbprod
colnames(arbprod)<-c("BPW", "Löhne")
arbprod$Prod<-c(arbprod$BPW/arbprod$Löhne);arbprod

arbprod[order(arbprod$Prod, decreasing=TRUE),]
#Kapitalintensität
#Zeile 20 Abschreibungen
#Zeile 18 Löhne
kapital<-as.data.frame(cbind(t(da[20,c(2:13)]), t(da[18,c(2:13)])));kapital
colnames(kapital)<-c("Abschreibungen", "Löhne")
kapital$Inten<-c(kapital$Abschreibungen/kapital$Löhne);kapital

kapital[order(kapital$Inten, decreasing=TRUE),]

#Graphische Darstellung
barplot(kapital$Inten~rownames(kapital))


#Aufgabe Nr.3)
#a) Grafische Darstellung der Anteile der einzelnen Produktionsbereiche an den Erwerbstätigen
View(db)
#et: Erwerbstätige
#an: Arbeitnehmer
colnames(db)<-c("Prodbereich", "Erwerbstätige","Arbeitnehmer")
#Absolute Zahlen
barplot(db$Erwerbstätige~db$Prodbereich, 
        xlab="Produktionsbereiche",
        ylab="Erwerbstätige in absoluten Zahlen",
        main="Erwerbstätige je Produktionsbereich")
#Relative Anteile
db<-as.data.frame(db)
db$RelET<-c(db$Erwerbstätige/sum(db$Erwerbstätige))
db$RelAN<-c(db$Arbeitnehmer/sum(db$Arbeitnehmer))
View(db)

#Darstellen
barplot(db$RelET~db$Prodbereich, 
        xlab="Produktionsbereiche",
        ylab="Erwerbstätige in relativen Zahlen",
        main="Erwerbstätige je Produktionsbereich",ylim=c(0,0.35))
#Als Tortendiagramm
pie(db$Erwerbstätige, labels=db$Prodbereich)

#b) Bruttowertschöpfung je Erwerbstätigem
str(db)
#BWS Zeile 22 in da
da[22,c(2:13)]
length(da[22,c(2:13)])
nrow(db)

#Zusammenführen
db$BWS<-(t(da[22,c(2:13)]));db
colnames(db)[6]<-"BWS";db

db$BWSjeET<-c(db$BWS/db$Erwerbstätige);db
#Darstellen
barplot(db$BWSjeET~db$Prodbereich, 
        xlab="Produktionsbereiche",
        ylab="BWS je ET",
        main="Bruttowertschöpfung je Erwerbstätigem",ylim=c(0,200))

#c)Vergleich der Selbstständigenquoten je Prodbereich
#Berechnung aus der Tabelle db, dort dann AN/ET bzw- 1-AN/ET
View(db)

db$SLQ<-c(1-(db$Arbeitnehmer/db$Erwerbstätige));View(db)

#Darstellen
barplot(db$SLQ~db$Prodbereich, 
        main="Selbstständigenquote je Prodbereich",
        xlab="Produktionsbereiche",
        ylab="Selbstständigenquote",
        ylim=c(0,0.50))
        
#d) Durchschnittslöhne der Erwerbstätigen in den Prodbereiche
str(db)
as.numeric(db$BWS)
#Lohn je Bereich durch die Anzahl der Beschäftigten teilen
#Dann Lohn pro Kopf


barplot(t(da[18, c(2:13)])~db$Prodbereich, 
        main="Durchschnittslöhne je Prodbereich",
        xlab="Produktionsbereiche",
        ylab="Löhne in absoluten Zahlen")

#Aufgabe Nr.4)
View(db)
db$KapitalInten<-kapital$Inten;db
#Zusammenhang plotten
plot(y=db$BWSjeET,x=db$KapitalInten,
     ylab="Kapitalintensität",
     xlab="Einkommensproduktivität",
     main="Zusammenhang Einkommensproduktivität und Kapitalintensität")
text(db$KapitalInten, db$BWSjeET, labels=db$Prodbereich, cex= 0.7, pos=3)


#Vermutung: Je höher die Einkommensproduktivität desto höher die Kapitalintensität
#Linearer Korrelationskoeffizient berechnen
cor(db$BWSjeET,db$KapitalInten)
#Ohne den Outlier 
cor(db$BWSjeET[2:12],db$KapitalInten[2:12])
#Deutlich höheres Ergebnis

#Plotten ohne den Outlier
plot(y=db$BWSjeET[2:12],x=db$KapitalInten[2:12],
     ylab="Kapitalintensität",
     xlab="Einkommensproduktivität",
     main="Zusammenhang Einkommensproduktivität und Kapitalintensität")
text(db$KapitalInten, db$BWSjeET, labels=db$Prodbereich, cex= 0.7, pos=3)

#Je höher die Abschreibungen desto höher jedoch auch die Bruttowertschöpfung

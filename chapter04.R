###CHAPTER 4: DESCRIPTIVE STATISTICS###
###POLITICAL ANALYSIS USING R, BY JAMIE MONOGAN###

#ciscenje data iz R
rm(list=ls())

##SECTION 4.1: MEASURES OF CENTRAL TENDENCY##
#Ucitavanje podataka putem 'cem' paketa i data LL
#install.packages('cem')
library(cem)
data(LL)

#Srednja primanja sudionika u 1974
mean(LL$re74)
# Srednja vrijednost primanja sudionika u 1974. iznosi 3630.738.

#Grafikon gustoce prihoda sudionika u 1974., s okomitom crtom u sredini
dens.74<-density(LL$re74,from=0)
plot(dens.74,main="Income in 1974")
abline(v=mean(LL$re74),col='red')
# Naredba "density" omogucuje nam izracunavanje gustoce opservacija za svaku vrijednost prihoda.
# Naredbom "from" specificiramo da je minimalna moguca vrijednost prihoda 0. Naredbom plot iscrtavamo objekt.
# "abline" koristimo da bismo iscrtali vertikalnu liniju gdje je smjestena srednja vrijednost.

#Medijan prihoda sudionika 1974
median(LL$re74)
# Medijan prihoda sudionika 1974. iznosi 823.8215.

#Medukvartilni raspon prihoda sudionika 1974
summary(LL$re74)
IQR(LL$re74)
# Moze se reci da nitko nije ostvario negativan prihod, 75% ispitanika se poklapa u rang.

#Povecanje broja znamenki
options(digits=9)

#Sazetak statistike prihoda sudionika u 1974
summary(LL$re74)
# Odsupanja su rezultat zaokruzivanja.

#Sazetak statistike za sve varijable u skupu podataka
summary(LL)
# Prikazuju se podatci za sve varijable.

#SECTION 4.1.1: FREQUENCY TABLES#
#Tablica utrke sudionika, 1=Afroamerikanac, 0 inace
table(LL$black)
# Mozemo saznati da su 578 sudionika utrke bili Afroamerikanci, a 144 sudionika nisu bili Afroamerikanci

#Tablica obrazovanja sudionika po godinama
table(LL$education)
# Na prvi pogled, nekoliko ispitanika nikada nije islo u srednju skolu.
# Samo nekoliko ispitanika ima nesto vise od srednjoskolskog obrazovanja.
# Mod je 11 godina obrazovanja, sto opisuje 195 ispitanika. Taj podatak lakse mozemo izvuci pomocu funkcije "which.max", navedene ispod

#Modalni broj godina obrazovanja
which.max(table(LL$education))
# Dobiva se direktno izvucen podatak da najvise ispitanika (195) ima 11 godina obrazovanja.

#Barplot ucestalosti broja godina obrazovanja
barplot(table(LL$education),xlab="Years of Education",ylab="Frequency",cex.axis=.9,cex.names=.9,ylim=c(0,200))
abline(h=0,col='gray60')
box()
# Ucestalost broja godina obrazovanja prikazano je pomocu funkcije "barplot".
# Horizontalna linija ponovo je iscrtana pomocu naredbe "abline" i postavljena je na 0.
# Naredba "box()" omogucuje iscrtavanje "margina" barplota
# Opet je lako vidjeti prema prikazu da najvise ispitanika ima 11 godina obrazovanja.

#Postotak ispitanika koji pripadaju u svaku kategoriju obrazovanja
100*table(LL$education)/sum(table(LL$education))

##SECTION 4.2: MEASURES OF DISPERSION##
#Varijansa prihoda sudionika u 1974
var(LL$re74)
# Iznos varijanse prihoda sudionika u 1974. iznosi 38696327.9.

#Standardna devijacija prihoda sudionika 1974
sd(LL$re74)
# Standardna devijacija prihoda sudionika 1974. iznosi 6220.63726.

#Srednja apsolutna devijacija prihoda sudionika 1974
mad(LL$re74)
# Srednja apsolutna devijacija prihoda sudionika 1974. iznosi 1221.39776.

#SECTION 4.2.1: QUANTILES AND PERCENTILES#
#Kvantili (prema zadanim postavkama, kvartili) prihoda sudionika 1974. godine
quantile(LL$re74)
# Ova informacija potvrduje informaciju koju smo ranije dobili naredbom "summary".

#Decili dohotka sudionika 1974. godine
quantile(LL$re74, c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
# Dodan je vektor od 11 kvantila zbog informacije o decilima, sto nam daje rubne tocke za svakih
# dodanih 10% podataka.


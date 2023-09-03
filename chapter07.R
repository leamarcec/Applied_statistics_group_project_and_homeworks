###CHAPTER 7: GENERALIZED LINEAR MODELS###
###POLITICAL ANALYSIS USING R, BY JAMIE MONOGAN###

##Potrebne datoteke: SinghJTP.dta, SinghEJPR.dta, and PESenergy.csv

##SECTION 7.1: BINARY OUTCOMES##
#ciscenje data iz R
rm(list=ls())

#Ucitavanje paketa
library(foreign)

#Ucitavanje podataka
voting<-read.dta("SinghJTP.dta",convert.factors=FALSE)

#SECTION 7.1.1: LOGIT MODELS#
#Procijenjivanje modela logisticke regresije jesu li ispitanici glasali za aktualnu stranku na temelju LINEARNE ideoloske udaljenosti
inc.linear<-glm(votedinc~distanceinc,family=binomial(link="logit"),data=voting)
summary(inc.linear)
# Moze se pretpostaviti da ovi statisticki podaci slijede normalnu distribuciju, vise nego t distribuciju

#LaTeX kod za tablicu rezultata
library(xtable)
xtable(inc.linear)

#Racunanje predvidenih ishode
predicted<-as.numeric(predict.glm(inc.linear,type="response")>.5)

#Spremanje vektora jesu li biraci glasali za aktualnog predsjednika
true<-voting$votedinc[voting$voted==1]

#Zabiljezavanje odgovara li predvideni glas stvarnim glasovima i izracunavanje postotka
correct<-as.numeric(predicted==true)
100*table(correct)/sum(table(correct))
# Model tocno predvida 69% rezultata 

#Procijenjivanje modela logisticke regresije jesu li ispitanici glasali za aktualnu stranku na temelju QUADRATIC ideoloske distance
inc.squared<-glm(votedinc~distanceincsq,family=binomial(link="logit"),data=voting)
summary(inc.squared)
# Sa vecom vrijednosti 43091 u kvadratnom modelu, zakljucuje se da model s linearnom ideoloskom distancom pristaje bolje sa
# AIC = 42 914  
# To korespondira sa originalnim zakljuckom clanka da linearni oblik varijable bolje pristaje

#SECTION 7.1.2: PROBIT MODELS#
#Procjena postenja regresijskog modela jesu li ispitanici uopce glasovali na temelju linearne udaljenosti do najblize stranke
turnout.linear<-glm(voted~distanceweighted,family=binomial(link="probit"),data=voting)
summary(turnout.linear)
# Promjena distribucije termina latentne pogreske u normalnu distribuciju mijenja skalu koeficijenata pa ce vrijednosti biti drugacije
# izmedu modela.

#SECTION 7.1.3: INTERPRETING LOGIT AND PROBIT RESULTS#
#Omjer vjerojatnosti iz logistickog modela trenutnog glasanja na temelju linearne udaljenosti
exp(inc.linear$coefficients[-1])
# Omjer vjerojatnosti iz logistickog modela trenutnog glasanja na temelju linearne udaljenosti iznosi 0.6097611.

#Tumacenje omjera vjerojatnosti u postotcima
100*(exp(inc.linear$coefficients[-1])-1)
# vjerojatnost u postotcima iznosi 39%

#Stvaranje predvidene vjerojatnosti jesu li biraci izabrali aktualnu stranku na temelju LINEARNE ideoloske distance
distances<-seq(0,10,by=.1)
inputs<-cbind(1,distances)
colnames(inputs)<-c("constant","distanceinc")
inputs<-as.data.frame(inputs)
forecast.linear<-predict(inc.linear,newdata=inputs,type="response")
# U prvoj liniji kreiramo sekvencu mogucih udaljenosti aktualne stranke.
# Kreiramo matricu "inputs" koja pohranjuje predvidene vrijednosti interesa za sve prediktore u modelu.
# Imenujemo stupce kako bismo spojili imena varijabli i prepoznali matricu kao data frame.
# Naredba "predict" sprema predvidene vjerojatnosti u vektor. 

#Stvaranje predvidene vjerojatnosti jesu li biraci izabrali aktualnu stranku na temelju KVADRATI?NE ideoloske distance
inputs2<-cbind(1,distances^2)
colnames(inputs2)<-c("constant","distanceincsq")
inputs2<-as.data.frame(inputs2)
forecast.squared<-predict(inc.squared,newdata=inputs2,type="response")
# Koristimo originalni vektor "distances" koji dohvaca hipotetske prediktorske vrijednosti i kvadrira ih

#Planiranje predvidene vjerojatnosti na temelju linearnih i kvadratnih funkcionalnih oblika
plot(y=forecast.linear,x=distances,ylim=c(0,.6),type="l",lwd=2,xlab="",ylab="")
lines(y=forecast.squared,x=distances,lty=2,col="blue",lwd=2)
legend(x=6,y=.5,legend=c("linear","squared"),lty=c(1,2),col=c("black","blue"),lwd=2)
mtext("Ideological Distance",side=1,line=2.75,cex=1.2)
mtext("Probability of Voting for Incumbent",side=2,line=2.5,cex=1.2)
# Model sa kvadriranim udaljenostima je vise responzivan kod srednjih vrijednosti 

#Izracunavanje predvidene vjerojatnosti odaziva na temelju probit modela
wght.dist<-seq(0,4,by=.1)
inputs.3<-cbind(1,wght.dist)
colnames(inputs.3)<-c("constant","distanceweighted")
inputs.3<-as.data.frame(inputs.3)
forecast.probit<-predict(turnout.linear,newdata=inputs.3,type="link",se.fit=TRUE)

#Izracunavanje intervale povjerenja predvidenih vjerojatnosti odaziva
lower.ci<-forecast.probit$fit-1.95996399*forecast.probit$se.fit
upper.ci<-forecast.probit$fit+1.95996399*forecast.probit$se.fit

#Graficki prikaz predvidene vjerojatnosti odaziva s intervalima povjerenja
plot(y=pnorm(forecast.probit$fit),x=wght.dist,ylim=c(.7,.9),type="l",lwd=2,xlab="Weighted Ideological Distance",ylab="Probability of Turnout")
lines(y=pnorm(lower.ci),x=wght.dist,lty=3,col="red",lwd=2)
lines(y=pnorm(upper.ci),x=wght.dist,lty=3,col="red",lwd=2)
# Bitno u ovom prikazu je da interval pouzdanosti postaje primjetno sirok za vece vrijednosti ponderirane udaljenosti

##SECTION 7.2: ORDINAL OUTCOMES##
#ciscenje data iz R 
rm(list=ls())

#Ucitavanje paketa
library(foreign)
library(MASS)

#Ucitavanje podataka
satisfaction<-read.dta("SinghEJPR.dta")

#Zadovoljstvo demokracijom pretvoriti u uredeni cimbenik
satisfaction$satisfaction<-ordered(as.factor(satisfaction$satisfaction))

#Procijenjivanje naruceni logit modela zadovoljstva demokracijom na temelju glasovanja za ideoloski blisku stranku
ideol.satisfaction<-polr(satisfaction~voted_ideo*winner+abstained+educ+efficacy+majoritarian_prez+freedom+gdppercapPPP+gdpgrowth+CPI+prez,method="logistic",data=satisfaction)
summary(ideol.satisfaction)
# Prikazuje se procjena svakog koeficijenta, standardna pogreska i "z" vrijednost.

#LaTeX kod za tablicu
coef<-c(ideol.satisfaction$coefficients,ideol.satisfaction$zeta)
se<-sqrt(diag(vcov(ideol.satisfaction)))
z<-coef/se
p<-2*(1-pnorm(abs(z)))
xtable(cbind(coef,se,z,p),digits=4)

#One-tailed p-value testiranje je li interakcija izmedu glasovanja za blizu stranku, koja je takoder pobijedila, pozitivna
1-pnorm(5.0957)

#Omjeri vjerojatnosti za naruceni logit zadovoljstva, s postotnim promjenama
exp(-ideol.satisfaction$coefficients)
100*(exp(-ideol.satisfaction$coefficients)-1)

#Procijenjivanje poredane probit modela zadovoljstva demokracijom na temelju glasovanja za stranku koja se smatra s najvecim utjecajem
affect.satisfaction<-polr(satisfaction~voted_affect*winner+abstained+educ+efficacy+majoritarian_prez+freedom+gdppercapPPP+gdpgrowth+CPI+prez,method="probit",data=satisfaction)
summary(affect.satisfaction)
# Hipoteza je potvrdena rezultatom

##SECTION 7.3: EVENT COUNTS##
#ciscenje podataka iz R
rm(list=ls())

#Ucitavanje podataka
pres.energy<-read.csv("PESenergy.csv")

#SECTION 7.3.1: POISSON REGRESSION#
#Procjena Poissonov regresijski model mjesecnog pokrivanja energetske politike
energy.poisson<-glm(Energy~rmn1173+grf0175+grf575+jec477+jec1177+jec479+embargo+hostages+oilc+Approval+Unemploy,family=poisson(link=log),data=pres.energy)
summary(energy.poisson)

#Broj omjera ucinka predsjednickog odobrenja
exp(-.034096)
# Broj omjera ucinka predsjednickog odobrenja iznosi 0.966478718.

#Broj omjera svih prediktora, s postotnim promjenama
exp(energy.poisson$coefficients[-1])
100*(exp(energy.poisson$coefficients[-1])-1)

#SECTION 7.3.2: NEGATIVE BINOMIAL REGRESSION#
#Negativni binomni regresijski model mjesecnog pokrivanja energetske politike
energy.nb<-glm.nb(Energy~rmn1173+grf0175+grf575+jec477+jec1177+jec479+embargo+hostages+oilc+Approval+Unemploy,data=pres.energy)
summary(energy.nb)
# Moze se interpretirati na isti nacin kao i koeficijenti iz Poissonova modela

#SECTION 7.3.3: PLOTTING PREDICTED COUNTS#
#Kreiranje vektora mogucih vrijednosti odobrenja
approval<-seq(24,72.3,by=.1)

#Kreiranje podatkovnog okvira vrijednosti prediktora za prognozu, koristeci tipicne vrijednosti prediktora i vektor mogucih vrijednosti odobrenja
inputs.4<-cbind(1,0,0,0,0,0,0,0,0,mean(pres.energy$oilc),approval,mean(pres.energy$Unemploy))
colnames(inputs.4)<-c("constant","rmn1173","grf0175","grf575","jec477","jec1177","jec479","embargo","hostages","oilc","Approval","Unemploy")
inputs.4<-as.data.frame(inputs.4)

#Kreiranje predvidenih brojeva za Poissonove i negativne binomne regresijske modele na temelju podatkovnog okvira tipicnih vrijednosti, manipulirajuci odobrenjem
forecast.poisson<-predict(energy.poisson,newdata=inputs.4,type="response")
forecast.nb<-predict(energy.nb,newdata=inputs.4,type="response")
# Ova dva modela razlikuju se samo po modelu iz kojeg crtaju procjene koeficijenata za prognozu.

#Graficki prikaz za predvideni broj za Poissonove i negativne binomne regresije
plot(y=forecast.poisson,x=approval,type="l",lwd=2,ylim=c(0,60),xlab="Presidential Approval",ylab="Predicted Count of Energy Policy Stories")
lines(y=forecast.nb,x=approval,lty=2,col="blue",lwd=2)
legend(x=50,y=50,legend=c("Poisson","Negative Binomial"),lty=c(1,2),col=c("black","blue"),lwd=2)
# Predikcije dva modela su slicne i prikazuju slicni negativni efekt odobrenja. 
###CHAPTER 6: LINEAR MODELS AND REGRESSION DIAGNOSTICS###
###POLITICAL ANALYSIS USING R, BY JAMIE MONOGAN###

##Potrebna datoteka za rad programa: BPchap7.dta

##SECTION 6.1: ESTIMATION WITH ORDINARY LEAST SQUARES##
#ciscenje data u R
rm(list=ls())

#Ucitavanje paketa
library(foreign)

#Ucitavanje podataka
evolution<-read.dta("BPchap7.dta",convert.factors=FALSE)

#Brzi pregled deskriptivne statistike
summary(evolution)

#Recode nedostajuce vrijednosti vrijable "female"
evolution$female[evolution$female==9]<-NA

#Revidirani prikaz deskriptivne statistike
summary(evolution)
# Moze se uociti da je nedostajucih vrijednosti u varijabli "female" 13

#Prije postupanja potrebno je ponovno klasificirati nedostajuce vrijednosti varijable "female"
evolution<-subset(evolution,!is.na(female))
evolution<-na.omit(evolution)

#Procjenjivanje modela linearne regresije sati provedenih u poducavanju evolucije s obicnim najmanjim kvadratima
mod.hours<-lm(hrs_allev~phase1*senior_c+phase1*notest_p+female+biocred3+degr3+evol_course+certified+idsci_trans+confident,data=evolution)
summary(mod.hours)
# U standardna sintaksi za formulu modela s lijeve strane znaka tilda (~) nalaze se ishodisne varijable, a s desne strane ulazne
# varijable odvojene znakom "+"

#Kreiranje LaTeX tablice za rezultate
install.packages('xtable')
library(xtable)
xtable(mod.hours)

#Intervali povjerenja za regresijske koeficijente
confint(mod.hours,level=0.90)


##SECTION 6.2: REGRESSION DIAGNOSTICS##
#SECTION 6.2.1: FUNCTIONAL FORM#
#Graficki prikaz ostataka u odnosu na ugradene vrijednosti
plot(y=mod.hours$residuals,x=mod.hours$fitted.values,xlab="Fitted Values",ylab="Residuals")
# Cini se da ce ostaci "pododiti" dno prikaza, zbog toga sto ucitelj ne moze potrositi manje od nule
# sati uceci o evoluciji

#Graficki prikaz za izracunavanje ostataka naspram SIROVIH VRIJEDNOSTI koliko ucitelj misli o sebi kao znanstveniku
plot(y=mod.hours$residuals,x=evolution$idsci_trans,xlab="Identifies as Scientist",ylab="Residuals")
# Neobrađeni podatci

#Graficki prikaz za izracunavanje ostataka u odnosu na TRETNE VRIJEDNOSTI koliko ucitelj misli o sebi kao znanstveniku
plot(y=mod.hours$residuals,x=jitter(evolution$idsci_trans,amount=.01),xlab="Identifies as Scientist (Jittered)",ylab="Residuals")
# Obradeni podatci

#Ramsey's  RESET test, s obzirom na kubicni funkcionalni oblik
evolution$fit<-mod.hours$fitted.values
# Spremanje "fitted" vrijednosti iz originalnog modela kao varijablu u data frame 
reset.mod<-lm(hrs_allev~phase1*senior_c+phase1*notest_p+female+biocred3+degr3+evol_course+certified+idsci_trans+confident+I(fit^2)+I(fit^3),data=evolution)
# Dodavanje kvadratnih i kubicnih oblika "fitted" vrijednosti u regresijski model
anova(mod.hours, reset.mod)
# Predstavlja rezultate F-testa koji usporeduje originalni model s modelom koji ukljucuje kvadratni i kubicni oblik "fitted" vrijednosti

#Ucitavanje 'lmtest' paketa
install.packages('lmtest')
library(lmtest)

#Durbin-Watsonov test za utvrdivanje jesu li ostaci slicni na slicnim razinama strogosti standarda evolucije
dwtest(mod.hours,order.by=evolution$phase1)
# Ispis pokazuje da su ostaci slicno bazirani na vrijednost kovarijante

#refitirani model linearne regresije s kubicnim oblikom strogosti standarda evolucije
mod.cubic<-lm(hrs_allev~phase1*senior_c+phase1*notest_p+female+biocred3+degr3+evol_course+certified+idsci_trans+confident+I(phase1^2)*senior_c+I(phase1^3)*senior_c++I(phase1^2)*notest_p+I(phase1^3)*notest_p,data=evolution)
summary(mod.cubic)

#SECTION 6.2.2: HETEROSCEDASTICITY#
#Breusch-Pagan test
bptest(mod.hours, studentize=FALSE)
# Odbacujemo nul hipotezu i zakljucujemo da ostaci nisu homoskedasticni

#Ucitavnanje 'sandwich' paketa
install.packages('sandwich')
library(sandwich)

#compute Huber-White standardne pogreske i ponovno prijaviti rezultate
coeftest(mod.hours,vcov=vcovHC)

#Izracunavanje ostatka na kvadrat i prilagodite regresiju za izracunavanje tezina
evolution$resid2<-mod.hours$residuals^2
weight.reg<-lm(log(resid2)~phase1*senior_c+phase1*notest_p+female+biocred3+degr3+evol_course+certified+idsci_trans+confident, data=evolution)
summary(weight.reg)

#reprocijenjivanje modela linearne regresije s ponderiranim najmanjim kvadratima
wls.mod<-lm(hrs_allev~phase1*senior_c+phase1*notest_p+female+biocred3+degr3+evol_course+certified+idsci_trans+confident,data=evolution,weights=I(1/exp(weight.reg$fitted.values)))
summary(wls.mod)
# Skup procjena koji uzima u obzir heteroskedasticnost ostataka

#SECTION 6.2.3: NORMALITY#
#histogram reziduala
hist(mod.hours$residuals,xlab="Residuals",main="")
# Ostaci su desno iskrivljeni, prikazujuci da normalnost nije sigurna pretpostavka u ovom slucaju

#kvantil-kvantilni grafikon, s dodanom linijom vodilice
qqnorm(mod.hours$residuals)
# Kreiranje Q-Q plota
qqline(mod.hours$residuals,col='red')
# Dodavanje linije kao vodica postojecem plotu

#Ucitavanje 'tseries' paketa
install.packages('tseries')
library(tseries)

#Jarque-Bera test preostale normalnosti
jarque.bera.test(mod.hours$residuals)
# X-squared iznosi 191.5709 pa odbacujemo nul hipotezu i zakljucujemo da ostaci nisu normalno distribuirani

#SECTION 6.2.4: MULTICOLLINEARITY#
#Ucitavanje 'car' paketa
library(car)

#Izracunavanje faktora inflacije varijance
vif(mod.hours)
# VIF vrijednosti su male, sto implicira da multikolinearnost nije veliki problem ovog modela

#SECTION 6.2.5: OUTLIERS, LEVERAGE, AND INFLUENTIAL DATA POINTS#
#Graficki prikaz Cookov D, Studentizirani ostaci i vrijednosti ?e?ira
influenceIndexPlot(mod.hours,vars=c("Cook","Studentized","hat"),id.n=5)
# U svakom od prikaza ekstremna vrijednost u odnosu na ostale ukazuje da observacija moze biti djelomican problem

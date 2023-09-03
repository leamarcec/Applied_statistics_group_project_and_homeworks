###CHAPTER 5: BASIC INFERENCES AND BIVARIATE ASSOCIATION###
###POLITICAL ANALYSIS USING R, BY JAMIE MONOGAN###

##POTREBNA JE DATOTEKA PODATAKA: LL.csv, ili  LL iz 'cem' paketa

#ciscenje data iz R
rm(list=ls())

##SECTION 5.1: SIGNIFICANCE TESTS FOR MEANS##
#Ucitavanje podataka iz cem paketa za LL
install.packages('cem')
library(cem)
data(LL)

#Testiranje je li populacija dugotrajno nezaposlenih imala nizi prosjecni prihod od stanovnistva SAD-a 1974.
t.test(LL$re74, mu=6059, alternative="less")
# Prvi argument je varijabla koja nas zanima, a za koju R automatski izracunava srednju vrijednost uzorka i
# standardnu pogresku.
# Drugi argument, "mu=6059", ispisuje vrijednost interesa nul-hipoteze.
# Alternativna hipoteza je oznacena kao "less", sto znaci da je srednja vrijednost populacije manja od nulte kolicine

#SECTION 5.1.1: TWO-SAMPLE DIFFERENCE OF MEANS TEST, INDEPENDENT SAMPLES
#Testiranje jesu li lijeceni pojedinci zaradili vise 1978. od kontrolne skupine, uz pretpostavku razlicite varijacije prihoda za svaku skupinu
t.test(re78~treated,data=LL,alternative="less",var.equal=F)
# Prvi argument "re78~treated" je u funkcionalnoj notaciji i ukazuje da je prihod u 1978. bio odvojen prema vrijednosti
# indikatora lijecenja
# Opcija "data" omogucava imenovanje skupa podataka, pa ga ne moramo pozivati za svaku varijablu zasebno
# Argumentom "alternative="less"" deklariramo alternativnu hipotezu, sto znaci da je prosjecni prihod za nizu vrijednost 
# varijable "treated" (grupa 0, kontrolna) trebao biti nizi od prosjeka vise vrijednosti varijable "treated"
# (grupa 1, lijecena grupa)
# Argument "var.equal=F" ukazuje da se pretpostavlja da su varijance nejednake.

#Testiranje jesu li lijeceni pojedinci zaradili vise 1978. od kontrolne skupine, uz pretpostavku iste varijacije prihoda za obje grupe
t.test(re78~treated,data=LL,alternative="less",var.equal=T)
# Objasnjenje je slicno kao za prethodni primjer uz razliku sto je zadnji argument prikazan kao "var.equal=T", sto ukazuje da 
# se pretpostavlja da su varijance jednake.

#SECTION 5.1.2: COMPARING MEANS WITH DEPENDENT SAMPLES#
#Podskup samo za kontrolnu grupu
LL.0<-subset(LL,treated==0)

#Test je li prihod kontrolne skupine porastao od 1974. do 1978
t.test(LL.0$re74,LL.0$re78,paired=T,alternative="less")
# Prvi argument je mjera prihoda u 1974., a drugi je mjera prihoda u 1978.
# Treci argument je upareni uzorak 

#Podskup samo za tretiranu grupu
LL.1<-subset(LL,treated==1)

#Testiranje je li prihod lijecene grupe porastao od 1974. do 1978
t.test(LL.1$re74,LL.1$re78,paired=T,alternative="less")
# Slicno objasnjenje kao i iznad

##SECTION 5.2: CROSS-TABULATIONS##
#Ucitavanje 'gmodels' paketa
install.packages('gmodels')
library(gmodels)

#Unakrsna tabela nezaposlenosti u 1975. ovisno o nezaposlenosti iz 1974.
CrossTable(y=LL$u75,x=LL$u74,prop.c=F,prop.t=F,prop.chisq=F,chisq=T,format="SPSS")
# "y" oznacava varijablu stupca, "x" oznacava varijablu retka --> zavisna varijabla cini stupce, a nezavisna retke
# "prop.c" --> proportion of the column 
# "prop.t" --> proportion of the total sample
# "prop.chisq" --> contribution to the chisquare statistic
# postavljanjem argumenta "format="SPSS" postavljaju se postotci u celijama

#Unakrsna tabela nezaposlenosti u 1975. ovisno o primanju lijecenja
CrossTable(y=LL$u75,x=LL$treated,prop.c=F,prop.t=F,prop.chisq=F,chisq=T,format="SPSS")
# Slicno objasnjenje kao iznad

##SECTION 5.3: CORRELATION COEFFICIENTS##
#Korelacija izmedu obrazovanja i prihoda 1974. godine
cor(LL$education,LL$re74)
# Korelacija izmedu obrazovanja i prihoda 1974. godine iznosi 0.0891645773.

#Varijansa objasnjena jednim prediktorom
cor(LL$education,LL$re74)^2
# Varijansa objasnjena jednim prediktorom iznosi 0.00795032184.

#Korelacija izmedu prihoda 1975. i 1978. godine
cor(LL$re75,LL$re78)
# Korelacija izmedu prihoda 1975. i 1978. godine iznosi 0.154898167.

#Varijansa objasnjena jednim prediktorom
cor(LL$re75,LL$re78)^2
# Varijansa objasnjena jednim prediktorom iznosi 0.0239934421.

#Rasprseni prikaz prihoda iz 1978. u odnosu na prihod iz 1975. godine
plot(x=LL$re75,y=LL$re78,xlab="1975 Income",ylab="1978 Income",asp=1,xlim=c(0,60000),ylim=c(0,60000),pch=".")
# Mnoge opservacija klasteriraju se prema nuli u jednoj ili u obje godine, postoji granicni stupanj do kojeg linearni 
# odnos karakterizira ove podatke.
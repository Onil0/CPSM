
# Studente: D'amora Lino
# Matricola: 0512108687

if (!require(moments)) {
  install.packages("moments")
}

library(moments)
#PROBLEMA: Si vuole analizzare il costo del gasolio degli ultimi 10 anni.
#I dati presi in considerazione tengono conto della media del costo mensile del gasolio relativo agli ultimi 10 anni considerando il periodo: Maggio 2013 - Maggio 2023
#Lo storico dei dati è stato reperito al seguente link: https://www.rivaluta.it/prezzi/prezzi-carburanti-italia.asp
#Per convenienza e miglior lettura i dati sono stati approsimati a 2 cifre (rispetto ai dati riportati sul sito).
#taglia campione: n = 121

gasolio<-c(1.61, 1.63, 1.64, 1.66, 1.68, 1.66, 1.64, 1.66, 1.65, 1.64, 1.63, 1.63, 
           1.63, 1.63, 1.64, 1.62, 1.61, 1.59, 1.55, 1.49, 1.39, 1.40, 1.46, 1.45, 
           1.48, 1.48, 1.45, 1.40, 1.36, 1.35, 1.34, 1.31, 1.23, 1.19, 1.23, 1.24, 
           1.28, 1.32, 1.30, 1.29, 1.30, 1.32, 1.33, 1.35, 1.40, 1.40, 1.40, 1.40, 
           1.38, 1.35, 1.34, 1.35, 1.37, 1.39, 1.41, 1.42, 1.44, 1.43, 1.42, 1.45,
           1.50, 1.52, 1.51, 1.51, 1.52, 1.56, 1.54, 1.45, 1.43, 1.47, 1.50, 1.51,
           1.51, 1.49, 1.48, 1.46, 1.47, 1.47, 1.47, 1.48, 1.48, 1.44, 1.38, 1.30,
           1.26, 1.27, 1.29, 1.29, 1.27, 1.26, 1.26, 1.30, 1.34, 1.39, 1.44, 1.44,
           1.45, 1.47, 1.51, 1.51, 1.51, 1.59, 1.61, 1.59, 1.63, 1.72, 1.97, 1.77,
           1.82, 1.97, 1.93, 1.79, 1.79, 1.85, 1.81, 1.73, 1.88, 1.85, 1.80, 1.75, 
           1.66)


# CALCOLO DELLE FREQUENZE ASSOLUTE E RELATIVE

table(gasolio) 

#frequenze relative (approssimazione a 3 cifre):

round(table(gasolio)/length(gasolio),3) 


#frequenze assolute cumulate:

cumsum(table(gasolio))

#frequenze relative cumulate (approssimazione a 3 cifre):

round(cumsum(table(gasolio)/length(gasolio)),3) 

#CACOLO DELLE MODALITÀ DEL CARATTERE

plot(table(gasolio), col=1:50, ylab="fr. ass. costo")

#plot con frequenze relative

plot(table(gasolio)/length(gasolio), col=1:50, ylab="fr. rel costo")


#DISEGNO ISTOGRAMMA RELATIVO AI DATI

table(cut(gasolio,breaks=c(1.1,1.21,1.31,1.41,1.51,1.61,1.71,1.81,2.1), right=FALSE))
brk<-c(1.1,1.21,1.31,1.41,1.51,1.61,1.71,1.81,2.1)
hist(gasolio, breaks=brk,main="Istogramma")


#CALCOLO INDICI DI POSIZIONE

media<-mean(gasolio)
media

getmode<-function(v){
  uniqv<-unique(v) # crea un vettore senza duplicati
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
moda<-getmode(gasolio)
moda


mediana<-median(gasolio)
mediana


#calcolo dei quartili: dividono in 4 parti il nostro set di dati

quartili<-quantile(gasolio)
quartili

#è possibile sintetizzare tutti i risultati ottenuti mostrando
#minimo, massimo, primo quartile, media, mediana, secondo quartile
#e terzo quartile attraverso la funzione summary().

indiciDiPosizioneEQuartili<-summary(gasolio)
indiciDiPosizioneEQuartili



#CALCOLO INDICI DI DISPERSIONE:

varianza<-var(gasolio)
varianza

#calcolo della deviazione standard: rappresenta la radice quadrata della varianza campionaria.

deviazioneStandard<-sd(gasolio)
deviazioneStandard



#CALCOLO INDICI DI FORMA: misurano caratteristiche relative alla forma della distribuzione dei dati. I più usati sono: curtosi e asimmetria(skewness)

n<-length(gasolio)
skewness<-sum((gasolio-media)^3)/(n*var(gasolio)^(3/2))
skewness

#nel caso in considerazione, vi è un assimetria positiva con una distribuzione con coda più lunga a destra

skewnessConFunzione<-skewness(gasolio)
skewnessConFunzione

#calcolo indice di curtosi: si usa per stabilire se la distribuzione è poco o molto appiattita:

curtosiConFunzione<-kurtosis(gasolio)
curtosiConFunzione





#DISEGNO BOX PLOT DEI DATI

#(estremo inferiore)

cardine1<-quantile(gasolio)[[2]]-1.5*(quantile(gasolio)[[4]]-quantile(gasolio)[[2]])
cardine1
#(estremo superiore) 

cardine2<-quantile(gasolio)[[4]]+1.5*(quantile(gasolio)[[4]]-quantile(gasolio)[[2]])
cardine2

gasolioNuovo<-ifelse((gasolio<cardine1)|(gasolio>cardine2),0,gasolio)
gasolioNuovo

#rappresentazione

boxplot(gasolio,xlab="costo della gasolio",col="green")



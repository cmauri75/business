rm(list=ls(all=TRUE))

data=read.table('DATA_4.03_MNT.csv',sep=',',header=TRUE)

summary(data)

#calcolo il modello lineare su tutte le variabili, tranne broken
linregmodel = lm(lifetime~.-broken,data=data)
#sembra che il teamC e soprattutto il provider3 siano coloro che fanno rompere più pezzi
#ma non è affidabile in quanto quelli non rotti hanno un lifetime non predefinito
summary(linregmodel)

library(survival)

#definisco quali sono le variabili indipendenti, in questo caso è doppia
dependantvars = Surv(data$lifetime, data$broken)
#calcolo la mia regressione per dati survivar, tra le variabili indipendenti e quelli dipendenti che specifico una d una
survreg = survreg(dependantvars~pressureInd+moistureInd+temperatureInd+team+provider, dist="gaussian",data=data)
summary(survreg)

#Ora provo a fare delle previsioni future, in particolare su quando si romperanno, basate sul mio modello
Ebreak=predict(survreg, newdata=data, type="quantile", p=.5)

#tabellizzo e calcolo i giorni previsti di vita rimanente (per i non rotti, per gli altri da l'errore di previsione)
Forecast=data.frame(Ebreak)
Forecast$lifetime=data$lifetime
Forecast$broken=data$broken
Forecast$RemainingLT=Forecast$Ebreak-data$lifetime

#calcolo dove agire prima
Forecast=Forecast[order(Forecast$RemainingLT),]
ActionsPriority=Forecast[Forecast$broken==0,]

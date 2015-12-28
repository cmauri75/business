rm(list=ls(all=TRUE))

#carico sia i vecchi che i nuovi dati
dataold=read.table('DATA_3.01_CREDIT.csv',sep=',',header=TRUE)
datanew=read.table('DATA_4.01_CREDIT2.csv',sep=',',header=TRUE)

summary(dataold)
summary(datanew)

#calcolo un modello regressione lineare sui dati di sample
#prima l'avevo usato per trovare relazioni tra cause e coseguenze
#ora vedo di usare queste conoscenze per predirre i dati futuri
linreg=lm(Rating~.,data=dataold)

#ora uso predict per testare il mio modello su un insieme di dati
#questo crea una previsione del Rating per ogni riga dei nuovi dati
#la variabile da predirre è quella specificata in LM come variabile indipendente
predCreditScore = predict(linreg,newdata=datanew,type="response")

#controllo la correlazione tra i dati del modello e quelli originali, vedo che sono ben correlati
#plottandoli vedo che da 200 in su sono ben legati
cor(linreg$fitted.values,dataold$Rating)
plot(dataold$Rating,linreg$fitted.values)

#faccio lo stesso tra i dati predetti e quelli reali (ce li ho perchè è un caso di test) buona anche qui
#sempre da 200 in su
cor(predcreditscore,datanew$Rating)
plot(datanew$Rating,predcreditscore)

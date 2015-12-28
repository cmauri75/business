rm(list=ls())

data=read.table('DATA_4.04_CHOC.csv',sep=',',header=TRUE)
summary(data)

#plotto le vendite per tempo, per darmi un'idea
plot(data$time,data$sales,main="Chocolate sales over time",xlab="Time (in month)",ylab="Monthly sales",ylim=c(0,max(data$sales*1.2)),type='l')

#solito modello vendite per mese
regres=lm(sales~month,data=data)
summary(regres)

#plotto le vendite per mese, mi esce un diagramma a box, questo perchè il mese è fattorizzato
#vedo bene la distribuzione nelle vendite. A dicembre ho vendite alte ma anche molta variabilità
plot(data$month,data$sales,main="Chocolate sales by month",xlab="Month",ylab="Monthly sales",ylim=c(0,max(data$sales*1.2)))

#riplotto come prima ma sovrappongo i dati del mio modello (che si ripete sempre nel tempo perchè considera solo il mese)
plot(data$time,data$sales,main="Chocolate sales over time",xlab="Time (in month)",ylab="Monthly sales",ylim=c(0,max(data$sales)*1.2),type='l')
lines(data$time,regres$fitted.values,type='l',col='blue',lty=2)
legend("topleft",c("Actual sales","Sales by the model"),lty=c(1,2),col=c('black','blue'))


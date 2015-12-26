rm(list=ls(all=TRUE))

datatot=read.table('DATA_3.02_HR2.csv', header = T,sep=',')

summary(datatot)

#controllo come stanno i dati dell'abbandono
table(datatot$left)
table(datatot$left)/nrow(datatot)
hist(datatot$left)

#calcolo le correlazioni tra tutte le variabili per iniziare a darmi un'idea
#la soddisfazione è correlata negativamente, il numero di progetti leggermente positivo e il newborn un po' positivo
#le correlazioni però sono analizzate in modo separato, senza capire come interagiscono tra di loro. (in verità il numero di progetti aiuta a non far lasciare)
cor(datatot)

#qui uso un modello di regressione logica, ottengo una probabilità di abbandono
logreg = glm(left ~ ., family=binomial(logit), data=datatot)

#controllo come sono distribuite le frequenze calcolate
hist(logreg$fitted.values)
#controllo che vi sia una buona correlazione, va abbastanza bene
cor(logreg$fitted.values,datatot$left)

#definisco un limite che mi dice se una probabilità va considerata come uno che se ne va o che rimane
cutoff=.5
#calcolo le percentuali di successo per on e off, le vorrei entrambe alte
sum((logreg$fitted.values<=cutoff)&(datatot$left==0))/sum(datatot$left==0)
sum((logreg$fitted.values>cutoff)&(datatot$left==1))/sum(datatot$left==1)
#calcolo la percentuale di classificazione corretta (in base al cutoff, così posso tunarlo)
mean((logreg$fitted.values>cutoff)==(datatot$left==1))

#così vedo che 0.3 è un  buon valore, a me interessa tra l'altro tenere la gente, quindi allargo le maglie
res = matrix(ncol = 3)
for (v in seq(0,1, by=0.005)){
    res <- rbind(res,c(v,
                       sum((logreg$fitted.values<=v)&(datatot$left==0))/sum(datatot$left==0),
                       sum((logreg$fitted.values>v)&(datatot$left==1))/sum(datatot$left==1)
    ))
}
plot(res[,1],res[,2])
plot(res[,1],res[,3])

#controllo il modello, in questo caso tutti i valori sembrano importanti (molti dati)
# z mi dice quanto sono importanti, quindi soddisfazione, Time in Company e #Projects
summary(logreg)


#provo a plottare ma ottengo un brutto diagramma, ho tante palle esattamente sovrapposte
plot(datatot$TIC,datatot$left,main= "Time and Employee Attrition", ylab="Attrition", xlab= "Time spent")

#rifaccio il plot usando al posto di si/no la percentuale di persone che ha mollato sul totale
tempdata=datatot
#raggruppo per il tempo in azienda e sopra faccio la media dei left (come split)
aggbTimeRank=aggregate(left~ TIC, data=tempdata, FUN=mean)
#adesso posso graficare
plot(aggbTimeRank$TIC,aggbTimeRank$left,main= "Abbandono", ylab="Media abbandoni", xlab= "Tempo in azienda")
#se non se ne vanno dopo 5 anni poi rimangono


#posso migliorare mostrando il valore assoluto del numero di persone
#come sopra, ma qui conto il numero di persone
cntbTimeRank=aggregate(left~ TIC, data=tempdata, FUN=length)
#mostro il grafico
symbols(aggbTimeRank$TIC,aggbTimeRank$left,circles=cntbTimeRank$left, inches=.75, fg="white", bg="red",main= "Time and Employee Attrition", ylab="Average Attrition Rate", xlab= "Time spent")

#ora ragiono sulla soddisfazione
tempdata=datatot
#raggruppo i livelli di soddisfazione, per ogni categoria calcolo la media della soddisfazion e trovo il numero di persone nel gruppo, quindi plotto
tempdata$rankSatis = round(rank(-tempdata$S)/600)
aggbSatisRank = aggregate(left~ rankSatis, data=tempdata, FUN=mean)
cntbSatisRank = aggregate(left~ rankSatis, data=tempdata, FUN=length)
symbols(aggbSatisRank$rankSatis,aggbSatisRank$left,circles=cntbSatisRank$left, inches=.2, fg="white", bg="red",main= "Satisfaction and Employee Attrition", ylab="Average Attrition Rate", xlab= "Rank of Satisfaction")



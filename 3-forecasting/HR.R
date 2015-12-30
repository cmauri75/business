rm(list=ls(all=TRUE))

dataold=read.table('DATA_3.02_HR2.csv', header = T,sep=',')
datanew=read.table('DATA_4.02_HR3.csv', header = T,sep=',')

#eseguo usa regressione lineare binomiale sui dati sample
logreg = glm(left ~ ., family=binomial(logit), data=dataold)

#provo a predirre i dati di abbandono basati sul mio modello
probToLeave=predict(logreg,newdata=datanew,type="response")

#metto in tabella i miei dati ipotizzati per migliore leggibilità
#e ci metto affianco il Last Project Evaluation
predAttrition = data.frame(probToLeave)
predAttrition$performance=datanew$LPE

#ora posso plottare per ragionarci, devo lavorare sulle persone nel quadrante alto a destra
plot(predAttrition$probToLeave,predAttrition$performance)

#per dare priorità uso come parametro il prodotto tra la performance e la probabilità di andarsene
predAttrition$priority=predAttrition$performance*predAttrition$probToLeave
orderPredAttrition=predAttrition[order(predAttrition$priority,decreasing = TRUE),]

head(orderPredAttrition,20)

best <- predAttrition[predAttrition$performance>0.9,]

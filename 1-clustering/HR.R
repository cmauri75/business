
#svuoto l'ambiente
rm(list=ls(all=TRUE))

#statisfation, evaluation, project worked, numero ore lavorate, tempo in azienda, nuovo bimbo negli ultimi 12 mesi
data=read.table('DATA_2.02_HR.csv', header = T,sep=',')

#rimuovo newborn dall'analisi perchè binario, questo crea artefatti nel clustering
#in pratica la distanza è sempre 1, quindi sicuramente verranno formati 2 cluster, uno con gli 0 e gli altri con 1
#anche se in effetti non c'è una vera clusterizzazione sugli altri valori
testdata = scale(data[,1:5])

#calcolo la distanza tra i punti col metodo euclideo
d = dist(testdata, method = "euclidean")

#calcolo i cluster
hcward = hclust(d, method="ward.D")

#riduco i cluster a meno e aggiungo la colonna ai miei dati originali
data$groups<-cutree(hcward,k=4)


#aggrego i miei dati in una tabella, usando la media
aggdata = aggregate(.~ groups, data=data, FUN=mean)

#conto il numero di valori in ogni gruppo, posso usare una colonna qualsiasi
#ne faccio la percentuale e la aggiungo come colonna ai dati
#quindi ordino desc per tale valore
proptemp=aggregate(S~ groups, data=data, FUN=length)
aggdata$proportion=(proptemp$S)/sum(proptemp$S)
aggdata=aggdata[order(aggdata$proportion,decreasing=T),]

write.csv2(aggdata, "HRResults.csv")

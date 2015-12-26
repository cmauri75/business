
#svuoto l'ambiente
rm(list=ls(all=TRUE))

#statisfation, evaluation, project worked, numero ore lavorate, tempo in azienda, nuovo bimbo negli ultimi 12 mesi
data=read.table('DATA_2.03_Telco.csv', header = T,sep=',')

testdata = scale(data)

#calcolo la distanza tra i punti col metodo euclideo
d = dist(testdata, method = "euclidean")

#calcolo i cluster
hcward = hclust(d, method="ward.D")

#riduco i cluster a meno e aggiungo la colonna ai miei dati originali
data$groups<-cutree(hcward,k=8)


#aggrego i miei dati in una tabella, usando la media
aggdata = aggregate(.~ groups, data=data, FUN=mean)

#conto il numero di valori in ogni gruppo, posso usare una colonna qualsiasi
#ne faccio la percentuale e la aggiungo come colonna ai dati
#quindi ordino desc per tale valore
proptemp=aggregate(Calls~ groups, data=data, FUN=length)
aggdata$proportion=(proptemp$Calls)/sum(proptemp$Calls)
aggdata=aggdata[order(aggdata$proportion,decreasing=T),]

palette(rainbow(12, s = 0.6, v = 0.75)) # Select the colors to use
stars(aggdata[,2:(ncol(data))], len = 0.6, key.loc = c(11, 6),xlim=c(2,12),main = "Segments", draw.segments = TRUE,nrow = 2, cex = .75,labels=aggdata$groups)


#svuoto l'ambiente
rm(list=ls(all=TRUE))

data=read.table('DATA_2.01_SKU.csv', header = T,sep=',')

plot(data$CV, data$ADS, main = "SKU Example", ylab="Vendite medie", xlab= "Coefficente di varianza")

#questa funzione effettua lo scaling dei dati per renderli comparabili tra loro
##posso replicarla con (data-lapply(data,mean))/lapply(data,sd) in pratica ad ogni valore tolgo la media della sua colonna e divido per la varianza della sua colonna
testdata = scale(data)

#calcolo la distanza tra i punti col metodo euclideo
d = dist(testdata, method = "euclidean")

#calcolo i cluster
hcward = hclust(d, method="ward.D")

#riduco i cluster a 3 e aggiungo la colonna ai miei dati originali
data$groups<-cutree(hcward,k=3)

library(lattice)

xyplot(ADS~ CV, main = "Grafico colorato coi cluster", type="p", group=groups, data=data, auto.key=list(title="Group", space = "left", cex=1.0, just = 0.95), par.settings = list(superpose.line=list(pch = 0:18, cex=1)), col=c('blue','green','red','gray','brown','black'))

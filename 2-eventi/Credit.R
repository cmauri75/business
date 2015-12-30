rm(list=ls(all=TRUE))

#income in kdollari, rating (score, la variabile indipendente da predirre), numero cc, età, #anni di studio, gender, studente?, sposato?, etnia, debito medio sulla carta
data=read.table('DATA_3.01_CREDIT.csv',sep=',',header=TRUE)

#View(data)
summary(data)

#creo un istogramma per vedere come sono distribuiti i vari dati
hist(data$Rating)
hist(data$Balance)
hist(data$Income)
tapply(data$Income,data$Ethnicity,mean) #gli africani guadagnano di più

#qui cerco la correlazione tra tutti i dati numerici nella tabella, escludo quindi quelli anagrafici discreti
##trovo delle relazioni tra le varie colonne, ma non mi serve molto (non ho capito bene come mai, in effetti la correlazione tra rating e income/balancing è alta come poi capirò alla fine)
# la corr mi da indicazioni solo sulla forza e la direzione della correlazione
cor(data[,c(1:5,10)])

#creo un modello di regressione lineare tra la mia variabile cercata (Rating) e tutte le altre (.=*, se volevo indagarne una sola la specificavo, se più di una le unico con +)
linreg=lm(Rating~.,data=data)
summary(linreg)

linregSmall=lm(Rating~Income+Cards+Married,data=data)
summary(linregSmall)

#dal modello posso ottenere la previsione dei rate, li intabello per dare un'idea dell'accuratezza
compareTable <- matrix(ncol=2, nrow = 300, dimnames = list(c(1:300),c("Rate","CRate")))
compareTable[,1] <- data$Rating
compareTable[,2] <- linreg$fitted.values
#se la plotto ho la perfezione se ho una retta di 45°
plot(compareTable)

#controllo che vi sia buona correlazione tra i dati , ce l'ho, però questa ha dei limiti (con molti numeri riesco sempre a trovare un buon algoritmo che fitta)
cor(linreg$fitted.values,data$Rating)
#graficamente vedo che per numeri bassi non fitta bene
#infatti:
compareTable <- compareTable[ order(compareTable[,1]), ]
cor(head(compareTable[,1],10),head(compareTable[,2],10))

#controllo i dettagli del mio modello
summary(linreg)
#noto che income, student e balance sono quelli che pesano di più (me lo dicono gli asterischi)
#primo e terzo positivi, il secondo influenza in modo negativo


#Ora posso presentare le relazioni scoperte, nelle prime due si vede bene una correlazione lineare, la terza bo
plot(data$Balance,data$Rating)
plot(data$Income,data$Rating)
plot(data$Student,data$Rating)


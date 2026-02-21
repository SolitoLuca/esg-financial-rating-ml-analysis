rm(list=ls(all=TRUE))


##################################################################
#LIBRARIES

library(glmnet)             ## For glmnet

library(rpart)              ## For rpart
library(rattle)             ## For plotting rpart
library(rpart.plot)
library(vip)

library(ipred)              ## For Bagging
library(ranger)             ## For Random Forests
library(gbm)                ## For GBM

library(caret)              ## For classification stats
library(verification)       ## For ROC based analysis

##################################################################

source("C://Users//lucas//Desktop//TESI//FILE DEFINITIVO//DS4E-Functions (1).R")

##############loaded and read file data in formato XLSX

library(openxlsx)


DATA1<-read.xlsx("C://Users//lucas//Desktop//TESI//FILE DEFINITIVO//SP 500 2022.xlsx",
                 detectDates = TRUE,na.strings = c("NA","@NA"),startRow = 2)

DATA2<-read.xlsx("C://Users//lucas//Desktop//TESI//FILE DEFINITIVO//ALTRE VARIABILI FIN 2022.xlsx",
                 detectDates = TRUE,na.strings = c("NA","@NA"),startRow = 2)

DATA1<-DATA1[,-20]  # levata colonna doppiona Price earn ratio

summary(DATA1)
DATA1<-DATA1[,-35]  #levata colonna doppiona Business model


DATA<-merge(DATA1,DATA2,by="Symbol")

colnames(DATA)

DATA<-DATA[,-49] #levata colonna doppiona "NAME"

DATA$`S&P.Rating.Text`<- factor(DATA$`S&P.Rating.Text`)
summary(DATA$`S&P.Rating.Text`)

summary(DATA)

#####################ANALISI ESPLORATIVA DEI DATI (EDA)###########################

##########PASSAGGIO 1: AGGREGARE VARIABILI TVL ESG

#lascio solo var ESG che hanno più valori
DATA<-DATA[,-c(13,33,34,36,37,38,42,47,48)]

#####NOTA: SABS SI BASANO SU 5 MACROCATEGORIE
#ENVIRONMENT,SOCIAL CAPITAL,HUMAN CAPITAL, LEADERSHIP E GOVERNANCE, BUSINESS MODEL
#PER 26 SOTTOCATEGORIE (DI CUI 9 ELIMINATE PER TROPPI NA)
#creiamo indici sintetici macrocategorie facendo media di queste sottocategorie:

media<-rowMeans(DATA[,c(10,11,12,39)],na.rm=TRUE)
media2<-rowMeans(DATA[,c(14,17,31)],na.rm=TRUE)
media3<-rowMeans(DATA[,c(13,33,34)],na.rm=TRUE)
media4<-rowMeans(DATA[,c(15,32,35)],na.rm=TRUE)
media5<-rowMeans(DATA[,c(36,38,16)],na.rm=TRUE)

#aggiungo il vettore come colonna (variabile) del dataset
DATA[,"ESG Insight Score Environment"]<-media
DATA[,"ESG Insight Score Social Capital"]<-media2
DATA[,"ESG Insight Score Human Capital"]<-media3
DATA[,"ESG Insight Score Governance"]<-media4
DATA[,"ESG Insight Score Business Model"]<-media5

colnames(DATA)
#summary(DATA)

###########RIORDINO le colonne del dataset in modo più leggibile,
#e RIMUOVO variabili ESG ridondanti (levato anche TVL ESG SEC che non serve)

NEWDATA<-DATA[,c(1,2,4:7,45,43,44,46,8,9,41,42,47:51, 18:30)]

colnames(NEWDATA)

#######ICB SUPERSECTOR: supersettori su cui vengono rilevati FTSE ESG SUPERSECTOR
#ho i codici dei supersettori, li converto nei loro nomi, trovati in un documento di FTSE

library(car)
table(NEWDATA$FTSE.ESG.Super.Sec)

NEWDATA$FTSE.ESG.Super.Sec<- recode(NEWDATA$FTSE.ESG.Super.Sec," 1010 = 'Tech';
                                    1510 = 'Telecom';
                                    2010 = 'Healt Care';
                                    3010 = 'Banks';
                                    3020 = 'Finance';
                                    3030 = 'Insurance';
                                    3510 = 'Real Estate';
                                    4010 = 'Automobiles';
                                    4020 = 'Consumer P&S';
                                    4030 = 'Media';
                                    4040 = 'Retail';
                                    4050 = 'Travel';
                                    4510 = 'FBT';
                                    4520 = 'PDG';
                                    5010 = 'Construction';
                                    5020 = 'Industrial G&S';
                                    5510 = 'Basic Resources';
                                    5520 = 'Chemicals';
                                    6010 = 'Energy';
                                    6510 = 'Utilities'")

table(NEWDATA$FTSE.ESG.Super.Sec)

#######AGGIUNTE ULTERIORI VARIABILI FINANZIARIE

DATA3<-read.xlsx("C://Users//lucas//Desktop//TESI//FILE DEFINITIVO//ALTRE VAR FIN 2 2022.xlsx",
                 detectDates = TRUE,na.strings = c("NA","@NA"),startRow = 2)

colnames(NEWDATA)
colnames(DATA3)

DATA<-merge(NEWDATA,DATA3, by = "Symbol")

colnames(DATA)

DATA<-DATA[,-33] #levata colonna doppiona "NAME"

##########PASSAGGIO 2: VARIE PULIZIE E RICODIFICHE DATI

summary(DATA)

#####variabili da rendere categoriche:

DATA$TVL.ESG.Rank.All.Cat.6.months.ago<-factor(DATA$TVL.ESG.Rank.All.Cat.6.months.ago)
summary(DATA$TVL.ESG.Rank.All.Cat.6.months.ago)

DATA$TVL.ESG.Rank.All.CaT..12.months.ago<-factor(DATA$TVL.ESG.Rank.All.CaT..12.months.ago)
summary(DATA$TVL.ESG.Rank.All.CaT..12.months.ago)

NEWDATA$`S&P.Rating.Text`<- factor(NEWDATA$`S&P.Rating.Text`)
table(NEWDATA$`S&P.Rating.Text`)

DATA$FTSE.ESG.Super.Sec<-factor(DATA$FTSE.ESG.Super.Sec)
summary(DATA$FTSE.ESG.Super.Sec)   #elenco dei ICB supersector

DATA$TVL.ESG.IND<-factor(DATA$TVL.ESG.IND)
summary(DATA$TVL.ESG.IND)  #elenco dei SICS industry

summary(DATA)
colnames(DATA)
str(DATA)

DATA<-DATA[,-29] #tolta price/ book value

colnames(DATA)

###########rinominate le variabili in modo da essere piu leggibili

names(DATA)[1]<-"SYMBOL"
names(DATA)[2]<-"NAME"
names(DATA)[3]<-"MARKET.VALUE"
names(DATA)[4]<-"CLOSE.PRICE"
names(DATA)[5]<-"SP.RATING"
names(DATA)[6]<-"FTSE.ESG.RAT."
names(DATA)[7]<-"FTSE.ESG.RAT.1.y.AGO"
names(DATA)[8]<-"ICB.SUPERSEC"
names(DATA)[9]<-"FTSE.ESG.RAT.SUPERSEC"
names(DATA)[10]<-"FTSE.ESG.RAT.SUPERSEC.1.y.AGO"
names(DATA)[11]<-"TVL.ESG.RAT."
names(DATA)[12]<-"TVL.ESG.RAT.1.y.AGO"
names(DATA)[13]<-"TVL.ESG.TOT.SCORE"
names(DATA)[14]<-"SICS.INDUSTRY"
names(DATA)[15]<-"TVL.ESG.SCORE.ENVIRON."
names(DATA)[16]<-"TVL.ESG.SCORE.SOC.CAP"
names(DATA)[17]<-"TVL.ESG.SCORE.HUM.CAP"
names(DATA)[18]<-"TVL.ESG.SCORE.GOV."
names(DATA)[19]<-"TVL.ESG.SCORE.BUSS.MODEL"
names(DATA)[20]<-"NET.SALES"
names(DATA)[21]<-"EBITDA"
names(DATA)[22]<-"EBIT"
names(DATA)[23]<-"TOT.ASSET"
names(DATA)[24]<-"TOT.DEBT.FIN."
names(DATA)[25]<-"NET.INCOME"
names(DATA)[26]<-"ROAE"
names(DATA)[27]<-"ROAIC"
names(DATA)[28]<-"PRICE.ON.EARNS.RATIO"
names(DATA)[29]<-"CURR.RATIO"
names(DATA)[30]<-"EBITDA.MARGIN"
names(DATA)[32]<-"FREE.CASH.FLOW"
names(DATA)[33]<-"PRICE_SALE.SHARE"
names(DATA)[34]<-"EARNS.PER.SHARE."
names(DATA)[35]<-"TOT.EQUITY"

summary(DATA)

colnames(DATA)

#############ricodificare SP RATING: investment grade e speculative grade

str(DATA)

levels(DATA$SP.RATING)  #mi da elenco delle categorie

DATA$SP.RATING<-recode(DATA$SP.RATING," 'A' = 'investment grade';
                       'A-' = 'investment grade';
                       'A+' ='investment grade';
                       'B' = 'speculative grade';
                       'B-' = 'speculative grade';
                       'B+' = 'speculative grade';
                       'C' = 'speculative grade'; 
                       'D' = 'speculative grade'")

table(DATA$SP.RATING)   #OCCHIO:possibile sbilanciamento del dataset
summary(DATA$SP.RATING)  #91 NA

#############PASSAGGIO 3: COSTRUZIONE VARIABILI FINANZIARIE

####NOTA: tot debt contengono solo debiti finanziari
#creare nuova variabile LIABILITIES (deb fin+ deb comm)

#DATA$TOT.DEBT  #var vecchia
var<-DATA$TOT.ASSET - DATA$TOT.EQUITY  #nuova var

######creata variabile LIABILITIES

DATA<-add_column(DATA,var)
names(DATA)[36]<-"TOT.LIAB."

######COSTRUIAMO QUESTE DUE VARIABILI: TOT EQUITY/ASSET, TOT EQUITY/DEBT
var<-(DATA$TOT.EQUITY/DATA$TOT.ASSET)

var2<-(DATA$TOT.EQUITY/DATA$TOT.LIAB)

#aggiungiamo la prima
DATA<-add_column(DATA,var)
names(DATA)[37]<-"TOT.EQUITY.ON.ASSET"

#sostituiamo tot debt/equity con la seconda:

DATA$`Total.Debt%.Equity`<- var2
names(DATA)[31]<-"TOT.EQUITY.ON.LIAB"

#CREIAMO ANCHE VAR DEBITI COMMERCIALI
var<-DATA$TOT.LIAB.-DATA$TOT.DEBT.FIN.
DATA<-add_column(DATA,var)
names(DATA)[38]<-"TOT.DEBT.COMM."

colnames(DATA)
summary(DATA)


####PROBLEMA: FACSET FA RITORNARE "A SCELTA" RAPPORTI CON DEN NEGATIVO UGUALI AD "NA"
#PRIMO PASSO: VAR ASS FIN SENZA NA O STRETT POSITIVE
#->RAP FIN DA TENERE: ROAIC,CURR RATIO (perchè den:pass.corr.:non possono essere negative)
#EBITDA.MARGIN(den:sales: QUESTE MAI NEGATIVE),TOT.EQUITY.ON.DEBT, TOT.EQUITY.ON.ASSET

#->LEVATE: 

colnames(DATA)
DATA<-DATA[,-c(21,22,26,28)]

colnames(DATA)
summary(DATA)

######CREIAMO ALTRI RAPPORTI FINANZIARI UTILIZZANDO GRANDEZZE ECONOMICHE PRIMITIVE:
#ROS:
var<-DATA$NET.INCOME/DATA$NET.SALES
DATA<-add_column(DATA,var)
names(DATA)[35]<-"ROS"

#INDICE DI DIPENDENZA FINANZIARIA: TOT.LIAB/TOT.ASSET (magari fallo anche sugli altri debt)
var<-DATA$TOT.LIAB./DATA$TOT.ASSET
DATA<-add_column(DATA,var)
names(DATA)[36]<-"TOT.LIAB.ON.ASSET"

#turnover:rotazione cap inv
var<-DATA$NET.SALES/DATA$TOT.ASSET
DATA<-add_column(DATA,var)
names(DATA)[37]<-"TURNOVER"

#rot.DEB.commerciali:
var<-DATA$NET.SALES/DATA$TOT.DEBT.COMM.
DATA<-add_column(DATA,var)
names(DATA)[38]<-"ROT.DEBT.COMM."

#ROD:
var<-DATA$NET.INCOME/DATA$TOT.LIAB.
DATA<-add_column(DATA,var)
names(DATA)[39]<-"ROD"

#EARNINGS/PRICE(AZIONE)
var<-DATA$EARNS.PER.SHARE./DATA$CLOSE.PRICE
DATA<-add_column(DATA,var)
names(DATA)[40]<-"E_P.SHARE"

#FREE.CASH.FLOW/SALES
var<-DATA$FREE.CASH.FLOW/DATA$NET.SALES
DATA<-add_column(DATA,var)
names(DATA)[41]<-"FCF_SALES"

summary(DATA)


####PROBLEMA: siccome ci sono outlier, necessario gestirli
#utilizzata la tecnica della Winsorizzazione(solo sugli indici finanziari)
#utilizzo funzione desctool(winsorize)->

str(DATA)
summary(DATA)
library(DescTools)

####
boxplot(DATA$ROAIC)
DATA$ROAIC<-Winsorize(DATA$ROAIC,minval = NULL, maxval = NULL, 
                      probs = c(0.01, 0.975), na.rm = TRUE)
boxplot(DATA$ROAIC)  #è in % , coerente con i valori standard, reali
summary(DATA$ROAIC)


####
boxplot(DATA$CURR.RATIO)
sort(DATA$CURR.RATIO)  #non ha val neg, winsorizzo solo da destra
DATA$CURR.RATIO<-Winsorize(DATA$CURR.RATIO,minval = NULL, maxval = NULL, 
                           probs = c(0, 0.975), na.rm = TRUE)
boxplot(DATA$CURR.RATIO)
summary(DATA$CURR.RATIO)    #coerente con i valori standard, reali


####
boxplot(DATA$EBITDA.MARGIN)
#quando il denominatore(fatturato) è negativo: MA NON è MAI NEG
sort(DATA$EBITDA.MARGIN)
DATA$EBITDA.MARGIN<-Winsorize(DATA$EBITDA.MARGIN,minval = NULL, maxval = NULL, 
                              probs = c(0.01, 0.99), na.rm = TRUE)
boxplot(DATA$EBITDA.MARGIN)
sort(DATA$EBITDA.MARGIN)   
summary(DATA$EBITDA.MARGIN)                           # i valori standard reali


###
boxplot(DATA$TOT.EQUITY.ON.ASSET)
sort(DATA$TOT.EQUITY.ON.ASSET)   
DATA$TOT.EQUITY.ON.ASSET<-Winsorize(DATA$TOT.EQUITY.ON.ASSET,minval = NULL, maxval = NULL, 
                                    probs = c(0.01,1), na.rm = TRUE)
boxplot(DATA$TOT.EQUITY.ON.ASSET)   
summary(DATA$TOT.EQUITY.ON.ASSET)


####
boxplot(DATA$TOT.EQUITY.ON.LIAB) 
sort(DATA$TOT.EQUITY.ON.LIAB) 
DATA$TOT.EQUITY.ON.LIAB<-Winsorize(DATA$TOT.EQUITY.ON.LIAB,minval = NULL, maxval = NULL, 
                                   probs = c(0.025, 0.975), na.rm = TRUE)
boxplot(DATA$TOT.EQUITY.ON.LIAB) 
summary(DATA$TOT.EQUITY.ON.LIAB)  #valori coerenti con quelli
#standard, reali (2, 2,5)


####
boxplot(DATA$ROS) 
DATA$ROS<-Winsorize(DATA$ROS,minval = NULL, maxval = NULL, 
                                   probs = c(0.01, 0.99), na.rm = TRUE)
boxplot(DATA$ROS) 
summary(DATA$ROS) 


####
boxplot(DATA$ROD) 
DATA$ROD<-Winsorize(DATA$ROD,minval = NULL, maxval = NULL, 
                    probs = c(0.01, 0.99), na.rm = TRUE)
boxplot(DATA$ROD) 
summary(DATA$ROD) 


####
boxplot(DATA$TOT.LIAB.ON.ASSET) 
DATA$TOT.LIAB.ON.ASSET<-Winsorize(DATA$TOT.LIAB.ON.ASSET,minval = NULL, maxval = NULL, 
                    probs = c(0, 0.995), na.rm = TRUE)
boxplot(DATA$TOT.LIAB.ON.ASSET) 
summary(DATA$TOT.LIAB.ON.ASSET) 


###
boxplot(DATA$TURNOVER) 
DATA$TURNOVER<-Winsorize(DATA$TURNOVER,minval = NULL, maxval = NULL, 
                                  probs = c(0, 0.98), na.rm = TRUE)
boxplot(DATA$TURNOVER) 
summary(DATA$TURNOVER) 


###
boxplot(DATA$ROT.DEBT.COMM.) 
DATA$ROT.DEBT.COMM.<-Winsorize(DATA$ROT.DEBT.COMM.,minval = NULL, maxval = NULL, 
                         probs = c(0, 0.99), na.rm = TRUE)
boxplot(DATA$ROT.DEBT.COMM.) 
summary(DATA$ROT.DEBT.COMM.) 


###
boxplot(DATA$FCF_SALES) 
DATA$FCF_SALES<-Winsorize(DATA$FCF_SALES,minval = NULL, maxval = NULL, 
                               probs = c(0.01, 0.99), na.rm = TRUE)
boxplot(DATA$FCF_SALES) 
summary(DATA$FCF_SALES) 


###
boxplot(DATA$PRICE_SALE.SHARE) 
DATA$PRICE_SALE.SHARE<-Winsorize(DATA$PRICE_SALE.SHARE,minval = NULL, maxval = NULL, 
                               probs = c(0, 0.975), na.rm = TRUE)
boxplot(DATA$PRICE_SALE.SHARE) 
summary(DATA$PRICE_SALE.SHARE) 


###
boxplot(DATA$E_P.SHARE) 
DATA$E_P.SHARE<-Winsorize(DATA$E_P.SHARE,minval = NULL, maxval = NULL, 
                               probs = c(0.01, 0.99), na.rm = TRUE)
boxplot(DATA$E_P.SHARE) 
summary(DATA$E_P.SHARE) 

########facciamo log di alcune var finanziarie
DATA$NET.SALES<-log(DATA$NET.SALES)
DATA$TOT.ASSET<-log(DATA$TOT.ASSET)

#######dataset

data<-DATA
summary(data)  #salvati valori winsorizzati e logaritmizzati

######sintesi caratteristiche del dataset

dim(data)   #503 rows and 40 columns
str(data)
colnames(data) 
NROW(colnames(data))   #40 variables

#######PASSAGGIO 4: GESTIONE VALORI MANCANTI

colSums(is.na(data)) # conteggia i dati mancanti per colonna/variabile

#####DUNQUE ALL'INTERNO DEL DATASET SONO PRESENTI MOLTI NA
#LA NOSTRA VAR DIPENDENTE è SP.RATING

#1) necessario eliminare gli na dalla var dipendente "SP.RATING"


data.alg<-data[!is.na(data$SP.RATING),]
#View(data.alg)

data<-data.alg
summary(data)

##########NOTA IMP:NUMERO NA PRESENTI NEGLI INDICI ESG ########################

colSums(is.na(data))

#FTSE.ESG.RAT. =1;  FTSE.ESG.RAT.2.y.AGO=7; FTSE.ESG.RAT.SUPERSEC=1;
#FTSE.ESG.RAT.SUPERSEC.2.y.AGO=7; TVL.ESG.RAT.=3;  TVL.ESG.RAT.1.y.AGO= 3;
#TVL.ESG.TOT.SCORE= 3

#TVL.ESG.SCORE.ENVIRON.=5; TVL.ESG.SCORE.SOC.CAP= 3;  TVL.ESG.SCORE.HUM.CAP=3;
#TVL.ESG.SCORE.GOV.=4; TVL.ESG.SCORE.BUSS.MODEL = 5

###############################################################################

#2)agli NA delle var indipendneti imputeremo un valore utilizzando la funzione missforest()

library(missForest)

help("missForest")

#hyperparameter: mtry e ntrees
#funziona solo con var num e cat -> elimino NAME e SYMBOL
#eliminare dummies lunghe:ICB SUPERSECTOR e SICS INDUSTRY
#sono tutte variabili non importanti al fine dei modelli

colnames(data)
data<-data[,-c(1,2,8,14)]

#NOTA: ho verificato l inutilità di queste variabili facendo delle regressioni logistiche
##################VERIFICA INUTILITA' DI QUESTE VARIABILI###################
"fit<-glm(formula=SP.RATING ~ SYMBOL + NAME + MARKET.VALUE + 
           CLOSE.PRICE  +               
           FTSE.ESG.RAT.   +             
           FTSE.ESG.RAT.2.y.AGO +     
           ICB.SUPERSEC          +      
           FTSE.ESG.RAT.SUPERSEC  +      
           FTSE.ESG.RAT.SUPERSEC.2.y.AGO +
           TVL.ESG.RAT.                 +
           TVL.ESG.RAT.1.y.AGO          +
           TVL.ESG.TOT.SCORE            +
           SICS.INDUSTRY                +
           TVL.ESG.SCORE.ENVIRON.       +
           TVL.ESG.SCORE.SOC.CAP      +
           TVL.ESG.SCORE.HUM.CAP      +
           TVL.ESG.SCORE.GOV.         +
           TVL.ESG.SCORE.BUSS.MODEL  +
           NET.SALES       +           
           EBITDA                     +  
           EBIT              +           
           TOT.ASSET          +          
           TOT.DEBT            +        
           NET.INCOME           +        
           ROAE                  +       
           ROAIC          +
           PRICE.ON.EARNS.RATIO +        
           CURR.RATIO            +       
           TOT.EQUITY            +
           ONEYEAR.GROWTH.DVS     +      
           EV.ON.EBITDA            +   
           TOT.EQUITY.ON.ASSET +
           TOT.EQUITY.ON.DEBT, data = data, family = binomial)

summary(fit)

#TOGLIERE SYMBOL,NAME,

fit<-glm(formula=SP.RATING ~MARKET.VALUE + 
           CLOSE.PRICE  +               
           FTSE.ESG.RAT.   +             
           FTSE.ESG.RAT.2.y.AGO +     
           ICB.SUPERSEC          +      
           FTSE.ESG.RAT.SUPERSEC  +      
           FTSE.ESG.RAT.SUPERSEC.2.y.AGO +
           TVL.ESG.RAT.                 +
           TVL.ESG.RAT.1.y.AGO          +
           TVL.ESG.TOT.SCORE            +
           SICS.INDUSTRY                +
           TVL.ESG.SCORE.ENVIRON.       +
           TVL.ESG.SCORE.SOC.CAP      +
           TVL.ESG.SCORE.HUM.CAP      +
           TVL.ESG.SCORE.GOV.         +
           TVL.ESG.SCORE.BUSS.MODEL  +
           NET.SALES       +           
           EBITDA                     +  
           EBIT              +           
           TOT.ASSET          +          
           TOT.DEBT            +        
           NET.INCOME           +        
           ROAE                  +       
           ROAIC          +
           PRICE.ON.EARNS.RATIO +        
           CURR.RATIO            +       
           TOT.EQUITY            +
           ONEYEAR.GROWTH.DVS     +      
           EV.ON.EBITDA            +   
           TOT.EQUITY.ON.ASSET +
           TOT.EQUITY.ON.DEBT, data = data, family = binomial)
summary(fit)"

#TOGLIERE SICS E ICB:TROPPE DUMMY
##############################FINE VERIFICA########################################


#maxiter utile, fissa il numero max di iterazioni
#VERBOSE=TRUE utile, mi dice quante iterazioni fa
#nodesize messi i valori di default

#it gives an OUT OF BAG (OOB) IMPUTATION ERROR ESTIMATE

#the NRMSE(normalized root mean squared error) is computed over continuos missing values only
#->NRMSE= ERRORE DI IMPUTAZIONE VARIABILI CONTINUE (DA MIN)
#The proportion of falsely classified (PFC) is also computed over the categorical missing values only.
#->PFC= ERRORE DI IMPUTAZIONE VARIABILI CATEGORICHE (DA MIN)

summary(data)
str(data)


imputation<-missForest(data, maxiter = 10, ntree = 100, variablewise = FALSE,
                       decreasing = FALSE, verbose = TRUE,
                       mtry = floor(sqrt(ncol(data))), replace = TRUE,
                       classwt = NULL, cutoff = NULL, strata = NULL,
                       sampsize = NULL, nodesize = c(1,5), maxnodes = NULL,
                       xtrue = NA, parallelize = c('no'))

imputation    #vettore 2 elementi: dataset riempito e errori
View(imputation)

#OSSERVAZIONI/PROVE
#con nodesize = c(1,5) (questi sono i valori standard)->meglio, lo tengo fisso

#con ntrees=200: aumentano gli errori, il tempo; diminuisce le iterazioni  ->no
#con ntrees=150: variano leggermente errori, aumenta tempo e iter ->no 
#con ntrees=50:migliorano errori, diminuisce tempo e iter-> meglio

####BEST TUNE: ntree=50,verbose=TRUE,nodesize=c(1,5),mtry=radice(n var), parallelize=no


#####################RISULTATI BEST MISSFOREST: 
## The imputation is finished after 5 iterations having a final
## true NRMSE of 0.00006104775  and a PFC of  0.2176039  

#ottenere dataset riempito:
truedata<-imputation[1]  #ho assegnato il dataset
truedata<-data.frame(truedata)  #letto come dataframe
#View(truedata)  #la missforest ha cambiato i nomi alle variabili ->
colnames(truedata)  #rinomino variabili

colnames(truedata)<-substr(colnames(truedata), 6,200)

colnames(truedata)
summary(truedata)


data<-truedata
summary(data) #salvato il dataset riempito
colnames(data)

save.image(file="TESI DATASET 2022.RData")   #non lo pigiare più

######NON MODIFCARE PIU NIENTE DI QUESTO SCRIPT###################################










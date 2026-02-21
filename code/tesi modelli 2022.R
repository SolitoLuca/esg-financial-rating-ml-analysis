##########################FILE##########################

####clean memory

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
library(openxlsx)
library(car)
library(DescTools)
library(missForest)

##################################################################

source("C://Users//lucas//Desktop//TESI//FILE DEFINITIVO//DS4E-Functions (1).R")

###################################################################

load("TESI DATASET 2022.Rdata")
imputation  #NRMSE: 0.00006104775  , PFC: 0.2176039 

###salviamo il dataset in un file xlsx per sicurezza:
write.xlsx(data,"C:\\Users\\lucas\\Desktop\\TESI\\FILE DEFINITIVO\\TESI DATASET 2022.xlsx")
###

summary(data) #salvato il dataset riempito

#######MATRICE DI COEFF DI CORRELAZIONE DEI RATIO FINANZ: serve per capire se
# ci sono variabili altamente correlate tra loro, nel caso sceglierne una 

ratios<-data[,c(20:23,25,29,31:37)]
colnames(ratios)

matrice_corr<-cor(ratios)
#View(matrice_corr)

matrice_corr[7,]  #ROS
matrice_corr[3,]  #EBITDA.MARGIN
################altamente correlati (0.7) ma possiamo lasciarli

matrice_corr[4,]  #TOT EQUITY ON LIAB
matrice_corr[6,]  #TOT EQUITY ON ASSET
matrice_corr[8,]  #TOT LIAB ON ASSET
##################LASCIAMO SOLO tot equity on liab, GLI ALTRI LEVIAMOLI

matrice_corr[9,] #TURNOVER
matrice_corr[10,] #ROT.DEB.COMMERCIALI
################altamente correlati (0.7) ma possiamo lasciarli

########questi sono i ratio da mettere nelle logistiche######

matrice<-matrice_corr[-c(6,8),-c(6,8)]
#View(matrice)

###########################ANALISI PARAMETRICA#################################

#########INNANZITUTTO DIAMO UN OCCHIO ALLA REFERENCE DELLA VAR.DIP.

help("relevel")

table(data$SP.RATING)
levels(data$SP.RATING)  #controlliamo quale sia la 0 e la 1

#ricodifica SP.RATING in binaria (0,1)
data$SP.RATING<-factor(x=data$SP.RATING, levels = c("speculative grade","investment grade"), 
                       labels = c(0,1))

levels(data$SP.RATING)  #controllo:  OK!

################################################################################
## Train and test sets
################################################################################

#### Split
set.seed(100000)
n <- NROW(data)
#### Select a "small" number to make model fitting faster
ind <- sample(x = 1 : n, size = round(0.8 * n))
data.orig  <- data
data.train <- data[ ind, , drop = FALSE]   
data.test  <- data[-ind, , drop = FALSE]

#### Copy
data <- data.train

################################################################################
# Manually estimated models: REGRESSIONE LOGISTICA
#obbiettivo: STABILIRE IL FIT FULL
################################################################################

####NOTA: gli indici ESG "1y ago" non utilizzati perchè altamente correlati
cor(data$FTSE.ESG.RAT.,data$FTSE.ESG.RAT.1.y.AGO)
cor(data$FTSE.ESG.RAT.SUPERSEC,data$FTSE.ESG.RAT.SUPERSEC.1.y.AGO)

#########PRIMA PROVA: USIAMO NELLA LOGIT SOLO I RATIO E GLI ESG PIU ASSOLUTI: 

#PROVIAMO FTSE.ESG E POI TVL.ESG.
###1)FTSE ESG RAT
#colnames(matrice)

table(data$SP.RATING)

fit<-glm(formula=SP.RATING ~               
           FTSE.ESG.RAT.   +
           ROAIC          +
           CURR.RATIO            +       
           EBITDA.MARGIN   +
           FCF_SALES  +
           PRICE_SALE.SHARE   +
           TOT.EQUITY.ON.LIAB  +
           ROT.DEBT.COMM.  +
           ROD  +
           E_P.SHARE  +
           ROS   +
           TURNOVER, data = data, family = "binomial")

summary(fit)

R2<-(fit$null.deviance-fit$deviance)/fit$null.deviance
R2 #0.09451582   AIC: 416.7

#############OSSERVAZIONI:
#FTSE.ESG.RAT. : NON SIGN (segno positivo)
#EBITDA.MARGIN:SIGN AL 99%, SEGNO neg  ->segno strano
#ROT.DEB.COMM.: sign al 90% , segno positivo -> segno strano
#E_P.SHARE: SIGN AL 99,9%, SEGNO neg  ->OK
#ROS: SIGN AL 99,9%, SEGNO POSTIVO  ->OK 
#TURNOVER: SIGN AL 90%, SEGNO NEGATIVO ->segno strano 


####2)USIAMO TVL.ESG. RAT.

table(data$TVL.ESG.RAT.)  
#utilizza come reference(base per il confonto con le altre categorie) "Above Average"
#NECESSARIO CAMBIARE LA REFERENCE: UTILIZZIAMO RELEVEL

data$TVL.ESG.RAT.<-relevel(data$TVL.ESG.RAT., ref = "Laggard")
#adesso come reference ho "Laggard"

fit<-glm(formula=SP.RATING ~               
           TVL.ESG.RAT.   +
           ROAIC          +
           CURR.RATIO            +       
           EBITDA.MARGIN   +
           FCF_SALES  +
           PRICE_SALE.SHARE   +
           TOT.EQUITY.ON.LIAB  +
           ROT.DEBT.COMM.  +
           ROD  +
           E_P.SHARE  +
           ROS   +
           TURNOVER, data = data, family = "binomial")

summary(fit)

R2<-(fit$null.deviance-fit$deviance)/fit$null.deviance
R2 #0.09901447 (aume legg)  AIC: 420.76  (aum legg)  

#############OSSERVAZIONI:
#TVL ESG RAT: NO SIGN, SEGNI COERENTI
#EBITDA.MARGIN:SIGN AL 99%, SEGNO neg  ->segno strano 
#PRICE-SALES.SHARE: DIVENTA SIGN AL 95%, SEGNO NEGATIVO->OK
#ROT.DEB.COMM.: sign al 90%, segno positivo -> segno strano
#E_P.SHARE: SIGN AL 99%, SEGNO neg  ->OK
#ROS: SIGN AL 99%, SEGNO POSTIVO  ->OK 
#TURNOVER: NON PIU SIGN


###3)PROVIAMO A USARE TVL ESG TOT SCORE: 
#misura del track record ESG a lungo termine di un'azienda, simile a un sistema di rating
#(quantitativa)

#aggregate(FUN=mean,x=data$TVL.ESG.TOT.SCORE,by=list(data$TVL.ESG.RAT.))

fit<-glm(formula=SP.RATING ~               
           TVL.ESG.TOT.SCORE   +
           ROAIC          +
           CURR.RATIO            +       
           EBITDA.MARGIN   +
           FCF_SALES  +
           PRICE_SALE.SHARE   +
           TOT.EQUITY.ON.LIAB  +
           ROT.DEBT.COMM.  +
           ROD  +
           E_P.SHARE  +
           ROS   +
           TURNOVER, data = data, family = "binomial")

summary(fit)

R2<-(fit$null.deviance-fit$deviance)/fit$null.deviance
R2 #0.0938858 (DIMINUITO)  AIC: 416.98 (diminuisce legg)  

#############OSSERVAZIONI:
#TVL ESG TOT SCORE: NO SIGN, MA SEGNO COERENTE
#STESSE VAR SIGN E STESSI SEGNI DEL MODELLO 2


######MEGLIO UTILIZZARE LA QUANTITATIVA(TVL.ESG.TOT.SCORE) CHE LA CATEGORICA (TVL.ESG.RAT)

###4)PROVIAMO AD UTILIZZARLI INSIEME: FTSE ESG RAT E TVL ESG TOT SCORE

cor(data$FTSE.ESG.RAT.,data$TVL.ESG.TOT.SCORE) #praticamente incorrelate
# -> ha senso usarle insieme

fit<-glm(formula=SP.RATING ~               
           TVL.ESG.TOT.SCORE   +
           FTSE.ESG.RAT.  +
           ROAIC          +
           CURR.RATIO            +       
           EBITDA.MARGIN   +
           FCF_SALES  +
           PRICE_SALE.SHARE   +
           TOT.EQUITY.ON.LIAB  +
           ROT.DEBT.COMM.  +
           ROD  +
           E_P.SHARE  +
           ROS   +
           TURNOVER, data = data, family = "binomial")

summary(fit)

R2<-(fit$null.deviance-fit$deviance)/fit$null.deviance
R2 #0.09463424 (prat identici)  AIC: 418.65  

######OSSERVAZIONI: 
#VAR ESG NO SIGN MA SEGNO COERENTE
#STESSE VAR SIGN CON STESSI SEGNI

###5)PROVIAMO AD AGGIUNGERE GLI INDICI ESG PIU SPECIFICI: ENVIRONMENT, SOCIAL
#CAPITAL, HUMAN CAPITAL, GOVERNMENT,BUSINESS (AGGIUNTI UNO ALLA VOLTA)

fit<-glm(formula=SP.RATING ~ 
           TVL.ESG.SCORE.ENVIRON. +
           TVL.ESG.SCORE.SOC.CAP +
           TVL.ESG.SCORE.HUM.CAP +
           TVL.ESG.SCORE.GOV. + 
           TVL.ESG.SCORE.BUSS.MODEL +
           ROAIC          +
           CURR.RATIO            +       
           EBITDA.MARGIN   +
           FCF_SALES  +
           PRICE_SALE.SHARE   +
           TOT.EQUITY.ON.LIAB  +
           ROT.DEBT.COMM.  +
           ROD  +
           E_P.SHARE  +
           ROS   +
           TURNOVER, data = data, family = "binomial")

summary(fit)

R2<-(fit$null.deviance-fit$deviance)/fit$null.deviance
R2  #0.09798637     AIC: 423.21

#####CONCLUSIONI: SEGNI VAR ESG STRANI E NON SIGN, NON AGGIUNGONO NIENTE AL MODELLO 
#DI SIGNIFICATIVo
# -> MEGLIO TENERE IL TVL.ESG.TOT.SCORE


###6)PROVIAMO AD AGGIUNGERE UN INDICE FTSE ESG PIU RELATIVO: FTSE.ESG.RAT.SUPERSEC

fit<-glm(formula=SP.RATING ~
           FTSE.ESG.RAT.SUPERSEC +
           ROAIC          +
           CURR.RATIO            +       
           EBITDA.MARGIN   +
           FCF_SALES  +
           PRICE_SALE.SHARE   +
           TOT.EQUITY.ON.LIAB  +
           ROT.DEBT.COMM.  +
           ROD  +
           E_P.SHARE  +
           ROS   +
           TURNOVER, data = data, family = "binomial")

summary(fit)

R2<-(fit$null.deviance-fit$deviance)/fit$null.deviance
R2 #0.09370863 (prat identici)  AIC: 417.05  

############################OSSERVAZIONI:
#FTSE.ESG.RAT.SUPERSEC: NO SIGN SEGNO COERENTE
#STESSE VAR SIGN E STESSI SEGNI
#MA TORNA SIGN TURNOVER AL 90%, SEGNO NEGATIVO.->SEGNO STRANO

###8) VEDIAMO SE HA SENSO USARE FTSE.ESG.RAT.SUPERSEC INSIEME A TVL.ESG.TOT.SCORE E FTSE.ESG.RAT

#####CREIAMO UNA MATRICE DI CORRELAZIONE:
colnames(data)
var<-data[,c(4,6,10)]

nuovamatrice<-cor(var)
#View(nuovamatrice)

####NOTIAMO CHE FTSE.ESG.RAT.E FTSE.ESG.RAT.SUPERSEC SONO MOLTO CORRELATE (0.9):
#DECIDIAMO QUALE TRA LE DUE LASCIARE NEL LARGEST MODEL:

#USIAMO FTSE.ESG.RAT

fit<-glm(formula=SP.RATING ~        #MODELLO UGUALE AL MODELLO 4        
           TVL.ESG.TOT.SCORE   +
           FTSE.ESG.RAT.  +
           ROAIC          +
           CURR.RATIO            +       
           EBITDA.MARGIN   +
           FCF_SALES  +
           PRICE_SALE.SHARE   +
           TOT.EQUITY.ON.LIAB  +
           ROT.DEBT.COMM.  +
           ROD  +
           E_P.SHARE  +
           ROS   +
           TURNOVER, data = data, family = "binomial")

summary(fit)

R2<-(fit$null.deviance-fit$deviance)/fit$null.deviance
R2 #0.09463424 (prat identici)  AIC: 418.65  

#############OSSERVAZIONI:
#TVL ESG RAT: NO SIGN, SEGNI COERENTI
#FTSE ESG RAT: NO SIGN, SEGNI COERENTI
#EBITDA.MARGIN:SIGN AL 99%, SEGNO neg  ->segno strano
#PRICE-SALES.SHARE: SIGN AL 95%, SEGNO NEGATIVO ->OK
#ROT.DEB.COMM.: sign al 90%, segno positivo -> segno strano
#E_P.SHARE: SIGN AL 99%, SEGNO neg  ->OK
#ROS: SIGN AL 99%, SEGNO POSTIVO  ->OK

fit1<-fit

####USIAMO FTSE.ESG.RAT.SUPERSEC

fit<-glm(formula=SP.RATING ~               
           TVL.ESG.TOT.SCORE   +
           FTSE.ESG.RAT.SUPERSEC  +
           ROAIC          +
           CURR.RATIO            +       
           EBITDA.MARGIN   +
           FCF_SALES  +
           PRICE_SALE.SHARE   +
           TOT.EQUITY.ON.LIAB  +
           ROT.DEBT.COMM.  +
           ROD  +
           E_P.SHARE  +
           ROS   +
           TURNOVER, data = data, family = "binomial")

summary(fit)

R2<-(fit$null.deviance-fit$deviance)/fit$null.deviance
R2 #0.09388588 (LEGG PIU PICCOLO)  AIC: 418.98 (prat.identico)  

#####CONCLUSIONE CON OSS.COMPARATE: SICCOME FTSE.ESG.RAT. è UN INDICE PIU ASSOLUTO E CON SEGNO COERENTE
#RISPETTO ALLA CONTROPARTE SUPERSEC, E NOTANDO ANCHE CHE GIà è PRESENTE UN INDICE
#ESG PIU RELATIVO (TVL.ESG.SCORE); CONSIDERANDO CHE R2 E VAR SIGNIFICATIVE SONO LE STESSE 
#TRA I DUE MODELLI; DECIDO DI PRENDERE IL PRIMO.

####OUTCOMES OF FIT.FULL
fit1
summary(fit1)
summary(fit1)$coefficients
NROW(summary(fit1)$coefficients)  #14 features with intercept(13 without)

#### Copy:
fit.full <- fit1

fit.full
summary(fit.full)

################################################################################
## Stepwise model selection
################################################################################

#### Smallest model
form.null <- SP.RATING ~ TVL.ESG.TOT.SCORE + FTSE.ESG.RAT.  #smallest model: has got only intercept
fit.null <- glm(formula = form.null, data = data, family = binomial)
#### Largest model
form.full <- formula(fit.full)

#to check if i do well:

summary(fit.null)  #NESSUN REGRESSORE è SIGN
summary(fit.full)

#### Set k = 2 for AIC, k = log(NROW(data)) for BIC
###use AIC because is more simple to explain than BIC
#### FORWARD
fit.forw <- step(object = fit.null, scope = list(lower = form.null, upper = form.full), 
                 direction = "forward", k = 2) # k = 2)

#### BACKWARD
fit.back <- step(object = fit.full, scope = list(lower = form.null, upper = form.full), 
                 direction = "backward", k = 2) # k = 2)

#in general, backward is better 


#### BOTH
fit.both <- step(object = fit.full, scope = list(lower = form.null, upper = form.full), 
                 direction = "both", k = 2) # k = 2)

####OUTCOMES STEPWISE REGRESSIONS 
#(df=number of coefficients (=features) appended or canceled)

fit.forw
fit.forw$anova
NROW(summary(fit.forw)$coefficients)   #6 features with intercept (5 without)
R2<-(fit.forw$null.deviance-fit.forw$deviance)/fit.forw$null.deviance
R2   #0.04831887 (diminuito molto rispetto alla logistica)  AIC: 422.6  (legg aume)

summary(fit.forw)  
#############OSSERVAZIONI RISPETTO A LARGEST MODEL:
#INTERCETTA: NO SIGN
#TOT.EQUITY.ON.LIAB: DIVENTA SIGN AL 99.9%, SEGNO NEG ->SEGNO STRANO
#EBITDA.MARGIN:NON AGGGIUNTA
#PRICE-SALES.SHARE: NON AGGIUNTA
#ROT.DEB.COMM.: NON AGGIUNTA
#E_P.SHARE: AGGIUNTA SIGN AL 95%, SEGNO neg  ->OK
#ROS: NON AGGIUNTA
#ROAIC: AGGIUNTA, SIGN AL 95%, SEGNO POS ->OK

######NOTA IMP: ESG NON SIGN


fit.back
fit.back$anova  
NROW(summary(fit.back)$coefficients)   #8 features with intercept (7 without)
R2<-(fit.back$null.deviance-fit.back$deviance)/fit.back$null.deviance
R2  #0.08400633 (LEGG DIM AL LARGEST MODEL)  AIC: 411.24 (DIM) 


summary(fit.back)
#############OSSERVAZIONI RISPETTO LARGEST MODEL: 
#INTERCETTA: DIVENTA SIGN AL 99%, SEGNO POS ->OK
#EBITDA.MARGIN:SIGN AL 99%, SEGNO neg  ->segno strano
#PRICE-SALES.SHARE: SIGN AL 95%, SEGNO NEGATIVO ->OK
#TOT.EQUITY.ON LIAB: AGGIUNTA, DIVENTA SIGN AL 99%, SEGNO NEG ->SEGNO STRANO
#E_P.SHARE: SIGN AL 99,9%, SEGNO neg  ->OK
#ROS: SIGN AL 99,9%, SEGNO POSTIVO  ->OK

######NOTA IMP: ESG NON SIGN

fit.both
fit.both$anova
NROW(summary(fit.both)$coefficients)   #6 features with intercept (5 without)
R2<-(fit.both$null.deviance-fit.both$deviance)/fit.both$null.deviance
R2  #0.08211008  AIC: 408.06

summary(fit.both)   #IDENTICA ALLA BACKWARD 

#CONCLUSIONE: PRENDIAMO COME MIGLIORE MODELLO STEPWISE LA BACKWARD

################################################################################
## Regularized Regression (type.measure = "deviance" for logistic regression)
################################################################################

######## Global settings
#### Dependent variable
y <- data$SP.RATING

#### Matrix of independent variables
form <- formula(fit.full) 
xmat <- model.matrix(object = form, data = data)[, -1, drop = FALSE]

#to check:
form

#### Manual choice of the set of lambda's to try
lambda.all <- exp( seq(from = -8.5, to = 4, by = 0.2) )
lambda.all
#63 values of lambda (penalty parameter)

################################ RIDGE ##########################################

alpha <- 0

#### Fit (many other arguments omitted)
fit <- glmnet(x = xmat, y = y, family = "binomial", alpha = alpha, 
              lambda = lambda.all, standardize = TRUE)
fit.ridge.all <- fit
#### Print output (Explain columns Df and %Dev)
fit

#dunque, per ogni lambda del range impostato, viene creato un modello con
#caratteristiche differenti: infatti impostati 63 lambda, si ottengono 63
#modelli diversi: quale è il  migliore lambda, 
# e quindi il miglior modello stimato?

#### Plot trajectories of coefficients (DA RIGUARDARE)
par(mfrow = c(1,3))
plot(x = fit, xvar = "lambda", label = TRUE) ## lambda
#DEFINISCE DOPO QUALE LOG(LAMBDA) I COEFFICIENTI TENDONO A ZERO: 
#A LOG(LAMBDA)=3, I COEFFICIENTI CROLLANO A ZERO

plot(x = fit, xvar = "norm",   label = TRUE) ## L1-norm
#SOMMA DEI VALORI ASSOLUTI DEI COEFFICIENTI(SENZA PENALTY PARAMETERE LAMBDA)

plot(x = fit, xvar = "dev",    label = TRUE) ## R^2
#ALL AUMENTARE DEL VALORE DEI COEFFICIENTI, AUMENTA L R2

#### Which one is the best lambda?
cvfit <- cv.glmnet(x = xmat, y = y, alpha = alpha, lambda = lambda.all, 
                   family = "binomial", type.measure = "deviance", nfolds = 10)
par( mfrow = c(1,1) )
plot(cvfit)   #NOTA: qui la binomial deviance è molto ampia causa basso R2

cvfit   


## Print best lambdas
cat("min(lambda) = ", cvfit$lambda.min, "1se(lambda) = ", cvfit$lambda.1se, "\n")


#### Refit the best lambda in ridge(alpha=0: già imposto in precedenza)
lambda <- cvfit$lambda.min  #= 0.011109    
fit <- glmnet(x = xmat, y = y, family = "binomial", alpha = alpha, 
              lambda = lambda, standardize = TRUE)


####################### Store
lambda.ridge <- lambda
fit.ridge <- fit

fit.ridge     #lambda=0.01111  R2= 8.75     13 (df) features  (without intercept)

fit.ridge$beta

######## Lasso

alpha <- 1

#### Fit (many other arguments omitted)
fit <- glmnet(x = xmat, y = y, family = "binomial", alpha = alpha, 
              lambda = lambda.all, standardize = TRUE)
fit.lasso.all <- fit

#### Plot trajectories of coefficients
par(mfrow = c(1,2))
plot(x = fit.lasso.all, xvar = "lambda", label = TRUE, main = "Lasso") ## lambda Lasso
plot(x = fit.ridge.all, xvar = "lambda", label = TRUE, main = "Ridge") ## lambda Ridge


#### Select the best lambda
cvfit <- cv.glmnet(x = xmat, y = y, alpha = alpha, lambda = lambda.all, 
                   family = "binomial", type.measure = "deviance", nfolds = 10)
par(mfrow = c(1,1))
plot(cvfit)

cvfit
## Print best lambdas
cat("min(lambda) = ", cvfit$lambda.min, "1se(lambda) = ", cvfit$lambda.1se, "\n")

#### Estimate best
lambda <- cvfit$lambda.min   #=0.004086771     
fit <- glmnet(x = xmat, y = y, family = "binomial", alpha = alpha, 
              lambda = lambda, standardize = TRUE)

###################################### Store
lambda.lasso <- lambda
fit.lasso <- fit

fit.lasso    #lambda= 0.004087   R2=8.94       12 features (without intercept)

fit.lasso$beta   ##NOTA: ROD non viene aggiunta al modello


######## Elastic net in between ridge and lasso
alpha <- 0.5
#### Fit (many other arguments omitted)
fit <- glmnet(x = xmat, y = y, family = "binomial", alpha = alpha, 
              lambda = lambda.all, standardize = TRUE)
fit.enet.all <- fit

#### Plot trajectories of coefficients
par(mfrow = c(1,3))
plot(x = fit.lasso.all, xvar = "lambda", label = TRUE, main = "Lasso") ## lambda Lasso
plot(x = fit.ridge.all, xvar = "lambda", label = TRUE, main = "Ridge") ## lambda Ridge
plot(x = fit.enet.all, xvar = "lambda", label = TRUE, main = "Elastic Net")  ## lambda Elastic Net

#### Select the best lambda
cvfit <- cv.glmnet(x = xmat, y = y, alpha = alpha, lambda = lambda.all, 
                   family = "binomial", type.measure = "deviance", nfolds = 10)
par(mfrow = c(1,1))
plot(cvfit)

cvfit
## Print best lambdas
cat("min(lambda) = ", cvfit$lambda.min, "1se(lambda) = ", cvfit$lambda.1se, "\n")

#### Estimate best
lambda <- cvfit$lambda.min   #=0.004991594    
fit <- glmnet(x = xmat, y = y, family = "binomial", alpha = alpha, 
              lambda = lambda, standardize = TRUE)

#### Store
lambda.enet <- lambda
fit.enet <- fit

fit.enet    #lambda=0.004992 R2(%Dev)=9.03       12 features (without intercept)

fit.enet$beta  #NOTA: ROD non viene aggiunta al modello

###CONCLUSIONE: LA MIGLIORE SEMBRA ESSERE LA ELASTIC NET( R2 legg piu ALTO)
#comunque lasso e elastic net hanno valori molto simili

######tabella di confronto coefficienti stimati:

ridge<-as.data.frame(as.matrix(fit.ridge$beta))
str(ridge)
names(ridge)[1]<-"RIDGE"

lasso<-as.data.frame(as.matrix(fit.lasso$beta))
names(lasso)[1]<-"LASSO"

ridge<-add_column(ridge,lasso)

enet<-as.data.frame(as.matrix(fit.enet$beta))
names(enet)[1]<-"ENET"

ridge<-add_column(ridge,enet)

tab<-ridge
tab

####################################################################################
#MODEL ESTIMATION BY ALGORITHMIC APPROACH
###########################################################################################

#PUNTO 1) RIORDINARE VARIABILI CATEGORICHE RIPARTENDO DAL DATASET DI PARTENZA
#(anche se non le uso nei modelli, facciamolo per sicurezza):

data<-data.orig
summary(data)
str(data)


table(data$TVL.ESG.RAT.)
data$TVL.ESG.RAT.<-factor(x=data$TVL.ESG.RAT.,levels =c("Laggard","Below Average",
                                                  "Average","Above Average","Leader"),
                          ordered=TRUE)
#to check:
table(data$TVL.ESG.RAT.)  


table(data$TVL.ESG.RAT.1.y.AGO)
data$TVL.ESG.RAT.1.y.AGO<-factor(x=data$TVL.ESG.RAT.1.y.AGO,levels =c("Laggard","Below Average",
                                                        "Average","Above Average","Leader"),
                          ordered=TRUE)
#to check:
table(data$TVL.ESG.RAT.1.y.AGO) 

#to check:
table(data$SP.RATING)

################################################################################
## Train and test sets
################################################################################

#### Split
set.seed(100000)
n <- NROW(data)
#### Select a "small" number to make model fitting faster
ind <- sample(x = 1 : n, size = round(0.8 * n))
data.orig  <- data
data.train <- data[ ind, , drop = FALSE]   
data.test  <- data[-ind, , drop = FALSE]

#### Copy
data <- data.train

summary(data)

table(data$SP.RATING)  #controllo:  OK!

################################################################################
## RANDOM FORESTS
################################################################################

###prova iniziale standard della RF
fit <- ranger(formula = SP.RATING ~ TVL.ESG.TOT.SCORE + FTSE.ESG.RAT. + 
                ROAIC + CURR.RATIO + EBITDA.MARGIN + FCF_SALES + PRICE_SALE.SHARE + 
                TOT.EQUITY.ON.LIAB + ROT.DEBT.COMM. + ROD + E_P.SHARE + ROS + 
                TURNOVER, data = data, 
              probability = TRUE,                           ## Explain 
              num.trees = 500, mtry = floor(sqrt(13)), 
              min.node.size = 5, min.bucket = NULL, max.depth = NULL,
              importance = "impurity", splitrule = "gini",
              keep.inbag = TRUE, oob.error = TRUE, verbose = TRUE, seed = 2000)

## Main output
fit         ## Summary: the error return in term of brier(similar to MSE)=0.2225752 
fit$treetype              #if correct tell "probability    
fit$splitrule          
fit$confusion.matrix                      ## Only with probability = FALSE


## A Larger set of error measures 
.rf.errorMeasures(fit = fit, data = data)# no with prob=FALSE    
## A richer set of error measures: take oob-me(uguale alla bagging)
#oob-me=0.3636364


## Check the 'type' error measure as function of num.trees: optimize ntrees
par(mfrow = c(1,1), mar = c(4,4,0.2,0.2))   # no with prob=FALSE
error.cum <- .rf.errorPlot(fit = fit, data = data, type = "me") ## "me", "ce", "brier"

##IMPORT:
# a numtrees=100: since this point the misclassification
#error seems stable
#this is the threshold beyond which rf prediction accuracy levels off
#so,to reduce computational time but maintain a ME stable
#PUT numtrees=100

#### Variable importance
par(mar = c(4,10, 0.5, 0.5))  
vimp <- .rf.varImp(fit = fit, plot = TRUE)

## Fitted values/predictions
fit$predictions                 ## Compare probability = FALSE vs TRUE
#da gli 0% e 1% per ogni obs (azienda) con prob=true
#da un vettore di 0 e 1 con prob=False

fitted <- predict(object = fit, data = data.orig)        ## Fitted values (original data)
#original data è in -data.orig-

## Store for future use
fit.rf <- fit

#### Tuning hyper-parameter with train() (package caret): optimize splitrule and mtry
trControl <- trainControl(method = "cv", number = 10)
tuneGrid <- expand.grid(mtry = 1:13, splitrule = c("gini", "hellinger"), min.node.size = 5)
##   Optimize 
#set numtrees=100
fit <- caret::train(
  form = SP.RATING ~ TVL.ESG.TOT.SCORE + FTSE.ESG.RAT. + 
    ROAIC + CURR.RATIO + EBITDA.MARGIN + FCF_SALES + PRICE_SALE.SHARE + 
    TOT.EQUITY.ON.LIAB + ROT.DEBT.COMM. + ROD + E_P.SHARE + ROS + 
    TURNOVER, data = data, method = "ranger",
  trControl = trControl, tuneGrid = tuneGrid, 
  num.trees = 100, seed = 2000, verbose = TRUE)


#nota: usiamo cv=10 perchè senno i fold contengono troppe poche osservazioni: 10 
#è il migliore

## Explore output
fit$results  #print results of each tunegrid settings.best model has the highest accuracy
fit$bestTune  #extraction of the best performing tunegrid settings (highest accuracy)
#the best model, in this case, is the 8: mtry=4, SPlitrule=hellinger, minnodsize=5,
#accuracy=0.6666889 
fit$resample    #details og results of the 10 cv repetitions
fit$finalModel   #fitted best model(classification)
## Store
fit.train <- fit


## Refit the model with the best hyperparameters with PROB=TRUE(PARTE IMPORTANTE)
fit <- ranger(formula = SP.RATING ~ TVL.ESG.TOT.SCORE + FTSE.ESG.RAT. + 
                ROAIC + CURR.RATIO + EBITDA.MARGIN + FCF_SALES + PRICE_SALE.SHARE + 
                TOT.EQUITY.ON.LIAB + ROT.DEBT.COMM. + ROD + E_P.SHARE + ROS + 
                TURNOVER, 
              data = data, num.trees = 100, mtry = fit$bestTune$mtry, 
              probability = TRUE,
              min.node.size = fit$bestTune$min.node.size,
              min.bucket = NULL, max.depth = NULL,
              importance = "impurity", splitrule =fit$bestTune$splitrule,
              keep.inbag = TRUE, oob.error = TRUE, verbose = TRUE, seed = 2000)

#outcome of best random forest:(PARTE PIU IMP)

fit         ## Summary: the error return in term of brier(similar to MSE)=0.2239903 
fit$treetype              #if correct tell "probability    
fit$splitrule          
fit$confusion.matrix                      ## Only with probability = FALSE


## A Larger set of error measures on the best RF model:(IMP)
.rf.errorMeasures(fit = fit, data = data)           
## A richer set of error measures: take oob-me(uguale alla bagging)
#oob-me=0.3787879  

## Variable importance on the best RF model(IMP)
par(mar = c(4, 10, 0.5, 0.5))
vimp <- .rf.varImp(fit = fit, plot = TRUE)

## Fitted values/predictions on the best RF model(IMP)
fit$predictions                 ## Compare probability = FALSE vs TRUE
#da gli 0% e 1% per ogni obs (azienda) con prob=true
#da un vettore di 0 e 1 con prob=False

## Store for future use

fit.best.rf.prob <- fit

#NOTA: OSSERVAZIONI RUN BEST RF MODEL WITH PROBABILITY=FALSE
#confusion matrix mostra che modello è buono nel preveder gli 0,
#ma no gli 1


#### Refit the model with the best hyperparameters with PROB=FALSE(PARTE IMPORTANTE)
fit <- ranger(formula = SP.RATING ~ TVL.ESG.TOT.SCORE + FTSE.ESG.RAT. + 
                ROAIC + CURR.RATIO + EBITDA.MARGIN + FCF_SALES + PRICE_SALE.SHARE + 
                TOT.EQUITY.ON.LIAB + ROT.DEBT.COMM. + ROD + E_P.SHARE + ROS + 
                TURNOVER, 
              data = data, num.trees = 100, mtry = fit.best.rf.prob$mtry, 
              probability = FALSE,
              min.node.size = fit.best.rf.prob$min.node.size,
              min.bucket = NULL, max.depth = NULL,
              importance = "impurity", splitrule =fit.best.rf.prob$splitrule,
              keep.inbag = TRUE, oob.error = TRUE, verbose = TRUE, seed = 2000)

#outcome of best random forest:(PARTE PIU IMP)

fit         ## Summary: the error return in term of OOB prediction error= 38.18 % 
fit$treetype              #if correct tell "probability    
fit$splitrule          
fit$confusion.matrix                      ## Only with probability = FALSE

## Variable importance on the best RF model(IMP)
par(mar = c(4, 10, 0.5, 0.5))
vimp <- .rf.varImp(fit = fit, plot = TRUE)

fit$predictions 

##store for future use:

fit.best.rf.class<-fit

################################################################################
## Gradient Boosting
################################################################################

###   1) Transform all character vars in factor (otherwise we may get errors 
##   in gbm())

##   2) Important: gbm() does not like the dependent as factor (it causes 
##   RStudio crashing!) but train() wants it. What a mess...

SP.RATINGnum<-as.numeric(data$SP.RATING)-1
str(SP.RATINGnum)   #controllo: OK!

###############################################################################

####standard run: set hyper parameter with tiypical and default values
#but, in particular, select an enough shrinkage (0.1) and enough ntrees (500)

####parametri standard non toccati: interaction.depth=5, cvfold=10, train.fraction=0.8,
#bag.fraction=0.5

########diverse prove: 
#1) ntrees=500, shrinkage=0.1
#nota: ntrees molto bassi=>posso diminuire shrinkage per migliorare previsioni
#e aumentare ntrees, tanto il computational time è molto basso

#2) ntrees=500, shrinkage=0.05
#nota: ntrees ancora bassi e computational time ancora basso =>possiamo diminuire
#ancora shrinkage, facciamo fino a shrinkage=0.01

#3 e ultima run) ntrees=500,shrinkage=0.01

set.seed(1000)
fit <- gbm(formula = SP.RATINGnum ~ TVL.ESG.TOT.SCORE + FTSE.ESG.RAT. + 
             ROAIC + CURR.RATIO + EBITDA.MARGIN + FCF_SALES + PRICE_SALE.SHARE + 
             TOT.EQUITY.ON.LIAB + ROT.DEBT.COMM. + ROD + E_P.SHARE + ROS + 
             TURNOVER, 
           data = data, distribution = "bernoulli", var.monotone = NULL, 
           n.trees = 500, interaction.depth = 5, shrinkage = 0.01, bag.fraction = 0.5, 
           n.minobsinnode = 5,  
           train.fraction = 0.8, cv.folds = 10, keep.data = TRUE)

## Main output
fit
#cross-validation iterations is approximately the best value of ntrees
#is 200 in this case, so:
n.trees<-200

par(mar = c(4, 10, 0.5, 0.5))                        ## Plot var importance
summary(fit, ntrees = n.trees, las = 1, normalize = TRUE)
#now it isn't so interesting becasue it isn't the best gbm model


fitted  <- fit$fit             ## Fitted values (train data)
fitted1 <- predict(object = fit, newdata = data, n.trees = n.trees, 
                   type = "link", single.tree = FALSE)    ##link=linear scale   

## Check that they are equal
head(fitted)
head(fitted1)    #sono diverse: perchè fit fatto su 500 e fitted1 su 200
#con ntrees=500 in fitted1 tornano uguali

fitted2 <- predict(object = fit, newdata = data, n.trees = n.trees, 
                   type = "response", single.tree = FALSE)    ##response=probability
#scale
head(fitted2)


#####EXPLAIN TRAIN ERROR, VALID ERROR, OOB IMPROVE 
# fit$train.error                                   ## Explain
par(mar = c(4, 4, 0.5, 0.5))                        ## Plot
plot(x = fit$train.error, xlab = "Iteration", ylab = "Error (Loss) Measure", 
     type = "l", ylim = c(0, 2))
# fit$valid.error                                     ## "
lines(x = fit$valid.error, xlab = "Iteration", type = "l", col = "red")
# fit$oobag.improve                                   ## "
lines(x = fit$cv.error, xlab = "Iteration", type = "l", col = "green")                                 ## "


##Approximate Tuning (try options)
n.trees.best <- gbm.perf(object = fit, 
                         plot.it = TRUE, oobag.curve = FALSE, overlay = FALSE, 
                         method = "cv") ## "cv", "OOB", "test"
legend(x = "bottomright", 
       legend = c("Train error", "Validation error", "CV error", "Best n.trees choice"), 
       col = c("black", "red", "green", "blue"), 
       lty = c(1, 1, 1, 2), lwd = c(1, 1, 1, 2), 
       fill = NULL, border = "white", pch = NULL, bty = "o", bg = par("bg"))

cat("The best number of trees is",n.trees.best)
#### Store for future use
fit1 <- fit


################################################################################
## Tuning hyper-parameters
################################################################################

#### The main hyper-parameters require tuning

#since ntrees is relative low, i can decrease more shrinkage
# in the way to obtain a better approximation, although ntrees increase


trControl <- trainControl(method = "cv", number = 10)
tuneGrid <- expand.grid(n.trees = c(150,170,200), shrinkage = c(0.01,0.05,0.1), 
                        interaction.depth = 3:6, n.minobsinnode = 10)


##   Optimize
#    Update formula by changing the dependent as factor: SP.RATING
fit <- caret::train( SP.RATING ~ TVL.ESG.TOT.SCORE + FTSE.ESG.RAT. + 
                       ROAIC + CURR.RATIO + EBITDA.MARGIN + FCF_SALES + PRICE_SALE.SHARE + 
                       TOT.EQUITY.ON.LIAB + ROT.DEBT.COMM. + ROD + E_P.SHARE + ROS + 
                       TURNOVER, data = data, 
                    method = "gbm", trControl = trControl, tuneGrid = tuneGrid, 
                    verbose = TRUE, keep.data = FALSE, bag.fraction = 0.5)


##   Explore output
fit$results    #give all different combinations of iteration that i put in "caret"
fit$bestTune    ## Select the best: 0.6451434 highest accuracy (model n.12)
#ntrees=200, interaction.depth=6,shrinkage=0.01, minobsinnode=10, accuracy= 0.6665274  

###Stored best tune
fit.best.tune<-fit$bestTune

fit$resample
fit$finalModel
##   Store the train output
fit.train <- fit


##   Refit the model with the best settings to get the best gbm model
fit <- gbm(
  formula = SP.RATINGnum ~ TVL.ESG.TOT.SCORE + FTSE.ESG.RAT. + 
    ROAIC + CURR.RATIO + EBITDA.MARGIN + FCF_SALES + PRICE_SALE.SHARE + 
    TOT.EQUITY.ON.LIAB + ROT.DEBT.COMM. + ROD + E_P.SHARE + ROS + 
    TURNOVER, data = data, distribution = fit1$distribution,
  n.trees = fit$bestTune$n.trees, 
  interaction.depth = fit$bestTune$interaction.depth, 
  n.minobsinnode = fit$bestTune$n.minobsinnode, 
  shrinkage = fit$bestTune$shrinkage, 
  bag.fraction = 0.5, train.fraction = 0.8, cv.folds = 10,
  var.monotone = NULL, keep.data = TRUE, verbose = FALSE)

#NOTA: DA QUI IN POI IL FIT$BEST.TUNE VIENE SOVRASCRITTO;
#PER QUESTO PRECEDENTEMENTE LHO SALVATO IN FIT.BEST.TUNE


#OUTCOMES OF BEST GBM MODEL:

fit      #now best ntrees=168 of the best GBM model
n.trees<-168

n.trees.best.gbm<-n.trees

par(mar = c(4, 10, 0.5, 0.5))                        ## Plot var importance
summary(fit, ntrees =n.trees, las = 1, normalize = TRUE)

#plots of the contribution of the most important variables
#of best GBM model(prime 4):
n.trees<-168

plot(x = fit, i.var = "TVL.ESG.TOT.SCORE", n.trees = n.trees, type = "response", ylab="SP.RATING",
     main="Impatto stimato di TVL.ESG.TOT.SCORE su SP.RATING")

plot(x = fit, i.var = "FTSE.ESG.RAT.", n.trees = n.trees, type = "response", ylab="SP.RATING",
     main="Impatto stimato di FTSE.ESG.RAT. su SP.RATING")

plot(x = fit, i.var = "E_P.SHARE", n.trees = n.trees, type = "response", ylab="SP.RATING",
     main="Impatto stimato di E_P.SHARE su SP.RATING")

plot(x = fit, i.var = "ROAIC", n.trees =n.trees, type = "response",
     ylab="SP.RATING", main="Impatto stimato di ROAIC su SP.RATING")


plot(x = fit, i.var = "TOT.EQUITY.ON.LIAB", n.trees = n.trees, type = "response",
     ylab="SP.RATING", main="Impatto stimato di TOT.EQUITY.ON.LIAB su SP.RATING")

plot(x = fit, i.var = "PRICE_SALE.SHARE", n.trees = n.trees, type = "response",
     ylab="SP.RATING", main="Impatto stimato di PRICE_SALE.SHARE su SP.RATING")


fit$initF                      ## It is on the lin-pred scale! 
#To obtain the true average, i need to transform eta to f

fitted  <- fit$fit             ## Fitted values (train data)
fitted1 <- predict(object = fit, newdata = data, n.trees = fit.best.tune$n.trees, #on 200 trees
                   type = "link", single.tree = FALSE)    ##link=linear scale 

## Check that they are equal

head(fitted)
head(fitted1)     #controllo OK         

fitted2 <- predict(object = fit, newdata = data, n.trees = n.trees.best.gbm , #on 168 trees
                   type = "response", single.tree = FALSE)    ##response=probability

#scale
head(fitted2)   #probability scale

## Store
fit.best.gbm <- fit


################################################################################
## Classification
################################################################################


######## Start from the data used for model fitting (just to get feeling...)
data <- data.train
######## Use fit.full (just to learn what we are doing)
fit <- fit.full

#### Proportion of 1 in data (as a reference)
prop1 <- mean(SP.RATINGnum)
prop1
#### Predict 
#pred.l <- predict(object = fit, newdata = data, type = "link")       ## Not used
pred.p <- predict(object = fit, newdata = data, type = "response")
#### 0.5 rule
coff <- 0.5
class.05 <- .classify(x = pred.p, coff = coff)

#### average rule
coff <- prop1     ## mu hat=0.3606061
class.av <- .classify(x = pred.p, coff = coff)
#### quantile rule
##   Important: it depends on the estimated model 
coff <- quantile(x = fitted(fit), prob = 1 - prop1)
coff  #=0.42212
class.qn <- .classify(x = pred.p, coff = coff)

#### Compare
rbind(
  true       = prop.table(table(SP.RATINGnum, useNA = "ifany")),
  class.05   = prop.table(table(class.05, useNA = "ifany")),
  class.av   = prop.table(table(class.av, useNA = "ifany")),
  class.qn   = prop.table(table(class.qn, useNA = "ifany")) )

#### Classification based summary statistics to understand what is best cut off value
#### 1) Confusion matrix
#NOTA: sensitivity: how model is good in classifying 1, specificity 0
#NOTA:confusionmatrix: reference=true, prediction=class

#statistics 05 cut off(typical class rule)
cmat.s <- confusionMatrix(data = as.factor(class.05), reference = as.factor(SP.RATINGnum), 
                          positive = "1")
cmat.s  #RESULTS: accuracy=0.6576, sensitivity=0.2941,specificity=0.8626

#statistics av cut off(uninformed class rule)
cmat.s <- confusionMatrix(data = as.factor(class.av), reference = as.factor(SP.RATINGnum), 
                          positive = "1")
cmat.s  #RESULTS: accuracy=0.6606 , sensitivity=0.7815,specificity=0.5924  

#statistics quantile cut off
cmat.s <- confusionMatrix(data = as.factor(class.qn), reference = as.factor(SP.RATINGnum), 
                          positive = "1")
cmat.s    ##RESULTS: accuracy=0.6485 , sensitivity=0.5126,specificity=0.7251 

###CONCLUSIONE: CONSIDERANDO ACCURACY E IL MIGLIOR BILANCIAMENTO
#TRA SENSITIVITY E SPECIFICITY, IL MIGLIORE CUT OFF è L'AV (UNINFORMED CUT OFF RULE) 

#### 2) ROC: necessario var dipendente num

#roc plot curve of fit.full
roc <- roc.plot(x = SP.RATINGnum, pred = pred.p, 
                xlab = "False Positive Rate", ylab = "True Positive Rate", 
                leg.text = "fit.full", legend = TRUE)

####ROC PLOT ON MORE MODEL (full,back,lasso,rf,gbm) ON TRAINING SET

#1)predict dei modelli interessati sul training data:

pred.full  <- predict(object = fit.full, newdata = data, type = "response")
pred.back  <- predict(object = fit.back, newdata = data, type = "response")
pred.enet  <- predict(object = fit.enet, newx = xmat, type = "response")
pred.rf  <- predict(object = fit.best.rf.prob, data=data.train, newx = xmat, type = "response")
pred.gbm  <- predict(object = fit.best.gbm, newx = xmat, type = "response", 
                     n.trees=n.trees.best.gbm)

str(pred.rf)
PRED<-data.frame(pred.full,pred.back,pred.enet,pred.rf$predictions[,"1"],pred.gbm)
View(PRED)

#2) specifica le soglie per rendere il roc.plot più semplice da interpretare:
thresholds <- seq(min(PRED), max(PRED), length.out = 200)



#3) roc curve: siccome i colori non sono associati corrett., ho fatto una legenda manuale
roc <- roc.plot(x = SP.RATINGnum, pred = PRED, thresholds = thresholds,
                xlab = "False Positive Rate", ylab = "True Positive Rate", 
                leg.text = c("fit.full","fit.back","fit.enet","fit.rf",
                "fit.best.gbm"), legend=TRUE,show.thres=FALSE)

roc <- roc.plot(x = SP.RATINGnum, pred = PRED, thresholds = thresholds,
                xlab = "False Positive Rate", ylab = "True Positive Rate", 
                leg.text = c("fit.full","fit.back","fit.enet","fit.rf",
                             "fit.best.gbm"),show.thres=FALSE)

legend("bottomright", legend = c("fit.full  0.703", "fit.back  0.701",
                                 "fit.enet  0.701","fit.rf  1","fit.best.gbm  0.889"),
       col = c("black", "red", "green","blue","skyblue" ), lty = 1)


#NOTA: necessario che colonne dataframe siano ORDINATE come
#NOTA: il numero nella legenda affianco al nome del modello, è 
#il valore dell'auroc (controllato con funzione successiva roc.area)

auroc <- roc.area(obs = SP.RATINGnum, pred = pred.p)$A
auroc


####ROC PLOT ON MORE MODEL (full,back,lasso,rf,gbm) ON TEST SET

#1)predict dei modelli interessati sul test set:

data<-data.test
xmat <- model.matrix(object = form, data = data)[, -1, drop = FALSE]

pred.full  <- predict(object = fit.full, newdata = data, type = "response")
pred.back  <- predict(object = fit.back, newdata = data, type = "response")
pred.enet  <- predict(object = fit.enet, newx = xmat, type = "response")
pred.rf  <- predict(object = fit.best.rf.prob, data=data, newx = xmat, type = "response")
pred.gbm  <- predict(object = fit.best.gbm, newdata=data, newx = xmat, type = "response", 
                     n.trees=n.trees.best.gbm)
length(pred.gbm)


str(pred.rf)
PRED<-data.frame(pred.full,pred.back,pred.enet,pred.rf$predictions[,"1"],pred.gbm)
#View(PRED)

#2) specifica le soglie per rendere il roc.plot più semplice da interpretare:
thresholds <- seq(min(PRED), max(PRED), length.out = 100)

SP.RATINGnum.test<-as.numeric(data$SP.RATING)-1

#3) roc curve: siccome i colori non sono associati corrett., ho fatto una legenda manuale
roc <- roc.plot(x = SP.RATINGnum.test, pred = PRED, thresholds = thresholds,
                xlab = "False Positive Rate", ylab = "True Positive Rate", 
                leg.text = c("fit.full","fit.back","fit.enet","fit.rf",
                             "fit.best.gbm"), legend=TRUE,show.thres=FALSE)

roc <- roc.plot(x = SP.RATINGnum.test, pred = PRED, thresholds = thresholds,
                xlab = "False Positive Rate", ylab = "True Positive Rate", 
                leg.text = c("fit.full","fit.back","fit.enet","fit.rf",
                             "fit.best.gbm"),show.thres=FALSE)

legend("bottomright", legend = c("fit.full  0.618", "fit.back  0.628",
                                 "fit.enet  0.641","fit.rf  0.673","fit.best.gbm  0.727"),
       col = c("black", "red", "green","blue","skyblue" ), lty = 1)




#### 3) cut-off point suggested by ROC stats
coff.yuden <- .yuden.coff(x = SP.RATINGnum, pred = pred.p)
coff.yuden  #0.3693153

#increase TPR=SENSITIVITY until 75% and decrease FPR until 38%

################################################################################
## Summary of fits
##  Two parts: probability and classification
################################################################################

######### 1) Probability based ###############

#### ON THE TRAINING DATA
## Redefines quantities for safety reasons
data <- data.train
form <- formula(fit.full) 
y <- data$SP.RATING
xmat <- model.matrix(object = form, data = data)[, -1, drop = FALSE]
## Compute
fit.stats <- rbind(
  Mfull = .fit.stats.bin(fit = fit.full),
  Mback = .fit.stats.bin(fit = fit.back),
  Mforw = .fit.stats.bin(fit = fit.forw),
  Mboth = .fit.stats.bin(fit = fit.both),
  Mridge = .fit.stats.bin(fit = fit.ridge, y = y, x = xmat),
  Mlasso = .fit.stats.bin(fit = fit.lasso, y = y, x = xmat),
  Menet = .fit.stats.bin(fit = fit.enet, y = y, x = xmat),
  Mrandomforests= .fit.stats.bin(fit=fit.best.rf.prob,y = y, x = data),
  Mgbm= .fit.stats.bin(fit=fit.best.gbm,y = y, x = data, n.trees=n.trees.best.gbm))
fit.stats
fit.stats.train <- fit.stats     ## Store

#NOTA: sembra che RF overfitti i dati, la meglio sembra la GBM

####### ON THE TEST DATA
## Redefines quantities for safety reasons
data <- data.test
form <- formula(fit.full) 
y <- data$SP.RATING
xmat <- model.matrix(object = form, data = data)[, -1, drop = FALSE]
## Compute
fit.stats <- rbind(
  Mfull = .fit.stats.bin(fit = fit.full, y = y, x = data),
  Mback = .fit.stats.bin(fit = fit.back, y = y, x = data),
  Mforw = .fit.stats.bin(fit = fit.forw, y = y, x = data),
  Mboth = .fit.stats.bin(fit = fit.both, y = y, x = data),
  Mridge = .fit.stats.bin(fit = fit.ridge, y = y, x = xmat),
  Mlasso = .fit.stats.bin(fit = fit.lasso, y = y, x = xmat),
  Menet = .fit.stats.bin(fit = fit.enet, y = y, x = xmat),
  Mrandomforests= .fit.stats.bin(fit=fit.best.rf.prob,y = y, x = data),
  Mgbm= .fit.stats.bin(fit=fit.best.gbm,y = y, x = data, n.trees=n.trees.best.gbm))
fit.stats
fit.stats.test <- fit.stats     ## Store

fit.stats.test

######### 2) Classification based  

#### Set the classification rule: AVERAGE RULE OR YUDEN COFF
#coff <- prop1     ## mu hat: average rule->coff=0.3606061

coff <- coff.yuden$coff  #yuden coff=0.3693153

#coff<-0.42212 #quantile rule

#coff<-0.5 #standard rule


##### ON THE TRAINING DATA
## Redefines quantities for safety reasons
data <- data.train
form <- formula(fit.full) 
y <- data$SP.RATING
xmat <- model.matrix(object = form, data = data)[, -1, drop = FALSE]
## Compute the probability predictions and classify
pred.full  <- predict(object = fit.full, newdata = data, type = "response")
pred.back  <- predict(object = fit.back, newdata = data, type = "response")
pred.forw  <- predict(object = fit.forw, newdata = data, type = "response")
pred.both  <- predict(object = fit.both, newdata = data, type = "response")
pred.ridge <- predict(object = fit.ridge, newx = xmat, type = "response")
pred.lasso <- predict(object = fit.lasso, newx = xmat, type = "response")
pred.enet  <- predict(object = fit.enet, newx = xmat, type = "response")
pred.rf  <- predict(object = fit.best.rf.prob, data=data.train, newx = xmat, type = "response")
pred.gbm  <- predict(object = fit.best.gbm, newx = xmat, type = "response", 
                     n.trees=n.trees.best.gbm)

## Classify
class.full  <- .classify(x = pred.full, coff = coff)
class.back  <- .classify(x = pred.back, coff = coff)
class.forw  <- .classify(x = pred.forw, coff = coff)
class.both  <- .classify(x = pred.both, coff = coff)
class.ridge <- .classify(x = pred.ridge, coff = coff)
class.lasso <- .classify(x = pred.lasso, coff = coff)
class.enet  <- .classify(x = pred.enet, coff = coff)
class.rf  <- .classify(x = pred.rf$predictions[,"1"], coff = coff)
class.gbm  <- .classify(x = pred.gbm, coff = coff)

## Stats
class.stats <- rbind(
  Mfull = .class.stats(true = data$SP.RATING, class = class.full)$stats,
  Mback = .class.stats(true = data$SP.RATING, class = class.back)$stats,
  Mforw = .class.stats(true = data$SP.RATING, class = class.forw)$stats,
  Mboth = .class.stats(true = data$SP.RATING, class = class.both)$stats,
  Mridge = .class.stats(true = data$SP.RATING, class = class.ridge)$stats,
  Mlasso = .class.stats(true = data$SP.RATING, class = class.lasso)$stats,
  Menet = .class.stats(true = data$SP.RATING, class = class.enet)$stats,
  Mrandomforest = .class.stats(true = data$SP.RATING, class = class.rf)$stats,
  Mgbm = .class.stats(true = data$SP.RATING, class = class.gbm)$stats)
class.stats

#### Copy
class.stats.train <- class.stats

####CONFUSIONMATRIX ON TRAIN DATA OF GBM:
c.mat.gbm<-confusionMatrix(as.factor(class.gbm),data$SP.RATING )
c.mat.gbm


#NOTA: RF CONTINUA OVERFITTING, GBM RISULTATI MOLTO MIGLIORI RISPETTO ALLA 
#PROBABILITY BASED

#### ON THE TEST DATA
## Redefines quantities for safety reasons
data <- data.test
form <- formula(fit.full) 
y <- data$SP.RATING
xmat <- model.matrix(object = form, data = data)[, -1, drop = FALSE]
## Compute the probability predictions and classify
pred.full  <- predict(object = fit.full, newdata = data, type = "response")
pred.back  <- predict(object = fit.back, newdata = data, type = "response")
pred.forw  <- predict(object = fit.forw, newdata = data, type = "response")
pred.both  <- predict(object = fit.both, newdata = data, type = "response")
pred.ridge <- predict(object = fit.ridge, newx = xmat, type = "response")
pred.lasso <- predict(object = fit.lasso, newx = xmat, type = "response")
pred.enet  <- predict(object = fit.enet, newx = xmat, type = "response")
pred.rf  <- predict(object = fit.best.rf.prob, data= data, newx = xmat, type = "response")
pred.gbm  <- predict(object = fit.best.gbm, newdata=data, newx = xmat, type = "response",
                     n.trees=n.trees.best.gbm)


## Classify
class.full  <- .classify(x = pred.full, coff = coff)
class.back  <- .classify(x = pred.back, coff = coff)
class.forw  <- .classify(x = pred.forw, coff = coff)
class.both  <- .classify(x = pred.both, coff = coff)
class.ridge <- .classify(x = pred.ridge, coff = coff)
class.lasso <- .classify(x = pred.lasso, coff = coff)
class.enet  <- .classify(x = pred.enet, coff = coff)
class.rf  <- .classify(x = pred.rf$predictions[,"1"], coff = coff)
class.gbm  <- .classify(x = pred.gbm, coff = coff)


## Stats    
class.stats <- rbind(
  Mfull = .class.stats(true = data$SP.RATING, class = class.full)$stats,
  Mback = .class.stats(true = data$SP.RATING, class = class.back)$stats,
  Mforw = .class.stats(true = data$SP.RATING, class = class.forw)$stats,
  Mboth = .class.stats(true = data$SP.RATING, class = class.both)$stats,
  Mridge = .class.stats(true = data$SP.RATING, class = class.ridge)$stats,
  Mlasso = .class.stats(true = data$SP.RATING, class = class.lasso)$stats,
  Menet = .class.stats(true = data$SP.RATING, class = class.enet)$stats,
  Mrandomforest = .class.stats(true = data$SP.RATING, class = class.rf)$stats,
  Mgbm = .class.stats(true = data$SP.RATING, class = class.gbm)$stats)
class.stats

#### Copy
class.stats.test <- class.stats

#NOTA: LA CLASSIFICATION BASED DA RISULTATI MIGLIORI DELLA PROBABILITY BASED####

#####RISULTATI CON CUT OFF: AVERAGE CUT OFF RULE (coff=0.3606061)
#BACKWARD(MIGLIOR MODELLO) NEL DATA.TEST HA UN ACCURACY DI 0.6341463,
#SENSITIVITY DI 0.6666667  ( sufficiente)
# E UNA SPECIFICITY DI 0.6181818  (sufficiente)
#UNA CLASSIFICATION BILANCIATA MA PEGGIORE DEL YUDEN CUT OFF


#####RISULTATI CON CUT OFF: YUDEN CUT OFF RULE (coff=0.3693153)
#GBM(MIGLIOR MODELLO) NEL DATA.TEST HA UN ACCURACY DI 0.6585366 , 
#SENSITIVITY DI 0.6666667 (discreta)
# E UNA SPECIFICITY DI 0.6545455  (discreta)
###LA CLASSIFICATION PIU BILANCIATA

#####risultati quantile cut off: c=0.42212 
#GBM, la migliore, nel data.test ha un accuracy di  0.7317073 (migliore),
#MA una sensitivity di 0.5185185  (molto piu bassa) e una specificity di 0.8363636   
#(molto piu alta). Probabilmente resta non il migliore cut off, dato che 
# la sua accuracy è data dalla miglior capacità di classificare gli 0 e dalla
# maggior presenza di 0 nel dataset, ma resta molto carente nel class 1 


#lo stesso discorso vale per la standard rule (c=0.5) da cui ottengo simili 
#risultati: accuracy=0.7317073 ,sensitivity= 0.3703704 , specificity=0.9090909 
#La sensitivity qui è troppo bassa per essere presa in considerazione questa 
#cut off rule
##########################

#DUNQUE IL MODELLO CHE DA I MIGLIORI RISULTATI
# SUL TEST.SET è LA GBM,
#che da risultati sufficientemente buoni considerando la classification:
#LA MIGLIORE CUT OFF rule è la YUDEN o la QUANTILE cut off, a seconda 
#che si voglia una classification più bilanciata o meno

###############

###TOTAL OUTCOMES
fit.stats.train
fit.stats.test
class.stats.train
class.stats.test  #il piu importante

### CONFUSION MATRIX SUL TEST.SET DELLA GBM(MIGLIOR MODELLO)

#TEST.SET
c.mat.gbm<-confusionMatrix(as.factor(class.gbm),data$SP.RATING, positive="1")
c.mat.gbm

####REFIT THE BEST MODEL(FIT.BEST.GBM) ON ALL ORIGINAL DATA#######################
#REFIT GBM ON ORIGINAL DATA

data <- data.orig

#refit gbm

set.seed(1000)

SP.RATINGnum.orig<-as.numeric(data$SP.RATING)-1
str(SP.RATINGnum.orig)

fit<-gbm(formula = SP.RATINGnum.orig ~ TVL.ESG.TOT.SCORE + FTSE.ESG.RAT. + 
           ROAIC + CURR.RATIO + EBITDA.MARGIN + FCF_SALES + PRICE_SALE.SHARE + 
           TOT.EQUITY.ON.LIAB + ROT.DEBT.COMM. + ROD + E_P.SHARE + ROS + 
           TURNOVER, distribution = "bernoulli", data = data, 
         var.monotone = NULL, n.trees = fit.best.gbm$n.trees, 
         interaction.depth = fit.best.gbm$interaction.depth, 
         n.minobsinnode = fit.best.gbm$n.minobsinnode, shrinkage = fit.best.gbm$shrinkage, 
         bag.fraction = 0.5, train.fraction = 0.8, cv.folds = 10, 
         keep.data = TRUE, verbose = FALSE)

#OUTCOMES OF BEST GBM MODEL:

fit      #now best ntrees=153 of the best GBM model
n.trees<-153

###Stored best ntress
n.trees.best.gbm<-n.trees

par(mar = c(4, 10, 0.5, 0.5))                        ## Plot var importance
summary(fit, ntrees =n.trees.best.gbm , las = 1, normalize = TRUE)

#plots of the contribution of the most important variables
#of best GBM model(prime 3):
n.trees<-153

plot(x = fit, i.var = "TVL.ESG.TOT.SCORE", n.trees = n.trees, type = "response", ylab="SP.RATING",
     main="Impatto stimato di TVL.ESG.TOT.SCORE su SP.RATING")

plot(x = fit, i.var = "FTSE.ESG.RAT.", n.trees = n.trees, type = "response", ylab="SP.RATING",
     main="Impatto stimato di FTSE.ESG.RAT. su SP.RATING")

plot(x = fit, i.var = "E_P.SHARE", n.trees = n.trees, type = "response", ylab="SP.RATING",
     main="Impatto stimato di E_P.SHARE su SP.RATING")

plot(x = fit, i.var = "ROAIC", n.trees =n.trees, type = "response",
     ylab="SP.RATING", main="Impatto stimato di ROAIC su SP.RATING")


plot(x = fit, i.var = "TOT.EQUITY.ON.LIAB", n.trees = n.trees, type = "response",
     ylab="SP.RATING", main="Impatto stimato di TOT.EQUITY.ON.LIAB su SP.RATING")

plot(x = fit, i.var = "PRICE_SALE.SHARE", n.trees = n.trees, type = "response",
     ylab="SP.RATING", main="Impatto stimato di PRICE_SALE.SHARE su SP.RATING")


## Store
fit.best.gbm<-fit

################################################################################
## Prediction OF Y(SP.RATING values 2024) ON DATASET 2023
################################################################################

#### As usual...
#### Paste predictions to live-data: both prob predictions and classifications

#### Read live data
##   Since live data are not available, reuse here the original data


data.live <- read.xlsx("C:\\Users\\lucas\\Desktop\\TESI\\FILE DEFINITIVO\\TESI DATASET 2023.xlsx",
                       detectDates = TRUE,na.strings = c("NA","@NA"))

View(data.live)


#### Compute predictions using predict(), as usual
##   fit.gbm is the model we decided to use

pred <- predict(object = fit.best.gbm, newdata = data.live, type = "response",
                n.trees=n.trees.best.gbm)

class <- .classify(x = pred, coff = coff.yuden$coff)

#### Append predictions values of SP.RATING at time 2024 to live data
data.live <- data.frame(data.live, pred = pred, class = class, check.names = FALSE)

#View(data.live)

data.live.w.pred.<-data.live

#salvare la tabella in un file xlsx

write.xlsx(data.live.w.pred.,
           "C:\\Users\\lucas\\Desktop\\TESI\\FILE DEFINITIVO\\TESI DATASET 2023+PREV.xlsx")





















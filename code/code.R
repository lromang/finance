
##################################################
## This code generates the variables needed for
## the project.
##################################################


################################
## Libraries
################################

#Manipulacion datos
library(plyr)
library(dplyr)
library(data.table)
library(readr)
## Gráficas
library(ggplot2)

#fechas
library(lubridate)

#manipular strings
library(stringr)

#bit64 package
library(bit64)

#para la curtosis
library(e1071)


################################
## Read datasets
################################
data.comp <- fread("../data/compustatFA.csv")
names(data.comp) <- tolower(names(data.comp))
data.crsp <- fread("../data/CRSP_Convolumen.csv")

##Quitamos los datos faltantes

names(data.crsp) <- c("date", "compname", "cusip",
                      "ticker", "permno", "newpermno",
                      "shrout", "eventdate", "prc",
                      "volume", "returns", "bid", "ask",
                      "dred")
data.crsp$newpermno <- NULL

################################
## Process datasets
################################

## Omit NA values
data.crsp <- na.omit(data.crsp)
data.comp <- na.omit(data.comp)
## Date crsp
year  <- str_sub(data.crsp$date, 1, 4)
month <- str_sub(data.crsp$date, 5, 6)
day   <- str_sub(data.crsp$date, 7, 8)
date  <- as.Date(paste(year, month, day, sep = "-"))
data.crsp$date <- date

## Date comp
year  <- str_sub(data.comp$datadate, 1, 4)
month <- str_sub(data.comp$datadate, 5, 6)
day   <- str_sub(data.comp$datadate, 7, 8)
date  <- as.Date(paste(year, month, day, sep = "-"))
data.comp$datadate <- date

###################################
## Calculate variables | TABLE 1 ##
###################################

## ------------------------------
## Enterprise number
## ------------------------------

data.crsp$month <- month(data.crsp$date)
data.crsp$year  <- year(data.crsp$date)
enterprises     <- data.crsp$compname

n.enterprises        <- data.crsp[, .N, by = c("month", "year", "compname")]
names(n.enterprises) <- c("month", "year", "compname", "n_firms")

## min, max, promedio
summary(n.enterprises$n_firms)

## Desviación estándar
sd(n.enterprises$n_firms)

## Cuantiles
quantile(n.enterprises$n_firms, prob = c(0.10, 0.25, 0.5, 0.75, 0.9))

## Kurtosis and skewness
kurtosis(n.enterprises$n_firms)

skewness(n.enterprises$n_firms)

## ------------------------------
## Market value (size)
## ------------------------------

## por mes
mv <- data.crsp[, shrout*prc, by = c("month", "year", "compname")]
names(mv) <- c("month", "year", "compname", "mv")

## min, max, promedio
summary(mv$mv)

## Desviación estándar
sqrt(var(mv$mv))

## Cuantiles
quantile(mv$mv, prob = c(0.10, 0.25, 0.5, 0.75, 0.9))

## Kurtosis and skewness
kurtosis(mv$mv)
skewness(mv$mv)

## ------------------------------
## Book value / market value
## ------------------------------

## Tomamos los datos de la Base crsp, tomando como ID los primeros cinco dígitos de CUSIP
data.crsp$cusip2 <- str_sub(data.crsp$cusip, 1, 5)
vlvm1 <- data.crsp[, shrout,
                   by = c("date", "cusip2")]
vlvm1$cusip2 <- as.factor(vlvm1$cusip2)

## Tomamos los datos de la Base comp, tomando como ID los primeros cinco dígitos de CUSIP
data.comp$cusip2 <- str_sub(data.comp$cusip, 1, 5)
vlvm2 <- data.comp[,bkvlps, by=c("ceq", "datadate","cusip2")]
names(vlvm2) <- c("ceq", "date", "cusip2", "bkvlps")
vlvm2$cusip2 <- as.factor(vlvm2$cusip2)

## Hacemos el merge de las dos bases
vlvm_base <- merge(vlvm1, vlvm2, by = c("cusip2", "date"))
vlvm_base$year <- year(vlvm_base$date)
vlvm_base$month <- month(vlvm_base$date)

## calculamos Book value over market value
vlvm <- vlvm_base[,ceq/(bkvlps*shrout), by = c("cusip2", "year", "month")]
names(vlvm) <- c("cusip", "year", "month", "bvmv")

## min, max, promedio
summary(vlvm$bvmv)

## Desviación estándar
sqrt(var(vlvm$bvmv))

## Cuantiles
quantile(vlvm$bvmv, prob = c(0.10, 0.25, 0.5, 0.75, 0.9))

## Kurtosis and skewness
kurtosis(vlvm$bvmv)
skewness(vlvm$bvmv)

## ------------------------------
## Volume
## ------------------------------

volume <- data.crsp[,volume, by = c("month", "year", "compname")]
volume

#min, max, promedio
summary(volume$volume)
#Desviación estándar
sqrt(var(volume$volume))
#Cuantiles
quantile(volume$volume, prob = c(0.10, 0.25, 0.5, 0.75, 0.9))
#Kurtosis and skewness
kurtosis(volume$volume)
skewness(volume$volume)

## ------------------------------
## Price
## ------------------------------

prc <- data.crsp[,prc, by = c("month", "year", "compname")]
prc

## min, max, promedio
summary(prc$prc)

## Desviación estándar
sqrt(var(prc$prc))

## Cuantiles
quantile(prc$prc, prob = c(0.10, 0.25, 0.5, 0.75, 0.9))

## Kurtosis and skewness
kurtosis(prc$prc)
skewness(prc$prc)

## ------------------------------
## Bid ask spread
## ------------------------------

## Obtenemos el spred promedio mensual
ba.spread <- data.crsp[, mean(ask - bid), by = c("month", "year", "compname")]
names(ba.spread) <- c("month", "year", "compname", "baspread")

## min, max, promedio
summary(ba.spread$baspread)

## Desviación estándar
sqrt(var(ba.spread$baspread))

## Cuantiles
quantile(ba.spread$baspread, prob = c(0.10, 0.25, 0.5, 0.75, 0.9))

## Kurtosis and skewness
kurtosis(ba.spread$baspread)
skewness(ba.spread$baspread)


###################################
## Calculate variables | TABLE 2 ##
###################################

##### Tabla 2 ######
n.enterprises      <- n.enterprises[order(year, month), ] 
n.enterprises_mean <- n.enterprises[,mean(n_firms), by = c("year")]

##### Tabla 2 ######
mv <- mv[order(year,month),] 
mv_mean <- mv[,mean(mv), by = c("year")]

#####Tabla 2######
vlvm <- vlvm[order(year,month),] 
vlvm_mean <- vlvm[,mean(bvmv), by = c("year")]

#####Tabla 2######
volume <- volume[order(year,month),] 
volume_mean <- volume[,mean(volume), by = c("year")]

#####Tabla 2######
prc <- prc[order(year,month),] 
prc_mean <- prc[,mean(prc), by = c("year")]

#####Tabla 2######
ba.spread <- ba.spread[order(year,month),] 
ba.spread <- ba.spread[,mean(baspread), by = c("year")]

#####Tabla 2######
betas_1 <- betas_1[order(year,month),] 
betas_1_mean <- betas_1[,mean(beta), by = c("year")]

## ------------------------------
## Volatilidad idiosincrática
## ------------------------------
dataBeta <- fread("../data/tabla_1.csv")
betas    <- fread("../data/betas_1.csv")

errors       <- merge(dataBeta, betas, by = c("month", "year", "compname"))
error        <- errors[,sd(returns - beta*(mkt_ret - rfree)), by = c("month", "year", "compname")]
names(error) <- c("month", "year", "compname", "error")

## min, max, promedio
summary(error$error)

## Desviación estándar
sqrt(var(error$error))

## Cuantiles
quantile(error$error, prob = c(0.10, 0.25, 0.5, 0.75, 0.9))

## Kurtosis and skewness
kurtosis(error$error)
skewness(error$error)


## ------------------------------
## Leverage
## ------------------------------
data.comp$month <- month(data.comp$datadate)
data.comp$year <- year(data.comp$datadate)
lever <- data.comp[, dltt/ceq, by = c("month", "year")]
names(lever) <- c("month", "year", "leverage")

#min, max, promedio
summary(lever$leverage)
#Desviación estándar
sqrt(var(lever$leverage))
#Cuantiles
quantile(lever$leverage, prob = c(0.10, 0.25, 0.5, 0.75, 0.9))
#Kurtosis and skewness
kurtosis(lever$leverage)
skewness(lever$leverage)

#####Tabla 2######
lever <- lever[order(year,month),] 
lever <- lever[,mean(leverage), by = c("year")]

################################
## Table 3: Correlation matrixes
################################






################################
## Table 4: Promedio de cada variable para 3 grupos
################################
small=0.3
medium=0.4
big=0.3


################################
## Table 5: Promedio de cada variable para 3 grupos
################################


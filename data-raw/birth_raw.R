#----------------------
# Births 2006-2016
#----------------------

library(xlsx)
library(abind)

setwd("data-raw")

# Age groups
Age <- c("<18",
         "18-19", "20-24", "25-29",
         "30-34", "35-39", "40-44",
         "45-49", "50+", "NA")
a <- length(Age)

# Regions
Regions <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA",
             "TRENTINO-ALTO ADIGE", "BOLZANO- BOZEN",
             "TRENTO", "VENETO", "FRIULI-VENEZIA GIULIA",
             "LIGURIA", "EMILIA - ROMAGNA", "TOSCANA",
             "UMBRIA", "MARCHE", "LAZIO", "ABRUZZO",
             "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA",
             "CALABRIA", "SICILIA", "SARDEGNA")
r <- length(Regions)

# Years
Years <- c(2006:2016)
t <- length(Years)

# 2006
BpR06<- read.xlsx("BpR06.xls",sheetIndex = 1)

# Take only the mothers' age
b06a<-BpR06[BpR06[,1]=="Totale",]

# Eliminate NAs rows and take only relevant counts
b06b<- as.matrix(b06a[-which(is.na(b06a[,1])),2:11])

# Take only regions not country and macro-areas
# make it numeric
b06c<- apply(b06b[1:22,], 2, as.numeric)

# Check no NAs
which(is.na(b06c))

colnames(b06c) <- Age
rownames(b06c) <- Regions

# 2007
BpR07<- read.xlsx("BpR07.xls",sheetIndex = 1)

b07a <-BpR07[BpR07[,1]=="Totale",]
b07b<-as.matrix(b07a[-which(is.na(b07a[,1])),2:11])
b07c <- apply(b07b[1:22,], 2, as.numeric)
which(is.na(b07c))
colnames(b07c) <- Age
rownames(b07c) <- Regions

# 2008
BpR08<- read.xlsx("BpR08.xls",sheetIndex = 1)

b08a <-BpR08[BpR08[,1]=="Totale",]
b08b<-as.matrix(b08a[-which(is.na(b08a[,1])),2:11])
b08c <- apply(b08b[1:22,], 2, as.numeric)
which(is.na(b08c))
colnames(b08c) <- Age
rownames(b08c) <- Regions

# 2009
BpR09<- read.xlsx("BpR09.xls",sheetIndex = 1)

b09a <-BpR09[BpR09[,1]=="Totale",]
b09b<-as.matrix(b09a[-which(is.na(b09a[,1])),2:11])
b09c <- apply(b09b[1:22,], 2, as.numeric)
which(is.na(b09c))
colnames(b09c) <- Age
rownames(b09c) <- Regions

# 2010
BpR10<- read.xlsx("BpR10.xls",sheetIndex = 1)

b10a <-BpR10[BpR10[,1]=="Totale",]
b10b<- as.matrix(b10a[-which(is.na(b10a)[,1]),2:11])
b10c <- apply(b10b[1:22,], 2, as.numeric)
which(is.na(b10c))
colnames(b10c) <- Age
rownames(b10c) <- Regions

# 2011
BpR11<- read.xlsx("BpR11.xls",sheetIndex = 1)

b11a <-BpR11[BpR11[,1]=="Totale",]
b11b<- as.matrix(b11a[-which(is.na(b11a)[,1]),2:11])
b11c <- apply(b11b[1:22,], 2, as.numeric)
which(is.na(b11c))
colnames(b11c) <- Age
rownames(b11c) <- Regions

# 2012
BpR12<- read.xlsx("BpR12.xls",sheetIndex = 1)

b12a <-BpR12[BpR12[,1]=="Totale",]
b12b<- as.matrix(b12a[-which(is.na(b12a)[,1]),2:11])
b12c <- apply(b12b[1:22,], 2, as.numeric)
which(is.na(b12c))
colnames(b12c) <- Age
rownames(b12c) <- Regions

# 2013
BpR13<- read.xlsx("BpR13.xls",sheetIndex = 1)

b13a <-BpR13[BpR13[,1]=="Totale",]
b13b<- as.matrix(b13a[-which(is.na(b13a)[,1]),2:11])
b13c <- apply(b13b[1:22,], 2, as.numeric)
which(is.na(b13c))
colnames(b13c) <- Age
rownames(b13c) <- Regions

# 2014
BpR14<- read.xlsx("BpR14.xls",sheetIndex = 1)

b14a <-BpR14[BpR14[,1]=="Totale",]
b14b<- as.matrix(b14a[-which(is.na(b14a)[,1]),2:11])
b14c <- apply(b14b[1:22,], 2, as.numeric)
b14c[which(is.na(b14c))] <- 0
colnames(b14c) <- Age
rownames(b14c) <- Regions

# 2015
BpR15<- read.xlsx("BpR15.xls",sheetIndex = 1)

b15a<-BpR15[BpR15[,1]=="Totale",]
b15b<- as.matrix(b15a[-which(is.na(b15a)[,1]),2:11])
b15c <- apply(b15b[1:22,], 2, as.numeric)
which(is.na(b15c))
colnames(b15c) <- Age
rownames(b15c) <- Regions

# 2016
BpR16<- read.xlsx("BpR16.xls",sheetIndex = 1)

b16a <-BpR16[BpR16[,1]=="Totale",]
b16b<- as.matrix(b16a[-which(is.na(b16a)[,1]),2:11])
b16c <- apply(b16b[1:22,], 2, as.numeric)
which(is.na(b16c))
colnames(b16c) <- Age
rownames(b16c) <- Regions


dimn<- list(region= Regions,
            age = Age,
            year = Years)

italy.births.reg<- abind(b06c,b07c,b08c,b09c,b10c,b11c,b12c,
                   b13c,b14c,b15c,b16c, along = 3)
which(is.na(italy.births.reg))
dimnames(italy.births.reg)<- dimn
italy.births.reg
save(italy.births.reg, file = "italy.births.reg.RData")


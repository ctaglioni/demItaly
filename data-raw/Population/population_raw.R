#--------------------------------
# Population 1/1/2002 - 1/1/2011
#--------------------------------

library(abind)
library(tidyverse)
library(demest)

# Load datasets
# Regional data from 2002 to 2011
popR<- as.matrix(read.csv("PopolazioneEta-Territorio-Regioni.csv", sep=";", header = FALSE))
# Province data from 2002 to 2011
popP <- as.matrix(read.csv("PopolazioneEta-Territorio-Province.csv", sep=";", header = FALSE))

# Table has "all citizenships" counts and then "italian" or "foreigner" citizeship
# The last row of interest is the one before "italian" citizeship counts begins
stop <- which(popR[,1] == "Cittadinanza italiana - Anno: 2002")

# Sex index, interested in male and female population not total
sexInd <- which(popR[,3] %in% c("Maschi", "Femmine"))
sexInd<- sexInd[sexInd<stop] # only index for "all citizenship" counts

Idx <- rbind(sexInd, sexInd + 23) # index intervals of interest
matidx <- matrix(NA, nrow = ncol(Idx), ncol = 23) # matrix of indexes

for(i in 1:ncol(Idx)){
matidx[i,] <- Idx[1,i]:(Idx[2,i]-1)
}

Ind<- as.vector(t(matidx)) # all the indexes needed
popR1 <- popR[Ind,-1] # matrix needed minus column of region codes

Regions <- unique(popR[,2])[ - c(1, which(unique(popR[,2])%in% c("Regione", "Totale")))] # Regions list, first element is an empty case
Ages <- 0:100 # Ages list
Sex <- c("Male", "Female") # Sexes list
Years <- 2002:2011 # Years list

# Length of each dimension
Nreg <- length(Regions)

Nsex <- length(Sex)
Nage <- length(Ages)
Nyear <- length(Years)
popR2<- popR1[popR1[,1] %in% Regions,-1] # Only counts without region column (1)

# Creation of the array
PopArray1 <- array(as.numeric(popR2), dim = c(Nreg, Nsex, Nyear, Nage),
                   dimnames = list(region = Regions,
                                   sex = Sex,
                                   time = Years,
                                   age = Ages))

# Add provinces of Trento and Bolzano
stopP <- which(popP[,1] == "Cittadinanza italiana - Anno: 2002") #Last row of interest

# Indexes for Provinces of interest
idx1 <- which(popP[1:stopP,2] %in% c("Bolzano/Bozen","Trento"))
# Indexes corresponding to Total, i.e. to exclude to keep only male and female
idxTot <- c(seq(1,(length(idx1)-5), by=6),seq(2,(length(idx1)-4), by=6))
IndP <- idx1[-idxTot] # Removing rows number corresponding to Total

TrBz<- popP[IndP, -c(1:2)] # Selecting only rows of interest

# Creating array for Bolzano and Trento
TrBzArray<- array(as.numeric(TrBz), dim = c(2, Nsex, Nyear, Nage),
                   dimnames = list(region = c("Bolzano/Bozen","Trento"),
                                   sex = Sex,
                                   time = Years,
                                   age = Ages))

Pop1 <- abind(PopArray1, TrBzArray, along = 1) # Unifying arrays
Pop2 <- aperm(Pop1, c(4,2,1,3)) # Changing dimension order for demest

# Put Bolzano and Trento below region of Trentino Alto-Adige and Female before Males
Pop3 <- Pop2
Pop3 <- Pop2[,c(2,1),c(1:4,21,22,5:20),]
dimnames(Pop3)[[3]][5:22] <- dimnames(Pop2)[[3]][c(21,22,5:20)]
dimnames(Pop3)[[2]] <- dimnames(Pop2)[[2]][c(2,1)]

#--------------------------------
# Population 1/1/2012 - 1/1/2017
#--------------------------------

# Function
transform.data <- function(R,P){
  R <- R %>%
    select(c(V1, V7, V12)) %>%
    slice(-c(1,2))

  first<- unique(R$V1)[1:4]
  P <- P %>%
    select(c(V2, V8, V13)) %>%
    filter(V2 %in% c("Bolzano/Bozen", "Trento"))
  Pop <- rbind(as.matrix(R[R$V1 %in% first,]),
                 as.matrix(P),
                 as.matrix(R[!R$V1 %in% first,]))
  dim(Pop)
  PopArr <- array(NA, dim = c(Nage, Nsex, length(unique(Pop[,1]))))
  for(i in seq(length(unique(Pop[,1])))){
    PopArr[,,i] <- abind(Pop[(1+(i-1)*101):(i*101),2:3])
  }
  mode(PopArr) <- "numeric"
  PopArr <- PopArr[,c(2,1),]
  return(PopArr)

}

# Load data

R12<- read.csv("regioni12.csv", header = FALSE)
P12 <- read.csv("province12.csv", header = FALSE)
R13 <- read.csv("regioni13.csv", header = FALSE)
P13 <- read.csv("province13.csv", header = FALSE)
R14 <- read.csv("regioni14.csv", header = FALSE)
P14 <- read.csv("province14.csv", header = FALSE)
R15 <- read.csv("regioni15.csv", header = FALSE)
P15 <- read.csv("province15.csv", header = FALSE)
R16 <- read.csv("regioni16.csv", header = FALSE)
P16 <- read.csv("province16.csv", header = FALSE)
R17 <- read.csv("regioni17.csv", header = FALSE)
P17 <- read.csv("province17.csv", header = FALSE)

Pop12 <- transform.data(R12,P12)
Pop13 <- transform.data(R13, P13)
Pop14 <- transform.data(R14, P14)
Pop15 <- transform.data(R15, P15)
Pop16 <- transform.data(R16, P16)
Pop17 <- transform.data(R17, P17)

Pop0217 <- abind(Pop3, Pop12, Pop13, Pop14, Pop15, Pop16, Pop17, along = 4)
dimnames(Pop0217) <- list(age = Ages,
                          sex = Sex[c(2,1)],
                          region = dimnames(Pop3)[[3]],
                          time = 2002:2017)

italy.popn.reg.1Y <- Pop0217

# Create 5 years age groups + group "100+"
Age5y<- Counts(italy.popn.reg.1Y[-101,,,], dimscales = c(time="Points"))%>%
  collapseIntervals(dimension = "age", width = 5)
italy.popn5Y.100 <- abind(as.array(Age5y), italy.popn.reg.1Y[101,,,], along= 1)
dimnames(italy.popn5Y.100)[[1]][21] <- "100+"
dimnames(italy.popn5Y.100) <- list(age = c(dimnames(Age5y)$age, "100+"),
                                   sex = Sex[c(2,1)],
                                   region = dimnames(Pop3)[[3]],
                                   time = 2002:2017)

# Create age group 90+
Age90 <- apply(italy.popn5Y.100[19:21,,,], c(2,3,4), sum)
italy.popn.reg<-abind(italy.popn5Y.100[1:18,,,], Age90, along = 1)
dimnames(italy.popn.reg) <- list(age = c(dimnames(Age5y)$age[1:18], "90+"),
                                 sex = Sex[c(2,1)],
                                 region = dimnames(Pop3)[[3]],
                                 time = 2002:2017)

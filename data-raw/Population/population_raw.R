library(abind)
library(tidyverse)
library(demest)

#--------------------------------
# Population 1/1/2006 - 1/1/2017
#--------------------------------

# Function
transform.data <- function(R,P){
  R <- R %>%
    select(c(V1, V7, V12)) %>%
    slice(-c(1,2))

  first<- unique(R$V1)[1:4]
  P <- P %>%
    select(c(V2, V8, V13)) %>%
    filter(V2 %in% unique(P$V2)[23:24])

  Pop <- rbind(as.matrix(R[R$V1 %in% first,]),
                 as.matrix(P),
                 as.matrix(R[!R$V1 %in% first,]))
  dim(Pop)
  Regions <-  unique(Pop[,1])
  Ages <- 0:100 # Ages list
  Sex <- c("Male", "Female") # Sexes list

  # Length of each dimension
  Nreg <- length(Regions)
  Nsex <- length(Sex)
  Nage <- length(Ages)

  PopArr <- array(NA, dim = c(Nage, Nsex, Nreg))
  for(i in seq(Nreg)){
    PopArr[,,i] <- abind(Pop[(1+(i-1)*101):(i*101),2:3])
  }
  mode(PopArr) <- "numeric"
  PopArr <- PopArr[,c(2,1),]
  return(PopArr)

}

# Load data
R06 <- read.csv("regioni06.csv", header = FALSE)
P06 <- read.csv("province06.csv", header = FALSE)
R07 <- read.csv("regioni07.csv", header = FALSE)
P07 <- read.csv("province07.csv", header = FALSE)
R08 <- read.csv("regioni08.csv", header = FALSE)
P08 <- read.csv("province08.csv", header = FALSE)
R09 <- read.csv("regioni09.csv", header = FALSE)
P09 <- read.csv("province09.csv", header = FALSE)
R10 <- read.csv("regioni10.csv", header = FALSE)
P10 <- read.csv("province10.csv", header = FALSE)
R11 <- read.csv("regioni11.csv", header = FALSE)
P11 <- read.csv("province11.csv", header = FALSE)
R12 <- read.csv("regioni12.csv", header = FALSE)
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

Pop06 <- transform.data(R06,P06)
Pop07 <- transform.data(R07,P07)
Pop08 <- transform.data(R08,P08)
Pop09 <- transform.data(R09,P09)
Pop10 <- transform.data(R10,P10)
Pop11 <- transform.data(R11,P11)
Pop12 <- transform.data(R12,P12)
Pop13 <- transform.data(R13, P13)
Pop14 <- transform.data(R14, P14)
Pop15 <- transform.data(R15, P15)
Pop16 <- transform.data(R16, P16)
Pop17 <- transform.data(R17, P17)

Pop0617 <- abind(Pop06, Pop07, Pop08, Pop09, Pop10, Pop11, Pop12, Pop13, Pop14, Pop15, Pop16, Pop17, along = 4)

Ages <- 0:100 # Ages list
Sex <- c("Male", "Female") # Sexes list
Regions <- c("Piemonte","Valle D'Aosta","Lombardia",
              "Trentino Alto Adige","Bolzano/Bozen","Trento",
              "Veneto","Friuli Venezia Giulia", "Liguria",
              "Emilia Romagna","Toscana","Umbria",
              "Marche","Lazio","Abruzzo",
              "Molise","Campania","Puglia",
              "Basilicata","Calabria","Sicilia",
              "Sardegna")
Years <- 2006:2017 # Years list


dimnames(Pop0617) <- list(age = Ages,
                          sex = Sex[c(2,1)],
                          region = Regions,
                          time = Years)

italy.popn.reg.1Y <- Pop0617

# Create 5 years age groups + group "100+"
Age5y<- Counts(italy.popn.reg.1Y[-101,,,], dimscales = c(time="Points"))%>%
  collapseIntervals(dimension = "age", width = 5)
italy.popn5Y.100 <- abind(as.array(Age5y), italy.popn.reg.1Y[101,,,], along= 1)
dimnames(italy.popn5Y.100)[[1]][21] <- "100+"
dimnames(italy.popn5Y.100) <- list(age = c(dimnames(Age5y)$age, "100+"),
                                   sex = Sex[c(2,1)],
                                   region = Regions,
                                   time = 2006:2017)

# Create age group 90+
Age90 <- apply(italy.popn5Y.100[19:21,,,], c(2,3,4), sum)
italy.popn.reg<-abind(italy.popn5Y.100[1:18,,,], Age90, along = 1)
dimnames(italy.popn.reg) <- list(age = c(dimnames(Age5y)$age[1:18], "90+"),
                                 sex = Sex[c(2,1)],
                                 region = Regions,
                                 time = 2006:2017)

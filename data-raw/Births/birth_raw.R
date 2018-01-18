#----------------------
# Births 2006-2016
#----------------------

library(xlsx)
library(abind)

# Age groups
Age <- c("<18", "18-19", "20-24", "25-29",
         "30-34", "35-39", "40-44", "45-49",
         "50+", "NA")
Aidx <- length(Age)

# Regions
Regions <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA",
             "TRENTINO-ALTO ADIGE", "BOLZANO-BOZEN",
             "TRENTO", "VENETO", "FRIULI-VENEZIA GIULIA",
             "LIGURIA", "EMILIA-ROMAGNA", "TOSCANA",
             "UMBRIA", "MARCHE", "LAZIO", "ABRUZZO",
             "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA",
             "CALABRIA", "SICILIA", "SARDEGNA")
Ridx <- length(Regions)

# Years
Years <- c(2006:2016)
t <- length(Years)

# Function to extract birth data
birth.data <- function(data){

  # Take only the mothers' age
  ba <- data[data[ , 1] == "Totale", ]

  # Eliminate NAs rows and take only relevant counts
  bb <- as.matrix(ba[-which(is.na(ba[ ,1])), 2:11])

  # Take only regions not country and macro-areas
  # make it numeric
  bc <- apply(bb[1:22, ], 2, as.numeric)

  # Check no NAs
  ifelse(is.na(bc), print("Error: NAs data"), bc)

  colnames(bc) <- Age
  rownames(bc) <- Regions
  return(bc)
}

# Apply function for each year
BpR06<- read.xlsx("BpR06.xls",sheetIndex = 1)
b06 <- birth.data(BpR06)
BpR07<- read.xlsx("BpR07.xls",sheetIndex = 1)
b07 <- birth.data(BpR07)
BpR08<- read.xlsx("BpR08.xls",sheetIndex = 1)
b08 <- birth.data(BpR08)
BpR09 <- read.xlsx("BpR09.xls", sheetIndex = 1)
b09 <- birth.data(BpR09)
BpR10 <- read.xlsx("BpR10.xls", sheetIndex = 1)
b10 <- birth.data(BpR10)
BpR11 <- read.xlsx("BpR11.xls", sheetIndex = 1)
b11 <- birth.data(BpR11)
BpR12<- read.xlsx("BpR12.xls",sheetIndex = 1)
b12 <- birth.data(BpR12)
BpR13 <- read.xlsx("BpR13.xls", sheetIndex = 1)
b13 <- birth.data(BpR13)
BpR14 <- read.xlsx("BpR14.xls", sheetIndex = 1)
b14 <- birth.data(BpR14)
BpR15 <- read.xlsx("BpR15.xls", sheetIndex = 1)
b15 <- birth.data(BpR15)
BpR16 <- read.xlsx("BpR16.xls", sheetIndex = 1)
b16 <- birth.data(BpR16)

dimn <- list(region = Regions, age = Age, time = Years)

italy.births.regNA <- abind(b06, b07 , b08, b09, b10, b11, b12,
                          b13, b14, b15, b16, along = 3)

dimnames(italy.births.regNA)<- dimn

# Modify age groups and distribute NAs births

tot.birth <- apply(italy.births.regNA[,1:9,], c(1, 3), sum)

births.prop <- array(NA, dim = c(22,9,11))
NAspread <- array(NA, dim = c(22,9,11))
italy.births.reg <- array(NA, dim = c(22,7,11))

for(i in 1:dim(italy.births.regNA)[3]){
  births.prop[,,i] <- italy.births.regNA[,1:9,i]/tot.birth[,i]
  NAspread[,,i] <- births.prop[,,i]*italy.births.regNA[,10,i]
  births.new <- round(italy.births.regNA[,1:9,]+NAspread)
  italy.births.reg[,,i] <- cbind(rowSums(births.new[,1:2,i]),
                            births.new[,3:7,i],
                            rowSums(births.new[,8:9,i]))

}

dimnames(italy.births.reg) <- list(region = Regions,
                                   age = c("15-19", "20-24", "25-29",
                                           "30-34", "35-39", "40-44", "45-49"),
                                   time = Years)

#----------------------
# Deaths 2006-2016
#----------------------

library(xlsx)
library(abind)
library(demItaly)

# NOTE: The format changes until 2009 the last age group is 100+
# from 2010 to 2013 it is 90+ and from 2014 onward 95+
# so all thedata will be adjusted to 90+

# Function to select regional data for 2006-2009 data
transf <- function(data){

  data[data$PROVINCE.E.REGIONI.DI.RESIDENZA..  %in% c("Piemonte", "Valle d'Aosta/Vallee d'Aoste", "Lombardia",
                                                      "Trentino-Alto Adige", "Bolzano/Bozen",
                                                      "Trento", "Veneto", "Friuli-Venezia Giulia",
                                                      "Liguria", "Emilia-Romagna", "Toscana",
                                                      "Umbria", "Marche", "Lazio", "Abruzzo",
                                                      "Molise", "Campania", "Puglia", "Basilicata",
                                                      "Calabria", "Sicilia", "Sardegna"),]
}

# Deaths 2006-2009
setwd("~/demItaly/data-raw/Deaths")
# Male and Female data
D06m <- read.xlsx("D2006.xlsx", sheetIndex = 1, startRow = 1, endRow = 131, header = TRUE)
D06f <- read.xlsx("D2006.xlsx", sheetIndex = 2, startRow = 1, endRow = 131, header = TRUE)
D07m <- read.xlsx("D2007.xlsx", sheetIndex = 1, startRow = 1, endRow = 131, header = TRUE)
D07f <- read.xlsx("D2007.xlsx", sheetIndex = 2, startRow = 1, endRow = 131, header = TRUE)
D08m <- read.xlsx("D2008.xlsx", sheetIndex = 1, startRow = 1, endRow = 131, header = TRUE)
D08f <- read.xlsx("D2008.xlsx", sheetIndex = 2, startRow = 1, endRow = 131, header = TRUE)
D09m <- read.xlsx("D2009.xlsx", sheetIndex = 1, startRow = 1, endRow = 131, header = TRUE)
D09f <- read.xlsx("D2009.xlsx", sheetIndex = 2, startRow = 1, endRow = 131, header = TRUE)

d06m <- transf(D06m)
d06f <- transf(D06f)
d07m <- transf(D07m)
d07f <- transf(D07f)
d08m <- transf(D08m)
d08f <- transf(D08f)
d09m <- transf(D09m)
d09f <- transf(D09f)

# Matrix without regiorn column (first column)
data0609 <- as.matrix(cbind(d06m[ ,2:ncol(d06m)], d06f[ ,2:ncol(d06f)], d07m[,2:ncol(d07m)],
                            d07f[ ,2:ncol(d07f)], d08m[ ,2:ncol(d08m)], d08f[,2:ncol(d08f)],
                            d09m[ ,2:ncol(d09m)], d09f[ ,2:ncol(d09f)]))
mode(data0609) <- "numeric"

# Dimensions names
load("~/demItaly/data/italy.popn.reg.rda")
Age <- dimnames(italy.popn.reg)[[1]]
Sex <- dimnames(italy.popn.reg)[[2]]
Regions <- dimnames(italy.popn.reg)[[3]]

D0609 <- array(data0609, dim = c(22, 23, 2, 4))

# Change in Age groups
d0609a <- apply(D0609[ , 1:2, , ], c(1, 3, 4), sum) # From 0, 1-4 to 0-4
d0609b <- apply(D0609[ , 3:4, , ], c(1,3,4), sum) # From 5, 6-9 to 5-9
d0609c <- apply(D0609[ , 21:23, , ], c(1, 3, 4), sum) # From 90-94, 95-99, 100+ to 90+
D69 <- abind(d0609a, d0609b, D0609[ , 5:20, , ], d0609c, along = 2)
d0609ok <- aperm(D69, c(2, 3, 1, 4))
d0609ok <- d0609ok[,c(2,1),,]
dimnames(d0609ok) <- list(age = Age,
                          sex = Sex,
                          region = Regions,
                          time = 2006:2009)
#------------------
# Deaths 2010-2013
#------------------

D1013 <- read.xlsx("D2010-2013.xlsx", sheetIndex = 1, startRow = 1, endRow = 25)
D1013 <- as.matrix(D1013)

#create an array of 4 dimension with rows=age classes, col=sex, time, region

D201013 <- array(data = D1013[4:23, 2:ncol(D1013)],
                 dim = c(20, 2, 4, 22))
D1013t <- aperm(D201013, c(1, 2, 4, 3))
mode(D1013t) <- "numeric"

d1013a <- apply(D1013t[1:2, , , ], c(2, 3, 4), sum) # From "0" and "1-4" to "0-4"
d1013b <- abind(d1013a, D1013t[3:nrow(D1013t), , , ], along = 1)
d1013b <- d1013b[,c(2,1),,]
dimnames(d1013b) <- list(age = Age,
                         sex = Sex,
                         region = Regions,
                         time = 2010:2013)
#-------------
# Deaths 2014
#-------------

D14 <- read.csv("D2014.csv", header = TRUE)

vars <- colnames(D14)
Regions <- unique(D14$Denominazione.territoriale)[9:30]


# Subarray of interest
D14sub <- D14 %>%
  select(vars[-c(1:3)]) %>% # remove variables not of interes
  filter(Denominazione.territoriale %in% Regions) %>% # select only regions of interest
  filter(Sesso != "Totale") %>% # only M and F
  filter(Classi.di.eta != "Totale") %>% # only age groups not total
  distinct() # remove duplicates (i.e. Valle d'Aosta twice)

Nsex14 <- length(unique(D14sub$Sesso))
Nreg14 <-length(Regions)
Nage14 <-length(unique(D14sub$Classi.di.eta))

D14arr <- array(D14sub$Decessi, dim = c(Nsex14,Nage14, Nreg14))
D14arr2 <- aperm(D14arr, c(2,1,3))
D14arr2 <- D14arr2[,c(2,1), c(1:4, 21, 22, 5:20)]
D14arr3<- apply(D14arr2[19:20,,], c(2,3),sum)
D14ok <- abind(D14arr2[1:18,,], D14arr3, along = 1)

dimnames(D14ok) <- list(age = Age,
                          sex = Sex,
                          region= Regions)

#--------------------------------------
# Deaths 2015, same formatting as 2014
#--------------------------------------
D15 <- read.csv("D2015.csv", header = TRUE)

vars <- colnames(D15)
Regions <- unique(D15$Denominazione.territoriale)[9:30]


# Subarray of interest
D15sub <- D15 %>%
  select(vars[-c(1:3,8)]) %>% # remove variables not of interes
  filter(Denominazione.territoriale %in% Regions) %>% # select only regions of interest
  filter(Sesso != "Maschi e femmine") %>% # only M and F
  filter(Classi.di.eta != "Totale") %>% # only age groups not total
  distinct() # remove duplicates (i.e. Valle d'Aosta twice)

Nsex15 <- length(unique(D15sub$Sesso))
Nreg15 <-length(Regions)
Nage15 <-length(unique(D15sub$Classi.di.eta))

D15arr <- array(D15sub$Decessi, dim = c(Nsex15,Nage15, Nreg15))
D15arr2 <- aperm(D15arr, c(2,1,3))
D15arr2 <- D15arr2[,c(2,1), c(1:4, 21, 22, 5:20)]
D15arr3<- apply(D15arr2[19:20,,], c(2,3),sum)
D15ok <- abind(D15arr2[1:18,,], D15arr3, along = 1)

dimnames(D15ok) <- list(age = Age,
                          sex = Sex,
                          region= Regions)

# TOTAL

Dead<- abind(d0609ok, d1013b, D14ok, D15ok)

dimnames(Dead) <- list(age = Age,
                       sex = Sex,
                       region = Regions,
                       time = c(2006:2015))

italy.deaths.reg <- Dead


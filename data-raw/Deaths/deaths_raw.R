#----------------------
# Deaths 2006-2016
#----------------------

library(xlsx)
library(abind)

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
Regions <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA",
             "TRENTINO-ALTO ADIGE", "BOLZANO-BOZEN",
             "TRENTO", "VENETO", "FRIULI-VENEZIA GIULIA",
             "LIGURIA", "EMILIA-ROMAGNA", "TOSCANA",
             "UMBRIA", "MARCHE", "LAZIO", "ABRUZZO",
             "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA",
             "CALABRIA", "SICILIA", "SARDEGNA")
Sex <- c("Male","Female")
Age100 <- c("0", "1-4", "5", "6-9","10-14", "15-19", "20-24", "25-29",
           "30-34", "35-39", "40-44", "45-49", "50-54",
           "55-59","60-64","65-69", "70-74", "75-79",
           "80-84", "85-89", "90-94", "95-99", "100+")
Age95 <- c("0-4", "5-9","10-14", "15-19", "20-24", "25-29",
           "30-34", "35-39", "40-44", "45-49", "50-54",
           "55-59","60-64","65-69", "70-74", "75-79",
           "80-84", "85-89", "90-94", "95+")
Age <- c("0-4", "5-9","10-14", "15-19", "20-24", "25-29", "30-34",
         "35-39", "40-44", "45-49", "50-54", "55-59","60-64",
         "65-69", "70-74", "75-79", "80-84", "85-89" , "90+")


dimn95 <- list(sex = Sex, age = Age95, region = Regions)
dimn90 <- list(sex = Sex, age = Age, region = Regions)

dimn0609 <- list(region = Regions, age = Age100,
                 sex = Sex, time = c(2006:2009))

D0609 <- array(data0609, dim = c(22, 23, 2, 4), dimnames = dimn0609)

# Change in Age groups
d0609a <- apply(D0609[ , 1:2, , ], c(1, 3, 4), sum) # From 0, 1-4 to 0-4
d0609b <- apply(D0609[ , 3:4, , ], c(1,3,4), sum) # From 5, 6-9 to 5-9
d0609c <- apply(D0609[ , 21:23, , ], c(1, 3, 4), sum) # From 90-94, 95-99, 100+ to 90+
D69 <- abind(d0609a, d0609b, D0609[ , 5:20, , ], d0609c, along = 2,
             new.names = list(region = Regions, age = Age,
                              sex = Sex, time = c(2006:2009)))
d0609ok <- aperm(D69, c(2, 3, 1, 4))

# Deaths 2010-2013

D1013 <- read.xlsx("D2010-2013.xlsx", sheetIndex = 1, startRow = 1, endRow = 25)
D1013 <- as.matrix(D1013)

#create an array of 4 dimension with rows=age classes, col=sex, time, region
dimn1013 <- list(age = c("0", Age) , sex = Sex,
                 time = c(2010:2013), region = Regions)

D201013 <- array(data = D1013[4:23, 2:ncol(D1013)],
                 dim = c(20, 2, 4, 22), dimnames = dimn1013)
D1013t <- aperm(D201013, c(1, 2, 4, 3))
mode(D1013t) <- "numeric"

d1013a <- apply(D1013t[1:2, , , ], c(2, 3, 4), sum) # From "0" and "1-4" to "0-4"
d1013b <- abind(d1013a, D1013t[3:nrow(D1013t), , , ], along = 1,
                new.names = list(age = Age, sex = Sex,
                                 region = Regions, time = c(2010:2013)))

# Deaths 2014
D14 <- read.csv("D2014.csv", header = TRUE)
D14 <- D14[D14$Entita..territoriale == "Regione", ]
D14 <- D14[D14$Classi.di.eta != "Totale" & D14$Sesso != "Totale", ]
first <- D14[D14$Denominazione.territoriale %in% c("Piemonte", "Valle d'Aosta/Vall?e d'Aoste", "Lombardia",
                                                 "Trentino-Alto Adige"), ]
trbz <- D14[D14$Denominazione.territoriale %in%
            c("Provincia Autonoma di Bolzano","Provincia Autonoma di Trento") , ]
others <- D14[D14$Denominazione.territoriale %in% c("Veneto", "Friuli-Venezia Giulia",
                                                  "Liguria", "Emilia-Romagna", "Toscana",
                                                  "Umbria", "Marche", "Lazio", "Abruzzo",
                                                  "Molise", "Campania", "Puglia", "Basilicata",
                                                  "Calabria", "Sicilia", "Sardegna"), ]
# Include TR and BZ in the right place
D14ok <- rbind(first, trbz, others)
# sex, age groups, region
D14tot <- array(D14[ ,7], dim = c(2, 20, 22), dimnames = dimn95)
D14_90 <- apply(D14tot[ , 19:20, ], c(1, 3), sum) # From 90-94 and 95+ to 90+
d14 <- abind(D14tot[ , 1:18, ], D14_90, along = 2, new.names = dimn90)
D14t <- aperm(d14, c(2, 1, 3)) # right dimension order

# Deaths 2015, same as for 2014
D15 <- read.csv("D2015.csv", header = TRUE)
D15 <- D15[D15$Entita.territoriale == "Regione", ]
D15 <- D15[D15$Classi.di.eta != "Totale" & D15$Sesso != "Maschi e femmine", ]
first <- D15[D15$Denominazione.territoriale %in% c("Piemonte", "Valle d'Aosta/Vall?e d'Aoste", "Lombardia",
                                                 "Trentino-Alto Adige"), ]
trbz <- D15[D15$Denominazione.territoriale %in%
            c("Provincia Autonoma di Bolzano","Provincia Autonoma di Trento") , ]
others <- D15[D15$Denominazione.territoriale %in% c("Veneto", "Friuli-Venezia Giulia",
                                                  "Liguria", "Emilia-Romagna", "Toscana",
                                                  "Umbria", "Marche", "Lazio", "Abruzzo",
                                                  "Molise", "Campania", "Puglia", "Basilicata",
                                                  "Calabria", "Sicilia", "Sardegna"),]
D15ok <- rbind(first, trbz, others)
D15tot <- array(D15[ , 7], dim = c(2, 20, 22), dimnames = dimn95)
D15_90 <- apply(D15tot[ , 19:20, ], c(1, 3), sum)
d15 <- abind(D15tot[ , 1:18, ], D15_90, along = 2, new.names = dimn90)
D15t <- aperm(d15, c(2, 1, 3)) #dimensioni come per 2015

# TOTAL
dimn <- list(age = Age, sex = Sex, region = Regions, time = c(2006:2015))
Dead <- abind(d0609ok, d1013b, D14t, D15t, new.names = dimn)
italy.deaths.reg <- array(Dead, dim = c(19, 2, 22, 10), dimnames = dimn)

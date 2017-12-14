#----------------------
# School data 2010-2014
#---------------------


setwd("C:\\Users\\Cha\\Documents\\demItaly\\data-raw\\School")
preprim <- read.csv("preprim.csv", header = TRUE, sep=",")
prim1 <- read.csv("prim.csv", header = TRUE, sep=",")
lowsec <- read.csv("lowersec.csv", header = TRUE, sep=",")
upsec <- read.csv("uppersec.csv", header = TRUE, sep=",")

# Bolzano: years 2010 and 2014 are missing
# Trento: year 2010 is missing

head(prim1)
prim1 <- prim1[, -c(2,3,8)]
prim1 <- prim1[prim1$Tipo.dato %in% c("iscritti - maschi e femmine","iscritti - femmine"), ]
prim1 <-  prim1[prim1$Gestione.della.scuola=="totale", ]
prim1 <- prim1[ !(prim1$Territorio %in%
                    c("Italia", "Nord-ovest", "Nord-est",
                      "Centro", "Sud", "Isole", "Provincia Autonoma Bolzano / Bozen",
                      "Provincia Autonoma Trento")),-4]
head(prim1)
dimn<- list(year = c(2010:2014) ,
            region= c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "BOLZANO-BOZEN",
                      "TRENTO", "VENETO", "FRIULI-VENEZIA GIULIA", "LIGURIA", "EMILIA-ROMAGNA",
                      "TOSCANA", "UMBRIA", "MARCHE", "LAZIO", "ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA",
                      "BASILICATA", "CALABRIA", "SICILIA", "SARDEGNA"),
            sex= c("Male","Female"))

prim<- array(prim1$X0, dim = c(5, 20, 2), dimnames = dimn)
males <- prim[ , ,"Male"]- prim[ , ,"Female"]
prim[ , , "Male"] <- males
prim <- aperm(prim, c(2,3,1))
prim <- prim[c(1,2,4,5,6,7,3,8:20), , ]
unique(prim1$Territorio)
save(prim, file="prim.RData")
prim

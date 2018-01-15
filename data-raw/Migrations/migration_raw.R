#----------------------
# Migrations 2006-2014
#---------------------

library(xlsx)
library(abind)

# Internal immigration and emigration
immtot <- read.xlsx("imm0614.xlsx", sheetIndex = 1,
                    startRow = 3, endRow = 139, header = FALSE)
emitot <- read.xlsx("emi0614.xlsx", sheetIndex = 1,
                    startRow = 3, endRow = 139, header = FALSE)
# External immigration and emigration
immest <- read.xlsx("imm0614est.xlsx", sheetIndex = 1,
                  startRow = 5, endRow = 141, header = FALSE)
emiest <- read.xlsx("emi0614est.xlsx", sheetIndex = 1,
                  startRow = 5, endRow = 141, header = FALSE)

# Vector of region names as in the .xlxs file
RegOrig <- c("Piemonte", "Valle d'Aosta / Vallee d'Aoste",
             "Lombardia", "Trentino-Alto Adige",
             "Provincia Autonoma Bolzano",
             "Provincia Autonoma Trento", "Veneto",
             "Friuli-Venezia Giulia", "Liguria",
             "Emilia-Romagna", "Toscana", "Umbria",
             "Marche", "Lazio", "Abruzzo", "Molise",
             "Campania", "Puglia", "Basilicata",
             "Calabria", "Sicilia","Sardegna")

imm <- immtot[immtot$X1 %in% RegOrig, ]
imm <- as.matrix(imm[ ,2:ncol(imm)])

emi <- emitot[emitot$X1 %in% RegOrig, ]
emi <- as.matrix(emi[ ,2:ncol(emi)])

EI <- immest[immest$X1 %in% RegOrig, ]
EI <- as.matrix(EI[ ,2:ncol(EI)])

EO <- emiest[emiest$X1 %in% RegOrig, ]
EO <- as.matrix(EO[,2:ncol(EO)])

dimn <- list(region = c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA", "TRENTINO-ALTO ADIGE", "BOLZANO-BOZEN",
                      "TRENTO", "VENETO", "FRIULI-VENEZIA GIULIA", "LIGURIA", "EMILIA-ROMAGNA",
                      "TOSCANA", "UMBRIA", "MARCHE", "LAZIO", "ABRUZZO", "MOLISE", "CAMPANIA", "PUGLIA",
                      "BASILICATA", "CALABRIA", "SICILIA", "SARDEGNA"),
           sex = c("Male", "Female"),
           age =c("0-17", "18-39",	"40-64",	"65+"),
           time = c(2006:2014))

R <- length(dimn$region)
S <- length(dimn$sex)
A <- length(dimn$age)
Y <- length(dimn$time)

italy.intl.imm <- array(imm, dim = c(R, S, A, Y), dimnames = dimn)
italy.intl.emi <- array(emi, dim = c(R, S, A, Y), dimnames = dimn)
italy.ext.imm <- array(EI, dim = c(R, S, A, Y), dimnames = dimn)
italy.ext.emi <- array(EO, dim = c(R, S, A, Y), dimnames = dimn)



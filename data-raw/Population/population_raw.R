#----------------------
# Population 2006-2016
#----------------------

library(abind)

# Load datasets
# Regional data from 2002 to 2011
popR <- read.csv("PopolazioneEta-Territorio-Regioni.csv", sep=";", header = FALSE)
# Province data from 2002 to 2011
popP <- read.csv("PopolazioneEta-Territorio-Province.csv", sep=";", header = FALSE)

# Select only all citizenship from 2006 to 2011
popP <- popP[1370:3411, ]
# Provinces of Bolzano (BZ) and Trento (TR)
BZ <- popP[popP$V1==21, ]
TR <- popP[popP$V1==22, ]

#
Regions <- c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA",
             "TRENTINO-ALTO ADIGE", "BOLZANO-BOZEN",
             "TRENTO", "VENETO", "FRIULI-VENEZIA GIULIA",
             "LIGURIA", "EMILIA-ROMAGNA", "TOSCANA",
             "UMBRIA", "MARCHE", "LAZIO", "ABRUZZO",
             "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA",
             "CALABRIA", "SICILIA", "SARDEGNA")

Sex <- c("Male","Female")

# Indexes
Ridx <- length(Regions)
Sidx <- length(Sex)
A100idx <- 101
A90idx <- 19

# Select separately male and female population
# Indexes
idxM <- NULL
idxF <- NULL
for(i in 1:6){
  idxM[i] <- (2 + 3 * (i - 1))
  idxF[i] <- (3 * i)
}

# Select males and females for BZ and TR
BZ_M <- BZ[idxM, 3:103]
BZ_F <- BZ[idxF, 3:103]

TR_M <- BZ[idxM, 3:103]
TR_F <- BZ[idxF, 3:103]

# Take all citizenships population for each year
# Male and female separately
M06 <- popR[1023:1042, 3:103]
F06 <- popR[1046:1065, 3:103]
M07 <- popR[1094:1113, 3:103]
F07 <- popR[1117:1136, 3:103]
M08 <- popR[1165:1184, 3:103]
F08 <- popR[1188:1207, 3:103]
M09 <- popR[1236:1255, 3:103]
F09 <- popR[1259:1278, 3:103]
M10 <- popR[1307:1326, 3:103]
F10 <- popR[1330:1349, 3:103]
M11 <- popR[1378:1397, 3:103]
F11 <- popR[1401:1420, 3:103]

# Add BZ and TR to regions
# Male population
M06 <- rbind(M06[1:4, ], BZ_M[1, ], TR_M[1, ], M06[5:nrow(M06), ])
M07 <- rbind(M07[1:4, ], BZ_M[2, ], TR_M[2, ], M07[5:nrow(M07), ])
M08 <- rbind(M08[1:4, ], BZ_M[3, ], TR_M[3, ], M08[5:nrow(M08), ])
M09 <- rbind(M09[1:4, ], BZ_M[4, ], TR_M[4, ], M09[5:nrow(M09), ])
M10 <- rbind(M10[1:4, ], BZ_M[5, ], TR_M[5, ], M10[5:nrow(M10), ])
M11 <- rbind(M11[1:4, ], BZ_M[6, ], TR_M[6, ], M11[5:nrow(M11), ])

# Single year age groups matrix
male_allclass <- as.matrix(rbind(M06, M07, M08, M09, M10, M11))
mode(male_allclass) <- "numeric"
dim(male_allclass) # Check dimension: matrix (22regions*6years=)132rows x 101 age classes columns

# Female population
F06 <- rbind(F06[1:4, ], BZ_F[1, ], TR_F[1, ], F06[5:nrow(F06), ])
F07 <- rbind(F07[1:4, ], BZ_F[2, ], TR_F[2, ], F07[5:nrow(F07), ])
F08 <- rbind(F08[1:4, ], BZ_F[3, ], TR_F[3, ], F08[5:nrow(F08), ])
F09 <- rbind(F09[1:4, ], BZ_F[4, ], TR_F[4, ], F09[5:nrow(F09), ])
F10 <- rbind(F10[1:4, ], BZ_F[5, ], TR_F[5, ], F10[5:nrow(F10), ])
F11 <- rbind(F11[1:4, ], BZ_F[6, ], TR_F[6, ], F11[5:nrow(F11), ])

# Single year age groups matrix
female_allclass <- as.matrix(rbind(F06, F07, F08, F09, F10, F11))
mode(female_allclass) <- "numeric"
dim(female_allclass)

# Create array for 2006-2011
MF <- t(cbind(male_allclass, female_allclass))
# Dimensions names
dimn0611 <- list(age = c(0:99, "100+"), sex = Sex,
                region = Regions, year = c(2006:2011))

Pop0611 <- array(MF, dim = c(A100idx, Sidx, Ridx, 6), dimnames = dimn0611)

# Random checkings
for(i in 1:Ridx){
  # Expect "integer(0)" for all i
  print(which(Pop0611[, 1, i, 3] != M08[i, ]))
}

# From 2012 to 2017

# Single year dimensions names
dimn1217<- list(age = c(0:99, "100+"), sex = Sex, region = Regions)

# Function to apply to datasets
single.year.matrix <- function(region, province, dimn){
  #Select data of interest
  R <- region[3:nrow(region), c(7, 12)]
  mode(R) <- "numeric"
  # Provinces
  BZ <- as.matrix(province[province$V1=="021", c(8, 13)])
  TR <- as.matrix(province[P12$V1=="022", c(8, 13)])
  mode(BZ) <- "numeric"
  mode(TR) <- "numeric"
  # Complete matrices
  MM <- matrix(c(R[1:404, 1], BZ[ ,1], TR[ ,1], R[405:nrow(R), 1]), Ridx, byrow = TRUE)
  FF<- matrix(c(R[1:404, 2], BZ[ ,2], TR[ ,2], R[405:nrow(R), 2]), Ridx, byrow = TRUE)
  MF <- t(cbind(MM, FF))
  # Array
  Pop <- array(MF, dim = c(A100idx, Sidx, Ridx), dimnames = dimn)
  return(list(Pop = Pop, Males = MM, Females = FF))
}

# Load data
R12 <- as.matrix(read.csv("regioni12.csv", header = FALSE))
P12 <- read.csv("province12.csv", header = FALSE)
R13 <- as.matrix(read.csv("regioni13.csv", header = FALSE))
P13 <- read.csv("province13.csv", header = FALSE)
R14 <- as.matrix(read.csv("regioni14.csv", header = FALSE))
P14 <- read.csv("province14.csv", header = FALSE)
R15 <- as.matrix(read.csv("regioni15.csv", header = FALSE))
P15 <- read.csv("province15.csv", header = FALSE)
R16 <- as.matrix(read.csv("regioni16.csv", header = FALSE))
P16 <- read.csv("province16.csv", header = FALSE)
R17 <- as.matrix(read.csv("regioni17.csv", header = FALSE))
P17 <- read.csv("province17.csv", header = FALSE)

Pop12 <- single.year.matrix(R12, P12, dimn1217)
Pop13 <- single.year.matrix(R13, P13, dimn1217)
Pop14 <- single.year.matrix(R14, P14, dimn1217)
Pop15 <- single.year.matrix(R15, P15, dimn1217)
Pop16 <- single.year.matrix(R16, P16, dimn1217)
Pop17 <- single.year.matrix(R17, P17, dimn1217)


# Random check
for(i in 1:Ridx){
  # Expecting "integer(0)" for all i
  print(which(Pop12$Pop[ , 2, i]!=Pop12$Females[i, ]))
}


# Complete array single year age groups

# Dimensions names
dimn1 <- list(age = c(0:99, "100+"), sex = Sex,
             region = Regions, year = c(2006:2017))

italy.popn.reg.1Y <- abind(Pop0611, Pop12$Pop, Pop13$Pop, Pop14$Pop,
                           Pop15$Pop, Pop16$Pop, Pop17$Pop, along = 4)

# Complete array 5year age groups

# Dimensions names
dimn5 <- list(age = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                     "30-34", "35-39", "40-44", "45-49", "50-54",
                     "55-59", "60-64", "65-69", "70-74", "75-79",
                     "80-84", "85-89", "90-94", "95-99", "100+"),
             sex = Sex, region = Regions, year = c(2006:2017))

italy.popn5Y.100 <- array(NA, dim = c(21, Sidx, Ridx, 12), dimnames = dimn5)

for(t in 1:12){
  for(r in 1:Ridx){
    for(s in 1:Sidx){
      for(i in 1:20){
        italy.popn5Y.100[i, s, r, t] <- sum(italy.popn.reg.1Y[(((i - 1) * 5 + 1):(5 * i)), s, r, t])
      }
      italy.popn5Y.100[21, s, r, t] <- italy.popn.reg.1Y[A100idx, s, r, t]
    }
  }
}

# Complete array 5year age groups, last age group "90+"

# Dimensions names
dimn5.90 <- list(age = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                        "30-34", "35-39", "40-44", "45-49", "50-54",
                        "55-59", "60-64", "65-69", "70-74", "75-79",
                        "80-84", "85-89", "90+"),
                sex = Sex, region = Regions, year = c(2006:2017) )

italy.popn.reg <- array(NA, dim = c(A90idx,Sidx,Ridx,12), dimnames = dimn5.90)
italy.popn.reg[1:18, , , ] <- italy.popn5Y.100[1:18, , , ]

for(t in 1:12){
  for(r in 1:Ridx){
    for(s in 1:Sidx){
      italy.popn.reg[A90idx, s, r, t]<-sum(italy.popn.reg.1Y[A90idx:21, s, r, t])
    }
  }
}

setwd("C:\\Users\\Cha\\Documents\\demItaly\\data-raw")
popR <- read.csv("PopolazioneEta-Territorio-Regioni.csv", sep=";", header=FALSE)
popP <- read.csv("PopolazioneEta-Territorio-Province.csv", sep=";", header=FALSE)
popP <- popP[1370:3411,] # only take all citizenship from 2006 to 2011


# from 2006 to 2011

# provinces of Bolzano (BZ) and Trento (TR)
BZ<-popP[popP$V1==21,]

# Male and female population

# Indexes
idxM<-NULL
idxF<-NULL
for(i in 1:6){
  idxM[i]<-(2+3*(i-1))
  idxF[i]<-(3*i)
}

# Select males and females
BZ_M<-BZ[idxM,3:103]
BZ_F<-BZ[idxF,3:103]

TR<-popP[popP$V1==22,]
TR_M<-BZ[idxM,3:103]
TR_F<-BZ[idxF,3:103]

# take all citizenships population for each year
M06 <- popR[1023:1042,3:103]
F06 <- popR[1046:1065,3:103]
M07 <- popR[1094:1113,3:103]
F07 <- popR[1117:1136,3:103]
M08 <- popR[1165:1184,3:103]
F08 <- popR[1188:1207,3:103]
M09 <- popR[1236:1255,3:103]
F09 <- popR[1259:1278,3:103]
M10 <- popR[1307:1326,3:103]
F10 <- popR[1330:1349,3:103]
M11 <- popR[1378:1397,3:103]
F11 <- popR[1401:1420,3:103]

# add BZ and TR to regions
M06<- rbind(M06[1:4,], BZ_M[1,], TR_M[1,], M06[5:nrow(M06),])
M07<- rbind(M07[1:4,], BZ_M[2,], TR_M[2,], M07[5:nrow(M07),])
M08<- rbind(M08[1:4,], BZ_M[3,], TR_M[3,], M08[5:nrow(M08),])
M09<- rbind(M09[1:4,], BZ_M[4,], TR_M[4,], M09[5:nrow(M09),])
M10<- rbind(M10[1:4,], BZ_M[5,], TR_M[5,], M10[5:nrow(M10),])
M11<- rbind(M11[1:4,], BZ_M[6,], TR_M[6,], M11[5:nrow(M11),])

# Single year age groups
male_allclass<- as.matrix(rbind(M06,M07,M08,M09,M10,M11)) # matrix (22regions*6years=)132rows x 101 age classes columns
mode(male_allclass)<-"numeric"
is.numeric(male_allclass)
dim(male_allclass)


F06<- rbind(F06[1:4,], BZ_F[1,], TR_F[1,], F06[5:nrow(F06),])
F07<- rbind(F07[1:4,], BZ_F[2,], TR_F[2,], F07[5:nrow(F07),])
F08<- rbind(F08[1:4,], BZ_F[3,], TR_F[3,], F08[5:nrow(F08),])
F09<- rbind(F09[1:4,], BZ_F[4,], TR_F[4,], F09[5:nrow(F09),])
F10<- rbind(F10[1:4,], BZ_F[5,], TR_F[5,], F10[5:nrow(F10),])
F11<- rbind(F11[1:4,], BZ_F[6,], TR_F[6,], F11[5:nrow(F11),])

# Single year age groups
female_allclass<-as.matrix(rbind(F06,F07,F08,F09,F10,F11)) #matrix 132 x 101
mode(female_allclass)<-"numeric"
is.numeric(female_allclass)
dim(female_allclass)

MF <- cbind(male_allclass, female_allclass)
dimn0611<- list(age = c(0:99, "100+"),
             sex= c("Male","Female"),
             region= c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA",
                       "TRENTINO-ALTO ADIGE", "BOLZANO- BOZEN",
                       "TRENTO", "VENETO", "FRIULI-VENEZIA GIULIA",
                       "LIGURIA", "EMILIA - ROMAGNA", "TOSCANA",
                       "UMBRIA", "MARCHE", "LAZIO", "ABRUZZO",
                       "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA",
                       "CALABRIA", "SICILIA", "SARDEGNA"),
             year = c(2006:2011))
MFt <- t(MF)
Pop0611 <- array(MFt, dim = c(101, 2, 22, 6), dimnames = dimn0611)

# Check
for(i in 1:22){
print(which(Pop0611[,2,i,1]!=F06[i,]))
}


# From 2012 to 2017

dimn1217<- list(age = c(0:99, "100+"),
                sex= c("Male","Female"),
                region= c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA",
                          "TRENTINO-ALTO ADIGE", "BOLZANO- BOZEN",
                          "TRENTO", "VENETO", "FRIULI-VENEZIA GIULIA",
                          "LIGURIA", "EMILIA - ROMAGNA", "TOSCANA",
                          "UMBRIA", "MARCHE", "LAZIO", "ABRUZZO",
                          "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA",
                          "CALABRIA", "SICILIA", "SARDEGNA"))


R12<-as.matrix(read.csv("regioni12.csv", header= FALSE))
R12<-R12[3:nrow(R12),c(7,12)]
mode(R12)<-"numeric"

P12<-read.csv("province12.csv", header= FALSE)
BZ12<-as.matrix(P12[P12$V1=="021",c(8,13)])
TR12<-as.matrix(P12[P12$V1=="022",c(8,13)])
mode(BZ12)<-"numeric"
mode(TR12)<-"numeric"

# Single year age groups
M12a<-matrix(c(R12[1:404,1],BZ12[,1],TR12[,1],R12[405:nrow(R12),1]),22, byrow = TRUE)
F12a<-matrix(c(R12[1:404,2],BZ12[,2],TR12[,2],R12[405:nrow(R12),2]),22, byrow = TRUE)

MF12 <- cbind(M12a, F12a)
MF12t <- t(MF12)
Pop12 <- array(MF12t, dim = c(101, 2, 22), dimnames = dimn1217)

# Check
for(i in 1:22){
  print(which(Pop12[,1,i]!=M12a[i,]))
}

# 2013
R13<-as.matrix(read.csv("regioni13.csv", header= FALSE))
R13<-R13[3:nrow(R13),c(7,12)]
mode(R13)<-"numeric"

P13<-read.csv("province13.csv", header= FALSE)
BZ13<-as.matrix(P13[P13$V1=="021",c(8,13)])
TR13<-as.matrix(P13[P13$V1=="022",c(8,13)])
mode(BZ13)<-"numeric"
mode(TR13)<-"numeric"

M13a<-matrix(c(R13[1:404,1],BZ13[,1],TR13[,1],R13[405:nrow(R13),1]),22, byrow = TRUE)
F13a<-matrix(c(R13[1:404,2],BZ13[,2],TR13[,2],R13[405:nrow(R13),2]),22, byrow = TRUE)

MF13 <- cbind(M13a, F13a)
MF13t <- t(MF13)
Pop13 <- array(MF13t, dim = c(101, 2, 22), dimnames = dimn1217)

# Check
for(i in 1:22){
  print(which(Pop13[,2,i]!=F13a[i,]))
}

# 2014
R14<-as.matrix(read.csv("regioni14.csv", header= FALSE))
R14<-R14[3:nrow(R14),c(7,12)]
mode(R14)<-"numeric"

P14<-read.csv("province14.csv", header= FALSE)
BZ14<-as.matrix(P14[P14$V1=="021",c(8,13)])
TR14<-as.matrix(P14[P14$V1=="022",c(8,13)])
mode(BZ14)<-"numeric"
mode(TR14)<-"numeric"

M14a<-matrix(c(R14[1:404,1],BZ14[,1],TR14[,1],R14[405:nrow(R14),1]),22, byrow = TRUE)
F14a<-matrix(c(R14[1:404,2],BZ14[,2],TR14[,2],R14[405:nrow(R14),2]),22, byrow = TRUE)

MF14 <- cbind(M14a, F14a)
MF14t <- t(MF14)
Pop14 <- array(MF14t, dim = c(101, 2, 22), dimnames = dimn1217)

# Check
for(i in 1:22){
  print(which(Pop14[,2,i]!=F14a[i,]))
}


#2015
R15<-as.matrix(read.csv("regioni15.csv", header= FALSE))
R15<-R15[3:nrow(R15),c(7,12)]
mode(R15)<-"numeric"

P15<-read.csv("province15.csv", header= FALSE)
BZ15<-as.matrix(P15[P15$V1=="021",c(8,13)])
TR15<-as.matrix(P15[P15$V1=="022",c(8,13)])
mode(BZ15)<-"numeric"
mode(TR15)<-"numeric"

M15a<-matrix(c(R15[1:404,1],BZ15[,1],TR15[,1],R15[405:nrow(R15),1]),22, byrow = TRUE)
F15a<-matrix(c(R15[1:404,2],BZ15[,2],TR15[,2],R15[405:nrow(R15),2]),22, byrow = TRUE)

MF15 <- cbind(M15a, F15a)
MF15t <- t(MF15)
Pop15 <- array(MF15t, dim = c(101, 2, 22), dimnames = dimn1217)

# Check
for(i in 1:22){
  print(which(Pop15[,2,i]!=F15a[i,]))
}

#2016
R16<-as.matrix(read.csv("regioni16.csv", header= FALSE))
R16<-R16[3:nrow(R16),c(7,12)]
mode(R16)<-"numeric"

P16<-read.csv("province16.csv", header= FALSE)
BZ16<-as.matrix(P16[P16$V1=="021",c(8,13)])
TR16<-as.matrix(P16[P16$V1=="022",c(8,13)])
mode(BZ16)<-"numeric"
mode(TR16)<-"numeric"

M16a<-matrix(c(R16[1:404,1],BZ16[,1],TR16[,1],R16[405:nrow(R16),1]),22, byrow = TRUE)
F16a<-matrix(c(R16[1:404,2],BZ16[,2],TR16[,2],R16[405:nrow(R16),2]),22, byrow = TRUE)

MF16 <- cbind(M16a, F16a)
MF16t <- t(MF16)
Pop16 <- array(MF16t, dim = c(101, 2, 22), dimnames = dimn1217)

# Check
for(i in 1:22){
  print(which(Pop16[,2,i]!=F16a[i,]))
}

#2017
R17<-as.matrix(read.csv("regioni17.csv", header= FALSE))
R17<-R17[3:nrow(R17),c(7,12)]
mode(R17)<-"numeric"

P17<-read.csv("province17.csv", header= FALSE)
BZ17<-as.matrix(P17[P17$V1=="021",c(8,13)])
TR17<-as.matrix(P17[P17$V1=="022",c(8,13)])
mode(BZ17)<-"numeric"
mode(TR17)<-"numeric"

M17a<-matrix(c(R17[1:404,1],BZ17[,1],TR17[,1],R17[405:nrow(R17),1]),22, byrow = TRUE)
F17a<-matrix(c(R17[1:404,2],BZ17[,2],TR17[,2],R17[405:nrow(R17),2]),22, byrow = TRUE)

MF17 <- cbind(M17a, F17a)
MF17t <- t(MF17)
Pop17 <- array(MF17t, dim = c(101, 2, 22), dimnames = dimn1217)

# Check
for(i in 1:22){
  print(which(Pop17[,1,i]!=M17a[i,]))
}


# Complete array single year age groups

# Dimensions names
dimn1<- list(age = c(0:99, "100+"),
             sex= c("Male","Female"),
             region= c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA",
                       "TRENTINO-ALTO ADIGE", "BOLZANO- BOZEN",
                       "TRENTO", "VENETO", "FRIULI-VENEZIA GIULIA",
                       "LIGURIA", "EMILIA - ROMAGNA", "TOSCANA",
                       "UMBRIA", "MARCHE", "LAZIO", "ABRUZZO",
                       "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA",
                       "CALABRIA", "SICILIA", "SARDEGNA"),
             year = c(2006:2017))

italy.popn.reg.1Y<- abind(P0611, P12,P13,P14,P15,P16,P17, along = 4)
save(italy.popn.reg.1Y, file= "italy.popn.reg.1Y.RData")


# Complete array 5year age groups

# Dimensions names
dimn5<- list(age = c("0-4", "5-9","10-14", "15-19", "20-24", "25-29",
                     "30-34", "35-39", "40-44", "45-49", "50-54",
                     "55-59","60-64","65-69", "70-74", "75-79",
                     "80-84", "85-89", "90-94", "95-99", "100+"),
             sex= c("Male","Female"),
             region= c("PIEMONTE", "VALLE D'AOSTA", "LOMBARDIA",
                       "TRENTINO-ALTO ADIGE", "BOLZANO- BOZEN",
                       "TRENTO", "VENETO", "FRIULI-VENEZIA GIULIA",
                       "LIGURIA", "EMILIA - ROMAGNA", "TOSCANA",
                       "UMBRIA", "MARCHE", "LAZIO", "ABRUZZO",
                       "MOLISE", "CAMPANIA", "PUGLIA", "BASILICATA",
                       "CALABRIA", "SICILIA", "SARDEGNA"),
             year = c(2006:2017) )

italy.popn5y.100<-array(NA, dim = c(21,2,22,12), dimnames = dimn5)

for(t in 1:12){
for(r in 1:22){
for(s in 1:2){
for(i in 1:20){
  italy.popn5y.100[i,s,r,t]<-sum(italy.popn.reg.1Y[(((i-1)*5+1):(5*i)),s,r,t])
}
  italy.popn5y.100[21,s,r,t] <- italy.popn.reg.1Y[101,s,r,t]
}
}
}

save(italy.popn5y.100, file= "italy.popn5y.100.RData")

#----------------------
# Migrations 2006-2014
#---------------------
library(xlsx)
library(abind)
library(tidyverse)
setwd("~\\demItaly\\data-raw\\Migrations")

#----------------------
# Immigration 2006-2016
#----------------------
imm <- read.csv("imm0616.csv")
summary(imm)
#----------------------------
# External immigration
#----------------------------

ext_imm <- select(imm, -c(Territorio.di.origine, Tipo.di.indicatore.demografico, Flags)) %>% # remove useless variables
  filter(Tipo.di.trasferimento == "estero") %>% # only over region movements
  filter(Territorio.di.di.destinazione %in% c("Piemonte", "Valle d'Aosta / Vallée d'Aoste",
                                              "Lombardia", "Trentino Alto Adige / Südtirol",
                                              "Provincia Autonoma Bolzano / Bozen",
                                              "Provincia Autonoma Trento", "Veneto",
                                              "Friuli-Venezia Giulia", "Liguria",
                                              "Emilia-Romagna", "Toscana", "Umbria",
                                              "Marche", "Lazio", "Abruzzo", "Molise",
                                              "Campania", "Puglia", "Basilicata",
                                              "Calabria", "Sicilia","Sardegna")) %>% # only regional data
  filter(Sesso != "totale") %>% # only male and female
  filter(Età != "totale") %>% # age group division
  filter(Paese.di.cittadinanza == "Totale") %>% # not considering citizenship
  select(-c(Tipo.di.trasferimento, Paese.di.cittadinanza)) %>%
  distinct() #remove duplicate

A <- length(unique(ext_imm$Età))
S <- length(unique(ext_imm$Sesso))
R <- length(unique(ext_imm$Territorio.di.di.destinazione))
Y <- length(unique(ext_imm$Seleziona.periodo))

dimn <- list(time = unique(ext_imm$Seleziona.periodo),
             region = unique(ext_imm$Territorio.di.di.destinazione),
             age = unique(ext_imm$Età),
             sex = unique(ext_imm$Sesso)
)

italy.ext.imm.perm <- array(ext_imm[,5], dim = c(Y, R, A, S), dimnames = dimn)
italy.ext.imm <- aperm(italy.ext.imm.perm, c(3, 4, 2, 1))
italy.ext.imm <- italy.ext.imm[c(1,3,4,2),c(2,1),c(1,2,4:9,3, 10:22),]
italy.ext.in <- italy.ext.imm

cbind(dimnames(italy.ext.in)[[1]],c("0-17", "18-39",	"40-64",	"65+"))
cbind(dimnames(italy.ext.in)[[2]],dimnames(italy.popn.reg)[[2]])
cbind(dimnames(italy.ext.in)[[3]],dimnames(italy.popn.reg)[[3]])
cbind(dimnames(italy.ext.in)[[4]],2006:2016)

dimnames(italy.ext.in) <- list(age = c("0-17", "18-39",	"40-64",	"65+"),
                               sex = dimnames(italy.popn.reg)[[2]],
                               region = dimnames(italy.popn.reg)[[3]],
                               time = 2006:2016)

sum(italy.ext.in[,,4,])==sum(italy.ext.in[,,5:6,])



#-------------------------
# Internal immigration
#-------------------------

int_imm <- select(imm, -c(Territorio.di.origine, Tipo.di.indicatore.demografico, Flags)) %>% # remove useless variables
  filter(Tipo.di.trasferimento == "in altre regioni") %>% # only over region movements
  filter(Territorio.di.di.destinazione %in% c("Piemonte", "Valle d'Aosta / Vallée d'Aoste",
                                              "Lombardia", "Trentino Alto Adige / Südtirol",
                                              "Provincia Autonoma Bolzano / Bozen",
                                              "Provincia Autonoma Trento", "Veneto",
                                              "Friuli-Venezia Giulia", "Liguria",
                                              "Emilia-Romagna", "Toscana", "Umbria",
                                              "Marche", "Lazio", "Abruzzo", "Molise",
                                              "Campania", "Puglia", "Basilicata",
                                              "Calabria", "Sicilia","Sardegna")) %>% # only regional data
  filter(Sesso != "totale") %>% # only male and female
  filter(Età != "totale") %>% # age group division
  filter(Paese.di.cittadinanza == "Totale") %>% # not considering citizenship
  select(-c(Tipo.di.trasferimento, Paese.di.cittadinanza)) %>%
  distinct() #remove duplicate

A <- length(unique(int_imm$Età))
S <- length(unique(int_imm$Sesso))
R <- length(unique(int_imm$Territorio.di.di.destinazione))
Y <- length(unique(int_imm$Seleziona.periodo))

dimn <- list(time = unique(int_imm$Seleziona.periodo),
             region = unique(int_imm$Territorio.di.di.destinazione),
             age = unique(int_imm$Età),
             sex = unique(int_imm$Sesso))


italy.intl.imm.perm <- array(int_imm[,5], dim = c(Y, R, A, S), dimnames = dimn)
italy.intl.imm <- aperm(italy.intl.imm.perm, c(3, 4, 2, 1))
italy.intl.imm <- italy.intl.imm[c(1,3,4,2),c(2,1),c(1,2,4:9,3, 10:22),]
italy.int.in <- italy.intl.imm

cbind(dimnames(italy.int.in)[[1]],c("0-17", "18-39",	"40-64",	"65+"))
cbind(dimnames(italy.int.in)[[2]],dimnames(italy.popn.reg)[[2]])
cbind(dimnames(italy.int.in)[[3]],dimnames(italy.popn.reg)[[3]])
cbind(dimnames(italy.int.in)[[4]],2006:2016)

dimnames(italy.int.in) <- list(age = c("0-17", "18-39",	"40-64",	"65+"),
                               sex = dimnames(italy.popn.reg)[[2]],
                               region = dimnames(italy.popn.reg)[[3]],
                               time = 2006:2016)

sum(italy.int.in[,,4,])==sum(italy.int.in[,,5:6,])
#---------------------
# External emigration
#---------------------

emiest <- read.xlsx("emi0614est.xlsx", sheetIndex = 1,
                    startRow = 5, endRow = 141, header = FALSE)

# Vector of region names as in the .xlxs file
RegOrig<- as.character(unique(emiest$X1)[c(2, 11, 13, 18,
                                           31, 32, 34, 36,
                                           44, 49, 60, 71,
                                           74, 79, 87, 92,
                                           95, 101, 107, 110,
                                           117, 127)])

ee1 <- emiest %>%
  filter(X1 %in% RegOrig) %>%
  column_to_rownames(var = "X1")

ee2 <- as.matrix(ee1)

# check region dimension names and order
cbind(RegOrig, dimnames(italy.popn.reg)[[3]])

# Liguria is row 3 in migr data and 9 in pop data!

dimn <- list(region = RegOrig,
             sex = c("Male", "Female"),
             age =c("0-17", "18-39",	"40-64",	"65+"),
             time = c(2006:2014))

R <- length(dimn$region)
S <- length(dimn$sex)
A <- length(dimn$age)
Y <- length(dimn$time)

ee3 <- aperm(array(ee2, dim = c(R, S, A, Y), dimnames = dimn), c(3,2,1,4))
ee3 <- ee3[, c(2,1),c(1,2,4:9,3,10:22), ]

# Check Trento and Bolzano
# sum(ee3[,,4,])==sum(ee3[,,5:6,])

# Adding emigration for 2015 and 2016

emi1516 <- read.csv("emi1516.csv")
summary(emi1516)
ee1516<- filter(emi1516, Tipo.di.trasferimento == "estero")%>%
  select(-c(Tipo.di.indicatore.demografico, Tipo.di.trasferimento))


Y56 <- length(unique(ee1516$Seleziona.periodo))
A56 <- length(unique(ee1516$Età))
S56 <-length(unique(ee1516$Sesso))
R56 <-length(unique(ee1516$Territorio.di.origine))

dimn1516 <- list(time = unique(ee1516$Seleziona.periodo),
                 age = unique(ee1516$Età),
                 sex = unique(ee1516$Sesso),
                 region = unique(ee1516$Territorio.di.origine))

ee1516arr1<- array(ee1516$X0, dim = c(Y56, A56, S56, R56),
                   dimnames = dimn1516)
ee1516arr2<- aperm(ee1516arr1, c(2,3,4,1))
dimnames(ee1516arr2) <- list(age = unique(ee1516$Età),
                             sex = unique(ee1516$Sesso),
                             region = unique(ee1516$Territorio.di.origine),
                             time = unique(ee1516$Seleziona.periodo))

# Check Trento and Bolzano
#  sum(ee1516arr2[,,5,])==sum(ee1516arr2[,,6:7,])

# Dimensions reordering age groups, sex and regions
ee1516arr2 <- ee1516arr2[c(1,3,4,2),c(2,1),c(1,2,4:9,3,10:22),]

# Unite 2006-2014 with 2015-2016
italy.ext.out <- abind(ee3, ee1516arr2, along = 4)
# cbind(c("0-17", "18-39",	"40-64",	"65+"), dimnames(ee1516arr2)[[1]])
# cbind(dimnames(italy.popn.reg)[[2]],dimnames(ee1516arr2)[[2]])
# cbind(dimnames(italy.popn.reg)[[3]], dimnames(ee1516arr2)[[3]])
dimnames(italy.ext.out) <- list(age = c("0-17", "18-39",	"40-64",	"65+"),
                                sex = dimnames(italy.popn.reg)[[2]],
                                region = dimnames(italy.popn.reg)[[3]],
                                time = 2006:2016)

sum(italy.ext.out[,,4,])==sum(italy.ext.out[,,5:6,])

#---------------------
# Internal emigration
#---------------------

emiint <- read.xlsx("emi0614int.xlsx", sheetIndex = 1,
                    startRow = 3, endRow = 139, header = FALSE)

ie1 <- emiint %>%
  filter(X1 %in% RegOrig) %>%
  column_to_rownames(var = "X1")
ie2 <- as.matrix(ie1)

ie3 <- aperm(array(ie2, dim = c(R, A, S, Y),
                   dimnames = list( region = RegOrig,
                                    age =c("0-17", "18-39",	"40-64",	"65+"),
                                    sex = c("Male", "Female"),
                                    time = 2006:2014)), c(2,3,1,4))
ie3 <- ie3[ , c(2,1), c(1,2,4:9,3,10:22), ]

# Check Trento and Bolzano
# sum(ie3[,,4,])==sum(ie3[,,5:6,])

# 2015 - 2016
ie1516 <- filter(emi1516, Tipo.di.trasferimento == "in altre regioni")%>%
  select(-c(Tipo.di.indicatore.demografico, Tipo.di.trasferimento))


ie1516arr1<- array(ie1516$X0, dim = c(Y56, A56, S56, R56),
                   dimnames = dimn1516)
ie1516arr2<- aperm(ie1516arr1, c(2,3,4,1))
dimnames(ie1516arr2) <- list(age = unique(ie1516$Età),
                             sex = unique(ie1516$Sesso),
                             region = unique(ie1516$Territorio.di.origine),
                             time = unique(ie1516$Seleziona.periodo))

# Dimensions reordering age groups, sex and regions
ie1516arr2 <- ie1516arr2[c(1,3,4,2),c(2,1),c(1,2,4:9,3,10:22),]

# Check Trento and Bolzano
#  sum(ie1516arr2[,,4,])==sum(ie1516arr2[,,5:6,])


# Unite 2006-2014 with 2015-2016
italy.int.out <- abind(ie3, ie1516arr2, along = 4)
# cbind(c("0-17", "18-39",	"40-64",	"65+"), dimnames(ie1516arr2)[[1]])
# cbind(dimnames(italy.popn.reg)[[2]],dimnames(ie1516arr2)[[2]])
# cbind(dimnames(italy.popn.reg)[[3]], dimnames(ie1516arr2)[[3]])
dimnames(italy.int.out) <- list(age = c("0-17", "18-39",	"40-64",	"65+"),
                                sex = dimnames(italy.popn.reg)[[2]],
                                region = dimnames(italy.popn.reg)[[3]],
                                time = 2006:2016)

sum(italy.int.out[,,4,])==sum(italy.int.out[,,5:6,])
#
#----------------------------------
# Consistent net migration
#----------------------------------

intarr<- Counts(italy.int.in , dimscales = c(time="Intervals")) %>%
  collapseDimension(margin = c("age", "sex", "time"))
intdep<- Counts(italy.int.out, dimscales = c(time="Intervals")) %>%
  collapseDimension(margin = c("age", "sex", "time"))
plot(intarr-intdep)

netmigr<- intarr - intdep
# If only poisitive values then it means the
# problem should be mainly in delays in cancellations
which(netmigr<0)

# So to have a consistent Net migration we add netmigr to intdep
# we spread the people proportionally to regional departures

props <- array(NA, dim = dim(italy.int.out))
for(i in 1:dim(italy.int.out)[3]){
  props[,,i,] <-  round(netmigr * italy.int.out[,,i,] /intdep)
}


# Add people and create consistent data
italy.int.out.cons <- italy.int.out + props
italy.int.in.cons <- italy.int.in

# Difference due to rounding
round_diff <- sum(props)-sum(netmigr)

# sample random coordinates to remove difference
coords<- sample(1:length(props), abs(round_diff))

italy.int.out.cons[coords] <- italy.int.out.cons[coords] - 1


# Checks
sum(italy.int.out.cons) - sum(italy.int.in.cons)

intarr2<- Counts(italy.int.in.cons , dimscales = c(time="Intervals")) %>%
  collapseDimension(margin = c("age", "sex", "time"))
intdep2<- Counts(italy.int.out.cons, dimscales = c(time="Intervals")) %>%
  collapseDimension(margin = c("age", "sex", "time"))

netmigr2 <- round(intarr2 - intdep2)
plot(netmigr2)

# Differences because of rounding put on the largest region
addarr <- ifelse(netmigr2>0, netmigr2, 0)
adddep <- ifelse(netmigr2<0, abs(netmigr2), 0)

regarr <- apply(italy.int.in.cons, 3, sum)
which(regarr==max(regarr))
italy.int.in.cons[,,3,] <-  italy.int.in.cons[,,3,] - addarr

regdep <- apply(italy.int.out.cons, 3, sum)
which(regdep==max(regdep))
italy.int.out.cons[,,3,] <-  italy.int.out.cons[,,3,] - adddep

# Checks
sum(italy.int.out.cons) - sum(italy.int.in.cons)

intarr3<- Counts(italy.int.in.cons , dimscales = c(time="Intervals")) %>%
  collapseDimension(margin = c("age", "sex", "time"))
intdep3<- Counts(italy.int.out.cons, dimscales = c(time="Intervals")) %>%
  collapseDimension(margin = c("age", "sex", "time"))

netmigr3 <- round(intarr3 - intdep3)
plot(netmigr3)

# Differences between consistent data and non consistent
sum(italy.int.out.cons-italy.int.out)
sum(netmigr)

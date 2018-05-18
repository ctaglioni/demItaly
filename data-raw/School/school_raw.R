#----------------------
# School data 2010-2014
#---------------------

library(tidyverse)

preprim_raw <- read.csv("preprim.csv", header = TRUE, sep=",")
prim1_raw <- read.csv("prim.csv", header = TRUE, sep=",")
lowsec_raw <- read.csv("lowersec.csv", header = TRUE, sep=",")
upsec_raw <- read.csv("uppersec.csv", header = TRUE, sep=",")

# Bolzano: years 2010 and 2014 are missing
# Trento: year 2010 is missing

############
# Preprim
############
summary(preprim_raw)
dimnames(preprim_raw)
dim(preprim_raw)
apply(preprim_raw[,1:6], 2, unique)


preprim <- select(preprim_raw, Tipo.dato, Territorio,
                  Seleziona.periodo, Gestione.della.scuola,X0) %>%
  filter(Tipo.dato %in% c("iscritti - maschi e femmine", "iscritti - femmine" )) %>%
  filter(Territorio %in% c("Piemonte", "Valle d'Aosta / Vallée d'Aoste",
                           "Liguria", "Lombardia",
                           "Trentino Alto Adige / Südtirol",
                           "Veneto", "Friuli-Venezia Giulia",
                           "Emilia-Romagna",
                           "Toscana", "Umbria",
                           "Marche", "Lazio",
                           "Abruzzo",
                           "Molise", "Campania",
                           "Puglia", "Basilicata",
                           "Calabria", "Sicilia", "Sardegna" )) %>%
  filter(Gestione.della.scuola == "totale") %>%
  select(-Gestione.della.scuola)


head(preprim)
summary(preprim)
dimnames(preprim)
dim(preprim)
apply(preprim, 2, unique)


pp.arr <- array(NA, dim = c(length(unique(preprim$Tipo.dato )),
                            length(unique(preprim$Territorio)),
                            length(unique(preprim$Seleziona.periodo))))
dimnames(pp.arr) <- list(sex = unique(preprim$Tipo.dato),
                         region = unique(preprim$Territorio),
                         time = unique(preprim$Seleziona.periodo))

for(a in 1:length(unique(preprim$Tipo.dato)) ){
  preprim1 <- preprim[preprim$Tipo.dato == dimnames(pp.arr)[[1]][a], c(2, 3, 4)]
  for(s in 1:length(unique(preprim$Territorio)) ){
    preprim2 <- preprim1[preprim1$Territorio==dimnames(pp.arr)[[2]][s], 2:3]
    for(r in 1:length(unique(preprim$Seleziona.periodo)) ){
      preprim3 <- preprim2[preprim2$Seleziona.periodo == dimnames(pp.arr)[[3]][r], 2]
      pp.arr[a, s, r] <- preprim3
        }
    }
}

pp.arr2 <- pp.arr
dim(pp.arr)
pp.male <- pp.arr[1,,]-pp.arr[2,,]
pp.arr[1,,] <- pp.male

#check
pp.arr[1,,]+pp.arr[2,,]==pp.arr2[1,,]

dimnames(pp.arr)[[1]] <- c("Male", "Female")
pp.arr <- aperm(pp.arr, c(2,1,3))

##########
# Primary
##########
summary(prim1_raw)
dimnames(prim1_raw)
dim(prim1_raw)
apply(prim1_raw[,1:6], 2, unique)


prim <- select(prim1_raw, Tipo.dato, Territorio,
                  Seleziona.periodo, Gestione.della.scuola,X0) %>%
  filter(Tipo.dato %in% c("iscritti - maschi e femmine", "iscritti - femmine" )) %>%
  filter(Territorio %in% c("Piemonte", "Valle d'Aosta / Vallée d'Aoste",
                           "Liguria", "Lombardia",
                           "Trentino Alto Adige / Südtirol",
                           "Veneto", "Friuli-Venezia Giulia",
                           "Emilia-Romagna",
                           "Toscana", "Umbria",
                           "Marche", "Lazio",
                           "Abruzzo",
                           "Molise", "Campania",
                           "Puglia", "Basilicata",
                           "Calabria", "Sicilia", "Sardegna" )) %>%
  filter(Gestione.della.scuola == "totale") %>%
  select(-Gestione.della.scuola)


head(prim)
summary(prim)
dimnames(prim)
dim(prim)
apply(prim, 2, unique)


prim.arr <- array(NA, dim = c(length(unique(prim$Tipo.dato )),
                            length(unique(prim$Territorio)),
                            length(unique(prim$Seleziona.periodo))))
dimnames(prim.arr) <- list(sex = unique(prim$Tipo.dato),
                         region = unique(prim$Territorio),
                         time = unique(prim$Seleziona.periodo))

for(a in 1:length(unique(prim$Tipo.dato)) ){
  prim1 <- prim[prim$Tipo.dato == dimnames(prim.arr)[[1]][a], c(2, 3, 4)]
  for(s in 1:length(unique(prim$Territorio)) ){
    prim2 <- prim1[prim1$Territorio==dimnames(prim.arr)[[2]][s], 2:3]
    for(r in 1:length(unique(prim$Seleziona.periodo)) ){
      prim3 <- prim2[prim2$Seleziona.periodo == dimnames(prim.arr)[[3]][r], 2]
      prim.arr[a, s, r] <- prim3
    }
  }
}

prim.arr2 <- prim.arr
dim(prim.arr)
prim.male <- prim.arr[1,,]-prim.arr[2,,]
prim.arr[1,,] <- prim.male

#check
prim.arr[1,,] + prim.arr[2,,] == prim.arr2[1,,]

dimnames(prim.arr)[[1]] <- c("Male", "Female")
prim.arr <- aperm(prim.arr, c(2,1,3))

########################
#  low secondary school
########################
summary(lowsec_raw)
dimnames(lowsec_raw)
dim(lowsec_raw)
apply(lowsec_raw[,1:6], 2, unique)


lowsec <- select(lowsec_raw, Tipo.dato, Territorio,
               Seleziona.periodo, X0) %>%
  filter(Tipo.dato %in% c("iscritti - maschi e femmine", "iscritti - femmine" )) %>%
  filter(Territorio %in% c("Piemonte", "Valle d'Aosta / Vallée d'Aoste",
                           "Liguria", "Lombardia",
                           "Trentino Alto Adige / Südtirol",
                           "Veneto", "Friuli-Venezia Giulia",
                           "Emilia-Romagna",
                           "Toscana", "Umbria",
                           "Marche", "Lazio",
                           "Abruzzo",
                           "Molise", "Campania",
                           "Puglia", "Basilicata",
                           "Calabria", "Sicilia", "Sardegna" ))


head(lowsec)
summary(lowsec)
dimnames(lowsec)
dim(lowsec)
apply(lowsec, 2, unique)


lowsec.arr <- array(NA, dim = c(length(unique(lowsec$Tipo.dato )),
                              length(unique(lowsec$Territorio)),
                              length(unique(lowsec$Seleziona.periodo))))
dimnames(lowsec.arr) <- list(sex = unique(lowsec$Tipo.dato),
                           region = unique(lowsec$Territorio),
                           time = unique(lowsec$Seleziona.periodo))

for(a in 1:length(unique(lowsec$Tipo.dato)) ){
  lowsec1 <- lowsec[lowsec$Tipo.dato == dimnames(lowsec.arr)[[1]][a], c(2, 3, 4)]
  for(s in 1:length(unique(lowsec$Territorio)) ){
    lowsec2 <- lowsec1[lowsec1$Territorio==dimnames(lowsec.arr)[[2]][s], 2:3]
    for(r in 1:length(unique(lowsec$Seleziona.periodo)) ){
      lowsec3 <- lowsec2[lowsec2$Seleziona.periodo == dimnames(lowsec.arr)[[3]][r], 2]
      lowsec.arr[a, s, r] <- lowsec3
    }
  }
}

lowsec.arr2 <- lowsec.arr
dim(lowsec.arr)
lowsec.male <- lowsec.arr[1,,]-lowsec.arr[2,,]
lowsec.arr[1,,] <- lowsec.male

#check
lowsec.arr[1,,] + lowsec.arr[2,,] == lowsec.arr2[1,,]

dimnames(lowsec.arr)[[1]] <- c("Male", "Female")
lowsec.arr <- aperm(lowsec.arr, c(2,1,3))

########################
#  upper secondary school
########################
summary(upsec_raw)
dimnames(upsec_raw)
dim(upsec_raw)
apply(upsec_raw[,1:6], 2, unique)


upsec <- select(upsec_raw, Tipo.dato, Tipo.di.scuola.superiore, Territorio,
                 Seleziona.periodo, X0) %>%
  filter(Tipo.dato %in% c("iscritti - maschi e femmine", "iscritti - femmine" )) %>%
  filter(Territorio %in% c("Piemonte", "Valle d'Aosta / Vallée d'Aoste",
                           "Liguria", "Lombardia",
                           "Trentino Alto Adige / Südtirol",
                           "Veneto", "Friuli-Venezia Giulia",
                           "Emilia-Romagna",
                           "Toscana", "Umbria",
                           "Marche", "Lazio",
                           "Abruzzo",
                           "Molise", "Campania",
                           "Puglia", "Basilicata",
                           "Calabria", "Sicilia", "Sardegna" )) %>%
  filter(Tipo.di.scuola.superiore == "totale") %>%
  select(-Tipo.di.scuola.superiore)


head(upsec)
summary(upsec)
dimnames(upsec)
dim(upsec)
apply(upsec, 2, unique)


upsec.arr <- array(NA, dim = c(length(unique(upsec$Tipo.dato )),
                                length(unique(upsec$Territorio)),
                                length(unique(upsec$Seleziona.periodo))))
dimnames(upsec.arr) <- list(sex = unique(upsec$Tipo.dato),
                             region = unique(upsec$Territorio),
                             time = unique(upsec$Seleziona.periodo))

for(a in 1:length(unique(upsec$Tipo.dato)) ){
  upsec1 <- upsec[upsec$Tipo.dato == dimnames(upsec.arr)[[1]][a], c(2, 3, 4)]
  for(s in 1:length(unique(upsec$Territorio)) ){
    upsec2 <- upsec1[upsec1$Territorio==dimnames(upsec.arr)[[2]][s], 2:3]
    for(r in 1:length(unique(upsec$Seleziona.periodo)) ){
      upsec3 <- upsec2[upsec2$Seleziona.periodo == dimnames(upsec.arr)[[3]][r], 2]
      upsec.arr[a, s, r] <- upsec3
    }
  }
}

upsec.arr2 <- upsec.arr
dim(upsec.arr)
upsec.male <- upsec.arr[1,,]-upsec.arr[2,,]
upsec.arr[1,,] <- upsec.male

#check
upsec.arr[1,,] + upsec.arr[2,,] == upsec.arr2[1,,]

dimnames(upsec.arr)[[1]] <- c("Male", "Female")
upsec.arr <- aperm(upsec.arr, c(2,1,3))
dimnames(upsec.arr)$region

################################

italy.school.preprimary <- pp.arr[c(1,2,4,5,6,7,3,8:20), c(2,1),]
dimnames(italy.school.preprimary) <- list(region = dimnames(pp.arr)[[1]][c(1,2,4,5,6,7,3,8:20)],
                              sex = dimnames(pp.arr)[[2]][c(2,1)],
                              time = dimnames(pp.arr)[[3]])


italy.school.primary <- prim.arr[c(1,2,4,5,6,7,3,8:20), c(2,1),]
dimnames(italy.school.primary) <- list(region = dimnames(prim.arr)[[1]][c(1,2,4,5,6,7,3,8:20)],
                              sex = dimnames(prim.arr)[[2]][c(2,1)],
                              time = dimnames(prim.arr)[[3]])


italy.school.lowsec <- lowsec.arr[c(1,2,4,5,6,7,3,8:20), c(2,1),]
dimnames(italy.school.lowsec) <- list(region = dimnames(lowsec.arr)[[1]][c(1,2,4,5,6,7,3,8:20)],
                              sex = dimnames(lowsec.arr)[[2]][c(2,1)],
                              time = dimnames(lowsec.arr)[[3]])

italy.school.upsec <- upsec.arr[c(1,2,4,5,6,7,3,8:20), c(2,1),]
dimnames(italy.school.upsec) <- list(region = dimnames(upsec.arr)[[1]][c(1,2,4,5,6,7,3,8:20)],
                              sex = dimnames(upsec.arr)[[2]][c(2,1)],
                              time = dimnames(upsec.arr)[[3]])

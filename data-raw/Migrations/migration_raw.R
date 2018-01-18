### A.A.A. Problem with new data!

#----------------------
# Migration 2006-2016
#----------------------

setwd("C:\\Users\\Cha\\Documents\\demItaly\\data-raw\\Migrations")
library(tidyverse)

imm <- read.csv("imm0616.csv")
emi <- read.csv("emi0616.csv")

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

summary(int_imm)
head(int_imm)

A <- length(unique(int_imm$Età))
S <- length(unique(int_imm$Sesso))
R <- length(unique(int_imm$Territorio.di.di.destinazione))
Y <- length(unique(int_imm$Seleziona.periodo))

dimn <- list(time = unique(int_imm$Seleziona.periodo),
             region = unique(int_imm$Territorio.di.di.destinazione),
             age = unique(int_imm$Età),
             sex = unique(int_imm$Sesso)
        )


italy.intl.imm.perm <- array(int_imm[,5], dim = c(Y, R, A, S), dimnames = dimn)
italy.intl.imm <- aperm(italy.intl.imm.perm, c(3, 4, 2, 1))

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

summary(ext_imm)
head(ext_imm)

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


library(devtools)
setwd("C:\\Users\\Cha\\Documents\\demItaly\\data")
devtools::use_data(italy.ext.imm, overwrite=T)

#----------------------
# Internal emigration
#----------------------

summary(emi)
head(emi)
int_emi <- select(emi, -c(Tipo.di.indicatore.demografico, Flags)) %>% # remove useless variables
  filter(Tipo.di.trasferimento == "in altre regioni") %>% # only over region movements
  filter(Territorio.di.origine %in% c("Piemonte", "Valle d'Aosta / Vallée d'Aoste",
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
  filter(Paese.di.cittadinanza == "totale") %>% # not considering citizenship
  select(-c(Tipo.di.trasferimento, Paese.di.cittadinanza)) %>%
  distinct() #remove duplicate

summary(int_emi)
head(int_emi)

A <- length(unique(int_emi$Età))
S <- length(unique(int_emi$Sesso))
R <- length(unique(int_emi$Territorio.di.di.destinazione))
Y <- length(unique(int_emi$Seleziona.periodo))

dimn <- list(time = unique(int_emi$Seleziona.periodo),
             region = unique(int_emi$Territorio.di.di.destinazione),
             age = unique(int_emi$Età),
             sex = unique(int_emi$Sesso)
)


italy.intl.emi.perm <- array(int_emi[,5], dim = c(Y, R, A, S), dimnames = dimn)
italy.intl.emi <- aperm(italy.intl.emi.perm, c(3, 4, 2, 1))

#----------------------------
# External emigration
#----------------------------

ext_emi <- select(emi, -c(Territorio.di.origine, Tipo.di.indicatore.demografico, Flags)) %>% # remove useless variables
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

summary(ext_emi)
head(ext_emi)

A <- length(unique(ext_emi$Età))
S <- length(unique(ext_emi$Sesso))
R <- length(unique(ext_emi$Territorio.di.di.destinazione))
Y <- length(unique(ext_emi$Seleziona.periodo))

dimn <- list(time = unique(ext_emi$Seleziona.periodo),
             region = unique(ext_emi$Territorio.di.di.destinazione),
             age = unique(ext_emi$Età),
             sex = unique(ext_emi$Sesso)
)


italy.ext.emi.perm <- array(ext_emi[,5], dim = c(Y, R, A, S), dimnames = dimn)
italy.ext.emi <- aperm(italy.ext.emi.perm, c(3, 4, 2, 1))



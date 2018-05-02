library(demest)
library(tidyverse)
library(abind)

r16<- read.csv("regioni_bd2016.csv", header=TRUE) %>%
  column_to_rownames("Regione")

p16<- read.csv("province_bd2016.csv", header=TRUE) %>%
  filter(Provincia %in% c("Bolzano/Bozen", "Trento"))

n16 <- select(r16, Popolazione.al.1Â..gennaio...Maschi, Popolazione.al.1Â..gennaio...Femmine)
n16p <- select(p16, Popolazione.al.1Â..gennaio...Maschi, Popolazione.al.1Â..gennaio...Femmine)

b16 <- select(r16, Nati...Maschi, Nati...Femmine)
b16p <- select(p16, Nati...Maschi, Nati...Femmine)

d16 <- select(r16, Morti...Maschi, Morti...Femmine)
d16p <- select(p16, Morti...Maschi, Morti...Femmine)

ei16 <- select(r16,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)
ei16p <- select(p16,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)

eo16 <- select(r16,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)
eo16p <- select(p16,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)


# 2015
r15<- read.csv("regioni_bd2015.csv", header=TRUE) %>%
  column_to_rownames("Regione")

p15<- read.csv("province_bd2015.csv", header=TRUE) %>%
  filter(Provincia %in% c("Bolzano/Bozen", "Trento"))

n15 <- select(r15, Popolazione.al.1Â..gennaio...Maschi, Popolazione.al.1Â..gennaio...Femmine)
n15p <- select(p15, Popolazione.al.1Â..gennaio...Maschi, Popolazione.al.1Â..gennaio...Femmine)

b15 <- select(r15, Nati...Maschi, Nati...Femmine)
b15p <- select(p15, Nati...Maschi, Nati...Femmine)

d15 <- select(r15, Morti...Maschi, Morti...Femmine)
d15p <- select(p15, Morti...Maschi, Morti...Femmine)

ei15 <- select(r15,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)
ei15p <- select(p15,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)

eo15 <- select(r15,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)
eo15p <- select(p15,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)

# 2014
r14<- read.csv("regioni_bd2014.csv", header=TRUE) %>%
  column_to_rownames("Regione")

p14<- read.csv("province_bd2014.csv", header=TRUE) %>%
  filter(Provincia %in% c("Bolzano/Bozen", "Trento"))

n14 <- select(r14, Popolazione.al.1Â..gennaio...Maschi, Popolazione.al.1Â..gennaio...Femmine)
n14p <- select(p14, Popolazione.al.1Â..gennaio...Maschi, Popolazione.al.1Â..gennaio...Femmine)

b14 <- select(r14, Nati...Maschi, Nati...Femmine)
b14p <- select(p14, Nati...Maschi, Nati...Femmine)

d14 <- select(r14, Morti...Maschi, Morti...Femmine)
d14p <- select(p14, Morti...Maschi, Morti...Femmine)

ei14 <- select(r14,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)
ei14p <- select(p14,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)

eo14 <- select(r14,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)
eo14p <- select(p14,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)

# 2013
r13<- read.csv("regioni_bd2013.csv", header=TRUE) %>%
  column_to_rownames("Regione")

p13<- read.csv("province_bd2013.csv", header=TRUE) %>%
  filter(Provincia %in% c("Bolzano/Bozen", "Trento"))

n13 <- select(r13, Popolazione.al.1Â..gennaio...Maschi, Popolazione.al.1Â..gennaio...Femmine)
n13p <- select(p13, Popolazione.al.1Â..gennaio...Maschi, Popolazione.al.1Â..gennaio...Femmine)

b13 <- select(r13, Nati...Maschi, Nati...Femmine)
b13p <- select(p13, Nati...Maschi, Nati...Femmine)

d13 <- select(r13, Morti...Maschi, Morti...Femmine)
d13p <- select(p13, Morti...Maschi, Morti...Femmine)

ei13 <- select(r13,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)
ei13p <- select(p13,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)

eo13 <- select(r13,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)
eo13p <- select(p13,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)

# 2012
r12<- read.csv("regioni_bd2012.csv", header=TRUE) %>%
  column_to_rownames("Regione")

p12<- read.csv("province_bd2012.csv", header=TRUE) %>%
  filter(Provincia %in% c("Bolzano/Bozen", "Trento"))

n12 <- select(r12, Popolazione.al.1Â..gennaio...Maschi, Popolazione.al.1Â..gennaio...Femmine)
n12p <- select(p12, Popolazione.al.1Â..gennaio...Maschi, Popolazione.al.1Â..gennaio...Femmine)

b12 <- select(r12, Nati...Maschi, Nati...Femmine)
b12p <- select(p12, Nati...Maschi, Nati...Femmine)

d12 <- select(r12, Morti...Maschi, Morti...Femmine)
d12p <- select(p12, Morti...Maschi, Morti...Femmine)

ei12 <- select(r12,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)
ei12p <- select(p12,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)

eo12 <- select(r12,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)
eo12p <- select(p12,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)

# 2011 post census

r11post<- read.csv("regioni_bd_cens.csv", header=TRUE) %>%
  column_to_rownames("Regione")

p11post<- read.csv("province_bd_cens.csv", header=TRUE) %>%
  filter(Provincia %in% c("Bolzano/Bozen", "Trento"))

b11post <- select(r11post, Nati...Maschi, Nati...Femmine)
b11ppost <- select(p11post, Nati...Maschi, Nati...Femmine)

d11post <- select(r11post, Morti...Maschi, Morti...Femmine)
d11ppost <- select(p11post, Morti...Maschi, Morti...Femmine)

ei11post <- select(r11post, Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)
ei11ppost <- select(p11post, Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)

eo11post <- select(r11post, Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)
eo11ppost <- select(p11post, Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)


# 2011 pre-census
r11pre<- read.csv("regioni_bd2011.csv", header=TRUE) %>%
  column_to_rownames("Regione")

p11pre<- read.csv("province_bd2011.csv", header=TRUE) %>%
  filter(Provincia %in% c("Bolzano/Bozen", "Trento"))

n11 <- select(r11pre, Popolazione.al.1Â..Gennaio...Maschi, Popolazione.al.1Â..Gennaio...Femmine)
n11p <- select(p11pre, Popolazione.al.1Â..Gennaio...Maschi, Popolazione.al.1Â..Gennaio...Femmine)

b11pre <- select(r11pre, Nati...Maschi, Nati...Femmine)
b11ppre <- select(p11pre, Nati...Maschi, Nati...Femmine)

d11pre <- select(r11pre, Morti...Maschi, Morti...Femmine)
d11ppre <- select(p11pre, Morti...Maschi, Morti...Femmine)

ei11pre <- select(r11pre, Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)
ei11ppre <- select(p11pre, Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)

eo11pre <- select(r11pre, Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)
eo11ppre <- select(p11pre, Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)

# Total births, deaths migration 2011

b11 <- b11pre + b11post
b11p <- b11ppre + b11ppost

d11 <- d11pre + d11post
d11p <- d11ppre + d11ppost

ei11 <- ei11pre + ei11post
ei11p <- ei11ppre + ei11ppost

eo11 <- eo11pre + eo11post
eo11p <- eo11ppre + eo11ppost

# 2010

r10<- read.csv("regioni_bd2010.csv", header=TRUE) %>%
  column_to_rownames("Regione")

p10 <- read.csv("province_bd2010.csv", header=TRUE) %>%
  filter(Provincia %in% c("Bolzano/Bozen", "Trento"))

n10 <- select(r10, Popolazione.al.1Â..Gennaio...Maschi, Popolazione.al.1Â..Gennaio...Femmine)
n10p <- select(p10, Popolazione.al.1Â..Gennaio...Maschi, Popolazione.al.1Â..Gennaio...Femmine)

b10 <- select(r10, Nati...Maschi, Nati...Femmine)
b10p <- select(p10, Nati...Maschi, Nati...Femmine)

d10 <- select(r10, Morti...Maschi, Morti...Femmine)
d10p <- select(p10, Morti...Maschi, Morti...Femmine)

ei10 <- select(r10,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)
ei10p <- select(p10,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)

eo10 <- select(r10,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)
eo10p <- select(p10,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)

# 2009

r09<- read.csv("regioni_bd2009.csv", header=TRUE) %>%
  column_to_rownames("Regione")

p09<- read.csv("province_bd2009.csv", header=TRUE) %>%
  filter(Codice %in% c("21", "22"))

n09 <- select(r09, Popolazione.al.1Â..Gennaio...Maschi, Popolazione.al.1Â..Gennaio...Femmine)
n09p <- select(p09, Popolazione.al.1Â..Gennaio...Maschi, Popolazione.al.1Â..Gennaio...Femmine)

b09 <- select(r09, Nati...Maschi, Nati...Femmine)
b09p <- select(p09, Nati...Maschi, Nati...Femmine)

d09 <- select(r09, Morti...Maschi, Morti...Femmine)
d09p <- select(p09, Morti...Maschi, Morti...Femmine)

ei09 <- select(r09,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)
ei09p <- select(p09,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)

eo09 <- select(r09,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)
eo09p <- select(p09,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)

# 2008
r08<- read.csv("regioni_bd2008.csv", header=TRUE) %>%
  column_to_rownames("Regione")

p08 <- read.csv("province_bd2008.csv", header=TRUE) %>%
  filter(Codice %in% c("21", "22"))

n08 <- select(r08, Popolazione.al.1Â..Gennaio...Maschi, Popolazione.al.1Â..Gennaio...Femmine)
n08p <- select(p08, Popolazione.al.1Â..Gennaio...Maschi, Popolazione.al.1Â..Gennaio...Femmine)

b08 <- select(r08, Nati...Maschi, Nati...Femmine)
b08p <- select(p08, Nati...Maschi, Nati...Femmine)

d08 <- select(r08, Morti...Maschi, Morti...Femmine)
d08p <- select(p08, Morti...Maschi, Morti...Femmine)

ei08 <- select(r08,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)
ei08p <- select(p08,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)

eo08 <- select(r08,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)
eo08p <- select(p08,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)

# 2007

r07<- read.csv("regioni_bd2007.csv", header=TRUE) %>%
  column_to_rownames("Regione")

p07 <- read.csv("province_bd2007.csv", header=TRUE) %>%
  filter(Codice %in% c("21", "22"))

n07 <- select(r07, Popolazione.al.1..Gennaio...Maschi, Popolazione.al.1..Gennaio...Femmine)
n07p <- select(p07, Popolazione.al.1..Gennaio...Maschi, Popolazione.al.1..Gennaio...Femmine)

b07 <- select(r07, Nati...Maschi, Nati...Femmine)
b07p <- select(p07, Nati...Maschi, Nati...Femmine)

d07 <- select(r07, Morti...Maschi, Morti...Femmine)
d07p <- select(p07, Morti...Maschi, Morti...Femmine)

ei07 <- select(r07,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)
ei07p <- select(p07,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)

eo07 <- select(r07,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)
eo07p <- select(p07,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)


# 2006

r06<- read.csv("regioni_bd2006.csv", header=TRUE) %>%
  column_to_rownames("Regione")

p06 <- read.csv("province_bd2006.csv", header=TRUE) %>%
  filter(Codice %in% c("21", "22"))

n06 <- select(r06, Popolazione.al.1..Gennaio...Maschi, Popolazione.al.1..Gennaio...Femmine)
n06p <- select(p06, Popolazione.al.1..Gennaio...Maschi, Popolazione.al.1..Gennaio...Femmine)

b06 <- select(r06, Nati...Maschi, Nati...Femmine)
b06p <- select(p06, Nati...Maschi, Nati...Femmine)

d06 <- select(r06, Morti...Maschi, Morti...Femmine)
d06p <- select(p06, Morti...Maschi, Morti...Femmine)

ei06 <- select(r06,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)
ei06p <- select(p06,Iscritti.dall.estero...Maschi, Iscritti.dall.estero...Femmine)

eo06 <- select(r06,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)
eo06p <- select(p06,Cancellati.per.l.estero...Maschi, Cancellati.per.l.estero...Femmine)

# dimnames

Regions <- c("Piemonte","Valle D'Aosta","Lombardia",
             "Trentino Alto Adige", "Bolzano/Bozen","Trento",
             "Veneto","Friuli Venezia Giulia", "Liguria",
             "Emilia Romagna","Toscana","Umbria",
             "Marche","Lazio","Abruzzo",
             "Molise","Campania","Puglia",
             "Basilicata","Calabria","Sicilia",
             "Sardegna")
Sex <- c("Female", "Male") # Sexes list
Years <- 2006:2016 # Years list

italy.popn.bd <- array(c(rbind(data.matrix(n06),data.matrix(n06p)),
                         rbind(data.matrix(n07),data.matrix(n07p)),
                         rbind(data.matrix(n08),data.matrix(n08p)),
                         rbind(data.matrix(n09),data.matrix(n09p)),
                         rbind(data.matrix(n10),data.matrix(n10p)),
                         rbind(data.matrix(n11),data.matrix(n11p)),
                         rbind(data.matrix(n12),data.matrix(n12p)),
                         rbind(data.matrix(n13),data.matrix(n13p)),
                         rbind(data.matrix(n14),data.matrix(n14p)),
                         rbind(data.matrix(n15),data.matrix(n15p)),
                         rbind(data.matrix(n16),data.matrix(n16p)))
                         , dim = c(22, 2, 11))

italy.popn.bd <- italy.popn.bd[c(1,2,4,5,21,22,6,7,3,8,10,11,9,12,14,15,13,16,17,18,19,20),
                               c(2,1),]
dimnames(italy.popn.bd) <- list(region = Regions,
                                 sex = Sex,
                                 time = Years)

# Births

italy.births.bd <- array(c(rbind(data.matrix(b06),data.matrix(b06p)),
                         rbind(data.matrix(b07),data.matrix(b07p)),
                         rbind(data.matrix(b08),data.matrix(b08p)),
                         rbind(data.matrix(b09),data.matrix(b09p)),
                         rbind(data.matrix(b10),data.matrix(b10p)),
                         rbind(data.matrix(b11),data.matrix(b11p)),
                         rbind(data.matrix(b12),data.matrix(b12p)),
                         rbind(data.matrix(b13),data.matrix(b13p)),
                         rbind(data.matrix(b14),data.matrix(b14p)),
                         rbind(data.matrix(b15),data.matrix(b15p)),
                         rbind(data.matrix(b16),data.matrix(b16p)))
                       , dim = c(22, 2, 11))

italy.births.bd <- italy.births.bd[c(1,2,4,5,21,22,6,7,3,8,10,11,9,12,14,15,13,16,17,18,19,20),
                                   c(2,1),]
dimnames(italy.births.bd) <- list(region = Regions,
                                sex = Sex,
                                time = Years)



italy.deaths.bd <- array(c(rbind(data.matrix(d06),data.matrix(d06p)),
                           rbind(data.matrix(d07),data.matrix(d07p)),
                           rbind(data.matrix(d08),data.matrix(d08p)),
                           rbind(data.matrix(d09),data.matrix(d09p)),
                           rbind(data.matrix(d10),data.matrix(d10p)),
                           rbind(data.matrix(d11),data.matrix(d11p)),
                           rbind(data.matrix(d12),data.matrix(d12p)),
                           rbind(data.matrix(d13),data.matrix(d13p)),
                           rbind(data.matrix(d14),data.matrix(d14p)),
                           rbind(data.matrix(d15),data.matrix(d15p)),
                           rbind(data.matrix(d16),data.matrix(d16p)))
                         , dim = c(22, 2, 11))
italy.deaths.bd <- italy.deaths.bd[c(1,2,4,5,21,22,6,7,3,8,10,11,9,12,14,15,13,16,17,18,19,20),
                                   c(2,1),]
dimnames(italy.deaths.bd) <- list(region = Regions,
                                  sex = Sex,
                                  time = Years)

italy.ext.in.bd <- array(c(rbind(data.matrix(ei06),data.matrix(ei06p)),
                           rbind(data.matrix(ei07),data.matrix(ei07p)),
                           rbind(data.matrix(ei08),data.matrix(ei08p)),
                           rbind(data.matrix(ei09),data.matrix(ei09p)),
                           rbind(data.matrix(ei10),data.matrix(ei10p)),
                           rbind(data.matrix(ei11),data.matrix(ei11p)),
                           rbind(data.matrix(ei12),data.matrix(ei12p)),
                           rbind(data.matrix(ei13),data.matrix(ei13p)),
                           rbind(data.matrix(ei14),data.matrix(ei14p)),
                           rbind(data.matrix(ei15),data.matrix(ei15p)),
                           rbind(data.matrix(ei16),data.matrix(ei16p)))
                         , dim = c(22, 2, 11))

italy.ext.in.bd <- italy.ext.in.bd[c(1,2,4,5,21,22,6,7,3,8,10,11,9,12,14,15,13,16,17,18,19,20),
                                   c(2,1),]
dimnames(italy.ext.in.bd) <- list(region = Regions,
                                  sex = Sex,
                                  time = Years)

italy.ext.out.bd <- array(c(rbind(data.matrix(eo06),data.matrix(eo06p)),
                            rbind(data.matrix(eo07),data.matrix(eo07p)),
                            rbind(data.matrix(eo08),data.matrix(eo08p)),
                            rbind(data.matrix(eo09),data.matrix(eo09p)),
                            rbind(data.matrix(eo10),data.matrix(eo10p)),
                            rbind(data.matrix(eo11),data.matrix(eo11p)),
                            rbind(data.matrix(eo12),data.matrix(eo12p)),
                            rbind(data.matrix(eo13),data.matrix(eo13p)),
                            rbind(data.matrix(eo14),data.matrix(eo14p)),
                            rbind(data.matrix(eo15),data.matrix(eo15p)),
                            rbind(data.matrix(eo16),data.matrix(eo16p)))
                          , dim = c(22, 2, 11))
italy.ext.out.bd <- italy.ext.out.bd[c(1,2,4,5,21,22,6,7,3,8,10,11,9,12,14,15,13,16,17,18,19,20),
                                     c(2,1),]
dimnames(italy.ext.out.bd) <- list(region = Regions,
                                  sex = Sex,
                                  time = Years)


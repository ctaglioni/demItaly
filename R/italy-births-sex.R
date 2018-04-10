library(demest)
library(tidyverse)
library(abind)
library(readxl)

# 2016
b16.raw <- read_excel("BpS16.xls",sheet=1)
b16a<- filter(b16.raw, `Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2016` %in% c("Piemonte","Valle d'Aosta-Vallèe d'Aoste","Lombardia",
                      "Bolzano-Bozen", "Trento", "Trentino-A. Adige/Südtirol",
                      "Veneto","Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna",
                      "Toscana", "Umbria")) %>%
  select(c(`Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2016`, X__1, X__2))
colnames(b16a) <- c("Region", "Male", "Female")

b16b <- filter(b16.raw, X__7 %in% c("Marche", "Lazio", "Abruzzo", "Molise",
                     "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia",
                     "Sardegna")) %>%
  select(c(X__7, X__8, X__9))
colnames(b16b) <- c("Region", "Male", "Female")
b16 <- rbind(data.matrix(column_to_rownames(as.data.frame(b16a), "Region")),
             data.matrix(column_to_rownames(as.data.frame(b16b), "Region")))

# 2015
b15.raw <- read_excel("BpS15.xls",sheet=1)
b15a<- filter(b15.raw, `Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2015` %in% c("Piemonte","Valle d'Aosta-Vallèe d'Aoste","Lombardia",
                                                                                                    "Bolzano-Bozen", "Trento", "Trentino-A. Adige/Südtirol",
                                                                                                    "Veneto","Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna",
                                                                                                    "Toscana", "Umbria")) %>%
  select(c(`Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2015`, X__1, X__2))
colnames(b15a) <- c("Region", "Male", "Female")

b15b <- filter(b15.raw, X__7 %in% c("Marche", "Lazio", "Abruzzo", "Molise",
                                    "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia",
                                    "Sardegna")) %>%
  select(c(X__7, X__8, X__9))
colnames(b15b) <- c("Region", "Male", "Female")
b15 <- rbind(data.matrix(column_to_rownames(as.data.frame(b15a), "Region")),
             data.matrix(column_to_rownames(as.data.frame(b15b), "Region")))

# 2014
b14.raw <- read_excel("BpS14.xls",sheet=1)
b14a<- filter(b14.raw, `Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2014` %in% c("Piemonte","Valle d'Aosta-Vallèe d'Aoste","Lombardia",
                                                                                                    "Bolzano-Bozen", "Trento", "Trentino-A. Adige/Südtirol",
                                                                                                    "Veneto","Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna",
                                                                                                    "Toscana", "Umbria")) %>%
  select(c(`Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2014`, X__1, X__2))
colnames(b14a) <- c("Region", "Male", "Female")

b14b <- filter(b14.raw, X__7 %in% c("Marche", "Lazio", "Abruzzo", "Molise",
                                    "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia",
                                    "Sardegna")) %>%
  select(c(X__7, X__8, X__9))
colnames(b14b) <- c("Region", "Male", "Female")
b14 <- rbind(data.matrix(column_to_rownames(as.data.frame(b14a), "Region")),
             data.matrix(column_to_rownames(as.data.frame(b14b), "Region")))

# 2013
b13.raw <- read_excel("BpS13.xls",sheet=1)
b13a<- filter(b13.raw, `Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2013` %in% c("Piemonte","Valle d'Aosta-Vallèe d'Aoste","Lombardia",
                                                                                                    "Bolzano-Bozen", "Trento", "Trentino-A. Adige/Südtirol",
                                                                                                    "Veneto","Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna",
                                                                                                    "Toscana", "Umbria")) %>%
  select(c(`Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2013`, X__1, X__2))
colnames(b13a) <- c("Region", "Male", "Female")

b13b <- filter(b13.raw, X__7 %in% c("Marche", "Lazio", "Abruzzo", "Molise",
                                    "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia",
                                    "Sardegna")) %>%
  select(c(X__7, X__8, X__9))
colnames(b13b) <- c("Region", "Male", "Female")
b13 <- rbind(data.matrix(column_to_rownames(as.data.frame(b13a), "Region")),
             data.matrix(column_to_rownames(as.data.frame(b13b), "Region")))

# 2012
b12.raw <- read_excel("BpS12.xls",sheet=1)
b12a<- filter(b12.raw, `Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2012` %in% c("Piemonte","Valle d'Aosta-Vallèe d'Aoste","Lombardia",
                                                                                                    "Bolzano-Bozen", "Trento", "Trentino-A. Adige/Südtirol",
                                                                                                    "Veneto","Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna",
                                                                                                    "Toscana", "Umbria")) %>%
  select(c(`Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2012`, X__1, X__2))
colnames(b12a) <- c("Region", "Male", "Female")

b12b <- filter(b12.raw, X__7 %in% c("Marche", "Lazio", "Abruzzo", "Molise",
                                    "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia",
                                    "Sardegna")) %>%
  select(c(X__7, X__8, X__9))
colnames(b12b) <- c("Region", "Male", "Female")
b12 <- rbind(data.matrix(column_to_rownames(as.data.frame(b12a), "Region")),
             data.matrix(column_to_rownames(as.data.frame(b12b), "Region")))

# 2011
b11.raw <- read_excel("BpS11.xls",sheet=1)
b11a<- filter(b11.raw, `Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2011` %in% c("Piemonte","Valle d'Aosta-Vallèe d'Aoste","Lombardia",
                                                                                                    "Bolzano-Bozen", "Trento", "Trentino-A. Adige/Südtirol",
                                                                                                    "Veneto","Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna",
                                                                                                    "Toscana", "Umbria")) %>%
  select(c(`Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2011`, X__1, X__2))
colnames(b11a) <- c("Region", "Male", "Female")

b11b <- filter(b11.raw, X__7 %in% c("Marche", "Lazio", "Abruzzo", "Molise",
                                    "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia",
                                    "Sardegna")) %>%
  select(c(X__7, X__8, X__9))
colnames(b11b) <- c("Region", "Male", "Female")
b11 <- rbind(data.matrix(column_to_rownames(as.data.frame(b11a), "Region")),
             data.matrix(column_to_rownames(as.data.frame(b11b), "Region")))

# 2010
b10.raw <- read_excel("BpS10.xls",sheet=1)
b10a<- filter(b10.raw, `Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2010` %in% c("Piemonte","Valle d'Aosta-Vallèe d'Aoste","Lombardia",
                                                                                                    "Bolzano-Bozen", "Trento", "Trentino-A. Adige/Südtirol",
                                                                                                    "Veneto","Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna",
                                                                                                    "Toscana", "Umbria")) %>%
  select(c(`Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2010`, X__1, X__2))
colnames(b10a) <- c("Region", "Male", "Female")

b10b <- filter(b10.raw, X__7 %in% c("Marche", "Lazio", "Abruzzo", "Molise",
                                    "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia",
                                    "Sardegna")) %>%
  select(c(X__7, X__8, X__9))
colnames(b10b) <- c("Region", "Male", "Female")
b10 <- rbind(data.matrix(column_to_rownames(as.data.frame(b10a), "Region")),
             data.matrix(column_to_rownames(as.data.frame(b10b), "Region")))

# 2009
b09.raw <- read_excel("BpS09.xls",sheet=1)
b09a<- filter(b09.raw, `Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2009` %in% c("Piemonte","Valle d'Aosta-Vallèe d'Aoste","Lombardia",
                                                                                                    "Bolzano-Bozen", "Trento", "Trentino-A. Adige/Südtirol",
                                                                                                    "Veneto","Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna",
                                                                                                    "Toscana", "Umbria")) %>%
  select(c(`Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2009`, X__1, X__2))
colnames(b09a) <- c("Region", "Male", "Female")

b09b <- filter(b09.raw, X__7 %in% c("Marche", "Lazio", "Abruzzo", "Molise",
                                    "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia",
                                    "Sardegna")) %>%
  select(c(X__7, X__8, X__9))
colnames(b09b) <- c("Region", "Male", "Female")
b09 <- rbind(data.matrix(column_to_rownames(as.data.frame(b09a), "Region")),
             data.matrix(column_to_rownames(as.data.frame(b09b), "Region")))

# 2008
b08.raw <- read_excel("BpS08.xls",sheet=1)
b08a<- filter(b08.raw, `Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2008` %in% c("Piemonte","Valle d'Aosta-Vallèe d'Aoste","Lombardia",
                                                                                                    "Bolzano-Bozen", "Trento", "Trentino-Alto Adige",
                                                                                                    "Veneto","Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna",
                                                                                                    "Toscana", "Umbria")) %>%
  select(c(`Tavola 1.13 - Nati per sesso e provincia -  Anno di iscrizione 2008`, X__1, X__2))
colnames(b08a) <- c("Region", "Male", "Female")

b08b <- filter(b08.raw, X__7 %in% c("Marche", "Lazio", "Abruzzo", "Molise",
                                    "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia",
                                    "Sardegna")) %>%
  select(c(X__7, X__8, X__9))
colnames(b08b) <- c("Region", "Male", "Female")
b08 <- rbind(data.matrix(column_to_rownames(as.data.frame(b08a), "Region")),
             data.matrix(column_to_rownames(as.data.frame(b08b), "Region")))

# 2007
b07.raw <- read_excel("BpS07.xls",sheet=1)
b07a<- filter(b07.raw, `Tavola 1.2 - Nati per sesso e provincia -  Anno di iscrizione 2007` %in% c("Piemonte","Valle d'Aosta-Vallèe d'Aoste","Lombardia",
                                                                                                    "Bolzano-Bozen", "Trento", "Trentino-Alto Adige",
                                                                                                    "Veneto","Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna",
                                                                                                    "Toscana", "Umbria")) %>%
  select(c(`Tavola 1.2 - Nati per sesso e provincia -  Anno di iscrizione 2007`, X__1, X__2))
colnames(b07a) <- c("Region", "Male", "Female")

b07b <- filter(b07.raw, X__7 %in% c("Marche", "Lazio", "Abruzzo", "Molise",
                                    "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia",
                                    "Sardegna")) %>%
  select(c(X__7, X__8, X__9))
colnames(b07b) <- c("Region", "Male", "Female")
b07 <- rbind(data.matrix(column_to_rownames(as.data.frame(b07a), "Region")),
             data.matrix(column_to_rownames(as.data.frame(b07b), "Region")))

# 2006
b06.raw <- read_excel("BpS06.xls",sheet=1)
b06a<- filter(b06.raw, `Tavola 1.2 - Nati per sesso e provincia -  Anno di iscrizione 2006` %in% c("Piemonte","Valle d'Aosta-Vallèe d'Aoste","Lombardia",
                                                                                                   "Bolzano-Bozen", "Trento", "Trentino-Alto Adige",
                                                                                                   "Veneto","Friuli-Venezia Giulia", "Liguria", "Emilia-Romagna",
                                                                                                   "Toscana", "Umbria")) %>%
  select(c(`Tavola 1.2 - Nati per sesso e provincia -  Anno di iscrizione 2006`, X__1, X__2))
colnames(b06a) <- c("Region", "Male", "Female")

b06b <- filter(b06.raw, X__7 %in% c("Marche", "Lazio", "Abruzzo", "Molise",
                                    "Campania", "Puglia", "Basilicata", "Calabria", "Sicilia",
                                    "Sardegna")) %>%
  select(c(X__7, X__8, X__9))
colnames(b06b) <- c("Region", "Male", "Female")
b06 <- rbind(data.matrix(column_to_rownames(as.data.frame(b06a), "Region")),
             data.matrix(column_to_rownames(as.data.frame(b06b), "Region")))

b0616 <- array(c(b06, b07, b08, b09, b10, b11, b12, b13, b14, b15, b16),
               dim = c(22, 2, 11))
italy.births.sex <- b0616[c(1:3,6,4,5,7:22),c(2,1),]

Sex <- c("Female", "Male") # Sexes list
Regions <- c("Piemonte","Valle D'Aosta","Lombardia",
             "Trentino Alto Adige","Bolzano/Bozen","Trento",
             "Veneto","Friuli Venezia Giulia", "Liguria",
             "Emilia Romagna","Toscana","Umbria",
             "Marche","Lazio","Abruzzo",
             "Molise","Campania","Puglia",
             "Basilicata","Calabria","Sicilia",
             "Sardegna")
Years <- 2006:2016 # Years list
dimnames(italy.births.sex) <- list(region = Regions,
                         sex = Sex,
                         time = Years)

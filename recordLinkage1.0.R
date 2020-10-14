# Probabilistic Record Linkage
# Bron: https://github.com/djvanderlaan/reclin
# laad de benodigde packages.
library(reclin)
library(dplyr)

# Zet de werkdirectory goed.
setwd("C:/Rcursus/Week14")

# Maak de werkruimte leeg
rm(list=ls())

# Functies ophalen
source("functieruimte.R")

# Lees twee dataframes in die gekoppeld moeten worden.
df1<-read.csv2("AISdata1.csv")
df2<-read.csv2("IVSdata1.csv")

# Zet datums in standaardformaat.
df1$begindatum<-strptime(df1$begindatum,"%Y-%m-%d %T")
df1$einddatum<-strptime(df1$einddatum,"%Y-%m-%d %T")

df2$einddatum<-strptime(df2$einddatum,"%Y-%m-%d %T")
df2$begindatum<-strptime(df2$begindatum,"%Y-%m-%d %T")

# Geef kolommen in verschillende dataframes met zelfde inhoud, dezelfde naam.
names(df1)[names(df1) == "EU_nr"] <- "schipID"
names(df2)[names(df2) == "IVS_Scheepsnummer"] <- "schipID"

# Automatische koppeling van df2 aan df1
df3 <- pair_blocking(df1, df2, "schipID") %>%
  compare_pairs(by = c("begindatum", "einddatum"),
                comparators = list(absoluutVerschil),
                default_comparator = absoluutVerschil()) %>%
  score_simsum(var = "simsum") %>%
  linTrans() %>%
  select_n_to_m("simsum", var = "ntom", threshold = 0) %>%
  link()

write.csv2(df3, "Koppelresulaat1.csv")

# Beoordeel koppelresultaat
df4 <- df3[, c("reis.x", "reis.y", "begindatum.x", "einddatum.x", "begindatum.y",
               "einddatum.y", "laadregio", "losregio", "IVS_Plaats_lading",
               "IVS_Plaats_lossing", "afstand.x", "afstand.y", "IVS_Reis_id.y")]

df4$penalty <- round(absoluutVerschil(df3$begindatum.x, df3$begindatum.y) + 
  absoluutVerschil(df3$einddatum.x, df3$einddatum.y))


f <- df4$afstand.x
distx <- as.numeric(levels(f))[f]

disty <- df4$afstand.y

df4$dAfstand <- distx - disty

aantKop <- 0

for (i in 1:nrow(df4)) {
  if (!is.na(df4$reis.x[i]) & !is.na(df4$reis.y[i])) {aantKop <- aantKop + 1}
  df4$dLaad[i] <- TRUE
  df4$dLos[i] <- TRUE
  if (!is.na(df4$begindatum.x[i])) {
    df4$v_AIS[i] <- round(distx[i] / 
                    absoluutVerschil(df4$einddatum.x[i], df4$begindatum.x[i])*60)
  }
  else {
    df4$v_AIS[i] <- 0
  }
  if (!is.na(df4$begindatum.y[i])) {
    df4$v_IVS[i] <- round(disty[i] / 
                    absoluutVerschil(df4$einddatum.y[i], df4$begindatum.y[i])*60)
  }
  else {
    df4$v_IVS[i] <- 0
  }
}

if (aantKop > 0) {
  for (i in 1:aantKop) {
    if ((df4$IVS_Plaats_lading[i] != "BESTE") &
        (substr(df4$IVS_Plaats_lading[i],3,5) != "XXX") &
        (substr(df4$laadregio[i],3,5) != "XXX") &
        (df4$laadregio[i] != df4$IVS_Plaats_lading[i])) {
      df4$dLaad[i] <- FALSE  
    }
    if ((df4$IVS_Plaats_lossing[i] != "BESTE") &
        (substr(df4$IVS_Plaats_lossing[i],3,5) != "XXX") &
        (substr(df4$losregio[i],3,5) != "XXX") &
        (df4$losregio[i] != df4$IVS_Plaats_lossing[i])) {
      df4$dLos[i] <- FALSE  
    }
  }  
}

write.csv2(df4, "BeoordeelResultaat1.csv", row.names = FALSE)
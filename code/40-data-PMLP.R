# ESS9 Sampling (Population) frame

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)

# Packages
require(data.table)
require(openxlsx)

# Reset ####
rm(list = ls())
gc()



# ATVK ####

dat.ATVK <- fread("data/ATVK.csv", encoding = "UTF-8",
                  colClasses = list(character = c("kods", "vecaka_kods")),
                  drop = "apraksts")
dat.ATVK




# CSP kolektīvo mājokļu saraksts ####

dat.kol <- read.xlsx("data/CSP/piepras_kolekt_sab_062018.xlsx")
setDT(dat.kol)

dat.kol


# PMLP dati

dat.preg <- read.xlsx("data/PMLP/rezult_20181102.xlsx")
setDT(dat.preg)

dat.preg
anyDuplicated(dat.preg)

# ATVK
dat.preg[!is.na(pašvaldības.ATVK.kods),
         pašvaldības.ATVK.kods := paste0("0", pašvaldības.ATVK.kods)]
dat.preg[!is.na(teritorijas.ATVK.kods),
         teritorijas.ATVK.kods := paste0("0", teritorijas.ATVK.kods)]
dat.preg[is.na(pašvaldības.ATVK.kods), .N]

dat.preg[ is.na(teritorijas.ATVK.kods), ATVK.kods := pašvaldības.ATVK.kods]
dat.preg[!is.na(teritorijas.ATVK.kods), ATVK.kods := teritorijas.ATVK.kods]

dat.preg <- merge(dat.preg, dat.ATVK,
                  by.x = "ATVK.kods", by.y = "kods",
                  all.x = T, sort = F)
dat.preg

dat.preg[order(ATVK.kods)]
dat.preg[, .N, keyby = .(limenis)]

tab.ATVK <- dat.preg[, .N, keyby = .(ATVK.kods, nosaukums, limenis)]
tab.ATVK
tab.ATVK[order(N)]


# ARIS
dat.preg[order(ARIS.kods)]
dat.preg[, .N, keyby = .(is.na(ARIS.kods))][, P := round(prop.table(N), 2)][]
anyDuplicated(dat.preg, by = "ARIS.kods")
dat.preg[, n1 := .N, by = .(ARIS.kods)]
tab.ARIS <- dat.preg[n1 > 1 & !is.na(ARIS.kods), .(ATVK.kods, nosaukums,
                                                   ARIS.kods, adrese, n1)]
tab.ARIS


# Adrese
dat.preg[order(adrese), .(ATVK.kods, ARIS.kods, adrese)][1:10]

dat.preg[, .N, keyby = .(nchar(adrese))]

dat.preg[order(nchar(adrese)), .(ATVK.kods, ARIS.kods, adrese)]
dat.preg[nchar(adrese) == min(nchar(adrese)), .(ATVK.kods, ARIS.kods, adrese)]


anyDuplicated(dat.preg, by = "adrese")
dat.preg[, n2 := .N, by = .(adrese)]
tab.adrese <- dat.preg[n2 > 1, .(ATVK.kods, nosaukums, ARIS.kods, adrese, n2)]
tab.adrese



# Export tables

tabnames <- ls(pattern = "^tab")
tablist <- mget(tabnames)

write.xlsx(tablist, file = "results/PMLP-tables.xlsx",
           colWidths = "auto", firstRow = T)

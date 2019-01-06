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

file.exists(file.path(getwd(), "data/ATVK_act.csv"))

readlist <- paste("iconv -f ISO-8859-13 -t UTF-8",
                  file.path(getwd(), "data/ATVK_act.csv"))
readlist

dat.ATVK <- fread(cmd = readlist, colClasses = "character")
dat.ATVK




# CSP kolektīvo mājokļu saraksts ####

dat.kol <- read.xlsx("data/CSP/piepras_kolekt_sab_062018.xlsx")
setDT(dat.kol)

dat.kol

dat.kol.adrese <- tolower(unique(dat.kol$AdreseAdrešuReģistrāAR))

dat.kol.adr_kodi <- melt(data = dat.kol,
                         measure.vars = c("Ēkaskods", "Dzīvokļakods"),
                         na.rm = T, value.name = "adr_kods")
setorder(dat.kol.adr_kodi, adr_kods)
dat.kol.adr_kodi[grep("^[0-9]{9}$", adr_kods)]

dat.kol.adr_kodi_eka  <- dat.kol.adr_kodi[grep("^10[0-9]{7}$", adr_kods), adr_kods]
dat.kol.adr_kodi_dziv <- dat.kol.adr_kodi[grep("^11[0-9]{7}$", adr_kods), adr_kods]


# PMLP dati
load("data/frame_pmlp.Rdata")

frame_pmlp

pmlp_adr_kodi <- frame_pmlp[!is.na(ARIS.kods), unique(ARIS.kods)]
pmlp_adreses  <- frame_pmlp[!is.na(adrese), tolower(unique(adrese))]


# VZD dati
load("data/frame_majo_vzd.Rdata")

frame_majo_vzd[, kol1 := tolower(adrese) %in% dat.kol.adrese]
frame_majo_vzd[, kol2 := adr_kods_eka %in% dat.kol.adr_kodi_eka]
frame_majo_vzd[, kol3 := adr_kods_dziv %in% dat.kol.adr_kodi_dziv]

frame_majo_vzd[, .N, keyby = .(kol1, kol2, kol3)]
frame_majo_vzd[, kol := kol1 | kol2 | kol3]
frame_majo_vzd[, .N, keyby = .(kol, kol1, kol2, kol3)]
frame_majo_vzd[, .N, keyby = .(kol)]
frame_majo_vzd[, c("kol1", "kol2", "kol3") := NULL]

frame_majo_vzd[, dekl1 := tolower(adrese) %in% pmlp_adreses]
frame_majo_vzd[, dekl2 := adr_kods %in% pmlp_adr_kodi]

frame_majo_vzd[, .N, keyby = .(dekl1, dekl2)]
frame_majo_vzd[, dekl := dekl1 | dekl2]
frame_majo_vzd[, .N, keyby = .(dekl, dekl1, dekl2)]
frame_majo_vzd[, c("dekl1", "dekl2") := NULL]

frame_majo_vzd[, .N, keyby = .(kol, dekl)]



frame_majo_vzd[!(kol) & tips_cd == "108" & dziv_sk > 0, .N]
frame_majo_vzd[!(kol) & tips_cd == "108" & dziv_sk > 0, .N, keyby = .(dekl)]
frame_majo_vzd[!(kol) & tips_cd == "108" & dziv_sk > 0 & (dekl)]

frame_majo_vzd[, tmp := any(!(kol) & tips_cd == "108" & dziv_sk > 0 & (dekl)),
               by = .(adr_kods_eka)]
frame_majo_vzd[, .N, keyby = .(tips_cd, tmp)]

frame_majo_vzd[, sum_dekl := sum(dekl), by = .(adr_kods_eka)]
frame_majo_vzd[, sum_tmp  := sum(tmp),  by = .(adr_kods_eka)]

tab <- frame_majo_vzd[(tmp) & sum_dekl < sum_tmp,
                      .(adr_kods_eka, tips_cd, ATVK_code, adrese, adrese_sort,
                        dekl, sum_dekl)][order(ATVK_code, adr_kods_eka,
                                               tips_cd, adrese_sort)]
tab[, adrese_sort := NULL]

tab1 <- tab[sum_dekl == 1]
tab2 <- tab[sum_dekl >  1]

tab1[tips_cd == "109", .N, keyby = .(dekl)][, P := prop.table(N)][]
tab2[tips_cd == "109", .N, keyby = .(dekl)][, P := prop.table(N)][]

write.xlsx(list(tab1, tab2),
           file = "data/ekas-test.xlsx", firstRow = T, colWidths = "auto")

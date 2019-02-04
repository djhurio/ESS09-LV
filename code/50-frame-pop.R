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


# UR data ####

load(file = "data/UR/dat_UR.Rdata")
dat_UR


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

# Atzīmē daudzdzīvokļu ēkas (un visus tās dzīvokļus),
# kuras nav kolektīvās un kurās ēkas līmenī ir deklarētās personas
frame_majo_vzd[, problem := any(tips_cd == "108" & dziv_sk > 0 & !(kol) & (dekl)),
               by = .(adr_kods_eka)]
frame_majo_vzd[, .N, keyby = .(problem, tips_cd)]
frame_majo_vzd[, .N, keyby = .(kol, problem, tips_cd)]

frame_majo_vzd[(kol) & (problem)]

frame_majo_vzd[adr_kods_eka %in% frame_majo_vzd[(kol) & (problem), adr_kods_eka]]

frame_majo_vzd[, sum_dekl := sum(dekl), by = .(adr_kods_eka)]

# Problemātiskās ēkās, kurās personas ir deklarētas gan ēkas, gan dzīvokļu līmenī,
# un ir dzīvokļi, kuros nav deklarētas personas
tab <- frame_majo_vzd[(problem) & sum_dekl < dziv_sk + 1,
                      .(adr_kods_eka, tips_cd, ATVK_code, adrese, adrese_sort,
                        dekl, sum_dekl)][order(ATVK_code, adr_kods_eka,
                                               tips_cd, adrese_sort)]
tab[, adrese_sort := NULL]

# Ēkas, kur personas ir tikai ēkas līmenī
tab1 <- tab[sum_dekl == 1]
# Ēkas, kur personas ir gan ēkas, gan dzīvokļa līmenī
tab2 <- tab[sum_dekl >  1]

# Dzīvokļos nav deklarētu personu
tab1[tips_cd == "109", .N, keyby = .(dekl)][, P := prop.table(N)][]
# Dzīvokļu īpatsvars, kuros ir deklarētas personas
tab2[tips_cd == "109", .N, keyby = .(dekl)][, P := prop.table(N)][]

write.xlsx(list(tab1, tab2),
           file = "data/ekas-test.xlsx", firstRow = T, colWidths = "auto")

# Lēmums: šajos gadījumos mājokļu ietvarā tiek ietverti visi šo ēku dzīvokļi

tab3 <- frame_majo_vzd[, .N, keyby = .(kol, tips_cd, ddzeka = dziv_sk > 0,
                                       problem, dekl)]
tab3

write.xlsx(x = tab3, file = "data/frame-tab.xlsx",
           firstRow = T, colWidths = "auto")

frame_majo <- frame_majo_vzd[!kol & ((tips_cd == "108" & dziv_sk == 0 & dekl) |
                                       (tips_cd == "109" & (dekl | problem)))]

frame_majo[, .N]

frame_majo[, .N, keyby = .(kol, tips_cd, ddzeka = dziv_sk > 0, problem, dekl)]

# 1. līmenis – 119 administratīvās teritorijas
frame_majo[, .N, keyby = .(ATVK_L1)]
frame_majo[, .N, keyby = .(ATVK_L1)][order(N)]

frame_majo[is.na(ATVK_L2), .N, keyby = .(ATVK_L1)]
frame_majo[is.na(ATVK_L2), .N, keyby = .(ATVK_L1)][order(N)]

# 2. līmenis – 564 novadu teritoriālā iedalījuma vienības
frame_majo[!is.na(ATVK_L2), .N, keyby = .(ATVK_L2)]
frame_majo[!is.na(ATVK_L2), .N, keyby = .(ATVK_L2)][order(N)]

frame_majo[, .N, keyby = .(ATVK_L1)][order(-N)][1:10]
frame_majo[, .N, keyby = .(ATVK_code)][order(-N)][1:10]


frame_majo[, c("kol", "problem", "sum_dekl") := NULL]

sapply(dat_UR, class)

# Reģistrēto uzņēmumu skaits mājoklī
frame_majo <- merge(x = frame_majo,
                    y = dat_UR[, .(addressid, uzn_sk)],
                    by.x = "adr_kods", by.y = "addressid",
                    all.x = T, sort = F)

frame_majo[is.na(uzn_sk), uzn_sk := 0L]

frame_majo[, uzn_sk_dummy := as.integer(uzn_sk > 0)]

frame_majo[, .N, keyby = .(uzn_sk_dummy)][, P := prop.table(N)][]

# frame_majo[order(-uzn_sk), .(adr_kods, adrese, uzn_sk)][1:5]
# frame_majo[adr_kods == "115070060", .(adr_kods, adrese, uzn_sk)]

frame_majo[, lapply(.SD, sum), .SDcols = patterns("uzn_sk")]
frame_majo[, lapply(.SD, mean), .SDcols = patterns("uzn_sk")]

save(frame_majo, file = "results/frame_majo.Rdata")

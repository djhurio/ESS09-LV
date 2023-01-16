# ESS9 Sampling - VZD frame

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)

# Packages
require(data.table)

# Reset ####
rm(list = ls())
gc()


# Load data ####

load("~/data/VZD/data-VZD.Rdata")


# Dzīvokļi ####

aw_dziv

aw_dziv[, .N, keyby = .(tips_cd)]
# 1:     109 932251

aw_dziv[, .N, keyby = .(statuss)]
# 1:     DEL  16427
# 2:     EKS 845122
# 3:     ERR  70702

aw_dziv[, .N, keyby = .(apstipr)]
# 1:         126549
# 2:       Y 805702

aw_dziv[, .N, keyby = .(apst_pak)]
# 1:       NA 147744
# 2:      251  10217
# 3:      252 278902
# 4:      253  51075
# 5:      254 444313

aw_dziv[, .N, keyby = .(vkur_tips)]
# 1:       108 932251

frame_dziv <- aw_dziv[statuss == "EKS", .(kods, tips_cd, vkur_cd, vkur_tips,
                                          nosaukums, sort_nos)]


# Ēkas un zemes ####

aw_eka

aw_eka[, .N, keyby = .(statuss)]
# 1:     DEL  26764
# 2:     EKS 525262
# 3:     ERR  27950

aw_eka[, .N, keyby = .(apstipr)]
# 1:          15006
# 2:       Y 564970

aw_eka[, .N, keyby = .(apst_pak)]
# 1:       NA  24399
# 2:      251  13095
# 3:      252 447956
# 4:      253  46617
# 5:      254  47909

aw_eka[, .N, keyby = .(vkur_tips)]
# 1:       104   1826
# 2:       105 119836
# 3:       106 131511
# 4:       107 322030
# 5:       113   4773

# Pasta indekss
aw_eka[, .N, keyby = .(atrib)]

# Pasta nodaļas kods
aw_eka[, .N, keyby = .(pnod_cd)]

# Pazīme, ka adresācijas objekts  ir apbūvei paredzēta zemes vienība.
# Y – ir apbūvei paredzēta zemes vienība; N – ir ēka
aw_eka[, .N, keyby = .(for_build)]
# 1:         N 435216
# 2:         Y 144760

aw_eka[, .N, keyby = .(statuss, for_build)]

aw_eka[kods == "101005035"]

aw_eka[, koord_x := as.numeric(koord_x)]
aw_eka[, koord_y := as.numeric(koord_y)]

frame_eka <- aw_eka[statuss == "EKS",
                       .(kods, tips_cd, vkur_cd, vkur_tips,
                         nosaukums, sort_nos, atrib,
                         koord_x, koord_y)]


# Teritorijas ####

aw_ter

aw_ter[, .N, keyby = .(tips_cd)]

aw_ter[, .N, keyby = .(vkur_tips)]

dcast(aw_ter[statuss == "EKS"], tips_cd ~ vkur_tips)

aw_ter[, .N, keyby = .(apstipr)]
# 1:          1506
# 2:       Y 29177

aw_ter[, .N, keyby = .(apst_pak)]
# 1:       NA  1603
# 2:      251   728
# 3:      252 23006
# 4:      253  5061
# 5:      254   285

aw_ter[, .N, keyby = .(statuss)]
# 1:     DEL  3476
# 2:     EKS 24735
# 3:     ERR  2472



# Ielas un teritorijas ####

frame_ter <- aw_ter[statuss == "EKS", .(kods, tips_cd, vkur_cd, vkur_tips,
                                        nosaukums, sort_nos, atrib)]
# frame_ter
dcast(frame_ter, tips_cd ~ vkur_tips)

vnames <- c("nosaukums", "tips_cd", "vkur_cd", "atrib")

tab <- frame_ter[, c("kods", vnames), with = F]

tab0 <- copy(tab)
setnames(tab0, vnames, paste0(vnames, 0))

for (i in 1:10) {
  cat(i, "\n")
  
  setnames(tab, 1 + seq_along(vnames), paste0(vnames, i))

  tab0 <- merge(x = tab0, y = tab, all.x = T,
                by.x = paste0("vkur_cd", i - 1), by.y = "kods",
                sort = F)

  if (all(is.na(tab0[[length(tab0)]]))) {
    tab0[, c(paste0(vnames, i)) := NULL]
    break
  }
}

tab0[, .N, .(tips_cd0, tips_cd1, tips_cd2, tips_cd3)]

# Ciemi, kuri ir novados bez pagastiem
tab0[tips_cd0 == "106" & tips_cd1 == "113", .N,
     keyby = .(tips_cd1, nosaukums1)]

vnames1 <- sort(grep("tips_cd", names(tab0), value = T))
tab1 <- melt(data = tab0, id.vars = "kods", measure.vars = vnames1,
             variable.name = "id", variable.factor = T,
             value.name = "tips_cd", na.rm = T)
tab1[, id := as.integer(id)]
setkey(tab1)
tab1

vnames2 <- sort(grep("nosaukums", names(tab0), value = T))
tab2 <- melt(data = tab0, id.vars = "kods", measure.vars = vnames2,
             variable.name = "id", variable.factor = T,
             value.name = "nosaukums", na.rm = T)
tab2[, id := as.integer(id)]
setkey(tab2)
tab2

vnames3 <- sort(grep("atrib", names(tab0), value = T))
tab3 <- melt(data = tab0, id.vars = "kods", measure.vars = vnames3,
             variable.name = "id", variable.factor = T,
             value.name = "atrib", na.rm = T)
tab3[, id := as.integer(id)]
setkey(tab3)
tab3

tab4 <- Reduce(f = merge, x = list(tab1, tab2, tab3))

tab4[, .N, keyby = .(tips_cd)]
tab4[, tips_cd := factor(x = tips_cd,
                         levels = c(104:107, 113),
                         labels = c("pilseta", "pagasts", "ciems",
                                    "iela", "novads"))]
tab4[, .N, keyby = .(tips_cd)]

setnames(tab4, c("nosaukums", "atrib"), c("nos", "ATVK"))

tab5 <- dcast(data = tab4, formula = kods ~ tips_cd,
              value.var = c("ATVK", "nos"))
tab5

tab5[, .N, keyby = .(is.na(ATVK_novads), is.na(ATVK_pilseta), is.na(ATVK_pagasts))]

tab5[!is.na(ATVK_novads), ATVK_L1 := ATVK_novads]
tab5[ is.na(ATVK_novads), ATVK_L1 := ATVK_pilseta]
tab5[, .N, keyby = .(ATVK_L1)]

tab5[!is.na(ATVK_novads) & !is.na(ATVK_pilseta), ATVK_L2 := ATVK_pilseta]
tab5[!is.na(ATVK_novads) & !is.na(ATVK_pagasts), ATVK_L2 := ATVK_pagasts]

tab5[, c("ATVK_pilseta", "ATVK_pagasts", "ATVK_ciems",
         "ATVK_iela", "ATVK_novads") := NULL]


frame_ter
tab5


paste2 <- function(x) paste(x[!is.na(x)], collapse = ", ")
tab5[, adrese0 := paste2(c(nos_pilseta, nos_ciems, nos_pagasts, nos_novads)),
     by = .(kods)]


frame_ter_2 <- merge(x = frame_ter[, .(kods, tips_cd_ter = tips_cd)],
                     y = tab5, by = "kods")
frame_ter_2





# Mājokļu ietvars ####

frame_eka
frame_dziv


# Ēku nosaukumi bez cipariem
frame_eka[!grepl("[0-9]", nosaukums)]
frame_eka[!grepl("[0-9]", nosaukums) & sort_nos != nosaukums]
frame_eka[!grepl("[0-9]", nosaukums), sort_nos := nosaukums]
frame_eka[!grepl("[0-9]", nosaukums) & sort_nos != nosaukums]


# Ēku nosaukumi ar cipariem
frame_eka[grepl("[0-9]", nosaukums) & !grepl("[0-9]{4}", nosaukums)]
frame_eka[grepl("[0-9]", nosaukums) & !grepl("[0-9]{4}", nosaukums) & sort_nos == nosaukums,
          .(kods, nosaukums, sort_nos)]



# Pēdiņas mājas nosaukumiem
frame_eka[!grepl("^[0-9]+[A-Z]?( k-[0-9]+[A-Z]?)?$", nosaukums)]

frame_eka[!grepl("^[0-9]+[A-Z]?( k-[0-9]+[A-Z]?)?$", nosaukums),
          nosaukums := paste0('"', nosaukums, '"')]

frame_eka[!grepl("^[0-9]+[A-Z]?( k-[0-9]+[A-Z]?)?$", nosaukums)]

frame_eka[grepl("^[0-9]+[A-Z]?( k-[0-9]+[A-Z]?)?$", nosaukums)]

setnames(frame_eka, c("nosaukums", "sort_nos", "atrib", "vkur_cd"),
         c("nos_eka", "nos_eka_sort", "pasts", "vkur_cd_eka"))


frame_eka
aw_geo


# aw_geo[, koord_x := as.numeric(koord_x)]
# aw_geo[, koord_y := as.numeric(koord_y)]
# 
# frame_eka <- merge(x = frame_eka, y = aw_geo,
#                     by.x = "kods", by.y = "vieta_cd",
#                     all.x = T)
frame_eka[is.na(koord_x) | is.na(koord_y)]


# Ēkas nosaukums + pasta indekss
tab <- frame_eka[, .(kods, nos_eka, nos_eka_sort, pasts, vkur_cd_eka,
                      koord_x, koord_y)]
tab

frame_dziv2 <- merge(x = frame_dziv, y = tab,
                     by.x = "vkur_cd", by.y = "kods", all.x = T)
frame_dziv2[is.na(pasts)]

setnames(frame_dziv2, c("nosaukums", "sort_nos", "vkur_cd"),
         c("nos_dziv", "nos_dziv_sort", "vkur_cd_dziv"))

frame_dziv2


# Mājokļu ietvars (ēkas + dzīvokļi)
frame_majo <- rbindlist(list(frame_eka, frame_dziv2), fill = T)

setkey(frame_majo, kods)

anyDuplicated(frame_majo, by = "kods")

frame_majo[, .N, keyby = .(tips_cd)]

frame_majo[, adr_kods := kods]

frame_majo[tips_cd == 108, adr_kods_eka := kods]
frame_majo[tips_cd == 109, adr_kods_eka := vkur_cd_dziv]

frame_majo[tips_cd == 109, adr_kods_dziv := kods]

frame_majo <- merge(x = frame_majo, y = frame_ter_2,
                    by.x = "vkur_cd_eka", by.y = "kods", all.x = T)

frame_majo[, .N, keyby = .(tips_cd_ter)]
frame_majo[, .N, keyby = .(tips_cd, tips_cd_ter == "107")]

# Ēka
frame_majo[tips_cd == "108", adrese1  := nos_eka]
frame_majo[tips_cd == "108", adrese1s := nos_eka_sort]
# Dzīvoklis
frame_majo[tips_cd == "109", adrese1  := paste(nos_eka, nos_dziv, sep = " - ")]
frame_majo[tips_cd == "109", adrese1s := paste(nos_eka_sort, nos_dziv_sort, sep = " - ")]

# Ir iela
frame_majo[tips_cd_ter == "107", adrese2  := paste(nos_iela, adrese1)]
frame_majo[tips_cd_ter == "107", adrese2s := paste(nos_iela, adrese1s)]
# Nav ielas
frame_majo[tips_cd_ter != "107", adrese2  := adrese1]
frame_majo[tips_cd_ter != "107", adrese2s := adrese1s]

# Adrese
frame_majo[, adrese      := paste(adrese2,  adrese0, pasts, sep = ", ")]
frame_majo[, adrese_sort := paste(adrese2s, adrese0, pasts, sep = ", ")]

del_varl <- grep("^adrese[0-9]", names(frame_majo), value = T)
frame_majo[, c(del_varl) := NULL]

del_varl <- grep("^nos_", names(frame_majo), value = T)
frame_majo[, c(del_varl) := NULL]

tab <- frame_majo[sample(.N, 10), .(adr_kods, adrese)]
fwrite(tab, file = "~/data/tab.csv")

frame_majo[ is.na(ATVK_L2), ATVK_code := ATVK_L1]
frame_majo[!is.na(ATVK_L2), ATVK_code := ATVK_L2]

frame_majo_vzd <- frame_majo[, .(adr_kods, adr_kods_eka, adr_kods_dziv,
                                 ATVK_code, ATVK_L1, ATVK_L2,
                                 koord_x, koord_y, tips_cd,
                                 adrese, adrese_sort)]

setkey(frame_majo_vzd, adr_kods_eka, adr_kods)

frame_majo_vzd[, dziv_sk := .N - 1, by = .(adr_kods_eka)]
frame_majo_vzd[, .N, keyby = .(dziv_sk)]

frame_majo_vzd[, .N, keyby = .(tips_cd, dziv_sk > 0)]

frame_majo_vzd[tips_cd == "108", .(adr_kods, adrese, dziv_sk)][order(-dziv_sk)][1:10]

save(frame_majo_vzd, file = "data/frame_majo_vzd.Rdata")

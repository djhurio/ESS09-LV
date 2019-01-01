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

load("data/VZD/data-VZD.Rdata")


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

aw_nlieta

aw_nlieta[, .N, keyby = .(statuss)]
# 1:     DEL  26764
# 2:     EKS 525262
# 3:     ERR  27950

aw_nlieta[, .N, keyby = .(apstipr)]
# 1:          15006
# 2:       Y 564970

aw_nlieta[, .N, keyby = .(apst_pak)]
# 1:       NA  24399
# 2:      251  13095
# 3:      252 447956
# 4:      253  46617
# 5:      254  47909

aw_nlieta[, .N, keyby = .(vkur_tips)]
# 1:       104   1826
# 2:       105 119836
# 3:       106 131511
# 4:       107 322030
# 5:       113   4773

# Pasta indekss
aw_nlieta[, .N, keyby = .(atrib)]

# Pasta nodaļas kods
aw_nlieta[, .N, keyby = .(pnod_cd)]

# Pazīme, ka adresācijas objekts  ir apbūvei paredzēta zemes vienība.
# Y – ir apbūvei paredzēta zemes vienība; N – ir ēka
aw_nlieta[, .N, keyby = .(for_build)]
# 1:         N 435216
# 2:         Y 144760

aw_nlieta[, .N, keyby = .(statuss, for_build)]

frame_eka <- aw_nlieta[statuss == "EKS" & for_build == "N",
                       .(kods, tips_cd, vkur_cd, vkur_tips,
                         nosaukums, sort_nos, atrib)]


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

frame_ter <- aw_ter[statuss == "EKS", .(kods, tips_cd, vkur_cd, vkur_tips,
                                        nosaukums, sort_nos, atrib)]




# Ielas un teritorijas ####

frame_ter

merge(x = frame_ter, y = frame_ter, all.x = T,
      by.x = "vkur_cd", by.y = "kods", sort = F)



# Mājokļu ietvars ####

frame_majo <- rbindlist(list(frame_eka, frame_dziv), fill = T)

anyDuplicated(frame_majo, by = "kods")

frame_majo[, .N, keyby = .(tips_cd)]
frame_majo[, .N, keyby = .(vkur_tips)]

frame_eka[, .(kods, vkur_cd, nosaukums, sort_nos, atrib)]

frame_majo_2 <- merge(x = frame_majo, y = frame_eka,
                      by.x = "vkur_cd", by.y = "kods", all.x = T,
                      suffixes = c("1", "2"), sort = F)

frame_majo_2

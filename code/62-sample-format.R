# ESS9-LV Sample Formatting

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)


# Packages ####
require(data.table)
require(openxlsx)
require(ggplot2)
require(OpenStreetMap)
require(sampling)


# Reset ####
rm(list = ls())
gc()


# DEGURBA ####

tmp <- read.xlsx(xlsxFile = "data/2018 DEGURBA updated_official since 2019.xlsx")
setDT(tmp)

degurba <- tmp[, .(nov_code = substr(LAU.2.national.code, 1, 5),
                   frame1 = DEGURBA.code)]
degurba[, .N, keyby = .(frame1)]


# LUZ ####

tmp <- read.xlsx(xlsxFile = "data/LUZ-FUA.xlsx", rows = 1:41, cols = 1:2)
setDT(tmp)

tmp[order(ATVK)]

luz <- tmp[grep("LV[0-9]{3}L[1-9]", `Spatial_unit.-.City/LUZ./National`),
           .(nov_code = substr(ATVK, 1, 5),
             frame2 = substr(`Spatial_unit.-.City/LUZ./National`, 1, 5))]
luz[, .N, keyby = .(frame2)]

anyDuplicated(luz)


# DEGURBA / LUZ ####

degurba_luz <- merge(x = degurba, y = luz, by = "nov_code", all = T)
degurba_luz[is.na(frame2), frame2 := "LV000"]
degurba_luz

dcast.data.table(data = degurba_luz, formula = frame2 ~ frame1)

# Sample ####

load("results/sample_majo.Rdata")
sample_majo

sample_majo <- merge(x = sample_majo, y = degurba_luz, by = "nov_code",
                     all.x = T, sort = F)

sample_majo[, .N, keyby = .(frame1)]
sample_majo[, .N, keyby = .(frame2)]
dcast.data.table(data = sample_majo, formula = frame2 ~ frame1)


# Extra variables
sample_majo[, prob3 := NA]
sample_majo[, outcome := NA]


# Number of enterprises and organisation registered at the dwelling address
sample_majo[, frame3 := uzn_sk]
sample_majo[, as.list(summary(frame3))]
sample_majo[, as.list(summary(frame3)), keyby = .(stratex1)]

# PSU population size
sample_majo[, frame4 := psu_pop]


# Sample file for the fieldwork ####

ESS9_LV_sample <- sample_majo[, .(idno, stratex1, stratim1, stratim2,
                                  reg_code, reg_name,
                                  nov_code, nov_name,
                                  ATVK_code, atvk_name,
                                  pil_lauk, pil_lauk_name,
                                  apkaime_gid, apkaime,
                                  psu,
                                  adr_kods, adr_kods_eka, adr_kods_dziv,
                                  adrese,
                                  koord_x, koord_y, lat, lon)]

ESS9_LV_SDDF <- sample_majo[, .(idno,
                                prob1, prob2, prob3,
                                stratex1,
                                stratim1, stratim2,
                                strtval1, strtval2,
                                psu, outcome,
                                frame1, frame2, frame3, frame4)]


# Save ####

sample_majo_2 <- sample_majo
save(sample_majo_2, file = "results/sample_majo_2.Rdata")

fwrite(x = ESS9_LV_sample, file = "results/sent/ESS9_LV_sample.csv")
fwrite(x = ESS9_LV_SDDF, file = "results/sent/ESS9_LV_SDDF.csv")

write.xlsx(x = ESS9_LV_sample, file = "results/sent/ESS9_LV_sample.xlsx")
write.xlsx(x = ESS9_LV_SDDF, file = "results/sent/ESS9_LV_SDDF.xlsx")

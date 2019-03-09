# ESS9-LV Sample Formatting

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)


# Packages ####
require(data.table)
require(openxlsx)


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

dcast.data.table(data = degurba_luz, formula = frame2 ~ frame1,
                 fun.aggregate = length, value.var = "nov_code")

# Sample ####

load("results/sample_majo.Rdata")
sample_majo

sample_majo <- merge(x = sample_majo, y = degurba_luz, by = "nov_code",
                     all.x = T, sort = F)

sample_majo[, .N, keyby = .(frame1)]
sample_majo[, .N, keyby = .(frame2)]
dcast.data.table(data = sample_majo, formula = frame2 ~ frame1,
                 fun.aggregate = length, value.var = "idno")


# Number of enterprises and organisation registered at the dwelling address
sample_majo[, frame3 := uzn_sk]
sample_majo[, as.list(summary(frame3))]
sample_majo[, as.list(summary(frame3)), keyby = .(stratex1)]

# PSU population size
sample_majo[, frame4 := psu_pop]


# Recode variables according to the SDDF specifiaction ####

ESS9_LV_STRATEX1 <- unique(sample_majo[, .(stratex1)])
setorder(ESS9_LV_STRATEX1)
ESS9_LV_STRATEX1[, STRATEX1 := 1:.N]
ESS9_LV_STRATEX1

ESS9_LV_STRTVAL1 <- unique(sample_majo[, .(strtval1)])
setorder(ESS9_LV_STRTVAL1)
ESS9_LV_STRTVAL1[, STRTVAL1 := 1:.N]
ESS9_LV_STRTVAL1

ESS9_LV_STRTVAL2 <- unique(sample_majo[, .(strtval2)])
setorder(ESS9_LV_STRTVAL2)
ESS9_LV_STRTVAL2[, STRTVAL2 := 1:.N]
ESS9_LV_STRTVAL2

ESS9_LV_PSU <- unique(sample_majo[, .(stratex1, stratim1, psu)])
setorder(ESS9_LV_PSU)
ESS9_LV_PSU[, PSU := 1:.N]
ESS9_LV_PSU[, STRATIM1 := 1:.N]
ESS9_LV_PSU

ESS9_LV_SSU <- unique(sample_majo[, .(stratex1, stratim1, psu, stratim2, idno)])
setorder(ESS9_LV_SSU)
ESS9_LV_SSU[, SSU := 1:.N]
ESS9_LV_SSU[, STRATIM2 := 1:.N]
ESS9_LV_SSU[, all.equal(idno, SSU)]
ESS9_LV_SSU

sample_majo_2 <- copy(sample_majo)
sample_majo_2 <- merge(sample_majo_2, ESS9_LV_STRATEX1, by = "stratex1")
sample_majo_2 <- merge(sample_majo_2, ESS9_LV_STRTVAL1, by = "strtval1")
sample_majo_2 <- merge(sample_majo_2, ESS9_LV_STRTVAL2, by = "strtval2")
sample_majo_2 <- merge(sample_majo_2, ESS9_LV_PSU[, .(psu, PSU, STRATIM1)], by = "psu")
sample_majo_2 <- merge(sample_majo_2, ESS9_LV_SSU[, .(idno, SSU, STRATIM2)], by = "idno")
setorder(sample_majo_2, idno)


# Sample file for the fieldwork ####
ESS9_LV_sample <- sample_majo_2[, .(IDNO = idno,
                                    STRATEX1, STRATIM1, STRATIM2,
                                    PSU, SSU,
                                    reg_code, reg_name,
                                    nov_code, nov_name,
                                    ATVK_code, atvk_name,
                                    pil_lauk, pil_lauk_name,
                                    apkaime_gid, apkaime,
                                    adr_kods, adr_kods_eka, adr_kods_dziv,
                                    adrese,
                                    koord_x, koord_y, lat, lon)]


# SDDF ####
ESS9_LV_SDDF <- sample_majo_2[, .(idno,
                                  CNTRY = "LV",
                                  prob1,
                                  prob2,
                                  prob3 = NA,
                                  prob4 = NA,
                                  PSU,
                                  SAMPPOIN = NA,
                                  SSU,
                                  STRATEX1,
                                  STRATIM1,
                                  STRATIM2,
                                  STRTVAL1,
                                  STRTVAL2,
                                  STRTVAL3 = NA,
                                  OUTCOME = NA,
                                  frame1, frame2, frame3, frame4)]
setnames(ESS9_LV_SDDF, toupper(names(ESS9_LV_SDDF)))
ESS9_LV_SDDF


# Test ####

ESS9_LV_SDDF[, sum(1 / (PROB1 * PROB2))]


# Save ####

save(sample_majo_2, file = "results/sample_majo_2.Rdata")

fwrite(x = ESS9_LV_sample, file = "results/sent/ESS9_LV_sample.csv")
fwrite(x = ESS9_LV_SDDF, file = "results/sent/ESS9_LV_SDDF.csv")

write.xlsx(x = ESS9_LV_sample, file = "results/sent/ESS9_LV_sample.xlsx")
write.xlsx(x = ESS9_LV_SDDF, file = "results/sent/ESS9_LV_SDDF.xlsx")
write.xlsx(x = list(ESS9_LV_STRATEX1, ESS9_LV_STRTVAL1, ESS9_LV_STRTVAL2,
                    ESS9_LV_PSU, ESS9_LV_SSU),
           file = "results/sent/ESS9_LV_variable_recode.xlsx")

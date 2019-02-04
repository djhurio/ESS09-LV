# ESS9 Sampling VZD data
# Register of Enterprises

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)

# Packages
require(data.table)

# Reset ####
rm(list = ls())
gc()


# Metadata ####
metadat <- fread(input = "http://dati.ur.gov.lv/register/register.txt",
                 sep = "", skip = 2, header = F, fill = T,
                 colClasses = "character")

metadat[, V1 := gsub("\t+", "\t", V1)]
metadat[, c("name", "description") := tstrsplit(V1, "\t")]
metadat[, V1 := NULL]

# Data ####
dat <- fread(input = "http://dati.ur.gov.lv/register/register.csv",
             na.strings = "NA", colClasses = "character")

dat[, .N]

sapply(dat, class)

dat[, .N, keyby = .(terminated != "", closed)]

dat <- dat[terminated == ""]

dat[, .N, keyby = .(closed)]
dat[, .N, keyby = .(reregistration_term)]

dat[, .N, keyby = .(grepl("^[0-9]{9}$", addressid))]

dat[!grepl("^[0-9]{9}$", addressid)]
dat[grepl("^[0-9]{9}$", addressid)]

dat <- dat[grepl("^[0-9]{9}$", addressid)]
dat

dat[, .N, keyby = .(addressid)][order(-N)]

dat[, .N, keyby = .(regtype, regtype_text)]

dat[, .N, keyby = .(type, type_text)]

dat_UR <- dat[, .(uzn_sk = .N), keyby = .(addressid)]
dat_UR

save(dat_UR, file = "data/UR/dat_UR.Rdata")

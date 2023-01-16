# ESS9 Sampling VZD data

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)

# Packages
require(data.table)

# Reset ####
rm(list = ls())
gc()


# Test for usage of the open VZD data
# https://data.gov.lv/dati/lv/dataset/valsts-adresu-registra-informacijas-sistemas-atvertie-dati/resource/1d3cbdf2-ee7d-4743-90c7-97d38824d0bf

dir.data <- "~/data/VZD/csv"
dir.create(path = dir.data, recursive = T, showWarnings = F)

download.file(
  url = "https://data.gov.lv/dati/dataset/0c5e1a3b-0097-45a9-afa9-7f7262f3f623/resource/1d3cbdf2-ee7d-4743-90c7-97d38824d0bf/download/aw_csv.zip",
  destfile = "~/data/VZD/aw_csv.zip"
)

unzip(zipfile = "~/data/VZD/aw_csv.zip", exdir = dir.data)


# setwd(dir.data)
getwd()

flist <- list.files(path = dir.data, pattern = "CSV$", full.names = T)
flist
system2(command = "file", args = flist)

namelist <- tolower(gsub(".*/|.CSV", "", flist))
namelist

# readlist <- paste("iconv -f ISO-8859-13 -t UTF-8", file.path(getwd(), flist))
# readlist

# tmp <- fread(file = flist[1], quote = "#", encoding = "UTF-8")
# tmp
# 
# tmp <- fread(file = flist[1], quote = "#", encoding = "Latin-1")
# tmp
# 
# tmp <- fread("iconv -f ISO-8859-13 -t UTF-8 /home/djhurio/Dropbox/Darbs/ESS9-LV-2018-2019/ESS9-LV/data/VZD/AK_20181211/csv/AW_CIEMS.CSV",
#              quote = "#")
# tmp

# dat <- fread(cmd = readlist[1], quote = "#")
dat <- fread(file = flist[1], quote = "#")

# dat <- lapply(readlist, function(x) fread(cmd = x, quote = "#",
#                                           colClasses = "character",
#                                           integer64 = "character"))
dat <- lapply(flist, function(x) fread(file = x, quote = "#",
                                       colClasses = "character",
                                       integer64 = "character"))

length(dat)

names(dat) <- namelist

list2env(dat, globalenv())

rm(dat)
gc()

aw_ternames <- c("aw_rajons", "aw_pilseta", "aw_novads",
                 "aw_pagasts", "aw_ciems", "aw_iela")
aw_ter <- rbindlist(mget(aw_ternames), use.names = T, fill = T)
rm(list = aw_ternames)

# aw_geonames <- c("aw_eka_geo", "aw_vieta_centroid_geo")
aw_geonames <- c("aw_vietu_centroidi")
aw_geo <- rbindlist(mget(aw_geonames), use.names = T)
rm(list = aw_geonames)

aw_dziv
# aw_nlieta
aw_eka

# aw_iela_geo
# aw_vieta_geo

for (aw in list(aw_ter, aw_dziv, aw_eka, aw_geo)) {
  setnames(x = aw, new = tolower(names(aw)))
}
rm(aw)

# save(aw_ter, aw_dziv, aw_nlieta, aw_geo, file = "data/VZD/data-VZD.Rdata")
save(aw_ter, aw_dziv, aw_eka, aw_geo, file = "~/data/VZD/data-VZD.Rdata")

# ESS9 Sampling VZD data

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)

# Packages
require(data.table)

# Reset ####
rm(list = ls())
gc()


# encoding: ISO-8859-13

dir.data <- "data/VZD/AK_20181211/csv"

# setwd(dir.data)
getwd()

flist <- list.files(path = dir.data, pattern = "CSV$", full.names = T)
flist

namelist <- tolower(gsub(".*/|.CSV", "", flist))
namelist

readlist <- paste("iconv -f ISO-8859-13 -t UTF-8", file.path(getwd(), flist))
readlist

# tmp <- fread(file = flist[1], quote = "#", encoding = "UTF-8")
# tmp
# 
# tmp <- fread(file = flist[1], quote = "#", encoding = "Latin-1")
# tmp
# 
# tmp <- fread("iconv -f ISO-8859-13 -t UTF-8 /home/djhurio/Dropbox/Darbs/ESS9-LV-2018-2019/ESS9-LV/data/VZD/AK_20181211/csv/AW_CIEMS.CSV",
#              quote = "#")
# tmp

dat <- fread(cmd = readlist[1], quote = "#")

dat <- lapply(readlist, function(x) fread(cmd = x, quote = "#",
                                          colClasses = "character",
                                          integer64 = "character"))

length(dat)

names(dat) <- namelist

list2env(dat, globalenv())

rm(dat)
gc()

aw_ternames <- c("aw_rajons", "aw_pilseta", "aw_novads",
                 "aw_pagasts", "aw_ciems", "aw_iela")
aw_ter <- rbindlist(mget(aw_ternames), use.names = T)
rm(list = aw_ternames)

aw_geonames <- c("aw_eka_geo", "aw_vieta_centroid_geo")
aw_geo <- rbindlist(mget(aw_geonames), use.names = T)
rm(list = aw_geonames)

aw_dziv
aw_nlieta

aw_iela_geo
aw_vieta_geo

save(aw_ter, aw_dziv, aw_nlieta, aw_geo, file = "data/VZD/data-VZD.Rdata")

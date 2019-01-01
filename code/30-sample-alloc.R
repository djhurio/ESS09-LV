# ESS9 LV Sample Allocation

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)

# Packages
require(data.table)
require(openxlsx)

# Reset ####
rm(list = ls())
gc()


# NUTS-3 regions ####

nuts3 <- fread("data/nuts_10.04.2014.csv", dec = ",",
               encoding = "UTF-8", colClasses = "character")

setnames(nuts3, 1:4, c("nuts3_code", "nuts3_name", "lau2_code", "lau2_name"))

nuts3 <- nuts3[lau2_code != "", .(nuts3_code, nuts3_name, lau2_code, lau2_name)]

nuts3[grep("Mērsraga", lau2_name)]
nuts3[grep("Mērsraga", lau2_name), lau2_code := "0887600"]

nuts3[grep("Rojas", lau2_name)]
nuts3[grep("Rojas", lau2_name), lau2_code := "0888301"]

nuts3[, .N, keyby = .(nuts3_code, nuts3_name)]

setkey(nuts3, lau2_code)



# ATVK ####

atvk <- fread("data/ATVK.csv", encoding = "UTF-8", colClasses = "character")

setnames(atvk, c("ter_name", "ter_code", "lau2_code", "level", "description"))

atvk[, description := NULL]

atvk[, .N, keyby = .(level)]

atvk[, c("name", "type") := tstrsplit(ter_name, " ")]

atvk[, .N, keyby = .(type)]

atvk[is.na(type)]
atvk[type == "novads"]
atvk[type == "pilsēta"]
atvk[type == "pagasts"]

atvk[lau2_code == ""]
atvk[lau2_code == "", lau2_code := ter_code]

atvk[, n := .N, by = .(lau2_code)]

# Novadi bez pilsētām un pagastiem
atvk[n == 1 & !is.na(type)]

atvk <- atvk[n == 1 | type == "pilsēta" | type == "pagasts"]

# Stratifikācija
atvk[is.na(type), ter_type := 1L]
atvk[type == "pilsēta", ter_type := 2L]
atvk[type == "pagasts" | type == "novads", ter_type := 3L]

atvk[, .N, keyby = .(ter_type, type)]

atvk <- atvk[, .(ter_code, ter_name, lau2_code, level, ter_type)]

atvk <- merge(atvk, nuts3, by.x = "lau2_code", by.y = "lau2_code", all.x = T)

atvk[, .N, keyby = .(nuts3_code, nuts3_name, ter_type)]

dcast(atvk, nuts3_code + nuts3_name ~ ter_type)


# RIG010. Pastāvīgo iedzīvotāju skaits pēc dzimuma un vecuma statistiskajos
# reģionos, republikas pilsētās, novados, novadu pilsētās, pagastos, ciemos un
# Rīgas apkaimēs (atbilstoši robežām 2018. gada sākumā)

dat1 <- fread("http://data.csb.gov.lv/sq/21876", encoding = "UTF-8")

dat1[, ter := sub(" ", ";", `Visas teritorijas`)]

dat1[, c("ter_code", "ter_name") := tstrsplit(ter, split = ";")]
dat1[, ter_code := sub("^LV", "", ter_code)]

dat1[, .N, keyby = .(nchar(ter_code))]

dat1[nchar(ter_code) == 7]

dat1 <- dat1[ter_code %in% atvk$ter_code]

grep("Skaits", names(dat1), value = T)

dat1 <- melt(dat1, id.vars = "ter_code",
             measure.vars = grep("Skaits", names(dat1), value = T))

dat1 <- dat1[, .(pop_pers15p = sum(as.integer(value))), keyby = .(ter_code)]

dat1[, sum(pop_pers15p)]


pop_table <- merge(atvk, dat1, by = "ter_code", all.x = T)

setcolorder(pop_table, c("ter_code", "ter_name", "ter_type", "level",
                         "lau2_code", "lau2_name",
                         "nuts3_code", "nuts3_name", "pop_pers15p"))

pop_table[is.na(pop_pers15p)]
pop_table[pop_pers15p == 0]
pop_table[order(pop_pers15p)]

pop_table[, ter_type := factor(ter_type, 1:3, c("cities", "towns", "rural"))]

pop_table[unique(lau2_code)]

# strata_table <- dcast(pop_table, nuts3_code + nuts3_name ~ ter_type,
#                       value.var = "pop_pers15p", fun.aggregate = sum)
# 
# write.xlsx(strata_table, file = "results/strata-table.xlsx")

strata_table <- pop_table[, lapply(.SD, sum), .SDcols = "pop_pers15p",
                          keyby = .(nuts3_code, nuts3_name, ter_type)]

strata_table[, prop := prop.table(pop_pers15p)]

strata_table[, n_psu := round(505 * prop)]

strata_table[, n_ssu := n_psu * 5]

strata_table[, lapply(.SD, sum), .SDcols = c("n_psu", "n_ssu")]


# Save ####

save(pop_table, file = "results/pop_table.Rdata")
write.xlsx(pop_table, file = "results/pop_table.xlsx")

save(strata_table, file = "results/strata_table.Rdata")
write.xlsx(strata_table, file = "results/strata_table.xlsx")

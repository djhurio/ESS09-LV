# Mājsaimniecības ienākumu deciļu novērtējums ar EU-SILC PUF datiem

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)
# options("openxlsx.numFmt" = "# ##0")
options("openxlsx.numFmt" = "")

# Packages
require(data.table)
require(eurostat)
require(ggplot2)
require(openxlsx)
require(laeken)


# Reset ####
rm(list = ls())
gc()


# Load PUF data ####

list.files <- paste0(2014:2017, ".zip")

list.url <- file.path("https://www.csb.gov.lv/sites/default/files/PUF/SILC",
                      list.files)

# Atslēdzu download, lai nelādē failus katru reizi
# mapply(download.file, list.url, file.path("data", list.files))

lapply(file.path("data", list.files), unzip, junkpath = T, exdir = "data")


# Mājsaimniecību dati ####
tmp <- list.files("data", pattern = "H.*csv", full.names = T)
dat_H <- lapply(tmp, fread)
dat_H <- rbindlist(dat_H, use.names = T)

# Personu dati ####
tmp <- list.files("data", pattern = "R_P.*csv", full.names = T)
dat_P <- lapply(tmp, fread)
dat_P <- rbindlist(dat_P, use.names = T)


# Deciles ####
probs <- seq(.1, .9, .1)


# Add household income HY020 to the person data
tmp <- dat_H[, .(HB010, HB030, HY020)]

dat_P2 <- merge(dat_P, tmp,
                by.x = c("RB010", "HB030"),
                by.y = c("HB010", "HB030"))

# Filter persons aged 16+
dat_P2[, .N, keyby = .(vec_gr, RB245)]
dat_P2 <- dat_P2[RB245 == 1]
dat_P2[, .N, keyby = .(vec_gr, RB245)]


# Transform to long format

names(dat_P2)

# RB010: APSEKOJUMA GADS
# HB030: MĀJSAIMNIECĪBAS IDENTIFIKACIJAS NUMURS (anonimizēts)
# RB030: PERSONAS IDENTIFIKACIJAS NUMURS (anonimizēts)
# RB050: PERSONAS SVĒRUMS

# HY020: KOPĒJAIS MĀJSAIMNIECĪBAS RĪCĪBĀ ESOŠAIS IENĀKUMS (anonimizēts)
# PY010_050N: DARBA ŅĒMĒJA IENĀKUMI un NAUDAS IENĀKUMI NO PAŠNODARBINĀTĪBAS
#   (neto)
# PY100N: VECUMA PENSIJAS (neto)
# PY090_110_120_130_140N: BEZDARBNIEKU, APGĀDNIEKA ZAUDĒJUMA, SLIMĪBAS,
#   INVALIDITĀTES UN AR IZGLĪTĪBU SAISTĪTI PABALSTI (neto)

inc_var_names <- c("HY020", "PY010_050N", "PY100N", "PY090_110_120_130_140N")

# Type convertation to double
dat_P2[, lapply(.SD, class), .SDcols = inc_var_names]
dat_P2[, c(inc_var_names) := lapply(.SD, as.double), .SDcols = inc_var_names]
dat_P2[, lapply(.SD, class), .SDcols = inc_var_names]

dat_P3 <- melt(dat_P2, id.vars = c("RB010", "HB030", "RB030", "RB050"),
               measure.vars = inc_var_names)

dat_P3[, variable := sub("HY020", "majs_ien", variable)]
dat_P3[, variable := sub("PY010_050N", "nodarb_ien_neto", variable)]
dat_P3[, variable := sub("PY100N", "vec_pens_neto", variable)]
dat_P3[, variable := sub("PY090_110_120_130_140N", "pab_neto", variable)]

dat_P3[, .N, keyby = .(RB010, variable)]
dat_P3[, .N, keyby = .(RB010, variable, value > 0)]

dat_P3[value < 0, .N, keyby = .(variable, RB010)]
dat_P3[value == 0, .N, keyby = .(variable, RB010)]

tab2a <- dat_P3[grepl("^H", variable) | value > 0,
                as.list(weightedQuantile(value, RB050, probs)),
                keyby = .(variable, RB010)]
tab2b <- dat_P3[grepl("^H", variable) | value > 0,
                .(M = weightedMean(value, RB050)),
                keyby = .(variable, RB010)]
tab2 <- merge(tab2a, tab2b)

# DSG10. Bruto darba ienākumu deciles ####

DSG10 <- fread("http://data.csb.gov.lv/sq/21964", encoding = "UTF-8")

DSG10 <- DSG10[Sektors == "PAVISAM"]
DSG10[, Sektors := NULL]

DSG10[, variable := "nodarb_ien_bruto"]

setnames(DSG10, "Gads", "RB010")
setnames(DSG10, "Decembris vidējie aritmētiskie mēneša ienākumi", "M")
setnames(DSG10, grep("decile", names(DSG10)), paste0("V", 1:9))

tab2c <- rbindlist(list(tab2, DSG10[RB010 >= 2013]), use.names = T,
                   idcol = "source")
tab2c[, source := factor(source, 1:2, c("EU-SILC", "Darba samaksa"))]



# Reformat to long format ####

tab2long <- melt(tab2c, id.vars = c("source", "variable", "RB010"),
                 variable.name = "decile")
tab2long[, decile := sub("V", "D", decile)]
tab2long[, pop := "P(16+)"]
tab2long

tab2long[source == "Darba samaksa", value := value * 12]






# Forecast ####

# Population: Persons 16+
tab2long[, .N, keyby = .(source, variable)]

tab2long[source == "EU-SILC", year := RB010 - 1L]
tab2long[source == "Darba samaksa", year := RB010]
tab2long[, .N, keyby = .(source, RB010, year)]

tab2long[, time := year - min(year) + 1L]
tab2long[, .N, keyby = .(source, RB010, year, time)]

setorder(tab2long, variable, decile, time)

# ggplot(tab2long, aes(x = year, y = value,
#                      colour = decile, group = decile)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~ variable, scales = "free_y") +
#   theme_bw()

cor_table <- tab2long[, .(cor00 = cor(value, time),
                          cor01 = cor(value, log(time)),
                          cor10 = cor(log(value), time),
                          cor11 = cor(log(value), log(time))),
                      keyby = .(variable, decile)]
cor_table <- melt(cor_table, id.vars = c("variable", "decile"),
                  variable.name = "cor")

cor_table[, max_cor := max(abs(value)), by = .(variable, decile)]
cor_table <- cor_table[abs(value) == max_cor]
setorder(cor_table, variable, decile)

# cor_table[cor == "cor00", model := "value ~ 1 + time"]
# cor_table[cor == "cor01", model := "value ~ 1 + log(time)"]
# cor_table[cor == "cor10", model := "log(value) ~ 1 + time"]
# cor_table[cor == "cor11", model := "log(value) ~ 1 + log(time)"]

dcast(cor_table, variable ~ cor)

cor_table[grep("majs|nodarb_ien_neto", variable),
          model := "log(value) ~ log(time)"]
cor_table[-grep("majs|nodarb_ien_neto", variable),
          model := "log(value) ~ time"]

# cor_table[value < 0, model := sub("\\+", "-", model)]

# Modeling ####

setorder(tab2long, variable, decile, time)

dat.list <- split(tab2long, by = c("variable", "decile"))

all.equal(names(dat.list), cor_table[, paste(variable, decile, sep = ".")])

mod.list <- mapply(glm, data = dat.list, formula = cor_table$model,
                   MoreArgs = list(family = gaussian), SIMPLIFY = F)
names(mod.list)

tab_coef <- rbindlist(lapply(mod.list,
                             function(x) as.data.table(coef(summary(x)),
                                                       keep.rownames = T)),
                      idcol = T)

tab_coef[, .N, keyby = .(`Pr(>|t|)` < .05)]
tab_coef[`Pr(>|t|)` > .05]

max_p <- tab_coef[, .(max_p = max(`Pr(>|t|)`)), keyby = .(.id)]

dummy.data <- tab2long[, .(time = c(sort(unique(time)), max(time) + 1),
                           pop = unique(pop))]
dummy.data[, year := min(tab2long$year) + time - 1L]
dummy.data

tmp <- lapply(mod.list,
              function(x) data.table(dummy.data,
                                     value = exp(predict(x, dummy.data))))
pred_glm <- rbindlist(tmp, idcol = T)
pred_glm <- merge(pred_glm, max_p, by = ".id")

pred_glm[, c("variable", "decile") := tstrsplit(.id, "\\.")]
pred_glm[, .id := NULL]


# Bind
tab2long[, method := "data"]
pred_glm[, method := "glm"]

# tab2long[, max_p := 0L]

tab2long_glm <- rbindlist(list(tab2long, pred_glm), fill = T)

pl_glm <- ggplot(tab2long_glm, aes(x = year, y = value,
                                   colour = decile,
                                   linetype = method)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  theme_bw() +
  ggtitle("Quantiles for yearly income with prediction [glm]",
          "LV, EUR, Population: Persons (16+)")
pl_glm

ggsave("results/dec-forecast-LV.pdf", pl_glm,
       device = "pdf", width = 16, height = 9)

# sink(file = "results/model-summary.txt")
# lapply(mod.list, summary.glm)
# sink()

tab3_glm <- dcast(tab2long_glm, variable + year + decile ~ method,
                  value.var = "value")

tab3_glm[, data_men := data / 12]
tab3_glm[, glm_men := glm / 12]
tab3_glm

tab_glm_men <- dcast(tab3_glm, year + decile ~ variable,
                     value.var = c("glm_men", "data_men"))

write.xlsx(list(prediction = tab3_glm,
                tab_glm_men = tab_glm_men,
                mod_coef = tab_coef),
           file = "results/dec-forecast-LV.xlsx", firstRow = T)

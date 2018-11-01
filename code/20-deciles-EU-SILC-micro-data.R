# Mājsaimniecības ienākumu deciļu novērtējums ar EU-SILC PUF datiem

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)
options("openxlsx.numFmt" = "# ##0")

# Packages
require(data.table)
require(eurostat)
require(ggplot2)
require(openxlsx)
require(laeken)


# Reset ####
rm(list = ls())
gc()


# Load data ####

list.files <- paste0(2014:2017, ".zip")

list.url <- file.path("https://www.csb.gov.lv/sites/default/files/PUF/SILC",
                      list.files)

mapply(download.file, list.url, file.path("data", list.files))

lapply(file.path("data", list.files), unzip, junkpath = T, exdir = "data")





# Mājsaimniecību dati ####
tmp <- list.files("data", pattern = "H.*csv", full.names = T)
dat_H <- lapply(tmp, fread)
dat_H <- rbindlist(dat_H, use.names = T)

# Personu dati ####
tmp <- list.files("data", pattern = "R_P.*csv", full.names = T)
dat_P <- lapply(tmp, fread)
dat_P <- rbindlist(dat_P, use.names = T)



# HH Estimates ####
dat_H[, sum(DB090), keyby = .(HB010)]

probs <- seq(.1, .9, .1)

# Bez svariem
dat_H[, as.list(quantile(HY020, probs = probs)), keyby = .(HB010)]
dat_H[, as.list(weightedQuantile(HY020, probs = probs)), keyby = .(HB010)]

# Ar svariem
dat_H[, as.list(weightedQuantile(HY020, weights = DB090, probs = probs)),
    keyby = .(HB010)]

dat_H[, as.list(weightedQuantile(EQ_INC20, weights = DB090, probs = probs)),
    keyby = .(HB010)]


# Test 2: mean income ####

tab1 <- dat_H[, sum(HY020 * DB090) / 12 / sum(DB090), keyby = .(HB010, kvpc.i)]
dcast(tab1, HB010 ~ kvpc.i)

tab2 <- fread("http://data.csb.gov.lv/sq/21701", skip = "Gads", encoding = "UTF-8")
dcast(tab2, Gads ~ `Kvintiļu grupas`,
      value.var = "IIG09. Mājsaimniecību rīcībā esošie ienākumi kvintiļu grupās (euro, mēnesī)")



# P Estimates ####
dat_P[, sum(RB050), keyby = .(RB010)]

probs <- seq(.1, .9, .1)

# Bez svariem
dat_P[, class(EQ_INC20)]
dat_P[, .(EQ_INC20)]
dat_P[, EQ_INC20 := as.double(EQ_INC20)]

dat_P[, as.list(quantile(EQ_INC20, probs = probs)), keyby = .(RB010)]
dat_P[, as.list(weightedQuantile(EQ_INC20, probs = probs)), keyby = .(RB010)]

# Ar svariem
dat_P[, as.list(weightedQuantile(EQ_INC20, weights = RB050, probs = probs)),
      keyby = .(RB010)]


# Tests: ekvivalentie rīcībā esošie ienākumi

# PUF estimates
tab1 <- dat_P[, as.list(weightedQuantile(EQ_INC20, weights = RB050,
                                         probs = probs)),
              keyby = .(RB010)]

tab1 <- melt(tab1, id.vars = "RB010", value.name = "values",
             variable.factor = F)

tab1[, quantile := paste0("D", substr(variable, 2, 2))]

tab1[, dtsrc := "PUF"]

tab1

unique(tab1$RB010)

# Eurostat estimates
tab2 <- get_eurostat("ilc_di01")
setDT(tab2)

tab2[, RB010 := year(time)]

tab2 <- tab2[grepl("D[1-9]", quantile) & indic_il == "TC" & currency == "EUR" &
               geo == "LV" & RB010 %in% unique(tab1$RB010)]

tab2[, dtsrc := "Eurostat"]

tab1[, .(RB010, quantile, values, dtsrc)]
tab2[, .(RB010, quantile, values, dtsrc)]

tab3 <- rbindlist(list(tab1[, .(RB010, quantile, values, dtsrc)],
                       tab2[, .(RB010, quantile, values, dtsrc)]))
tab3

pl1 <- ggplot(tab3, aes(x = RB010, y = values,
                        colour = quantile, linetype = dtsrc)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_continuous("Year") +
  scale_y_continuous("Equivalised Household Income") +
  ggtitle("Distribution of Income by Quantiles - EU-SILC survey [ilc_di01]",
          "LV, EUR, Population: Persons")
pl1

pl2 <- ggplot(tab3, aes(x = quantile, y = values, fill = dtsrc)) +
  geom_col(position = "dodge") +
  facet_grid(RB010 ~ .) +
  theme_bw() +
  scale_x_discrete("Decile") +
  scale_y_continuous("Equivalised Household Income") +
  ggtitle("Distribution of Income by Quantiles - EU-SILC survey [ilc_di01]",
          "LV, EUR, Population: Persons")
pl2

tab4 <- dcast(tab3, RB010 + quantile ~ dtsrc, value.var = "values")
tab4[, diff := PUF - Eurostat]

pdf(file = "results/deciles-test.pdf", width = 16, height = 9)
pl1
pl2
dev.off()

write.xlsx(tab4, file = "results/deciles-test.xlsx")




# Estimation ####

dat_H[, as.list(weightedQuantile(HY020, weights = DB090, probs = probs)),
      keyby = .(HB010)]

dat_H[, dec_grp := cut(HY020, breaks = weightedQuantile(HY020, weights = DB090,
                                                        probs = seq(0, 1, .1)),
                       labels = F, include.lowest = T), by = .(HB010)]
dat_H[, .N, keyby = .(HB010, dec_grp)]

tmp <- dat_H[, .(N = sum(DB090)), keyby = .(HB010, dec_grp)]
tmp[, P := round(prop.table(N), 3), by = .(HB010)]
tmp


tmp <- dat_H[, .(HB010, HB030, HY020)]

dat_P2 <- merge(dat_P, tmp,
                by.x = c("RB010", "HB030"),
                by.y = c("HB010", "HB030"))

dat_P2[, .N, keyby = .(vec_gr, RB245)]

dat_P2 <- dat_P2[RB245 == 1]

dat_P2[, as.list(weightedQuantile(HY020, weights = RB050, probs = probs)),
       keyby = .(RB010)]

dat_P2[, dec_grp := cut(HY020, breaks = weightedQuantile(HY020, weights = RB050,
                                                        probs = seq(0, 1, .1)),
                       labels = F, include.lowest = T), by = .(RB010)]
dat_P2[, .N, keyby = .(RB010, dec_grp)]

dat_P2[, .(N = sum(RB050)), keyby = .(RB010, dec_grp)]

tmp <- dat_P2[, .(N = sum(RB050)), keyby = .(RB010, dec_grp)]
tmp[, P := round(prop.table(N), 3), by = .(RB010)]
tmp

tmp <- dat_P2[, .(pers_n = sum(RB245 == 1)), keyby = .(RB010, HB030)]
tmp

dat_P2[, .(mean_inc = sum(HY020 * RB050) / sum(RB050) / 12),
       keyby = .(RB010, dec_grp)]

ggplot(dat_P2, aes(x = HY020 / 12, colour = factor(RB010))) +
  geom_density() +
  facet_wrap(dec_grp ~ ., scales = "free") +
  theme_bw()


dat_H2 <- merge(dat_H, tmp,
                by.x = c("HB010", "HB030"),
                by.y = c("RB010", "HB030"))

dat_H2[, mean(pers_n), keyby = .(HB010, dec_grp)]

dcast(dat_H2, HB010 ~ dec_grp, value.var = "pers_n", fun.aggregate = mean)



tab1 <- dat_H[, as.list(weightedQuantile(HY020, weights = DB090,
                                         probs = probs)), keyby = .(HB010)]
tab1long <- melt(tab1, id.vars = "HB010", variable.name = "decile")
tab1long[, decile := sub("V", "D", decile)]
tab1long[, pop := "H"]
tab1long

tab2 <- dat_P2[RB245 == 1,
               as.list(weightedQuantile(HY020, weights = RB050,
                                        probs = probs)), keyby = .(RB010)]
tab2long <- melt(tab2, id.vars = "RB010", variable.name = "decile")
tab2long[, decile := sub("V", "D", decile)]
tab2long[, pop := "P(16+)"]
setnames(tab2long, "RB010", "HB010")
tab2long

tab3long <- rbindlist(list(tab1long, tab2long))
tab3long

pl3 <- ggplot(tab3long, aes(x = decile, y = value, fill = pop)) +
  geom_col(position = "dodge") +
  facet_grid(HB010 ~ .) +
  theme_bw() +
  ggtitle("Household income deciles (LV)",
          "Estimated from the EU-SILC data")

ggsave("results/dec-est-LV.pdf", pl3, device = "pdf", width = 16, height = 9)

tmp <- dat_H2[, .(pers_n = sum(pers_n * DB090)), keyby = .(HB010, dec_grp)]
tmp[, P := prop.table(pers_n), by = .(HB010)]
tmp



# Forecast ####

# Population: Persons 16+
tab2long

tab2long[, year := HB010 - 1L]

dec <- sort(unique(tab2long$decile))
dec

mod.list <- lapply(dec, function(x) tab2long[decile == x,
                                             glm(log(value) ~ log(year - 2012))])

lapply(mod.list, summary.glm)

dummy.data <- data.table(year = (2013:2018), pop = "P(16+)")
dummy.data

tmp <- lapply(mod.list, function(x) data.table(dummy.data,
                                               value = exp(predict.glm(x, dummy.data))))
pred_glm <- rbindlist(tmp, idcol = "decile")
pred_glm[, decile := paste0("D", decile)]

tab2long[, method := "EU_SILC"]
pred_glm[, method := "glm"]


tab2long_glm <- rbindlist(list(tab2long, pred_glm), fill = T)

pl_glm <- ggplot(tab2long_glm, aes(x = year, y = value,
                                   colour = decile, linetype = method)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_continuous("Year") +
  scale_y_continuous("Equivalised Household Income") +
  ggtitle("Distribution of Income by Quantiles - prediction [lm]",
          "LV, EUR, Population: Persons")
pl_glm

ggsave("results/dec-forecast-LV.pdf", pl_glm,
       device = "pdf", width = 16, height = 9)

sink(file = "results/model-summary.txt")
lapply(mod.list, summary.glm)
sink()

tab3_glm <- dcast(tab2long_glm, year + decile ~ method, value.var = "value")

tab3_glm[, EU_SILC_men := EU_SILC / 12]
tab3_glm[, glm_men := glm / 12]
tab3_glm

write.xlsx(tab3_glm, file = "results/dec-forecast-LV.xlsx", firstRow = T)

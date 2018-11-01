#  Distribution of income by quantiles - EU-SILC survey
# [ilc_di01]

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)

# Packages
require(data.table)
require(eurostat)
require(ggplot2)
require(openxlsx)


# Reset ####
rm(list = ls())
gc()


# Load data ####

dat <- get_eurostat("ilc_di01")
setDT(dat)

dat <- dat[grepl("D[1-9]", quantile) & indic_il == "TC" & currency == "EUR" & geo == "LV"]

dat[, year := year(time) - 1L]

dat

13*9

pl <- ggplot(dat, aes(x = year, y = values, colour = quantile)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous("Pārskata gads", breaks = 2000:2020, minor_breaks = NULL) +
  scale_y_continuous("Ekvivalizētie mājsaimniecības ienākumi",
                     breaks = seq(0, 1e6, by = 1e3)) +
  ggtitle("Distribution of income by quantiles - EU-SILC survey [ilc_di01]",
          "LV, EUR, Top cut-off points")

ggsave("results/deciles.pdf", pl, "pdf")

write.xlsx(dat, file = "results/deciles.xlsx")

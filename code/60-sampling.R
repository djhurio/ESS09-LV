# ESS9-LV Sampling

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


# Functions ####

plot.DT <- function(DT, group = NULL, colour = NULL, size = NULL,
                    title = NULL, subtitle = NULL) {
  ggplot(DT, aes_string(x = "koord_x", y = "koord_y", group = group)) +
    geom_point(aes_string(colour = colour, size = size)) +
    geom_path(linetype = "dashed") +
    coord_fixed() +
    ggtitle(label = title, subtitle = subtitle) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
}

plot.DT.by <- function(DT, by, group = NULL, colour = NULL, size = NULL,
                       title = NULL) {
  by.values <- unique(DT[, get(by)])
  lapply(by.values, function(x) plot.DT(DT = DT[get(by) == x],
                                        group = group, colour = colour,
                                        size = size,
                                        title = title,
                                        subtitle = paste(by, x, sep = ": ")))
}





# Load sample allocation ####

load("results/strata_table.Rdata")
strata_table

strata_table[, stratex1 := paste(nuts3_code, as.integer(ter_type), sep = "_")]
strata_table


# Load frame ####

load("results/frame_majo_3.Rdata")


# 1st stage sampling ####

frame_psu <- frame_majo_3[, .(pop_majo = .N,
                              koord_x = mean(koord_x),
                              koord_y = mean(koord_y)),
                          keyby = .(stratex1, strtval1, strtval2,
                                    stratim1, psu)]
frame_psu

frame_psu[, as.list(summary(pop_majo)), keyby = .(stratex1)]

strata_table[, .(stratex1, n_psu)]

frame_psu <- merge(x = frame_psu, y = strata_table[, .(stratex1, n_psu)],
                   by = "stratex1")
frame_psu

# Probability of selection at the first stage of sampling
frame_psu[, prob1 := n_psu * pop_majo / sum(pop_majo), by = .(stratex1)]

frame_psu[, as.list(summary(prob1)), keyby = .(stratex1)]

tmp <- merge(x = frame_psu[, sum(prob1), keyby = .(stratex1)],
             y = strata_table[, .(stratex1, n_psu)])
tmp[, all.equal(target = V1, current = n_psu)]


# 1st stage sample
set.seed(8)
frame_psu[, sample1 := UPsystematic(pik = prob1), by = .(stratex1)]
frame_psu[, sample1_f := factor(sample1)]
frame_psu

plot.DT.by(DT = frame_psu, by = "stratex1", colour = "sample1_f")

plot.DT.by(DT = frame_psu[sample1 == 1], by = "strtval2",
           group = "strtval1", colour = "strtval1")

frame_psu[, .(stratex1, stratim1, psu, prob1, sample1)]

frame_majo_4 <- merge(x = frame_majo_3,
                      y = frame_psu[, .(stratex1, stratim1, psu, prob1, sample1)],
                      by = c("stratex1", "stratim1", "psu"), sort = F)
frame_majo_4


# 2st stage sample

frame_majo_4[, psu_pop := .N, by = .(psu)]

# Conditional probability of selection at the second stage of sampling
frame_majo_4[, prob2 := sample1 * 5 / psu_pop]
frame_majo_4[sample1 == 1]

frame_majo_4[sample1 == 1, as.list(summary(prob2)), keyby = .(stratex1)]

frame_majo_4[sample1 == 1, as.list(summary(prob1 * prob2))]
frame_majo_4[sample1 == 1, as.list(summary(prob1 * prob2)), keyby = .(strtval1)]
frame_majo_4[sample1 == 1, as.list(summary(prob1 * prob2)), keyby = .(strtval2)]
frame_majo_4[sample1 == 1, as.list(summary(prob1 * prob2)), keyby = .(stratex1)]


# 2nd stage sample
set.seed(10)
frame_majo_4[, sample2 := UPsystematic(pik = prob2), by = .(psu)]
frame_majo_4[sample1 == 1 & sample2 == 1]

tmp <- merge(x = frame_majo_4[, sum(prob2), keyby = .(stratex1)],
             y = strata_table[, .(stratex1, n_ssu)])
tmp[, all.equal(target = V1, current = n_ssu)]

plot.DT.by(DT = frame_majo_4[sample2 == 1], by = "strtval2",
           group = "strtval1", colour = "strtval1")


sample_majo <- frame_majo_4[sample2 == 1]
sample_majo[, idno := .I]


# Save ####

save(frame_majo_4, file = "results/frame_majo_4.Rdata")
save(sample_majo, file = "results/sample_majo.Rdata")

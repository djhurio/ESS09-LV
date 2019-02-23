# ESS9-LV Frame clustering

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)


# Packages ####
require(data.table)
require(openxlsx)
require(ggplot2)
require(rJava)
require(OpenStreetMap)
require(TSP)


# Reset ####
rm(list = ls())
gc()


# Functions ####

sort.DT <- function(DT, method = "arbitrary_insertion") {
  tour <- solve_TSP(ETSP(DT[, .(koord_x, koord_y)]), method = method)
  DT <- DT[tour]
  DT[, i := .I]
  DT
}

sort.DT.by <- function(DT, by, method = "arbitrary_insertion") {
  by.values <- unique(DT[, get(by)])
  rbindlist(lapply(by.values, function(x) sort.DT(DT[get(by) == x])))
}

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

# plot.DT.map <- function(DT) {
#   bbox <- make_bbox(lon, lat, DT, f = 1)
#   base.map <- get_map(location = bbox, maptype = 'roadmap', color = "bw")
#   pl <- ggmap(base.map, extent = "normal") +
#     geom_point(aes(x = lon, y = lat), data = frame_reg, colour = "red") +
#     geom_path(aes(x = lon, y = lat), data = frame_reg, linetype = "dashed") +
#     theme_bw()
#   return(pl)
# }



# Load data ####

load("results/frame_majo_2.Rdata")
frame_majo_2

names(frame_majo_2)

frame_ter <- frame_majo_2[, c(lapply(.SD, mean), .(n_majo = .N)),
                          .SDcols = patterns("koord|lat|lon|osm"),
                          keyby = .(stratex1, strtval1, strtval2, ter_code)]

frame_psu <- frame_majo_2[, c(lapply(.SD, mean), .(n_majo = .N)),
                          .SDcols = patterns("koord|lat|lon|osm"),
                          keyby = .(stratex1, strtval1, strtval2, ter_code, psu)]

frame_eka <- frame_majo_2[, c(lapply(.SD, mean), .(n_majo = .N)),
                          .SDcols = patterns("koord|lat|lon|osm"),
                          keyby = .(stratex1, strtval1, strtval2, ter_code,
                                    psu, adr_kods_eka)]

frame_ter
set.seed(38)
frame_ter <- sort.DT.by(DT = frame_ter, by = "stratex1")
setnames(x = frame_ter, old = "i", new = "i_ter")
frame_ter

frame_psu
set.seed(38)
frame_psu <- sort.DT.by(DT = frame_psu, by = "ter_code")
setnames(x = frame_psu, old = "i", new = "i_psu")
frame_psu

frame_eka
set.seed(38)
frame_eka <- sort.DT.by(DT = frame_eka, by = "psu")
setnames(x = frame_eka, old = "i", new = "i_eka")
frame_eka

plot.DT(DT = frame_ter[strtval2 == 1], group = "strtval1", colour = "stratex1",
        title = "Republikas pilsētas")
plot.DT(DT = frame_ter[strtval2 == 2], group = "strtval1", colour = "stratex1",
        title = "Novadu pilsētas")
plot.DT(DT = frame_ter[strtval2 == 3], group = "strtval1", colour = "stratex1",
        title = "Novadu pagasti")

pl_level1 <- plot.DT.by(DT = frame_ter, by = "strtval2",
                        group = "strtval1", colour = "stratex1",
                        size = "n_majo",
                        title = "1. līmenis: teritorijas")
# pl_level1

pl_level2 <- plot.DT.by(DT = frame_psu, by = "ter_code", size = "n_majo",
                        title = "2. līmenis: PSU")
# pl_level2

pl_level3 <- plot.DT.by(DT = frame_eka, by = "psu", size = "n_majo",
                        title = "3. līmenis: ēkas")
pl_level3[[434]]

# Save plots ####
cairo_pdf(filename = "results/plots_TSP1.pdf", onefile = T,
          width = 16, height = 9)
pl_level1
dev.off()

cairo_pdf(filename = "results/plots_TSP2.pdf", onefile = T,
          width = 16, height = 9)
pl_level2
dev.off()

cairo_pdf(filename = "results/plots_TSP3.pdf", onefile = T,
          width = 16, height = 9)
pl_level3[sort(sample(x = length(pl_level3), size = 50L))]
dev.off()


# Sort frame ####

frame_ter
frame_psu
frame_eka

frame_ter[, .(ter_code, i_ter)]
frame_psu[, .(psu, i_psu)]
frame_eka[, .(adr_kods_eka, i_eka)]

frame_psu_2 <- merge(x = frame_psu[, .(stratex1, ter_code, psu, i_psu)],
                     y = frame_ter[, .(stratex1, ter_code, i_ter)],
                     by = c("stratex1", "ter_code"))
setorder(x = frame_psu_2, stratex1, i_ter, i_psu)
# Order of selection of cluster
frame_psu_2[, stratim1 := 1:.N, by = .(stratex1)]
frame_psu_2
frame_psu_2[, .(.N, max(stratim1)), keyby = .(stratex1)]


frame_eka_2 <- merge(x = frame_eka[, .(stratex1, ter_code, psu, adr_kods_eka, i_eka)],
                     y = frame_psu_2[, .(stratex1, ter_code, psu, stratim1)],
                     by = c("stratex1", "ter_code", "psu"))
setorder(x = frame_eka_2, stratex1, stratim1, i_eka)
frame_eka_2


frame_majo_3 <- merge(x = frame_majo_2,
                      y = frame_eka_2,
                      by = c("stratex1", "ter_code", "psu", "adr_kods_eka"))
frame_majo_3

setorder(frame_majo_3, stratex1, stratim1, i_eka, adrese_sort)
frame_majo_3

# Order of selection of dwelling
frame_majo_3[, stratim2 := 1:.N, by = .(stratex1, psu)]

setorder(frame_majo_3, stratex1, stratim1, stratim2)

frame_majo_3[, i_eka := NULL]
frame_majo_3


# Test
frame_psu_3 <- frame_majo_3[, c(lapply(.SD, mean), .(n_majo = .N)),
                            .SDcols = patterns("koord|lat|lon|osm"),
                            keyby = .(stratex1, strtval1, strtval2, stratim1, psu, nov_code)]
frame_psu_3

pl_psu_sorted <- plot.DT.by(DT = frame_psu_3, by = "strtval2",
                            group = "nov_code", colour = "stratex1",
                            size = "n_majo",
                            title = "PSU diagrammas")

pl_psu_sorted


# Save ####

frame_majo_3

save(frame_majo_3, file = "results/frame_majo_3.Rdata")

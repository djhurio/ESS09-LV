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
require(scclust)
require(rgdal)
require(ICC)
require(sampling)


# Reset ####
rm(list = ls())
gc()


# ATVK ####

file.exists(file.path(getwd(), "data/ATVK_act.csv"))

readlist <- paste("iconv -f ISO-8859-13 -t UTF-8",
                  file.path(getwd(), "data/ATVK_act.csv"))
readlist

dat.ATVK <- fread(cmd = readlist, colClasses = "character")
dat.ATVK[, c("raj_code", "raj_name") := NULL]



# Rīgas apkaimes ####

# Folders / cels uz shape failu
dir.shp <- "data/Apkaimes_shp_05_03_15"

# Shape file info
ogrInfo(dir.shp, layer = sub("\\..{3}$", "", list.files(dir.shp)[1]),
        encoding = "UTF-8", use_iconv = T)

# Rīgas apkaimju shapefile
Riga_shp <- readOGR(dir.shp, layer = sub("\\..{3}$", "", list.files(dir.shp)[1]),
                    encoding = "UTF-8", use_iconv = T)

# plot(Riga_shp)

dim(Riga_shp@data)
head(Riga_shp@data)



# Frame ####

load("results/frame_majo.Rdata")

frame_majo
dat.ATVK

frame_majo_2 <- merge(x = frame_majo, y = dat.ATVK,
                      by.x = "ATVK_code", by.y = "atvk_code",
                      all.x = T, sort = F)

frame_majo_2[, .N, keyby = .(ATVK_L1, nov_code, nov_name)]

frame_majo_2[, .N, keyby = .(nov_code, nov_name)]
frame_majo_2[, .N, keyby = .(reg_code, reg_name)]
frame_majo_2[, .N, keyby = .(pil_lauk, pil_lauk_name)]

frame_majo_2[, .N, keyby = .(reg_code, reg_name, pil_lauk, pil_lauk_name)]


# Stratification ####

frame_majo_2[, strtval1 := reg_code]

frame_majo_2[pil_lauk == "1", strtval2 := 1L]
frame_majo_2[pil_lauk != "1", strtval2 := as.integer(pil_lauk) - 1L]

frame_majo_2[, stratex1 := paste(strtval1, strtval2, sep = "_")]

frame_majo_2[, .N, keyby = .(stratex1, strtval1, strtval2)]



# Riga by localities ####
frame_majo_2

# Rīgas adreses
koord <- frame_majo_2[nov_code == "00100", .(adr_kods, koord_x, koord_y)]

# Pārtaisa par telpisko punktu datu ietvaru, norādot koordinātas
coordinates(koord) <- ~ koord_x + koord_y
# Norāda koordinātu projekciju no shapefile
proj4string(koord) <- proj4string(Riga_shp)

head(koord)

# Sadalam ēkas pa Rīgas apkaimēm
koord_apk <- over(koord, Riga_shp)
dim(koord_apk)
head(koord_apk)

koord_dt <- cbind(as.data.table(koord), as.data.table(koord_apk))
koord_dt

setnames(koord_dt, "gid", "apkaime_gid")
koord_dt[, apkaime_gid := as.integer(apkaime_gid)]

koord_dt[, c("koord_x", "koord_y") := NULL]


# Pievienojam apkaimes pie ēku ietvara
frame_majo_2 <- merge(x = frame_majo_2, y = koord_dt, by = "adr_kods",
                      all.x = T, sort = F)

frame_majo_2[, .N, keyby = .(nov_code == "00100", !is.na(apkaime))]
frame_majo_2[nov_code == "00100", .N, keyby = .(apkaime_gid, apkaime)]
frame_majo_2[nov_code == "00100", .N, keyby = .(apkaime_gid, apkaime)][order(N)]




# Clustering ####
sapply(frame_majo_2, class)

setorder(frame_majo_2, nov_code, apkaime_gid, stratex1)

frame_majo_2[, ID := .I]

frame_majo_2[, .N, keyby = .(strtval2)]

if ("ter_code" %in% names(frame_majo_2)) frame_majo_2[, ter_code := NULL]

# Rīga
frame_majo_2[strtval2 == 1L & nov_code == "00100",
             ter_code := paste("00100", sprintf("%03d", apkaime_gid), sep = "_")]
frame_majo_2[strtval2 == 1L & nov_code == "00100",
             ter_name := paste("Rīga", apkaime, sep = ", ")]

# Republikas nozīmes pilsētas bez Rīgas
frame_majo_2[strtval2 == 1L & nov_code != "00100", ter_code := ATVK_code]
frame_majo_2[strtval2 == 1L & nov_code != "00100", ter_name := atvk_name]

# Novadu pilsētas
frame_majo_2[strtval2 == 2L, ter_code := ATVK_code]
frame_majo_2[strtval2 == 2L, ter_name := atvk_name]

# Novadu pagasti
frame_majo_2[strtval2 == 3L, ter_code := nov_code]
frame_majo_2[strtval2 == 3L, ter_name := nov_name]

frame_majo_2[, .N, keyby = .(ter_code, ter_name)]
58 + 8 + 67 + 110
# 58 Rīgas apkaimes
# 8 + 67 pilsētas
# 110 novadi

frame_majo_2[, .N] / 4e3

tab <- frame_majo_2[, .N, keyby = .(ter_code, ter_name,
                                    stratex1, strtval1, strtval2)]

tab[strtval2 == 1L, PSU.size.min := 200L]
tab[strtval2 == 2L, PSU.size.min := 150L]
tab[strtval2 == 3L, PSU.size.min := 100L]

tab[, .N, keyby = .(strtval2, PSU.size.min)]
tab[, .N, keyby = .(strtval2, strtval1, PSU.size.min)]

tab[, k := ceiling(N / PSU.size.min)]
tab[order(k)]

tab[, min_size := floor(N / k)]
tab
tab[order(min_size)]

tab[, as.list(summary(min_size)), keyby = k == 1]
tab[k == 1]

tab[grep("Engures", ter_name)]

print.tab <- function(x) paste(paste(names(x), x, sep = " = "), collapse = "; ")

f.scclust <- function(i) {
  cat(unlist(tab[i, .(stratex1, ter_code, ter_name)]), "\n")
  arg_stratex1 <- tab[i, stratex1]
  arg_ter_code <- tab[i, ter_code]
  dat <- frame_majo_2[stratex1 == arg_stratex1 & ter_code == arg_ter_code,
                      .(ID, koord_x, koord_y, ATVK_code, atvk_name)]
  
  tmp <- dat[, .N, keyby = .(ATVK_code, atvk_name)]
  dat[, ter := factor(x = ATVK_code,
                      levels = tmp$ATVK_code,
                      labels = tmp$atvk_name)]
  
  dat.dist <- distances(data = dat, id_variable = "ID",
                        dist_variables = c("koord_x", "koord_y"),
                        normalize = "studentize")
  # dim(dat.dist)
  # str(dat.dist)
  # dat.dist[, 1:6]
  
  clust <- sc_clustering(distances = dat.dist,
                         size_constraint = tab[i, min_size])
  # str(clust)
  
  cat("Klasteru skaits:", attr(clust, "cluster_count"), "\n")
  dat[, cluster := as.integer(clust)]
  dat_agg <- dat[, .N, keyby = cluster]
  
  tab[i, cluster_count_f := as.integer(attr(clust, "cluster_count"))]
  tab[i, mean_size_f := round(N / cluster_count_f)]
  tab[i, min_size_f := dat_agg[, min(N)]]
  tab[i, max_size_f := dat_agg[, max(N)]]
  
  cat(unlist(tab[i]), "\n")
  
  # return(list(dat = dat, tab = tab[i], plot.clusters = pl))
  return(list(dat = dat, tab = tab[i]))
}

# Test 1
tab[177]
test <- f.scclust(177)
test[[1]]
test[[2]]

# Test 2
res.clust <- lapply(1:5, f.scclust)

# Run
set.seed(1411)
res.clust <- lapply(1:tab[, .N], f.scclust)

res.dat <- rbindlist(lapply(res.clust, `[[`, 1))
res.tab <- rbindlist(lapply(res.clust, `[[`, 2))

res.tab[order(cluster_count_f)]

res.tab[grep("Ādažu", ter_name)]
res.tab[grep("Engures", ter_name)]

res.tab[order(-max_size_f), .(stratex1, ter_code, ter_name, N, cluster_count_f,
                              mean_size_f, min_size_f, max_size_f)]

res.tab[order(-max_size_f), .(stratex1, ter_code, ter_name, N, cluster_count_f,
                              mean_size_f, min_size_f, max_size_f)][cluster_count_f == 1]

if ("cluster" %in% names(frame_majo_2)) frame_majo_2[, cluster := NULL]
frame_majo_2 <- merge(frame_majo_2, res.dat[, .(ID, cluster)], by = "ID", all.x = T)

frame_majo_2[, summary(cluster)]
frame_majo_2[, psu := paste(stratex1, ter_code, sprintf("%04d", cluster + 1L),
                            sep = "_")]

frame_majo_2[, n_PSU := length(unique(psu)), by = .(ter_code)]
frame_majo_2[n_PSU == 1, .N, keyby = .(ter_code, ter_name)]

setkey(frame_majo_2, psu)

frame_psu <- frame_majo_2[, .N, keyby = .(psu)]

frame_majo_2[, const := T]

# n_psu <- 100
# n_ssu <- 5
draw.sample <- function(id = 1, n_psu = 100, n_ssu = 5) {
  
  # PSU sample
  frame_psu[, prob := inclusionprobabilities(a = N, n = n_psu)]
  frame_psu[, sampled := UPsystematic(prob)]
  
  dat <- frame_majo_2[frame_psu[sampled == 1, psu]]
  dat[, rand := runif(n = .N)]
  setorder(dat, psu, rand)
  dat[, i := 1:.N, by = .(psu)]
  
  tab <- dat[i <= n_ssu, lapply(.SD, ICCbare, x = factor(psu)),
             .SDcols = patterns("^uzn_sk"), by = .(const)]
  tab[, id := id]
  tab[, n_PSU := n_psu]
  tab[, m_SSU := n_ssu]
  return(tab[])
}

draw.sample()

rbindlist(lapply(1:10, draw.sample, n_psu = 100, n_ssu = 10))

run.sim <- function(n, m) rbindlist(lapply(1:10, draw.sample,
                                           n_psu = n, n_ssu = m))
run.sim(10, 10)
run.sim(100, 20)
run.sim(500, 20)


estim_ICC <- rbindlist(lapply(1:100, draw.sample, n_psu = 500, n_ssu = 20))
estim_ICC

qplot(x = uzn_sk, data = estim_ICC, geom = "density")
qplot(x = uzn_sk_dummy, data = estim_ICC, geom = "density")

estim_ICC2 <- melt(data = estim_ICC, id.vars = "id",
                   measure.vars = c("uzn_sk", "uzn_sk_dummy"), value.name = "ICC")
estim_ICC2

tab <- estim_ICC2[, as.list(summary(ICC)), keyby = .(variable)]
write.xlsx(x = tab, file = "doc/ICC-sim.xlsx", colWidths = "auto")

frame_psu <- frame_majo_2[, .N, keyby = .(stratex1, strtval1, strtval2,
                                          reg_code, reg_name,
                                          nov_code, nov_name,
                                          ter_code, ter_name, psu)]
frame_psu[, .N]

tab_psu <- frame_psu[, c(as.list(summary(N)),
                         .(sd = sd(N), N_psu = .N), N_maj = sum(N)),
                     keyby = .(stratex1, strtval1, strtval2,
                               reg_code, reg_name)]
setorder(tab_psu, strtval2, strtval1)
tab_psu

write.xlsx(x = tab_psu, file = "doc/tab-PSU.xlsx", colWidths = "auto")

frame_psu[, c(as.list(summary(N)), .(sd = sd(N), N_psu = .N), N_maj = sum(N)),
          keyby = .(strtval2)]


# PSU plots ####

# LKS koordinātu transformāciju uz Lat / Lon #####

projection <- "+proj=tmerc +lat_0=0 +lon_0=24 +k=0.9996 +x_0=500000 +y_0=-6000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

cord <- project(xy = as.matrix(frame_majo_2[, .(koord_x, koord_y)]),
                proj = projection, inv = T)
head(cord)

frame_majo_2[, c("lat", "lon") := .(cord[, 2], cord[, 1])]



# Plot on OSM map ####

frame_majo_2[, projectMercator(lat = lat, long = lon)]

osm_cord <- frame_majo_2[, projectMercator(lat = lat, long = lon)]
head(osm_cord)

frame_majo_2[, c("osm_x", "osm_y") := .(osm_cord[, 1], osm_cord[, 2])]


# x <- ggmap::make_bbox(lon = lon, lat = lat, data = frame_majo_2)
# map <- openmap(upperLeft = x[c(4, 1)], lowerRight = x[c(2, 3)])
# autoplot.OpenStreetMap(data = map, expand = T)
# 
# plot.map <- function(data = frame_majo_2, lon = lon, lat = lat) {
#   x <- ggmap::make_bbox(lon = lon, lat = lat, data = data)
#   map <- openmap(upperLeft = x[c(4, 1)], lowerRight = x[c(2, 3)])
#   autoplot.OpenStreetMap(data = map, expand = T) +
#     theme(axis.title = element_blank(),
#           axis.text = element_blank(),
#           axis.ticks = element_blank())
# }
# 
# plot.map()
# plot.map(data = frame_majo_2[nov_code == "00100"])
# plot.map(data = frame_majo_2[grep("Ķekavas", nov_name)])

plot.map.points <- function(data, lon, lat, x, y,
                            colour = NULL, label = NULL, subtitle = NULL) {
  bbox <- ggmap::make_bbox(lon = lon, lat = lat, data = data)
  map <- openmap(upperLeft = bbox[c(4, 1)], lowerRight = bbox[c(2, 3)])
  autoplot(data = map, expand = T) +
    geom_point(aes_(x = x, y = y, colour = colour), data = data) +
    scale_color_discrete(name = "Cluster") +
    ggtitle(label = label, subtitle = subtitle) +
    theme_bw() +
    guides(colour = FALSE) +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
}

plot.map.points(data = frame_majo_2[nov_code == "00500"],
                lon = lon, lat = lat, x = ~osm_x, y = ~osm_y)

frame_psu

frame_ter <- frame_psu[, .(N_PSU = .N, N_majo = sum(N)),
                       keyby = .(strtval2, strtval1, stratex1,
                                 ter_code, ter_name)]
frame_ter

plot.ter <- function(i) {
  print(frame_ter[i, .(nr = i, stratex1, ter_code, ter_name)])
  dat <- frame_majo_2[stratex1 == frame_ter[i, stratex1] &
                        ter_code == frame_ter[i, ter_code]]
  plot.map.points(data = dat,
                  lon = lon, lat = lat, x = ~osm_x, y = ~osm_y,
                  colour = ~factor(cluster),
                  label = print.tab(frame_ter[i, .(stratex1, ter_code, ter_name)]),
                  subtitle = print.tab(frame_ter[i, .(N_PSU, N_majo)]))
}

plot.ter(6)

frame_ter[, .I]

res.plots <- lapply(frame_ter[, .I], plot.ter)


# Save data ####

save(frame_majo_2, file = "results/frame_majo_2.Rdata")

save(frame_psu, file = "results/frame_psu.Rdata")
write.xlsx(x = frame_psu, file = "results/frame_psu.xlsx",
           colWidths = "auto")


# Save plots ####
cairo_pdf(filename = "results/PSU_plots_v3.pdf", onefile = T,
          width = 16, height = 9)
res.plots
dev.off()

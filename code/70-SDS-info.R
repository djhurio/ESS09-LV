# ESS9-LV tables and plots for the SDS

# Options ####
options(encoding = "UTF-8")
options(stringsAsFactors = F)


# Packages ####
require(reshape2)
require(data.table)
require(ggplot2)


# Reset ####
rm(list = ls())
gc()


# Load PSU frame ####

load("results/frame_psu.Rdata")

frame_psu

frame_psu[, round(mean(N))]
frame_psu[, round(mean(N)), keyby = .(strtval2)]

tab <- reshape2::dcast(data = frame_psu, formula = strtval1 ~ strtval2,
                       fun.aggregate = function(x) round(mean(x)),
                       value.var = "N", margins = T)
tab

dim(tab)

paste(tab$strtval1, collapse = ", ")

# Kopēt bez rindu nosaukumiem un bez pēdiņām (")
write.table(tab, file = "clipboard", sep = "\t", row.names = F, quote = F,
            fileEncoding = "native.enc")

frame_psu[, strtval2 := factor(strtval2)]

tab2 <- frame_psu[, .(N = mean(N)), keyby = .(strtval1, strtval2)]

pl2 <- ggplot(data = frame_psu,
              mapping = aes(x = N, fill = strtval2, colour = strtval2)) +
  geom_density(alpha = .6) +
  geom_vline(mapping = aes(xintercept = N, colour = strtval2),
             data = tab2, linetype = "dashed") +
  facet_wrap(facets = ~strtval1) +
  theme_bw() +
  labs(x = "Number of dwellings")

ggsave(filename = "results/PSU-size-density.png", plot = pl2,
       device = png(), width = 18, height = 9, units = "cm")

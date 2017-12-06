# inspiré de http://kelpsandthings.org/robert/r/polar-plot-climatologies/

#install imagemagick, make sure to check option for "install legacy component convert.exe etc..)

library(purrr)
library(magrittr)
library(dplyr)
library(readr)
library(tidyverse)
library(viridis)
library(gridExtra)
library(animation)
library(ggplot2)

## import weather data, save clean data to csv.
download.file("http://climate.weather.gc.ca/prods_servs/cdn_climate_summary_report_e.html?intYear=2017&intMonth=11&prov=&dataFormat=csv&btnSubmit=Download+data",
              destfile="data/qc_2017_11.csv")
year <- rep( seq(from = 1840, to = 2017), each= 12)
month <- rep( seq (1:12), times = length(year)/12)
liste <- tibble(year, month)


map2(liste$year,liste$month, 
           ~download.file(
             paste0("http://climate.weather.gc.ca/prods_servs/cdn_climate_summary_report_e.html?intYear=", .x, "&intMonth=", .y, "&prov=&dataFormat=csv&btnSubmit=Download+data"),
                         destfile=paste0("data/qc_",.x , "_", .y , ".csv")))


z <- bind_rows(map2(liste$year,liste$month, 
     ~  read_csv(paste0("data/qc_", .x, "_", .y , ".csv"), skip = 31)  %>%
      mutate(year = .x, month = .y) %>% 
      rename(snow_pct_normal = "S%N",
              precip_pct_normal = "P%N",
              bright_sunshine_pct_normal = "BS%") %>%
       mutate_at( vars(Lat, Long, Tm, DwTm, D, Tx, DwTx, Tn, DwTn, S, DwS,
                       snow_pct_normal, P, DwP, precip_pct_normal, S_G, Pd, BS, DwBS,bright_sunshine_pct_normal, HDD, CDD),
                  funs(as.numeric))))
write_csv(z, "data/alldata.csv")

## prepare for graph
z <- read_csv("data/alldata.csv")
y <- z %>% 
  filter(Prov == "QC") %>%
  group_by(Stn_Name) %>% 
  mutate(miny = min(year), maxy = max(year)) %>% 
  filter(miny<= 1981, maxy >=2010) %>% ## stations avec 30 ans de données pour mes normales.
  group_by(Stn_Name, month) %>%
  mutate(clim = mean(Tm[year %in% seq(1981,2010)], na.rm = T),
         anom = Tm - clim,
         site = Stn_Name) %>%
  ungroup() %>%
  select(site,year,month,anom)
y$month <- month.abb[y$month]

#View(y %>%  distinct(site))
#filter( Stn_Name == "QUEBEC/JEAN LESAGE INTL A") %>% 
  

## Function that creates a polar plot
polar.plot <- function(df, i){
  # Add bridges for polar coordinates
  years <- unique(df$year)[1:i]
  df2 <- filter(df, year %in% years)
  bridges <- df2[df2$month == 'Jan',]
  bridges$year <- bridges$year - 1
  if(nrow(bridges) == 0){
    bridges <- data.frame(site = df2$site[1], year = min(df2$year), month = NA, anom = NA)
  } else {
    bridges$month <- NA
  }
  blanks <- data.frame(site = df2$site[1], expand.grid(year = min(df2$year)-1, month = month.abb), anom = NA)
  # Polar plot
  pp <- ggplot(data = rbind(blanks, df2, bridges), aes(x = month, y = anom, group = year)) +
    # Circular black background
    geom_rect(colour = "black", fill = "black", aes(xmin = "Jan", xmax = NA,
                                                    ymin = min(df$anom, na.rm = T), ymax = max(df$anom, na.rm = T))) +
    # ymin = min(df$anom, na.rm = T), ymax = 3)) +
    # Anomaly threshold labels
    geom_hline(aes(yintercept = 1.0), colour = "red") +
    geom_label(aes(x = "Jan", y = 1.0, label = "1.0°C"),
               colour = "red", fill = "black", size = 3) +
    geom_hline(aes(yintercept = 2.0), colour = "red") +
    geom_label(aes(x = "Jan", y = 2.0, label = "2.0°C"),
               colour = "red", fill = "black", size = 3) +
    geom_hline(aes(yintercept = 3.0), colour = "red") +
    geom_label(aes(x = "Jan", y = 3.0, label = "3.0°C"),
               colour = "red", fill = "black", size = 3) +
    geom_hline(aes(yintercept = 4.0), colour = "red") +
    geom_label(aes(x = "Jan", y = 4.0, label = "4.0°C"),
               colour = "red", fill = "black", size = 3) +
    # Temperature spiral
    geom_path(aes(colour = anom), show.legend = F) +
    geom_point(data = df2[i,], aes(colour = anom), show.legend = F) +
    # Scale corrections
    scale_colour_viridis(limits = c(min(df$anom, na.rm = T), max(df$anom, na.rm = T))) +
    scale_x_discrete(expand = c(0,0), breaks = month.abb) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(min(df$anom, na.rm = T), max(df$anom, na.rm = T))) +
    # Year label
    geom_text(aes(x = "Jan", y = min(df$anom, na.rm = T), label = max(df2$year, na.rm = T)),
              colour = "ivory", size = 8) +
    # Additional tweaks
    ggtitle(paste0(df$site[1]," temperature change (",min(df$year),"-",max(df$year),")")) +
    coord_polar() +
    theme(panel.background = element_rect(fill = "grey20"),
          plot.background = element_rect(fill = "grey20"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # axis.text.x = element_text(colour = "ivory"),
          axis.text.x = element_text(colour = "ivory", angle =
                                       (360/(2*pi)*rev(seq(pi/12, 2*pi-pi/12, len = 12)))+15,
                                     size = 12),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.ticks.length = unit(0, "cm"),
          plot.title = element_text(hjust = 0.5, colour = "ivory", size = 15))
  print(pp)
}

## Create animation of polar plots
animate.polar.plot <- function(df) {
  lapply(seq(1,length(unique(df$year))), function(i) {
    polar.plot(df = df, i = i)
  })
}


# By default 'saveGIF()' outputs to the same folder 
# the script where the code is being run from is located.
# For that reason one may want to manually change the
# working directory beforehand.
# setwd("somewhere else")
system.time(saveGIF(animate.polar.plot(df = y %>% filter(site == "QUEBEC/JEAN LESAGE INTL A")), interval = 0.4, ani.width = 457, 
                    movie.name = "polar_plot_qc.gif")) ## 262 seconds
system.time(saveGIF(animate.polar.plot(df = y %>% filter(site == "KUUJJUAQ A")), interval = 0.4, ani.width = 457, 
                    movie.name = "polar_plot_kuuj.gif")) ## 221 seconds
system.time(saveGIF(animate.polar.plot(df = y %>% filter(site == "MONTREAL/PIERRE ELLIOTT TRUDEAU INTL A")), interval = 0.4, ani.width = 457, 
                    movie.name = "polar_plot_mtl.gif")) ## 183 seconds
# setwd("back to where you were")
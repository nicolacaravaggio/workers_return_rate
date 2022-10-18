# ==============================================================================
# RE-ENTRY TIMES AFTER A WORK TERMINATION
# ------------------------------------------------------------------------------
# Nicola Caravaggio
# ==============================================================================

# Clear all
rm(list = ls())

# Set directory
choose.dir()

# Import libraries
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(gganimate)
library(berryFunctions)
library(av)
library(gifski)

# Import data
df <- read.csv("cum_rientri.csv", header = TRUE)

# Rename columns
df <- df %>% 
  rename(
    anno = periodo_cessazione,
    contratto = fonte_contr_prec,
    t_rientro = t_rientro_3,
    eta = eta_f_prec,
    count = COUNT
  )

df$t_rientro[is.na(df$t_rientro)] <- 367
df$t_rientro <- as.integer(df$t_rientro)
df = df[-1,]

# ------------------------------------------------------------------------------
# Re-entry times: Unisomm, work agencies (2014-2020)
# ------------------------------------------------------------------------------

# Calculate cumulated value for each year
years = unique(unlist(df$anno))
sub_years = vector("list", length(years))
for (i in 1:length(years)) {
  tab = 
    df[(df$anno == years[i]),] %>%
    group_by(t_rientro) %>%
    summarise(
      anno = years[i],
      giorni = sum(count)
    ) %>%
    arrange(t_rientro)
  tab = within(tab, cum_giorni <- Reduce("+", giorni, accumulate = TRUE))
  tab$cum_perc = tab$cum_giorni / max(tab$cum_giorni)
  sub_years[[i]] = tab
}
names(sub_years) = paste(years)

tab <- sub_years[[paste0(years[1])]]
tab = tab[,1]
for (i in 1:length(years)) {
  tab = merge(tab, sub_years[[paste0(years[i])]][,c("t_rientro","cum_perc")], by = "t_rientro", all.x = TRUE)
  names(tab)[names(tab) == "cum_perc"] = paste0(years[i])
  tab <- tab[order(as.integer(tab$t_rientro)),]
  tab <- tab %>% fill(paste0(years[i]))
}

# Rename columns
names(tab)[names(tab) == '1415'] <- "2014-2015"
names(tab)[names(tab) == '1617'] <- "2016-2017"
names(tab)[names(tab) == '1819'] <- "2018-2019"
names(tab)[names(tab) == '2020'] <- "2020"

# Table
under_tab <- tab[tab$t_rientro %in% c(29, 59, 89, 180, 365, 366, 367),]
names(under_tab)[names(under_tab_unisomm) == 't_rientro'] <- "Tempi di rientro"

# Data adjustments
tab <- tab[tab$t_rientro <= 365,]
tab <- melt(setDT(tab), id.vars = c("t_rientro"), variable.name = "anno")
tab$anno <- as.character(tab$anno)
names(tab)[names(tab) == "value"] = "cum_perc"
tab <- na.omit(tab)
tab <- filter(tab, t_rientro != ".")
tab$t_rientro <- as.integer(tab$t_rientro)
tab_unisomm <- tab

# Plot
plot = tab[tab$anno != '2021',] %>%
  ggplot(aes(x = t_rientro, y = cum_perc, group = anno, color = anno))+
  geom_line(alpha = 0.8) + 
  geom_point() +
  ylim(0, 1) +
  labs(title = "Re-entry time after a work termination",
       subtitle = "Share of workers re-entered in the job market after a termination within one year",
       caption = "Source: Universita' degli Studi Roma Tre, Ministry of Labour and Social Policies data (first quarter 2022)",
       y = 'Share of workers', x = 'Days from termination') + 
  theme(
    legend.position = c(0.9, 0.3),
    legend.title = element_blank(),
    plot.title = element_text(size = 12, hjust = 0, vjust = 0, face = "bold", colour = "black"),
    plot.subtitle = element_text(size = 10, hjust = 0, vjust = 0, face = "italic", color = "grey"),
    plot.caption = element_text(hjust = 0, vjust = 0, color = "black"),
  ) +
  transition_reveal(t_rientro) +
  enter_fade() +exit_fade() + ease_aes('sine-in-out')

plot_workers <- animate(plot,
                        fps = 25,
                        duration = 10,
                        end_pause = 5,
                        width = 600, 
                        height = 400)
plot_workers
anim_save("plot_reentry_times.gif")

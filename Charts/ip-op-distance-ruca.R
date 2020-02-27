# library -----------------------------------------------------------------
library(tidyverse)
library(extrafont)
loadfonts()
library(sf)
library(ggrepel)
library(scales)
library(cowplot)

rm(list = ls())

# themes ------------------------------------------------------------------

theme_bar <- theme_bw() +
  theme(panel.grid.major = element_line(color = "grey70", size = 0.1),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.border = element_blank(),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(l = 2)))

theme_line <- theme_bw() +
  theme(legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.text = element_text(margin = margin(l = 2)),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey70", size = 0.1),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.border = element_blank())

theme_sf <- theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(l = 2))
  )

regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  select(5,6) %>%
  unique()

counties.regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"),
         Dem_Desc = ifelse(Name == "Minnesota", "Minnesota", Dem_Desc))

color.ruca <- c("Entirely rural" = "#5CA81F", "Town/rural mix" = "#C7EF99", "Urban/town/rural mix" = "#d8b365", "Entirely urban" = "#a6611a")

color.pr <- c("Northwest" = "#810f7c","Northeast" = "#fe9929", "Central" = "#076324", "Seven County Mpls-St Paul" = "#d8b365", "Southwest" = "#1f78b4", "Southeast" = "#d7301f", "Minnesota" = "black")

color.edr <- c("EDR 1 - Northwest" = "#b3cde3", "EDR 2 - Headwaters" = "#8c96c6", "EDR 3 - Arrowhead" = "#fe9929", "EDR 4 - West Central" = "#8856a7", "EDR 5 - North Central" = "#810f7c", "EDR 6E- Southwest Central" = "#e5f5f9", "EDR 6W- Upper Minnesota Valley" = "#bdc9e1", "EDR 7E- East Central" = "#99d8c9", "EDR 7W- Central" = "#2ca25f", "EDR 8 - Southwest" = "#74a9cf", "EDR 9 - South Central" = "#0570b0", "EDR 10 - Southeast" = "#d7301f", "EDR 11 - 7 County Twin Cities" = "#d8b365", "Minnesota" = "black")


# Import and organize ip and op distances ---------------------------------

master.ip.distances.ruca <- read_csv("Data/Access/Master-ip-distances-ruca.csv") %>%
  mutate(Dem_Desc = fct_relevel(Dem_Desc, "Entirely rural", "Town/rural mix", "Urban/town/rural mix", "Entirely urban"))

master.ip.distances.pr <- read_csv("Data/Access/Master-ip-distances-pr.csv") %>%
  mutate(planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"))

master.ip.distances.edr <- read_csv("Data/Access/Master-ip-distances-edr.csv") %>%
  mutate(planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"))

master.op.distances.ruca <- read_csv("Data/Access/Master-op-distances-ruca.csv") %>%
  mutate(Dem_Desc = fct_relevel(Dem_Desc, "Entirely rural", "Town/rural mix", "Urban/town/rural mix", "Entirely urban"))

master.op.distances.pr <- read_csv("Data/Access/Master-op-distances-pr.csv") %>%
  mutate(planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"))

master.op.distances.edr <- read_csv("Data/Access/Master-op-distances-edr.csv") %>%
  mutate(planning.region = str_replace(planning.region, " Minnesota", ""),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"),
         edr = fct_relevel(edr, "EDR 1 - Northwest", "EDR 2 - Headwaters", "EDR 3 - Arrowhead", "EDR 4 - West Central", "EDR 5 - North Central", "EDR 6E- Southwest Central", "EDR 6W- Upper Minnesota Valley", "EDR 7E- East Central", "EDR 7W- Central", "EDR 8 - Southwest", "EDR 9 - South Central", "EDR 10 - Southeast", "EDR 11 - 7 County Twin Cities", "Minnesota"))


# IP chart ruca -----------------------------------------------------------

ggplot(master.ip.distances.ruca, aes(Year, mean.miles, color = Dem_Desc)) +
  geom_smooth(se = FALSE, size = 2) +
  geom_point(size = 3) +
  facet_wrap(~service.line, ncol = 2) +
  labs(x="", y = "", color="", title = "Average miles traveled for inpatient service")+
  geom_label_repel(data = filter(master.ip.distances.ruca, Year == 2019), aes(x = Year, y = mean.miles, label = paste(comma(mean.miles), " miles", sep = "")), show.legend = FALSE) +
  scale_y_continuous(labels=scales::comma)+
  scale_x_continuous(breaks = seq(1900, 2050, 2)) +
  theme_line+
  scale_color_manual(values= color.ruca,
                     guide = guide_legend(ncol = 3)) +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        panel.spacing.y = unit(2, "lines"))

ggsave(filename = "Charts/ip-distance-ruca.png", type = "cairo", dpi = "print", width = 6.5, height = 6)

# OP chart ruca -----------------------------------------------------------

ggplot(master.op.distances.ruca, aes(Year, mean.miles, color = Dem_Desc)) +
  geom_smooth(se = FALSE, size = 2) +
  geom_point(size = 3) +
  facet_wrap(~type, ncol = 2) +
  labs(x="", y = "", color="", title = "Average miles traveled for outpatient service")+
  geom_label_repel(data = filter(master.op.distances.ruca, Year == 2019), aes(x = Year, y = mean.miles, label = paste(comma(mean.miles), " miles", sep = "")), show.legend = FALSE) +
  scale_y_continuous(labels=scales::comma)+
  scale_x_continuous(breaks = seq(1900, 2050, 2)) +
  theme_line+
  scale_color_manual(values= color.ruca,
                     guide = guide_legend(ncol = 3)) +
  theme(legend.position = "bottom",
        text = element_text(size = 15),
        panel.spacing.y = unit(2, "lines"))

ggsave(filename = "Charts/op-distance-ruca.png", type = "cairo", dpi = "print", width = 6.5, height = 7)


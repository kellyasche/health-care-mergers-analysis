
# library -----------------------------------------------------------------


library(tidyverse)
library(extrafont)
loadfonts()
library(sf)
library(ggrepel)
library(scales)

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


# import and organize payer mix ruca --------------------------------------

master.har <- read_csv("Data/HAR/Master-har.csv") %>%
  rename(year = rpt_year,
         `Hospital Name` = 5) %>%
  mutate(planning.region = str_replace(planning.region, " Minnesota", ""),
         edr = str_replace(edr, "  ", " "))

har.ma <- master.har %>%
  filter(code %in% c(7250, 7253, 7256, 7259, 850),
         hccis_id != 145) %>%
  mutate(value = as.numeric(value),
         value = ifelse(is.na(value), 0, value),
         code = ifelse(code == 7250, "medicare",
                       ifelse(code == 7253, "ma.pmap", 
                              ifelse(code == 7256, "mncare",
                                     ifelse(code == 7259, "commercial", "total.patient.charges")))))

har.ma.each <- har.ma %>%
  filter(hccis_id != 91) %>%
  group_by(year, `Hospital Name`, Dem_Desc, planning.region) %>%
  summarise(public.charges = sum(value[code %in% c("medicare", "ma.pmap", "mncare")], na.rm = TRUE),
            total.patient.charges = sum(value[code == "total.patient.charges"], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct.public.charges = public.charges / total.patient.charges,
         `Hospital Name` = str_replace(`Hospital Name`, "'", "")) %>%
  mutate(Dem_Desc = fct_relevel(Dem_Desc, "Entirely rural", "Town/rural mix", "Urban/town/rural mix", "Entirely urban"),
         planning.region = fct_relevel(planning.region, "Northwest", "Northeast", "Central", "Seven County Mpls-St Paul", "Southwest", "Southeast"))

har.ma.avg.ruca <- har.ma.each %>%
  group_by(year, Dem_Desc) %>%
  summarise(mean.pct.public.charges = mean(pct.public.charges, na.rm = TRUE),
            median.pct.public.charges = median(pct.public.charges, na.rm = TRUE)) %>%
  ungroup()


# Plot --------------------------------------------------------------------

ggplot(filter(har.ma.each, year == 2018), aes(Dem_Desc, pct.public.charges, color = Dem_Desc)) +
  geom_boxplot()+
  geom_label(data = filter(har.ma.avg.ruca, year == 2018), size = 3, show.legend = FALSE, aes(x = Dem_Desc, y = .5, label = paste("Mean: ", percent(mean.pct.public.charges), "\nMedian: ", percent(median.pct.public.charges), sep = ""))) +
  labs(x="", y = "", color="", title = "Percent of patient charges to public program\nin each hospital by RUCA county group")+
  scale_y_continuous(labels=scales::percent) +
  theme_line+
  scale_color_manual(values= color.ruca,
                     guide = guide_legend(ncol = 3)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 15, hjust = 1),
        text = element_text(size = 15))

ggsave(filename = "Charts/pct-charges-public-ruca.png", type = "cairo", dpi = "print", width = 6, height = 5)

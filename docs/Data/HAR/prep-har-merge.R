library(tidyverse)

counties.regions <- read_csv("Data/Join docs/county_regions.csv") %>%
  mutate(countyfp = formatC(countyfp, width = 3, flag = "0"),
         Name = str_to_title(Name),
         Name = str_replace(Name, "Q", "q"),
         Name = str_replace(Name, "Of The", "of the"),
         Dem_Desc = ifelse(Name == "Minnesota", "Minnesota", Dem_Desc))

# import har reports ------------------------------------------------------

har.codes <- read_csv("Data/HAR/har-codes.csv")

hospital_id <- read_csv("Data/HAR/hospital-id.csv")

har.2017 <- read_csv("Data/HAR/hccis2017.csv") %>%
  left_join(hospital_id[,c(1,3,4,8)], by = c("hccis_id" = "HCCIS ID")) %>%
  drop_na(County)

har.2016 <- read_csv("Data/HAR/hccis2016.csv") %>%
  left_join(hospital_id[,c(1,3,4,8)], by = c("hccis_id" = "HCCIS ID")) %>%
  drop_na(County)

har.2015 <- read_csv("Data/HAR/hccis2015.csv") %>%
  left_join(hospital_id[,c(1,3,4,8)], by = c("hccis_id" = "HCCIS ID")) %>%
  drop_na(County)

har.2014 <- read_csv("Data/HAR/hccis2014.csv") %>%
  left_join(hospital_id[,c(1,3,4,8)], by = c("hccis_id" = "HCCIS ID")) %>%
  drop_na(County)


# Merge and write -------------------------------------------------------------

har <- har.2017 %>%
  rbind(har.2016, har.2015, har.2014) %>%
  mutate(County = str_to_title(County),
         County = str_replace(County, "Lac Qui Parle", "Lac qui Parle"),
         County = str_replace(County, "Lake Of The Woods", "Lake of the Woods")) %>%
  left_join(counties.regions, by = c("County" = "Name"))

write_csv(har, "Data/HAR/Master-har.csv")

install.packages(pkgs="RAQSAPI", dependencies = TRUE )

library(RAQSAPI)
library(tidyverse)
library(lubridate)
library(dplyr)

RAQSAPI::aqs_credentials("lylla.younes@propublica.org", "indigokit95")

## NOTES ##

# The general equation is
# µg/m3 = (ppb)*(12.187)*(M) / (273.15 + °C)
# where M is the molecular weight of the gaseous pollutant. An atmospheric pressure of 1
# atmosphere is assumed.

# Risk screening levels
# Calculated from:
# Draft Calvert City Air Toxics Monitoring Risk/Hazard Screening, Dr. Solomon Pollard, US EPA, 10/31/2017
# Converted values from ug/m3 to ppbv using STP at: https://www3.epa.gov/ceampubl/learn2model/part-two/onsite/ia_unit_conversion.html
# Then converted ppbv to ppbvC by multiplying each by the number of carbon atoms in each chemical (2 carbon atoms in both chemicals)
# https://cfpub.epa.gov/ncer_abstracts/index.cfm/fuseaction/display.files/fileid/14285

# ILCR = conc * (unitriskinhale / 1000)
# benchmark ~ ilcr of 1 ppbC edc = 0.5 ppbV edc = 2.0225ug/m3 edc 
# ilcr = 2.0225ug/m3 edc * (0.026/1000)

edc_screening_level_ppbC = 0.0190172 # 0.03846154 ug/m3 0.00952 
vc_screening_level_ppbC  = 0.0889640 # 0.01136364 ug/m3 
  
## vinyl chloride param      = 43860 ##
## ethylene dichloride param = 43815 ##
# calvert <- aqs_sampledata_by_box(
#     parameter = c("43815", "43860"), 
#     bdate = as.Date("20050101", format = "%Y%m%d"),
#     edate = as.Date("20211231", format = "%Y%m%d"),
#     minlat = "36", 
#     maxlat = "37", 
#     minlon = "-90", 
#     maxlon = "-86", 
#     return_header = TRUE
# )
#   
# calvert <- aqs_sampledata_by_state(
#   parameter = c("43815", "43860"), 
#   bdate = as.Date("20050101", format = "%Y%m%d"),
#   edate = as.Date("20211231", format = "%Y%m%d"),
#   stateFIPS = "21", 
#   return_header = TRUE
# )
# 
# View(calvert)
# ## $$c(W 89°02'00"--W 87°38'00"/N 37°19'00"--N 36°29'00")
# calvert_data <- calvert[[1]][["Data"]]
# View(calvert_data)
# 
# acslist <- lapply(2012:2018, function (x)  censusGrab(x))
# acslist12_18 <- acslist[[1]]
# for (i in 2:length(acslist)) acslist12_18 <- bind_rows(acslist12_18, acslist[[i]])


#### SET PARAMETERS ###

params = c("43815", "43860")
begin_date <- ymd("20100101")
end_date <- ymd("20211231")
state_code <- "21" #KY

# Marshall County, KY (the county for Calvert City)
# Carter County (Grayson Lake NATTS site)
# Livingston County (Bloodworth site)
county_codes <- c("157", "043", "139") 

calvert_city_data <- aqs_sampledata_by_state(
  parameter = params,
  bdate = begin_date,
  edate = end_date,
  stateFIPS = state_code
)

View(calvert_city_data)

calvert_city_monitor_info <- aqs_monitors_by_state(
  parameter = params,
  bdate = begin_date,
  edate = end_date,
  stateFIPS = state_code
)

View(calvert_city_monitor_info)

# Clean and set data types
calvert_city_data_cleaned <- calvert_city_data %>%
  mutate(
    site_code = paste(state_code, county_code, site_number, parameter_code, poc, sep = "-"),
    date_local = ymd(date_local),
    sample_measurement = as.numeric(sample_measurement)
  )

View(calvert_city_data_cleaned)

calvert_city_monitor_info_cleaned <- calvert_city_monitor_info %>%
  mutate (
    site_code = paste(state_code, county_code, site_number, parameter_code, poc, sep = "-"),
    monitor_code = paste(state_code, county_code, site_number, parameter_code, poc, sep="-" ),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )

View(calvert_city_monitor_info_cleaned)

# Join location information
calvert_city_data_joined_loc <- calvert_city_data_cleaned %>%
  left_join(calvert_city_monitor_info_cleaned %>% select(site_code, local_site_name) %>% unique(), by = "site_code")


View(calvert_city_data_joined_loc)

write.csv(calvert_city_data_joined_loc, "/Users/lylla.younes/Desktop/calvert_city_data_joined_loc.csv")

calvert_city_data_joined_loc_poc6 <- calvert_city_data_joined_loc %>%
    filter(poc==6)

calvert_city_data_joined_loc_poc6_by_year <- calvert_city_data_joined_loc_poc6 %>% 
  group_by(year = lubridate::floor_date(date_local, "year"))%>%
  summarise(sample_measurement = mean(sample_measurement, na.rm =T))

View(calvert_city_data_joined_loc_poc6_by_year)

calvert_city_data<-calvert_city_data %>%
    mutate(latitude=as.numeric(latitude),
          longitude=as.numeric(longitude)) %>%
  filter(county_code %in% county_codes)

View(calvert_city_data)

# Set site objectives
calvert_city_data <- calvert_city_data %>%
    mutate(site_objective = case_when (
      local_site_name == "TVA SUBSTATION" | local_site_name == "LWD" ~ "max conc vc prod", 
      local_site_name == "ATMOS ENERGY" | local_site_name == "Johnson-Riley Road" ~ "max conc PVC prod",
      local_site_name == "Calvert City Elementary" | local_site_name == "LAZY DAZ" ~ "population exposure",
      local_site_name == "GRAYSON LAKE" ~ "background/NATTS",
      TRUE ~ ""
    ))

View(calvert_city_data)

# Average by day
calvert_city_data_summed_days <- calvert_city_data %>% group_by(date_local, parameter) %>% 
  summarise(sample_measurement = mean(sample_measurement, na.rm =T))

# Filter for EDC
calvert_city_data_edc <- calvert_city_data_summed_days %>%
  filter(parameter == "Ethylene dichloride")

cc_edc_by_year <-calvert_city_data_edc %>%
  group_by(year = lubridate::floor_date(date_local, "year")) %>%
  summarise(sample_measurement = mean(sample_measurement, na.rm =T))
View(cc_edc_by_year)

# Filter for VC
calvert_city_data_vc <- calvert_city_data_summed_days %>%
  filter(parameter == "Vinyl chloride")

cc_vc_by_year <-calvert_city_data_vc %>%
  group_by(year = lubridate::floor_date(date_local, "year")) %>%
  summarise(sample_measurement = mean(sample_measurement, na.rm =T))


View(cc_vc_by_year)



write.csv(calvert_city_data, "/Users/lylla.younes/Desktop/calvert_city_data.csv")

#calvert_city_data <- calvert_city_data %>%
#  mutate(site_objective = case_when(
#    local_site_name == "TVA SUBSTATION" | local_site_name
#  ))


### ### ### ### ### ### ###
### ### ISOLATE EDC ### ###
### ### ### ### ### ### ###


cc_edc_poc6_by_year <- calvert_city_data %>%
  filter(poc==6 & parameter_code==43815) %>%
  mutate(
    site_code = paste(state_code, county_code, site_number, parameter_code, poc, sep = "-"),
    date_local = ymd(date_local),
    sample_measurement = as.numeric(sample_measurement),
    sample_measurement_ugm3 = (  (as.numeric(sample_measurement) / 2) * 0.0409 * 98.96 ),
    ilcr = ( (as.numeric(sample_measurement) / 2) * 0.0409 * 98.96 ) * (0.026/1000)
  ) %>%
  left_join(calvert_city_monitor_info_cleaned %>% select(site_code, local_site_name) %>% unique(), by = "site_code") %>%
  # group_by(local_site_name, year = lubridate::floor_date(date_local, "year"), latitude , longitude)%>%
  group_by(local_site_name,date_local, latitude , longitude) %>%
  summarise(sample_measurement = mean(sample_measurement, na.rm =T), 
            sample_measurement_ugm3 = mean(sample_measurement_ugm3, na.rm =T),
            ilcr = mean(ilcr, na.rm =T))

View(cc_edc_poc6_by_year)

cc_edc_poc6_total_avg <- cc_edc_poc6_by_year %>%
    group_by(local_site_name, latitude, longitude) %>%
    summarise(sample_measurement = mean(sample_measurement, na.rm =T), 
              sample_measurement_ugm3 = mean(sample_measurement_ugm3, na.rm =T),
              ilcr = mean(ilcr, na.rm =T))
View(cc_edc_poc6_total_avg)

write.csv(cc_edc_poc6_by_year, "/Users/lylla.younes/Desktop/cc_edc_poc6_by_day_compete.csv")

### ### ### ### ### ### ###
### ### ISOLATE VC ### ####
### ### ### ### ### ### ###

cc_vc_poc6_by_year <- calvert_city_data %>%
  filter(poc==6 & parameter_code==43860) %>%
  mutate(
    site_code = paste(state_code, county_code, site_number, parameter_code, poc, sep = "-"),
    date_local = ymd(date_local),
    sample_measurement = as.numeric(sample_measurement),
    sample_measurement_ugm3 = (  (as.numeric(sample_measurement) / 2) * 0.0409 * 62.50 ),
    ilcr = ( (as.numeric(sample_measurement) / 2) * 0.0409 * 62.50 ) * (0.0088/1000)
  ) %>%
  left_join(calvert_city_monitor_info_cleaned %>% select(site_code, local_site_name) %>% unique(), by = "site_code") %>%
  group_by(local_site_name, year = lubridate::floor_date(date_local, "year"))%>%
  summarise(sample_measurement = mean(sample_measurement, na.rm =T), 
            sample_measurement_ugm3 = mean(sample_measurement_ugm3, na.rm =T),
            ilcr = mean(ilcr, na.rm =T))

View(cc_vc_poc6_by_year)




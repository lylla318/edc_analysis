install.packages(pkgs="RAQSAPI", dependencies = TRUE )

library(RAQSAPI)
library(tidyverse)
library(lubridate)
library(dplyr)

RAQSAPI::aqs_credentials("lylla.younes@propublica.org", "indigokit95")

######### NOTES #########

### Unit conversions ###
# The general equation is
# µg/m3 = (ppb)*(12.187)*(M) / (273.15 + °C)
# where M is the molecular weight of the gaseous pollutant. An atmospheric pressure of 1
# atmosphere is assumed.

# Calculated from:
# Draft Calvert City Air Toxics Monitoring Risk/Hazard Screening, Dr. Solomon Pollard, US EPA, 10/31/2017
# Converted values from ug/m3 to ppbv using STP at: https://www3.epa.gov/ceampubl/learn2model/part-two/onsite/ia_unit_conversion.html
# Then converted ppbv to ppbvC by multiplying each by the number of carbon atoms in each chemical (2 carbon atoms in both chemicals)
# https://cfpub.epa.gov/ncer_abstracts/index.cfm/fuseaction/display.files/fileid/14285

# ILCR = conc * (unitriskinhale / 1000)
# benchmark ~ ilcr of 1 ppbC edc = 0.5 ppbV edc = 2.0225ug/m3 edc 
# ilcr = 2.0225ug/m3 edc * (0.026/1000)

### County information ###
# Marshall County, KY (the county for Calvert City)
# Carter County (Grayson Lake NATTS site)
# Livingston County (Bloodworth site)

### Risk screening levels ###
edc_screening_level_ppbC = 0.0190172 # 0.03846154 ug/m3 0.00952 
vc_screening_level_ppbC  = 0.0889640 # 0.01136364 ug/m3 

## Output is in ppbC !!!


#### SET PARAMETERS ###

params = c("43815", "43860")
begin_date <- ymd("20100101")
end_date <- ymd("20211231")
state_code <- "21" #KY

f_params = c("43502")

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



##########################################
######### NATIONAWIDE ANALYSIS ###########
##########################################

# params = c("43815", "43860") # EDC, VC
# state_code <- "21" #KY

# Set parameters
state_codes = c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "72", "44", "45", "46", "47", "48", "49", "50", "51", "78", "53", "54", "55", "56")
params = c("43502") # formaldehyde
begin_date <- ymd("20100101")
end_date <- ymd("20211231")

# Pull and clean sample data.
get_state_sample_data <- function(state_code) {
  print(state_code)
  state_data <- aqs_sampledata_by_state(
    parameter = params,
    bdate = begin_date,
    edate = end_date,
    stateFIPS = state_code
  )
  return(state_data)
}

national_data <- lapply(state_codes, function (x)  get_state_sample_data(x))
national_data_all <- national_data[[1]]
for (i in 2:length(national_data)) national_data_all <- bind_rows(national_data_all, national_data[[i]])
View(national_data_all)

national_data_all_cleaned <- national_data_all %>%
  mutate(
    site_code = paste(state_code, county_code, site_number, parameter_code, poc, sep = "-"),
    date_local = ymd(date_local),
    sample_measurement = as.numeric(sample_measurement)
  )


# Pull and clean monitor data
get_state_monitor_data <- function(state_code) {
  print(state_code)
  state_monitor_data <- aqs_monitors_by_state(
    parameter = params,
    bdate = begin_date,
    edate = end_date,
    stateFIPS = state_code
  )
  return(state_monitor_data)
}

national_monitoring_data <- lapply(state_codes, function (x)  get_state_monitor_data(x))
national_monitoring_data_all <- national_monitoring_data[[1]]
for (i in 2:length(national_monitoring_data)) national_monitoring_data_all <- bind_rows(national_monitoring_data_all, national_monitoring_data[[i]])
View(national_monitoring_data_all)

national_monitoring_data_all_cleaned <- national_monitoring_data_all %>%
  mutate (
    site_code = paste(state_code, county_code, site_number, parameter_code, poc, sep = "-"),
    monitor_code = paste(state_code, county_code, site_number, parameter_code, poc, sep="-" ),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )


# Join location information
national_aqs <- national_data_all_cleaned %>%
  left_join(national_monitoring_data_all_cleaned %>% select(site_code, local_site_name) %>% unique(), by = "site_code")

View(national_aqs)

# Filter for EDC
# national_aqs_edc <- national_aqs %>%
#   filter(parameter == "Ethylene dichloride")

write.csv(national_aqs, "/Users/lylla.younes/Desktop/national_aqs_formaldehyde.csv")























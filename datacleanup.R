library(dplyr)

truncate_nypd <- function() {
  nypd_historic<-read.csv("csv_files/NYPD_Complaint_Data_Historic_20240703.csv")
  years<-format(as.Date(nypd_historic$CMPLNT_FR_DT, format="%m/%d/%Y"),"%Y")
  nypd_historic$cmplt_year<-years
  nypd_11_19 <- (nypd_historic %>% filter(cmplt_year >= 2011 & cmplt_year <= 2019))
  nchk <- "(null)"
  nypd_11_19 <- nypd_11_19[nypd_11_19["CMPLNT_FR_DT"] != nchk & nypd_11_19["CMPLNT_FR_TM"] != nchk & nypd_11_19["BORO_NM"] != nchk & nypd_11_19["SUSP_RACE"] != nchk & nypd_11_19["SUSP_SEX"] != nchk & nypd_11_19["VIC_RACE"] != nchk & nypd_11_19["VIC_SEX"] != nchk,]
  write.csv(nypd_11_19, "csv_files/nypd_11_19.csv")
}

truncate_nypd()

get_ny_codes <- function() {
  zcta <- sf::read_sf("/Users/krishna/Rutgers/DataScience/Project/final_project/csv_files/cb_2020_us_zcta520_500k/cb_2020_us_zcta520_500k.shp")
  nyc_codes<-zipcodeR::zip_code_db[zipcodeR::zip_code_db$state=="NY",]
  nyc_codes<-nyc_codes[nyc_codes$county %in% c("New York County", "Bronx County", "Kings County", "Queens County", "Richmond County"),c("zipcode", "major_city", "county", "state", "lat", "lng")]
  nyc_codes<-na.omit(nyc_codes)
  names(zcta)[names(zcta)=="GEOID20"] <- "zipcode"
  nyc_db<-nyc_codes[nyc_codes$zipcode %in% zcta$zipcode,c("zipcode", "county", "lat", "lng")]
  nyc_shp <- zcta[as.character(zcta$zipcode) %in% as.character(nyc_codes$zipcode),]
  sf::write_sf(nyc_shp, "/Users/krishna/Rutgers/DataScience/Project/final_project/csv_files/ny/ny.shp")
}

get_ny_codes()

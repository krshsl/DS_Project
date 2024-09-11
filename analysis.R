library(ggplot2)
library(ggstats)
library(dplyr)
library(tidyverse)
library(GGally)

zcta <- sf::read_sf("/Users/krishna/Rutgers/DataScience/Project/final_project/csv_files/cb_2020_us_zcta520_500k/cb_2020_us_zcta520_500k.shp")
nyc_codes<-zipcodeR::zip_code_db[zipcodeR::zip_code_db$state=="NY",]
nyc_codes<-nyc_codes[nyc_codes$county %in% c("New York County", "Bronx County", "Kings County", "Queens County", "Richmond County"),c("zipcode", "major_city", "county", "state", "lat", "lng")]
nyc_codes<-na.omit(nyc_codes)
names(zcta)[names(zcta)=="GEOID20"] <- "zipcode"
nyc_db<-nyc_codes[nyc_codes$zipcode %in% zcta$zipcode,c("zipcode", "county", "lat", "lng")]
nyc_shp <- zcta[as.character(zcta$zipcode) %in% as.character(nyc_codes$zipcode),]
rm(zcta, nyc_codes)
base_plot <- ggplot() +  geom_sf(data = nyc_shp)

merge_dfs <- function(nyc_shp, nyc_db, df) {
  df <- merge(nyc_shp, df, by="zipcode")
  return(merge(df, nyc_db, by="zipcode"))
}

read_merge <- function(nyc_db, file) {
  csv_df <- read.csv(file)
  nyc_counties<-nyc_db[,c("zipcode", "county")]
  return(merge(csv_df, nyc_counties, by="zipcode"))
}

nypd_11_19 <- read_merge(nyc_db, "csv_files/nypd_final.csv")
nypd_11_19$CMPLNT_FR_DT <- as.POSIXct(nypd_11_19$CMPLNT_FR_DT, format="%m/%d/%Y")
nypd_11_19$CMPLNT_FR_TM <- as.POSIXct(nypd_11_19$CMPLNT_FR_TM, format = "%H:%M:%S")
# income_11_19 <- read_merge(nyc_db, "csv_files/income.csv")
pop_employment_11_19 <- read_merge(nyc_db, "csv_files/pop_employment.csv")
schooling_11_19 <- read_merge(nyc_db, "csv_files/schooling.csv")

nypd_11_19_all <- data.frame(unclass(table(nypd_11_19$zipcode)))
nypd_11_19_all <- tibble::rownames_to_column(nypd_11_19_all, "zipcode")
names(nypd_11_19_all) <- c("zipcode", "count")
nypd_11_19_all <- merge_dfs(nyc_shp, nyc_db, nypd_11_19_all)

nypd_11_19_county <- data.frame(unclass(table(nypd_11_19$county, nypd_11_19$cmplt_year)))
names(nypd_11_19_county) <- c(2011:2019)
nypd_11_19_county <- tibble::rownames_to_column(nypd_11_19_county, "county")
nypd_11_19_county <- nypd_11_19_county %>% pivot_longer(cols=-c("county"))
names(nypd_11_19_county) <- c("county","year","count")
nypd_11_19_county$year<-as.numeric(nypd_11_19_county$year)

options(scipen=10)
get_count_data <- function(df) {
  counts_all_desc = table(df$OFNS_DESC)
  counts_all_desc = counts_all_desc[order(counts_all_desc, decreasing=T)]
  count_ofns <- counts_all_desc[1:10]
  counts_df <- data.frame(t(data.frame(rbind(count_ofns))))
  counts_df <- tibble::rownames_to_column(counts_df, "OFNS_DESC")
  return(counts_df)
}
all_counts_df <- get_count_data(nypd_11_19)
ggplot(all_counts_df, aes(y=reorder(OFNS_DESC, -count_ofns), x=count_ofns)) + geom_bar(stat = "identity", fill="steelblue") + geom_text(
  aes(label = count_ofns), position = position_dodge(width = .8), angle = 0, hjust = 1.5, vjust = 0.3, color = "white"
) + labs(title="Top 10 offenses from 2011-2019", x ="Count", y = "Offense Description")


get_yoy_count <- function() {
  counts_years_df <- list()
  for (i in 1:9) {
    counts_years_df[[i]] <- get_count_data(nypd_11_19[nypd_11_19$cmplt_year==(i+2010),])
    counts_years_df[[i]]$year <- i+2010
  }
  return(bind_rows(counts_years_df))
}

year_over_year_plot <- function() {
  counts_years_df<-get_yoy_count()
  ggplot(counts_years_df, aes(x=year, y=count_ofns, group=OFNS_DESC, color=OFNS_DESC)) + facet_wrap(~OFNS_DESC, ncol = 3) + geom_line() + labs(title="Top 10 offenses recorded over each year from 2011 to 2019", x ="Year", y = "Count", color="Offense Description") + scale_x_continuous(breaks=2010:2020) + theme(legend.key.size = unit(1.75, "lines"))
}
year_over_year_plot()

timeplace_plot <- function() {
  nypd_timeplace <- nypd_11_19 %>% select(CMPLNT_FR_TM, BORO_NM, ADDR_PCT_CD)
  ggplot(nypd_timeplace, aes(x=ADDR_PCT_CD, y=CMPLNT_FR_TM, color=BORO_NM)) + geom_point(size = 0.2) + scale_y_datetime(labels = function(x) format(x, "%H:%M", tz = "EST"), date_breaks = '1 hour', expand = c(0,0)) + labs(title="Offense through the day from 2011 to 2019", x ="Precinct", y = "Time of the day", color="Borough Name") + theme(text = element_text(size = 18), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18), legend.key.size = unit(1, 'lines')) + guides(colour = guide_legend(override.aes = list(size=10)))
}
timeplace_plot()

offense_fq_plot <- function() {
  nypd_11_19_fq <- nypd_11_19 %>% group_by(CMPLNT_FR_DT) %>% summarise(frequency= n())
  p1<-ggplot(nypd_11_19_fq, aes(x=CMPLNT_FR_DT, y=frequency)) + geom_point(size = 0.2) + labs(title="Offenses frequency from 2011 to 2019", x="Year", y="Frequenzy") + scale_x_datetime(labels = function(x) format(x, "%Y", tz = "EST"), date_breaks = '1 year', expand = c(0.01,0)) + geom_smooth(se = TRUE)
  p2<-ggplot(nypd_11_19_fq, aes(cut(CMPLNT_FR_DT, breaks="year", labels=2011:2019), frequency)) + geom_boxplot(notch = TRUE) + labs(title="Offenses frequency from 2011 to 2019", x="Year", y="Frequenzy")
  print(p1)
  print(p2)
}
offense_fq_plot()

print_map_plot <- function(df, title) {
  print(base_plot + geom_sf(data=df, aes(fill=count)) + labs(fill="Count") + scale_fill_gradient(low = ("white"), high = ("black")) + new_scale("fill") + new_scale("size") + geom_sf(data=df, aes(fill=county, color=county, alpha = 0.1), lwd=0.1) + guides(alpha="none", color="none") + labs(title=title, fill="County"))
}

print_map_plot(nypd_11_19_all, "Offenses frequency from 2011 to 2019 across NYC")


plot_pop_data <- function(nyc_shp, nyc_db, df, title, col_name) {
  colnames(df)[which(names(df) == col_name)] <- "count"
  df$count<-as.numeric(as.character(df$count))
  df[is.na(df)]<-0
  df_avg<-aggregate(count~zipcode, data=df, mean)
  df_avg<-merge_dfs(nyc_shp, nyc_db, df_avg)
  print_map_plot(df_avg, title)
  return(df)
}
plot_pop_data(nyc_shp, nyc_db, pop_employment_11_19, "Average Population above 16 years across NYC from 2011 to 2019", "S2301_C01_001E")
schooling_11_19<-plot_pop_data(nyc_shp, nyc_db, schooling_11_19, "Average population who are atleast High School Graduates across NYC from 2011 to 2019", "B20004_003E")
pop_employment_11_19$S2301_C01_001E<-as.numeric(as.character(pop_employment_11_19$S2301_C01_001E))
pop_employment_11_19$S2301_C03_001E<-as.numeric(as.character(pop_employment_11_19$S2301_C03_001E))
pop_employment_11_19$count<-pop_employment_11_19$S2301_C01_001E*pop_employment_11_19$S2301_C03_001E/100
pop_employment_11_19<-plot_pop_data(nyc_shp, nyc_db, pop_employment_11_19, "Average employed populous across NYC from 2011 to 2019", "count")

# income_11_19<-plot_pop_data(nyc_shp, nyc_db, income_11_19, "Estimated households with income in each region across NYC from 2011 to 2019", "S1902_C02_001E")

final<-table(nypd_11_19$county, nypd_11_19$cmplt_year)
ggplot()


plot_yoy_county_main <- function(df, title) {
  ggplot(df, aes(x=year, y=count, group=county, color=county)) + geom_line() + labs(title=title, x ="Year", y = "Count", color="County") + scale_x_continuous(breaks=2010:2020) + theme(legend.key.size = unit(1.75, "lines"))
}

aggregate_data <- function(df) {
  new_df<-aggregate(count~county+year, df, mean)
  return(new_df)
}

school<-aggregate_data(schooling_11_19)
employed<-aggregate_data(pop_employment_11_19)
names(school)<-c("county", "year", "school")
names(employed)<-c("county", "year", "employed")
final_county<-merge(school, employed, by=c("county", "year"))
final_county<-merge(final_county, nypd_11_19_county, by=c("county", "year"))
names(final_county)<-c("county","year","school","employed","crime")

plot_yoy_county_main(nypd_11_19_county, "Frequenzy of crime through the years across various counties from 2011 to 2019")
plot_yoy_county_main(aggregate_data(schooling_11_19), "Average population who are atleast High School Graduates across various counties from 2011 to 2019")
plot_yoy_county_main(aggregate_data(pop_employment_11_19), "Average employed populous across various counties from 2011 to 2019")

ggplot(final_county, aes(x=year, color=county, group=county)) + geom_line(aes(y = school, linetype = "school")) + geom_line(aes(y = employed, linetype = "employed")) + geom_line(aes(y = crime, linetype = "crime")) + scale_x_continuous(breaks=2010:2020) + theme(legend.key.size = unit(1.75, "lines")) + facet_wrap(~county, ncol = 1) + scale_linetype_manual(values = 1:3, labels=c("Crime", "Employed", "Educated")) + labs(title="Count of various factors across NYC counties from 2011 to 2019", x="Year", y="Frequenzy", linetype="Count", group="County", color="County")
ggcorr(final_county[2:5], label = TRUE) + labs(title="Correlation values of the various factors")
ggpairs(final_county, columns=2:5, ggplot2::aes(colour = county)) + labs(title="Correlation matrix of the various factors")


##################
#Loading libraries
##################

library(data.table)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(rgdal)
library(viridis) #for map coloring
library(outliers) #for testing max value in SeoulFloating

#Setting personal working directory with all relevant datasets

setwd("C:/Users/Rodrigo/Desktop/TUM/Wintersemester 2021/Data Analysis and Visualization in R/Case Study/data")

#Importing alls .csv files into a list

list_data = list.files(pattern="*.csv")
my_datatables = lapply(list_data, fread)
my_datatables
my_names <- c("case",
              "patientInfo",
              "Policy",
              "Pop_Density",
              "Region",
              "SearchTrend",
              "SeoulFloating",
              "Time",
              "TimeAge",
              "TimeGender",
              "TimeProvince",
              "Weather") #Sorted alphabetically
names(my_datatables) <- my_names

#Mapping list objects to environment

list2env(my_datatables, .GlobalEnv)

#First hypothesis: Population density correlates with number of cases

pop_dens_col_names <- as.character(Pop_Density[1, ]) #formatting the data for population density
names(Pop_Density) <- pop_dens_col_names
Pop_Density[, c("2005", "2010"):= NULL]
Pop_Density <- Pop_Density[2:.N]
Pop_Density[, Pop_dens_sq_km := `2015`][, `2015` := NULL]
infections_by_province <- patientInfo[, .(count = .N),by = c("province",
                                                             "confirmed_date")][, accumulated_sum := cumsum(count)]
infections_by_province[, n_infections := count]
latest_infections <- infections_by_province[order(-as.IDate(confirmed_date, 
                                                             "%Y-%m-%d")), 
                                             head(.SD, 1), 
                                             by = province][, c("count",
                                                                "n_infections") := NULL]
latest_total <- list(province = "Whole country",
                  confirmed_date = as.IDate("2020-06-30"),
                  accumulated_sum = sum(latest_infections$accumulated_sum)) #extra obersvation for country total

latest_infections_national <- rbind(latest_infections, latest_total)

cases_pop_density <- merge(latest_infections, Pop_Density, by.x = "province", by.y = "By administrative divisions", all = FALSE)


cases_pop_density[order(-accumulated_sum)] #checking provinces with most cases
cases_pop_density[order(-Pop_dens_sq_km)] #checking provinces with highest population density

plot_a <- ggplot(cases_pop_density) + #first plot density and cases
  geom_col(aes(x = reorder(province, -accumulated_sum),
               y = accumulated_sum),
           alpha = 0.75,
           width = 0.7,
           fill = "dark blue") +
  labs(title = "Population density and COVID-19 cases per province",
       x = "South Korean Province",
       y = "Number of cases") +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

plot_cases_popdensity <- plot_a + geom_line(cases_pop_density, #added line onto bar plot
                   mapping = aes(x = province,
                                 y = 0.3*Pop_dens_sq_km,
                                 group = 1),
                   color = "red",
                   size = 1) +
  scale_y_continuous(limits = c(0, 7000), 
                     breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000), 
                     sec.axis = sec_axis(~./0.3, name = "Population Density")) #adds a second y-axis #need to improve the graph

korea_map <- readOGR("C:/Users/Rodrigo/Desktop/TUM/Wintersemester 2021/Data Analysis and Visualization in R/Case Study/data/KOR_adm",
        layer = "KOR_adm1") #creating a map to visualize the differences
korea_map <- fortify(korea_map, region = "NAME_1") #shape file is now a dataframe
cases_pop_density$province <- gsub("Jeju-do", "Jeju", cases_pop_density$province) #changed the name of province Jeju-do to Jeju, since map uses this name
korea_map <- cases_pop_density[korea_map, 
                               on = c("province" = "id")]

map1 <- ggplot(data = korea_map, #this is the base for the maps
               aes(x = long,
                   y = lat,
                   group = group)) + theme_bw() + theme(legend.background = element_rect(fill = "white", color = "black"))
  
map_cases <- map1 + geom_polygon(aes(fill = accumulated_sum)) +
  labs(title = "Accumulated cases in South Korean Provinces", 
       fill = "Total number of cases") +
  scale_fill_viridis(option = "plasma", direction = 1) +
  geom_path(aes(x = long, y = lat, group = group), color = "black", size = 1) #outlines provinces

map_pop_density <- map1 + geom_polygon(aes(fill = Pop_dens_sq_km)) +
  labs(title = "Population Density in South Korean Provinces", 
       fill = "Population density") +
  scale_fill_viridis(option = "plasma", direction = 1) +
  geom_path(aes(x = long, y = lat, group = group), color = "black", size = 1) #outlines provinces

#Second hypothesis: The spread of COVID-19 affected floating population in Seoul

SeoulFloating[, mean_fp_num := mean(fp_num), by = date]

summary(SeoulFloating$mean_fp_num) # Max could be an outlier
tail(unique(sort(SeoulFloating$mean_fp_num)))
outlier_test <- grubbs.test(SeoulFloating$mean_fp_num)
print(outlier_test) #H0: The highest value is not an outlier. WIth p < 0.05, we reject H0, meaning the max value is an outlier. 

outlierReplace = function(dataframe, cols, rows, newValue = NA) { #function to remove specified outliers
  if(any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

outlierReplace(SeoulFloating, "mean_fp_num", which(SeoulFloating$mean_fp_num > 51000), NA) #might decide to keep the outlier

fp_plot <- ggplot(SeoulFloating, aes(x = date,
                                          y = mean_fp_num)) +
  geom_line() +
  labs(title = "Average Floating Population in Seoul over time",
       x = "Date",
       y = "Floating Population Average") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B",
               limits = as.Date(c("2020-01-01", "2020-05-31"))) +
  theme_bw() +
  geom_smooth()

infections_seoul <- patientInfo[province == "Seoul",
                                .(count = .N), 
                                by = c("confirmed_date")][, accumulated_sum := cumsum(count)]
is_plot <- ggplot(infections_seoul, aes(x = confirmed_date,
                                        y = accumulated_sum)) +
  geom_line() + 
  labs(title = "Total number of cases in Seoul",
       x = "Date",
       y = "Accumulated number of cases") +
  theme_bw()

#Statistical testing of the relationship

sf_is <- merge(SeoulFloating, infections_seoul, by.x = "date", by.y = "confirmed_date", all = TRUE)
sf_is <- sf_is[date >= "2020-01-01" & date <= "2020-05-31"][!duplicated(date)][, c("date",
                                                                           "mean_fp_num",
                                                                           "accumulated_sum")]
lm_cases <- lm(mean_fp_num ~ accumulated_sum, data = sf_is)
summary(lm_cases) #not statistically significant

weather_seoul_2020 <- Weather[date >= "2020-01-01" & date <= "2020-05-31" & province == "Seoul"][, c("date",
                                                                                                     "avg_temp")] #preparing weather dataset. Rising average temperature could affect floating population average
sf_is2 <- merge(sf_is, weather_seoul_2020, by = "date")
lm_cases_temp <- lm(mean_fp_num ~ accumulated_sum + avg_temp, data = sf_is2)
summary(lm_cases_temp) #not statistically significant

#Third hypothesis: Age groups are affected by different infection durations (Kedi)
patientInfo[, age := as.numeric(gsub("s", "", age))][,lengthcovid := (released_date - confirmed_date)] #What about deceased date? OR operator possible?
mean_length <-patientInfo[, .(mean_length = mean(lengthcovid, na.rm = TRUE)), by = age][order(age)]
mean_length <- mean_length[age != 100]


#patientInfo dataset



infections_by_province <- patientInfo[, .(count = .N),by = c("province", "confirmed_date")][, accumulated_sum := cumsum(count)]
infections_by_province[, n_infections := count]


#policy dataset

policy_type <- Policy[, .(count = .N), by = type] %>% arrange(desc(count))

#time dataset

time_cumsum <- Time[, `:=`(cummulative = cumsum(test), negative_cummulative = cumsum(negative), confirmed_cummulative = cumsum(confirmed), released_cummulative = cumsum(released), deceased_cummulative = cumsum(deceased)), by = date]

time_cumsum <- select(time_cumsum, c(date, cummulative, negative_cummulative, confirmed_cummulative, released_cummulative, deceased_cummulative))

#weather dataset, first two are not correct. Not sure why
weather_2020 <- Weather[filter(date > "2020-01-01")]

avg_temp_2020 <- weather_2020[, `:=`(average_temp = mean(avg_temp)), by = date]

weather_2020 <- Weather %>%
  filter(date > "2020-01-01")

avg_temp_2020 <- Weather %>%
  filter(date > "2020-01-01")%>%
  group_by(date)%>%
  summarize(average_temp = mean(avg_temp))

#Some plots

plot_infections_cumsum <- ggplot(infections_by_province, aes(x = confirmed_date, y = cumsum, color = province)) + geom_line() + ggtitle("Accumulative number of cases in provinces over time")
plot_infections_daily <- ggplot(infections_by_province, aes(x = confirmed_date, y = n_infections, color = province)) + geom_line() + labs(x = "Confirmed date of infection", y = "Number of daily infections") + theme(legend.position = "bottom") + ggtitle("Number of infections per day in South Korean provinces")
introduction_policy_time <- ggplot(policy, aes(x = start_date, y = type, color = type)) + geom_point(size = 2) + ggtitle("Introduction of policy over time") + labs(x = "Date policy was introduced", y = "Type of policy") + theme(legend.position = "bottom")
plot_weather_time <- ggplot(avg_temp_2020, aes(x = date, y = average_temp)) + geom_line() + labs(title = "Average temperature in Korea in 2020", x = "Date", y = "Average temperature")
length_covid_by_age <- ggplot(patientInfo, aes(age, lengthcovid, color=age)) + geom_point() + labs(title = "Duration of COVID-19 infections in days by age group", x = "Age", y = "Duration in days") + scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))
avg_length_covid_by_age <- ggplot(mean_length, aes(age, mean_length, color= age )) + geom_point() + labs(title = "Average duration of COVID-19 infection by age group", x = "Age", y = "Average duration in number of days") + scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))


multi_plot_1 <- plot_grid(plot_infections_daily, introduction_policy_time, labels = "AUTO")
multi_plot_2 <- plot_grid(plot_infections_daily, plot_weather_time, labels = "AUTO")

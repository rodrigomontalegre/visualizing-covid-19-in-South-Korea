##################
Loading libraries#
##################

library(data.table)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)

#Setting personal working directory with all relevant datasets

setwd("C:/Users/Rodrigo/Desktop/TUM/Wintersemester 2021/Data Analysis and Visualization in R/Case Study/data")

#Importing alls .csv files into a list

list_data = list.files(pattern="*.csv")
my_datatables = lapply(list_data, fread)
my_datatables
my_names <- c("case",
              "patientInfo",
              "Policy",
              "Pop_Info",
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

pop_info_col_names <- Pop_Info[2, ] #Formatting the data for population info
pop_info_col_names <- as.character(pop_info_col_names)
names(Pop_Info) <- pop_info_col_names
names(Pop_Info) <- gsub("-", "_", names(Pop_Info))
names(Pop_Info) <- gsub(" _ ", "_", names(Pop_Info))
names(Pop_Info) <- gsub(" ", "_", names(Pop_Info))
Pop_Info <- Pop_Info[3:.N, 1:10] #Selecting only necessary columns
cols <- c("Total_population_(Person)",
          "Male_(Person)",
          "Female_(Person)",
          "Korean_total_(Person)",
          "Korean_male_(Person)",
          "Korean_Female_(Person)",
          "Foreigner_Total_(Person)",
          "Foreigner_Male_(Person)",
          "Foreigner_Female_(Person)")
Pop_Info <- Pop_Info[, (cols) := lapply(.SD, as.numeric), .SDcols = cols] #Columns as type numeric

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
rbind(latest_infections, latest_total)






#patientInfo dataset

patientInfo[, age := as.numeric(gsub("s", "", age))][,lengthcovid := (released_date - confirmed_date)]
mean_length <-patientInfo[, .(mean_length = mean(lengthcovid, na.rm=TRUE)), by = age]
mean_length <- mean_length[1:10,]
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

#testing github connection
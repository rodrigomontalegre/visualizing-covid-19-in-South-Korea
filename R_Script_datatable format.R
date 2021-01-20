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
library(ggthemes) # for the theme_map()
library(gridExtra)

#Setting personal working directory with all relevant datasets

if (Sys.info()["user"] == "Rodrigo") {
  setwd("C:/Users/Rodrigo/Desktop/TUM/Wintersemester 2021/Data Analysis and Visualization in R/Case Study/data")
}

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

infections_by_province <- patientInfo[, .(count = .N), by = c("province",
                                                             "confirmed_date")][, accumulated_sum := cumsum(count)]
infections_by_province[, n_infections := count]

latest_infections <- infections_by_province[order(-as.IDate(confirmed_date, 
                                                             "%Y-%m-%d")), 
                                             head(.SD, 1), 
                                             by = province][, c("count",
                                                                "n_infections") := NULL]
latest_total <- list(province = "Whole country",
                  confirmed_date = as.IDate("2020-06-30"),
                  accumulated_sum = sum(latest_infections$accumulated_sum)) #extra obeservation for country total

latest_infections_national <- rbind(latest_infections, latest_total)

cases_pop_density <- merge(latest_infections, 
                           Pop_Density, 
                           by.x = "province", 
                           by.y = "By administrative divisions", 
                           all = FALSE)


plot_a <- ggplot(cases_pop_density) + #first plot density and cases
  geom_col(aes(x = reorder(province, -accumulated_sum),
               y = accumulated_sum),
           alpha = 0.75,
           width = 0.5,
           fill = "dark blue") +
  labs(title = "Number of confirmed COVID-19 cases per province in South Korea",
       x = "South Korean Province",
       y = "Number of cases") +
  theme(axis.text.x = element_text(angle = 90),
        panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


plot_cases_popdensity <- plot_a + geom_line(cases_pop_density, 
                                            mapping = aes(x = province,
                                                          y = 0.3*Pop_dens_sq_km,
                                                          group = 1),
                                            color = "dark red", 
                                            size = 3) + 
  scale_y_continuous(limits = c(0, 7000), 
                     breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000), 
                     sec.axis = sec_axis(~./0.3, name = "Population Density")) +
  labs("Number of confirmed COVID-19 cases and population density per South Korean province",
       x = "South Korean province",
       y = "Number of cases")

korea_map <- readOGR("C:/Users/Rodrigo/Desktop/TUM/Wintersemester 2021/Data Analysis and Visualization in R/Case Study/data/KOR_adm", layer = "KOR_adm1") #creating a map to visualize the differences. Change this to where the KOR_adm folder is saved and KOR_adm1 should be the shape file for the provinces

korea_map <- fortify(korea_map, 
                     region = "NAME_1") #shape file is now a dataframe

cases_pop_density$province <- gsub("Jeju-do", "Jeju", cases_pop_density$province) #changed the name of province Jeju-do to Jeju, since map uses this name

korea_map <- cases_pop_density[korea_map, 
                               on = c("province" = "id")]

map1 <- ggplot(data = korea_map, #this is the base for the maps
               aes(x = long,
                   y = lat,
                   group = group)) + 
  theme_bw() + #might just remove this theme
  theme(legend.background = element_rect(fill = "white", 
                                         color = "black"))
  
map_cases <- map1 + geom_polygon(aes(fill = accumulated_sum)) +
  labs(title = "Accumulated cases in South Korean Provinces", 
       fill = "Total number of cases") +
  scale_fill_viridis(option = "plasma", 
                     direction = 1) +
  geom_path(aes(x = long, 
                y = lat, 
                group = group), 
            color = "black", 
            size = 1) + 
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 3),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.background = element_blank(),
        legend.position = c(.85, .15),
        axis.ticks = element_blank())

map_pop_density <- map1 + geom_polygon(aes(fill = Pop_dens_sq_km)) +
  labs(title = "Population Density in South Korean Provinces", 
       fill = "Population density") +
  scale_fill_viridis(option = "plasma", 
                     direction = 1) +
  geom_path(aes(x = long, 
                y = lat, 
                group = group), 
            color = "black", 
            size = 1) + 
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 3),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.background = element_blank(),
        legend.position = c(.85, .15),
        axis.ticks = element_blank())

grid.arrange(map_cases, map_pop_density, ncol = 2)

#Qqplot to test for Gaussian distribution

qqplot1 <- ggplot(data = cases_pop_density, aes(sample = accumulated_sum)) + geom_qq() + stat_qq_line() +
  labs(title = "QQ-Plot for assessing distribution of cases data")

qqplot2 <- ggplot(data = cases_pop_density, aes(sample = Pop_dens_sq_km)) + geom_qq() + stat_qq_line() +
  labs(title = "QQ-Plot for assessing distribution of population density")

qqplots1and2 <- grid.arrange(qqplot1, qqplot2, ncol = 2)

(qqnorm(cases_pop_density$accumulated_sum)) ##qqplot indicates non-gaussian distribution

(qqnorm(cases_pop_density$Pop_dens_sq_km)) ##qqplot indicates non-gaussian distribution

#Spearman test for non-gaussian distribution continuous

cor.test(cases_pop_density$Pop_dens_sq_km, cases_pop_density$accumulated_sum, method = "spearman")

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
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 3)) +
  geom_smooth(method = "lm",
              size = 1) +
  scale_y_continuous(breaks = seq(0, 60000, by = 2500))

infections_seoul <- patientInfo[province == "Seoul",
                                .(count = .N), 
                                by = c("confirmed_date")][, accumulated_sum := cumsum(count)]
is_plot <- ggplot(infections_seoul, aes(x = confirmed_date,
                                        y = accumulated_sum)) +
  geom_line(size = 2,
            color = "black") + 
  labs(title = "Total number of confirmed cases in Seoul",
       x = "Date",
       y = "Accumulated number of cases") +
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 3))

weather_seoul_2020 <- Weather[date >= "2020-01-01" & date <= "2020-05-31" & province == "Seoul"][, c("date", "avg_temp")] #preparing weather dataset. Rising average temperature could affect floating population average

avg_temp_2020 <- ggplot(weather_seoul_2020, aes(x = date, y = avg_temp)) +
  geom_line() + 
  labs(title = "Daily Average Temperature in Seoul",
       x = "Date",
       y = "Average Temperature") +
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 3))

#Third hypothesis: Age groups are affected by different infection durations (Kedi)

patientInfo[, age := as.numeric(gsub("s", "", age))][,lengthcovid := (released_date - confirmed_date)] #What about deceased date? OR operator possible?

patientInfo$age <- as.factor(patientInfo$age)

mean_length <-patientInfo[, .(mean_length = mean(lengthcovid, na.rm = TRUE)), 
                          by = age][order(age)]

mean_length <- mean_length[age != 100]

mean_length$age <- as.factor(mean_length$age)

plot_b <- ggplot(patientInfo,
                 aes(x = age,
                     y = lengthcovid)) +
  geom_boxplot() +
  labs(title = "Average length of COVID-19 infection by age group",
       x = "Age group",
       y = "Amount of time in days") +
  theme_bw()

#patientInfo dataset

infections_by_province <- patientInfo[, .(count = .N),by = c("province", "confirmed_date")][, accumulated_sum := cumsum(count)]
infections_by_province[, n_infections := count]

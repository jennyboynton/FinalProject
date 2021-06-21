install.packages("skimr")
install.packages("gapminder")
library(tidyverse)
library(lubridate)
library(moderndive)
library(dplyr)
library(skimr)
library("gapminder")


#load two datasets:

meat_consumption <- read_csv('DP_LIVE_23042021010752391.csv')

cardiovascular_deathrate <- read_csv('cardiovascular-disease-death-rates.csv')

gapminder <- gapminder

#Create new column "country" and recode all country codes to appropriate country in new column in meat consumption data

meat_consumption_countrycode <- meat_consumption %>% 
  mutate(country = recode(LOCATION,
                          AUS = "Australia",
                          CAN = "Canada",
                          JPN = "Japan",
                          KOR = "Korea",
                          MEX = "Mexico",
                          NZL = "New Zealand",
                          TUR = "Turkey",
                          USA = "United States",
                          ARG = "Argentina",
                          BRA = "Brazil",
                          CHL = "Chile",
                          CHN = "China",
                          COL = "Colombia",
                          EGY = "Egypt",
                          ETH = "Ethiopia",
                          IND = "India",
                          IDN = "Indonesia",
                          IRN = "Iran",
                          ISR = "Israel",
                          KAZ = "Kazakhstan",
                          MYS = "Malayasia",
                          NGA = "Nigeria",
                          PAK = "Pakistan",
                          PRY = "Paraguay",
                          PER = "Peru",
                          PHL = "Philippines",
                          RUS = "Russia",
                          SAU = "Saudia Arabia",
                          ZAF = "South Africa",
                          THA = "Thailand",
                          UKR = "Ukraine",
                          VNM = "Vietnam",
                          NOR = "Norway",
                          CHE = "Switzerland",
                          GBR = "United Kingdom",
                          WLD = "NA",
                          OECD = "NA",
                          BRICS = "NA"))

#tidy data and eliminate columns "FREQUENCY", "LOCATION" and "Flag Codes"
new_meat_consumption <- select(meat_consumption_countrycode, INDICATOR, SUBJECT, MEASURE, TIME, Value, country)

#tidy data and separate by measurement type
Meat_KG <- new_meat_consumption %>% 
  filter(!is.na(Value)) %>%
  filter(MEASURE == "KG_CAP")

Meat_TONNE <- new_meat_consumption %>% 
  filter(!is.na(Value)) %>% 
  filter(MEASURE == "THND_TONNE")

#remove NA that didn't work first time

Meat_KG <- Meat_KG %>% filter(!grepl('NA', country))

Meat_TONNE <- Meat_TONNE %>% filter(!grepl('NA', country))

#more cleaning 
#round decimals to 2 decimal places
Meat_KG <- Meat_KG %>%
  mutate(value = round(Value,2),
         Value = NULL)

#Remember that the OECD's report included forecasted numbers all the way until 2026 so eliminate years after 2021
Meat_KG <- Meat_KG %>%
  filter(TIME < 2022)

#convert kg into lbs per capita
Meat_LB <- Meat_KG %>%
  mutate(value_lb = round(value*2.20462,2),
         value = NULL)

#eliminate column "MEASURE" now that it is all the same
Meat_LB <- select(Meat_LB, INDICATOR, SUBJECT, TIME, value_lb, country)

#look at raw data
glimpse(Meat_LB)

# plot w x-axis as year, y-axis as value, color as type of meat & separated by country
## Plot 1

ggplot(Meat_LB, aes(TIME, value_lb, color = SUBJECT)) + geom_point()+ geom_line(size=2)+ facet_wrap(~country)+
  theme_classic()+
  labs(x = "Years" , y = 'Per Capita Consumption in Pounds', color = "Type of Meat")


#------------------------------------------
#plot with meat totaled

Meat_LB_time <- Meat_LB %>%
  spread(TIME,value_lb)

#take care of those NAs
Meat_LB_time[is.na(Meat_LB_time)] <- 0 

#reformat(other one irrelevant)
Meat_LB_subject <- Meat_LB %>%
  spread(SUBJECT,value_lb)

#take care of those NAs again
Meat_LB_subject[is.na(Meat_LB_subject)] <- 0 

## Plot 2

# graph total meat over time by country
Meat_LB_subject %>% group_by(country, TIME)%>%
  summarize(total_meat = sum(c(BEEF, POULTRY, SHEEP, PIG)))%>%
  ggplot(aes(TIME, total_meat, color=country))+
  geom_line()+ geom_line(size=2)+
  labs(y='Per Capita Consumption in Pounds', x='Year', color = "Country", title='Total Meat Consumption by Country')+
  theme_classic()+
  theme(text = element_text(size=11),
        axis.text.x = element_text(angle=90, hjust=1))


# This showed me that there seemed to be a slight upward trend in meat consumption in most countries over time, however due to the color scheme it was challenging to tell some countries apart, so I decided to also split the graphs apart by continent to see if that gave more clarity:
## PROBLEM - continent not included in this data, will have to combine data first to accomplish this  

continent <- gapminder %>% select(country, continent)

continent_meat_consumption <- left_join(Meat_LB_subject, continent)

continent_meat_consumption <- continent_meat_consumption %>% filter(!is.na(continent))

#graph wasn't working so had to create a new column
continent_meat_consumption1 <- continent_meat_consumption %>% mutate(sum = rowSums(.[4:7]))
  
## Plot 3

#too hard to tell certain countries apart so manually entered color scale
continent_meat_consumption1 %>% ggplot(aes(TIME, sum, color= country))+ geom_point()+ geom_line(size=2)+
  labs(y='Per Capita Consumption in Pounds', x='Year', color = "Country", title='Total Meat Consumption by Country')+
  theme_classic()+
  theme(text = element_text(size=11),
        axis.text.x = element_text(angle=90, hjust=1)) + facet_wrap(~continent)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#FF00CC", "#669900", "#33FFFF", "#6666FF", "#FF9900", "#000000", "#00FFFF", "#660000", "#00FFCC", "#FF9900", "#666666", "#33FF99", "#FF6600", "#993300", "#99FF33", "#3366FF", "#FF3366", "#CCFF99"))

#-------------------------------------------
#Explore cardiovascular data quickly

#change column name
cardio_deathrate <- cardiovascular_deathrate %>% rename(deathrate=`Deaths - Cardiovascular diseases - Sex: Both - Age: Age-standardized (Rate)`)


glimpse(cardio_deathrate)

#ggplot(cardio_deathrate, aes(Year, deathrate, color = Entity)) + geom_point()+ geom_line(size=2)+
  #theme_classic()+
  #labs(x = "Years" , y = 'Death Rate', color = "Country")

#Too many countries to see the graph so lets left join the data but only look at this part

#Change column names first

meat_lb <- Meat_LB %>% rename(Year=TIME)

cardio_deathrate <- cardio_deathrate %>% rename(country=Entity)

#---------------------------------------------------

#combine data

combined_data <- left_join(meat_lb, cardio_deathrate)

#filter out unecessary columns

combined_data <- select(combined_data, SUBJECT, Year, value_lb, country, deathrate)

#filter out nas

combined_data <- combined_data %>% filter(!is.na(deathrate))

## Plot 4

ggplot(combined_data, aes(Year, deathrate, color = country)) + geom_point()+ geom_line(size=2)+
  theme_classic()+
  labs(x = "Years" , y = 'Death Rate', color = "Country")+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#FF00CC", "#669900", "#33FFFF", "#6666FF", "#FF9900", "#000000", "#00FFFF", "#660000", "#00FFCC", "#FF9900", "#666666", "#33FF99", "#FF6600", "#993300", "#99FF33", "#3366FF", "#FF3366", "#CCFF99"))


#----------------------------------------------------

## Plot 5
#plot combined data

ggplot(combined_data, aes(value_lb, deathrate, color = country)) + geom_point()+ geom_line(size=2)+
  theme_classic()+
  labs(x = "Per Capita Consumption in Pounds" , y = 'Death Rate', color = "Country")+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#FF00CC", "#669900", "#33FFFF", "#6666FF", "#FF9900", "#000000", "#00FFFF", "#660000", "#00FFCC", "#FF9900", "#666666", "#33FF99", "#FF6600", "#993300", "#99FF33", "#3366FF", "#FF3366", "#CCFF99", "#F3FF33", "#8D33FF", "#F5CBF1"))


#model

model_combined_data <- lm(deathrate ~ value_lb, data = combined_data)
summary(model_combined_data)

# split by percentage

percentage_deathrate <- mutate(combined_data, percentage = percent_rank(deathrate),
                                          percentage = case_when(percentage > 0 & percentage < 0.25 ~ "low",
                                                                 percentage > 0.25 & percentage < 0.50 ~ "low-medium",
                                                                 percentage > 0.50 & percentage < 0.75 ~ "high-medium",
                                                                 percentage == 0.75 | percentage < 1.0 ~ "high")) 

##PLot 6
ggplot(percentage_deathrate, aes(value_lb, deathrate, color = country)) + geom_point()+ geom_line(size=2)+
  theme_classic()+
  labs(x = "Per Capita Consumption in Pounds" , y = 'Death Rate', color = "Country")+ facet_wrap(~percentage, scale="free")+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#FF00CC", "#669900", "#33FFFF", "#6666FF", "#FF9900", "#000000", "#00FFFF", "#660000", "#00FFCC", "#FF9900", "#666666", "#33FF99", "#FF6600", "#993300", "#99FF33", "#3366FF", "#FF3366", "#CCFF99", "#F3FF33", "#8D33FF", "#F5CBF1"))


# maybe only in half will make more sense

percentage_deathrate_half <- mutate(combined_data, percentage = percent_rank(deathrate),
                               percentage = case_when(percentage > 0 & percentage < 0.5 ~ "low",
                                                      percentage == 0.5 | percentage < 1.0 ~ "high")) 

##Plot 7
ggplot(percentage_deathrate_half, aes(value_lb, deathrate, color = country)) + geom_point()+ geom_line(size=2)+
  theme_classic()+
  labs(x = "Per Capita Consumption in Pounds" , y = 'Death Rate', color = "Country")+ facet_wrap(~percentage, scale="free")+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#FF00CC", "#669900", "#33FFFF", "#6666FF", "#FF9900", "#000000", "#00FFFF", "#660000", "#00FFCC", "#FF9900", "#666666", "#33FF99", "#FF6600", "#993300", "#99FF33", "#3366FF", "#FF3366", "#CCFF99", "#F3FF33", "#8D33FF", "#F5CBF1"))


#fix issue with japan

percentage_deathrate1 <- percentage_deathrate %>% filter(deathrate > 79.36)

##PLot 6 Redo
ggplot(percentage_deathrate1, aes(value_lb, deathrate, color = country)) + geom_point()+ geom_line(size=2)+
  theme_classic()+
  labs(x = "Per Capita Consumption in Pounds" , y = 'Death Rate', color = "Country")+ facet_wrap(~percentage, scale="free")+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#FF00CC", "#669900", "#33FFFF", "#6666FF", "#FF9900", "#000000", "#00FFFF", "#660000", "#00FFCC", "#FF9900", "#666666", "#33FF99", "#FF6600", "#993300", "#99FF33", "#3366FF", "#FF3366", "#CCFF99", "#F3FF33", "#8D33FF", "#F5CBF1"))



model_percentage_deathrate1 <- percentage_deathrate1 %>% 
  split(.$percentage) %>% 
  map(~lm(deathrate ~ value_lb, data = .)) 

summary(model_percentage_deathrate1[[1]])
summary(model_percentage_deathrate1[[2]])
summary(model_percentage_deathrate1[[3]])
summary(model_percentage_deathrate1[[4]])


map(model_percentage_deathrate1, summary) %>% 
  map_dfr("r.squared")

percentage_deathrate_half1<- percentage_deathrate_half %>% filter(deathrate > 79.36)


##Plot 7 redo
ggplot(percentage_deathrate_half1, aes(value_lb, deathrate, color = country)) + geom_point()+ geom_line(size=2)+
  theme_classic()+
  labs(x = "Per Capita Consumption in Pounds" , y = 'Death Rate', color = "Country")+ facet_wrap(~percentage, scale="free")+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#FF00CC", "#669900", "#33FFFF", "#6666FF", "#FF9900", "#000000", "#00FFFF", "#660000", "#00FFCC", "#FF9900", "#666666", "#33FF99", "#FF6600", "#993300", "#99FF33", "#3366FF", "#FF3366", "#CCFF99", "#F3FF33", "#8D33FF", "#F5CBF1"))


#model & map

model_percentage_deathrate_half1 <- percentage_deathrate_half1 %>% 
  split(.$percentage) %>% 
  map(~lm(deathrate ~ value_lb, data = .)) 

summary(model_percentage_deathrate1[[1]])
summary(model_percentage_deathrate1[[2]])

#adjust & remodel combined data in case problem values were having impact
combined_data1 <- combined_data %>% filter(deathrate > 79.36)

model_combined_data1 <- lm(deathrate ~ value_lb, data = combined_data1)
summary(model_combined_data1)

#mapping based off other variables

model_combined_data2 <- combined_data1 %>% 
  split(.$SUBJECT) %>% 
  map(~lm(deathrate ~ value_lb, data = .))

summary(model_combined_data2[[1]])
summary(model_combined_data2[[2]])
summary(model_combined_data2[[3]])
summary(model_combined_data2[[4]])

#multiple linear regression model to see which variables account for most of the R-squared

model_combined_data3 <- lm(deathrate ~ value_lb+country, data = combined_data1)
summary(model_combined_data3)

model_combined_data4 <- lm(deathrate ~ value_lb+country+SUBJECT, data = combined_data1)
summary(model_combined_data4)

model_combined_data5 <- lm(deathrate ~ value_lb+country+SUBJECT+Year, data = combined_data1)
summary(model_combined_data5)


##taking a country with a super negative coefficient to look at::


combined_data_peru <- combined_data %>% filter(country == "Peru")
combined_data_egypt <- combined_data %>% filter(country == "Egypt")
combined_data_russia <- combined_data %>% filter(country == "Russia")

ggplot(combined_data_peru, aes(value_lb, deathrate, color = SUBJECT)) + geom_point()+ geom_line(size=2)+
  theme_classic()+
  labs(x = "Per Capita Consumption in Pounds" , y = 'Death Rate', color = "Country")

ggplot(combined_data_egypt, aes(value_lb, deathrate)) + geom_point()+ geom_line(size=2)+
  theme_classic()+
  labs(x = "Per Capita Consumption in Pounds" , y = 'Death Rate', color = "Country")

ggplot(combined_data_russia, aes(value_lb, deathrate, color = SUBJECT)) + geom_point()+ geom_line(size=2)+
  theme_classic()+
  labs(x = "Per Capita Consumption in Pounds" , y = 'Death Rate', color = "Country")


#looking for coefficent of beef

SUBJECTa = combined_data1 %>% factor(SUBJECT=c("BEEF", "PIG", "POULTRY", "SHEEP"))
a.lm = lm(deathrate ~ value_lb + SUBJECTa, data= combined_data1)

relevel

map(model_combined_data4, summary) %>% 
  map_dfr("coefficients")


model_combined_dataref <- lm(deathrate ~ value_lb+SUBJECT, data = combined_data1)
summary(model_combined_dataref)
##so now lets bring in gapminder and divide by income brackets and see if higher income has more of a negative correlation(bc more money for medical costs) and lower income has negative correlation


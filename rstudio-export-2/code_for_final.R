install.packages("gapminder")
install.packages("skimr")
library(tidyverse)
library(lubridate)
library("gapminder")
library(moderndive)
library(dplyr)
library(skimr)

#load two datasets:

meat_consumption <- read_csv('DP_LIVE_23042021010752391.csv')

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

#look at raw data
glimpse(Meat_LB)

# plot w x-axis as year, y-axis as value, color as type of meat & separated by country

ggplot(Meat_LB, aes(TIME, value_lb, color = SUBJECT)) + geom_point()+ geom_line(size=2)+ facet_wrap(~country)+
  theme_classic()+
  labs(x = "Years" , y = "Value", color = "Type of Meat")

#plot noticeable countries 
Meat_LB %>%
  filter(country == 'United States')%>%
  ggplot(aes(TIME, value_lb, color=SUBJECT, group=SUBJECT))+
  geom_line(size=2)+
  labs(y='Per Capita Consumption in Pounds', x='Year',title='Meat Consumption')+
  theme_minimal()

Meat_LB %>%
  filter(country == 'Argentina')%>%
  ggplot(aes(TIME, value_lb, color=SUBJECT, group=SUBJECT))+
  geom_line(size=2)+
  labs(y='Per Capita Consumption in Pounds', x='Year',title='Meat Consumption')+
  theme_minimal()

Meat_LB %>%
  filter(country == 'Israel')%>%
  ggplot(aes(TIME, value_lb, color=SUBJECT, group=SUBJECT))+
  geom_line(size=2)+
  labs(y='Per Capita Consumption in Pounds', x='Year',title='Meat Consumption')+
  theme_minimal()

Meat_LB %>%
  filter(country == 'New Zealand')%>%
  ggplot(aes(TIME, value_lb, color=SUBJECT, group=SUBJECT))+
  geom_line(size=2)+
  labs(y='Per Capita Consumption in Pounds', x='Year',title='Meat Consumption')+
  theme_minimal()

Meat_LB %>%
  filter(country == 'Paraguay')%>%
  ggplot(aes(TIME, value_lb, color=SUBJECT, group=SUBJECT))+
  geom_line(size=2)+
  labs(y='Per Capita Consumption in Pounds', x='Year',title='Meat Consumption')+
  theme_minimal()

#------------------------------------------
#plot with meat totaled

Meat_LB_time <- Meat_LB %>%
  spread(TIME,value_lb)

#take care of those NAs
Meat_LB_time[is.na(Meat_LB_time)] <- 0 

#reformat
Meat_LB_subject <- Meat_LB %>%
  spread(SUBJECT,value_lb)

#take care of those NAs again
Meat_LB_subject[is.na(Meat_LB_subject)] <- 0 

# graph total meat over time by country
Meat_LB_subject %>% group_by(country, TIME)%>%
  summarize(total_meat = sum(c(BEEF, POULTRY, SHEEP, PIG)))%>%
  ggplot(aes(TIME, total_meat, color=country))+
  geom_line()+
  theme_minimal()+
  theme(text = element_text(size=9),
        axis.text.x = element_text(angle=90, hjust=1))+ 
  facet_wrap(~country, nrow=5, ncol=10)
#---------------------------------------------------

#combine data

combined_data <- left_join(Meat_LB, gapminder)

#whoops lets rename TIME and year so they combine too

gapminder2.0 <- gapminder %>% rename(TIME=year)

#try again

combined_data <- left_join(Meat_LB, gapminder2.0)

#lets filter out those NAs

combined_data <- combined_data %>% filter(!is.na(continent)) %>% filter(!is.na(lifeExp)) %>% filter(!is.na(pop)) %>% filter(!is.na(gdpPercap))


#time to graph: life expectancy 
ggplot(combined_data, aes(value_lb, lifeExp, color = SUBJECT)) + geom_point()+ geom_line(size=2)+ facet_wrap(~country, scale = "free")+
  theme_classic()+
  labs(x = "Value in LBs", y = "Life Expectancy", color = "Type of Meat")

#first model(multiple regression)

model_combined_data <- lm(lifeExp ~ value_lb+country+SUBJECT+TIME, data = combined_data)
summary(model_combined_data)

#Now graph same thing except with meat type totaled into one value and no facet wrap

Total_meat_vs_lifeExp <- left_join(Meat_LB_subject, gapminder2.0)
Total_meat_vs_lifeExp <- Total_meat_vs_lifeExp %>% filter(!is.na(continent)) %>% filter(!is.na(lifeExp)) %>% filter(!is.na(pop)) %>% filter(!is.na(gdpPercap))


Total_meat_vs_lifeExp %>% group_by(country, lifeExp)%>%
  summarize(total_meat = sum(c(BEEF, POULTRY, SHEEP, PIG)))%>%
  ggplot(aes(total_meat, lifeExp, color=country))+
  geom_line(size = 2)+
  theme_classic()+
  theme(text = element_text(size=11),
        axis.text.x = element_text(angle=90, hjust=1))+
  labs(y = "Life Expectancy" , x = "Per Capita Consumption in Pounds", color = "Country", title='Total Meat Consumption vs. Life Expectancy by Country')

#sum the meat values across rows
Totalmeat_vs_lifeExp <- Total_meat_vs_lifeExp %>%  mutate(sum = rowSums(.[5:8]))

#Second model
model_total_meat <- lm(lifeExp ~ sum+country+TIME, data = Totalmeat_vs_lifeExp)
summary(model_total_meat)

# change country to continent
combined_data %>% ggplot(aes(value_lb, lifeExp, color = SUBJECT))+
  geom_line(size=2)+
  theme_classic()+theme(text = element_text(size=11),
                        axis.text.x = element_text(angle=90, hjust=1))+
  facet_wrap(~continent)+
  labs(y = "Life Expectancy" , x = "Per Capita Consumption in Pounds", color = "Meat Type", title='Meat Consumption vs. Life Expectancy by Continent')


#model 3
model_continent_separatemeattypes <- lm(lifeExp ~ value_lb+continent+TIME+SUBJECT, data = combined_data)
summary(model_continent_separatemeattypes)

# Now graph with meat types summed again

Totalmeat_vs_lifeExp %>%
  ggplot(aes(sum, lifeExp, color=continent))+
  geom_line(size = 2)+
  theme_classic()+
  theme(text = element_text(size=11),
        axis.text.x = element_text(angle=90, hjust=1))+
  labs(y = "Life Expectancy" , x = "Per Capita Consumption in Pounds", color = "Continent", title='Total Meat Consumption vs. Life Expectancy by Continent')

#model 4
model_meatsum_continent <- lm(lifeExp ~ sum+continent+TIME, data = Totalmeat_vs_lifeExp)
summary(model_meatsum_continent)

#now GDP vs meat consumption
#create function to make labels in thousandths cleaner(ex. 1000 = 1k)
addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}

#GDP vs meat by type
ggplot(combined_data, aes(gdpPercap, value_lb, color = SUBJECT)) + geom_point()+ geom_line(size=2)+ facet_wrap(~country, scale = "free")+
  theme_classic()+
  labs(x = "GDP Per Capita" , y = "Per Capita Consumption in Pounds", color = "Type of Meat", title = "GDP Per Capita vs. Meat Consumption by Country") + scale_x_continuous(labels = addUnits)

#model 5
model_meattype_GDP <- lm(lifeExp ~ value_lb+continent+TIME+SUBJECT, data = combined_data)
summary(model_meattype_GDP)


#Now graph total meat vs GDP by continent
Total_meat_vs_lifeExp %>% group_by(country, gdpPercap)%>%
  summarize(total_meat = sum(c(BEEF, POULTRY, SHEEP, PIG)))%>%
  ggplot(aes(x = gdpPercap, y = total_meat, color=country))+
  geom_line(size = 2)+
  theme_classic()+
  theme(text = element_text(size=11),
        axis.text.x = element_text(angle=90, hjust=1))+
 # facet_wrap(~country, scale = "free", nrow=5, ncol=10)+ theme(legend.position="none") +
  scale_x_continuous(labels = addUnits)+
  labs(x = "GDP Per Capita" , y = 'Per Capita Consumption in Pounds', color = "Country", title='Total Meat Consumption vs. GDP Per Capita')

# model 6
model_meatsum_GDP <- lm(lifeExp ~ sum+continent+TIME, data = Totalmeat_vs_lifeExp)
summary(model_meatsum_GDP)


#Now add column that divides data by percentage into "high", "high-medium", "low-medium" and "low" based on GDP to regular combined data
Meat_vs_GDPperc <- mutate(combined_data, percentage = percent_rank(gdpPercap),
                                          percentage = case_when(percentage > 0 & percentage < 0.25 ~ "low",
                                                                 percentage > 0.25 & percentage < 0.50 ~ "low-medium",
                                                                 percentage > 0.50 & percentage < 0.75 ~ "high-medium",
                                                                 percentage == 0.75 | percentage < 1.0 ~ "high")) 
#Now add column that divides data by percentage into "high", "high-medium", "low-medium" and "low" based on GDP to data containing summed meat values

Totalmeat_vs_GDPperc <- mutate(Totalmeat_vs_lifeExp, percentage = percent_rank(gdpPercap),
                          percentage = case_when(percentage > 0 & percentage < 0.25 ~ "low",
                                                 percentage > 0.25 & percentage < 0.50 ~ "low-medium",
                                                 percentage > 0.50 & percentage < 0.75 ~ "high-medium",
                                                 percentage == 0.75 | percentage < 1.0 ~ "high")) 


#graph with percentage rankings being the facet wrap and color being meat type 

ggplot(Meat_vs_GDPperc, aes(value_lb, lifeExp, color = SUBJECT)) + geom_point()+ geom_line(size=2)+ facet_wrap(~percentage)+
  theme_classic()+
  labs(y = "Life Expectancy" , x = 'Per Capita Consumption in Pounds', color = "Type of Meat", title='Meat Consumption vs. Life Expectancy by Income Percentage')

#model 7

model_Meat_vs_GDPperc <- Meat_vs_GDPperc %>% 
  split(.$percentage) %>% 
  map(~lm(lifeExp ~ value_lb, data = .)) 


#split up to view separate by percentage
summary(model_Meat_vs_GDPperc[[1]])
summary(model_Meat_vs_GDPperc[[2]])
summary(model_Meat_vs_GDPperc[[3]])
summary(model_Meat_vs_GDPperc[[4]])

#view just r-squared values
map(model_Meat_vs_GDPperc, summary) %>% 
  map_dfr("r.squared")

#filter out NA

Totalmeat_vs_GDPperc <- Totalmeat_vs_GDPperc %>% filter(!is.na(percentage))

#Plot again with meat values summed
ggplot(Totalmeat_vs_GDPperc, aes(sum, lifeExp, color = percentage)) + geom_point()+ geom_line(size=2)+
  theme_classic()+
  labs(y = "Life Expectancy" , x = 'Per Capita Consumption in Pounds', color = "Income Percentage", title='Meat Consumption vs. Life Expectancy by Income Percentage')


#model 8
model_Totalmeat_vs_GDPperc <- Totalmeat_vs_GDPperc %>% 
  split(.$percentage) %>% 
  map(~lm(lifeExp ~ sum, data = .)) 

#split up to view separate by percentage
summary(model_Totalmeat_vs_GDPperc[[1]])
summary(model_Totalmeat_vs_GDPperc[[2]])
summary(model_Totalmeat_vs_GDPperc[[3]])
summary(model_Totalmeat_vs_GDPperc[[4]])
 
#view just r-squared values                  
map(model_Totalmeat_vs_GDPperc, summary) %>% 
  map_dfr("r.squared")


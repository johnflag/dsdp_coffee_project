library(stringr)
library(tidyr)
library(dplyr)
library(zoo)
library(data.table)
library(tidystringdist)

######################################################################################################

# Creates the mode function that will be used to estimate some missing values
getmode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

##### LOAD DATASET #####

dir <- "C:/Users/Joao_Bandeira/Documents/DSDP/Cofee Global Project/"

setwd(dir)

# Read Dataset
df <- read.csv('Coffee global - fixed.csv', encoding = 'UTF-8', header=T, na.strings = c(""," ","NA"))

# Remove zero scores
df <- df %>% filter(Total.Cup.Points > 0)


df <- df %>% mutate(Specialty.Target = case_when(Total.Cup.Points >= 80 ~ 'Specialty',
                                                       TRUE ~ 'Not Specialty'))


##### FARM NAME #####
# Remove special characters, lowercase strings, remove broad strings

df$Farm.Name <- str_replace_all(df$Farm.Name, "[[:punct:]]", "")
df$Farm.Name <- as.character(lapply(df$Farm.Name, tolower))
df[df$Farm.Name %in% c('several','several farms', 'various', '-', 'mixed', '', ' ', 'unknown'),'Farm.Name'] = NA
df[df$Farm.Name %in% c('sertao farm'),'Farm.Name'] = 'fazenda do sertao'
sort(unique(df$Farm.Name))


##### VARIETY #####

# Clean special characters from Variety
df$Variety <- str_replace_all(df$Variety, "[[:punct:]]", "")
df$Variety <- with(df, ave(df$Variety, Producer, FUN = function(x) replace(x, is.na(x), getmode(x))))
df$Variety <- with(df, ave(df$Variety, Farm.Name, FUN = function(x) replace(x, is.na(x), getmode(x))))


##### COUNTRY #####

# Replace missing countries based on Owner's mode
df$Country.of.Origin <- with(df, ave(df$Country.of.Origin, Owner, FUN = function(x) replace(x, is.na(x), getmode(x))))


##### REGION #####

# Cean special characters from Region
df$Region <- str_replace_all(df$Region, "[[:punct:]]", "")

##### PRODUCERS #####

# Clean Producers: remove special chars, remove broad strings
df$Producer <- str_replace_all(df$Producer, "[[:punct:]]", "")
df[df$Producer %in% c('vrios productores','blend from various producers', 'blend of various producers','several producers', 'several producres', '', ' '),'Producer'] = NA
df[df$Producer %in% c('rolando lacayo cardenal'),'Producer'] = 'rolando lacayo'
sort(unique(df$Producer))


##### ALTITUDE #####

#Replace Altitude values that seem to be incorrect
df$altitude_mean_meters <- replace(df$altitude_mean_meters, df$altitude_mean_meters==190164, 1901.64)
df$altitude_mean_meters <- replace(df$altitude_mean_meters, df$altitude_mean_meters==11000, 1100)
df$altitude_mean_meters <- replace(df$altitude_mean_meters, df$altitude_mean_meters==110000, 1100)
df$altitude_mean_meters <- replace(df$altitude_mean_meters, df$altitude_mean_meters<100, NA)
sort(unique(df$altitude_mean_meters))

# Create concatenate columns with farm-region and region-country to be used in altitute estimation of missing values
df['Farm.Region'] = paste(df$Farm.Name, df$Region, sep = "-")
df['Region.Country'] = paste(df$Region, df$Country.of.Origin, sep = "-")

# Estimate an altitude for missing values with two criterias: 1st based on farm and reagion and 2nd based on region and country
df$altitude_mean_meters <- with(df, ave(altitude_mean_meters, Farm.Region, FUN = function(x) replace(x, is.na(x), median(x, na.rm = TRUE))))
df$altitude_mean_meters <- with(df, ave(altitude_mean_meters, Region.Country, FUN = function(x) replace(x, is.na(x), median(x, na.rm = TRUE))))
df$Altitude <- df$altitude_mean_meters


##### HARVEST YEAR #####

# Replace incorrect Harvest Year values
df$Harvest.Year <- as.character(df$Harvest.Year)
df$Harvest.Year <- replace(df$Harvest.Year, df$Harvest.Year=='08/09 crop', '2009')
df$Harvest.Year <- replace(df$Harvest.Year, df$Harvest.Year=='23-Jul-10', '2010')
df$Harvest.Year <- replace(df$Harvest.Year, df$Harvest.Year=='Jan-11', '2011')
df$Harvest.Year <- replace(df$Harvest.Year, df$Harvest.Year=='Mar-10', '2010')
df$Harvest.Year <- replace(df$Harvest.Year, df$Harvest.Year=='Spring 2011 in Colombia.', '2011')

# Considering Grading Year as the Harvest Year for missing values
df$Harvest.Year <- substr(df$Harvest.Year, nchar(df$Harvest.Year) - 3, nchar(df$Harvest.Year))
df$Harvest.Year <- as.numeric(as.character(df$Harvest.Year))
df$Harvest.Year <- ifelse(is.na(df$Harvest.Year), substr(df$Grading.Date, nchar(as.character(df$Grading.Date)) - 3, nchar(as.character(df$Grading.Date))), df$Harvest.Year)
sort(unique(df$Harvest.Year))

#Droping columns that won't be used
df <- select(df, c('Country.of.Origin', 'Farm.Name', 'Mill', 'Company', 'Region', 'Producer', 'Altitude', 'In.Country.Partner', 'Harvest.Year', 'Owner', 'Variety', 'Aroma', 'Flavor', 
                   'Aftertaste', 'Acidity', 'Body', 'Balance', 'Uniformity', 'Clean.Cup', 'Sweetness', 'Cupper.Points', 'Total.Cup.Points', 'Moisture', 'Category.One.Defects',
                   'Quakers', 'Color', 'Category.Two.Defects', 'Certification.Body', 'Processing.Method'))

# count NAs in each column
colSums(is.na(df))
apply(df, 2, function(x) length(unique(x)))

write.csv(df, file = 'Coffee global - cleaned.csv')


#############################################################################################

#SCRIPT USED TO IDENTIFY SIMILAR STRINGS
compare <- tidy_comb_all(unique(df$Producer), unique(df$Producer))
comparisons <- tidy_stringdist(compare) %>% 
  mutate(sim = 1-jaccard) %>% 
  select(V1, V2, sim)
recommendation <- comparisons %>% 
  group_by(V1) %>% 
  summarise(max_sim = max(sim)) %>% 
  ungroup()
comparisons %>% 
  inner_join(recommendation, by = c("V1" = "V1", "sim" = "max_sim"))


boxplot(Total.Cup.Points~Owner, data=df)


reg <- lm(Category.Two.Defects~Total.Cup.Points, data=df)
summary(reg)
with(df,plot(Category.Two.Defects, Total.Cup.Points))
abline(reg)

table(df$Variety)


#Removing NAs and zeros
df <- df %>% drop_na(altitude_mean_meters, Country.of.Origin)


sum(is.na(df$altitude_mean_meters))
sum(is.na(df$Processing.Method))
sum(df$Processing.Method=='')


df %>% group_by(Farm.Name, Variety)
typeof(ls())

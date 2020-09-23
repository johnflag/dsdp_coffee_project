library(stringr)
library(tidyr)
library(dplyr)
library(zoo)
library(data.table)
library(tidystringdist)

######################################################################################################

##### LOAD DATASET #####

dir <- "C:/Users/javba/OneDrive/Documents/GitHub/dsdp_coffee_project"

setwd(dir)

# Read Dataset
df <- read.csv('Coffee_new_datapoints.csv', encoding = 'UTF-8', header=T, na.strings = c(""," ","NA"))

#MOISTURE

df$Moisture <- str_replace_all(df$Moisture, "[[:punct:]]", "")
df$Moisture <- as.numeric(df$Moisture) / 100

#CATERGORY 1 and 2 DEFECTS

df$Category.One.Defects <- str_replace(df$Category.One.Defects, " full defects", "")
df$Category.Two.Defects <- str_replace(df$Category.Two.Defects, " full defects", "")


#SELECT ONLY IMPORTANT COLUMNS
df <- df %>% select(c('Country.of.Origin', 'Farm.Name', 'Mill', 'Company', 'Altitude', 'Region', 'Producer', 'Number.of.Bags', 'Bag.Weight', 'In.Country.Partner',
                      'Harvest.Year', 'Grading.Date', 'Owner', 'Variety', 'Processing.Method', 'Aroma', 'Flavor', 'Aftertaste', 'Acidity', 'Body', 'Balance', 'Uniformity', 
                      'Clean.Cup', 'Sweetness', 'Overall', 'Total.Cup.Points', 'Moisture', 'Category.One.Defects', 'Quakers', 'Color', 'Category.Two.Defects'))


write.csv(df, file = 'New_Dataset_Cleaned.csv', fileEncoding = 'UTF-8')










##### FARM NAME #####
# Remove special characters, lowercase strings

df$Farm.Name <- str_replace_all(df$Farm.Name, "[[:punct:]]", "")
df$Farm.Name <- as.character(lapply(df$Farm.Name, tolower))
df[df$Farm.Name %in% c('non'),'Farm.Name'] = NA


##### MILL #####
# Remove special characters, lowercase strings, remove 'non'

df$Mill <- str_replace_all(df$Mill, "[[:punct:]]", "")
df$Mill <- as.character(lapply(df$Farm.Name, tolower))
df[df$Mill %in% c('non'),'Mill'] = NA

##### COMPANY #####
# Remove special characters, lowercase strings, remove 'non'

df$Company <- str_replace_all(df$Company, "[[:punct:]]", "")
df$Company <- as.character(lapply(df$Company, tolower))
df[df$Company %in% c('non'),'Company'] = NA

##### REGION #####
# Remove special characters, lowercase strings, remove 'non'

df$Region <- str_replace_all(df$Region, "[[:punct:]]", "")
df$Region <- as.character(lapply(df$Region, tolower))
df[df$Region %in% c('non', 'none'),'Region'] = NA

##### PRODUCER #####
# Remove special characters, lowercase strings, remove 'non'

df$Producer <- str_replace_all(df$Producer, "[[:punct:]]", "")
df$Producer <- as.character(lapply(df$Producer, tolower))
df[df$Producer %in% c('non', 'none'),'Producer'] = NA

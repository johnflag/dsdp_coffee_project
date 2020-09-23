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

##### LOAD DATASETS #####

dir <- "C:/Users/javba/OneDrive/Documents/GitHub/dsdp_coffee_project"

setwd(dir)

# Read Dataset
df_old <- read.csv('Coffee global - fixed_rows.csv', encoding = 'UTF-8', header=T, na.strings = c(""," ","NA"))
df_old <- df_old %>% select(c('Country.of.Origin', 'Farm.Name', 'Mill', 'Company', 'altitude_mean_meters', 'Region', 'Producer', 'Number.of.Bags', 'Bag.Weight', 'In.Country.Partner',
                      'Harvest.Year', 'Grading.Date', 'Owner.1', 'Variety', 'Processing.Method', 'Aroma', 'Flavor', 'Aftertaste', 'Acidity', 'Body', 'Balance', 'Uniformity', 
                      'Clean.Cup', 'Sweetness', 'Cupper.Points', 'Total.Cup.Points', 'Moisture', 'Category.One.Defects', 'Quakers', 'Color', 'Category.Two.Defects'))

df_old <- df_old %>% rename(Owner = Owner.1,Altitude = altitude_mean_meters , Overall = Cupper.Points)

df_new <- read.csv('New_Dataset_Cleaned.csv', encoding = 'UTF-8', header=T, na.strings = c(""," ","NA"))
df_new <- subset(df_new, select=-c(X))

df <- rbind(df_old,df_new)


# Remove scores = 0
df <- df %>% filter(Total.Cup.Points > 0)


##### OWNER #####
# Remove special characters, lowercase strings, remove broad strings
df$Owner <- str_replace_all(df$Owner, "[[:punct:]]", "")
df$Owner <- as.character(lapply(df$Owner, tolower))

##### COMPANY #####
# Remove special characters, lowercase strings, remove broad strings
df$Company <- str_replace_all(df$Company, "[[:punct:]]", "")
df$Company <- as.character(lapply(df$Company, tolower))
df[df$Company %in% c('unex guatemala s a'),'Company'] = 'unex guatemala sa'
df[df$Company %in% c('ciracafe  cia sca'),'Company'] = 'racafe  cia sca'
df[df$Company %in% c('cafebras comercio de cafés do brasil sa'),'Company'] = 'cafebras comércio de cafes do brasil'
df[df$Company %in% c('mercon guatemala s a'),'Company'] = 'mercon guatemala sa'



##### FARM NAME #####
# Remove special characters, lowercase strings, remove broad strings

df$Farm.Name <- str_replace_all(df$Farm.Name, "[[:punct:]]", "")
df$Farm.Name <- as.character(lapply(df$Farm.Name, tolower))
df[df$Farm.Name %in% c('several','several farms', 'various', '-', 'mixed', '', ' ', 'unknown', 'unkown'),'Farm.Name'] = NA
df[df$Farm.Name %in% c('sertao farm'),'Farm.Name'] = 'fazenda do sertao'
sort(unique(df$Farm.Name))

##### COUNTRY #####

# Replace missing countries based on Owner's mode
df$Country.of.Origin <- with(df, ave(df$Country.of.Origin, Owner, FUN = function(x) replace(x, is.na(x), getmode(x))))
sort(unique(df$Country.of.Origin))

##### PRODUCERS #####

# Clean Producers: remove special chars, remove broad strings
df$Producer <- str_replace_all(df$Producer, "[[:punct:]]", "")
df$Producer <- as.character(lapply(df$Producer, tolower))
df[df$Producer %in% c('vrios productores','blend from various producers', 'blend of various producers','several producers', 'several producres', '', ' '),'Producer'] = NA
df[df$Producer %in% c('rolando lacayo cardenal'),'Producer'] = 'rolando lacayo'
sort(unique(df$Producer))

##### VARIETY #####

# Clean special characters
df$Variety <- str_replace_all(df$Variety, "[[:punct:]]", "")

df$Variety <- with(df, ave(df$Variety, Producer, FUN = function(x) replace(x, is.na(x), getmode(x))))
df$Variety <- with(df, ave(df$Variety, Farm.Name, FUN = function(x) replace(x, is.na(x), getmode(x))))
df$Variety <- with(df, ave(df$Variety, Country.of.Origin, FUN = function(x) replace(x, is.na(x), getmode(x))))
df$Variety <- ifelse(is.na(df$Variety), "Other", df$Variety)
sort(unique(df$Variety))

##### PROCESSING METHOD #####
df$Processing.Method <- with(df, ave(df$Processing.Method, Producer, FUN = function(x) replace(x, is.na(x), getmode(x))))
df$Processing.Method <- with(df, ave(df$Processing.Method, Farm.Name, FUN = function(x) replace(x, is.na(x), getmode(x))))
df$Processing.Method <- ifelse(is.na(df$Processing.Method), "Other", df$Processing.Method)
sort(unique(df$Processing.Method))

##### REGION #####

# Clean special characters from Region
df$Region <- str_replace_all(df$Region, "[[:punct:]]", "")
df$Region <- as.character(lapply(df$Region, tolower))
sort(unique(df$Region))




##### ALTITUDE #####

#Replace Altitude values that seem to be incorrect
df$Altitude <- replace(df$Altitude, df$Altitude==190164, 1901.64)
df$Altitude <- replace(df$Altitude, df$Altitude==11000, 1100)
df$Altitude <- replace(df$Altitude, df$Altitude==110000, 1100)
df$Altitude <- replace(df$Altitude, df$Altitude<100, NA)
sort(unique(df$Altitude))

# Create concatenate columns with farm-region and region-country to be used in altitude estimation of missing values
df['Farm.Region'] = paste(df$Farm.Name, df$Region, sep = "-")
df['Region.Country'] = paste(df$Region, df$Country.of.Origin, sep = "-")

# Estimate an altitude for missing values with two criteria: 1st based on farm and region and 2nd based on region and country
df$Altitude <- with(df, ave(Altitude, Farm.Region, FUN = function(x) replace(x, is.na(x), median(x, na.rm = TRUE))))
df$Altitude <- with(df, ave(Altitude, Region.Country, FUN = function(x) replace(x, is.na(x), median(x, na.rm = TRUE))))
df$Altitude <- with(df, ave(Altitude, Country.of.Origin, FUN = function(x) replace(x, is.na(x), median(x, na.rm = TRUE))))
alt_mean <- mean(df$Altitude, na.rm = TRUE)
df$Altitude = ifelse(is.na(df$Altitude), alt_mean, df$Altitude)
df[is.na(df$Altitude),]

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

####### IN COUNTRY PARTNER ##########
df$In.Country.Partner <- replace(df$In.Country.Partner, df$In.Country.Partner=='Blossom Valley International<U+5BB8><U+5DA7><U+570B><U+969B>', 'Blossom Valley International')
df$In.Country.Partner <- replace(df$In.Country.Partner, df$In.Country.Partner=='Specialty Coffee Ass', 'Specialty Coffee Association')
sort(unique(df$In.Country.Partner))

# count NAs in each column
colSums(is.na(df))
apply(df, 2, function(x) length(unique(x)))


df <- df %>% mutate(Quality.Classification = case_when((Total.Cup.Points >= 80 & Category.One.Defects == 0 & Category.Two.Defects <= 5 & Quakers == 0 & Moisture >=0.09 & Moisture <= 0.12) ~ "Specialty",
                                                       TRUE ~ 'Commodity'))


df$Owner <- ifelse(df$Owner %in% c("juan luis alvarado romero","racafe  cia sca","exportadora de cafe condor sa","kona pacific farmers cooperative","ipanema coffees","cqi taiwan icp cqi","lin chehao krude ","nucoffee","carcafe ltda ci","the coffee source inc","doi tung development project","alfredo bojalil","eileen koyanagi","bourbon specialty coffees","afca","ethiopia commodity exchange","rodolfo carneiro climaco","byron gonzalez","cadexsa","consejo salvadoreño del café","bismarck castro","ceca sa","saul m hernandez ramirez","compañia colombiana agroindustrial sa","essencecoffee","israel eduardo paz garcia","yunnan coffee exchange","jacques pereira carneiro","lusso lab","exportadora atlantic sa"," koju matsuzawa","dane loraas","eric thormaehlen","grounds for health admin","sunvirtue co ltd"),
                   df$Owner, "Other")

df$Company <- ifelse(df$Company %in% c("unex guatemala sa","ipanema coffees","racafe  cia sca","exportadora de cafe condor sa","kona pacific farmers cooperative","carcafe ltda","blossom valley","nucoffee","taiwan coffee laboratory","ecomtrading","the coffee source inc","bourbon specialty coffees","nestlé brasil sa","cadexsa","cecasa","cigrah sa de cv","siembras vision sa","ecom cca sa","cafeorganicomx","essence coffee","yunnan coffee exchange","consejo salvadoreño del café","doi tung development project","exportcafe","lusso coffee lab","red on tree co ltd","cqi","exportadora atlantic sa","exportadora de cafés carmo de minas ltda","mzuzu coffee planters coop union","sustainable harvest","cafebras comércio de cafes do brasil","cigrah","coricafe sa","kawacom uganda ltd","mercon guatemala sa","outspan guatemala sa","retrillas del pacifico s a","sunvirtue co ltd"),
                   df$Company, "Other")

write.csv(df, file = 'Coffee global - cleaned.csv', fileEncoding = 'UTF-8')


df[is.na(df$Altitude),]

#############################################################################################

partner <- sort(table(df$Company), decreasing = TRUE)
write.csv(partner, file = 'partner.csv')

#SCRIPT USED TO IDENTIFY SIMILAR STRINGS
compare <- tidy_comb_all(unique(df$Company), unique(df$Company))
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

#Count Unique
sapply(df, function(x) length(unique(x)))

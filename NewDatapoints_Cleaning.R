library(stringr)
library(tidyr)
library(dplyr)
library(zoo)
library(data.table)
library(tidystringdist)

######################################################################################################

##### LOAD DATASET #####

dir <- "C:/Users/Joao_Bandeira/Documents/DSDP/Cofee Global Project/"

setwd(dir)

# Read Dataset
df <- read.csv('Coffee_new.csv', encoding = 'UTF-8', header=T, na.strings = c(""," ","NA"))

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
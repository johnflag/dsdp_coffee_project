library(dplyr)
library(randomForest)
library(caTools)
library(caret)
library(e1071)

dir <- "C:/Users/javba/OneDrive/Documents/GitHub/dsdp_coffee_project"

setwd(dir)

db <- read.csv('Coffee global - cleaned.csv', encoding = 'UTF-8')

db <- db[!(is.na(db$Country.of.Origin) | db$Country.of.Origin==""), ]
db <- db[!(is.na(db$Altitude) | db$Altitude==""), ]
db <- db[!(is.na(db$Quakers) | db$Quakers==""), ]
db <- db[!(is.na(db$Variety) | db$Variety==""), ]
db <- db[!(is.na(db$Processing.Method) | db$Processing.Method==""), ]
db <- db[!(is.na(db$In.Country.Partner) | db$In.Country.Partner==""), ]
db <- db[!(is.na(db$Harvest.Year) | db$Harvest.Year==""), ]
#db <- db[!(is.na(db$Owner) | db$Owner==""), ]


#TREATMENT OF CATEGORICAL FIELDS
db$Country.of.Origin = factor(db$Country.of.Origin, levels = c('Ethiopia','Guatemala','Brazil','Peru','United States','United States (Hawaii)','Indonesia','China','Costa Rica','Mexico','Uganda','Honduras','Taiwan','Nicaragua','Tanzania, United Republic Of','Kenya','Thailand','Colombia','Panama','Papua New Guinea','El Salvador','Japan','Ecuador','United States (Puerto Rico)','Haiti','Burundi','Vietnam','Philippines','Rwanda','Malawi','Laos','Zambia','Myanmar','Mauritius','Cote d?Ivoire','India', 'Unknown', 'East Timor', 'Yemen'),
                                                              labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39))
db$Variety = factor(db$Variety, levels = c("Other","Catimor","Ethiopian Yirgacheffe","Caturra","Bourbon","SL14","Sumatra","SL34","Hawaiian Kona","SL28","Gesha","Catuai","Pacamara","Sumatra Lintong","Typica","Mundo Novo","Java","Peaberry","Pacas","Mandheling","Yellow Bourbon","Ruiru 11","Arusha","Ethiopian Heirlooms","Moka Peaberry","Sulawesi","Marigojipe","Blue Mountain","Pache Comun","Mocha","Sarchimor","SHG"), labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32))
db$Harvest.Year = factor(db$Harvest.Year, levels = c('2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020','2021'), labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13))
db$Processing.Method = factor(db$Processing.Method, levels = c('Washed / Wet','Natural / Dry','Pulped natural / honey','Semi-washed / Semi-pulped','Other','Unknown'), labels = c(1,2,3,4,5,6))
db$In.Country.Partner = factor(db$In.Country.Partner, levels = c("METAD Agricultural Development plc","Specialty Coffee Association","Specialty Coffee Institute of Asia","Ethiopia Commodity Exchange","Almacaf�","Yunnan Coffee Exchange","Blossom Valley International","AMECAFE","NUCOFFEE","Uganda Coffee Development Authority","Instituto Hondure�o del Caf�","Specialty Coffee Association of Costa Rica","Kenya Coffee Traders Association","Africa Fine Coffee Association","Asociacion Nacional Del Caf�","Centro Agroecol�gico del Caf� A.C.","Salvadoran Coffee Council","Specialty Coffee Association of Indonesia","Brazil Specialty Coffee Association","Asociaci�n Mexicana De Caf�s y Cafeter�as De Especialidad A.C.","Tanzanian Coffee Board","Central De Organizaciones Productoras De Caf� y Cacao Del Per� - Central Caf� & Cacao","Torch Coffee Lab Yunnan","Coffee Quality Institute","Asociaci�n de Caf�s Especiales de Nicaragua","Japan Coffee Exchange","NKG Quality Service (a division of Bernhard Rothfos Intercaf� AG)","CQI Colombia SAS"),
                               labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28))
db$Owner = factor(db$Owner, levels = c("juan luis alvarado romero","racafe  cia sca","exportadora de cafe condor sa","kona pacific farmers cooperative","ipanema coffees","cqi taiwan icp cqi","lin chehao krude ","nucoffee","carcafe ltda ci","the coffee source inc","doi tung development project","alfredo bojalil","eileen koyanagi","bourbon specialty coffees","afca","ethiopia commodity exchange","rodolfo carneiro climaco","byron gonzalez","cadexsa","consejo salvadore�o del caf�","bismarck castro","ceca sa","saul m hernandez ramirez","compa�ia colombiana agroindustrial sa","essencecoffee","israel eduardo paz garcia","yunnan coffee exchange","jacques pereira carneiro","lusso lab","exportadora atlantic sa"," koju matsuzawa","dane loraas","eric thormaehlen","grounds for health admin","sunvirtue co ltd", "Other"),
                  labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36))
db$Company = factor(db$Company, levels = c("unex guatemala sa","ipanema coffees","racafe  cia sca","exportadora de cafe condor sa","kona pacific farmers cooperative","carcafe ltda","blossom valley","nucoffee","taiwan coffee laboratory","ecomtrading","the coffee source inc","bourbon specialty coffees","nestl� brasil sa","cadexsa","cecasa","cigrah sa de cv","siembras vision sa","ecom cca sa","cafeorganicomx","essence coffee","yunnan coffee exchange","consejo salvadore�o del caf�","doi tung development project","exportcafe","lusso coffee lab","red on tree co ltd","cqi","exportadora atlantic sa","exportadora de caf�s carmo de minas ltda","mzuzu coffee planters coop union","sustainable harvest","cafebras com�rcio de cafes do brasil","cigrah","coricafe sa","kawacom uganda ltd","mercon guatemala sa","outspan guatemala sa","retrillas del pacifico s a","sunvirtue co ltd", "Other"),
                  labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40))



db$Quality.Classification = factor(db$Quality.Classification, levels = c('Specialty', 'Commodity'), labels = c(1,2))

#SCALING
db$Category.One.Defects = scale(db$Category.One.Defects)
db$Category.Two.Defects = scale(db$Category.Two.Defects)
db$Altitude = scale(db$Altitude)
db$Quakers = scale(db$Quakers)
db$Moisture = scale(db$Moisture)
db$Flavor = scale(db$Flavor)
db$Aftertaste = scale(db$Aftertaste)
db$Balance = scale(db$Balance)
db$Uniformity = scale(db$Uniformity)
db$Sweetness = scale(db$Sweetness)


######## SAMPLE SPLIT ##########
set.seed(1)
split = sample.split(db$Quality.Classification, SplitRatio = 0.75)
training = subset(db, split == TRUE)
test = subset(db, split == FALSE)

######## RANDOM FOREST ##########
set.seed(1)
rf_class = randomForest(Quality.Classification ~ Owner + Company + Country.of.Origin + Altitude + Variety + Harvest.Year + Processing.Method + In.Country.Partner, data = training, ntree = 50)
rf_prev = predict(rf_class, newdata = test[-35])
confusionMatrix(rf_prev, test[,35])




round(cor(db[13:23]),2)

class <- rpart(formula = Quality.Classification ~ Species + Country.of.Origin, data = db)
print(class)
plot(class)



sum(is.na(db$Moisture))

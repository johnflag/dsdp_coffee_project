library(dplyr)
library(randomForest)
library(caTools)
library(caret)

dir <- "C:/Users/Joao_Bandeira/Documents/DSDP/Cofee Global Project/"

setwd(dir)

db <- read.csv('Coffee global - cleaned.csv', encoding = 'UTF-8')

db <- db[!(is.na(db$Country.of.Origin) | db$Country.of.Origin==""), ]
db <- db[!(is.na(db$Altitude) | db$Altitude==""), ]
db <- db[!(is.na(db$Quakers) | db$Quakers==""), ]
db <- db[!(is.na(db$Variety) | db$Variety==""), ]
db <- db[!(is.na(db$Processing.Method) | db$Processing.Method==""), ]


db <- db %>% mutate(Quality.Classification = case_when((Total.Cup.Points >= 80 & Category.One.Defects == 0 & Category.Two.Defects <= 5 & Quakers == 0) ~ "Specialty",
                                                       TRUE ~ 'Commodity'))

table(db$Quality.Classification)

#boxplot(Total.Cup.Points~Processing.Method, data=db)

db$Country.of.Origin = factor(db$Country.of.Origin, levels = c('Ethiopia','Guatemala','Brazil','Peru','United States','United States (Hawaii)','Indonesia','China','Costa Rica','Mexico','Uganda','Honduras','Taiwan','Nicaragua','Tanzania, United Republic Of','Kenya','Thailand','Colombia','Panama','Papua New Guinea','El Salvador','Japan','Ecuador','United States (Puerto Rico)','Haiti','Burundi','Vietnam','Philippines','Rwanda','Malawi','Laos','Zambia','Myanmar','Mauritius','Cote d?Ivoire','India', 'Unknown'),
                                                              labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37))
db$Variety = factor(db$Variety, levels = c('Arusha','Blue Mountain','Bourbon','Catimor','Catuai','Caturra','Ethiopian Heirlooms','Ethiopian Yirgacheffe','Gesha','Hawaiian Kona','Java','Mandheling','Marigojipe','Moka Peaberry','Mundo Novo','Other','Pacamara','Pacas','Pache Comun','Peaberry','Ruiru 11','SL14','SL28','SL34','Sulawesi','Sumatra','Sumatra Lintong','Typica','Typica"','Yellow Bourbon','Unknown'), labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,28,29,30))
db$Harvest.Year = factor(db$Harvest.Year, levels = c('2009','2010','2011','2012','2013','2014','2015','2016','2017','2018'), labels = c(1,2,3,4,5,6,7,8,9,10))
db$Processing.Method = factor(db$Processing.Method, levels = c('Washed / Wet','Natural / Dry','Pulped natural / honey','Semi-washed / Semi-pulped','Other','Unknown'), labels = c(1,2,3,4,5,6))
db$Quality.Classification = factor(db$Quality.Classification, levels = c('Specialty', 'Commodity'), labels = c(1,2))

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

set.seed(453)

#test2 <- db
#db <- downSample(x = db[,-ncol(db)], y = db$Quality.Classification)
#colnames(db)[31] <- 'Quality.Classification'

split = sample.split(db$Quality.Classification, SplitRatio = 0.75)
training = subset(db, split == TRUE)
test = subset(db, split == FALSE)

class = randomForest(Quality.Classification ~ Country.of.Origin + Altitude, data = training, ntree = 50)
#Country.of.Origin + Processing.Method + Variety + Harvest.Year + Category.One.Defects + Category.Two.Defects + Altitude + Quakers + Moisture
prev = predict(class, newdata = test[-31])

confusionMatrix(prev, test[,31])



round(cor(db[13:23]),2)



class <- rpart(formula = Quality.Classification ~ Species + Country.of.Origin, data = db)
print(class)
plot(class)



sum(is.na(db$Moisture))

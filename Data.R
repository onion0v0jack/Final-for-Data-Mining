rm(list=ls())  #清除所有暫存
graphics.off() #清除圖片的顯示
getwd()        #顯示這個r所在的路徑
library(class) 
library(FNN) 
library(caret) 
library(plyr)
library(dplyr)
library(ggplot2) 
library(tree) 
library(randomForest) 
library(glmnet) 
library(pROC) 
library(gbm) 
library(RColorBrewer)
forest  <-read.csv("covtype.csv",header=T,na.string="?")

#######
#######1. 從母資料forest抽樣得到DATA，並將DATA分成training/testing
attach(forest)
DATA=forest[sample((dim(forest)[1]-dim(forest)[1]%%20)/20),]
train=sample((dim(DATA)[1]-dim(DATA)[1]%%10)*0.9); test=-train
training=DATA[train,]; testing=DATA[test,]

#######
#######2. 將training(有備份)的40+4個Dummy整合成類別資料，稱df
df=training
df <- df %>%
  gather(key=Region, value=region.indicator,Wilderness_Area1:Wilderness_Area4)%>%
  filter(region.indicator==1) %>%
  select(-region.indicator)
table(df$Cover_Type)


#從df這筆資料，將其中四個area資料濃縮成一個variable，並以四個area表示(唯一性)
df$Region <- ifelse(df$Region=="Wilderness_Area1","Rawah",
                    ifelse(df$Region=="Wilderness_Area2","Neota",
                           ifelse(df$Region=="Wilderness_Area3","Comanche Peak","Cache la Poudre")))
#將area1234都改名
df$Region <- as.factor(df$Region) #原本是character 改成factor
df$Cover_Type <- as.character(df$Cover_Type)  #原本是number 改成character
df$Cover_Type <- ifelse(df$Cover_Type==1,"Spruce/Fir",
                        ifelse(df$Cover_Type==2,"Lodgepole Pine",
                               ifelse(df$Cover_Type==3,"Ponderosa Pine",
                                      ifelse(df$Cover_Type==4,"Cottonwood/Willow ",
                                             ifelse(df$Cover_Type==5,"Aspen ",
                                                    ifelse(df$Cover_Type==6,"Douglas-fir ","Krummholz"))))))
#將Cover_Type1234567都改名
df <- df %>%
  gather(key=Soil, value=soil.indicator,Soil_Type1:Soil_Type40)%>%
  filter(soil.indicator==1) %>%
  select(-soil.indicator)
#從df這筆資料，將其中40個soil資料濃縮成一個variable，並以40個soil表示(唯一性)
df$Cover_Type <- as.factor(df$Cover_Type)#原本是character 改成factor


#######
#######3. 將testing(有備份)的40+4個Dummy整合成類別資料，稱dt
dt=testing
dt <- dt %>%
  gather(key=Region, value=region.indicator,Wilderness_Area1:Wilderness_Area4)%>%
  filter(region.indicator==1) %>%
  select(-region.indicator)
table(dt$Cover_Type)
#從dt這筆資料，將其中四個area資料濃縮成一個variable，並以四個area表示(唯一性)
dt$Region <- ifelse(dt$Region=="Wilderness_Area1","Rawah",
                    ifelse(dt$Region=="Wilderness_Area2","Neota",
                           ifelse(dt$Region=="Wilderness_Area3","Comanche Peak","Cache la Poudre")))
#將area1234都改名
dt$Region <- as.factor(dt$Region) #原本是character 改成factor
dt$Cover_Type <- as.character(dt$Cover_Type)  #原本是number 改成character
dt$Cover_Type <- ifelse(dt$Cover_Type==1,"Spruce/Fir",
                        ifelse(dt$Cover_Type==2,"Lodgepole Pine",
                               ifelse(dt$Cover_Type==3,"Ponderosa Pine",
                                      ifelse(dt$Cover_Type==4,"Cottonwood/Willow ",
                                             ifelse(dt$Cover_Type==5,"Aspen ",
                                                    ifelse(dt$Cover_Type==6,"Douglas-fir ","Krummholz"))))))
#將Cover_Type1234567都改名
dt <- dt %>%
  gather(key=Soil, value=soil.indicator,Soil_Type1:Soil_Type40)%>%
  filter(soil.indicator==1) %>%
  select(-soil.indicator)
#從dt這筆資料，將其中40個soil資料濃縮成一個variable，並以40個soil表示(唯一性)
dt$Cover_Type <- as.factor(dt$Cover_Type)#原本是character 改成factor
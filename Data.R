rm(list=ls())  #�M���Ҧ��Ȧs
graphics.off() #�M���Ϥ������
getwd()        #��ܳo��r�Ҧb�����|
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
#######1. �q�����forest��˱o��DATA�A�ñNDATA����training/testing
attach(forest)
DATA=forest[sample((dim(forest)[1]-dim(forest)[1]%%20)/20),]
train=sample((dim(DATA)[1]-dim(DATA)[1]%%10)*0.9); test=-train
training=DATA[train,]; testing=DATA[test,]

#######
#######2. �Ntraining(���ƥ�)��40+4��Dummy��X�����O��ơA��df
df=training
df <- df %>%
  gather(key=Region, value=region.indicator,Wilderness_Area1:Wilderness_Area4)%>%
  filter(region.indicator==1) %>%
  select(-region.indicator)
table(df$Cover_Type)


#�qdf�o����ơA�N�䤤�|��area��ƿ@�Y���@��variable�A�åH�|��area����(�ߤ@��)
df$Region <- ifelse(df$Region=="Wilderness_Area1","Rawah",
                    ifelse(df$Region=="Wilderness_Area2","Neota",
                           ifelse(df$Region=="Wilderness_Area3","Comanche Peak","Cache la Poudre")))
#�Narea1234����W
df$Region <- as.factor(df$Region) #�쥻�Ocharacter �令factor
df$Cover_Type <- as.character(df$Cover_Type)  #�쥻�Onumber �令character
df$Cover_Type <- ifelse(df$Cover_Type==1,"Spruce/Fir",
                        ifelse(df$Cover_Type==2,"Lodgepole Pine",
                               ifelse(df$Cover_Type==3,"Ponderosa Pine",
                                      ifelse(df$Cover_Type==4,"Cottonwood/Willow ",
                                             ifelse(df$Cover_Type==5,"Aspen ",
                                                    ifelse(df$Cover_Type==6,"Douglas-fir ","Krummholz"))))))
#�NCover_Type1234567����W
df <- df %>%
  gather(key=Soil, value=soil.indicator,Soil_Type1:Soil_Type40)%>%
  filter(soil.indicator==1) %>%
  select(-soil.indicator)
#�qdf�o����ơA�N�䤤40��soil��ƿ@�Y���@��variable�A�åH40��soil����(�ߤ@��)
df$Cover_Type <- as.factor(df$Cover_Type)#�쥻�Ocharacter �令factor


#######
#######3. �Ntesting(���ƥ�)��40+4��Dummy��X�����O��ơA��dt
dt=testing
dt <- dt %>%
  gather(key=Region, value=region.indicator,Wilderness_Area1:Wilderness_Area4)%>%
  filter(region.indicator==1) %>%
  select(-region.indicator)
table(dt$Cover_Type)
#�qdt�o����ơA�N�䤤�|��area��ƿ@�Y���@��variable�A�åH�|��area����(�ߤ@��)
dt$Region <- ifelse(dt$Region=="Wilderness_Area1","Rawah",
                    ifelse(dt$Region=="Wilderness_Area2","Neota",
                           ifelse(dt$Region=="Wilderness_Area3","Comanche Peak","Cache la Poudre")))
#�Narea1234����W
dt$Region <- as.factor(dt$Region) #�쥻�Ocharacter �令factor
dt$Cover_Type <- as.character(dt$Cover_Type)  #�쥻�Onumber �令character
dt$Cover_Type <- ifelse(dt$Cover_Type==1,"Spruce/Fir",
                        ifelse(dt$Cover_Type==2,"Lodgepole Pine",
                               ifelse(dt$Cover_Type==3,"Ponderosa Pine",
                                      ifelse(dt$Cover_Type==4,"Cottonwood/Willow ",
                                             ifelse(dt$Cover_Type==5,"Aspen ",
                                                    ifelse(dt$Cover_Type==6,"Douglas-fir ","Krummholz"))))))
#�NCover_Type1234567����W
dt <- dt %>%
  gather(key=Soil, value=soil.indicator,Soil_Type1:Soil_Type40)%>%
  filter(soil.indicator==1) %>%
  select(-soil.indicator)
#�qdt�o����ơA�N�䤤40��soil��ƿ@�Y���@��variable�A�åH40��soil����(�ߤ@��)
dt$Cover_Type <- as.factor(dt$Cover_Type)#�쥻�Ocharacter �令factor
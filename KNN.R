library(class)
dall=as.data.frame(t(cbind(t(df),t(dt))))
data_x_matrix <- as.matrix(cbind(dall$Elevation,dall$Aspect,dall$Slope,
                                 dall$Horizontal_Distance_To_Hydrology,dall$Vertical_Distance_To_Hydrology,
                                 dall$Horizontal_Distance_To_Roadways,dall$Hillshade_9am
                                 ,dall$Hillshade_Noon,testing$Hillshade_3pm
                                 ,dall$Horizontal_Distance_To_Fire_Points,dall$Region,as.factor(dall$Soil)))
###knn.cv
###This uses leave-one-out cross validation
error2=numeric();

for(i in 1:20){
knn.pred.data=knn.cv(data_x_matrix,dall$Cover_Type,k=i,prob=TRUE)
error2=c(error2,mean(knn.pred.data!=dall$Cover_Type))
}
par(mfrow=c(1,1))
plot(error2,pch=19,type="b",xlab="K",ylab="error rate")

knn.pred.data1=knn.cv(data_x_matrix,dall$Cover_Type,k=1,prob=TRUE)
table(knn.pred.data1,dall$Cover_Type)
mean(knn.pred.data1!=dall$Cover_Type)

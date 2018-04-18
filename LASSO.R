n=0.5#耕ぃ続獶絬┦
#1. Lasso
##df,钡帝俱戈,磅︽logistic regression 跑Θ0蛤1
##lasso&cv.lasso test error
train_x_matrix <-as.matrix(cbind(df$Elevation,df$Aspect,df$Slope,df$Horizontal_Distance_To_Hydrology,
                                 df$Vertical_Distance_To_Hydrology,df$Horizontal_Distance_To_Roadways,
                                 df$Hillshade_9am,df$Hillshade_Noon,df$Hillshade_3pm,df$Horizontal_Distance_To_Fire_Points,
                                 df$Region,as.factor(df$Soil)))
test_x_matrix <- as.matrix(cbind(dt$Elevation,dt$Aspect,dt$Slope,dt$Horizontal_Distance_To_Hydrology,
                                 dt$Vertical_Distance_To_Hydrology,dt$Horizontal_Distance_To_Roadways,
                                 dt$Hillshade_9am,dt$Hillshade_Noon,dt$Hillshade_3pm,dt$Horizontal_Distance_To_Fire_Points,
                                 dt$Region,as.factor(dt$Soil)))
#####1 - Spruce/Fir 
##lasso
lasso.mod1 =glmnet(train_x_matrix,training_y1,alpha=1)
plot(lasso.mod1,xvar="lambda",col=c("aquamarine4","chartreuse2","chocolate","coral3","azure4","black","cornflowerblue","darkblue","darkgoldenrod","darkolivegreen","darkorchid2","lightpink4"),lwd=2)
legend("left",legend=c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am",
                          "Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil"),lty=1, bty='n', cex=1,lwd=3,
       col=c("aquamarine4","chartreuse2","chocolate","coral3","azure4","black","cornflowerblue","darkblue","darkgoldenrod","darkolivegreen","darkorchid2","lightpink4"))
#瓜
#####inportant
bestlam1 =min(lasso.mod1$lambda)#珼程lambda(璶糶)
lasso.prob1=predict(lasso.mod1,s=bestlam1,newx=train_x_matrix,type="response")
A1=roc(training_y1,lasso.prob1);
n1=which.max(A1$sensitivities*A1$specificities)
N1=A1$thresholds[n1] #禲ㄓ计讽耞翴

lasso.prob1t=predict(lasso.mod1,s=bestlam1,newx=test_x_matrix,type="response")#箇代家
lasso.pred1 <- rep("0",length(lasso.prob1t))
lasso.pred1[lasso.prob1t > N1] <- "1"
table(lasso.pred1,testing_y1) #材贺摸test confusion table
cat(c("test error for type 1",mean(lasso.pred1!=testing_y1)))
lasso.coef1=predict(lasso.mod1 ,type ="coefficients",s=bestlam1)#材家玒计
row.names(lasso.coef1)=c("(Intercept)","Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am","Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil")
##ユがノlasso
cv.glmmod1 <- cv.glmnet(train_x_matrix,training_y1,alpha=1)
plot(cv.glmmod1)
best.lambda1 <- cv.glmmod1$lambda.min#匡材贺摸程lambda
cvlasso.coef1 <- predict(cv.glmmod1,type ="coefficients",s=best.lambda1)#玒计
row.names(cvlasso.coef1)=c("(Intercept)","Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am","Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil")
cv.lasso.prob1=predict(cv.glmmod1,s=best.lambda1,newx=test_x_matrix,type="response")
cv.lasso.pred1 <- rep("0",length(cv.lasso.prob1))
cv.lasso.pred1[cv.lasso.prob1 > N1] <- "1"
table(cv.lasso.pred1,testing_y1)#test confusion table
cat(c("test error(CV) for type 1",mean(cv.lasso.pred1!=testing_y1)))



#####2 - Lodgepole Pine
##lasso
lasso.mod2 =glmnet(train_x_matrix,training_y2,alpha=1)
plot(lasso.mod2,xvar="lambda",col=c("aquamarine4","chartreuse2","chocolate","coral3","azure4","black","cornflowerblue","darkblue","darkgoldenrod","darkolivegreen","darkorchid2","lightpink4"),lwd=2)
legend("bottomright",legend=c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am",
                       "Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil"),lty=1, bty='n', cex=1,lwd=3,
       col=c("aquamarine4","chartreuse2","chocolate","coral3","azure4","black","cornflowerblue","darkblue","darkgoldenrod","darkolivegreen","darkorchid2","lightpink4"))
#瓜
#####inportant
bestlam2 =min(lasso.mod2$lambda)#程lambda
lasso.prob2=predict(lasso.mod2,s=bestlam1,newx=train_x_matrix,type="response")
A2=roc(training_y2,lasso.prob2);
n2=which.max(A2$sensitivities*A2$specificities)
N2=A2$thresholds[n2] #禲ㄓ计讽耞翴

lasso.prob2t=predict(lasso.mod2,s=bestlam2,newx=test_x_matrix,type="response")#箇代家
lasso.pred2 <- rep("0",length(lasso.prob2t))
lasso.pred2[lasso.prob2t > N2] <- "1"
table(lasso.pred2,testing_y2)#test confusion table
cat(c("test error for type 2",mean(lasso.pred2!=testing_y2)))
lasso.coef2=predict(lasso.mod2 ,type ="coefficients",s=bestlam2)
row.names(lasso.coef2)=c("(Intercept)","Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am","Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil")

##cv.lasso
cv.glmmod2 <- cv.glmnet(train_x_matrix,training_y2,alpha=1)
plot(cv.glmmod2)
best.lambda2 <- cv.glmmod2$lambda.min#程lambda
cvlasso.coef2 <- predict(cv.glmmod2,type ="coefficients",s=best.lambda2)#玒计
row.names(cvlasso.coef2)=c("(Intercept)","Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am","Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil")
cv.lasso.prob2=predict(cv.glmmod2,s=best.lambda2,newx=test_x_matrix,type="response")
cv.lasso.pred2 <- rep("0",length(cv.lasso.prob2))
cv.lasso.pred2[cv.lasso.prob2 > N2] <- "1"
table(cv.lasso.pred2,testing_y2)
cat(c("test error(CV) for type 2",mean(cv.lasso.pred2!=testing_y2)))


#####3 - Ponderosa Pine 
##lasso
lasso.mod3 =glmnet(train_x_matrix,training_y3,alpha=1)
plot(lasso.mod3,xvar="lambda",col=c("aquamarine4","chartreuse2","chocolate","coral3","azure4","black","cornflowerblue","darkblue","darkgoldenrod","darkolivegreen","darkorchid2","lightpink4"),lwd=2)
legend("bottomright",legend=c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am",
                              "Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil"),lty=1, bty='n', cex=1,lwd=3,
       col=c("aquamarine4","chartreuse2","chocolate","coral3","azure4","black","cornflowerblue","darkblue","darkgoldenrod","darkolivegreen","darkorchid2","lightpink4"))
#瓜
#####inportant
bestlam3 =min(lasso.mod3$lambda)#程lambda
lasso.prob3=predict(lasso.mod3,s=bestlam1,newx=train_x_matrix,type="response")
A3=roc(training_y3,lasso.prob3);
n3=which.max(A3$sensitivities*A3$specificities)
N3=A3$thresholds[n3] #禲ㄓ计讽耞翴

lasso.prob3t=predict(lasso.mod3,s=bestlam3,newx=test_x_matrix,type="response")#箇代家
lasso.pred3 <- rep("0",length(lasso.prob3t))
lasso.pred3[lasso.prob3t > N3] <- "1"
table(lasso.pred3,testing_y3)#test confusion table
cat(c("test error for type 3",mean(lasso.pred3!=testing_y3)))
lasso.coef3=predict(lasso.mod3 ,type ="coefficients",s=bestlam3)
row.names(lasso.coef3)=c("(Intercept)","Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am","Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil")

##cv.lasso
cv.glmmod3 <- cv.glmnet(train_x_matrix,training_y3,alpha=1)
plot(cv.glmmod3)
best.lambda3 <- cv.glmmod3$lambda.min#程lambda
cvlasso.coef3 <- predict(cv.glmmod3,type ="coefficients",s=best.lambda3)#玒计
row.names(cvlasso.coef3)=c("(Intercept)","Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am","Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil")
cv.lasso.prob3=predict(cv.glmmod3,s=best.lambda3,newx=test_x_matrix,type="response")
cv.lasso.pred3 <- rep("0",length(cv.lasso.prob3))
cv.lasso.pred3[cv.lasso.prob1 > N3] <- "1"
table(cv.lasso.pred3,testing_y3)
cat(c("test error(CV) for type 3",mean(cv.lasso.pred3!=testing_y3)))

#####4 - Cottonwood/Willow
##lasso
lasso.mod4 =glmnet(train_x_matrix,training_y4,alpha=1)
plot(lasso.mod4,xvar="lambda",col=c("aquamarine4","chartreuse2","chocolate","coral3","azure4","black","cornflowerblue","darkblue","darkgoldenrod","darkolivegreen","darkorchid2","lightpink4"),lwd=2)
legend("bottomright",legend=c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am",
                              "Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil"),lty=1, bty='n', cex=1,lwd=3,
       col=c("aquamarine4","chartreuse2","chocolate","coral3","azure4","black","cornflowerblue","darkblue","darkgoldenrod","darkolivegreen","darkorchid2","lightpink4"))
#瓜
#####inportant
bestlam4 =min(lasso.mod4$lambda)#程lambda
lasso.prob4=predict(lasso.mod4,s=bestlam1,newx=train_x_matrix,type="response")
A4=roc(training_y4,lasso.prob4);
n4=which.max(A4$sensitivities*A4$specificities)
N4=A4$thresholds[n4] #禲ㄓ计讽耞翴

lasso.prob4t=predict(lasso.mod4,s=bestlam4,newx=test_x_matrix,type="response")#箇代家
lasso.pred4 <- rep("0",length(lasso.prob4t))
lasso.pred4[lasso.prob4t > N4] <- "1"
table(lasso.pred4,testing_y4)#test confusion table
cat(c("test error for type 4",mean(lasso.pred4!=testing_y4)))
lasso.coef4=predict(lasso.mod4 ,type ="coefficients",s=bestlam4)
row.names(lasso.coef4)=c("(Intercept)","Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am","Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil")

##cv.lasso
cv.glmmod4 <- cv.glmnet(train_x_matrix,training_y4,alpha=1)
plot(cv.glmmod4)
best.lambda4 <- cv.glmmod4$lambda.min#程lambda
cvlasso.coef4 <- predict(cv.glmmod4,type ="coefficients",s=best.lambda4)#玒计
row.names(cvlasso.coef4)=c("(Intercept)","Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am","Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil")
cv.lasso.prob4=predict(cv.glmmod4,s=best.lambda4,newx=test_x_matrix,type="response")
cv.lasso.pred4 <- rep("0",length(cv.lasso.prob4))
cv.lasso.pred4[cv.lasso.prob4 > N4] <- "1"
table(cv.lasso.pred4,testing_y4)
cat(c("test error(CV) for type 4",mean(cv.lasso.pred4!=testing_y4)))

#####5 - Aspen
##lasso
lasso.mod5 =glmnet(train_x_matrix,training_y5,alpha=1)
plot(lasso.mod5,xvar="lambda",col=c("aquamarine4","chartreuse2","chocolate","coral3","azure4","black","cornflowerblue","darkblue","darkgoldenrod","darkolivegreen","darkorchid2","lightpink4"),lwd=2)
legend("bottomright",legend=c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am",
                              "Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil"),lty=1, bty='n', cex=1,lwd=3,
       col=c("aquamarine4","chartreuse2","chocolate","coral3","azure4","black","cornflowerblue","darkblue","darkgoldenrod","darkolivegreen","darkorchid2","lightpink4"))
#瓜
#####inportant
bestlam5 =min(lasso.mod5$lambda)#程lambda
lasso.prob5=predict(lasso.mod5,s=bestlam1,newx=train_x_matrix,type="response")
A5=roc(training_y5,lasso.prob5);
n5=which.max(A5$sensitivities*A5$specificities)
N5=A5$thresholds[n5] #禲ㄓ计讽耞翴

lasso.prob5t=predict(lasso.mod5,s=bestlam5,newx=test_x_matrix,type="response")#箇代家
lasso.pred5 <- rep("0",length(lasso.prob5t))
lasso.pred5[lasso.prob5t > N5] <- "1"
table(lasso.pred5,testing_y5)#test confusion table
cat(c("test error for type 5",mean(lasso.pred5!=testing_y5)))
lasso.coef5=predict(lasso.mod5 ,type ="coefficients",s=bestlam5)
row.names(lasso.coef5)=c("(Intercept)","Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am","Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil")

##cv.lasso
cv.glmmod5 <- cv.glmnet(train_x_matrix,training_y5,alpha=1)
plot(cv.glmmod5)
best.lambda5 <- cv.glmmod5$lambda.min#程lambda
cvlasso.coef5 <- predict(cv.glmmod5,type ="coefficients",s=best.lambda5)#玒计
row.names(cvlasso.coef5)=c("(Intercept)","Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am","Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil")
cv.lasso.prob5=predict(cv.glmmod5,s=best.lambda5,newx=test_x_matrix,type="response")
cv.lasso.pred5 <- rep("0",length(cv.lasso.prob5))
cv.lasso.pred5[cv.lasso.prob5 > N5] <- "1"
table(cv.lasso.pred5,testing_y5)
cat(c("test error(CV) for type 5",mean(cv.lasso.pred5!=testing_y5)))

#####6 - Douglas-fir 
##lasso
lasso.mod6 =glmnet(train_x_matrix,training_y6,alpha=1)
plot(lasso.mod6,xvar="lambda",col=c("aquamarine4","chartreuse2","chocolate","coral3","azure4","black","cornflowerblue","darkblue","darkgoldenrod","darkolivegreen","darkorchid2","lightpink4"),lwd=2)
legend("bottomright",legend=c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am",
                              "Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil"),lty=1, bty='n', cex=1,lwd=3,
       col=c("aquamarine4","chartreuse2","chocolate","coral3","azure4","black","cornflowerblue","darkblue","darkgoldenrod","darkolivegreen","darkorchid2","lightpink4"))
#瓜
#####inportant
bestlam6 =min(lasso.mod6$lambda)#程lambda
lasso.prob6=predict(lasso.mod6,s=bestlam1,newx=train_x_matrix,type="response")
A6=roc(training_y6,lasso.prob6);
n6=which.max(A6$sensitivities*A6$specificities)
N6=A6$thresholds[n6] #禲ㄓ计讽耞翴

lasso.prob6t=predict(lasso.mod6,s=bestlam6,newx=test_x_matrix,type="response")#箇代家
lasso.pred6 <- rep("0",length(lasso.prob6t))
lasso.pred6[lasso.prob6t > N6] <- "1"
table(lasso.pred6,testing_y6)#test confusion table
cat(c("test error for type 6",mean(lasso.pred6!=testing_y6)))
lasso.coef6=predict(lasso.mod6 ,type ="coefficients",s=bestlam6)
row.names(lasso.coef6)=c("(Intercept)","Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am","Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil")

##cv.lasso
cv.glmmod6 <- cv.glmnet(train_x_matrix,training_y6,alpha=1)
plot(cv.glmmod6)
best.lambda6 <- cv.glmmod6$lambda.min#程lambda
cvlasso.coef6 <- predict(cv.glmmod6,type ="coefficients",s=best.lambda6)#玒计
row.names(cvlasso.coef6)=c("(Intercept)","Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am","Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil")
cv.lasso.prob6=predict(cv.glmmod6,s=best.lambda6,newx=test_x_matrix,type="response")
cv.lasso.pred6 <- rep("0",length(cv.lasso.prob6))
cv.lasso.pred6[cv.lasso.prob6 > N6] <- "1"
table(cv.lasso.pred6,testing_y6)
cat(c("test error(CV) for type 6",mean(cv.lasso.pred6!=testing_y6)))

#####7 - Krummholz
lasso.mod7 =glmnet(train_x_matrix,training_y7,alpha=1)
plot(lasso.mod7,xvar="lambda",col=c("aquamarine4","chartreuse2","chocolate","coral3","azure4","black","cornflowerblue","darkblue","darkgoldenrod","darkolivegreen","darkorchid2","lightpink4"),lwd=2)
legend("bottomright",legend=c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am",
                              "Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil"),lty=1, bty='n', cex=1,lwd=3,
       col=c("aquamarine4","chartreuse2","chocolate","coral3","azure4","black","cornflowerblue","darkblue","darkgoldenrod","darkolivegreen","darkorchid2","lightpink4"))
#瓜
#####inportant
bestlam7 =min(lasso.mod7$lambda)#程lambda
lasso.prob7=predict(lasso.mod7,s=bestlam1,newx=train_x_matrix,type="response")
A7=roc(training_y7,lasso.prob7);
n7=which.max(A7$sensitivities*A7$specificities)
N7=A7$thresholds[n7] #禲ㄓ计讽耞翴

lasso.prob7t=predict(lasso.mod7,s=bestlam7,newx=test_x_matrix,type="response")#箇代家
lasso.pred7 <- rep("0",length(lasso.prob7t))
lasso.pred7[lasso.prob7t > N7] <- "1"
table(lasso.pred7,testing_y7)#test confusion table
test.error7 <- mean(lasso.pred7!=testing_y7)
lasso.coef7=predict(lasso.mod7 ,type ="coefficients",s=bestlam7)
row.names(lasso.coef7)=c("(Intercept)","Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am","Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil")

##cv.lasso
cv.glmmod7 <- cv.glmnet(train_x_matrix,training_y7,alpha=1)
plot(cv.glmmod7)
best.lambda7 <- cv.glmmod7$lambda.min#程lambda
cvlasso.coef7 <- predict(cv.glmmod7,type ="coefficients",s=best.lambda7)#玒计
row.names(cvlasso.coef7)=c("(Intercept)","Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology","Horizontal_Distance_To_Roadways","Hillshade_9am","Hillshade_Noon","Hillshade_3pm","Horizontal_Distance_To_Fire_Points","Region","Soil")
cv.lasso.prob7=predict(cv.glmmod7,s=best.lambda7,newx=test_x_matrix,type="response")
cv.lasso.pred7 <- rep("0",length(cv.lasso.prob7))
cv.lasso.pred7[cv.lasso.prob7 > N7] <- "1"
table(cv.lasso.pred7,testing_y7)
cat(c("test error(CV) for type 7",mean(cv.lasso.pred7!=testing_y7)))


###########LASSO俱瞶###########
par(mfrow=c(2,4))
plot(roc(training_y1,lasso.prob1),main="trpe 1 ROC curve")
plot(roc(training_y2,lasso.prob2),main="trpe 2 ROC curve")
plot(roc(training_y3,lasso.prob3),main="trpe 3 ROC curve")
plot(roc(training_y4,lasso.prob4),main="trpe 4 ROC curve")
plot(roc(training_y5,lasso.prob5),main="trpe 5 ROC curve")
plot(roc(training_y6,lasso.prob6),main="trpe 6 ROC curve")
plot(roc(training_y7,lasso.prob7),main="trpe 7 ROC curve")

NLASSO=c(N1,N2,N3,N4,N5,N6,N7)#type1~7ROC匡耞翴
MINLAM=c(bestlam1,bestlam2,bestlam3,bestlam4,bestlam5,bestlam6,bestlam7)#type1~7程lambda
E1=mean(lasso.pred1!=testing_y1); E2=mean(lasso.pred2!=testing_y2);
E3=mean(lasso.pred3!=testing_y3); E4=mean(lasso.pred4!=testing_y4);
E5=mean(lasso.pred5!=testing_y5); E6=mean(lasso.pred6!=testing_y6); E7=mean(lasso.pred7!=testing_y7)
LASSOER=c(E1,E2,E3,E4,E5,E6,E7)#LASSO trpe1~7error rate
CV1=mean(cv.lasso.pred1!=testing_y1); CV2=mean(cv.lasso.pred2!=testing_y2);
CV3=mean(cv.lasso.pred3!=testing_y3); CV4=mean(cv.lasso.pred4!=testing_y4);
CV5=mean(cv.lasso.pred5!=testing_y5); CV6=mean(cv.lasso.pred6!=testing_y6); CV7=mean(cv.lasso.pred7!=testing_y7)
CVLAER=c(CV1,CV2,CV3,CV4,CV5,CV6,CV7)#CV.LASSO trpe1~7error rate
LASSOresult=t(cbind(NLASSO,MINLAM,LASSOER,CVLAER))
rownames(LASSOresult)=c("ROC匡耞翴","程lambda","Lasso error rate","CV.Lasso error rate")
colnames(LASSOresult)=c("Type 1","Type 2","Type 3","Type 4","Type 5","Type 6","Type 7")

LASSOresult
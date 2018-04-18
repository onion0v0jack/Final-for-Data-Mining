#1. 利用logistic regression建模，分類與預測，得到training/test error
Res_training=matrix(0,dim(training)[1],7); Res_test=matrix(0,dim(testing)[1],7)
Num_test=dim(testing)[1]; Num_training=dim(training)[1]
n=Num_test+Num_training;  Num_feature=(dim(testing)[2])-1

All_coefficient=matrix(0,7,Num_feature+1)

for(i in 1:7)
{
  training_Ypos=which(training$Cover_Type==i)
  training_Y01 =rep(0,Num_training)
  training_Y01[training_Ypos]=1
  training_Y01 =as.matrix(training_Y01,Num_training,1)
  training_X   =as.data.frame(training[,-(Num_feature+1)])
  glm_fit=glm(training_Y01~.,family=binomial(link=logit),data=training_X)
  Res_training[,i]   =predict(glm_fit,training_X,type="response")
  All_coefficient[i,]=as.vector(glm_fit$coefficients)
  
  test_X=as.data.frame(testing[,-(Num_feature+1)])
  Res_test[,i]=predict(glm_fit, test_X,type="response")
}

training1 = apply(Res_training,1,which.max)
test1 = apply(Res_test,1,which.max)

cat("Logistic regression","\n",
    "Training error:",mean(training1!=training$Cover_Type),"\n",
    "Test error:",mean(test1!=testing$Cover_Type),"\n")
table(training1,training$Cover_Type) #training confusion table
table(test1,testing$Cover_Type)      #test confusion table

#2. 對training的個別樹種做logistic regression建模與預測，得到七個test error
Num_training=dim(training)[1]
Num_feature=(dim(testing)[2])-1
training_X=as.data.frame(training[,-(Num_feature+1)])
###1 - Spruce/Fir 
training_y1 =rep(0,Num_training); 
training_y1[which(training$Cover_Type==1)]=1
training_y1 =as.matrix(training_y1,Num_training,1)
glm_fit1=glm(training_y1~.,family=binomial(link=logit),data=training_X)

glm.probs1=predict(glm_fit1,testing,type="response")
glm.pred1 =rep(0,length(glm.probs1)); glm.pred1[glm.probs1>.5]=1
testing_y1=rep(0,dim(testing)[1]); 
testing_y1[which(testing$Cover_Type==1)]=1

###2 - Lodgepole Pine 
training_y2 =rep(0,Num_training); 
training_y2[which(training$Cover_Type==2)]=1
training_y2 =as.matrix(training_y2,Num_training,1)
glm_fit2=glm(training_y2~.,family=binomial(link=logit),data=training_X)

glm.probs2=predict(glm_fit2,testing,type="response")
glm.pred2 =rep(0,length(glm.probs2)); glm.pred2[glm.probs2>.5]=1
testing_y2=rep(0,dim(testing)[1]); 
testing_y2[which(testing$Cover_Type==2)]=1

###3 - Ponderosa Pine 
training_y3 =rep(0,Num_training); 
training_y3[which(training$Cover_Type==3)]=1
training_y3 =as.matrix(training_y3,Num_training,1)
glm_fit3=glm(training_y3~.,family=binomial(link=logit),data=training_X)

glm.probs3=predict(glm_fit3,testing,type="response")
glm.pred3 =rep(0,length(glm.probs3)); glm.pred3[glm.probs3>.5]=1
testing_y3=rep(0,dim(testing)[1]); 
testing_y3[which(testing$Cover_Type==3)]=1

###4 - Cottonwood/Willow 
training_y4 =rep(0,Num_training);
training_y4[which(training$Cover_Type==4)]=1
training_y4 =as.matrix(training_y4,Num_training,1)
glm_fit4=glm(training_y4~.,family=binomial(link=logit),data=training_X)

glm.probs4=predict(glm_fit4,testing,type="response")
glm.pred4 =rep(0,length(glm.probs4)); glm.pred4[glm.probs4>.5]=1
testing_y4=rep(0,dim(testing)[1]); 
testing_y4[which(testing$Cover_Type==4)]=1

###5 - Aspen 
training_y5 =rep(0,Num_training); 
training_y5[which(training$Cover_Type==5)]=1
training_y5 =as.matrix(training_y5,Num_training,1)
glm_fit5=glm(training_y5~.,family=binomial(link=logit),data=training_X)

glm.probs5=predict(glm_fit5,testing,type="response")
glm.pred5 =rep(0,length(glm.probs5)); glm.pred5[glm.probs5>.5]=1
testing_y5=rep(0,dim(testing)[1]); 
testing_y5[which(testing$Cover_Type==5)]=1

###6 - Douglas-fir 
training_y6 =rep(0,Num_training); 
training_y6[which(training$Cover_Type==6)]=1
training_y6 =as.matrix(training_y6,Num_training,1)
glm_fit6=glm(training_y6~.,family=binomial(link=logit),data=training_X)

glm.probs6=predict(glm_fit6,testing,type="response")
glm.pred6 =rep(0,length(glm.probs6)); glm.pred6[glm.probs6>.5]=1
testing_y6=rep(0,dim(testing)[1]); 
testing_y6[which(testing$Cover_Type==6)]=1

###7 - Krummholz
training_y7 =rep(0,Num_training); 
training_y7[which(training$Cover_Type==7)]=1
training_y7 =as.matrix(training_y7,Num_training,1)
glm_fit7=glm(training_y7~.,family=binomial(link=logit),data=training_X)

glm.probs7=predict(glm_fit7,testing,type="response")
glm.pred7 =rep(0,length(glm.probs7)); glm.pred7[glm.probs7>.5]=1
testing_y7=rep(0,dim(testing)[1]); 
testing_y7[which(testing$Cover_Type==7)]=1

table(glm.pred1,testing_y1) #第一種類的test confusion table
cat(c("Test error for type 1(Spruce/Fir):",mean(glm.pred1!=testing_y1)))
table(glm.pred2,testing_y2) #第二種類的test confusion table
cat(c("Test error for type 2(Lodgepole Pine):",mean(glm.pred2!=testing_y2)))
table(glm.pred3,testing_y3) #第三種類的test confusion table
cat(c("Test error for type 3(Ponderosa Pine):",mean(glm.pred3!=testing_y3)))
table(glm.pred4,testing_y4) #第四種類的test confusion table
cat(c("Test error for type 4(Cottonwood/Willow):",mean(glm.pred4!=testing_y4)))
table(glm.pred5,testing_y5) #第五種類的test confusion table
cat(c("Test error for type 5(Aspen):",mean(glm.pred5!=testing_y5)))
table(glm.pred6,testing_y6) #第六種類的test confusion table
cat(c("Test error for type 6(Douglas-fir):",mean(glm.pred6!=testing_y6)))
table(glm.pred7,testing_y7) #第七種類的test confusion table
cat(c("Test error for type 7(Krummholz):",mean(glm.pred7!=testing_y7)))

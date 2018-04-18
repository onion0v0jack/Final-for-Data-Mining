trainingRF=training; testingRF=testing
trainingRF2=df; testingRF2=dt
par(mfrow=c(1,1))
#1.  bagging(mtry=54)
tree.bagging=randomForest(factor(Cover_Type)~.,data=trainingRF,mtry=54,ntree=200,importance=TRUE)
yhat.bag=predict(tree.bagging,testingRF2,type="class")
tree.bagging
confusionMatrix(yhat.bag,testingRF$Cover_Type)
cat(c("Bagging","\n","Training error of :",mean(tree.bagging$predicted!=trainingRF$Cover_Type),
      "\n","Test error of :",mean(yhat.bag!=testingRF$Cover_Type)))
varImpPlot(tree.bagging)
importance=varImp(tree.bagging); head(importance)

#2.  random forest(mtry=7)
tree.randomforest=randomForest(factor(Cover_Type)~.,data=trainingRF,mtry=7,ntree=200,importance=TRUE)
yhat.rf=predict(tree.randomforest,testingRF,type="class")
tree.randomforest
confusionMatrix(yhat.rf,testingRF$Cover_Type)
cat(c("Random forest","\n","Training error of :",mean(tree.randomforest$predicted!=trainingRF$Cover_Type),
      "\n","Test error of :",mean(yhat.rf!=testingRF$Cover_Type)))
varImpPlot(tree.randomforest)
importance2=varImp(tree.randomforest)

#3.  boosting
tree.boost=gbm(factor(Cover_Type)~.,data=trainingRF,distribution="multinomial",n.trees=250,interaction.depth=4)
yhat.boost=predict(tree.boost,newdata=testingRF,n.trees=250,type="response")
boost_predict=apply(yhat.boost,1,which.max)
mean(boost_predict!=testingRF$Cover_Type) #test error


#Cross Validation
boosttesterror=array()
for(i in 1:5)
{
  tree.boost=gbm(factor(Cover_Type)~.,data=trainingRF,distribution="multinomial",n.trees=250,interaction.depth=i)

  yhat.boost=predict(tree.boost,newdata=testingRF,n.trees=250,type="response")
  boost_predict=apply(yhat.boost,1,which.max)
  boosttesterror[i]=mean(boost_predict!=testingRF$Cover_Type) #test error
}
plot(boosttesterror,pch=19,type="b",xlab="depth of tree",ylab="test error rate",main="Boosting")

min(boosttesterror)

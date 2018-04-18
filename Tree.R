##Fitting classification tree
#1. 樹種對非類別資料做TREE
training$Cover_Type <- as.factor(training$Cover_Type)
testing$Cover_Type <- as.factor(testing$Cover_Type)
tree.forest <- tree(training$Cover_Type~Hillshade_3pm+
                      Hillshade_9am+ Hillshade_Noon+
                      Horizontal_Distance_To_Fire_Points+Horizontal_Distance_To_Hydrology+
                      Horizontal_Distance_To_Roadways+Slope+Elevation+Aspect,training)          
summary(tree.forest) #Misclassification error rate:Training error
plot(tree.forest)#跑出骨架
text(tree.forest)#增加分支註解
tree.pred <- predict(tree.forest,testing,type="class")#預測
table(tree.pred,testing$Cover_Type)#confusion table
cat("Classification Trees","\n","Training error:",
    summary(tree.forest)$misclass[1]/summary(tree.forest)$misclass[2],
    "\n","Test error:",mean(tree.pred!=testing$Cover_Type))

#2. 透過tree交叉驗證，優化決策樹
cv.forest <- cv.tree(tree.forest,FUN=prune.misclass)
names(cv.forest)
cv.forest
par(mfrow=c(1,1))
which(cv.forest$dev==min(cv.forest$dev))#可以回去看多少葉子的時候dev最小，若同時就取最小的
plot(cv.forest$size,cv.forest$dev,type="b",ylab="CV forest deviance",xlab="CV forest size")
points(cv.forest$size[which(cv.forest$dev==min(cv.forest$dev))[1]]
       ,min(cv.forest$dev),col="red",cex=2,pch=20) 
points(cv.forest$size[which(cv.forest$dev==min(cv.forest$dev))[2]]
       ,min(cv.forest$dev),col="red",cex=2,pch=20)
plot(cv.forest$k,cv.forest$dev,type="b")#看懲罰參數，較直觀

#透過cv刪減一些葉子
prune_tree=prune.misclass(tree.forest,best=8)
par(mfrow=c(1,1))
plot(prune_tree)
text(prune_tree,pretty = 0)#跑出新的決策樹

tree.pred2 <- predict(prune_tree,testing,type="class")#預測
table(tree.pred2,testing$Cover_Type)#confusion table
cat("CV Trees","\n",
    "修剪前 Test error:",mean(tree.pred!=testing$Cover_Type),"\n",
    "修剪後 Test error:",mean(tree.pred2!=testing$Cover_Type))

#3. 改成樹種對所有變量做TREE
tree.forest2<- tree(training$Cover_Type~.,training)          
summary(tree.forest2) #Misclassification error rate:Training error
plot(tree.forest2)#跑出骨架
text(tree.forest2)#增加分支註解
tree.pred2<- predict(tree.forest2,testing,type="class")#預測
table(tree.pred2,testing$Cover_Type)#confusion table
cat("Classification Trees","\n","Training error:",
    summary(tree.forest2)$misclass[1]/summary(tree.forest2)$misclass[2],
    "\n","Test error:",mean(tree.pred2!=testing$Cover_Type))

#4. 透過tree交叉驗證，優化決策樹
cv.forest2<- cv.tree(tree.forest2,FUN=prune.misclass)
names(cv.forest2)
cv.forest2
par(mfrow=c(1,1))
which(cv.forest2$dev==min(cv.forest2$dev))#可以回去看多少葉子的時候dev最小，若同時就取最小的
plot(cv.forest2$size,cv.forest2$dev,type="b",ylab="CV forest deviance",xlab="CV forest size")
points(cv.forest2$size[which(cv.forest2$dev==min(cv.forest2$dev))[1]]
       ,min(cv.forest2$dev),col="red",cex=2,pch=20) 
points(cv.forest2$size[which(cv.forest2$dev==min(cv.forest2$dev))[2]]
       ,min(cv.forest2$dev),col="red",cex=2,pch=20)
plot(cv.forest2$k,cv.forest2$dev,type="b")#看懲罰參數，較直觀

#透過cv刪減一些葉子(如果不用刪減就算了)
prune_tree2=prune.misclass(tree.forest2,best=11)
par(mfrow=c(1,1))
plot(prune_tree2)
text(prune_tree2,pretty = 0)#跑出新的決策樹

tree.pred22<- predict(prune_tree2,testing,type="class")#預測
table(tree.pred22,testing$Cover_Type)#confusion table
cat("CV Trees","\n",
    "修剪前 Test error:",mean(tree.pred2!=testing$Cover_Type),"\n",
    "修剪後 Test error:",mean(tree.pred22!=testing$Cover_Type))

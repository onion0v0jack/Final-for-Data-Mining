##Fitting classification tree
#1. ��ع�D���O��ư�TREE
training$Cover_Type <- as.factor(training$Cover_Type)
testing$Cover_Type <- as.factor(testing$Cover_Type)
tree.forest <- tree(training$Cover_Type~Hillshade_3pm+
                      Hillshade_9am+ Hillshade_Noon+
                      Horizontal_Distance_To_Fire_Points+Horizontal_Distance_To_Hydrology+
                      Horizontal_Distance_To_Roadways+Slope+Elevation+Aspect,training)          
summary(tree.forest) #Misclassification error rate:Training error
plot(tree.forest)#�]�X���[
text(tree.forest)#�W�[�������
tree.pred <- predict(tree.forest,testing,type="class")#�w��
table(tree.pred,testing$Cover_Type)#confusion table
cat("Classification Trees","\n","Training error:",
    summary(tree.forest)$misclass[1]/summary(tree.forest)$misclass[2],
    "\n","Test error:",mean(tree.pred!=testing$Cover_Type))

#2. �z�Ltree��e���ҡA�u�ƨM����
cv.forest <- cv.tree(tree.forest,FUN=prune.misclass)
names(cv.forest)
cv.forest
par(mfrow=c(1,1))
which(cv.forest$dev==min(cv.forest$dev))#�i�H�^�h�ݦh�ָ��l���ɭ�dev�̤p�A�Y�P�ɴN���̤p��
plot(cv.forest$size,cv.forest$dev,type="b",ylab="CV forest deviance",xlab="CV forest size")
points(cv.forest$size[which(cv.forest$dev==min(cv.forest$dev))[1]]
       ,min(cv.forest$dev),col="red",cex=2,pch=20) 
points(cv.forest$size[which(cv.forest$dev==min(cv.forest$dev))[2]]
       ,min(cv.forest$dev),col="red",cex=2,pch=20)
plot(cv.forest$k,cv.forest$dev,type="b")#���g�@�ѼơA�����[

#�z�Lcv�R��@�Ǹ��l
prune_tree=prune.misclass(tree.forest,best=8)
par(mfrow=c(1,1))
plot(prune_tree)
text(prune_tree,pretty = 0)#�]�X�s���M����

tree.pred2 <- predict(prune_tree,testing,type="class")#�w��
table(tree.pred2,testing$Cover_Type)#confusion table
cat("CV Trees","\n",
    "�װūe Test error:",mean(tree.pred!=testing$Cover_Type),"\n",
    "�װū� Test error:",mean(tree.pred2!=testing$Cover_Type))

#3. �令��ع�Ҧ��ܶq��TREE
tree.forest2<- tree(training$Cover_Type~.,training)          
summary(tree.forest2) #Misclassification error rate:Training error
plot(tree.forest2)#�]�X���[
text(tree.forest2)#�W�[�������
tree.pred2<- predict(tree.forest2,testing,type="class")#�w��
table(tree.pred2,testing$Cover_Type)#confusion table
cat("Classification Trees","\n","Training error:",
    summary(tree.forest2)$misclass[1]/summary(tree.forest2)$misclass[2],
    "\n","Test error:",mean(tree.pred2!=testing$Cover_Type))

#4. �z�Ltree��e���ҡA�u�ƨM����
cv.forest2<- cv.tree(tree.forest2,FUN=prune.misclass)
names(cv.forest2)
cv.forest2
par(mfrow=c(1,1))
which(cv.forest2$dev==min(cv.forest2$dev))#�i�H�^�h�ݦh�ָ��l���ɭ�dev�̤p�A�Y�P�ɴN���̤p��
plot(cv.forest2$size,cv.forest2$dev,type="b",ylab="CV forest deviance",xlab="CV forest size")
points(cv.forest2$size[which(cv.forest2$dev==min(cv.forest2$dev))[1]]
       ,min(cv.forest2$dev),col="red",cex=2,pch=20) 
points(cv.forest2$size[which(cv.forest2$dev==min(cv.forest2$dev))[2]]
       ,min(cv.forest2$dev),col="red",cex=2,pch=20)
plot(cv.forest2$k,cv.forest2$dev,type="b")#���g�@�ѼơA�����[

#�z�Lcv�R��@�Ǹ��l(�p�G���ΧR��N��F)
prune_tree2=prune.misclass(tree.forest2,best=11)
par(mfrow=c(1,1))
plot(prune_tree2)
text(prune_tree2,pretty = 0)#�]�X�s���M����

tree.pred22<- predict(prune_tree2,testing,type="class")#�w��
table(tree.pred22,testing$Cover_Type)#confusion table
cat("CV Trees","\n",
    "�װūe Test error:",mean(tree.pred2!=testing$Cover_Type),"\n",
    "�װū� Test error:",mean(tree.pred22!=testing$Cover_Type))
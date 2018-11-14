
library(data.table)

#データ読込
train<-fread("C:/study/titanic/train.csv", header=T)
test<-fread("C:/study/titanic/test.csv", header=T)
# train<-read.csv("C:/study/titanic/train.csv", header=T)
# test<-read.csv("C:/study/titanic/test.csv", header=T)

# train$Age[is.na(train$Age)]<- -1
# test$Age[is.na(test$Age)]<-  -1
train_f <-featureExtract(train)
train_f$Survived<-  train$Survived
test_f <-featureExtract(test)
###ロジスティック回帰モデル構築
  logi_model <- glm(
    Survived~. ,    #目的変数と説明変数の指定(全て使う場合はy~.)
    data=train_f,             #学習データ
    family=binomial(link="logit") #ロジスティック回帰を指定
  )

#モデルの中身を見る
summary(logi_model)

#モデルの精度確認
#モデルの当てはめ
pred_test<- predict(logi_model, newdata=featureExtract(test), type="response")


out<-data.frame(PassengerId = test$PassengerId)

out$Survived[pred_test>0.5] = 1
out$Survived[pred_test<=0.5] = 0
#出力
write.table(out, #出力データ
            "C:/study/titanic/submit_20171106_1_logi.csv", #出力先
            quote=FALSE, #文字列を「"」で囲む有無
            # col.names=FALSE, #変数名(列名)の有無
            row.names=FALSE, #行番号の有無
            sep="," #区切り文字の指定
)

# 
# submission = data.frame(PassengerId = test$PassengerId)
# submission$Survived[test$Sex=="female"] = 1
# submission$Survived[test$Sex=="male"] = 0
# write.csv(submission, file = "C:/study/titanic/submit_20171106_1.csv", row.names=FALSE, quote=FALSE)
# 

featureExtract = function(data) {
  feature = data[, c("Pclass","Sex","Age","SibSp","Parch","Fare","Embarked"), with=FALSE]
  feature[, Sex:=factor(Sex)]
  feature$Age[is.na(feature$Age)] = -1
  feature$Fare[is.na(feature$Fare)] = median(feature$Fare, na.rm=TRUE)
  feature$Embarked[feature$Embarked==""] = "S"
  feature[, Embarked:=factor(Embarked)]
  feature$Title = "Other"
  feature$Title[grep("Mr\\.", data$Name)] = "Mr"
  feature$Title[grep("Mrs\\.", data$Name)] = "Mrs"
  feature$Title[grep("Mme\\.", data$Name)] = "Mrs"
  feature$Title[grep("Miss\\.", data$Name)] = "Miss"
  feature$Title[grep("Mlle\\.", data$Name)] = "Miss"
  feature$Title[grep("Ms\\.", data$Name)] = "Miss"
  feature$Title[grep("Master\\.", data$Name)] = "Master"
  feature$Title[grep("Dr\\.", data$Name)] = "Dr"
  feature$Title[grep("Rev\\.", data$Name)] = "Rev"
  feature[, Title:=factor(Title)]
  return(feature)
}

library(randomForest)
rf = randomForest(featureExtract(train), as.factor(train$Survived), ntree=100, importance=TRUE)
submission = data.frame(PassengerId = test$PassengerId)
submission$Survived = predict(rf, featureExtract(test))
write.csv(submission, file = "C:/study/titanic/submit_20171106_2.csv", row.names=FALSE, quote=FALSE)

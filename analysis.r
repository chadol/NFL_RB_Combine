library(mice)
library(randomForest)
library(ggplot2)
library(caret)


temp = select(merge_data, forty:cone)
apply(temp, 2, function(x) sum(is.na(x))/length(x))

# number of draft stats running back has 
num_missing = rep(0, nrow(temp))

for(i in 1:nrow(temp)){
  num_missing[i] = sum(is.na(temp[i,]))  
}  

table(num_missing)


temp = select(merge_data, height:cone)

temp2 = mice(temp, m=10, maxit=20, seed=1, printFlag=T)


temp3 = complete(temp2)

impute_data = cbind(select(merge_data,Name:Year), temp3)


blah=lm(fpoints ~ height+weight+forty+bench+vert+broad+shuttle+cone, data=impute_data)


# Random Forest

set.seed(1)
rfgrid <- expand.grid(.mtry=c(1: 8))
traincontrol <- trainControl(method='repeatedcv', number=3, repeats=10)
X = select(impute_data, height:cone)
rf_fit = train(x=X, y=impute_data$fpoints, method="rf", trControl=traincontrol, metric="RMSE", tuneGrid=rfgrid )

blah = predict(rf_fit)

blah2 = data.frame(blah, impute_data$Name, impute_data$fpoints)

blah2 = mutate(blah2, diff = impute_data.fpoints - blah)

blah2[order(blah2$diff),][1:10,]
blah2[order(-blah2$diff),][1:10,]





qplot(height,fpoints,data=impute_data,geom=c("point","smooth"))
qplot(weight,fpoints,data=impute_data,geom=c("point","smooth"))

qplot(forty,fpoints,data=impute_data,geom=c("point","smooth"))
qplot(bench,fpoints,data=impute_data,geom=c("point","smooth"))

qplot(vert,fpoints,data=impute_data,geom=c("point","smooth"))
qplot(broad,fpoints,data=impute_data,geom=c("point","smooth"))

qplot(shuttle,fpoints,data=impute_data,geom=c("point","smooth"))

qplot(cone,fpoints,data=impute_data,geom=c("point","smooth"))


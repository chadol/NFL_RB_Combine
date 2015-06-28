library(mice)
library(randomForest)
library(ggplot2)
library(caret)
library(dplyr)
library(XML)

setwd("C:/Users/Me/Documents/GitHub/NFL_RB_Combine")
load("data.rdata")

### Preliminary Analysis ###
temp = select(merge_data, forty:cone)
apply(temp, 2, function(x) sum(is.na(x))/length(x))

# number of draft stats running back has 
num_missing = rep(0, nrow(temp))
for(i in 1:nrow(temp)){
  num_missing[i] = sum(is.na(temp[i,]))  
}  
table(num_missing)/nrow(merge_data)

# Top Players
merge_data[order(-merge_data$ry),][1:20,]
merge_data[order(-merge_data$scrim),][1:20,]
merge_data[order(-merge_data$fpoints),][1:10,]
select(merge_data[order(-merge_data$fpoints),][1:10,], fname:lname, fpoints, scrim, tot_td)


### Impute missing combine data ###
temp = select(merge_data, height:cone)
temp2 = mice(temp, m=20, maxit=30, seed=1, printFlag=T)
temp3 = complete(temp2)
impute_data = cbind(select(merge_data,Name:Year), temp3)

### Plots ###
plot_arg = list(geom_point(alpha=I(.7),size=4,shape=19), 
                geom_smooth(method="rlm", formula=y~poly(x,3), colour="orangered"),
                guides(colour=FALSE),  ylab("Fantasy Points"), 
                theme(axis.title=element_text(size=15)))

temp1 = ggplot(impute_data, aes(height,fpoints,colour=fpoints)) + xlab("Height") + plot_arg
temp2 = ggplot(impute_data, aes(weight,fpoints,colour=fpoints)) + xlab("Weight") + plot_arg
temp3 = ggplot(impute_data, aes(forty,fpoints,colour=fpoints)) + xlab("40 Yd Dash") + plot_arg
temp4 = ggplot(impute_data, aes(bench,fpoints,colour=fpoints)) + xlab("Bench Press") + plot_arg
multiplot(temp1, temp2, temp3, temp4, cols=2)

temp1 = ggplot(impute_data, aes(vert,fpoints,colour=fpoints)) + xlab("Vertical Jump") + plot_arg
temp2 = ggplot(impute_data, aes(broad,fpoints,colour=fpoints)) + xlab("Broad Jump") + plot_arg
temp3 = ggplot(impute_data, aes(shuttle,fpoints,colour=fpoints)) + xlab("20 Yd Shuttle") + plot_arg
temp4 = ggplot(impute_data, aes(cone,fpoints,colour=fpoints)) + xlab("3 Cone") + plot_arg
multiplot(temp1, temp2, temp3, temp4, cols=2)

### Linear Regression ###
lm_fit = lm(fpoints ~ height+weight+forty+bench+vert+broad+shuttle+cone, data=impute_data)
summary(lm_fit)

### Random Forest ###
set.seed(1)
rfgrid <- expand.grid(.mtry=c(1: 8))
traincontrol <- trainControl(method='repeatedcv', number=3, repeats=10)

X = select(impute_data, height:cone)
rf_fit = train(x=X, y=impute_data$fpoints, method="rf", trControl=traincontrol, 
               metric="RMSE", tuneGrid=rfgrid)

# Most over and under performing players
pred_rb = predict(rf_fit)
pred_rb = data.frame(pred_rb, impute_data$Name, impute_data$fpoints)
pred_rb = mutate(pred_rb, diff = impute_data.fpoints - pred_rb)
pred_rb[order(pred_rb$diff),][1:5,]
pred_rb[order(-pred_rb$diff),][1:5,]

### 2015 Rookies ###
temp = readHTMLTable("http://nflcombineresults.com/nflcombinedata.php?year=2015&pos=RB&college=")  
temp = temp[["NULL"]]  
comb2015 = select(temp, -College, -POS, -Wonderlic)
names(comb2015) = c('Year','Name','height','weight','forty','bench','vert','broad','shuttle','cone')

comb2015[,1:2] = apply(comb2015[,1:2],2, as.character)
comb2015[,3:10] = apply(comb2015[,3:10],2, function(x) as.numeric(gsub("[*]","",x) ))

# Select only players that participate in at least 4 events
temp = apply(select(comb2015, forty:cone), 1, function(x) sum(!is.na(x))) >= 4
comb2015 = comb2015[temp,]
# Impute missing
temp = rbind(select(comb2015,height:cone), select(merge_data,height:cone))
temp = select(temp, height:cone)
temp2 = mice(temp, m=20, maxit=30, seed=1, printFlag=T)
temp3 = complete(temp2)

comb2015_imp = cbind(select(comb2015,Name:Year), temp3[1:nrow(comb2015),])


pred_rb = predict(rf_fit, comb2015_imp)
pred_rb = data.frame(comb2015_imp$Name, pred_rb)
pred_rb[order(-pred_rb$pred_rb),][1:5,]
pred_rb[order(pred_rb$pred_rb),][1:3,]



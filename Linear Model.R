#Linear model is built
model<-lm(sunday~daily,data=NewspaperData)
#Summary of the model
summary(model)
test<-data.frame(NewspaperData$daily)
test
pred<-predict(model,newdata = c(NewspaperData$daily))
pred
install.packages("ggplot2")
library(ggplot2)
#Various Plots 
dotchart(NewspaperData$daily)
boxplot(NewspaperData$daily)
barplot(NewspaperData$daily)
qplot(NewspaperData$daily,NewspaperData$sunday)
# Predict the model
pred<-predict(model)
finaldata<-data.frame(NewspaperData,pred,"Error"=NewspaperData$sunday-pred)
finaldata

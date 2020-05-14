model<-lm(sunday~daily,data=NewspaperData)
summary(model)
test<-data.frame(NewspaperData$daily)
test
pred<-predict(model,newdata = c(NewspaperData$daily))
pred
install.packages("ggplot2")
library(ggplot2)

dotchart(NewspaperData$daily)
boxplot(NewspaperData$daily)
barplot(NewspaperData$daily)
qplot(NewspaperData$daily,NewspaperData$sunday)

pred<-predict(model)
finaldata<-data.frame(NewspaperData,pred,"Error"=NewspaperData$sunday-pred)
finaldata

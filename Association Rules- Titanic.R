library(arules)
install.packages("arules")
library(arules)
str(Titanic)
View(Titanic)

Titanic<-Titanic[,-c(1)]
#Algorithm implementation
rules <- apriori(Titanic)
#display the rules
arules::inspect(rules)
rules.sorted <- sort(rules, by="lift")
arules::inspect(rules.sorted)

# rules with rhs containing "Survived" only
rules <- apriori(Titanic,parameter = list(supp=0.2, conf=0.5),
                 appearance = list(rhs=c("Survived=No", "Survived=Yes")
                                                                                ,default="lhs"),control = list(verbose=F))
arules::inspect(rules)
View(rules.sorted)
rules.sorted

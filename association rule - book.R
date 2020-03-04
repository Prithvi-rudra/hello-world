book <- read.csv(file.choose(), header = T, colClasses = "factor")
View(book)

library(arules)
library(arulesViz)

summary(book)

rules <- apriori(book)
summary(rules)

basket_rules <- apriori(book, parameter = list(sup = 0.9, conf = 0.5, target="rules"))


inspect(rules[1:2])

rules <- sort(rules, by = "support", decreasing = T)
inspect(rules[1:2])

rules
radignant_rules <- is.redundant(rules)
radignant_rules
summary(radignant_rules)


rules <- rules[!radignant_rules]
rules

inspect(rules[1:2])

rules <- apriori(book, parameter = list(supp = .1, conf = .9), appearance = list(default = "rhs", lhs = "ItalAtlas=1"))
inspect(rules[1:2])

head(quality(rules))
plot(rules)
plot(rules,method = "grouped")
plot(rules, method = "graph")
plot(rules, method = "graph", interactive = T)
plot(rules,method = "graph",control = list(type="items"))
plot(rules,method = "scatterplot")
plot(rules,method = "mosiac")
input <- sort(rules,by="lift")
inspect(input[1:2])


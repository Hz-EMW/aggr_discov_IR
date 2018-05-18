## Asociation Analysis
# Transactions preparation
library(arules)
#Transaction sets from dtm
kwed <- 
trans <- DocumentTermMatrix(ircate, control = list(dictionary = kwed, removePunctuation = TRUE, stopwords=specialfilter, weighting = weightTfIdf))
# generate the frequent itemset using the eclat function
irt_fs<-eclat(trans, parameter = list(support=0.05, maxlen=10))

irt_fs<-apriori(trans, parameter = list(support=0.9, minlen=2, target = "rules"))
# find association rules with default settings
rules.all <- eclat(item_keywords)
rules.all
inspect(rules.all)

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

rules <- eclat(iri_cases, 
               parameter = list(supp=0.002, minlen=3, conf=0.2),
               appearance = list(rhs=c("Survived=Yes"),
                                 lhs=c("Class=1st", "Class=2nd", "Class=3rd",
                                       "Age=Child", "Age=Adult"),
                                 default="none"), 
               control = list(verbose=F))
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)

library(arulesViz)
plot(rules.all)
plot(rules.all, method="grouped")
plot(rules.all, method="graph")
plot(rules.all, method="graph", control=list(type="items"))
plot(rules.all, method="paracoord", control=list(reorder=TRUE))

library(arulesCBA)

library(arulesNBMiner)


# Mining Frequent Sequences
library(arulesSequences)
example(cspade)
## mine rules
r2 <- ruleInduction(s2, confidence = 0.5,
                    control = list(verbose = TRUE))
summary(r2)
as(r2, "data.frame")

library(opusminer)
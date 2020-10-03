### C3_T4
## Market Basket Analysis

# Install Necessary Packages
install.packages("arules")
install.packages("arulesViz")

# Call the packages
library(arules)
library(arulesViz)

# Read the dataset as Transaction
df <- read.transactions(
  file = "~/Desktop/ElectronidexTransactions2017.csv",
  header = F,
  sep = ",",
  rm.duplicates = T)

View(df)
summary(df)
inspect(df)

# Number of Transactions
length(df)

# Number of items per Transaction
size(df)

# List the transactions by conversions
LIST(df)

# To see item's label
itemLabels(df)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# List the number of each sold item
items <- itemFrequency(df, type = 'absolute')

# Sort the list
sort <- sort(items, decreasing = TRUE)

# Print only the top 15 items
head(sort, n=15)

### Visualization
## Plotting the whole sparce matrix
itemFrequencyPlot(df)
itemFrequencyPlot(df, type = "absolute", topN = 15)

# Image
image(df[1:300, 1:30])
image(df[1:500, 1:100])

# Image (with Sample)
image(sample(df, 300))


### Apriori 
# Support: % of items that include both "if" and "then" items
# Confidence: % of cases with "if" that also have "then" item
# Lift: confidencel (% of cases with "then" items)

# Basic Rule Set with defaults
# defaults of 0.8 confidence and 0.1 support
rules <- apriori(df)
summary(rules)

#  rules1: confidence of 0.5 and support of 0.5
rules1 <- apriori(df, parameter = list(supp=0.5, conf=0.5))
arules::inspect(rules1)
summary(rules1)

# rules2: confidence of 0.5 and support of 0.009
rules2 <- apriori(data=df, parameter=list (supp=0.009,conf = 0.5, minlen=2, maxlen=3))
quality(rules2) <- round(quality(rules2), digits = 3)

arules::inspect(rules2)
summary(rules2)

is.redundant(rules2)

# To see "iMac"'s rules in Rules2
ItemRules <- subset(rules2, items %in% "iMac")
inspect(ItemRules)

# rules3: confidence of 0.46 and support of 0.023
rules3 <- apriori(data=df, parameter=list (supp=0.023,conf = 0.46, minlen=2, maxlen=3))
quality(rules3) <- round(quality(rules3), digits = 3)

arules::inspect(rules3)
summary(rules3)

is.redundant(rules3)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Finding redundant rules
# subset.matrix <- is.subset(rules2, rules2)
# subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
# redundant <- colSums(subset.matrix, na.rm = T) >= 1
# which(redundant)

# Removing redundant rules
# rules2.pruned <- rules2[!redundant]
# rules2.pruned <- sort(rules2.pruned, by = "lift")
# inspect(rules2.pruned)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#*****************************************************
# Sort Rule2 by Support
inspect(head(sort(rules2, by="supp"), 5))

# Sort Rule2 by Confidence
inspect(head(sort(rules2, by="conf"), 5))

# Sort Rule2 by Lift
inspect(head(sort(rules2, by="lift"), 5))
#*****************************************************
# Sort Rule3 by Support
inspect(head(sort(rules3, by="supp"), 6))

# Sort Rule3 by Confidence
inspect(head(sort(rules3, by="conf"), 6))

# Sort Rule3 by Lift
inspect(head(sort(rules3, by="lift"), 6))
#*****************************************************

### Visualizations
# Plot_rules2
plot(rules2)
plot(rules2, method = "grouped")
plot(rules2, method = "graph", control = list(type="items"))

plot(rules2[1:5], method="graph", control = list(type="items")) 

# Plot_rules3
plot(rules3)
plot(rules3, method = "grouped")
plot(rules3, method = "graph", control = list(type="items"))

#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

#Plot subset of rules2
subrules1 <- head(sort(rules2, by="lift"), 5)
set.seed(1234)
plot(subrules1, method="grouped",  control=list(k=5))

#Plot relation between products in terms of rules
set.seed(1234)
plot(subrules1, method="graph")


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

#Plot subset of rules3
subrules1 <- head(sort(rules3, by="lift"), 6)
set.seed(1234)
plot(subrules1, method="grouped",  control=list(k=5))

#Plot relation between products in terms of rules
set.seed(1234)
plot(subrules1, method="graph")


save.image()
















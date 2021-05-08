library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(RColorBrewer)

retail <- read_excel("~/r-shiny/data/Online Retail.xlsx")

retail <- retail[complete.cases(retail),]

retail %>%
  mutate(Description = as.factor(Description))

retail %>%
  mutate(Country = as.factor(Country))

retail$Date <- as.Date(retail$InvoiceDate)

TransTime <- format(retail$InvoiceDate, "%H:%M:%S")

InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))

cbind(retail, TransTime)
cbind(retail, InvoiceNo)

transactionData <- ddply(retail, c("InvoiceNo", "Date"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))

transactionData$InvoiceNo <- NULL
transactionData$Date <- NULL
colnames(transactionData) <- c("items")

write.csv(transactionData,
          "~/r-shiny/data/market_basket_transactions.csv",
          quote = FALSE, row.names = FALSE)

tr <- read.transactions('~/r-shiny/data/market_basket_transactions.csv',
                        format = 'basket',
                        quote="",
                        sep = ',')

#view distribution of object
itemFrequencyPlot(tr, topN=20, type="absolute",
                  col=brewer.pal(8, 'Pastel2'),
                  main="Item Frequency Plot")

# generating rules
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8, maxlen=10))

inspect(association.rules[1:10])

#removing redundant rules
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1)

subset.association.rules. <- association.rules[-subset.rules] # remove subset rules

#finding rules for specific products
metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),
                                                        appearance=list(default='lhs', rhs="METAL"))
# play around with left right hand side
inspect(head(metal.association.rules))


#scatter plot
#rules with confidence 
subRules <- association.rules[quality(association.rules)$confidence > 0.8]

plot(subRules)

plotly_arules(subRules)  #interactive scatter plot

top10subRules <- head(subRules, n = 10, by = "confidence")

plot(top10subRules, method = "graph",  engine = "htmlwidget")

subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")
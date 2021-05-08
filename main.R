library(dplyr)
library(readxl)
library(xlsx)
data <- read_excel("~/r-shiny/data/Online Retail.xlsx")

data$CustomerID[is.na(data$CustomerID)] <- "000"

data$InvoiceDate <- as.Date(data$InvoiceDate, format = "%Y-%m-%d")

write.csv(data, file = "trans.csv")

data <- read.csv("~/r-shiny/data/trans.csv",
                 sep = ",")

data %>%
  select(UnitPrice) %>%
  sum %>%
  prettyNum(big.mark = ",")

data %>%
  group_by(InvoiceDate) %>%
  summarise(UnitPrice=sum(UnitPrice)) %>%
  filter(InvoiceDate >= "2010-12-01" & InvoiceDate <= "2010-12-06")

data %>%
  select(Quantity) %>%
  sum %>%
  prettyNum(big.mark = ",")

data %>%
  {.[,6]*.[,8]} %>%
  sum %>%
  prettyNum(big.mark = ",")
---------------------------------------------------------------------
data$TotalPrice <- data %>%
  {.[,6]*.[,8]}

topspend <- aggregate(data$TotalPrice, 
                      by=list(Category=data$CustomerID), FUN=sum)

topcust <- head(topspend[order(topspend$x, decreasing= T),], n = 10)
topcust$Category <- as.character(topcust$Category)

p_bar_1 <- ggplot(topcust, aes(x=reorder(Category, x), y=x, fill=Category)) +
  geom_bar(stat="Identity") +
  

p_bar_1 <- p_bar_1 + coord_flip()

ggplotly(p_bar_1)
----------------------------------------------------------------------
Books <- read.csv(file.choose())

View(Books)
summary(Books)

# converting everything into character format 
Books_matrix[] <- lapply(Books,as.matrix)
View(Books)
# Creating a custom fucntion to collapse all the items in a transaction into 
# a single sentence 
paste_fun <- function(i){
  return (paste(as.matrix(i),collapse=" "))
}
# Applying the custom function
Books_matrix["new_col"] <- apply(Books_matrix,1,paste_fun)
View(Books_matrix)

install.packages("tm")
# tm package is used to do text manipulation and forming DTM and TDM matrices
library(tm)
x <- Corpus(VectorSource(Books_matrix$new_col)) # Selecting the new column which
# contains all items of a transaction in a single sentence
x <- tm_map(x,stripWhitespace)
# Creating a TDM matrix
dtm0 <- t(TermDocumentMatrix(x))
# Converting TDM matrix to data frame
dtm0_df <- data.frame(as.matrix(dtm0))

# Association Rules 
install.packages("arulesViz")
library(arules)
library(arulesViz)
# Item Frequecy plot
windows()

# Applying apriori algorithm to get relevant rules
rules <- apriori(as.matrix(Books),parameter = list(support=0.02,confidence=0.5,minlen=2))
rules2 <- apriori(as.matrix(Books),parameter = list(support=0.01,confidence=0.2,minlen=3))
rules3 <- apriori(as.matrix(Books),parameter = list(support=0.02,confidence=0.1,minlen=4))
rules4 <- apriori(as.matrix(Books),parameter = list(support=0.01,confidence=0.1,minlen=2))


inspect(rules)
plot(rules)

# Sorting rules by confidence 
rules_conf <- sort(rules,by="confidence")
inspect(rules_conf)
# Sorint rules by lift ratio
rules_lift <- sort(rules,by="lift")
inspect(rules_lift)

# Visualizing rules in scatter plot

plot(rules,method = "graph")


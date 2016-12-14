#install.packages("devtools")
library(devtools)
#install.packages("pander")
library(pander)
#read file browse on my laptop northeastrern_class.csv
product_review<-read.csv(file.choose(),header = T)

#tokenizing, removingnon-essential  characters such as punctuation, numbers
product_review$Body = gsub("[[:punct:]]", "", product_review$Body)
product_review$Body = gsub("[[:digit:]]", "", product_review$Body)
product_review$Body = gsub("http\\w+", "", product_review$Body)
product_review$Body = gsub("[ \t]{2,}", "", product_review$Body)
product_review$Body = gsub("^\\s+|\\s+$", "", product_review$Body)

#chcek the rows number 
nrow(product_review)
#first set the positive in sentiment column
product_review["Sentiment"]<-"positive"
#loop all rows to match with dictionary uisng pander package
for(i in 1:nrow(product_review)) {
  row <- product_review[i,]
  nrc_data <- get_nrc_sentiment(as.String(product_review$Body[i]))
  pander::pandoc.table(nrc_data[, 1:8], split.table = Inf)
  pander::pandoc.table(nrc_data[, 9:10])
  valence <- (nrc_data[, 9]*-1) + nrc_data[, 10]#compare with dictionary
  if(valence<0)#define the negtive words 
  { product_review[i,c('Sentiment')]="negative"}
  
}

# write out the result 
setwd("C:/Users/Wanwan Zhang/Desktop/2016FALL/ADS/8/")
#tm, tm.plugin.sentiment
write.csv(product_review, file = "product_review.csv")

#Visualization of individual review
n <- readline(prompt="Enter an integer between 1 and 500: ")
n<-as.integer(n)
nrc_data <- get_nrc_sentiment(as.String(product_review$Body[n]))


pander::pandoc.table(nrc_data[, 1:8], split.table = Inf)
pander::pandoc.table(nrc_data[, 9:10])
#nrc_data[,9]

valence <- (nrc_data[, 9]*-1) + nrc_data[, 10]
valence


#fOR Visualization
barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)


#Overall sentiment

syuzhet_vector <- get_sentiment(as.String(product_review$Body), method="syuzhet")
head(syuzhet_vector)
nrc_data <- get_nrc_sentiment(as.String(product_review$Body))


pander::pandoc.table(nrc_data[, 1:8], split.table = Inf)
pander::pandoc.table(nrc_data[, 9:10])

barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)


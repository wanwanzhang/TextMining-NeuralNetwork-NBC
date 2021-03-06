# Web scraping live reviews about JW Marriott hotel from Trip-Advisor
df <- data.frame(Date=as.Date(character()), File=character(), User=character(), stringsAsFactors=FALSE)
x <- 0
for(i in c(1:100)){
  url <- ""
  if(x == 0){
    url <- "http://www.tripadvisor.com/Hotel_Review-g37209-d1762915-Reviews-JW_Marriott_Indianapolis-Indianapolis_Indiana.html"
    x <- x + 10
  } else{
    url <- paste("https://www.tripadvisor.com/Hotel_Review-g37209-d1762915-Reviews-or",x,"-JW_Marriott_Indianapolis-Indianapolis_Indiana.html#REVIEWS", sep = "")
    x <- x + 10
  }
  reviews <- url %>%
    read_html() %>%
    html_nodes("#REVIEWS .innerBubble")
  id <- reviews %>%
    html_node(".quote a") %>%
    html_attr("id")
  review <- reviews %>%
    html_node(".entry .partial_entry") %>%
    html_text()
  if(nrow(df) == 0){
    df <- data.frame(id, review, stringsAsFactors = FALSE)
  }
  else{
    temp <- df
    df <- rbind(temp, data.frame(id, review, stringsAsFactors = FALSE))
  }
}

write.csv(df, "C:/Users/agraw/Desktop/ADS/Final Project/tripadvisor_reviews.csv")

# Sentiment Analysis of Hotel Reviews on Trip Advisor

# Load library
library(tm)
library(stringr)

# Load the amazon review data and explore
trip <- read.csv("C:/Users/agraw/Desktop/ADS/Final Project/tripadvisor_reviews.csv")
hotel_reviews <- as.character(trip$review)

# Load the positive and negative lexicon data and explore
positive_lexicon <- read.csv("C:/Users/agraw/Desktop/ADS/Final Project/positive-lexicon.txt")
negative_lexicon <- read.csv("C:/Users/agraw/Desktop/ADS/Final Project/negative-lexicon.txt")

# Making a corpus out of the hotel reviews
reviews_corpus <- Corpus(VectorSource(hotel_reviews))
inspect(reviews_corpus)

# Remove stop words, punctuations, numbers from all the reviews and inspecting it
filtered_corpus_no_stopwords <- tm_map(reviews_corpus, removeWords, stopwords('english'))
inspect(filtered_corpus_no_stopwords)
filtered_corpus_no_puncts <- tm_map(filtered_corpus_no_stopwords, removePunctuation)
inspect(filtered_corpus_no_puncts)
filtered_corpus_no_numbers <- tm_map(filtered_corpus_no_puncts, removeNumbers)
inspect(filtered_corpus_no_numbers)
filtered_corpus_no_whitespace <- tm_map(filtered_corpus_no_numbers, stripWhitespace)
inspect(filtered_corpus_no_whitespace)
filtered_corpus_to_lower <- tm_map(filtered_corpus_no_whitespace, content_transformer(tolower))
inspect(filtered_corpus_to_lower)

# Load the stop words text file and explore
stop_words <- read.csv("C:/Users/agraw/Desktop/ADS/Final Project/stopwords_en.txt")

# Remove stop words of the external file from the corpus and whitespaces again and inspect
stopwords_vec <- as.data.frame(stop_words)
final_corpus_no_stopwords <- tm_map(corpus, removeWords, stopwords_vec[,1]) 
inspect(final_corpus_no_stopwords)
final_corpus <- tm_map(final_corpus_no_stopwords, stripWhitespace)
inspect(final_corpus)

# Character representation of the corpus of first review
final_corpus[[1]]$content
hotel_reviews[1]

# Stem the words to their root of all reviews present in the corpus
stemmed_corpus <- tm_map(final_corpus, stemDocument)

# Building a term document matrix of the stemmed corpus
TDM_corpus <- TermDocumentMatrix(stemmed_corpus)
findFreqTerms(TDM_corpus, 5)                    ##terms occurring with a minimum frequency of 5

# Calculating the count and percentage of total positive and negative words in each review and
# Labeling each review as either negative or positive
total_pos_count <- 0
total_neg_count <- 0
pos_count_vector <- c()
neg_count_vector <- c()

for(i in 1:1000){
  corpus_words<- list(strsplit(stemmed_corpus[[i]]$content, split = " "))
  #print(intersect(unlist(corpus_words), unlist(positive_lexicon))) ## positive words in current review
  pos_count <- length(intersect(unlist(corpus_words), unlist(positive_lexicon)))
  #print(intersect(unlist(corpus_words), unlist(negative_lexicon))) ## negative words in current review
  neg_count <- length(intersect(unlist(corpus_words), unlist(negative_lexicon)))
  if(pos_count>neg_count){
    #print("It's a positive review")
  } else{
    #print("It's a negative review")
  }
  total_count_for_current_review <- pos_count + neg_count ## current positive and negative count
  pos_percentage <- (pos_count*100)/total_count_for_current_review
  neg_percentage <- (neg_count*100)/total_count_for_current_review
  #print(pos_percentage)                          ## current positive percentage
  #print(neg_percentage)                          ## current negtive percentage
  total_pos_count <- total_pos_count + pos_count ## overall positive count
  total_neg_count <- total_neg_count + neg_count ## overall negative count
  pos_count_vector <- append(pos_count_vector, pos_count)
  neg_count_vector <- append(neg_count_vector, neg_count)
}

# Sentiment score of each review and visualizing using boxplot
counts <- data.frame(pos_count_vector, neg_count_vector)
sentiment <- data.frame(c(1:1000),(pos_count_vector - neg_count_vector) / (pos_count_vector + neg_count_vector))
sentiment
boxplot(sentiment$X.pos_count_vector...neg_count_vector...pos_count_vector...neg_count_vector.[400:500]~sentiment$c.1.1000.[400:500])

# Visualiztion of positive and negative count of single review
singe_review <- c(counts$pos_count_vector[1], counts$neg_count_vector[1])
barplot(t(as.data.frame(singe_review)), ylab = "Count", xlab = "Positve and Negative",  
        main = "Positive v/s Negative count of each Review")

# Calculating overall percentage of positive and negative words of all the reviews
total_pos_count                                  ## overall positive count
total_neg_count                                  ## overall negative count
total_count <- total_pos_count + total_neg_count
overall_positive_percentage <- (total_pos_count*100)/total_count
overall_negative_percentage <- (total_neg_count*100)/total_count
overall_positive_percentage                      ## overall positive percentage
overall_negative_percentage                      ## overall negative percentage

# Visualization of positive and negative word count for all the reviews
review_count_frame <- data.frame(matrix(c(pos_count_vector, neg_count_vector), nrow = 100, ncol = 2))
colnames(review_count_frame) <- c("Positive Word Count", "Negative Word Count")
barplot(review_count_frame$`Positive Word Count`, ylab = "Count", xlab = "Reviews from 1 to 100",  
        main = "Positive word count of Reviews", col="lightblue")
barplot(review_count_frame$`Negative Word Count`, ylab = "Count", xlab = "Reviews from 1 to 100",  
        main = "Negative word count of Reviews", col="lightblue")

# Visualization of Overall positive and negative reviews
percent_vec <- c(overall_positive_percentage, overall_negative_percentage)
percent_frame <- as.data.frame(percent_vec)
rownames(percent_frame) <- c("Positive Reviews","Negative Reviews")
colnames(percent_frame) <- c("Percentage")
percentage <- t(percent_frame)
barplot(percentage, ylab = "Percentage", main = "Sentiment Analysis of JW Marriot Reviews on TripAdvisor", col="lightblue")
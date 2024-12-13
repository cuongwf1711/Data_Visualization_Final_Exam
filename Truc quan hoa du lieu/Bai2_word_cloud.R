library(tm)
library(wordcloud)

text_data <- readLines(
  "/home/c/Downloads/Truc quan hoa du lieu/combined_content.txt",
  encoding = "UTF-8"
)

corpus <- VCorpus(VectorSource(text_data))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(
  corpus,
  removeWords,
  stopwords::stopwords("vi", source = "stopwords-iso")
)

dtm <- TermDocumentMatrix(corpus)
dtm_matrix <- as.matrix(dtm)

word_frequencies <- sort(rowSums(dtm_matrix), decreasing = TRUE)
word_freq_df <- data.frame(
  word = names(word_frequencies),
  freq = word_frequencies
)

set.seed(1711)
wordcloud(words = word_freq_df$word,
  freq = word_freq_df$freq,
  min.freq = 10,
  max.words = 200,
  scale = c(5, 0.5),
  random.order = FALSE,
  rot.per = 0,
  colors = brewer.pal(8, "Dark2")
)

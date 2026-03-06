library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

process_text <- function(file_path) {
  text <- tolower(readLines(file_path, encoding = "UTF-8"))
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- removeWords(text, stopwords("en"))
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[words != ""]
  return(words)
}

word_frequency <- function(words) {
  freq <- table(words)
  freq_df <- data.frame(word = names(freq), freq = as.numeric(freq))
  freq_df <- freq_df[order(-freq_df$freq), ]
  return(freq_df)
}

plot_wordcloud <- function(freq_df, title_text, color_palette = "Dark2") {
  wordcloud(words = freq_df$word, freq = freq_df$freq, min.freq = 16,
            colors = brewer.pal(8, color_palette))
  title(title_text)
}

file_paths <- c("Biden2021.txt", "Biden2024.txt")

custom_stopwords <- c("—", "–", "’s", "’re")

for (file_path in file_paths) {
  words <- process_text(file_path)
  words <- words[!words %in% custom_stopwords]
  freq_df <- word_frequency(words)
  plot_wordcloud(freq_df, paste("Chmura słów -", file_path))
  print(head(freq_df, 10))
}
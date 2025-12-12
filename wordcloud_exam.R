file.exists("wordcloud_exam.R")
file.exists("feedback.txt")

# PART 1 — DATA PREPARATION (25 pts)
# 1. Set working directory
setwd("C:/Users/beryl/Desktop/DM")
cat("Working directory:", getwd(), "\n")

# 2. Read the file using readLines()
raw_lines <- readLines("feedback.txt", encoding = "UTF-8")
raw_lines <- raw_lines[raw_lines != ""]

# 3. If feedback.txt contains "Category|Text", split it
split_line <- function(line) {
  if (grepl("\\|", line)) {
    parts <- strsplit(line, "\\|")[[1]]
    return(list(category = parts[1], text = paste(parts[-1], collapse = "")))
  } else {
    return(list(category = NA, text = line))
  }
}

parsed <- lapply(raw_lines, split_line)
feedback_df <- data.frame(
  category = sapply(parsed, `[[`, "category"),
  text     = sapply(parsed, `[[`, "text"),
  stringsAsFactors = FALSE
)

# 4. Create a Corpus from imported text
all_texts <- trimws(feedback_df$text)
all_texts <- all_texts[all_texts != ""]
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(gridExtra)
library(grid)

corpus <- VCorpus(VectorSource(all_texts))

# 5. Clean the text with required steps
clean_corpus <- function(corp) {
  corp <- tm_map(corp, content_transformer(tolower))           # For lowercase
  corp <- tm_map(corp, removeNumbers)                          # For remove numbers
  corp <- tm_map(corp, removePunctuation)                      # For remove punctuation
  corp <- tm_map(corp, removeWords, stopwords("english"))      # For remove stopwords
  corp <- tm_map(corp, stripWhitespace)                        # For strip whitespace
  corp <- tm_map(corp, stemDocument, language = "english")     # For stem words
  return(corp)
}

corpus_clean <- clean_corpus(corpus)

# PART 2 — WORD FREQUENCY ANALYSIS (25 pts)

# 1. Create a Term–Document Matrix
tdm <- TermDocumentMatrix(corpus_clean)
tdm_mat <- as.matrix(tdm)

# 2. Convert matrix to word frequency table
word_freqs <- rowSums(tdm_mat)
word_freqs <- sort(word_freqs, decreasing = TRUE)
df_freq <- data.frame(
  word = names(word_freqs),
  freq = as.integer(word_freqs),
  row.names = NULL,
  stringsAsFactors = FALSE
)

# 3. Display the top 10 most frequent words
top10 <- head(df_freq, 10)
print(top10)

write.csv(top10, "top10_table.csv", row.names = FALSE)
dev.off()

# 5. Write 3–5 sentences interpreting the results
interpretation <- c(
  "The most frequent terms indicate the dominant themes mentioned across the customer feedback.",
  "High-frequency words suggest common topics such as service quality, staff behavior, or general experience.",
  "These terms help show what areas of the experience customers repeatedly highlight.",
  "Rare words, by contrast, may represent isolated or unique issues that occurred only once.",
  "Overall, word frequency analysis helps identify primary concerns and frequently mentioned topics."
)
writeLines(interpretation, "interpretation.txt")

# PART 3 — WORD CLOUD GENERATION (30 pts)

main_df <- df_freq[df_freq$freq >= 2, , drop = FALSE]
if (nrow(main_df) == 0) {
  main_df <- df_freq   # fallback if very small dataset
}

png("wordcloud_exam.png", width = 800, height = 600)
set.seed(123)
wordcloud(
  words = main_df$word,
  freq = main_df$freq,
  min.freq = 2,
  max.words = 1000,
  random.order = FALSE,
  rot.per = 0.1,
  scale = c(4, 0.4),
  colors = brewer.pal(8, "Dark2")
)
dev.off()

# PART 4 — ADVANCED TASK (20 pts)

# 1. Identify least frequent words (frequency = 1)
min_freq_present <- min(df_freq$freq)
rare_df <- df_freq[df_freq$freq == min_freq_present, , drop = FALSE]

if (nrow(rare_df) > 0) {
  # If the least frequent words have a frequency > 1, adjust min.freq
  plot_min_freq <- min_freq_present 
  
  message("Found ", nrow(rare_df), " words with the lowest frequency (freq=", plot_min_freq, "). Generating cloud...")
  
# 2. Create separate word cloud for this lowest frequency group
  png("wordcloud_rare.png", width = 800, height = 600)
  set.seed(321)
  wordcloud(
    words = rare_df$word,
    freq = rare_df$freq,
    min.freq = plot_min_freq,  # Use the actual lowest frequency
    max.words = 1000,
    random.order = FALSE,
    rot.per = 0.1,
    scale = c(3.5, 0.3),
    colors = brewer.pal(8, "Dark2")
  )
  dev.off()
  message("Successfully created 'wordcloud_rare.png' using words with frequency ", plot_min_freq, ".")
} else {
  message("The main frequency data frame (df_freq) is empty or invalid.")
}


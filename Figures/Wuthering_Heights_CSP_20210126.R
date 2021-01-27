# Create the right-skewed distribution of word frequency.

# Corpus: Wuthering Heights (classic of English literature) 
# from Project Gutenberg
# https://en.wikipedia.org/wiki/Wuthering_Heights

# This follows the Zipf's law.
# https://en.wikipedia.org/wiki/Zipf%27s_law
# https://www.ahsthenest.com/archive/2018/03/19/zipfs-law/

# In a hypothetical corpus, assume the most frequent word
# appears 1 million times. Then the nth ranked word is expected 
# to have the frequency of 1 million divided by n.

# Set working directory to source file location
# load(file = "Wuthering_Heights_CSP.RData")

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

library(gutenbergr)
# Download a random book to show the number of 
# total and unique uni-/bi-/trigrams.
# https://cran.r-project.org/web/packages/gutenbergr/vignettes/intro.html
# http://self.gutenberg.org/articles/eng/Wuthering_Heights

library(dplyr)

selected = "Wuthering Heights" # classic of English literature

temp_df = gutenberg_metadata %>%  filter(title == selected)

book_id = temp_df$gutenberg_id # 768

# WARNING: This code worked in December 2020, but not January 2021.
# But I already downloaded the corpus, and can load it from the local file.

wuthering_heights <- gutenberg_download(book_id)

wuthering_heights$text # long file

write.table(wuthering_heights$text, file = "sample_corpus.txt", sep=" ",
            quote = FALSE, row.names = FALSE, col.names = FALSE)

# ----------------------------------------------------------------------

library(stringr)
# Find all n-grams
# https://rpubs.com/vecano/n-gram-milestone-report

# More data visualizations related to corpus statistics
# https://rstudio-pubs-static.s3.amazonaws.com/139151_f77ad584fd74420cb162a9b1db3cb9cc.html

# Frequency table for words (uni-grams)

corpus = readLines("sample_corpus.txt") # the Wuthering Heights corpus

# remove all punctuation
corpus = gsub("[[:punct:][:blank:]]+", " ", corpus)

# convert to lower case
corpus = tolower(corpus) 

# separate each line into words
corpus = strsplit(corpus, "\\s+")

# combine all lines of words
corpus_string = c()

for (ii in 1:length(corpus)) {
  corpus_string = c(corpus_string, corpus[[ii]])
}

nn = length(corpus_string) # 120,772 words (uni-grams) in the corpus

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

# Find all words (uni-grams)

length(corpus_string) # 120,772 words (uni-grams) in the corpus
length(unique(corpus_string)) # 9,207 distinct words in the corpus

unigram_table = sort(table(corpus_string),decreasing=TRUE)

write.csv(unigram_table,"word_frequency.csv",row.names = FALSE)

# unigram_by_freq = sort(table(unigram_table),decreasing=TRUE)
# colnames are the frequency (uni-gram occurrence)
# 3,983 words appear once, and 1,500 words appear twice.

# unigram_by_freq

# Clean up the word frequency table
word_df = data.frame(unigram_table)
names(word_df) = c("word","counts")
# word_df$counts_log10 = log10(word_df$counts)
dim(word_df) # 9207 x 2

words_to_remove = c("","s","t","ll","m","d")
word_df = word_df[!(word_df$word %in% words_to_remove),]
word_df$word_rank = 1:nrow(word_df)
dim(word_df) # 9201 x 3

# ----------------------------------------------------------------------

library(ggplot2)

# Simple plot: plot(x, y)
# plot(word_df$word[1:50], word_df$counts[1:50])
# Visualization is really bad, so we definitely need ggplot2.

# word_df$counts[1:50]
#  [1] 4748 4572 4129 3536 2326 2224 2134 1895 1551 1484 1424 1391 1338 1250 1127 1106
# [17] 1065  938  936  927  843  819  806  785  727  689  687  654  633  486  476  454
# [33]  447  443  408  406  393  382  375  359  352  343  323  323  321  312  312  311
# [49]  310  309

display = word_df[1:40,] # show only the top 40 words

# original graph for the top 40 words
corpus_graph = ggplot(data=display, aes(x=word, y=counts)) +
  geom_bar(mapping = aes(x = word, y = counts), stat = "identity") +
  xlab("Word") + ylab("Frequency") +
  scale_y_continuous(limits=c(0,5000)) +
  theme_bw(base_size=16) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  

corpus_graph

ggsave("corpus_graph.png", corpus_graph, width=8, height=4.78)

# The data are large, so we should save the .RData file for future use.
save.image("Wuthering_Heights_CSP.RData")

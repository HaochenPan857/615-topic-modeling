library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(jsonlite)
library("tm")
library(tidyverse)
library(corrplot)
library(PerformanceAnalytics)
library(stringi)
library(readr)
IMDB_Dataset <- read_csv("D:/MSSP/IMDB Dataset.csv")
IMDB_df <- tibble(IMDB_Dataset)
glimpse(IMDB_df)
head(IMDB_df)
IMDB_df %>% 
  mutate(review_number = row_number()) ->IMDB_df 
IMDB_df <- IMDB_df %>% select(-sentiment)
library(tm)
myCorpus <- Corpus(VectorSource(IMDB_df$review))
data_clean <- tm_map(myCorpus,  removeWords, stopwords("english"))
data_clean <- tm_map(data_clean, removeNumbers)
data_clean <- tm_map(data_clean, removePunctuation)
data_clean <- tm_map(data_clean, removeWords, c("this","the","one","can","also","but","br","moive","film","time"))
tdm <- TermDocumentMatrix(data_clean)
inspect(tdm)
library(stm)
tdm_dfm <- tidy(tdm) %>% 
  cast_dfm(document = document, term = term, value = count)

tdm_lda <- stm(tdm_dfm, 
               K = 10, 
               verbose = FALSE, 
               init.type = "LDA")
summary(tdm_lda)
tidy(tdm_lda)
tdm_topics <- tidy(tdm_lda, matrix = "beta")
tdm_topics
library(ggplot2)
library(dplyr)

tdm_top_terms <- tdm_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 30) %>% 
  ungroup() %>%
  arrange(topic, -beta)

tdm_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
library(dplyr)
library(tidytext)

ap_td <- tidy(tdm)
ap_td

ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments

library(ggplot2)

ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 2000) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(n, term, fill = sentiment)) +
  geom_col() +
  labs(x = "Contribution to sentiment", y = NULL)
library(ggplot2)
library(igraph)
library(ggraph)

set.seed(1234)
ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 2000) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.7, "lines")) +
  theme_void()
library(tidyr)
beta_wide <- tdm_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
beta_wide
tdm_documents <- tidy(tdm_lda, matrix = "gamma")
tdm_documents
ggplot(tdm_documents, aes(gamma)) +
  geom_histogram(alpha = 0.6, color="darkblue", fill="lightblue") +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for each topics",
       y = "Number of documents", x = expression(gamma))
ggplot(tdm_documents, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 3) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))
  tidy(tdm) %>%
  filter(document == 6) %>%
  arrange(desc(count))
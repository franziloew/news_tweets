# linebreak for long text
linebreak <- function(s) {
  gsub('(.{1,70})(\\s|$)', '\\1\n', s)
}

# count non -na entries in lists
n <- function(x) {
  unlist(lapply(x, function(y){length(y) - is.na(y[1])}))
}

# Word Network
word_network <- function(x) {
  
  # create dataframe
  news_word_counts <- rt_news %>%
    filter(news_name == x) %>%
    dplyr::select(stripped_text) %>%
    unnest_tokens(paired_words, 
                  stripped_text, token = "ngrams", n = 2) %>%
    separate(paired_words, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stopwords$word) %>%
    filter(!word2 %in% stopwords$word) %>%
    count(word1, word2, sort = TRUE)
  
  # plot word network
  news_word_counts %>%
    filter(n > 2) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
    geom_node_point(color = Mycol[2], size = 3) +
    geom_node_text(aes(label = name), 
                   color = Mycol[1],
                   vjust = 1.8, size = 4) +
    labs(title = paste(x),
         x = "", y = "") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank())
  
}
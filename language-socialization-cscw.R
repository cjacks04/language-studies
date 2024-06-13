### Quick Similarity 


library(dplyr)
library(lubridate)
library(tidytext)
library(text2vec)
library(Matrix)
library(tidyr)


# Load your dataset into 'df'
df <- gravityspy_comments[,c(7,9:10,13:14)]
df <- df[which(df$comment_created_at <= "2016-07-01 12:00:00 UTC"),]
domain_words <- domain_science_words
science_words <- astronomy_words

# Make sure 'science_words' and 'domain_words' are vectors of words

# Define the time period for the community language model
community_period <- 4 # in weeks


# Function to calculate cosine similarity between two matrices
cosine_similarity <- function(matrix1, matrix2) {
  similarity_matrix <- sim2(as.matrix(matrix1), as.matrix(matrix2), method = "cosine", norm = "l2")
  # We only want the upper triangle of the similarity matrix, excluding the diagonal
  return(mean(similarity_matrix[upper.tri(similarity_matrix)]))
}

# Preprocess and tokenize the comments
df_tokenized <- df %>%
  mutate(week = floor_date(as.POSIXct(comment_created_at), "week")) %>%
  unnest_tokens(word, comment_body) %>%
  filter(!word %in% stop_words$word) %>%
  select(comment_user_login, week, word)

# Function to get the term frequency matrix for a given set of words
get_tfm <- function(data, words = NULL) {
  if (!is.null(words)) {
    data <- data %>% filter(word %in% words)
  }
  data %>%
    count(comment_user_login, word) %>%
    cast_sparse(comment_user_login, word, n)
}


# Calculate similarity for each user in each week they post a comment
results <- df_tokenized %>%
  group_by(comment_user_login, week) %>%
  # Create a nested data frame for each group
  nest() %>%
  mutate(
    # For each nested data frame, calculate the similarity
    similarity = map2(data, week, ~{
      user_data <- .$data
      community_data <- df_tokenized %>%
        filter(week < .$week & week >= .$week - weeks(community_period))
      
      # Get the term frequency matrix for the user and community
      user_tfm <- get_tfm(user_data)
      community_tfm <- get_tfm(community_data)
      
      # Calculate the cosine similarity between the user and community
      overall_cosine <- cosine_similarity(user_tfm, community_tfm)
      
      # Calculate cosine similarity for science words
      science_tfm <- get_tfm(community_data, science_words)
      science_cosine <- cosine_similarity(user_tfm, science_tfm)
      
      # Calculate cosine similarity for domain words
      domain_tfm <- get_tfm(community_data, domain_words)
      domain_cosine <- cosine_similarity(user_tfm, domain_tfm)
      
      # Prepare the output with the required information
      tibble(
        overall_cosine = overall_cosine,
        science_cosine = science_cosine,
        domain_cosine = domain_cosine,
        user_word_count = sum(rowSums(user_tfm)),
        community_word_count = sum(rowSums(community_tfm)),
        community_user_count = n_distinct(community_data$comment_user_login)
      )
    })
  ) %>%
  select(-data) %>%
  unnest(similarity)

# Write the results to a CSV file
write.csv(results, "cosine_similarity_results.csv", row.names = FALSE)


                                     
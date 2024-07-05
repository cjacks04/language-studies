# Compute cosines similarity

library(lsa)
library(data.table)
library(dplyr)
library(tidytext)
library(lubridate)

unigrams <- fread("gravity-spy-comments_2024-01-29.csv") # All unigrams
#unigrams <- fread("/Volumes/cbjackson2/Zooniverse Datasets/Gravity Spy/2024-01-29/gravity-spy-comments_2024-01-29.csv") # All unigrams

# Import new comments and prepare for analysis
unigrams <- unigrams %>%
  mutate(week = floor_date(as.POSIXct(comment_created_at), "week")) %>%
  unnest_tokens(unigram, comment_body) %>%
  filter(!unigram %in% stop_words$word) 
  

unigrams <- data.frame(unigrams)

cos_unigrams <- unigrams %>%
  select(comment_user_id,comment_user_login,comment_created_at,unigram) %>%
  mutate(week=lubridate::floor_date(comment_created_at, "week")) 

cos_unigrams <- cos_unigrams[which(cos_unigrams$week > "2016-03-06"),]

lexical_population <- cos_unigrams %>%
  distinct(comment_user_id,week)

# Group comments by username and week, concatenate the comments within each group
results_df <- data.frame(username = character(),
                         week = integer(),
                         cosine_similarity = numeric(),
                         user_innovations = numeric(),
                         community_viral = numeric(),
                         community_nonviral = numeric(),
                         all_users = numeric(),
                         stringsAsFactors = FALSE)

# Calculate cosine similarity for each user
for (i in 1:nrow(lexical_population)) {
  current_user <- lexical_population$comment_user_id[i]
  current_week <- lexical_population$week[i]
  
  #current_user = "3462"
  #current_week = "2016-04-24"
  
  # Subset the data up until the current week for cosine similarity calculation
  community_comments <- cos_unigrams %>%
    filter(week <= current_week) %>%
    filter(comment_user_id != current_user)
  community_users = n_distinct(community_comments$comment_user_login)
  
  # Get current words and generate vector 1 and 0
  community_words <- community_comments %>% 
    group_by(unigram) %>%
    summarize(unique_users = n_distinct(comment_user_id)) 
  community_words$prop_users = community_words$unique_users/length(unique(community_comments$comment_user_id))  
  community_words <- data.frame(community_words)
  
  limited_words <- community_words$unigram[which(community_words$unique_users <= 10)]
  limited_words_count <- length(limited_words)
  community_words <- data.frame(community_words[which(community_words$unique_users > 10),])
  community_words_count <- length(community_words$unigram)
  
  user_comments <- cos_unigrams %>%
    filter(week <= current_week) %>%
    filter(comment_user_id == current_user) %>%
    filter(!unigram %in% limited_words)
  
  user_words <- user_comments %>% 
    group_by(unigram) %>%
    count(unigram) 
  
  #remove words not in curr_words and hold for innovations
  innovations <- length(user_words$unigram[which(!user_words$unigram %in% community_words$unigram)])
  user_words <- data.frame(user_words[which(user_words$unigram %in% community_words$unigram),])
  names(user_words)[2] <- "user_count"
  
  # if user vector disappears then create a blank vector of 0s
  
  # merge datasets and
  word_eval <- merge(community_words,user_words, by = "unigram",all.x=TRUE)
  
  # add vectors
  word_eval$comm_vec <- 1
  word_eval$user_vec <- ifelse(!is.na(word_eval$user_count),1,0)
  
  # replcae NA in user_words with 0 
  word_eval["user_vec"][is.na(word_eval["user_vec"])] <- 0
  
  # Calculate cosine similarity between current user and other contributors
  cosine_sim <- cosine(word_eval$comm_vec, word_eval$user_vec)
  
  user_results <- data.frame(username = current_user,
                             week = current_week,
                             cosine_similarity = cosine_sim,
                             community_viral = community_words_count,
                             community_nonviral = limited_words_count,
                             user_innovations = innovations,
                             all_users = community_users,
                             stringsAsFactors = FALSE)
  
  # Add user_results to the overall results dataframe
  results_df <- rbind(results_df, user_results)
}

write.csv(results_df,"cosine_similarity.csv")

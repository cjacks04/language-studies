# load libraries
library(readr)
library(tm)
library(dplyr)
library(stringi)
malletwords <- scan("~/Box/Research/Language Studies (CS)/Analysis/Data/mallet.txt", character(), quote = "")


#######  import comments from Gravity Spy
gravityspy_comments <- read_csv("~/Box/Research/Language Studies (CS)/Analysis/Data/gravity-spy-comments.csv")

# Data transformations
gravityspy_comments$comment_body <-  gsub("http[[:alnum:][:punct:]]*", "", gravityspy_comments$comment_body)# remove URLs
gravityspy_comments$comment_body <-  gsub("http[^[:space:]]*", "", gravityspy_comments$comment_body) # remove non-ASCII characters
gravityspy_comments$comment_body <-  removeNumbers(gravityspy_comments$comment_body) # remove non-ASCII characters
gravityspy_comments$filtered_wordsnew <- tolower(gsub('[[:punct:]]', ' ', gravityspy_comments$comment_body))
gravityspy_comments$filtered_wordsnew <- removeWords(gravityspy_comments$filtered_wordsnew, c(stopwords("english"),malletwords))

#rename old comment column
gravityspy_comments$filtered_words <- NULL
gravityspy_comments$filtered_words <- gravityspy_comments$filtered_wordsnew
gravityspy_comments$filtered_wordsnew <- NULL

# parse comment and hashtag datasets to unigrams
gravityspy_unigram <- gravityspy_comments %>% unnest_tokens(unigram, 
                                               filtered_words, token = "ngrams", n = 1)
gravityspy_unigram$unigram  <- lemmatize_words(gravityspy_unigram$unigram)

gravitspy_words <- unique(gravityspy_unigram$unigram)
remove(gravityspy_unigram,gravityspy_comments)

####### import comments from Snapshot
snapshot_comments <- read_csv("~/Box/Research/Language Studies (CS)/Analysis/Extracting Vocabulary/snapshot_prep2.csv")

snapshot_comments$comment <-  gsub("http[[:alnum:][:punct:]]*", "", snapshot_comments$comment)# remove URLs
snapshot_comments$comment <-  gsub("http[^[:space:]]*", "", snapshot_comments$comment) # remove non-ASCII characters
snapshot_comments$comment <-  removeNumbers(snapshot_comments$comment) # remove non-ASCII characters
snapshot_comments$filtered_wordsnew <- tolower(gsub('[[:punct:]]', ' ', snapshot_comments$comment))
snapshot_comments$filtered_wordsnew <- removeWords(snapshot_comments$filtered_wordsnew, c(stopwords("english"),malletwords))

#rename old comment column
snapshot_comments$filtered_words <- NULL
snapshot_comments$filtered_words <- snapshot_comments$filtered_wordsnew
snapshot_comments$filtered_wordsnew <- NULL

# parse comment and hashtag datasets to unigrams
snapshot_unigram <- snapshot_comments %>% unnest_tokens(unigram, 
                                                            filtered_words, token = "ngrams", n = 1)
snapshot_unigram$unigram  <- lemmatize_words(snapshot_unigram$unigram)

snapshot_words <- unique(snapshot_unigram$unigram)
remove(snapshot_unigram,snapshot_comments)

#######  import comments from Planet Hunters
planet_comments <- read_csv("~/Box/Research/Language Studies (CS)/Analysis/Extracting Vocabulary/PHComments.csv")
planet_comments <- as.data.frame(planet_comments)
names(planet_comments)[5] <- "comment"

planet_comments$comment <- stringi::stri_trans_general(planet_comments$comment, "latin-ascii")
planet_comments$comment <-  gsub("http[^[:space:]]*", "", planet_comments$comment) # remove non-ASCII characters
planet_comments$comment <- tolower(gsub('[[:punct:]]', ' ', planet_comments$comment))
planet_comments$comment <-  gsub("http[[:alnum:][:punct:]]*", "", planet_comments$comment)# remove URLs
planet_comments$comment <-  removeNumbers(planet_comments$comment) # remove non-ASCII characters
planet_comments$filtered_wordsnew <- removeWords(planet_comments$comment, c(stopwords("english"),malletwords))

#rename old comment column
planet_comments$filtered_words <- NULL
planet_comments$filtered_words <- planet_comments$filtered_wordsnew
planet_comments$filtered_wordsnew <- NULL

# parse comment and hashtag datasets to unigrams
planet_unigram <- planet_comments %>% unnest_tokens(unigram, 
                                                        filtered_words, token = "ngrams", n = 1)
planet_unigram$unigram  <- lemmatize_words(planet_unigram$unigram)

planet_words <- unique(planet_unigram$unigram)
remove(planet_unigram,planet_comments)

# domain science words (get those in Snapshot)
citizen_science_words <- gravitspy_words[gravitspy_words %in% snapshot_words] 
setwd("~/Box/Research/Language Studies (CS)/Analysis/Extracting Vocabulary/")
write.table(citizen_science_words,"citizen_science_words.txt", row.names=FALSE,sep="\t", quote = FALSE)

# astonomy science words (get those also in Planet Hunters)
astronomy_words <- gravitspy_words[gravitspy_words %in% planet_words & ! gravitspy_words %in% citizen_science_words] 
setwd("~/Box/Research/Language Studies (CS)/Analysis/Extracting Vocabulary/")
write.table(astronomy_words,"astronomy_words.txt", row.names=FALSE,sep="\t", quote = FALSE)

# gravity spy domain science words (get those not in Planet Hunters and not in Snapshot)
domain_science_words <- gravitspy_words[!gravitspy_words %in% astronomy_words & !gravitspy_words %in% citizen_science_words] 
setwd("~/Box/Research/Language Studies (CS)/Analysis/Extracting Vocabulary/")
write.table(domain_science_words,"domain_science_words.txt", row.names=FALSE,sep="\t", quote = FALSE)



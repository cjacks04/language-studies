#### LANGUAGE SOCIALIZATION 

### IMPORT PACKAGES
library(readr)
library(data.table)
library(tm)
library(dplyr)
library(tidytext)
library(lubridate)
library(stringr)
library(tidyr)
library(trend)
library(kableExtra)
library(ggplot2)
library(ggsci)
library(changepoint)
library(caret)
library(rcompanion)
library(performance)
library(lme4)
library(stargazer)
#Functions
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      median = median(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

### IMPORT DATASETS 
comments <- fread("/Volumes/cbjackson2/Zooniverse Datasets/Gravity Spy/2024-01-29/gravity-spy-comments_2024-01-29.csv") 
comments_original <- comments
#comments <- comments[1:5000, ]

project_team <- c("uber_pye","areeda","mzevin1","RF45","jrsmith02","sbc538","adamamiller","olipatane","smarttiz","
                  jafeldt","mcoughlin","citizenscientist1994","cjackso3", "camallen","lmp579","sciencejedi","crowston",
                  "Carsten","jessiemcd","ejm553","srallen", "costerlu@syr.edu","lcalian","joeykey","matsoulina",
                  "trouille","zooniverse")

#### MUNGE DATA
comments$created_at <- as.POSIXct(comments$comment_created_at , format="%Y-%m-%d %H:%M:%S")
comments$comment_created_at <- NULL
comments <- comments %>%
  select(4:14)

#### ADD FEATURES TO COMMENTS 
comments$tags <- stringr::str_count(comments$comment_body, "\\#") 
comments$user_references <- stringr::str_count(comments$comment_body, "\\@") 
comments$hyperlinks <- stringr::str_count(comments$comment_body, "http[s]?://[\\S]+")
comments$comment_body2 <-  gsub("http[[:alnum:][:punct:]]*", "", comments$comment_body)# remove URLs
comments$comment_body2 <-  gsub("http[^[:space:]]*", "", comments$comment_body2) # remove non-ASCII characters
comments$question_count <- stringr::str_count(comments$comment_body2, "\\?")
comments$comment_body2 <- removeNumbers(comments$comment_body2) # remvoe numbers
comments$comment_body2 <- tolower(gsub('[[:punct:]]', ' ', comments$comment_body2))
comments$comment_body2 <- removeWords(comments$comment_body2, c(stopwords("english")))

unigram_comments <- comments %>% 
  select(comment_user_login,created_at,comment_id,comment_body2) %>%
  unnest_tokens(unigram, comment_body2, token = "ngrams", n = 1)

######## TOKEN INTRODUCTION 
token_first_use <- unigram_comments %>% 
  group_by(unigram)%>%
  summarize(
    token_engagement = length(unigram),
    token_createdat = min(created_at),
    token_recentuse = max(created_at),
    token_starter = paste0(comment_user_login[which(token_createdat==min(created_at))],""),
    token_nonstarteruse = length(unigram[which(token_starter!=comment_user_login)]),
    token_nextuse = min(created_at[which(comment_user_login !=token_starter)]),
    token_leaderuserbefore = length(unigram[which(created_at < token_nextuse)]),
    token_uniqueusers = length(unique(comment_user_login))
  )

###### USER TOKEN INNOVATION
token_innovation <- token_first_use  %>%  
  group_by(user_name = token_starter) %>% 
  summarize(token_innovations = length(unique(unigram))) 

token_innovation_week <- token_first_use  %>%  
  group_by(user_name = token_starter, week=floor_date(token_createdat, "week")) %>% 
  summarize(token_innovations = length(unique(unigram))) 

######## THREAD INTRODUCTION 
thread_first <- comments %>% 
  group_by(discussion_id)%>%
  summarize(
    thread_engagement = length(discussion_id),
    thread_createdat = min(created_at),
    thread_recentuse = max(created_at),
    thread_starter = paste0(comment_user_login[which(thread_createdat==min(created_at))],""),
    thread_nonstarteruse = length(discussion_id[which(thread_starter!=comment_user_login)]),
    thread_nextuse = min(created_at[which(comment_user_login !=thread_starter)]),
    thread_leaderuserbefore = length(discussion_id[which(created_at < thread_nextuse)]),
    thread_uniqueusers = length(unique(comment_user_login))
    )

###### USER THREAD INNOVATION

thread_innovation <- thread_first  %>%  
  group_by(user_name = thread_starter) %>% 
  summarize(threads_started = length(unique(discussion_id))) 

thread_innovation_week <- thread_first  %>%  
  group_by(user_name = thread_starter, week=floor_date(thread_createdat, "week")) %>% 
  summarize(threads_started = length(unique(discussion_id))) 

###########
user_summary <- comments %>% 
  group_by(user_name=comment_user_login) %>%
  summarise(
            join_date = min(floor_date(created_at, "week")),
            exit_date = max(floor_date(created_at, "week")),
            tenure = difftime(exit_date,join_date, units = "days"), 
            weeks_contributed = length(unique(floor_date(created_at, "week"))), 
            comments=length(comment_id),
            questions = sum(question_count),
            links = sum(hyperlinks),
            tags = sum(tags),
            user_references = sum(user_references)
  )

user_summary <- user_summary %>%
  left_join(token_innovation, by = "user_name") %>%
  left_join(thread_innovation, by = "user_name") 

user_summary <- user_summary %>%
  mutate(
    threads_started = coalesce(threads_started, 0),
    token_innovations = coalesce(token_innovations, 0)
    )

user_week_summary <- comments %>% 
  group_by(user_name=comment_user_login, user_id = comment_user_id, week=floor_date(created_at, "week")) %>%
  summarise(
    days = n_distinct(as.Date(created_at)), 
    comments=length(comment_id),
    questions = sum(question_count),
    links = sum(hyperlinks),
    tags = sum(tags),
    user_references = sum(user_references)
  )
    
user_week_summary <- user_week_summary %>%
  left_join(token_innovation_week, by = c("user_name","week")) %>%
  left_join(thread_innovation_week, by = c("user_name","week"))

user_week_summary <- user_week_summary %>%
  mutate(
    threads_started = coalesce(threads_started, 0),
    token_innovations = coalesce(token_innovations, 0)
  )

######################################################################################################################################################

### COMMUNITY ANALYSIS 

# Compute power laws https://cran.r-project.org/web/packages/poweRlaw/vignettes/b_powerlaw_examples.pdf and https://towardsdatascience.com/analysing-power-law-distributions-with-r-4312c7b4261b 

############################################################# COMMENTS 

comments_summary <- comments  %>%  
  group_by(week=floor_date(created_at, "week")) %>% 
  summarize(comment_count =length(comment_id)) %>%
  mutate(
    comment_growth = cumsum(comment_count),
    comment_pct = cumsum(comment_count)/sum(comment_count),
    comment_diff_growth = comment_pct - lag(comment_pct)) 

############################################################# THREADS

all_threads_summary <- comments  %>%  
  group_by(week=floor_date(created_at, "week")) %>% 
  summarize(thread_count =length(unique(discussion_id))) %>%
  mutate(
    thread_growth = cumsum(thread_count),
    thread_pct = cumsum(thread_count)/sum(thread_count),
    thread_diff_growth = thread_pct - lag(thread_pct))


############################################################# NEW THREADS

new_threads_summary <- thread_first  %>%  
  group_by(week=floor_date(thread_createdat, "week")) %>% 
  summarize(newthread_count =length(unique(discussion_id))) %>%
  mutate(
    newthread_growth = cumsum(newthread_count),
    newthread_pct = cumsum(newthread_count)/sum(newthread_count),
    newthread_diff_growth = newthread_pct - lag(newthread_pct))

############################################################# NEW TOKENS 

new_token_summary <- token_first_use  %>%  
  group_by(week=floor_date(token_createdat, "week")) %>% 
  summarize(newtoken_count =length(unigram)) %>%
  mutate(
    newtoken_growth = cumsum(newtoken_count),
    newtoken_pct = cumsum(newtoken_count)/sum(newtoken_count),
    newtoken_diff_growth = newtoken_pct - lag(newtoken_pct))

############################################################# ALL TOKENS 

all_token_summary <- unigram_comments  %>%  
  group_by(week=floor_date(created_at, "week")) %>% 
  summarize(token_count =length(unigram)) %>%
  mutate(
    token_growth = cumsum(token_count),
    token_pct = cumsum(token_count)/sum(token_count),
    token_diff_growth = token_pct - lag(token_pct))

############################################################# USER  

all_user_summary <- comments  %>%  
  group_by(week=floor_date(created_at, "week")) %>% 
  summarize(user_count =length(comment_user_login)) %>%
  mutate(
    user_growth = cumsum(user_count),
    user_pct = cumsum(user_count)/sum(user_count),
    user_diff_growth = user_pct - lag(user_pct)) 

############################################################# NEW USERS  

new_user_summary <- user_summary  %>%  
  group_by(week = join_date) %>% 
  summarize(newuser_count =length(user_name)) 


### COMBINE COMMUNITY DATA BY WEEK 

community_summary <- comments_summary  %>% 
  left_join(all_threads_summary, by = "week") %>%
  left_join(new_threads_summary, by = "week") %>%
  left_join(all_token_summary, by = "week") %>%
  left_join(new_token_summary, by = "week") %>%
  left_join(all_user_summary, by = "week") %>%
  left_join(new_user_summary, by = "week")

### SINCE THERE ARE WEEKS WITHOUT NEW USERS, NEED TO COMPUTE AFTER JOINING 

community_summary <- community_summary %>%
  mutate(
    newuser_count = coalesce(newuser_count, 0)
  ) %>%
  mutate(
    newuser_growth = cumsum(newuser_count),
    newuser_pct = cumsum(newuser_count)/sum(newuser_count),
    newuser_diff_growth = newuser_pct - lag(newuser_pct)) 


remove(all_threads_summary,new_threads_summary,all_token_summary,new_token_summary,all_user_summary,new_user_summary)

############################################################ 
####################  Community Analysis ###################
############################################################ 

##### Conduct pettit test (changepoint) https://rpubs.com/shirazipooya/Pettitt-Test on the number of values each day using the community_summary dataframe 
eco_count_plot.m <- community_summary %>%
 select(week,comment_count,thread_count,newthread_count,token_count,newtoken_count,user_count,newuser_count) %>%
  pivot_longer(
    cols = c(comment_count, thread_count, newthread_count, token_count, newtoken_count, user_count, newuser_count),
    names_to = "variable",
    values_to = "value"
  )

eco_count_plot.m$variable <- 
  recode(eco_count_plot.m$variable, 
         comment_count = "New Comments", 
         newthread_count = "New Threads",
         newtoken_count = "New Tokens",
         newuser_count = "New Users",
         thread_count = "Threads",
         token_count = "Tokens",
         user_count = "Users",
  )

### USE THE DATE AFTER THE LAUNCH 
eco_count_plot.m.filtered <- eco_count_plot.m  %>% 
filter(week >= as.Date("2016-10-09")) 
  
growth_pettit_count <- eco_count_plot.m.filtered %>% 
  group_by(variable) %>% 
  mutate(pettit_statistics = pettitt.test(value)$statistic,
         pettit_estimate = pettitt.test(value)$estimate,
         pettit_p = pettitt.test(value)$p.value,
         week = eco_count_plot.m.filtered[['week']][pettitt.test(value)$estimate]) %>% 
  distinct(variable,pettit_statistics,pettit_estimate,week,pettit_p)

growth_sens_count <- eco_count_plot.m.filtered %>% 
  group_by(variable) %>% 
  mutate(sens_statistics = sens.slope(value, conf.level = 0.95)$statistic,
         sens_estimate = sens.slope(value, conf.level = 0.95)$estimate,
         sens_p = sens.slope(value, conf.level = 0.95)$p.value)%>% 
  distinct(variable,sens_statistics,sens_estimate,sens_p)

community_stats <- merge(growth_pettit_count,growth_sens_count, by = "variable", all.x = TRUE)
community_stats <- community_stats %>% 
  mutate(across(c(5:8), ~ round(., 3)))

##### Conduct growth models (answering ... which growth model describes content) https://rpubs.com/jaelison/200149 or #https://www.marinedatascience.co/blog/2019/09/28/comparison-of-change-point-detection-methods/

community_stats <- community_stats[,c(1,4,2,3,5:8)]

community_table <- community_stats %>%
  kbl(format = 'latex',
      col.names = c("Measure","Week","Statistic","Estimate","Sig.",
                    "Statistic","Estimate","Sig."),
      align="r") %>%
  kable_classic(full_width = F, html_font = "helvetica") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right") %>%
  add_header_above(c(" ", "Change-point(Pettit)" = 4, "Rate of Change (Sens Slope)" = 3)) 

remove(community_stats)

feature_growth <- ggplot(eco_count_plot.m, aes(as.Date(week), value)) + 
  geom_line(size=.5) + 
  geom_smooth(size=.7, color="grey") +
  labs(x="Date",y="Feature growth") + 
  scale_x_date(date_labels="%b %y",date_breaks  ="6 month") + 
  theme_classic() + 
  scale_color_nejm() +
  theme(plot.title = element_text(size=12,face = "bold"),
        legend.position = "none",
        axis.text = element_text(size=12,face = "bold"),
        axis.text.x = element_text(size=10,face="bold", angle=90),
        axis.title = element_blank()) +
  geom_vline(aes(xintercept=as.Date("2016-10-12")),linewidth=.25, colour="red") + 
  #geom_text(aes(x=as.Date("2016-10-11"), label="", y=.50), colour="red", angle=90, vjust = -1, text=element_text(size=2)) +
  facet_wrap(~variable, scales="free_y")


### SHOW CUMULATIVE GROWTH TREND IN COMMUNITY
eco_growth_plot.m <- community_summary %>%
  select(week,comment_growth,thread_growth,newthread_growth,token_growth,newtoken_growth,user_growth,newuser_growth) %>%
  pivot_longer(
    cols = c(comment_growth,thread_growth,newthread_growth,token_growth,newtoken_growth,user_growth,newuser_growth),
    names_to = "variable",
    values_to = "value"
  )

eco_growth_plot.m$variable <- 
  recode(eco_growth_plot.m$variable, 
         comment_growth = "Comments", 
         thread_growth = "Threads",
         newthread_growth = "New Threads",
         token_growth = "Tokens",
         newtoken_growth = "New Tokens",
         user_growth = "Users",
         newuser_growth = "New Users",
  )

cumulative_growth <- ggplot(eco_growth_plot.m, aes(as.Date(week), value)) + 
  geom_line(size=.5) + 
  geom_smooth(size=.7, color="grey") +
  labs(x="Date",y="Feature growth") + 
  scale_x_date(date_labels="%b %y",date_breaks  ="6 month") + 
  theme_classic() + 
  scale_color_nejm() +
  theme(plot.title = element_text(size=12,face = "bold"),
        legend.position = "none",
        axis.text = element_text(size=12,face = "bold"),
        axis.text.x = element_text(size=10,face="bold", angle=90),
        axis.title = element_blank()) +
  geom_vline(aes(xintercept=as.Date("2016-10-12")),linewidth=.25, colour="red") + 
  #geom_text(aes(x=as.Date("2016-10-11"), label="", y=.50), colour="red", angle=90, vjust = -1, text=element_text(size=2)) +
  facet_wrap(~variable, scales="free_y")

#### TRENDS FOR ALL FEATURES
eco_growth_plotall.m <- community_summary %>%
  select(week,
         comment_count,
         comment_growth,
         thread_count,
         thread_growth,
         newthread_count,
         newthread_growth,
         token_count,
         token_growth,
         newtoken_count,
         newtoken_growth,
         user_count,
         user_growth,
         newuser_count,
         newuser_growth) %>%
  pivot_longer(
    cols = c(comment_count,
             comment_growth,
             thread_count,
             thread_growth,
             newthread_count,
             newthread_growth,
             token_count,
             token_growth,
             newtoken_count,
             newtoken_growth,
             user_count,
             user_growth,
             newuser_count,
             newuser_growth),
    names_to = "variable",
    values_to = "value"
  )

eco_growth_plotall.m$variable <- 
  recode(eco_growth_plotall.m$variable,
         comment_count = "Comments",
         comment_growth = "Comments(Cum)",
         thread_count  = "Threads",
         thread_growth = "Threads(Cum)",
         newthread_count  = "New Threads",
         newthread_growth = "New Threads(Cum)",
         token_count  = "Tokens",
         token_growth = "Tokens(Cum)",
         newtoken_count  = "New Tokens",
         newtoken_growth  = "New Tokens(Cum)",
         user_count  = "Users",
         user_growth  = "Users(Cum)",
         newuser_count = "New Users",
         newuser_growth = "New Users(Cum)"
  )

all_growth <- ggplot(eco_growth_plotall.m, aes(as.Date(week), value)) + 
  geom_line(size=.5) + 
  geom_smooth(size=.7, color="grey") +
  labs(x="Date",y="Feature growth") + 
  scale_x_date(date_labels="%b %y",date_breaks  ="6 month") + 
  theme_minimal() + 
  scale_color_nejm() +
  theme(plot.title = element_text(size=12,face = "bold"),
        legend.position = "none",
        axis.text = element_text(size=12,face = "bold"),
        axis.text.x = element_text(size=10,face="bold", angle=90),
        axis.title = element_blank()) +
  geom_vline(aes(xintercept=as.Date("2016-10-12")),linewidth=.25, colour="red") + 
  #geom_text(aes(x=as.Date("2016-10-11"), label="", y=.50), colour="red", angle=90, vjust = -1, text=element_text(size=2)) +
  facet_wrap(~variable, scales="free_y", ncol = 2)

pdf("/Users/coreyjackson/Library/CloudStorage/Box-Box/_working papers/language socialization/figures/all_growth.pdf", width=11, height=10)
all_growth
dev.off()

nejm_colors <- pal_nejm("default")(8) 

all_growth_png <- ggplot(eco_growth_plotall.m, aes(as.Date(week), value)) + 
  geom_point(size=.25) + 
  geom_smooth(size=.7, color="#E18727FF") +
  labs(x="Date",y="Feature growth") + 
  scale_x_date(date_labels="%b %y",date_breaks  ="6 month") + 
  theme_minimal() + 
  scale_color_nejm() +
  theme(plot.title = element_text(size=12,face = "bold"),
        legend.position = "none",
        axis.text = element_text(size=6,face = "bold"),
        axis.text.x = element_text(angle=90),
        axis.title = element_blank()) +
  geom_vline(aes(xintercept=as.Date("2016-10-12")),linewidth=.5, colour="#0072B5FF") + 
  #geom_text(aes(x=as.Date("2016-10-11"), label="", y=.50), colour="red", angle=90, vjust = -1, text=element_text(size=2)) +
  facet_wrap(~variable, scales="free_y", ncol = 2)


png("/Users/coreyjackson/Library/CloudStorage/Box-Box/_working papers/language socialization/figures/all_growth.png",width = 6, height = 5, units='in', res = 300)
all_growth_png
dev.off()

############################################################ 
####################  Volunteer Analysis ###################
############################################################ 

# ADD OTHER PROJECT DATA
class_summary <- fread("/Volumes/cbjackson2/language/analysis_v2/user_week_contributions2024-04-14.csv") 
class_summary$V1 <- NULL
class_summary$week <- as.Date(class_summary$week)

user_class_summary <- class_summary %>% 
  group_by(user_id) %>% 
  summarize(
    joined_day = min(as.Date(started)),
    joined_week = min(as.Date(floor_date(started, "week"))),
    last_day = max(as.Date(finished)),
    last_week = max(as.Date(floor_date(finished, "week"))),
    last_comment_week = max(as.Date(floor_date(finished[which(comments > 0)], "week"))),
    last_classification_week = max(as.Date(floor_date(finished[which(classifications > 0)], "week")))
  )

cosine <- fread("/Volumes/cbjackson2/language/analysis_v2/cosine_similarity.csv") 
cosine$V1 <- NULL; names(cosine)[1] <-"user_id"

user_week_summary$week <- as.Date(user_week_summary$week)
cosine$week <- as.Date(cosine$week)

user_week_summary <- user_week_summary %>% 
  left_join(cosine, by = c("user_id","week")) %>% 
  left_join(class_summary %>% select(user_id, week,classifications), c("user_id","week"))
user_week_summary$user_innovations <- NULL

user_week_summary <- user_week_summary%>% 
  left_join(user_class_summary, by="user_id")

# Compute userinformation pertaining to when they joined and period since project st
user_week_summary <- user_week_summary %>% 
  mutate(weeks_since_joined = difftime(week,joined_week, units = "weeks"),
         weeks_project_started = difftime(week,as.Date("2016-10-09"),units = "weeks")
         )

user_week_summary <- data.frame(user_week_summary)
user_week_summary <- user_week_summary[order(user_week_summary$user_id,user_week_summary$week),]
user_week_summary <- as.data.table(user_week_summary)[, user_comment_week := 1:.N,, by = list(user_id)] 
user_week_summary <- data.frame(user_week_summary)

user_week_summary <- user_week_summary %>%
  arrange(user_name, week) %>%
  group_by(user_name) %>%
  mutate(weeks_between = as.numeric(difftime(week, lag(week), units = "weeks"))) %>%
  ungroup()

launch_date <- as.Date(as.POSIXct("2016-10-12 12:00:00",  format="%Y-%m-%d %H:%M:%S"))
launch_week <-  floor_date(launch_date, "week")

testers <- unique(user_week_summary$user_name[which(user_week_summary$joined_day < launch_date)])

####### NEWCOMERS
newcomers <- user_week_summary %>%
 filter(user_comment_week == 1 & !is.na(cosine_similarity)) %>%
  filter(!user_name %in% project_team & !user_name %in% testers)

newcomers_summary_stats <- newcomers %>%
  select(4:12,16) %>%
  summarise_all(list(mean = ~mean(.),
                     median = ~median(.),
                     sd = ~sd(.)))

newcomers_summary_stats <- newcomers_summary_stats %>%
  pivot_longer(cols = everything(), names_to = "statistic", values_to = "value") %>%
  mutate(value = round(value, digits=2))

# Do users have a more challenging time achieving similarity overtime and what factors play a role in simialrity? 
# Fit a linear regression model
cosine.newcomer.regression <- lm(cosine_similarity ~ week + weeks_since_joined + days, data = newcomers)
summary(cosine.newcomer.regression)

### FULL USER SUMMARY
newcomers <- newcomers %>%
  left_join(user_summary %>% select(user_name, weeks_contributed), by = "user_name") %>%
  mutate(retained = as.factor(ifelse(weeks_contributed>1,1,0)))


#### ADD RETAINED 
predictors <- newcomers %>%
  select(week, weeks_since_joined, days, comments, questions, links, tags, token_innovations, 
         user_references, threads_started, cosine_similarity, community_viral, 
         community_nonviral, all_users, retained)

trainIndex_commenter <- createDataPartition(predictors$retained, p = .7,
                                            list = FALSE,
                                            times = 1)
train_commenter <- predictors[ trainIndex_commenter,]
valid_commenter <- predictors[-trainIndex_commenter,]


# Fit a logistic regression model
retained.logistic <- glm(retained ~ week + weeks_since_joined + days + comments + questions + links +
               tags + token_innovations + user_references + threads_started + cosine_similarity +
               community_viral + community_nonviral + all_users, 
             data = train_commenter, family = binomial)

logit2prob(coef(retained.logistic)) #coefficients
nagelkerke(retained.logistic) # performance measures

predictcommenter <- predict(retained.logistic , newdata = valid_commenter, type = 'response')
predictcommenter <- as.integer(predictcommenter>0.5)
confusionMatrix(as.factor(predictcommenter),valid_commenter$retained)

t.test(cosine_similarity ~ retained, data = predictors)

###### NEWCOMER INTERACTIONS OVERTIME
newcomer_summary_time <- newcomers %>%
  select(3,4:12) %>%
  group_by(week)  %>%
  summarise_all(list(mean = ~mean(.)))

###### FULL DATASET

user_week_summary_qualified <- user_week_summary %>%
  filter(!user_name %in% project_team & !user_name %in% testers)

community_usertrends <- user_week_summary_qualified %>%
  select(25,4:16,26,27) %>%
  group_by(user_comment_week)  %>%
  summarise_all(list(mean = ~mean(., na.rm=TRUE),
                     median = ~median(., na.rm=TRUE),
                     sd = ~sd(., na.rm=TRUE))) 

week_con <- user_week_summary  %>%
  group_by(user_comment_week)  %>%
  summarize(
    users = length(unique(user_name)))
remove(week_con)

community_usertrends <- community_usertrends %>%
  left_join(week_con %>% select(user_comment_week, users), by = "user_comment_week" )


#CLEAN NAs 
user_week_summary_qualified_CLEANED <- user_week_summary_qualified %>%
 filter(!is.na(cosine_similarity)) %>%
  mutate(weeks_between = ifelse(is.na(weeks_between), 0, weeks_between))


# Scale the predictor variables
data_scaled <- user_week_summary_qualified_CLEANED %>%
  mutate_at(vars(week, weeks_since_joined, days, weeks_between, comments, token_innovations, 
                 tags, questions, links, user_references, classifications, user_count, 
                 user_comment_week, community_viral, community_nonviral), scale)

# Fit a linear mixed-effects model with scaled predictors
cosine.mixed.effects <- lmer(cosine_similarity ~ week + weeks_since_joined + days + weeks_between + comments + 
                token_innovations + tags + questions + links + user_references + classifications + 
                user_count + user_comment_week + community_viral + community_nonviral + 
                (1 | user_name), data = data_scaled)

summary(cosine.mixed.effects)

# Fit a null model (random intercept only)
null_model <- lmer(cosine_similarity ~ (1 | user_name), data = data_scaled)

# Calculate Nagelkerke pseudo-R-squared
nagelkerke(cosine.mixed.effects, null_model)

# MODELS FOR LATEX
stargazer(cosine.newcomer.regression,retained.logistic,cosine.mixed.effects, title="Retention Models",star.cutoffs = c(0.05, 0.01, 0.001), dep.var.labels=c(""),
          object.names = TRUE,
          font.size = "small",
          align = TRUE,
          #omit.stat=c("f", "ser"),
          column.sep.width = "-10pt",
          no.space = TRUE, # to remove the spaces after each line of coefficients
          type = "latex",
          t.auto=F, p.auto=F, 
          #ci = TRUE, 
          report = ('vcs*'), 
          single.row = TRUE,
          digits=2
          
)


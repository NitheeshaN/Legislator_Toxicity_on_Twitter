library(dplyr)
library(interplot)
library(effects)
library(readxl)
library(readr)
library(tidyr)

############################################################################
########################### METADATA p/ HANDLE ###########################
Race_Variable_Complete2_0 <- read_excel("Race Variable Complete2.0.xlsx")
nodes_R1 <- readRDS("nodes_R1.rds")
#import the handles csv

############# GENDER
AL_handles <- left_join(AL_handles, nodes_R1, by = c("name" = "name", "chamber" = "chamber", "state" = "state"))

############# RACE
AL_handles <- left_join(AL_handles, Race_Variable_Complete2_0, by = c("name" = "Name", "state" = "State", "chamber" = "Chamber", "party.x" = "Party"))

## clean
names(AL_handles)
AL_handles <- AL_handles[,c(1:6,17,19,20)]
write.csv(AL_handles, "AL_handles.csv")

############################################################################
########################### BUILD SCORED DATASET ###########################
#master.frameDE
#DE_handles as state_handles
#DE_twts

## merge the textID from masterframe to the state_twts 
AL_twts <- tibble::rowid_to_column(AL_twts, "Row_ID")

scored <- left_join(AL_twts, master.frameAL2, by = c("Row_ID" = "text_id"))

## clean scored data
names(scored)
scored <- scored[,c(1:2,6,11,14,20:21)]
sum(apply(scored, 1, anyNA)) #1195
scored <- scored %>% drop_na(TOXICITY)

## add race, gender, party, handles w/ labeler function
labeler <- function(df){
  
  # all unique twitter handles in the all_mention_data legis column
  legislators <- unique(df$legis)
  
  #make gender/race columns - fill with NA for now, will fill in later
  df$gender <- rep(NA, length(dim(df)[1]))
  df$race <- rep(NA, length(dim(df)[1]))
  df$party <- rep(NA, length(dim(df)[1]))
  df$handle_count <- rep(NA, length(dim(df)[1]))
  df$name <- rep(NA, length(dim(df)[1]))
  
  # iterate through the unique twitter handles
  for(leg in legislators){
    
    # index in legislator handles data that has that legislator - get row number of legislator
    leg.index <- which(state_handles$handle_1  == leg | state_handles$handle_2 == leg)
    
    # fill in Gender and Race for that legislator
    df[df$legis == leg, 'gender'] <- state_handles$gender[leg.index]
    df[df$legis == leg, 'race'] <- state_handles$race[leg.index]
    df[df$legis == leg, 'party'] <- state_handles$party[leg.index]
    df[df$legis == leg, 'handle_count'] <- state_handles$handles[leg.index]
    df[df$legis == leg, 'name'] <- state_handles$name[leg.index]
    
    
  }
  return(df)
  
}
scored <- labeler(scored)

## add total number of mentions
scored$mention <- 1
mentions <- aggregate(mention ~ legis, data = scored, sum)
scored <- merge(scored, mentions, by = "legis", all.x = T)

scored$mention.x <- NULL
names(scored)[names(scored) == 'mention.y'] <- 'mentions'

## add retweet, like, reply : scored$retweet <- scored$public_metrics$retweet_count

## SAVE ##
saveRDS(scored, "scored.AL.rds")

########################### SUBSET SCORED DATASET: for MA ###########################
scored.ma <- scored %>%
  subset(created_at >= as.POSIXct("2021-03-31 23:59:59"))

## SAVE ##
saveRDS(scored.ma, "scored.AL.ma.rds") 

########################### AGGREGATE SCORED DATASET ###########################
scored.agg <- scored %>%
  group_by(name) %>%
  mutate(TOX.avg = mean(TOXICITY),
         SEV.TOX.avg = mean(SEVERE_TOXICITY),
         like.avg = mean(public_metrics$like_count),
         retweet.avg = mean(public_metrics$retweet_count),
         reply.avg = mean(public_metrics$reply_count)
        )

## clean
names(scored.agg)
scored.agg <- scored.agg[,c(1,8:18)]
scored.agg <- scored.agg %>% distinct()

names(scored.agg)
scored.agg<- scored.agg[,2:12]
scored.agg <- scored.agg %>%                 
  group_by(name) %>%
  mutate(mentions = sum(mentions))
scored.agg <- scored.agg %>% distinct()

#scored.DE.agg$total.mentions <- aggregate(mentions ~ name, scored.DE.agg, sum)

## SAVE ##
saveRDS(scored.agg, "scored.AL.agg.rds")

###################### AGGREGATE SCORED DATASET for MA #########################
scored.ma.agg <- scored.ma %>%
  group_by(name) %>%
  mutate(TOX.avg = mean(TOXICITY),
         SEV.TOX.avg = mean(SEVERE_TOXICITY),
         like.avg = mean(public_metrics$like_count),
         retweet.avg = mean(public_metrics$retweet_count),
         reply.avg = mean(public_metrics$reply_count)
  )

## clean
names(scored.ma.agg)
scored.ma.agg <- scored.ma.agg[,c(1,8:18)]
scored.ma.agg <- scored.ma.agg %>% distinct()

names(scored.ma.agg)
scored.ma.agg<- scored.ma.agg[,2:12]
scored.ma.agg <- scored.ma.agg %>%                 
  group_by(name) %>%
  mutate(mentions = sum(mentions))
scored.ma.agg <- scored.ma.agg %>% distinct()


## SAVE ##
saveRDS(scored.ma.agg, "scored.MA.ma.agg.rds")

############################################################################
####################### BUILD SCORED LEGIS DATASET #########################
#note I don't have to split up this dataset since this dataset ONLY has the July-August legislator tweets

# load the legis tweets masterframe
# load the legis tweets 
# load state handles

## merge the textID from masterframe to the state_twts 
legis_twts <- tibble::rowid_to_column(AL_legis_twts, "Row_ID")

scored.leg <- left_join(legis_twts, master.framelegAL, by = c("Row_ID" = "text_id"))

## clean scored data
names(scored.leg)
scored.leg <- scored.leg[,c(14:16,18, 25:26, 28:29)]
sum(apply(scored.leg, 1, anyNA)) #1195
scored.leg <- scored.leg %>% drop_na(TOXICITY)

## add race, gender, party, handles w/ labeler function
labeler2 <- function(df){
  
  # all unique twitter handles in the all_mention_data legis column
  legislators <- unique(df$handle)
  
  #make gender/race columns - fill with NA for now, will fill in later
  df$gender <- rep(NA, length(dim(df)[1]))
  df$race <- rep(NA, length(dim(df)[1]))
  df$party <- rep(NA, length(dim(df)[1]))
  df$handle_count <- rep(NA, length(dim(df)[1]))
  df$name <- rep(NA, length(dim(df)[1]))
  
  # iterate through the unique twitter handles
  for(leg in legislators){
    
    # index in legislator handles data that has that legislator - get row number of legislator
    leg.index <- which(state_handles$handle_1  == leg | state_handles$handle_2 == leg)
    
    # fill in Gender and Race for that legislator
    df[df$handle == leg, 'gender'] <- state_handles$gender[leg.index]
    df[df$handle == leg, 'race'] <- state_handles$race[leg.index]
    df[df$handle == leg, 'party'] <- state_handles$party[leg.index]
    df[df$handle == leg, 'handle_count'] <- state_handles$handles[leg.index]
    df[df$handle == leg, 'name'] <- state_handles$name[leg.index]
    
    
  }
  return(df)
  
}

scored.leg <- labeler2(scored.leg)

## add total number of mentions
scored.leg$mention <- 1
mentions <- aggregate(mention ~ handle, data = scored.leg, sum)
scored.leg <- merge(scored.leg, mentions, by = "handle", all.x = T)

scored.leg$mention.x <- NULL
names(scored.leg)[names(scored.leg) == 'mention.y'] <- 'Lmentions'

## SAVE ##
saveRDS(scored.leg, "scored.leg.AL.rds")

########################### AGGREGATE SCORED DATASET ###########################
scored.leg.agg <- scored.leg %>%
  group_by(name) %>%
  mutate(LTOX.avg = mean(TOXICITY),
         LSEV.TOX.avg = mean(SEVERE_TOXICITY),
         Llike.avg = mean(like_count),
         Lretweet.avg = mean(retweet_count),
         Lreply.avg = mean(reply_count)
  )

## clean
names(scored.leg.agg)
scored.leg.agg <- scored.leg.agg[,c(1, 9:19)]
scored.leg.agg <- scored.leg.agg %>% distinct()

names(scored.leg.agg)
scored.leg.agg <- scored.leg.agg[,c(2:12)]
scored.leg.agg <- scored.leg.agg %>%                 
  group_by(name) %>%
  mutate(Lmentions = sum(Lmentions))
scored.leg.agg <- scored.leg.agg %>% distinct()

## SAVE ##
saveRDS(scored.leg.agg, "scored.ALleg.agg.rds")

########################## MERGE LEG/MENTIONS DATASET ##########################
AL.data <- merge(scored.leg.agg, scored.ma.agg, by = c("name" = "name", "race" = "race", "party" = "party", "gender" = "gender"), all= T)

AL.data$state <- rep("AL", nrow(AL.data))

saveRDS(AL.data, "AL.data.rds")




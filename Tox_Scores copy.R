library(peRspective)
library(readr)
library(tibble)

set.seed(1234)

#step 0: load _twts
Twts <- readRDS("all_mentions.rds")
  
#step 1: add a unique column ID to the Twitter data frame
Twts <- tibble::rowid_to_column(Twts, "Row_ID")

#step 2: specify where in the dataframe the text and the unique ID is for the API
Twts_text <- tibble(
  text = Twts$text,
  textid = Twts$Row_ID)

#step 3: set up what you will be sequencing through
seq.list <- seq(1, nrow(Twts), 100)

#step 4: loop through data and assign toxcity scores in batches of 1000
for(i in seq.list){
  Sys.sleep(10)
  ### for the first item in the sequence --> add it to the master frame to initialize a dataframe
  if(i == 1){
    sect1 <- Twts_text[(1:100),]
    sect1 = cbind(sect1, sect1 %>%
                    prsp_stream(text = text,
                                text_id = textid,
                                score_model = c("TOXICITY", "SEVERE_TOXICITY"),
                                key = "",
                                safe_output = T,
                                verbose = T
                    ))
    master.frame <- sect1
    
    ### for all the others (except the last part of the sequence) --> take the subset you want, 
    #### perform an action and rbind to masterframe
  }else if((i > 1) & (i < tail(seq.list, n = 1))){
    middle.sect1 <- Twts_text[i:(i+99),]
    middle.sect1 = cbind(middle.sect1, middle.sect1 %>%
                           prsp_stream(text = text,
                                       text_id = textid,
                                       score_model = c("TOXICITY", "SEVERE_TOXICITY"),
                                       key = "",
                                       safe_output = T,
                                       verbose = T
                           ))
    master.frame <- rbind(master.frame, middle.sect1)
    
    ### for the last one, go from the last index of the sequence to the length of the dataframe, 
    #### perform an action, and rbind to masterframe    
  }else if(i == tail(seq.list, n = 1)){
    final.sect1 <- Twts_text[(i:dim(Twts_text)[1]),]
    final.sect1 = cbind(final.sect1, final.sect1 %>%
                          prsp_stream(text = text,
                                      text_id = textid,
                                      score_model = c("TOXICITY", "SEVERE_TOXICITY"),
                                      key = "",
                                      safe_output = T,
                                      verbose = T
                          ))
    master.frame <- rbind(master.frame, final.sect1)
    
  }
  #saveRDS(master.frame, "master.sub.frame.rds")
  
}

saveRDS(master.frame, "master.frame.rds")





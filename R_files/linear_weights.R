# R Script for calculating linear weights for wOBA components
# does NOT use the full set of play-by-play data from 1950, but just the
# data from 2021 for demonstration purposes
library(dplyr)

# league stats since 1950, non-pitcher
lg_since_50 = readr::read_csv("https://raw.githubusercontent.com/ahawl42/changepoints-mlb-offence/main/data/fg_lg_since50.csv")
colnames(lg_since_50)[8:9] = c("K.", "BB")

lg_since_50 = lg_since_50 %>%
  mutate(BB. = as.numeric(gsub("%", "", BB))/100,
         K. = as.numeric(gsub("%", "", K.))/100,
         RperPA = R/PA)
colnames(lg_since_50)[1] = "year"

# play-by-play data for 2021
# zip file can be downloaded from data folder in repository, read the extracted
# csv file
pbp = read.csv("~/retrosheet_pbp_2021.csv")
pbp = pbp %>% 
  select(GAME_ID, INN_CT, BAT_HOME_ID, INN_NEW_FL, PA_NEW_FL, START_BAT_SCORE_CT, 
                                    START_BASES_CD, EVENT_CD, EVENT_OUTS_CT, DP_FL, TP_FL, ERR_CT, 
                                    END_BASES_CD, EVENT_RUNS_CT, INN_RUNS_CT, OUTS_CT)
## Function to calculate linear weights
# takes play-by-play data frame, x, returns a set of linear weights for all
# event types
linear_weights = function(x) {
  # make new useful variables
  pbp_sel = x %>%
    mutate(START_OUTS_CT = OUTS_CT,
           END_OUTS_CT = OUTS_CT + EVENT_OUTS_CT,
           INN_END_FL = (END_OUTS_CT == 3),
           START_BASE_OUT_CD = START_BASES_CD + (8*START_OUTS_CT),
           END_BASE_OUT_CD = ifelse(INN_END_FL == TRUE, 24, END_BASES_CD + (8*END_OUTS_CT))) %>%
    group_by(GAME_ID, INN_CT, BAT_HOME_ID) %>%
    # calculate runs through the end of the inning
    mutate(RUNS_TO_END_INN = max(INN_RUNS_CT)-INN_RUNS_CT) %>%
    ungroup() %>%
    # run expenctancy: on average, how many runs are scored through
    # the end of the inning from that base-out-state
    group_by(START_BASE_OUT_CD) %>%
    mutate(START_RUN_EXPECT = mean(RUNS_TO_END_INN)) %>%
    ungroup()
  
  re = pbp_sel %>% select(START_BASE_OUT_CD, START_RUN_EXPECT) %>% distinct()
  colnames(re) = c("BASE_OUT", "END_RUN_EXPECT")
  # join with above to get the run expectancy after the play
  pbp_sel = pbp_sel %>%
    left_join(re, by = c("END_BASE_OUT_CD"="BASE_OUT")) %>%
    #set the final out to a zero run expectancy
    replace(is.na(.), 0) %>%
    #calculate the linear weights for each event type
    group_by(EVENT_CD) %>%
    mutate(LIN_WT = mean(END_RUN_EXPECT-START_RUN_EXPECT+EVENT_RUNS_CT)) %>%
    ungroup()
  
  ln_wts = pbp_sel %>% select(EVENT_CD, LIN_WT) %>% distinct()
  ln_wts = ln_wts %>% 
    arrange(EVENT_CD) %>%
    #relative to an out
    mutate(rel_out = LIN_WT - LIN_WT[1])
  
  return(ln_wts)
}

#linear weights
lw_1 = linear_weights(pbp)
# relevant events
events = c(14, 16, 20, 21, 22, 23, 4, 6)
# reset caught stealing
lw_1[5, 3] = lw_1[5, 2]

wts1 = lw_1 %>% 
  filter(EVENT_CD %in% events) %>%
  select(rel_out) %>%
  t()
wts = as.data.frame(round(wts1,3))
rownames(wts) = NULL #drop row name
# reset column names
colnames(wts) = c("SB", "CS", "wBB", "wHBP", "w1B", "w2B", "w3B", "wHR")

#get leage OBP
lg_obp = lg_since_50 %>%
  filter(year == 2021) %>%
  transmute(sum(OBP*PA)/sum(PA)) %>%
  distinct() %>% unlist()

wts$lg_obp = round(lg_obp, 3)
wts$Year = 2021
#stolen base weight is constant
wts$SB = 0.200
#reorder
wts = wts %>% select(Year, lg_obp, wHBP, wBB, w1B, w2B, w3B, wHR, SB, CS)


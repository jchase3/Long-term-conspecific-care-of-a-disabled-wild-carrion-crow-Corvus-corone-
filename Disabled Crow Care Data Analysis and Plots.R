library(data.table)
library(lubridate) #change date formats
library(ggplot2) #plots
library(ggpubr) #combine plots for publication
library(ggsignif) #significance bars
library(car) #for type-II anovas
library(MASS) #for negative binomial glmms
library(lme4) #for other glmms
library(tidyverse)
library(png) #to read png files
library(grid) #to convert images to ggplot objects


setwd('______/Time Budget Files')

####################################PART 1######################################
#-------------------------------------------------------------------------------
#---------------------------------DATA FRAMES-----------------------------------
#-------------------------------------------------------------------------------
################################################################################
#NOTE: where errors appeared, 
#      all video was double checked before manual correction within the data frame

#import all csv files into one data frame, all as characters
master_df <- list.files(pattern = "\\.csv$") %>%     #pull in all file names as a character vector
  map_df(~ fread(., colClasses = "character")) %>%   #read all CSVs with those names, then map into a data frame
  distinct()                                         #remove duplicate rows (import issue)

#break observation ID into separate components
master_df <- master_df %>%
  mutate(separate_wider_delim(master_df, "Observation id"," ", 
                              names = c("Date", "Primary_Bird", "Video_Num"),
                              cols_remove = FALSE)) %>%          #separate into columns
  relocate(any_of(c("Date", "Primary_Bird", "Video_Num"))) %>%   #move columns to front
  mutate(Date = dmy(Date)) %>%                                   #convert "Date" from character to date
  mutate(Video_Num = gsub("#", "", Video_Num))                   #remove "#" from "Video_Num" string

#replace spaces with _ in column names
master_df <- master_df %>%
  rename_all(~str_replace_all(., "\\s+", "_"))

#rename some columns to get rid of characters like - and () that can disrupt operations
master_df <- master_df %>% 
  rename(Total_duration = `Total_duration_(s)`) %>%
  rename(Duration_mean = `Duration_mean_(s)`) %>%
  rename(inter_event_intervals_mean = `inter-event_intervals_mean_(s)`) %>%
  rename(inter_event_intervals_sd = `inter-event_intervals_std_dev`)

#remove unneeded columns
master_df <- master_df %>%
  dplyr::select(-Observation_id & -Observation_date & -`%_of_total_length`
                & -Description & -Duration_mean & -Duration_std_dev 
                & -inter_event_intervals_mean & -inter_event_intervals_sd 
                & -Time_budget_start & -Time_budget_stop & -Time_budget_duration
                & -Temperature & -Humidity & -Wind & -Weather & -Ground_softness
                & -Ladder_trap & -Primary_location)

#get rid of "double codes" where Subject and Modifier are the same 
#(basic video coding error, all double checked before manual correction)
master_df <- master_df %>%
  mutate(Modifiers = if_else(Modifiers == Subject, "", Modifiers))

#convert certain columns from characters to numeric
master_df <- master_df %>%
  mutate( 
    Total_number_of_occurences = as.numeric(Total_number_of_occurences),
    Total_duration = as.numeric(Total_duration))

#calculate overall behavior totals to double check video coding accuracy
totals <- master_df %>%
  group_by(Subject, Behavior, Modifiers) %>%                #group into subjects, behaviors, & modifiers
  summarise(Duration = sum(Total_duration),                 #total duration
            Occurrences = sum(Total_number_of_occurences),  #total occurrences
            .groups = "drop")                               #ungroup 
rm(totals) #all good --> remove from global environment

#update so codes match (Subject, Behavior, Modifiers)
#correct specific mistakes in video coding (all video double checked before manual correction)
master_df <- master_df %>%
  mutate(Modifiers = dplyr::recode(Modifiers,
                                   #standard missing values from when changed ethogram detail level 
                                   #to include a 3rd modifier (all up until then were "none")
                                   "Auto-|Successful"   = "Auto-|Successful|None",
                                   "Auto-|Unsuccessful" = "Auto-|Unsuccessful|None",
                                   "Auto-|Unknown" = "Auto-|Unknown|None",
                                   "Auto-" = "Auto-|None",
                                   #video coding error (double checked video before manual correction)
                                   "Auto-|Unknown|Broken beak" = "Auto-|Unknown|None"    
  )) %>%
  #find and fix specific video coding errors (double checked video before manual correction)
  mutate(Subject = if_else(             #change Subject value
    Subject == "No focal subject" & 
      Behavior == "Foraging" & 
      Modifiers == "Other crow #1",
    "Other crow #1",                    #new Subject value
    Subject),                           #keep original otherwise
    Modifiers = if_else(                #change Modifier value
      Subject == "Other crow #1" & 
        Behavior == "Foraging" & 
        Modifiers == "Other crow #1",
      "",                               #new Modifiers value
      Modifiers),                       #keep original otherwise
    #if Beg session, always no focal subject
    Subject = if_else(                  #change Subject value
      Behavior == "Begging session" ,
      "No focal subject",               #new Subject value
      Subject))                         #keep original otherwise


#test to ensure all video coding errors were fixed
#filter(Subject == "Broken beak" &  Behavior == "Foraging" & Modifiers == "Other crow #1")
#filter(Modifiers == "Allo-|Unsuccessful|None")


#-------------------------------------------------------------------------------
#Function to calculate the amount of time a subject was visible on video
#-------------------------------------------------------------------------------
#duration of state events *except* unclear video &
#present head, close proximity, within 5m (all 3 can overlap with other state events)
#redistribute begging session b/c no focal subject, but crow modifiers = crow modifiers are visible

make_visible_df <- function(data, group_vars = NULL) {
  #setup
  group_vars <- group_vars %||% character(0)
  group_cols <- c("Subject", group_vars)
  #ensure consistent formats
  data <- data %>%
    mutate(
      Subject = str_squish(as.character(Subject)),
      Behavior = as.character(Behavior),
      Modifiers = as.character(Modifiers))
  #keep only rows relevant for exposure logic
  state_data <- data %>%
    filter(!Behavior %in% c("Present head", "Within 5m", "Close proximity", "Unclear video"))
  
  #calculate base visibility (non-begging state events)
  base <- state_data %>%
    filter(Behavior != "Begging session") %>%
    filter(!is.na(Total_duration)) %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      base_time = sum(Total_duration),
      .groups = "drop")
  
  #begging redistribution
  #include both subject and modifiers as participants
  begging <- state_data %>%
    filter(Behavior == "Begging session") %>%
    #if Modifiers missing, assume only focal subject present
    mutate(Modifiers = if_else(
      is.na(Modifiers) | Modifiers == "",
      Subject, Modifiers)) %>%
    #split multiple participants
    separate_rows(Modifiers, sep = ",") %>%
    mutate(Modifiers = str_squish(Modifiers)) %>%
    #create unified participant set:
    #Subject ∪ Modifiers
    transmute(
      Subject = Modifiers,
      Total_duration,
      across(all_of(group_vars))) %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      begging_time = sum(Total_duration),
      .groups = "drop")
  
  #Combine components
  out <- full_join(base, begging, by = group_cols) %>%
    mutate(
      base_time = coalesce(base_time, 0),
      begging_time = coalesce(begging_time, 0),
      Total_visible = base_time + begging_time) %>%
    dplyr::select(all_of(group_cols), Total_visible)
  return(out)
}

#-------------------------------------------------------------------------------
#add columns with summary data to original data frame (master_df)
#-------------------------------------------------------------------------------
#NOTES:
#state event = has duration
#point event = does not have duration

#total time subject visible across study period
Time_visible <- make_visible_df(master_df)
#per day
day_visible <- make_visible_df(master_df, group_vars = "Date")
#per video
vid_visible <- make_visible_df(master_df, group_vars = c("Date", "Video_Num", "Primary_Bird"))

#join vid_visible into orignal master_df
master_df <- master_df %>%
  left_join(vid_visible, by = c("Date", "Subject", "Video_Num", "Primary_Bird")) %>%
  #change column names to reflect that "total" is actually "per video"
  rename(Vid_occurrences = Total_number_of_occurences) %>%
  rename(Vid_duration = Total_duration) %>%
  rename(Vid_visible = Total_visible)

#join in day_visible
master_df <- master_df %>%
  left_join(day_visible, by = c("Date", "Subject")) %>%
  rename(Day_visible = Total_visible)

#calculate number of occurrences per day
master_df <- master_df %>%   
  group_by(Date, Subject, Behavior, Modifiers) %>%
  mutate(Day_occurrences = sum(Vid_occurrences, na.rm = TRUE)) %>%
  ungroup()

#calculate duration per day for state events
master_df <- master_df %>%
  group_by(Date, Subject, Behavior, Modifiers) %>%
  mutate(Day_duration = if_else(!is.na(Vid_duration),
                                sum(Vid_duration), NA_real_)) %>%
  ungroup()

#point event rate (by video, by day) (occurrence/minute)
master_df <- master_df %>%
  #rate per video
  group_by(Date, Subject, Video_Num) %>%
  mutate(Rate_per_min_vid = if_else(is.na(Vid_duration),
                                    ((Vid_occurrences/Vid_visible)*60), NA_real_)) %>%
  ungroup() %>%
  #rate per day
  group_by(Date, Subject) %>%
  mutate(Rate_per_min_day = if_else(is.na(Day_duration),
                                    ((Day_occurrences/Day_visible)*60), NA_real_)) %>%
  ungroup()  

#state event % of total time visible (by video, by day), exclude if no focal subject
master_df <- master_df %>%
  #percent per video
  group_by(Date, Subject, Video_Num) %>%
  mutate(Percent_time_visible_vid = if_else(!is.na(Vid_duration) &
                                              (Subject != "No focal subject"),
                                            ((Vid_duration/Vid_visible)*100), NA_real_)) %>%
  ungroup() %>%
  #percent per day
  group_by(Date, Subject) %>%
  mutate(Percent_time_visible_day = if_else(!is.na(Day_duration) & 
                                              (Subject != "No focal subject"), 
                                            ((Day_duration/Day_visible)*100), NA_real_)) %>% 
  ungroup()

#-------------------------------------------------------------------------------
#master_df complete --> export for future use
write.csv(master_df,"______/Master DF.csv", 
          row.names = FALSE)

#-------------------------------------------------------------------------------
#Collapse master_df to video- and day-level per subject --> collapsed_df
#-------------------------------------------------------------------------------

#helper function: sum but keep NA if all values are NA
sum_na <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)}

collapsed_df <- master_df %>%
  #add Group column
  mutate(Group = case_when(
    Subject == "Broken beak" ~ "Broken beak",
    Subject == "No focal subject" ~ "No focal",
    TRUE ~ "Other crows")) %>%
  
  #video-level collapse (avoid duplication of Vid_visible)
  group_by(Date, Video_Num, Subject, Group, Behavior, Modifiers) %>%
  summarise(
    Vid_occurrences = sum_na(Vid_occurrences),
    Vid_duration    = sum_na(Vid_duration),
    Vid_visible     = first(Vid_visible), #so don't duplicate
    .groups = "drop") %>%
  
  #day-level collapse
  group_by(Date, Subject, Group, Behavior, Modifiers) %>%
  summarise(
    Day_occurrences = sum_na(Vid_occurrences),
    Day_duration    = sum_na(Vid_duration),
    .groups = "drop") %>%
  
  #correctly calculate amount of time a subject is visible per day
  left_join(master_df %>%
              mutate(Group = case_when(
                Subject == "Broken beak" ~ "Broken beak",
                Subject == "No focal subject" ~ "No focal",
                TRUE ~ "Other crows")) %>%
              distinct(Date, Subject, Video_Num, Vid_visible) %>%
              group_by(Date, Subject) %>%
              summarise(Day_visible = sum(Vid_visible, na.rm = TRUE),
                        .groups = "drop"), by = c("Date", "Subject")) %>%
  
  #add in percent of time visible for state events
  mutate(Percent_time_visible = if_else(
    !is.na(Day_duration) & !is.na(Day_visible) & Day_visible > 0,
    Day_duration / Day_visible,
    NA_real_))

#investigate summary stats per group/behavior
#time each group is visible on video
time_vis_group <- collapsed_df %>%
  distinct(Date, Subject, Group, Day_visible) %>%                  #prevent inflation
  group_by(Group) %>%
  summarise(Total_visible = sum(Day_visible, na.rm = TRUE)/60/60,  #in hours
            .groups = "drop")

#-------------------------------------------------------------------------------
#collapsed_df complete --> export for future use
write.csv(collapsed_df,"______/Collapsed DF.csv", row.names = FALSE)
#-------------------------------------------------------------------------------

#remove unneeded objects from global environment
rm(day_visible, time_vis_group, Time_visible, vid_visible, make_visible_df, sum_na)



##################################PARTS 2 & 3###################################
#-------------------------------------------------------------------------------
#-------------------------------MODELS AND PLOTS--------------------------------
#-------------------------------------------------------------------------------
################################################################################

##General format/method 
#1. Make sub-data frame (if needed)
#2. Fit GLM
#3. Check model for overdispersion, adjust model type if needed
#4. Compare AICs using drop1(), reduce models
#5. Check for correlation (if needed)
#6. Get summary of GLM results
#7. Anova to test for significance
#8. Plot (if needed)

#helper function to check models for overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

#-------------------------------------------------------------------------------
#--------------------------Autofeeding and Foraging-----------------------------
#-------------------------------------------------------------------------------

#---------------------------------------------
#Model 3: Autofeeding success 
#---------------------------------------------
#1. Make sub-data frame
#retain subject, don't pool other crows
collapsed_total_autofeeds_by_day <- collapsed_df %>%
  
  #keep only autofeeding events
  filter(Behavior == "Feed") %>%
  filter(grepl("Auto-", Modifiers)) %>%
  
  #extract outcome
  mutate(
    Outcome = str_split(Modifiers, "\\|") %>%
      map_chr(~ .x[2])) %>%
  
  #keep relevant columns
  dplyr::select(Date, Subject, Group, Outcome, Day_occurrences) %>%
  
  #sum per day per subject per outcome
  group_by(Date, Subject, Group, Outcome) %>%
  summarise(
    Occurrences = sum(Day_occurrences, na.rm = TRUE),
    .groups = "drop") %>%
  
  #wide format: Successful / Unsuccessful
  pivot_wider(
    names_from = Outcome,
    values_from = Occurrences,
    values_fill = 0) %>%
  
  #compute proportion
  mutate(Percent_Successful = Successful / (Successful + Unsuccessful)) %>%
  
  #remove invalid rows ( NaN and Inf values b/c either no suc or no unsuc in a day)
  filter(is.finite(Percent_Successful))

#2. Model
#binomial glm
autofeed_success <- glm(cbind(Successful, Unsuccessful) ~ Group*Date,
                        data = collapsed_total_autofeeds_by_day,
                        family = binomial)

#3. Check for overdispersion
overdisp_fun(autofeed_success) #overdispersed (ratio 1.419136e+01)
autofeed_success <- glm(cbind(Successful, Unsuccessful) ~ Group*Date,
                        data = collapsed_total_autofeeds_by_day,
                        family = quasibinomial)

#4. Reduce using drop1()
drop1(autofeed_success, test = "F") #Group:Date not significant (p = 0.5162)
autofeed_success <- glm(cbind(Successful, Unsuccessful) ~ Group + Date,
                        data = collapsed_total_autofeeds_by_day,
                        family = quasibinomial)
drop1(autofeed_success, test = "F") #both are significant, so keep

#5. Check for correlation - n/a

#6. Summary
summary(autofeed_success)

#7. Anova
Anova(autofeed_success, type = "II", test = "F")

#8. Plot (Fig 1C)
#for significance bars
#extract p-value
p_val <- summary(autofeed_success)$coefficients["GroupOther crows", "Pr(>|t|)"]
#convert to stars
stars <- ifelse(p_val < 0.001, "***",
                ifelse(p_val < 0.01, "**",
                       ifelse(p_val < 0.05, "*", "ns")))
#dynamic y position
y_max <- max(collapsed_total_autofeeds_by_day$Percent_Successful, na.rm = TRUE)
y_bracket <- y_max * 1.08
y_text <- y_max * 1.12

#Plot: violin wrapping a boxplot
autofeed_perc_successful <- collapsed_total_autofeeds_by_day %>%
  mutate(Group = dplyr::recode(Group,
                               "Broken beak" = "DC",
                               "Other crows" = "HCs")) %>%
  ggplot(aes(x=Group, y=Percent_Successful, fill=Group)) +
  labs(y="Successful Autofeed Attempts (%)") +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  scale_fill_manual(values = c(
    "DC" = "steelblue2",
    "HCs" = "indianred2"))+
  theme_classic() + #remove gray background
  theme(legend.position="none", #remove legend
        axis.text.x = element_text(size = 13)) + 
  xlab("") + #remove x-axis label ("Group")
  scale_y_continuous( #change y-axis into percents instead of decimals
    breaks = seq(0, 1, by = 0.2),
    limits = c(0, 1.15),
    labels = scales::percent) +
  #add significance bars (manually)
  #bracket (left vertical)
  annotate("segment", x = 1, xend = 1, 
           y = y_bracket * 0.98, yend = y_bracket) +
  # bracket (right vertical)
  annotate("segment", x = 2, xend = 2, 
           y = y_bracket * 0.98, yend = y_bracket) +
  # bracket (top horizontal)
  annotate("segment", x = 1, xend = 2, 
           y = y_bracket, yend = y_bracket) +
  # significance stars
  annotate("text", x = 1.5, y = y_text, 
           label = stars, size = 6)

#---------------------------------------------
#Model 1: Percent time foraging
#---------------------------------------------
#1. Make sub-data frame
#pool other crows into one group
foraging <- collapsed_df %>% 
  filter(Group != "No focal subject" & Behavior == "Foraging") %>%
  group_by(Date, Group) %>%
  summarise(
    Day_duration = sum(Day_duration),
    Day_visible  = sum(Day_visible),
    .groups = "drop") %>%
  mutate(Percent_foraging = Day_duration / Day_visible)

#2. Model
percent_time_foraging <- glm(Percent_foraging ~ Group*Date, data = foraging, 
                             weights = Day_visible, # treat Day_visible as "trials"
                             family = binomial)

#3. Check for overdispersion
overdisp_fun(percent_time_foraging) #overdispersed (ratio = 223.3369)
percent_time_foraging <- glm(Percent_foraging ~ Group*Date, data = foraging, 
                             weights = Day_visible, family = quasibinomial)

#4. Reduce using drop1()
drop1(percent_time_foraging, test = "F") #Group:Date not significant (p=0.4717)
percent_time_foraging <- glm(Percent_foraging ~ Group+Date, data = foraging, 
                             weights = Day_visible, family = quasibinomial)
drop1(percent_time_foraging, test = "F") #neither significant

#5. Check for correlation - n/a

#6. Summary
summary(percent_time_foraging)

#7. Anova
Anova(percent_time_foraging, type = "II", test = "F")

#8. Plot - n/a

#---------------------------------------------
#Model 2: Autofeeding attempt rate per minute 
#---------------------------------------------
#1. Make sub-data frame
#pool other crows into 1 group

All_autofeeds <- collapsed_df %>%
  filter(Behavior == "Feed" & grepl("Auto-", Modifiers)) %>%
  
  # Collapse multiple rows per Subject/Date (different Modifiers)
  group_by(Date, Subject, Group) %>%
  summarise(
    Day_auto_tries = sum(Day_occurrences, na.rm = TRUE),  #sum attempts across Modifiers
    Day_visible    = first(Day_visible),                  #take Day_visible once per subject
    .groups = "drop") %>%
  
  #Pool subjects per Group per Date
  group_by(Date, Group) %>%
  summarise(
    Day_auto_tries = sum(Day_auto_tries),  #sum attempts across subjects
    Day_visible    = sum(Day_visible),     #sum visible time across subjects
    .groups = "drop") %>%
  
  #Compute rate per minute (regardless of outcome)
  mutate(Rate_per_min_day = (Day_auto_tries / Day_visible) * 60)

#2. Model
autofeed_rate <- glm(Day_auto_tries ~ Group * Date + offset(log(Day_visible)),
                     data = All_autofeeds, family = poisson)

#3. Check for overdispersion
overdisp_fun(autofeed_rate) #overdispersed (ratio = 48.76534)
autofeed_rate <- glm(Day_auto_tries ~ Group*Date + offset(log(Day_visible)),
                     data = All_autofeeds, family = quasipoisson)

#4. Reduce using drop1()
drop1(autofeed_rate, test="Chisq") #Group:Date not significant (p=0.2251)
autofeed_rate <- glm(Day_auto_tries ~ Group + Date + offset(log(Day_visible)),
                     data = All_autofeeds, family = quasipoisson)
drop1(autofeed_rate, test="Chisq") #both significant

#5. Check for correlation - n/a

#6. Summary
summary(autofeed_rate)

#7. Anova
Anova(autofeed_rate, type = "II", test = "F")

#8. Plot (Fig 1B)
#p-value --> stars
p_val <- summary(autofeed_rate)$coefficients["GroupOther crows", "Pr(>|t|)"]

stars <- case_when(
  p_val < 0.001 ~ "***",
  p_val < 0.01  ~ "**",
  p_val < 0.05  ~ "*",
  TRUE          ~ "ns")

#dynamic y positions (based on rate variable)
y_max <- max(All_autofeeds$Rate_per_min_day, na.rm = TRUE)
y_bracket <- y_max * 1.08
y_text <- y_max * 1.12

#Plot: violin wrapping a boxplot
autofeed_attempt_rate <- All_autofeeds %>%
  mutate(Group = dplyr::recode(Group,
                               "Broken beak" = "DC",
                               "Other crows" = "HCs")) %>%
  ggplot(aes(x = Group, y = Rate_per_min_day, fill = Group)) +
  labs(y = "Autofeed Attempts Per Minute") +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.2) +
  scale_fill_manual(values = c(
    "DC" = "steelblue2",
    "HCs" = "indianred2"))+
  theme_classic() + #remove gray background
  theme(legend.position="none", #remove legend
        axis.text.x = element_text(size = 13)) + 
  xlab("") +
  
  #significance annotation
  annotate("segment", x = 1, xend = 1, 
           y = y_bracket * 0.98, yend = y_bracket) +
  annotate("segment", x = 2, xend = 2, 
           y = y_bracket * 0.98, yend = y_bracket) +
  annotate("segment", x = 1, xend = 2, 
           y = y_bracket, yend = y_bracket) +
  annotate("text", x = 1.5, y = y_text, label = stars, size = 6) + 
  theme(plot.margin = margin(t = 10, r = 5, b = 5, l = 5))


#-------------------------------------------------------------------------------
#--------------------------------Autopreening-----------------------------------
#-------------------------------------------------------------------------------

#---------------------------------------------
#Model 5: Percent time autopreening 
#---------------------------------------------
#1. Make sub-data frame
#pool other crows into one group
autopreening <- collapsed_df %>% 
  filter(Group != "No focal" & Behavior == "Preen" & grepl("Auto-", Modifiers)) %>%
  group_by(Date, Group) %>%
  summarise(
    Day_duration = sum(Day_duration),
    Day_visible  = sum(Day_visible),
    .groups = "drop") %>%
  mutate(Percent_autopreening = Day_duration / Day_visible)

#2. Model
percent_time_autopreening <- glm(Percent_autopreening ~ Group, data = autopreening, 
                                 weights = Day_visible, #treat Day_visible as "trials"
                                 family = binomial)

#3. Check for overdispersion
overdisp_fun(percent_time_autopreening) #overdispersed (ratio = 100.8779)
percent_time_autopreening <- glm(Percent_autopreening ~ Group, data = autopreening, 
                                 weights = Day_visible, family = quasibinomial)

#4. Reduce using drop1() - n/a

#5. Check for correlation - n/a

#6. Summary
summary(percent_time_autopreening)

#7. Anova
Anova(percent_time_autopreening, type = "II", test = "F") 

#8. Plot - n/a

#-------------------------------------------------------------------------------
#----------------------Solicitation Behavior (Begging)--------------------------
#-------------------------------------------------------------------------------
#NOTE: in this code, solicitation behavior is called "begging"
#-----------------------
#1. Make sub-data frames
#-----------------------
#dataframe of solicitation (beg) sessions
beg_session <- collapsed_df %>% 
  #keep beg sessions, allofeeds, and allopreens
  filter(
    Behavior %in% c(
      "Begging session",
      "Feed",
      "Preen"),
    Behavior != "Feed" | grepl("Allo-", Modifiers),
    Behavior != "Preen" | grepl("Allo-", Modifiers)
  ) %>%
  #split out allofeed outcome
  mutate(
    Outcome = if_else(
      Behavior == "Feed",
      str_split(Modifiers, "\\|") %>% map_chr(~ .x[2]), #select 2nd element
      NA_character_)) %>% #non-feed rows, return NA
  #rename "Feed" to "Allofeed" and "Preen" to "Allopreen"
  mutate(Behavior = if_else(Behavior == "Feed",
                            "Allofeed",Behavior)) %>%
  mutate(Behavior = if_else(Behavior == "Preen",
                            "Allopreen",Behavior)) %>%
  dplyr::select(Date, Behavior, Outcome, Day_occurrences) %>%
  group_by(Behavior, Date, Outcome) %>%
  summarise(Day_occurrences = sum(Day_occurrences),
            .groups = "drop")


#summarize sessions and outcomes by date
beg_summary <- beg_session %>%
  #names of new columns
  mutate(Behavior_outcome = case_when(
    Behavior == "Allofeed" & Outcome == "Successful"   ~ "Allofeed_S",
    Behavior == "Allofeed" & Outcome == "Unsuccessful" ~ "Allofeed_US",
    Behavior == "Allofeed" & Outcome == "Unknown"      ~ "Allofeed_UK",
    Behavior == "Begging session"                      ~ "Begging_sessions",
    Behavior == "Allopreen"                            ~ "Allopreen"
  )) %>%
  #guarantee column order
  mutate(Behavior_outcome = factor( 
    Behavior_outcome,
    levels = c("Begging_sessions", "Allofeed_S", "Allofeed_US", "Allofeed_UK", "Allopreen"))) %>%
  group_by(Date, Behavior_outcome) %>%
  #sum and pivot out
  summarise(total = sum(Day_occurrences), .groups = "drop") %>%
  pivot_wider(
    names_from = Behavior_outcome,
    values_from = total,
    values_fill = 0)

#took this df out and cross-referenced with field data to fill in 
#any gaps where a solicitation session occurred off camera (see methods)
combined_beg_summary <- 
  read.csv("______/Combined Beg Session Outcomes.csv", 
           fileEncoding="UTF-8-BOM", header = T, stringsAsFactors = F)

combined_beg_summary <- combined_beg_summary %>%
  mutate(Date = mdy(Date)) %>% #convert "Date" from character to date
  #calculate total AF attempts
  group_by(Date) %>%
  mutate(Total_AF_attempts = sum(Allofeed_S, Allofeed_US, Allofeed_UK)) %>%
  #calculate total allo- behavior of any kind
  mutate(Total_allo = sum(Allofeed_S, Allofeed_US, Allofeed_UK, Allopreen)) %>%
  ungroup %>%
  #calculate % of sessions with an AF attempt
  mutate(Session_AF_attempt = (Total_AF_attempts/Begging_sessions)*100) %>%
  #calculate AF success (confirmed)
  mutate(Session_AF_S = (Allofeed_S/Begging_sessions)*100)

#only dates with allofeed (AF) attempts
combined_beg_summary_AF <- combined_beg_summary %>%
  filter(Total_AF_attempts != 0)

#download session dates & lengths
session_dates <- 
  read.csv("______/Session Dates.csv", 
           fileEncoding="UTF-8-BOM", header = T, stringsAsFactors = F)
session_dates <- session_dates %>% mutate(Date = mdy(Date))


#merge in session dates
all_sessions_beg <- session_dates %>% 
  left_join(combined_beg_summary) %>% 
  mutate(across(
    c(Begging_sessions, Allofeed_S, Allofeed_US, Allofeed_UK, Allopreen,
      Total_AF_attempts, Total_allo),
    ~ coalesce(.x, 0))) #replace NAs with 0

#add in group (which allo- behavior offered)
all_sessions_beg <- all_sessions_beg %>%
  mutate(Group = 
           ifelse(Allopreen == 0 & Total_AF_attempts != 0, "Allofeed",
                  ifelse(Allopreen != 0 & Total_AF_attempts == 0, "Allopreen",
                         ifelse(Allopreen !=0 & Total_AF_attempts !=0, "Both", "Neither"))))

#remove dates with just allopreens
all_sessions_beg_AF <- all_sessions_beg %>%
  filter(!Date %in% as.Date(c("2025-07-17", "2025-07-22", "2025-08-29")))

#add in covariates of session length and total video length#
#need total video length by date
#import all time budget csv files into one df, all as characters (like earlier master_df creation)
df <- list.files(pattern = "\\.csv$") %>% #pull in all file names as a character vector
  map_df(~ fread(., colClasses = "character")) %>% #read all CSVs with those names, then map into a df
  distinct() #remove duplicate rows

#break observation ID into separate components
df <- df %>%
  mutate(separate_wider_delim(df, 'Observation id'," ", 
                              names = c("Date", "Primary_Bird", "Video_Num"),
                              cols_remove = FALSE)) %>% #separate into col
  relocate(any_of(c("Date", "Primary_Bird", "Video_Num"))) %>% #move col to front
  mutate(Date = dmy(Date)) %>% #convert "Date" from character to date
  mutate(Video_Num = gsub("#", "", Video_Num))  #remove "#" from "Video_Num" string

#replace spaces with _ in column names
df <- df %>%
  rename_all(~str_replace_all(., "\\s+", "_"))

#calculate total video length (time budget duration minus unclear video)
vid_dur <- df %>%
  dplyr::select(Date, Observation_id, Time_budget_duration) %>%
  distinct() %>%
  group_by(Date) %>%
  summarise(Video_duration = sum(as.numeric(Time_budget_duration)))

vid_unclear <- df %>%
  filter(Behavior == "Unclear video") %>%
  dplyr::select(Date, Observation_id, `Total_duration_(s)`) %>%
  group_by(Date) %>%
  summarise(Video_unclear = sum(as.numeric(`Total_duration_(s)`)))

vid_dur <- vid_dur %>%
  left_join(vid_unclear, by = "Date") %>%
  mutate(Video_duration = Video_duration - Video_unclear) %>%
  dplyr::select(-Video_unclear)

#merge into all_sessions_beg
all_sessions_beg <- all_sessions_beg %>%
  left_join(vid_dur, by="Date") %>%
  #move some columns for better visualization
  relocate(Video_duration, .after=Session_length) %>%
  relocate(Group, .after=Date) %>%
  mutate(Video_duration = ifelse(is.na(Video_duration), 0, Video_duration))

#remake all_sessions_beg_AF - fully remove allopreen-only dates 
all_sessions_beg_AF <- all_sessions_beg %>%
  filter(!Date %in% as.Date(c("2025-07-17", "2025-07-22", "2025-08-29")))

#and to only look at allofeeds (allopreen-only dates are changed to zero)
all_sessions_beg_AF <- all_sessions_beg %>%
  mutate(Begging_sessions = ifelse(Group == "Allopreen", 0, Begging_sessions)) %>%
  #recalculate some columns
  #calculate % of sessions with an AF attempt
  mutate(Session_AF_attempt = (Total_AF_attempts/Begging_sessions)*100) %>%
  #calculate AF success (confirmed)
  mutate(Session_AF_S = (Allofeed_S/Begging_sessions)*100)


#---------------------------------------------
#Model 4: Food-solicitation over time
#---------------------------------------------

#2. Model
#neg binomial - compare linear and quadratic
nb_beg <- glm.nb(Begging_sessions ~ Date + Session_length + Video_duration, 
                 data = all_sessions_beg_AF)
#scale data for quadratic fit
sc_all_sessions_beg_AF <- all_sessions_beg_AF %>%
  mutate(
    Date_num = as.numeric(Date),
    Date_sc = as.numeric(scale(Date_num)),
    Session_length_sc = as.numeric(scale(Session_length)),
    Video_duration_sc = as.numeric(scale(Video_duration)))

nb_beg_poly2 <- glm.nb(
  Begging_sessions ~ poly(Date_sc, 2) + Session_length_sc + Video_duration_sc,
  data = sc_all_sessions_beg_AF)

#compare AICs - nb_beg_poly2 has lower AIC
AIC(nb_beg, nb_beg_poly2)

#rename model (nb_beg_poly2) for final stats/plots
nb_model <- glm.nb(
  Begging_sessions ~ poly(Date_sc, 2) + Session_length_sc + Video_duration_sc,
  data = sc_all_sessions_beg_AF)


#3. Check for overdispersion - n/a

#4. Reduce using drop1() - n/a

#5. Check for correlation (session length and video duration)
cor(sc_all_sessions_beg_AF$Session_length, sc_all_sessions_beg_AF$Video_duration, 
    use = "complete.obs")
#not correlated (Pearson = 0.190261)

#6. Summary
summary(nb_model)

#7. Anova
Anova(nb_model, type = "II", test = "LR")

#8. Plot
#plot all beg sessions over time (regardless of behavior offered)
#make points with group neither always in the back if shapes overlap
all_sessions_beg <- all_sessions_beg %>%
  arrange(Group != "Neither") 

behavior_offered <- 
  ggplot(all_sessions_beg) + 
  aes(Date, y = Begging_sessions, shape = Group, col = Group) +
  geom_point(size = 2, stroke = 1.2) +
  labs(
    x = "Date",
    y = "Solicitation Sessions",
    col = "Behavior Offered",
    shape = "Behavior Offered"
  ) +
  scale_color_manual(values = c(
    "Allofeed" = "steelblue2",
    "Allopreen" = "indianred2",
    "Both" = "mediumorchid3",
    "Neither" = "gray"
  )) +
  theme_classic() +
  theme(
    legend.position = c(0.98, 0.98),      # inside top-right
    legend.justification = c(1, 1),
    legend.background = element_rect(
      colour = "black",
      fill = "white",
      linewidth = 0.4
    ),
    legend.key.size = unit(0.35, "cm"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.spacing.y = unit(0.05, "cm")
  )

#to fit neg binomial regression
#Create prediction dataset (hold covariates at mean = 0 because scaled)
newdat <- data.frame(
  Date_sc = seq(
    min(sc_all_sessions_beg_AF$Date_sc, na.rm = TRUE),
    max(sc_all_sessions_beg_AF$Date_sc, na.rm = TRUE),
    length.out = 100),
  Session_length_sc = 0,
  Video_duration_sc = 0)

#Predict with SE (link scale)
pred <- predict(nb_model, newdata = newdat, type = "link", se.fit = TRUE)

newdat$fit <- exp(pred$fit)
newdat$lwr <- exp(pred$fit - 1.96 * pred$se.fit)
newdat$upr <- exp(pred$fit + 1.96 * pred$se.fit)


#Convert Date back to original scale
date_center <- mean(sc_all_sessions_beg_AF$Date_num, na.rm = TRUE)
date_scale  <- sd(sc_all_sessions_beg_AF$Date_num, na.rm = TRUE)

newdat$Date_num <- newdat$Date_sc * date_scale + date_center
newdat$Date <- as.Date(newdat$Date_num, origin = "1970-01-01")

#Plot food-solicitation sessions over time (neg binomial regression)
#(excluding NA session length rows)
food_beg <- ggplot(
  sc_all_sessions_beg_AF %>% filter(!is.na(Session_length)),
  aes(x = Date, y = Begging_sessions)) +
  geom_point(color = "steelblue2") +
  geom_line(data = newdat,
            aes(x = Date, y = fit),
            color = "steelblue2",
            linewidth = 1) +
  geom_ribbon(data = newdat,
              aes(x = Date, ymin = lwr, ymax = upr),
              fill = "steelblue2",
              alpha = 0.2,
              inherit.aes = FALSE) +
  labs(x = "Date", y = "Food-Solicitation Sessions") +
  #cut off y axis at 15 without changing geom ribbon (like would if used ylim)
  coord_cartesian(ylim = c(0, 15)) +
  theme_classic() #remove gray background



#####################################PART 4#####################################
#-------------------------------------------------------------------------------
#--------------------------MAKE COMBINED FIGURE---------------------------------
#-------------------------------------------------------------------------------
################################################################################
#(Fig 1. Feeding and solicitation behavior)

library(png) #to read png files
library(grid) #to convert images to ggplot objects

#import images
#food-solicitation and allofeeding illustration
AF_pic <- readPNG("______.png")
#preen-solicitation and allopreening illustration
AP_pic <- readPNG("______.png")
#pictures of the crows
Crow_heads <- readPNG("______.png")


#convert images to ggplot objects
AF_plot <- as_ggplot(rasterGrob(AF_pic, interpolate = TRUE))
AP_plot <- as_ggplot(rasterGrob(AP_pic, interpolate = TRUE))
Crow_heads <- as_ggplot(rasterGrob(Crow_heads, interpolate = TRUE))

#compile the full figure 
#Row 1: (A) Crow_heads
#Row 2: (B) autofeed_attempt_rate, (C) autofeed_perc_successful
#Row 3: (D) AF_plot, (E) AP_plot
#Row 4: (F) behavior_offered, (G) food_beg
#Rows 1 & 3 (images) shorter; Rows 2 & 4 (plots) full height

combined_figure <- ggarrange(
  #Row 1: Panel A (shorter)
  ggarrange(
    Crow_heads + theme(
      plot.margin = margin(20, 5, 20, 5),
      plot.background = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.8)
    ),
    ncol = 1,
    labels = "A",
    font.label = list(size = 16, face = "bold")
  ),
  
  #Row 2: Panels B and C
  ggarrange(
    autofeed_attempt_rate + theme(
      plot.margin = margin(20, 5, 0, 25),
      plot.background = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.8)
    ),
    autofeed_perc_successful + theme(
      plot.margin = margin(20, 5, 0, 25),
      plot.background = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.8)
    ),
    ncol = 2,
    labels = c("B", "C"),
    font.label = list(size = 16, face = "bold")
  ),
  
  #Row 3: Panels D and E (even shorter than Row 1)
  ggarrange(
    AF_plot + theme(
      plot.margin = margin(20, 5, 5, 25),
      plot.background = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.8)
    ),
    AP_plot + theme(
      plot.margin = margin(20, 5, 5, 25),
      plot.background = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.8)
    ),
    ncol = 2,
    labels = c("D", "E"),
    font.label = list(size = 16, face = "bold")
  ),
  
  #Row 4: Panels F and G (same height as Row 2)
  ggarrange(
    behavior_offered + theme(
      plot.margin = margin(20, 5, 10, 25),
      plot.background = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.8)
    ),
    food_beg + theme(
      plot.margin = margin(20, 5, 10, 25),
      plot.background = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.8)
    ),
    ncol = 2,
    labels = c("F", "G"),
    font.label = list(size = 16, face = "bold")
  ),
  
  ncol = 1,
  heights = c(0.7, 1, 0.4, 1)   #Rows 1 & 3 shorter; Rows 2 & 4 full height
)

#export as tiff file
ggsave(file.path("_________", "______.tiff"), #location and file name
       plot = combined_figure, bg = "white", width = 18, height = 22, units = "cm")


library(tidyverse)
library(data.table)

getwd()
setwd("/Users/eleonorakirkizh/ownCloud/Nora_Seb/Conjoint_experiment")

# load that messy data first
pilot_dt <- read.csv(file = "data_social_survey_2019-10-21_16-57.csv", 
                     header = TRUE, sep=";", fileEncoding="latin1", 
                     blank.lines.skip = TRUE, na.strings="",
                     stringsAsFactors=FALSE, skipNul = TRUE, fill=T, quote = "\"")


names(pilot_dt)
sum(is.na(pilot_dt$IV01_01)) # number of NAs in the column with attributes and levels 

# with no missings in IV01_01
valid_dt <- pilot_dt[!is.na(pilot_dt$IV01_01),]

# The following results do not include Speeders, where Speeders are defined 
# as all respondents who finished the survey in less than half of the median time.
# from an Appendix from Bansak et al. 2016

table(valid_dt$TIME_SUM)
median.time <- median(valid_dt$TIME_SUM) # mean time for Pilot 1 is 160.5, for Pilot 2: 238
speeders <- sum((valid_dt$TIME_SUM < median.time/2) == TRUE) # any speeders?

# without speeders (it's needed if new data come up)
valid_dt <- subset(valid_dt, valid_dt$TIME_SUM > median.time/2)
nrow(valid_dt) # number of valid case for the analysis, 
               # which is 28 in Pilot 1, and 17 in Pilot 2.

table(valid_dt$REF) # one respondent accessed the survey twice in Pilot 1. 
# To take his first entry would be safer:
valid_dt <- valid_dt[-1,] # admin test
valid_dt <- valid_dt[-27,]

# Let's try to parse IV01_01, the output of randomozation
table(valid_dt$IV01_01)
valid_dt$IV01_01new1 <- gsub("}", "", paste(valid_dt$IV01_01)) # remove "{" from values
valid_dt$Attrb.Levels <- gsub("\\{", "", paste(valid_dt$IV01_01new1)) # remove "}"
valid_dt$IV01_01new1 <- NULL
table(valid_dt$REF)

mess <- data.frame(strsplit(valid_dt$Attrb.Levels, ",")) 
ncol(mess)
names(mess) <- c(valid_dt$REF) # rename columns with corresponding respondent id


########################################################################################
# Create a data frame with all valid cases from the conjoint experiment for the analysis
########################################################################################

# Reshaping wide format df to long format df
not.mess <- mess %>% gather(Panelist_ID, Levels, 1:17)

# separate keys from attrb and levels
not.mess <- not.mess %>% 
  separate(Levels, c("key","value"), ":", convert = TRUE) %>% 
  data.frame

# remove quotes from factor levels
table(not.mess$key)
not.mess$key <- as.factor(not.mess$key)
str(not.mess$key)
levels(not.mess$key) <- gsub('["\\]', "", levels(not.mess$key))

# For the first Attribute in every task, order1: if F-1-1, F-2-1, F-3-1, F-4-1, F-4-1, F-5-1, 
# F-6-1, F-7-1, F-8-1. F-9-1. F-10-1 then add to order1 and rep() by panelist_id.

order1 = c("F-1-1", "F-2-1", "F-3-1", "F-4-1", "F-5-1", "F-6-1", "F-7-1", "F-8-1",
           "F-9-1", "F-10-1")

order2 = c("F-1-2", "F-2-2", "F-3-2", "F-4-2", "F-5-2", "F-6-2", "F-7-2", "F-8-2",
           "F-9-2", "F-10-2")

order3 = c("F-1-3", "F-2-3", "F-3-3", "F-4-3", "F-5-3", "F-6-3", "F-6-3", "F-8-3",
           "F-9-3", "F-10-3")

order4 = c("F-1-4", "F-2-4", "F-3-4", "F-4-4", "F-5-4", "F-6-4", "F-7-4", "F-8-4",
           "F-9-4", "F-10-4")

order5 = c("F-1-5", "F-2-5", "F-3-5", "F-4-5", "F-5-5", "F-6-5", "F-7-5", "F-8-5",
           "F-9-5", "F-10-5")

not.mess$Order = ifelse(not.mess$key %in% order1, "1", 
                    ifelse(not.mess$key %in% order2, "2",
                           ifelse(not.mess$key %in% order3, "3",
                                  ifelse(not.mess$key %in% order4, "4",
                                         ifelse(not.mess$key %in% order5, "5",
                                                NA)))))


# create a var for Task.n: F-1(task)-1(profile)-1(attribute)

task1 = c("F-1-1-1", "F-1-1-2", "F-1-1-3", "F-1-1-4", "F-1-1-5", 
          "F-1-2-1", "F-1-2-2", "F-1-2-3", "F-1-2-4", "F-1-2-5")

task2 = c("F-2-1-1", "F-2-1-2", "F-2-1-3", "F-2-1-4", "F-2-1-5", 
          "F-2-2-1", "F-2-2-2", "F-2-2-3", "F-2-2-4", "F-2-2-5")

task3 = c("F-3-1-1", "F-3-1-2", "F-3-1-3", "F-3-1-4", "F-3-1-5", 
          "F-3-2-1", "F-3-2-2", "F-3-2-3", "F-3-2-4", "F-3-2-5")

task4 = c("F-4-1-1", "F-4-1-2", "F-4-1-3", "F-4-1-4", "F-4-1-5", 
          "F-4-2-1", "F-4-2-2", "F-4-2-3", "F-4-2-4", "F-4-2-5")

task5 = c("F-5-1-1", "F-5-1-2", "F-5-1-3", "F-5-1-4", "F-5-1-5", 
          "F-5-2-1", "F-5-2-2", "F-5-2-3", "F-5-2-4", "F-5-2-5")

task6 = c("F-6-1-1", "F-6-1-2", "F-6-1-3", "F-6-1-4", "F-6-1-5", 
          "F-6-2-1", "F-6-2-2", "F-6-2-3", "F-6-2-4", "F-6-2-5")

task7 = c("F-7-1-1", "F-7-1-2", "F-7-1-3", "F-7-1-4", "F-7-1-5", 
          "F-7-2-1", "F-7-2-2", "F-7-2-3", "F-7-2-4", "F-7-2-5")

task8 = c("F-8-1-1", "F-8-1-2", "F-8-1-3", "F-8-1-4", "F-8-1-5", 
          "F-8-2-1", "F-8-2-2", "F-8-2-3", "F-8-2-4", "F-8-2-5")

task9 = c("F-9-1-1", "F-9-1-2", "F-9-1-3", "F-9-1-4", "F-9-1-5", 
          "F-9-2-1", "F-9-2-2", "F-9-2-3", "F-9-2-4", "F-9-2-5")

task10 = c("F-10-1-1", "F-10-1-2", "F-10-1-3", "F-10-1-4", "F-10-1-5", 
           "F-10-2-1", "F-10-2-2", "F-10-2-3", "F-10-2-4", "F-10-2-5")


not.mess$Task.n = ifelse(not.mess$key %in% task1, "1", 
              ifelse(not.mess$key %in% task2, "2",
              ifelse(not.mess$key %in% task3, "3",
              ifelse(not.mess$key %in% task4, "4",
              ifelse(not.mess$key %in% task5, "5",
              ifelse(not.mess$key %in% task6, "6",
              ifelse(not.mess$key %in% task7, "7",
              ifelse(not.mess$key %in% task8, "8",
              ifelse(not.mess$key %in% task9, "9",
              ifelse(not.mess$key %in% task10, "10", NA))))))))))


# create a var for profile.n in a task: F-1(task)-1(profile)-1(attribute)

profile1 = c("F-1-1-1", "F-1-1-2", "F-1-1-3", "F-1-1-4", "F-1-1-5", # from task 1
             "F-2-1-1", "F-2-1-2", "F-2-1-3", "F-2-1-4", "F-2-1-5", # from task 2
             "F-3-1-1", "F-3-1-2", "F-3-1-3", "F-3-1-4", "F-3-1-5", # from task 3
             "F-4-1-1", "F-4-1-2", "F-4-1-3", "F-4-1-4", "F-4-1-5", # from task 4
             "F-5-1-1", "F-5-1-2", "F-5-1-3", "F-5-1-4", "F-5-1-5", # from task 5
             "F-6-1-1", "F-6-1-2", "F-6-1-3", "F-6-1-4", "F-6-1-5", # from task 6
             "F-7-1-1", "F-7-1-2", "F-7-1-3", "F-7-1-4", "F-7-1-5", # from task 7
             "F-8-1-1", "F-8-1-2", "F-8-1-3", "F-8-1-4", "F-8-1-5", # from task 8
             "F-9-1-1", "F-9-1-2", "F-9-1-3", "F-9-1-4", "F-9-1-5", # from task 9
             "F-10-1-1","F-10-1-2","F-10-1-3","F-10-1-4","F-10-1-5") # from task 10


profile2 =c("F-1-2-1", "F-1-2-2", "F-1-2-3", "F-1-2-4", "F-1-2-5",
            "F-2-2-1", "F-2-2-2", "F-2-2-3", "F-2-2-4", "F-2-2-5",
            "F-3-2-1", "F-3-2-2", "F-3-2-3", "F-3-2-4", "F-3-2-5",
            "F-4-2-1", "F-4-2-2", "F-4-2-3", "F-4-2-4", "F-4-2-5",
            "F-5-2-1", "F-5-2-2", "F-5-2-3", "F-5-2-4", "F-5-2-5",
            "F-6-2-1", "F-6-2-2", "F-6-2-3", "F-6-2-4", "F-6-2-5",
            "F-7-2-1", "F-7-2-2", "F-7-2-3", "F-7-2-4", "F-7-2-5",
            "F-8-2-1", "F-8-2-2", "F-8-2-3", "F-8-2-4", "F-8-2-5",
            "F-9-2-1", "F-9-2-2", "F-9-2-3", "F-9-2-4", "F-9-2-5",
            "F-10-2-1","F-10-2-2","F-10-2-3","F-10-2-4","F-10-2-5")


not.mess$Profile.n = ifelse(not.mess$key %in% profile1, "1", 
                        ifelse(not.mess$key %in% profile2, "2", NA))


# remove quotes from factor levels
str(not.mess$value)
not.mess$value <- as.factor(not.mess$value)
levels(not.mess$value) <- gsub('["\\]', "", levels(not.mess$value))


#### Create dummies for every level ####

# look at number of levels (+ attributes) that this particular respondent saw 
# during the whole conjoint task
table(not.mess$value)

# Recode every level into one and put it into a separate columns. 
# It will give a zero-one matrix with 15 vectors in total. 
# Every column will correspond to a level.
# recode(case$value,  "...um die Regierung zu unterstu00fctzen" = "1", .default = "0")
# add 15 empty columns to store new values
# for(i in case$value) case[,i] <- NA

zero.one_df = data.frame(row.names=rownames(not.mess))

for (i in "value") {
  for (x in not.mess$value) {
    zero.one_df[x] = as.numeric(not.mess[i] == x)
  }
}

# merge zero-one matrix of levels with 
conjoint <- cbind(not.mess, zero.one_df)
conjoint <- conjoint[,-c(2:4)] # remove unnecessary columns

# remove columns that are attributes, not levels
conjoint <- within(conjoint, rm("Einwanderung", "Kriminalitu00e4t", #"Order", 
                            "Partei tritt an...", "Sozialleistungen",
                            "Europu00e4ische Union"))

# remove observations that were generated by attributed, not levels
conjoint <- na.omit(conjoint)

#### Merge rows within task and profile #### 

final.conjoint <- conjoint %>% group_by(Panelist_ID, Task.n, Profile.n) %>% 
  summarise_all(funs(max(as.character(.)))) # as you can see, there are 560 observations, 
# 28 respondents evaluated 20 profiles - Pilot 1
# 17 respondents evaluated 16 profiles - Pilot 2


# NOTE: minor detail, but needs to be fixed: from character to numeric to make Task.n 
# ordered, otherwise Task 10 is after Task 1
final.conjoint$Task.n <- as.numeric(as.character(final.conjoint$Task.n))
final.conjoint <- final.conjoint[order(final.conjoint$Task.n),]


# check how many observations there are per respondent. it should be 20 per ID
table(final.conjoint$Panelist_ID)

# Now let's get ready choices for merging them with corresponding tasks and profiles
choices <- valid_dt[,11:20] # only take columns with respondents choices of profiles

# Here, 28 rows correspond to the number of respondents
rownames(choices) <- valid_dt$REF  # make row names correspond to a panelist's ID
colnames(choices) <- rep(1:10) # make column names correspond to a task number

setDT(choices, keep.rownames = TRUE)[] # move IDs from rownames into a column
names(choices)[1] <- "Panelist_ID"

# Reshaping wide format df to long format df to make choices correspond to task number and 
# to a respondent, who made that choice in that particular task
reshaped.choices <- choices %>% gather(Task.n, Choice_01, 2:11)

# This is tricky one: merging choices with corresponding profiles, tasks and respondents
dt.with.choices <- merge(final.conjoint, reshaped.choices, all=T)


# NOTE: minor detail, but needs to be fixed <-  from character to numeric to make Task.n 
# ordered, otherwise Task 10 is after Task 1
dt.with.choices <- dt.with.choices[order(dt.with.choices$Task.n), ]


# Basically here is THE outcome variable.
# Since we merged the data by task number, now we need to create a variable
# that would correspond to a profile choicen by respondents in a task
conjoint_pilot <- dt.with.choices %>% 
  mutate(Choices_binary = ifelse(Profile.n == 2 & Choice_01 == 2, 1,
                                 ifelse(Profile.n == 1 & Choice_01 == 1, 1, 0)))


# make all factors as vectors
for(i in c(2:ncol(conjoint_pilot))) {
  conjoint_pilot[,i] <- as.numeric(as.character(conjoint_pilot[,i]))
}

str(conjoint_pilot)

# And now the data from the conjoint experiment is ready for the analysis! 
# So let's save the data somewhere:
save(conjoint_pilot, file = "Conjoint_pilot2.RData")



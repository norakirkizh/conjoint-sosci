# Example of a conjoint survey experiment based on SoSci Survey platform

This repository consists of all nessesary scripts for running a conjoint survey experiment on SoSci Survey, pre-processing, and the analysis in R.

Conjoint survey experiment helps to identify peoples' preferences over a particular object of interest in a multidimentional framework. For instance, when users decides to read a news article online, they might consider several features (dimentions) of the article before reading it. Let's say topic relevance (climate change, immigration, health care), credibility of the news source (established media, new digital born news web site), and partizanship (Republican or Democrat) are the only possible features of news articles. How can we identify what feature has the largest utility and what combination of features is the most desiable for users? Conjoint survey experiments can help to answer this kind of questions.

In this working example, respondents are suggested to choose between two hypothetical parties, which are running for a seat in a local parliament. Attributes and levels represent policy proposals.

## Generating conjoint design

[Conjoint Survey Design Tool](https://github.com/astrezhnev/conjointsdt) developed by Anton Strezhnev (NYU) can generate a conjoint design that will set a randomization algorithm based on predefined attributes and levels of object of interest. Here two parties have five attributes with three levels each. The output from conjointsdt is a PHP script:

```php
$featurearray = array("Reason for running" => array("Because political elites don't represent the real people","To continue to serve the current government","To again enter parliament and influence policies"),"European Union" => array("EU integration should be stopped","EU integration should stay the same"),"Immigration" => array("Stop immigration from outside the EU","Let up to 200,000 immigrants from outside the EU a year"),"Social benefits" => array("Social benefits for immigrants after they become citizens","No social benefits for immigrants","Social benefits for immigrants immediately upon arrival"),"The role of the government" => array("To make people to provide for themselves","To ensure that the government provides for everyone"),"Crime" => array("Make measures to fight crime stronger","Relax sentences because it doesn't reduce crime"));

	$restrictionarray = array();


	// Indicator for whether weighted randomization should be enabled or not
	$weighted = 0;

	// K = Number of tasks displayed to the respondent
	$K = 5;

	// N = Number of profiles displayed in each task
	$N = 2;

	// num_attributes = Number of Attributes in the Array
	$num_attributes = count($featurearray);


	$attrconstraintarray = array(array("Immigration","Social benefits"));


	// Re-randomize the $featurearray

	// Place the $featurearray keys into a new array
	$featureArrayKeys = array();
	$incr = 0;

	foreach($featurearray as $attribute => $levels){	
		$featureArrayKeys[$incr] = $attribute;
		$incr = $incr + 1;
	}

	// Backup $featureArrayKeys
	$featureArrayKeysBackup = $featureArrayKeys;

	// If order randomization constraints exist, drop all of the non-free attributes
	if (count($attrconstraintarray) != 0){
		foreach ($attrconstraintarray as $constraints){
			if (count($constraints) > 1){
				for ($p = 1; $p < count($constraints); $p++){
					if (in_array($constraints[$p], $featureArrayKeys)){
						$remkey = array_search($constraints[$p],$featureArrayKeys);
						unset($featureArrayKeys[$remkey]);
					}
				}
			}
		}
	} 
	// Re-set the array key indices
	$featureArrayKeys = array_values($featureArrayKeys);
	// Re-randomize the $featurearray keys
	shuffle($featureArrayKeys);

	// Re-insert the non-free attributes constrained by $attrconstraintarray
	if (count($attrconstraintarray) != 0){
		foreach ($attrconstraintarray as $constraints){
			if (count($constraints) > 1){
				$insertloc = $constraints[0];
				if (in_array($insertloc, $featureArrayKeys)){
					$insert_block = array($insertloc);
					for ($p = 1; $p < count($constraints); $p++){
						if (in_array($constraints[$p], $featureArrayKeysBackup)){
							array_push($insert_block, $constraints[$p]);
						}
					}
					
					$begin_index = array_search($insertloc, $featureArrayKeys);
					array_splice($featureArrayKeys, $begin_index, 1, $insert_block);
				}
			}
		}
	}


	// Re-generate the new $featurearray - label it $featureArrayNew

	$featureArrayNew = array();
	foreach($featureArrayKeys as $key){
		$featureArrayNew[$key] = $featurearray[$key];
	}


	// Initialize the array returned to the user
	// Naming Convention
	// Level Name: F-[task number]-[profile number]-[attribute number]
	// Attribute Name: F-[task number]-[attribute number]
	// Example: F-1-3-2, Returns the level corresponding to Task 1, Profile 3, Attribute 2 
	// F-3-3, Returns the attribute name corresponding to Task 3, Attribute 3

	$returnarray = array();

	// For each task $p
	for($p = 1; $p <= $K; $p++){

		// For each profile $i
		for($i = 1; $i <= $N; $i++){

			// Repeat until non-restricted profile generated
			$complete = False;

			while ($complete == False){

				// Create a count for $attributes to be incremented in the next loop
				$attr = 0;
				
				// Create a dictionary to hold profile's attributes
				$profile_dict = array();

				// For each attribute $attribute and level array $levels in task $p
				foreach($featureArrayNew as $attribute => $levels){	
					
					// Increment attribute count
					$attr = $attr + 1;

					// Create key for attribute name
					$attr_key = "F-" . (string)$p . "-" . (string)$attr;

					// Store attribute name in $returnarray
					$returnarray[$attr_key] = $attribute;

					// Get length of $levels array
					$num_levels = count($levels);

					// Randomly select one of the level indices
					if ($weighted == 1){
						//$level_index = weighted_randomize($probabilityarray, $attribute) - 1;

					}else{
						$level_index = mt_rand(1,$num_levels) - 1;	
					}	

					// Pull out the selected level
					$chosen_level = $levels[$level_index];
				
					// Store selected level in $profileDict
					$profile_dict[$attribute] = $chosen_level;

					// Create key for level in $returnarray
					$level_key = "F-" . (string)$p . "-" . (string)$i . "-" . (string)$attr;

					// Store selected level in $returnarray
					$returnarray[$level_key] = $chosen_level;

				}

				$clear = True;
				// Cycle through restrictions to confirm/reject profile
				if(count($restrictionarray) != 0){

					foreach($restrictionarray as $restriction){
						$false = 1;
						foreach($restriction as $pair){
							if ($profile_dict[$pair[0]] == $pair[1]){
								$false = $false*1;
							}else{
								$false = $false*0;
							}
							
						}
						if ($false == 1){
							$clear = False;
						}
					}
				}
				$complete = $clear;
			}
		}


	}
  ```
  ## Embedding conjoint design into SoSci Survey
  
SoSci Survey is a survey platform that allows running a survey experiments free of charge if the project is made for academic purposes. After creating a survey project on the SoSci platform, the php script from conjointsdt goes to php field with important additions programmed by [Roberto Ulloa](https://github.com/robertour) (GESIS Leibnitz Institute of Social Sciences) below.

![php_sosci](php_sosci)

To make conjoint design to be distributed across all tasks this code whould go first:
```php
if (value('IV01_01')){
} else {

\\\ CONJOINT CODE GOES HERE

	put('IV01_01', json_encode($returnarray));
}
```

Randomized party attributes and levels combined with a HTML code for a table should appear in every conjoint task. If there are eight tasks, the following code should be added to every task:

```php
$task = 1;
$total_attributes = 6;

$error_message = '<h1>Error! Please let us know.</h1>';
$val = value('IV01_01');
if ($val){

	$returnarray = json_decode($val);

	$table = '';
	$header = '<tr><th></th><th>Party 1</th><th>Party 2</th></tr>';
	for ($i=1; $i<=$total_attributes ; $i++){
	$table .= '<tr><th>'.
	$returnarray['F-'.$task.'-'. $i].
	'</th><td>'.
	$returnarray['F-'.$task.'-1-'. $i].
	'</td><td>'.
	$returnarray['F-'.$task.'-2-'. $i].
	'</td></tr>';
	}
	html('<table border="4" cellpadding="10" cellspacing="0" >'.$header. $table . '</table>');

} else {
        html($error_message);
}
```
This will produce the following table across all tasks:

![conjoint_table_sosci](conjoint_table_sosci.png)

Conjoint on SciSci Survey is ready to be run through!

## Pre-processing data 

SoSci Survey stores participants responces in several formats including csv. Responces to classical survey questions are presented in straightforward way. The conjoint output, however, looks convoluted and make sence only pre-processing in R. Essentially, the output contains the information on attributes and levels a respondent saw on the screen: `F-1(task)-1(profile)-1(attribute)`. For instance, this is a two data points generated from one respondent:

`\"F-1-1\":\"Immigration\, \"F-1-1-1\":\"Allow up to 200,000 immigrants a year to come to the country",`

For this respondent, in Task 1, Attribute 1 took a value `Immigration`. Under Task 1, Profile 1, Attribute 1 took a level value `Allow up to 200,000 immigrants a year to come to the country`.

I wrote an R script with extensive notes that make collected data from conjoint survey experiment ready for analysis:

```r

rm(list=ls())

library(tidyverse)
library(data.table)

getwd()
setwd()

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

# This is a tricky one: merging choices with corresponding profiles, tasks and respondents
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

```
Once you save the pre-processed dataframe in a desirable format, the data is ready for conjoint analysis.

## Analysis

Following recent applications of conjoint survey experiment, for the pooled data, I estimate the average marginal component effect (AMCE) (see Hainmueller, Hopkins and Yamamoto 2014) and marginal means from Leeper, Hobolt and Tilley (2019) for comparison. Specifically, I use OLS to regress single choice outcomes on binary variables of attribute levels, where 1 indicates that a respondent saw that particular level while evaluating a candidate profile. For AMCE, one level in each attribute is omitted as a reference category, and standard errors will be clustered by respondent. A script for conjoint analysis is avaiable on [Thomas Leeper](https://github.com/leeper/conjoint-subgroups) github page. Leeper's script also contains code for making plot with `ggplot2`. Here is the result from the working example, where respondents choose between two parties with five attributes representing policy proposals:

![conjoint_result](conjoint_result.png)

## External validity

Recent debate over conjoint analysis external validity (Abramson, Kocak and Magazinnik, 2019) suggests that AMCE measures an average of respondents' ideal points rather than the most preferred policy proposal or candidate feature because the distribution of weights that respondents put on every attribute is not uniform. I will show how to address this issue with the method proposed in De La Cuesta, Egami and Imai (2019) soon.

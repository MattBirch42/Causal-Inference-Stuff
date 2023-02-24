rm(list = ls())
stem_data <- read.csv("https://raw.githubusercontent.com/MattBirch42/Causal-Inference-Stuff/main/Fake_DD_stemPolicy_scores.csv")


#######################################################################################
###### DATA DICTIONARY ################################################################
#######################################################################################
# This is fake data about a STEM program that gives students extra STEM training in
# Participating regions. We will measure how the program affects test scores.
# year = Year
# male = 1 if male, 0 otherwise
# Region: These represent some fictional geographic region
# Race: These categorize different unspecified races.
# Stem: stem denotes extra STEM prep in grades 10-12.
#       Stem program starts in 2013 in region 2 and in 2015 in region 5.
#       No other region has stem
# bday_m_cutoff: regions 1-4 have compulsory schooling laws (save this for RD later)
# Education: parent’s years of formal schooling
# Income: family income
# Score: Assessment exam score (this is our dependent variable)
# Varsity: did varsity sports
# single_parent_hh: Lives with single parent


# Setting up region 2 diff in diff variables
stem_data$r2<-ifelse(stem_data$region==2,1,0)
stem_data$y2013plus<-ifelse(stem_data$year>=2013,1,0)
stem_data$r2_2013<-stem_data$r2*stem_data$y2013plus
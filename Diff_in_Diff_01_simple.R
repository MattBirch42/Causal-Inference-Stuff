rm(list = ls())

library(ggplot2)

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
stem_data$region = as.factor(stem_data$region)


# Let's do the region 2 diff in diff only. 
# We'll save region 5 for another video because it is treated at a different year.
stem2 = stem_data[stem_data$region != 5,]


# Create plot of avg scores in region 2 vs other regions combined.
stem2$reg2 = ifelse(stem2$region == 2,'Region 2','Other Regions')

avg_scores <- aggregate(score ~ reg2 + year, data = stem2, FUN = mean)

ggplot(avg_scores, aes(x = year, y = score, color = reg2)) +
  geom_line() +
  labs(x = "Year", y = "Average Score", color = "Region") +
  theme_light()+
  geom_vline(xintercept = 2012, linetype = "dashed")



###################################################################
# This is how we set up diff-in-diff. Requires at least 3 variables.
#     Dummy variable for treatment group
stem2$r2<-ifelse(stem2$region==2,1,0)
#     Dummy variable for if treatment is happening
stem2$y2013plus<-ifelse(stem2$year>=2013,1,0)
#     Interaction: the treatment group is being treated
stem2$r2_2013<-stem2$r2*stem2$y2013plus
###################################################################


dd_reg1 <- lm(score ~ r2_2013 + r2 + y2013plus, data = stem2)

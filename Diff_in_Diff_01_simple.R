# Difference in differences
# Author: Matt Birch
# Date: 3/1/2023
# This will largely be divided into 4 sections. In (1), I introduce simple diff-in-diff.
# In (2), I do diff-in-diff with synthetic controls. In (3), I do diff-in-diff with
# multiple cutoffs. In (4), I do higher order diff-in-diff. 

###################################################################################################
###################################################################################################
###################### Section 1: Plain Vanilla DiD ###############################################
###################################################################################################
###################################################################################################
# video: https://youtu.be/zZrOfE4Lus0
rm(list = ls())

library(ggplot2)   # for visualizations
library(sandwich)  # to get fancy covariance matrix
library(lmtest)    # to connect covaiance matrix with regression

stem_data <- read.csv("https://raw.githubusercontent.com/MattBirch42/Causal-Inference-Stuff/main/vanilla_did.csv")

stem2 <- stem_data
cutoff <- 2015
min_year <- min(stem2$year)
max_year <- max(stem2$year)

#initial visualizations
stem2$reg24 = ifelse(stem2$region %in% c(2,4),'Region 2 or 4','Other Regions')

ggplot(stem2, aes(x = year, y = score, color = reg24)) +
  geom_point(size = 1, alpha = 0.1) +
  labs(x = "Year", y = "Average Score", color = "Region") +
  scale_color_manual(values = c("blue", "red")) +
  theme_light() +
  geom_vline(xintercept = (cutoff-0.5), linetype = "dashed") +
  scale_x_continuous(breaks = seq(min_year,max_year, by = 1))


#   displaying only average values instead of all values
avg_scores <- aggregate(score ~ reg24 + year, data = stem2, FUN = mean)

ggplot(avg_scores, aes(x = year, y = score, color = reg24)) +
  geom_point(size = 3) +
  labs(x = "Year", y = "Average Score", color = "Region") +
  scale_color_manual(values = c("blue", "red")) +
  theme_light() +
  geom_vline(xintercept = (cutoff-0.5), linetype = "dashed") +
  scale_x_continuous(breaks = seq(min_year,max_year, by = 1))

# setting up your first DiD
# the big 3
stem2$r24<-ifelse(stem2$region %in% c(2,4),1,0)
stem2$y2015plus<-ifelse(stem2$year>=cutoff,1,0)
stem2$r24_2015<-stem2$r24*stem2$y2015plus

dd_reg1 <- lm(score ~ r24_2015 + r24 + y2015plus, data = stem2)
dd_reg1$coefficients

# This plot can be a bit confusing, especially with our poorly specified model

ggplot(avg_scores, aes(x = year, y = score, color = reg24)) +
  geom_point(size = 3) +
  labs(x = "Year", y = "Average Score", color = "Region") +
  scale_color_manual(values = c("blue", "red")) +
  theme_light()+
  geom_vline(xintercept = (cutoff-0.5), linetype = "dashed") +
  scale_x_continuous(breaks = seq(min_year,max_year, by = 1)) + 
  # control
  geom_segment (aes (x=min_year,
                     xend=(cutoff-1),
                     y=dd_reg1$coefficients[1],
                     yend=dd_reg1$coefficients[1]), 
                color = "blue") + 
  geom_segment (aes (x=cutoff,
                     xend=max_year,
                     y=dd_reg1$coefficients[1]+dd_reg1$coefficients[4],
                     yend=dd_reg1$coefficients[1]+dd_reg1$coefficients[4]), 
                color = "blue") +
  # treatment
  geom_segment (aes (x=min_year,
                     xend=(cutoff-1),
                     y=dd_reg1$coefficients[1]+dd_reg1$coefficients[3],
                     yend=dd_reg1$coefficients[1]+dd_reg1$coefficients[3]), 
                color = "red") + 
  geom_segment (aes (x=cutoff,
                     xend=max_year,
                     y=dd_reg1$coefficients[1]+dd_reg1$coefficients[2]+dd_reg1$coefficients[3]+dd_reg1$coefficients[4],
                     yend=dd_reg1$coefficients[1]+dd_reg1$coefficients[2]+dd_reg1$coefficients[3]+dd_reg1$coefficients[4]), 
                color = "red")

stem2$year_adjusted <- stem2$year - min_year

dd_reg2 <- lm(score ~ r24_2015 + r24 + year_adjusted, data = stem2)
dd_reg2$coefficients

ggplot(avg_scores, aes(x = year, y = score, color = reg24)) +
  geom_point(size = 3) +
  labs(x = "Year", y = "Average Score", color = "Region") +
  scale_color_manual(values = c("blue", "red")) +
  theme_light()+
  geom_vline(xintercept = (cutoff-0.5), linetype = "dashed") +
  scale_x_continuous(breaks = seq(min_year,max_year, by = 1)) + 
  # control
  geom_segment (aes (x=min_year,
                     xend=(cutoff-1),
                     y=dd_reg2$coefficients[1],
                     yend=dd_reg2$coefficients[1] + (cutoff-1-min_year)*dd_reg2$coefficients[4]), 
                color = "blue") + 
  geom_segment (aes (x=cutoff,
                     xend=max_year,
                     y=dd_reg2$coefficients[1] + (cutoff-min_year)*dd_reg2$coefficients[4]),
                yend=dd_reg2$coefficients[1]+(max_year - min_year)*dd_reg2$coefficients[4],
                color = "blue") +
  # treatment
  geom_segment (aes (x=min_year,
                     xend=(cutoff-1),
                     y=dd_reg2$coefficients[1] + dd_reg2$coefficients[3],
                     yend=dd_reg2$coefficients[1]  + dd_reg2$coefficients[3] + (cutoff-1-min_year)*dd_reg2$coefficients[4]), 
                color = "red") + 
  geom_segment (aes (x=cutoff,
                     xend=max_year,
                     y=dd_reg2$coefficients[1] + dd_reg2$coefficients[2]+ dd_reg2$coefficients[3]+ (cutoff-min_year)*dd_reg2$coefficients[4]),
                yend=dd_reg2$coefficients[1]+ dd_reg1$coefficients[2]+ dd_reg2$coefficients[3] +(max_year - min_year)*dd_reg2$coefficients[4],
                color = "red") + 
  geom_segment (aes (x=cutoff,
                     xend=max_year,
                     y=dd_reg2$coefficients[1] + dd_reg2$coefficients[3]+ (cutoff-min_year)*dd_reg2$coefficients[4]),
                yend=dd_reg2$coefficients[1]+ dd_reg2$coefficients[3] +(max_year - min_year)*dd_reg2$coefficients[4],
                color = "red",
                linetype = 'dotted')

stem2$cluster_var <- paste(stem2$region, stem2$year_adjusted, sep = "-")

dd_reg2 <- lm(score ~ r24_2015 + r24 + year_adjusted, data = stem2)
better_cov <- vcovHC(dd_reg2, type = "HC0", cluster = c("cluster_var"))


print(summary(dd_reg2))

dd_reg2_clustered <-coeftest(dd_reg2, vcov = better_cov)
print(dd_reg2_clustered)


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

# Clear the environment
# %reset -f


# Load the required libraries
import seaborn as sns
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from statsmodels.formula.api import ols
from statsmodels.stats.sandwich_covariance import cov_cluster
from statsmodels.stats.api import anova_lm

# Load the data
url = 'https://raw.githubusercontent.com/MattBirch42/Causal-Inference-Stuff/main/vanilla_did.csv'
df = pd.read_csv(url)

stem2 = df.copy()
cutoff = 2015
min_year = min(stem2['year'])
max_year = max(stem2['year'])

# Initial visualizations
stem2['reg24'] = np.where(stem2['region'].isin([2, 4]), 'Region 2 or 4', 'Other Regions')

plt.figure()
sns.scatterplot(x = 'year', y = 'score', hue = 'reg24', data = stem2, alpha = 0.1)
plt.xlabel('Year')
plt.ylabel('Average Score')
plt.legend(title = "Region", labels = ['Other Regions', 'Region 2 or 4'])
plt.axvline(x = (cutoff - 0.5), linestyle = "dashed")
plt.xticks(range(min_year, max_year+1, 1))
plt.show()

avg_scores = stem2.groupby(['reg24', 'year'], as_index=False)['score'].mean()

plt.figure()
sns.scatterplot(x = 'year', y = 'score', hue = 'reg24', data = avg_scores)
plt.xlabel('Year')
plt.ylabel('Average Score')
plt.legend(title = "Region", labels = ['Other Regions', 'Region 2 or 4'])
plt.axvline(x = (cutoff - 0.5), linestyle = "dashed")
plt.xticks(range(min_year, max_year+1, 1))
plt.show()

# The big 3
stem2['r24'] = np.where(stem2['region'].isin([2, 4]), 1, 0)
stem2['y2015plus'] = np.where(stem2['year'] >= cutoff, 1, 0)
stem2['r24_2015'] = stem2['r24'] * stem2['y2015plus']

# Difference in difference regression
model1 = ols('score ~ r24_2015 + r24 + y2015plus', data = stem2).fit()
print(model1.summary())

plt.figure()
sns.scatterplot(x = 'year', y = 'score', hue = 'reg24', data = avg_scores)
plt.xlabel('Year')
plt.ylabel('Average Score')
plt.legend(title = "Region", labels = ['Other Regions', 'Region 2 or 4'])

plt.axvline(x = (cutoff - 0.5), linestyle = "dashed")

# Control group
plt.axhline(y = model1.params['Intercept'], xmin = min_year, xmax = (cutoff - 1), color = 'blue')
plt.axhline(y = model1.params['Intercept'] + model1.params['y2015plus'], xmin = cutoff, xmax = max_year, color = 'blue')

# Treatment group
plt.axhline(y = model1.params['Intercept'] + model1

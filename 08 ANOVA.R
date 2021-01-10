
# Page 318, Example 8.2.1
dim(datA_821_raw <- read.csv('data/EXA_C08_S02_01.csv'))
datA_821_raw
head(datA_821 <- reshape2::melt(datA_821_raw, 
                                # measure.vars = names(datA_821_raw),
                                value.name = 'Selenium', variable.name = 'Meat'))
boxplot(Selenium ~ Meat, data = datA_821)
(aov_821 = aov(Selenium ~ Meat, data = datA_821))
anova(aov_821)
# ANOVA shows that the three group means are not all same (p < .001)
# you don't need to specify 'alpha' here
# the line 'Signif. codes' gives you
# 1. potential choices of alpha's as the 'cut-off values'
# 2. significance level based on your choice of alpha
# for example, '< 2.2e-16 ***' means you should reject the null at 1/1000 significance level 



# Page 325, Example 8.2.2
library(multcomp)
summary(tukey_822 <- glht(aov_821, linfct = mcp(Meat = 'Tukey'))) # Tukey's Honestly Significant Difference
# Tukey HSD of multiple comparisons shows that all but one pairwise differences are significant (p < .001)
confint(tukey_822)



# Page 339, Example 8.3.1
head(datA_831_raw <- read.csv('data/EXA_C08_S03_01.csv'))
class(datA_831_raw$ageGroup)
datA_831 = within(datA_831_raw, expr = {
  ageGroup = factor(ageGroup)
})
(aov_831 = aov(time ~ method + ageGroup, data = datA_831))
anova(aov_831)



# Page 348, Example 8.4.1
head(datA_841_raw <- read.csv('data/EXA_C08_S04_01.csv'))
datA_841 = within(datA_841_raw, expr = {
  SUBJ = factor(SUBJ)
  TIME = factor(TIME)
})
(aov_841 = aov(FUNC ~ SUBJ + TIME, data = datA_841))
anova(aov_841)



# Page 352, Example 8.4.2 (optional; out of the scope of this course)
(datA_842_raw <- read.csv('data/EXA_C08_S04_02.csv'))
# 'subject': patient ID (NOT repeated measures, REMOVE)
# treatment: 1 (placebo) and 2 (aloe juice)
# totalC[1-4]: measurement at four timepoints, baseline, 2wk, 4wk and 6wk
datA_842 = reshape2::melt(within(datA_842_raw, expr = {
  subject = factor(subject)
  treatment = factor(treatment)
}), id.vars = c('subject', 'treatment'), variable.name = 'time', value.name = 'OralScores')
head(datA_842)
# Hypothesis: 
# Main effect of 'treatment';
# Main effect of 'time';
# Interaction between 'treatment' and 'time'
(aov_842 = aov(OralScores ~ treatment * time + Error(subject), data = datA_842))
class(aov_842)
summary(aov_842)
# Section 'Error: subject' in R output
# .. is Figure 8.4.4 (Page 355) 'Tests of Between-Subjects Effects' (without the row of 'Intercept')
# .. 'treatment' row: effect of treatment at \strong{baseline} (i.e. reference time), degree-of-freedom (dof) = 2-1 = 1
# .. 'Residuals' row: residual at baseline, dof = (25-1) - (2-1) = 23
# .. It's important to note that 'treatment' is a \strong{between-subject factor}.
# Section 'Error: Within' in R output
# .. is Figure 8.4.4 'Tests of Within-Subjects Effects'
# .. 'time' row: effect of time within subject for placebo (i.e. reference treatment), dof = 4-1 = 3
# .. 'treatment:time' row: interation of treatment and time, dof = (2-1)*(4-1) = 3
# .. 'Residuals' row: residual at 2wk, 4wk and 6wk, dof = (4-1)*23 = 69 [(4-1) timepoints, 23 dof at each timepoints]
# Analyasis Interpretation
# .. No significant difference detected between placebo vs. aloe at baseline (p = .815)
# .. No significant difference detected in the trends over time between placebo vs. aloe (p = .974)
# .. Significant difference detected among the four measurement times, for either placebo or aloe patients (p = 3e-7)
# R code below creates an equivalent ANOVA model
anova(aov(OralScores ~ treatment * time + subject, data = datA_842))
# .. 'subject' is considered as a block factor 


# Page 364, Example 8.5.2
head(datA_852_raw <- read.csv('data/EXA_C08_S05_02.csv'))
datA_852 = within(datA_852_raw, expr = {
  A = structure(A, levels = c('Cardiac', 'Cancer', 'CVA', 'Tuberculosis'), class = 'factor')
  B = structure(B, levels = c('20s', '30s', '40s', '50+'), class = 'factor')
})
(aov_852 = aov(HOME ~ A * B, data = datA_852))
anova(aov_852)

summary(lm(HOME ~ A * B, data = datA_852)) # producess alpha, beta and (alpha beta)'s in the formulation 





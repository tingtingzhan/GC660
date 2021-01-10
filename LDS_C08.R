
library(multcomp)

# Optional: follow Example 8.4.2
# MEDSCORES
head(MEDSCORES_raw <- read.csv('data/LDS_C08_MEDSCORES.csv'))
dim(MEDSCORES_raw)
MEDSCORES = within(MEDSCORES_raw, expr = {
  day = factor(day)
  test = factor(test)
  ID = factor(ID)
})
(aov_MEDSCORES = aov(score ~ day * test + ID, data = MEDSCORES))
anova(aov_MEDSCORES)
# End of optional 




# LSADATA
head(LSADATA_raw <- read.csv('data/LDS_C08_LSADATA.csv'))
dim(LSADATA_raw)
if (FALSE) { # only FYI
  is.data.frame(LSADATA_raw)
  is.list(LSADATA_raw)
  length(LSADATA_raw)
  (x = list(a = 1, b = 2, c = 3))
  x[1:2]
  x[-1L]
  class(x[1]) # sub-list
  class(x[[1]]) # element of list
}
# first column does not mean anything at all!  read the question carefully. 
names(LSADATA_raw)[-1L] = c('Normal', 'Benign', 'Primary', 'Metastatic')
head(LSA <- reshape2::melt(LSADATA_raw[-1L], variable.name = 'Group', value.name = 'LSA'))
# what you get is a 'message' instead of an 'error'; everything is fine
boxplot(LSA ~ Group, data = LSA)
(aov_LSA = aov(LSA ~ Group, data = LSA))
anova(aov_LSA)
summary(tukey_LSA <- glht(aov_LSA, linfct = mcp(Group = 'Tukey')))
confint(tukey_LSA)




# SACEDATA
head(SACEDATA_raw <- read.csv('data/LDS_C08_SACEDATA.csv'))
dim(SACEDATA_raw)
names(SACEDATA_raw)[-1L] = c('Never', 'Active', 'Stable', 'Recovered')
head(SACE <- reshape2::melt(SACEDATA_raw[-1L], variable.name = 'Group', value.name = 'SACE'))
boxplot(SACE ~ Group, data = SACE)
(aov_SACE = aov(SACE ~ Group, data = SACE))
anova(aov_SACE) 
summary(tukey_SACE <- glht(aov_SACE, linfct = mcp(Group = 'Tukey')))
confint(tukey_SACE)



# Page 408, Question 4 (Large Data Set ‘CSF’)
head(CSFDATA_raw <- read.csv('data/LDS_C08_CSFDATA.csv'))
dim(CSFDATA_raw)
head(CSF <- reshape2::melt(CSFDATA_raw[-1L], variable.name = 'Disease', value.name = 'CSF')) # 1st column 'OBSERV' is both incorrect and meaningless
dim(CSF)
boxplot(CSF ~ Disease, data = CSF)
anova(aov_CSF <- aov(CSF ~ Disease, data = CSF)) 
# ANOVA shows that the five group means are not all same (p < .001)
summary(glht(aov_CSF, linfct = mcp(Disease = 'Tukey')))
# Tukey HSD of multiple comparisons shows that all pairwise differences are significant (p < .001) except for Normal vs. C (p = .435)







# RBCDATA
head(RBCDATA_raw <- read.csv('data/LDS_C08_RBCDATA.csv'))
dim(RBCDATA_raw)
head(RBCDATA <- reshape2::melt(RBCDATA_raw[-1L], variable.name = 'Diet', value.name = 'RBC')) # 1st column 'OBSERV' is both incorrect and meaningless
boxplot(RBC ~ Diet, data = RBCDATA)
anova(aov_RBC <- aov(RBC ~ Diet, data = RBCDATA)) 
summary(glht(aov_RBC, linfct = mcp(Diet = 'Tukey')))






# SERUMCHO
head(SERUMCHO_raw <- read.csv('data/LDS_C08_SERUMCHO.csv'))
dim(SERUMCHO_raw)
SERUMCHO_0 = within(SERUMCHO_raw, expr = {
  SUBJ = factor(SUBJ)
})
head(SERUMCHO <- reshape2::melt(SERUMCHO_0, id.var = 'SUBJ', variable.name = 'Diet', value.name = 'SC'))
# anova(aov(SC ~ SUBJ * Diet, data = SERUMCHO)) # wrong
(aov_SERUMCHO = aov(SC ~ Diet + SUBJ, data = SERUMCHO)) # correct; 'SUBJ' as block factor
anova(aov_SERUMCHO)






source('shared_functions.R')
library(car)

# Question 1. 10pts. (p283, q18)
# let group 1 be IVF and group 2 be spontaneous conception
sample2_pnorm((3071-3172), sd1 = 761, sd2 = 702, n1 = 163L, n2 = 321L, lower.tail = TRUE)
# Birth weight from IVF is lower than spontaneous conception (p = .078) at alpha = .1 level.


# Question 2. 10pts. (p283, q19)
head(dat_Tindall <- read.csv('data/REV_C07_19.csv'))
mean(dat_Tindall$Before)
mean(dat_Tindall$AFter)
with(dat_Tindall, t.test(Before, AFter, alternative = 'greater', paired = TRUE))
# Total cholesterol decreased after taking a statin drug (mean of before 245.8, mean of after 166.4, p < .001, paired t-test).


# Question 3. 10pts (p379, q16)
head(dat_Lacroix <- read.csv('data/REV_C08_16.csv'))
anova(aov(Count ~ Sens * Treat, data = dat_Lacroix))
# the interaction term is not significant at alpha = .05 level (which is specified by the question), thus should be removed
anova(aov(Count ~ Sens + Treat, data = dat_Lacroix))
# we detected difference between ovalbumin-sensitized and nonsensitized outcomes, as well as among the three different exposures

# Question 4. 10pts (p466, q18)
head(dat_Mathias <- read.csv('data/REV_C09_18.csv'))
summary(lm(ATT ~ AGE, data = dat_Mathias))
plot(ATT ~ AGE, data = dat_Mathias)
cor.test(~ ATT + AGE, data = dat_Mathias)
# Age is correlated with ATT (estimated correlation = .399, p = .026)
summary(lm(SOC ~ AGE, data = dat_Mathias))
plot(SOC ~ AGE, data = dat_Mathias)
cor.test(~ SOC + AGE, data = dat_Mathias)
# No evidence support that Age is correlated with SOC (estimated correlation = -.185, p = .319) 
summary(lm(HYP ~ AGE, data = dat_Mathias))
plot(HYP ~ AGE, data = dat_Mathias)
cor.test(~ HYP + AGE, data = dat_Mathias)
# No evidence support that Age is correlated with HYP (estimated correlation = .010, p = .957) 


# Question 5. 10pts (p589, q23)
head(dat_Hayton <- read.csv('data/REV_C11_23.csv'))
scatterplot(V ~ W | GROUP, data = dat_Hayton, regLine = TRUE, smooth = FALSE)
summary(lm(V ~ W * GROUP, data = dat_Hayton))
# both figure and the regression model show that the interaction between W and GROUP is significant
# In A(dult) group, which is the reference group, for every 1kg increase in infants weight, 
# the steady-state apparent volume of distribution (V) will have an increase of .137 liters (coefficient of 'W', p = .010)
# In C(hildren) group, for every 1kg increase in W, V will increase for (.137+.223) liters (coefficient of 'W:GROUPC').
# The slope of C group is significantly higher than the slope of A group (p = .002).
# In I(nfant) group, for every 1kg increase in W, V will increase for (.137+.226) liters (coefficient of 'W:GROUPI').
# We do not detect a significantly difference in the slope of I group vs. the slope of A group (p = .311).


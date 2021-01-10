
source('shared_functions.R')

# One-Sample Mean (Unknown sd)

(.t2(level = .95, df = 3))
(.t2(level = .95, df = 100))

# Page 173, Example 6.3.1
CI_t(xbar = 250.8, xsd = 130.9, n = 19L)

# Preview Quiz on t-statistic
# t = 2.3, large sample size, two-sided p-value is
2 * pnorm(2.3, lower.tail = FALSE) # 2.1%
# t = 2.3, sample size n = 2L, two-sided p-value is
2 * pt(2.3, df = 2L - 1L, lower.tail = FALSE) # 26.1%

# Example 5.3.2, Variation III (in slides)
2 * sample_pt(178.2, mean = 185.6, sd = 12.7, n = 10L, lower.tail = TRUE)
# Example 5.3.2, Variation IV (in slides)
sample_pt(178.2, mean = 185.6, sd = 12.7, n = 10L, lower.tail = TRUE)

# Page 228, Example 7.2.3
head(datA_723 <- read.csv('data/EXA_C07_S02_03.csv'))
t.test(datA_723$DAYS, mu = 15)
t.test(datA_723$DAYS, mu = 15, alternative = 'greater')
t.test(datA_723$DAYS, mu = 15, alternative = 'less')
# Page 231, Example 7.2.4
sample_pt(146, mean = 140, sd = 27, n = 157L, lower.tail = FALSE)
sample_pnorm(146, mean = 140, sd = 27, n = 157L, lower.tail = FALSE) # textbook
# Page 232, Example 7.2.5
datA_725 = c(33.38, 32.15, 34.34, 33.95, 33.46, 34.13, 33.99, 34.10, 33.85, 34.23, 34.45, 34.19, 33.97, 32.73, 34.05)
t.test(datA_725, mu = 34.5)

# Page 235, Exercise 7.2.13
head(datR_7213 <- read.csv('data/EXR_C07_S02_13.csv'))
t.test(datR_7213$rate, mu = 110, conf.level = .99)
# what happens to p-value if I make conf.level = .95 ? Increase, decrease or not change?
# Page 235, Exercise 7.2.15
head(datR_7215 <- read.csv('data/EXR_C07_S02_15.csv'))
t.test(datR_7215$Age, mu = 30, alternative = 'less')
# Page 235, Exercise 7.2.15
head(datR_7216 <- read.csv('data/EXR_C07_S02_16.csv'))
t.test(datR_7216$V1, mu = 14, alternative = 'greater')







# Two-Sample Mean

# Page 145, Example 5.4.1
sample2_pnorm((92-105), sd1 = 20, sd2 = 20, n1 = 15L, n2 = 15L) 
# Q: what and why is my choice of `lower.tail` here?
sample2_pnorm((105-92), sd1 = 20, sd2 = 20, n1 = 15L, n2 = 15L, lower.tail = FALSE) 
# Page 148, Example 5.4.2
sample2_pnorm(20, mean1 = 45, mean2 = 30, sd1 = 15, sd2 = 20, n1 = 35L, n2 = 40L, lower.tail = FALSE)
# Page 177, Example 6.4.1
CI_2z(xbar1 = 4.5, xbar2 = 3.4, sd1 = sqrt(1), sd2 = sqrt(1.5), n1 = 12L, n2 = 15L)
# Page 178, Example 6.4.2
CI_2z(xbar1 = 4.3, xbar2 = 13, sd1 = 5.22, sd2 = 8.97, n1 = 328L, n2 = 64L, level = .99)
# Page 180, Example 6.4.3
CI_2t_pooled(xbar1 = 4.7, sd1 = 9.3, n1 = 18L, xbar2 = 8.8, sd2 = 11.5, n2 = 10L)
# Page 184, Exercise 6.4.10
head(dat_6410_raw <- read.csv('data/EXR_C06_S04_10.csv'))
head(dat_6410 <- within(dat_6410_raw, expr = {
  group = structure(group, levels = c('NonSmokers', 'Smokers'), class = 'factor')
}))
class(Ca ~ group) # 'formula'
t.test(Ca ~ group, data = dat_6410)
t.test(Ca ~ group, data = dat_6410, var.equal = TRUE)
# Page 239, Example 7.3.2
head(datA_732 <- read.csv('data/EXA_C07_S03_02.csv'))
with(datA_732, t.test(x = CONTROL, y = SCI, alternative = 'less', var.equal = TRUE))
t.test(x = datA_732$CONTROL, y = datA_732$SCI, alternative = 'less', var.equal = TRUE)
# your textbook p-value is incorrect
with(datA_732, t.test(x = CONTROL, y = SCI, alternative = 'less'))
# Page 246, Exercise 7.3.3
head(datR_733 <- read.csv('data/EXR_C07_S03_03.csv'))
t.test(Length ~ Group, data = datR_733, conf.level = .99)
# Page 246, Exercise 7.3.4
head(datR_734 <- read.csv('data/EXR_C07_S03_04.csv'))
t.test(Length ~ Group, data = datR_734)
# Page 247, Exercise 7.3.5
head(datR_735 <- read.csv('data/EXR_C07_S03_05.csv'))
t.test(Diff ~ Group, data = datR_735)
# Page 249, Exercise 7.3.10
head(datR_7310 <- read.csv('data/EXR_C07_S03_10.csv'))
t.test(Age ~ Group, data = datR_7310)
# Page 251, Example 7.4.1
head(datA_741 <- read.csv('data/EXA_C07_S04_01.csv'))
with(datA_741, t.test(x = POSTOP, y = PREOP, alternative = 'greater', paired = TRUE))







# One-Sample Proportion via CLT

# Page 150, Example 5.5.1 (without Correction for Continuity)
sample_p_pnorm(phat = .40, n = 150L, p = .357, lower.tail = FALSE)
# Page 152, Example 5.5.2
sample_p_pnorm(phat = .45, n = 200L, p = .51)
# Page 185, Example 6.5.1
CI_CLT_p(phat = .18, n = 1220L, level = .95)
Hmisc::binconf(x = 1220*.18, n = 1220L, alpha = 1-.95, method = 'asymptotic')
prop.test(as.integer(1120*.18), n = 1120L, correct = FALSE)
# Page 258, Example 7.5.1
prop_ztest(x = 24L, n = 301L, p = .063, alternative = 'greater') # textbook text wrong, MINITAB correct
prop.test(x = 24L, n = 301L, p = .063, alternative = 'greater', correct = FALSE)
binom.test(x = 24L, n = 301L, p = .063, alternative = 'greater') # exact test, will have different p-value






# Two-Sample Proportion via CLT

# Page 155, Example 5.6.1
sample2_p_pnorm(phat_diff = .1, p1 = .28, p2 = .21, n1 = 100L, n2 = 100L, lower.tail = FALSE)
# Page 155, Example 5.6.2
sample2_p_pnorm(phat_diff = .05, p1 = .34, p2 = .26, n1 = 250L, n2 = 200L)
# Page 187, Example 6.6.1
CI_CLT_2p(x1 = 31L, x2 = 53L, n1 = 68L, n2 = 255L, level = .99)
prop.test(x = c(31L, 53L), n = c(68L, 255L), conf.level = .99, correct = FALSE)
# Page 261, Example 7.6.1
# Event of interest is 'fell below the third percentile of the specific gender's adult height.'
# Alternative hypothesis is whether females are more likely than males to satisfy the event of interest
# Let female be '2' (i.e. `x2` and `n2`) and male as '1' (i.e. `x1` and `n1`)
# the alternative hypothesis translate to p1 < p2
prop2_ztest(x1 = 11L, n1 = 29L, x2 = 24L, n2 = 44L, p_diff = 0, alternative = 'less')
prop2_ztest(x1 = 24L, n1 = 44L, x2 = 11L, n2 = 29L, p_diff = 0, alternative = 'greater')
prop.test(x = c(11L, 24L), n = c(29L, 44L), alternative = 'less', correct = FALSE)




# Below: optional

# One-Sample Variance

# Page 196, Example 6.9.1
datA_691 = c(9.7, 12.3, 11.2, 5.1, 24.8, 14.8, 17.7)
CI_chisq(datA_691)
# Page 264, Example 7.7.1
(sd_771 = sqrt(670.81))
(sd0_771 = sqrt(600))
CI_chisq(xsd = sd_771, n = 16L) # confidence interval
CI_chisq(xsd = sd0_771, n = 16L) # non-rejection region
.chisq2(level = .95, df = 15L) # to match the textbook

# Two-Sample Variance

# Page 200, Example 6.10.1
CI_F(sd1 = 8.1, sd2 = 5.9, n1 = 16L, n2 = 4L)
# Page 268, Example 7.8.1
pf((30.62/11.37)^2, df1 = 6L - 1L, df2 = 6L - 1L, lower.tail = FALSE)
qf(.05, df1 = 6L - 1L, df2 = 6L - 1L, lower.tail = FALSE)
# Page 270, Example 7.8.2
head(datA_732 <- read.csv('data/EXA_C07_S03_02.csv'))
with(datA_732, var.test(x = CONTROL, y = SCI))




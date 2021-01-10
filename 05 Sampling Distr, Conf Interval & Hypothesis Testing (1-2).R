
source('shared_functions.R')

.z2(.95)
(.z2(.95))


# One-Sample Mean (Known sd)

# Page 142, Example 5.3.2
sample_pnorm(190, mean = 185.6, sd = 12.7, n = 10L, lower.tail = FALSE)
# Page 143, Example 5.3.3
sample_pnorm(125, mean = 120, sd = 15, n = 50L) - 
  sample_pnorm(115, mean = 120, sd = 15, n = 50L)
# Page 166, Example 6.2.1
CI_z(xbar = 22, n = 10L, sd = sqrt(45))
# Page 168, Example 6.2.2
CI_z(xbar = 84.3, n = 15L, sd = sqrt(144), level = .99)
# Page 168, Example 6.2.3
CI_z(xbar = 17.2, n = 35L, sd = 8, level = .9)
# Page 169, Example 6.2.4
dat_624 = read.csv('data/EXA_C06_S02_04.csv')
head(dat_624)
CI_z(xbar = mean(dat_624$ACTIVITY), n = nrow(dat_624), sd = sqrt(.36))
CI_z(x = dat_624$ACTIVITY, sd = sqrt(.36)) # `xbar` and `n` will be calculated from `x`

# Slide on 'Test Statistic'
2 * pnorm(3, lower.tail = FALSE)
2 * pnorm(1.2, lower.tail = FALSE)

# Example 5.3.2, Variation I (in slides)
2 * sample_pnorm(178.2, mean = 185.6, sd = 12.7, n = 10L, lower.tail = TRUE) # two-sided p-value
# Example 5.3.2, Variation II (in slides)
sample_pnorm(178.2, mean = 185.6, sd = 12.7, n = 10L, lower.tail = TRUE) # one-sided p-value

# Page 222, Example 7.2.1
2 * sample_pnorm(27, mean = 30, sd = sqrt(20), n = 10L, lower.tail = TRUE)
CI_z(xbar = 27, sd = sqrt(20), n = 10L)
# Page 226, Example 7.2.2
sample_pnorm(27, mean = 30, sd = sqrt(20), n = 10L, lower.tail = TRUE)

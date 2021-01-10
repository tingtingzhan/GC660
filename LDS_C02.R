
source('shared_functions.R')

# Page 62, Large Data Set `NCBIRTH800`

# inspect the data
head(NCBIRTH800_raw <- read.csv('data/LDS_C02_NCBIRTH800.csv'))
dim(NCBIRTH800_raw)

NCBIRTH800 = within(NCBIRTH800_raw, expr = {
  sex = structure(sex, levels = c('male', 'female'), class = 'factor')
  marital = structure(marital, levels = c('married', 'not married'), class = 'factor')
  smoke = structure(smoke + 1L, levels = c('not smoked', 'smoked'), class = 'factor')
  premie = structure(premie + 1L, levels = c('not premature', 'premature'), class = 'factor')
  low = structure(low + 1L, levels = c('not low', 'low'), class = 'factor')
  # Practice: convert 'racemom' to factor
})

table(NCBIRTH800_raw$sex)
table(NCBIRTH800$sex)


####################
## chapter 2

# For the variables of MAGE, WEEKS, GAINED, TOUNCES, and TGRAMS

# 1. Calculate the mean, median, standard deviation, IQR, and range.
# 2. For each, construct a histogram and comment on the shape of the distribution.
# 7. Calculate the skewness and kurtosis of the data set. What do they indicate?

# Learn how R function works
simple_stats(NCBIRTH800$mage)
simple_stats(NCBIRTH800$weeks)
simple_stats(NCBIRTH800$gained)
simple_stats(NCBIRTH800$tounces)
simple_stats(NCBIRTH800$tgrams)

# 3. Do the histograms for TOUNCES and TGRAMS look strikingly similar? Why?
range(NCBIRTH800$tounces / NCBIRTH800$tgrams)
range(with(NCBIRTH800, tounces / tgrams))

# 4. Construct box-and-whisker plots for all four variables.
boxplot_named(NCBIRTH800$mage)
boxplot_named(NCBIRTH800$weeks)
boxplot_named(NCBIRTH800$gained)
boxplot_named(NCBIRTH800$tounces)

# 5. Construct side-by-side box-and-whisker plots for the variable of TOUNCES for women 
# who admitted to smoking and women who did not admit to smoking. 
# Do you see a difference in birth weight in the two groups? Which group has more variability?
boxplot(tounces ~ smoke, data = NCBIRTH800)
class(tounces ~ smoke) # formula; 'endpoint ~ predictor'

# 6. Construct side-by-side box-and-whisker plots for the variable of MAGE for women 
# who are and are not married. Do you see a difference in ages in the two groups? 
# Which group has more variability? Are the results surprising?
boxplot(mage ~ marital, data = NCBIRTH800)



####################
## chapter 3

prob_from_table(table(NCBIRTH800[c('premie', 'smoke')]))
prob_from_table(table(NCBIRTH800[c('low', 'marital')]))


#####################
## chapter 6 (Page 210)

CI_CLT_p(x = sum(NCBIRTH800$sex == 'male'), n = length(NCBIRTH800$sex)) # 1(a)
t.test(NCBIRTH800$mage) # 1(b)
t.test(NCBIRTH800$gained) # 1(c)
CI_CLT_p(x = sum(NCBIRTH800$smoke == 'smoked', na.rm = TRUE), n = sum(!is.na(NCBIRTH800$smoke))) # 1(d)
t.test(gained ~ smoke, data = NCBIRTH800) # 1(e)
t.test(tounces ~ marital, data = NCBIRTH800) # 1(f)
# 1(g)
prob_from_table(with(NCBIRTH800, table(low, marital)))
# fill in `x` and `n` for `CI_CLT_2p` and `prop.test` in class
CI_CLT_2p(x1 = 35L, n1 = 537L, x2 = 35L, n2 = 263L)
prop.test(x = c(35L, 35L), n = c(537L, 263L), correct = FALSE)




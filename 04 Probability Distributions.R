

source('shared_functions.R')

# Page 93, Example 4.2.1
(datA_421 = c(rep(1, times = 62L),
              rep(2, times = 47L),
              rep(3, times = 39L),
              rep(4, times = 39L),
              rep(5, times = 58L),
              rep(6, times = 37L),
              rep(7, times = 4L),
              rep(8, times = 11L)))
length(datA_421)
mean(datA_421)
var(datA_421)

(tab_421 = c(table(datA_421)))
(prob_421 = tab_421 / sum(tab_421))
(cumprob_421 = cumsum(tab_421) / sum(tab_421))

(out_421 = cbind(Freq = tab_421, 
                 'Relative Freq' = sprintf(fmt = '%.3f%%', prob_421 * 100), 
                 'Cumulative Relative Freq' = sprintf(fmt = '%.3f%%', cumprob_421 * 100)))
noquote(out_421, right = TRUE)

barplot(tab_421, main = 'Frequence Bar Plot; Example 4.2.1')

# d(ensity) nomenclature in R
barplot(prob_421, main = 'Relative Frequence Bar Plot; Example 4.2.1')

# p(robability) nomenclature in R
plot(stepfun(x = seq_along(tab_421)[-1L], y = cumprob_421), 
     main = 'Cumulative Relative Frequence; Example 4.2.1')


# Probability Distributions
# Use of 
# d(ensity)*, which is our 'probability'
# p(robability)*, which is our 'cumulative probability'
# q(uantile)* and r(andom number generation)* functions


# Bernoulli distribution 
# size = 1: Binomial distribution with 'single' experiment)
table(rbinom(n = 1e4L, size = 5L, prob = .3))
table(rbinom(n = 1e4L, size = 1L, prob = .3))

# Binomial distribution
dbinom(x = 6L, size = 10L, prob = .5) # preview quiz
dbinom(x = 0:10, size = 10L, prob = .5) # R is vectorized
pbinom(q = 6L, size = 10L, prob = .5) # preview quiz; P(X <= 6)
pbinom(q = 6L, size = 10L, prob = .5, lower.tail = FALSE) # P(X > 6)
# for discrete random variables, p* function will include `q` when \code{lower.tail = TRUE}
# and will not inlude `q` when \code{lower.tail = FALSE}

# Preview Quiz: why these two has same return?
dbinom(x = 0L, size = 10L, prob = .5)
pbinom(q = 0L, size = 10L, prob = .5)

bar_binom(size = 10L, prob = .3) # preview quiz
bar_binom(size = 10L, prob = .7) # preview quiz

# Page 99, Example 4.3.1
dbinom(x = 3L, size = 5L, prob = .858)
# Page 103, Example 4.3.2
dbinom(x = 4L, size = 10L, prob = .14)

# Page 103, Example 4.3.3
(pL = pbinom(q = 5L, size = 25L, prob = .1, lower.tail = TRUE)) # (a) including!
(pU = pbinom(q = 5L, size = 25L, prob = .1, lower.tail = FALSE)) # (b) excluding!
pL + pU # R makes sure (lower.tail = TRUE) and (lower.tail = FALSE) add up to 1
(ds = dbinom(x = 0:25, size = 25L, prob = .1))
bar_binom(size = 25L, prob = .1)
range(pL - sum(ds[1:6])) # sum `ds` from 0 to 5
range(pU - sum(ds[7:26])) # sum `ds` from 6 to 25
# https://en.wikipedia.org/wiki/Scientific_notation

# Page 103, Example 4.3.4
dbinom(x = 7L, size = 12L, prob = .55)
pbinom(q = 5L, size = 12L, prob = .55)
pbinom(q = 7L, size = 12L, prob = .55, lower.tail = FALSE)






# Poisson distribution
# Page 110, Example 4.4.1
dpois(x = 3L, lambda = 12) 
# Page 110, Example 4.4.2
ppois(2L, lambda = 12, lower.tail = FALSE)
# Page 110, Example 4.4.3
ppois(3L, lambda = 2)
# Page 111, Example 4.4.4
dpois(3L, lambda = 2)
# Page 112, Example 4.4.5
ppois(5L, lambda = 2, lower.tail = FALSE)

# Football example
# If X ~ Poisson(lambda = 3.2), then P(X <= 1) is
ppois(1L, lambda = 3.2)
# If X ~ Bin(n = 11, p = 3.2/11), then P(X <= 1) is
pbinom(1L, size = 11L, prob = 3.2/11)

# Relationship between Binomial & Poisson
# If X1 ~ Bin(n = 10, p = .6), then P(X1 = 4) is
dbinom(4L, size = 10L, prob = .6)
# If X2 ~ Bin(n = 20, p = .3), then P(X2 = 4) is
dbinom(4L, size = 20L, prob = .3)
# If X3 ~ Bin(n = 30, p = .2), then P(X3 = 4) is
dbinom(4L, size = 30L, prob = .2)
# If X4 ~ Poisson(lambda = 6), then P(X4 = 4) is
dpois(4L, lambda = 6)

par(mfrow = c(4,1))
bar_binom(size = 10L, prob = .6, xlim = 16L)
bar_binom(size = 20L, prob = .3, xlim = 16L)
bar_binom(size = 30L, prob = .2, xlim = 16L)
bar_pois(lambda = 6L, xlim = 16L)
par()







# Normal Distribution

pnorm(1.65)
qnorm(.95)
pnorm(1.96)
qnorm(.975)
qnorm(.05)
qnorm(.025)
pnorm(qnorm(.025, lower.tail = FALSE)) - pnorm(qnorm(.025))

# Page 119. Example 4.6.1
pnorm(2)
# Page 120. Example 4.6.2
pnorm(2.55) - pnorm(-2.55)
1 - 2* pnorm(-2.55)
# Page 121. Example 4.6.3
pnorm(1.53) - pnorm(-2.74)
# Page 121. Example 4.6.4
pnorm(2.71, lower.tail = FALSE)
# Page 122. Example 4.6.5
pnorm(2.45) - pnorm(.84)

# Page 122. Example 4.7.1
pnorm(3, mean = 5.4, sd = 1.3)
# Page 125. Example 4.7.2
pnorm(649, mean = 491, sd = 119) - pnorm(292, mean = 491, sd = 119)
# Page 122. Example 4.7.3
1e4L * pnorm(8.5, mean = 5.4, sd = 1.3, lower.tail = FALSE)

# Preview Quiz
pnorm(1, lower.tail = FALSE)
pnorm(-1, lower.tail = FALSE)
pnorm(2.5, lower.tail = FALSE)
pnorm(-3)

# Page 132
# Exercise 27
qnorm(p = .0094, mean = 100, sd = 15) # (a)
qnorm(p = .1093, mean = 100, sd = 15, lower.tail = FALSE) # (b)
qnorm(p = .5 + .4778, mean = 100, sd = 15) # (c)
qnorm(p = 1-(1-.9660)/2, mean = 100, sd = 15) # (d)
qnorm(p = (1-.9660)/2, mean = 100, sd = 15, lower.tail = FALSE) # (d), easier to understand
# Exercise 28
# X ~ N(mu, 10); P(X <= 40) = .0080
# P((X-mu)/10 <= (40-mu)/10) = P(Z <= (40-mu)/10) = P(Z <= qnorm(.008))
# (40-mu)/10 = qnorm(.008); solve for mu
(ans_28 = 40 - qnorm(.008) * 10)
pnorm(40, mean = ans_28, sd = 10) # = .008; verified
# Exercise 29
# X ~ N(mu, 15); P(X <= 50) = .9904
# P((X-mu)/15 <= (50-mu)/15) = P(Z <= ((50-mu)/15)) = P(Z <= qnorm(.9904)); solve for mu
(ans_29 = 50 - qnorm(.9904) * 15)
pnorm(50, mean = ans_29, sd = 15) # = .9904; verified
# Exercise 30
# (25-mu)/5 = qnorm(.0526, lower.tail = FALSE); solve for mu
(ans_30 = 25 - qnorm(.0526, lower.tail = FALSE) * 5)
pnorm(25, mean = ans_30, sd = 5, lower.tail = FALSE) # = .0526; verified
# Exercise 31
# X ~ N(25, sigma); P(X <= 10) = .0778
# P((X-25)/sigma <= (10-25)/sigma) = P(Z <= qnorm(.0778))
# (10-25)/sigma = qnorm(.0778); solve for sigma
(ans_31 = (10 - 25) / qnorm(.0778))
pnorm(10, mean = 25, sd = ans_31) # = .0778; verified
# Exercise 32
# (50-30)/sigma = qnorm(.9772); solve for sigma
(ans_32 = (50 - 30) / qnorm(.9772))
pnorm(50, mean = 30, sd = ans_32) # = .9772; verified


# if x satisfies x + 1 = 2, then solve for x?  Answer: x = 2-1 = 1


# t-score
qt(p = .975, df = 5L)
qt(p = .975, df = 10L)
qt(p = .975, df = 100L)
qnorm(p = .975)


# why teacher does not introduce q functions for discrete distribution?
pbinom(2:4, size = 10L, prob = .5) # cumulative probabilities for cutoff values of 2:4
(p_try = seq(from = .05, to = .20, by = .05))
qbinom(p_try, size = 10L, prob = .5) # minimum cutoff values with cumulative probabilities >= than the specified cumulative probability


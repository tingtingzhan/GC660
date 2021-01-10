
# Page 605, Example 12.3.1
xo_1231 = c(1L, 3L, 8L, 18L, 6L, 4L, 4L, 3L) # observed
mean_1231 = 198.67 # given in this example
sd_1231 = 41.31 # given in this example
p0 = pnorm(q = seq(from = 125, to = 275, by = 25), mean = mean_1231, sd = sd_1231)

(p = c(p0[1L], # 1st element of p0
       diff(p0), 
       pnorm(q = 275, mean = mean_1231, sd = sd_1231, lower.tail = FALSE)))
# textbook grouped all observations greater than 275 as the last category

(xe_1231 = sum(xo_1231) * p) # expected
(chi_1231 = sum((xo_1231 - xe_1231)^2 / xe_1231))
pchisq(chi_1231, df = length(xo_1231) - 3L, lower.tail = FALSE)
# -3L: three restrictions (explained on Page 608)
# (1) making sum(xo) == sum(xe)
# (2) estimating mean_1231
# (3) estimating sd_1231



# Page 609, Example 12.3.2
ns_1232 = 0:10
freq_1232 = c(5L, 6L, 8L, 10L, 10L, 15L, 17L, 10L, 10L, 9L, 0L)
(xo_1232 = c(sum(freq_1232[1:2]), # sum of the first two elements of 'freq_1232'
             freq_1232[-(1:2)])) # the elements of 'freq_1232' *excluding* the first two

p_1232 = sum(ns_1232 * freq_1232) / (100 * 25)
p0 = dbinom(2:9, size = 25L, prob = p_1232)
(p = c(pbinom(1L, size = 25L, prob = p_1232),
       p0,
       pbinom(9, size = 25L, prob = p_1232, lower.tail = FALSE)))
(xe_1232 = sum(xo_1232) * p)      
(chi_1232 = sum((xo_1232 - xe_1232)^2 / xe_1232))
pchisq(chi_1232, df = length(xo_1232) - 2L, lower.tail = FALSE)
# -2L: two restrictions (explained on Page 611)
# (1) making sum(xo) == sum(xe)
# (2) estimating p_1232




# Page 611, Example 12.3.3
ns_1233 = 0:10
freq_1233 = c(5L, 14L, 15L, 23L, 16L, 9L, 3L, 3L, 1L, 1L, 0L)
(xo_1233 = c(freq_1233[1:8], sum(freq_1233[-(1:8)])))
lb_1233 = 3
(p = c(dpois(0:7, lambda = lb_1233),
       ppois(7L, lambda = lb_1233, lower.tail = FALSE)))
(xe_1233 = sum(xo_1233) * p)      
(chi_1233 = sum((xo_1233 - xe_1233)^2 / xe_1233))
pchisq(chi_1233, df = length(xo_1233) - 1L, lower.tail = FALSE)
# -1L: one restrictions
# (1) making sum(xo) == sum(xe)




# Page 614, Example 12.3.4
xo_1234 = c(62L, 84L, 17L, 16L, 21L)
xe_1234 = rep(mean(xo_1234), times = length(xo_1234))
(chi_1234 = sum((xo_1234 - xe_1234)^2 / xe_1234))
pchisq(chi_1234, df = length(xo_1234) - 1L, lower.tail = FALSE)



# Page 616, Example 12.3.5
xo_1235 = c(43L, 125L, 32L)
(xe_1235 = sum(xo_1235) * c(1,2,1) / sum(c(1,2,1)))
(chi_1235 = sum((xo_1235 - xe_1235)^2 / xe_1235))
qchisq(.95, df = length(xo_1235) - 1L)
pchisq(chi_1235, df = length(xo_1235) - 1L, lower.tail = FALSE)


# Page 621, Example 12.4.1
(tab_1241 = array(c(260L, 15L, 7L, 299L, 41L, 14L), dim = c(3L, 2L), dimnames = list(
   Race = c('White', 'Black', 'Other'),
   FolicAcid = c('TRUE', 'FALSE')
)))
source('shared_functions.R')
prob_from_table(tab_1241)
chisq.test(tab_1241)



# Page 626, Example 12.4.2
tab_1242 = array(c(131L, 14L, 52L, 36L), dim = c(2L, 2L), dimnames = list(
   Type = c('Faller', 'Non-Faller'),
   LifestyleChange = c('TRUE', 'FALSE')
))
prob_from_table(tab_1242)
chisq.test(tab_1242, correct = FALSE)
chisq.test(tab_1242, correct = TRUE)




# Page 631, Example 12.5.1
tab_1251 = array(c(21L, 19L, 75L, 77L), dim = c(2L, 2L), dimnames = list(
   Group = c('Narcoleptic', 'Healthy'),
   Migraine = c('TRUE', 'FALSE')
))
prob_from_table(tab_1251)
(chisq_1251 = chisq.test(tab_1251, correct = FALSE))
# using test on two proportions
prop2_ztest(x1 = 21L, n1 = 96L, x2 = 19L, n2 = 96L)
unname(0.355^2 - chisq_1251$statistic) # only true for 2*2 contingency table




# Page 638, Example 12.6.2
tab_1262 = array(c(2L, 8L, 7L, 4L), dim = c(2L, 2L), dimnames = list(
   Group = c('PI_Naive', 'PA_Experienced'),
   Regimen2yr = c('TRUE', 'FALSE')
))
prob_from_table(tab_1262)
fisher.test(tab_1262)




# Page 644, Example 12.7.1
tab_1271 = array(c(22L, 18L, 216L, 199L), dim = c(2L, 2L), dimnames = list(
   Exercising = c('Extreme', 'No'),
   PretermLabor = c('TRUE', 'FALSE')
))
prob_from_table(tab_1271)   
RR_OR(tab_1271)



# Page 647, Example 12.7.2
tab_1272 = array(c(64L, 68L, 342L, 3496L), dim = c(2L, 2L), dimnames = list(
   SmkPregnancy = c('TRUE', 'FALSE'),
   Obesity = c('TRUE', 'FALSE')
))
prob_from_table(tab_1272)   
RR_OR(tab_1272) # match textbook narative, different from SAS output



# Page 650, Example 12.7.3
tab_1273 = array(c(21L, 16L, 11L, 6L, 50L, 18L, 14L, 6L), dim = c(2L, 2L, 2L), dimnames = list(
   HTN = c('TRUE', 'FALSE'),
   OCAD = c('TRUE', 'FALSE'),
   AgeGroup = c('<=55', '>55')
))
addmargins(tab_1273)
?mantelhaen.test
mantelhaen.test(tab_1273) # chisq test statistic & common odds ratio (95% CI)
# Page 652, Example 12.7.4
# calculate common odds ratio by hand
commonOR_MH(tab_1273)


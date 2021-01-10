
source('shared_functions.R')

# Page 190, Example 6.7.1
(n_671 = uniroot(f = function(n, sd, level = .95) {
  qnorm(1-(1-level)/2) * sd/sqrt(n) - 5
}, interval = c(0, 1e3), sd = 20)$root)
ceiling(n_671)


# Page 192, Example 6.8.1
(n_681 = uniroot(f = function(n, p, level = .95) {
  qnorm(1-(1-level)/2) * sqrt(p*(1-p)/n) - .05
}, interval = c(0, 1e3), p = .35)$root)
ceiling(n_681)


# Page 272, Example 7.9.1
# Table 7.9.1 (on Page 275)
power_z(mean1 = seq(from = 16, to = 19, by = .5), mean0 = 17.5, sd = 3.6, n = 100L, alternative = 'two.sided')
# Figure 7.9.2
curve(expr = power_z(x, mean0 = 17.5, sd = 3.6, n = 100L, alternative = 'two.sided'), from = 15, to = 20, 
      ylab = 'Power curve for Example 7.9.1.', xlab = 'Alternative values of \u03bc')


# Page 276, Example 7.9.2
power_z(mean1 = c(50, 55, 60, 65), mean0 = 65, sd = 15, n = 20L, sig.level = .01, alternative = 'less')
# Figure 7.9.4
curve(expr = power_z(x, mean0 = 65, sd = 15, n = 20L, sig.level = .01, alternative = 'less'), from = 49, to = 66, 
      ylab = 'Power curve for Example 7.9.2.', xlab = 'Alternative values of \u03bc')

# for testing H0: mu <= 65 vs. Ha: mu > 65
power_z(mean1 = c(65, 70, 75, 80), mean0 = 65, sd = 15, n = 20L, sig.level = .01, alternative = 'greater')
curve(expr = power_z(x, mean0 = 65, sd = 15, n = 20L, sig.level = .01, alternative = 'greater'), from = 63, to = 80, 
      ylab = 'Power curve for Example 7.9.2(a) ', xlab = 'Alternative values of \u03bc')



# Page 278, Example 7.10.1
power_z(mean1 = 55, mean0 = 65, sd = 15, n = 36L, sig.level = .01, alternative = 'less') # textbook results
power_z(mean1 = 55, mean0 = 65, sd = 15, n = 35L, sig.level = .01, alternative = 'less')
# Preview quiz: why \code{stats::power.t.test} gives different answer?
power.t.test(delta = abs(55-65), sd = 15, sig.level = .01, power = .95, 
             type = 'one.sample', alternative = 'one.sided')

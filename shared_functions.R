

# chapter 1
sampleRow <- function(x, size, replace = FALSE, prob = NULL) {
  if (!is.data.frame(x)) stop('input must be data.frame')
  nr <- nrow(x)
  x[sample.int(nrow(x), size = size, replace = replace, prob = prob), ]
}


# chapter 2


coef_variation <- function(x, na.rm = TRUE) {
  # `na.rm` is R conventional nomenclature of 'removing missing elements or not'
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}

simple_stats <- function(x, na.rm = TRUE) {
  nm <- deparse(substitute(x))
  cat('\nSummary Statistics of', nm, '\n\n')
  cat('mean =', mean(x, na.rm = na.rm), '\n')
  cat('median =', median(x, na.rm = na.rm), '\n')
  cat('standard deviation =', sd(x, na.rm = na.rm), '\n')
  cat('IQR =', diff(quantile(x, probs = c(.25, .75), na.rm = na.rm)), '\n')
  cat('range =', paste(range(x, na.rm = na.rm), collapse = ' ~ '), '\n')
  cat('skewness =', e1071::skewness(x, na.rm = na.rm), '\n')
  #cat('kurtosis =', e1071::kurtosis(x, na.rm = na.rm), '\n')
  cat('\n')
  plot(hist(x), main = paste('Histogram of', nm))
}

boxplot_named <- function(x) {
  boxplot(x, main = paste('Boxplot of', deparse(substitute(x))))
}


# chapter 3

prob_from_table <- function(x) {
  dnm <- dimnames(x)
  if (length(dnm) != 2L) stop('Currently only deals with 2-way table')

  cat('\n==== Joint & Marginal probabilities ====\n\n')
  y_joint <- x_m <- addmargins(x)
  y_joint[] <- paste0(x_m, ' (', sprintf(fmt = '%.1f%%', x_m / sum(x) * 100), ')')
  print(y_joint, quote = FALSE, right = TRUE)
  
  cat('\n==== Conditional probabilities (on ', names(dnm)[1L], ') ====\n\n', sep = '')
  y_cond1 <- x_m1 <- addmargins(x, margin = 2L)
  y_cond1[] <- paste0(x_m1, ' (', sprintf(fmt = '%.1f%%', x_m1 / rowSums(x) * 100), ')')
  print(y_cond1, quote = FALSE, right = TRUE)
  
  cat('\n==== Conditional probabilities (on ', names(dnm)[2L], ') ====\n\n', sep = '')
  y_cond2 <- x_m2 <- addmargins(x, margin = 1L)
  y_cond2[] <- paste0(x_m2, ' (', sprintf(fmt = '%.1f%%', t(t(x_m2) / colSums(x)) * 100), ')')
  print(y_cond2, quote = FALSE, right = TRUE)
  
  cat('\n')
  return(invisible())
}



predictive_value <- function(
  x, # 2*2 Test-Disease table
  sens = x[1,1]/sum(x[,1]), 
  spec = x[2,2]/sum(x[,2]), 
  prevalence
) {
  if (!missing(x)) {
    if (!is.matrix(x) || any(dim(x) != 2L)) stop('Test-Disease table must be 2*2')
    if (length(dnm <- dimnames(x)) != 2L) stop('must provide complete dimension names')
    if (!length(nm_dnm <- names(dnm)) || any(!nzchar(nm_dnm))) stop('dimension names must have names')
    message('Confirm that your data setup satisfies that')
    message('{', nm_dnm[1L], ' = ', dnm[[1L]][1L], '} is test-positive')
    message('{', nm_dnm[2L], ' = ', dnm[[2L]][1L], '} is disease-positive')
    tmp <- readline(prompt = 'Press Enter to confirm, or any other key to abort: ')
    if (nzchar(tmp)) stop('Aborted by user')
  }
  
  cat('Sensitivity is', sprintf(fmt = '%.1f%%', 100*sens), '\n')
  cat('Specificity is', sprintf(fmt = '%.1f%%', 100*spec), '\n')
  out <- c(Sensitivity = sens, Specificity = spec)
  if (!missing(prevalence)) {
    pvp <- (sens * prevalence) / (sens * prevalence + (1-spec) * (1-prevalence))
    pvn <- (spec * (1-prevalence)) / (spec * (1-prevalence) + (1-sens) * prevalence)
    cat('Predictive value positive (PVP) is', sprintf(fmt = '%.3f%%', 100*pvp), '\n')
    cat('Predictive value negative (PVN) is', sprintf(fmt = '%.3f%%', 100*pvn), '\n')
    out <- c(out, pvp = pvp, pvn = pvn)
  }
  return(invisible(out))
}




# chapter 4

bar_binom <- function(size, prob, xlim = size) {
  if (!is.integer(size) || length(size) != 1L) stop('size must be len-1 integer')
  if (!is.numeric(prob) || length(prob) != 1L || is.na(prob) || prob < 0 || prob > 1) stop('prob must be between 0,1')
  if (!is.integer(xlim)) stop('xlim must be integer')
  nl <- length(xlim)
  xl <- if (nl == 1L) 0:xlim else if (nl == 2L) xlim[1L]:xlim[2L] else stop('illegal xlim')
  pr <- dbinom(setNames(nm = xl), size = size, prob = prob)
  barplot(pr, main = paste0('Binomial Distribution: n = ', size, '; p = ', prob), ylab = 'Prob Density')
}

bar_pois <- function(lambda, xlim = size) {
  if (!is.numeric(lambda) || length(lambda) != 1L || is.na(lambda) || lambda < 0) stop('lambda must be len-1 positive numeric')
  if (!is.integer(xlim)) stop('xlim must be integer')
  nl <- length(xlim)
  xl <- if (nl == 1L) 0:xlim else if (nl == 2L) xlim[1L]:xlim[2L] else stop('illegal xlim')
  pr <- dpois(setNames(nm = xl), lambda = lambda)
  barplot(pr, main = paste0('Poisson Distribution: lambda = ', lambda), ylab = 'Prob Density')
}




# chapter 5 & 6

.z2 <- function(level = .95) { # two-sided z-quantile
  if (length(level) != 1L || !is.double(level) || anyNA(level) || any(level < 0, level > 1)) stop('level must be len-1 between 0 and 1')
  z <- qnorm((1-level)/2, lower.tail = FALSE)
  cat('Reliability factor:', sprintf(fmt = '%.3f', z), '\n')
  return(invisible(z))
}

.t2 <- function(level = .95, ...) { # two-sided t-quantile
  if (length(level) != 1L || !is.double(level) || anyNA(level) || any(level < 0, level > 1)) stop('level must be len-1 between 0 and 1')
  tstat <- qt((1-level)/2, ..., lower.tail = FALSE)
  cat('Reliability factor:', sprintf(fmt = '%.3f', tstat), '\n')
  return(invisible(tstat))
}

.chisq2 <- function(level = .95, ...) {
  if (length(level) != 1L || !is.double(level) || anyNA(level) || any(level < 0, level > 1)) stop('level must be len-1 between 0 and 1')
  p_left <- (1-level)/2
  qchisq(c(p_left, 1-p_left), ..., lower.tail = TRUE)
}

.f2 <- function(level = .95, ...) {
  if (length(level) != 1L || !is.double(level) || anyNA(level) || any(level < 0, level > 1)) stop('level must be len-1 between 0 and 1')
  p_left <- (1-level)/2
  qf(c(p_left, 1-p_left), ..., lower.tail = TRUE)
}



sample_pnorm <- function(q, mean = 0, sd = 1, n = stop('must provide sample size'), lower.tail = TRUE) {
  if (!is.integer(n) || length(n) != 1L || n <= 0L) stop('n must be len-1 positive integer')
  pnorm(q, mean = mean, sd = sd/sqrt(n), lower.tail = lower.tail)
}



CI_z <- function(
  x, x0 = x[!is.na(x)], 
  xbar = base::mean(x0), 
  sd = stop('must provide population standard deviation'), 
  n = length(x0), 
  level = .95
) {
  xbar + c(-1, 1) * .z2(level) * sd/sqrt(n)
}


# sample2_pnorm(xbar1 = 3102, xbar2 = 2542, sd...)
# sample2_pnorm(q = (3102-2542), sd...)

sample2_pnorm <- function(
  xbar_diff = xbar1 - xbar2, xbar1 = stop('Input sample mean for group 1'), xbar2 = stop('Input sample mean for group 2'),
  mean_diff = mean1 - mean2, mean1 = 0, mean2 = 0, 
  sd1 = 1, sd2 = 1, 
  n1 = stop('must provide 1st sample size n1'),
  n2 = stop('must provide 2nd sample size n2'),
  lower.tail = TRUE
  ) {
  pnorm(q = xbar_diff, mean = mean_diff, sd = sqrt(sd1^2/n1 + sd2^2/n2), lower.tail = lower.tail)
}

CI_2z <- function(
  x1, x10 = x1[!is.na(x1)],
  x2, x20 = x2[!is.na(x2)],
  xbar1 = base::mean(x10), 
  xbar2 = base::mean(x20), 
  sd1 = stop('must provide population 1 standard deviation'), 
  sd2 = stop('must provide population 2 standard deviation'), 
  n1 = length(x10), n2 = length(x20), 
  level = .95
) {
  (xbar1 - xbar2) + c(-1, 1) * .z2(level) * sqrt(sd1^2/n1 + sd2^2/n2)
}


sample_pt <- function(q, mean = 0, sd = 1, n = stop('must provide sample size'), lower.tail = TRUE) {
  if (!is.integer(n) || length(n) != 1L || n <= 1L) stop('n must be len-1, >1 integer')
  pt((q - mean)/(sd/sqrt(n)), df = n - 1L, lower.tail = lower.tail)
}

CI_t <- function(
  x, x0 = x[!is.na(x)],
  xbar = base::mean(x0), 
  xsd = stats::sd(x0), 
  n = length(x0), 
  level = .95
) {
  xbar + c(-1, 1) * .t2(level = level, df = n - 1L) * xsd / sqrt(n)
}


CI_2t_pooled <- function(
  x1, x10 = x1[!is.na(x1)],
  x2, x20 = x2[!is.na(x2)],
  xbar1 = base::mean(x10), xbar2 = base::mean(x20), 
  sd1 = base::sd(x10), sd2 = base::sd(x20),
  sd_pooled = sqrt(((n1-1L)*sd1^2 + (n2-1L)*sd2^2)/(n1+n2-2L)),
  n1 = length(x10), n2 = length(x20),
  level = .95
) {
  (xbar1 - xbar2) + c(-1, 1) * .t2(level = level, df = n1 + n2 - 2L) * sd_pooled * sqrt(1/n1 + 1/n2)
}



sample_p_pnorm <- function(
  x, # integer, number of positive count
  n, # integer, total sample size
  phat = x/n, 
  p = stop('must provide population proportion'), 
  lower.tail = TRUE
) {
  pnorm(phat, mean = p, sd = sqrt(p*(1-p)/n), lower.tail = lower.tail)
}


CI_CLT_p <- function(
  x, # integer, number of positive count
  n, # integer, number of positive count
  phat = x/n, 
  level = .95
) {
  phat + c(-1, 1) * .z2(level) * sqrt(phat*(1-phat)/n)
}





CI_chisq <- function(x, x0 = x[!is.na(x)], xsd = stats::sd(x0), n = length(x0), level = .95) {
  out <- sort(xsd * sqrt((n - 1L) / .chisq2(level = level, df = n - 1L)))
  cat(out[1L], '< \u03c3 <', out[2L])
  return(invisible(out))
}


CI_F <- function(
  x1, x2, x10 = x1[!is.na(x1)], x20 = x2[!is.na(x2)],
  sd1 = sd(x10), sd2 = sd(x20),
  n1 = length(x10), n2 = length(x20),
  level = .95
  ) {
  out <- sort((sd1/sd2)^2 / .f2(level = level, df1 = n1 - 1L, df2 = n2 - 1L))
  cat(out[1L], '< (\u03c3\u2081/\u03c3\u2082)\u00B2 <', out[2L])
  return(invisible(out))
}




.ztest <- function(z, alternative = 'two.sided') {
  z_txt <- sprintf(z, fmt = '%.3g')
  switch(alternative,
         two.sided = {
           pval <- 2 * pnorm(abs(z), lower.tail = FALSE)
           txt <- paste0('\u2265 |', z_txt, '|')
         },
         greater = {
           pval <- pnorm(z, lower.tail = FALSE)
           txt <- paste0('> ', z_txt)
         },
         less = {
           pval <- pnorm(z, lower.tail = TRUE)
           txt <- paste0('< ', z_txt)
         }, stop('illegal alternative'))
  cat('Pr(Z ', txt, ') = ', pval, '\n', sep = '')
  return(invisible(pval))
}

prop_ztest <- function(
  x, # integer, number of positive count
  n, # integer, total sample size
  phat = x/n, 
  p = stop('must provide population proportion'), 
  alternative = 'two.sided'
) {
  zx <- (phat - p)/sqrt(p*(1-p)/n)
  return(.ztest(zx, alternative = alternative))
}

prop2_ztest <- function(
  obs1, obs10 = obs1[!is.na(obs1)], x1 = sum(obs10), n1 = length(obs10), phat1 = x1/n1, p1,
  obs2, obs20 = obs2[!is.na(obs2)], x2 = sum(obs20), n2 = length(obs20), phat2 = x2/n2, p2,
  p_diff = if (!missing(p1) && !missing(p2)) p1 - p2 else 0,
  alternative = 'two.sided'
) {
  zx <- if (p_diff == 0) {
    p_pooled <- (x1+x2)/(n1+n2)
    (phat1 - phat2) / sqrt(p_pooled * (1-p_pooled) * (1/n1 + 1/n2))
  } else {
    ((phat1 - phat2) - p_diff) / sqrt(phat1*(1-phat1)/n1 + phat2*(1-phat2)/n2)
  }
  return(.ztest(zx, alternative = alternative))
}


sample2_p_pnorm <- function(
  x1, n1, phat1 = x1/n1,
  x2, n2, phat2 = x2/n2,
  p1, p2,
  phat_diff = phat1 - phat2,
  lower.tail = TRUE
) {
  pnorm(phat_diff, mean = p1-p2, sd = sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2), lower.tail = lower.tail)
}
CI_CLT_2p <- function(
  x1, n1, phat1 = x1/n1,
  x2, n2, phat2 = x2/n2,
  level = .95
) {
  (phat1 - phat2) + c(-1, 1) * .z2(level) * sqrt(phat1*(1-phat1)/n1 + phat2*(1-phat2)/n2)
}
  

# Chapter 7

power_z <- function(
  mean1, # parameter in the alternative hypothesis
  mean0, # parameter in the null hypothesis
  sd, # population standard deviation
  n, # sample size
  alternative = c('two.sided', 'less', 'greater'),
  sig.level = .05
) {
  # `mean1` is vectorized
  alternative <- match.arg(alternative)
  switch(alternative, 'two.sided' = {
    # construct 2-sided rejection region
    rr <- CI_z(xbar = mean0, sd = sd, n = n, level = 1 - sig.level) # using \code{CI_z} as a surrogate of non-rejection region
    cat('Two-sided Rejection Region: (-\u221E, ', sprintf('%.3f', rr[1L]), ') \u222A (', sprintf('%.3f', rr[2L]), ', \u221E)\n', sep = '')
    # Question in class: why `rr` is rejection region?
    return(sample_pnorm(q = rr[1L], mean = mean1, sd = sd, n = n, lower.tail = TRUE) +
             sample_pnorm(q = rr[2L], mean = mean1, sd = sd, n = n, lower.tail = FALSE))
  }, 'less' = {
    upr <- qnorm(p = sig.level, mean = mean0, sd = sd/sqrt(n)) # upper-bound of 1-sided rejection region 
    cat('One-sided Rejection Region: (-\u221E, ', sprintf('%.3f', upr), ')\n', sep = '')
    return(sample_pnorm(q = upr, mean = mean1, sd = sd, n = n, lower.tail = TRUE)) 
  }, 'greater' = {
    lwr <- qnorm(p = sig.level, mean = mean0, sd = sd/sqrt(n), lower.tail = FALSE) # lower-bound of 1-sided rejection region 
    cat('One-sided Rejection Region: (', sprintf('%.3f', lwr), ', \u221E)\n', sep = '')
    return(sample_pnorm(q = lwr, mean = mean1, sd = sd, n = n, lower.tail = FALSE)) 
  })
}




# Chapter 12

RR_OR <- function(tab, level = .95) {
  if (!is.matrix(tab) || !is.integer(tab) || !all(dim(tab) == 2L) || anyNA(tab) || any(tab <= 0L)) stop('tab must be 2*2 integer matrix')
  dnm <- dimnames(tab)
  dimnm <- names(dnm)
  if (length(dimnm) == 2L && !all(nzchar(dimnm))) stop('name of dimensions must be specified')
  if (is.null(rnm <- dnm[[1L]]) || is.null(cnm <- dnm[[2L]])) stop('tab must have colnames and rownames')
  cat('Risk Factors:', paste0(dimnm[1L], ' = ', rnm, '(', c('+', '-'), ')', collapse = '; '), '\n')
  cat('Disease Status:', paste0(dimnm[2L], ' = ', cnm, '(', c('+', '-'), ')', collapse = '; '), '\n')
  rS <- rowSums(tab)
  risk <- tab[,1L] / rS
  odd <- tab[,1L] / tab[,2L] 
  chisq <- sum(tab) * (tab[1L,1L]*tab[2L,2L] - tab[2L,1L]*tab[1L,2L])^2 / (prod(rS) * prod(colSums(tab)))
  rr <- unname(risk[1L] / risk[2L])
  or <- unname(odd[1L] / odd[2L])
  
  # Equation (12.7.2) (Page 644), Equation (12.7.4) (Page 646), both should be z_(1-\alpha/2)
  pwr_ci <- 1 + c(-1, 1) * .z2(level) / sqrt(chisq)
  
  rr_ci <- rr^pwr_ci
  or_ci <- or^pwr_ci
  list(rr = rr, or = or, rr_ci = rr_ci, or_ci = or_ci)
}




commonOR_MH <- function(x) {
  dm <- dim(x)
  if (length(dm) != 3L || !(all(dm[1:2] == 2L))) stop('x must be 2*2*x shape of array')
  sliceSum <- vapply(seq_len(dm[3L]), FUN = function(i) sum(x[,,i]), FUN.VALUE = 0L)
  sum(x[1L,1L,]*x[2L,2L,]/sliceSum) / sum(x[2L,1L,]*x[1L,2L,]/sliceSum)
}

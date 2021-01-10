
library(car)
source('shared_functions.R')


# Page 417, Example 9.3.1
head(datA_931 <- read.csv('data/EXA_C09_S03_01.csv'))
dim(datA_931)
names(datA_931)[2:3] = c('Waist', 'AT')
head(datA_931)

plot(AT ~ Waist, data = datA_931)

cor(datA_931[2:3])
cor.test(~ AT + Waist, data = datA_931)

summary(mod_931 <- lm(AT ~ Waist, data = datA_931))
confint(mod_931)
anova(mod_931)
plot(AT ~ Waist, data = datA_931)
abline(a = mod_931$coefficients[1L], b = mod_931$coefficients[2L])

summary(mod_931_centered <- lm(AT ~ I(Waist - 90), data = datA_931))
plot(AT ~ I(Waist - 90), data = datA_931)
abline(reg = mod_931_centered)

head(datA_931_sorted <- datA_931[order(datA_931$Waist), ])
head(pred_931 <- predict(mod_931, newdata = datA_931_sorted, interval = 'predict'))
head(conf_931 <- predict(mod_931, newdata = datA_931_sorted, interval = 'confidence'))

plot(AT ~ Waist, data = datA_931)
abline(reg = mod_931)
matlines(x = datA_931_sorted$Waist, y = pred_931[,2:3], col = 2, lty = 2)
matlines(x = datA_931_sorted$Waist, y = conf_931[,2:3], col = 4, lty = 3)



# Page 447, Example 9.7.1
head(datA_971 <- read.csv('data/EXA_C09_S07_01.csv'))
dim(datA_971)

summary(mod_971 <- lm(CV ~ HEIGHT, data = datA_971))

# Page 452, Example 9.7.2
cor(do.call(cbind, datA_971))
cor.test(~ CV + HEIGHT, data = datA_971)





# Page 572, Example 11.4.1
datA_1141 = data.frame(
  OCAD = rep(c(TRUE, FALSE), times = c(107L, 41L)),
  Gender = c(rep(c('Male', 'Female'), times = c(92L, 15L)), 
             rep(c('Male', 'Female'), times = c(21L, 20L))) 
)
head(datA_1141)
prob_from_table(table(datA_1141))

qlogis(.5) # probability -> logit
plogis(qlogis(.5)) # logit -> probability

summary(mod_1141 <- glm(OCAD ~ Gender, family = binomial(link = 'logit'), data = datA_1141))
(ci_1141 = confint(mod_1141))
exp(mod_1141$coefficients[2L])
exp(ci_1141)

newd_1141 = data.frame(Gender = c('Male', 'Female'))
rownames(newd_1141) = c('A new male patient', 'A new female patient')
?`rownames<-` # https://en.wikipedia.org/wiki/Syntactic_sugar
?`row.names<-.data.frame`
?`.rowNamesDF<-`
(fit_1141 = predict(mod_1141, newdata = newd_1141)) # logit(Conditional Prob.)
plogis(fit_1141)




# Page 573, Example 11.4.2
head(datA_1142_raw <- read.csv('data/EXA_C11_S04_02.csv'))
datA_1142 = within(datA_1142_raw, expr = {
  ATT = as.logical(ATT)
})
summary(mod_1142 <- glm(ATT ~ AGE, family = binomial, data = datA_1142))
(ci_1142 = confint(mod_1142))
exp(mod_1142$coefficients[2L])
exp(ci_1142)
car::Anova(mod_1142) # Optional

head(datA_1142_sorted <- datA_1142[order(datA_1142$AGE), ])
class(pred_1142 <- predict(mod_1142, newdata = datA_1142_sorted, se.fit = TRUE)) # this is prediction interval
names(pred_1142)
head(exp_pred_1142 <- plogis(with(pred_1142, fit + qnorm(.975) * se.fit %*% t(c(-1, 0, 1)))))

plot(ATT ~ AGE, data = datA_1142)
matlines(x = datA_1142_sorted$AGE, y = exp_pred_1142, col = c(2, 1, 2), lty = c(2, 1, 2))

newd_1142 = data.frame(AGE = c(50, 65, 80))
rownames(newd_1142) = c('A new 50yr patient', 'A new 65yr patient', 'A new 80yr patient')
fit_1142 = predict(mod_1142, newdata = newd_1142)
plogis(fit_1142)

# Page 578, 11.4.4
library(DescTools)
PseudoR2(mod_1142, which = 'CoxSnell')
PseudoR2(mod_1142, which = 'Nagelkerke')


# Page 579, Example 11.4.5 (same as Example 11.4.4)





# Page 493, Example 10.3.1
head(datA_1031 <- read.csv('data/EXA_C10_S03_01.csv'))
scatterplotMatrix(~ AGE + EDLEVEL + CDA, data = datA_1031, smooth = FALSE, regLine = TRUE)

# Page 502, Example 10.4.1, 'Multiple R-squared' (not 'Adjusted R-squared')
summary(mod_1031 <- lm(CDA ~ AGE + EDLEVEL, data = datA_1031))
confint(mod_1031)

# Page 509, Example 10.5.1
(newd_1031 = data.frame(AGE = 68, EDLEVEL = 12))
predict(mod_1031, newdata = newd_1031, interval = 'prediction')
predict(mod_1031, newdata = newd_1031, interval = 'confidence')




# Page 511, Example 10.6.1
head(datA_1061 <- read.csv('data/EXA_C10_S06_01.csv'))
summary(mod_1061 <- lm(W ~ P + S, data = datA_1061))
confint(mod_1061)

# (optional) Page 515, Example 10.6.2
library(psych)
partial.r(datA_1061, x = 2:3, y = 1L)





# Page 545, Example 11.2.1
head(datA_1121_raw <- read.csv('data/EXA_C11_S02_01.csv'))
datA_1121 = within(datA_1121_raw, expr = {
  CASENO = factor(CASENO)
  SMOKE = as.logical(SMOKE)
})

scatterplot(GRAMS ~ WEEKS | SMOKE, data = datA_1121, regLine = FALSE, smooth = FALSE)

# main model (without interaction), as in textbook
summary(mod_1121_main <- lm(GRAMS ~ WEEKS + SMOKE, data = datA_1121))
confint(mod_1121_main)
(cf_main = mod_1121_main$coefficients)
scatterplot(GRAMS ~ WEEKS | SMOKE, data = datA_1121, regLine = FALSE, smooth = FALSE)
abline(a = cf_main[1L], b = cf_main[2L], col = 'blue')
abline(a = cf_main[1L] + cf_main[3L], b = cf_main[2L], col = 'magenta')

# model with interaction (over fit!)
summary(mod_1121_ita <- lm(GRAMS ~ WEEKS * SMOKE, data = datA_1121))
scatterplot(GRAMS ~ WEEKS | SMOKE, data = datA_1121, regLine = FALSE, smooth = FALSE)
(cf_ita = mod_1121_ita$coefficients)
# let the four elements of cf_ita be beta_0, beta_1, beta_2, beta_3
# add regression lines manually
abline(a = cf_ita[1L], b = cf_ita[2L], col = 'blue')
# Blue line for non-smokers: y = beta_0 + beta_1 * Week
abline(a = cf_ita[1L] + cf_ita[3L], b = cf_ita[2L] + cf_ita[4L], col = 'magenta')
# Magenta line for smokers: y = (beta_0 + beta_2) + (beta_1 + beta_3) * Week
# end of add regression lines manually
# equivalent to 
scatterplot(GRAMS ~ WEEKS | SMOKE, data = datA_1121, regLine = TRUE, smooth = FALSE)
# see ?car:::scatterplot.default

# how do I locate the colours?
?car::scatterplot
car::carPalette()[-1]





# Page 551, Example 11.2.3
head(datA_1123_raw <- read.csv('data/EXA_C11_S02_03.csv'))
datA_1123 = within(datA_1123_raw, expr = {
  METHOD = factor(METHOD, levels = c('C', 'A', 'B'))
})
scatterplot(EFFECT ~ AGE | METHOD, data = datA_1123, regLine = TRUE, smooth = FALSE)

summary(mod_1123 <- lm(EFFECT ~ AGE * METHOD, data = datA_1123))
confint(mod_1123)
(cf_1123 = mod_1123$coefficients)

scatterplot(EFFECT ~ AGE | METHOD, data = datA_1123, regLine = FALSE, smooth = FALSE)
abline(a = cf_1123[1L], b = cf_1123[2L], col = 'blue')
abline(a = cf_1123[1L] + cf_1123[3L], b = cf_1123[2L] + cf_1123[5L], col = 'magenta')
abline(a = cf_1123[1L] + cf_1123[4L], b = cf_1123[2L] + cf_1123[6L], col = 'cyan')



# Page 561, Example 11.3.1
head(datA_1131 <- read.csv('data/EXA_C11_S03_01.csv'))
names(datA_1131) = c('JOBPER', 'ASRV', 'ENTH', 'AMB', 'COMM', 'PROB', 'INIT')
head(datA_1131)

summary(mod_1131_raw <- lm(JOBPER ~ ASRV + ENTH + AMB + COMM + PROB + INIT, data = datA_1131))
library(MASS)
summary(mod_1131 <- stepAIC(mod_1131_raw, direction = 'backward'))




# Page 576, Example 11.4.3
head(datA_1143 <- read.csv('data/REV_C11_24.csv'))
dim(datA_1143)

summary(glm(ONSET ~ HIAA + TRYPT, family = binomial(link = 'logit'), data = datA_1143))
# This line of R code recreates Figure 11.4.4. 

summary(mod_1143 <- glm(ONSET ~ HIAA, family = binomial(link = 'logit'), data = datA_1143))
1 - exp(mod_1143$coefficients)
# Predictor TRYPT should be removed from model due to p-value \approx 1 
# For every one unit increase in HIAA, the odd of ONSET decrease by 1.33% (p = .014)







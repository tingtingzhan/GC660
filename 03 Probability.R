
source('shared_functions.R')

# Page 69, Example 3.4.1
(datA_341 = matrix(c(28L, 19L, 41L, 53L, 35L, 38L, 44L, 60L), ncol = 2L, 
                   dimnames = list(FamilyHx = c('none', 'Bipolar', 'Unipolar', 'UniBipolar'), 
                                   Onset = c('Early', 'Late'))))
class(datA_341)
addmargins(datA_341)
addmargins(datA_341, margin = 1L) # vector of dimensions over which to form margins.
addmargins(datA_341, margin = 2L)
rowSums(datA_341)
colSums(datA_341)

# joint, marginal & conditional probabilities
prob_from_table(datA_341)


# Page 81, Example 3.5.1
(datA_315 = matrix(c(436L, 14L, 5L, 495L), nrow = 2L, 
                   dimnames = list(Test = c('Positive', 'Negative'), 
                                   Alzheimer = c('Yes', 'No'))))
# textbook asks you to get specificity & sensitivity from this table,
# then get PVP/PVN using Bayes Theorem with prevalence
predictive_value(datA_315, prevalence = .113) # prevalence from textbook
# however if we calculate PVP and PVN naively from `datA_315`, we shall get
436/(436+5) # PVP
495/(14+495)# PVN

# our hypothetical examples
predictive_value(sens = .95, spec = .995, prevalence = .001)
predictive_value(sens = .95, spec = .995, prevalence = .95)


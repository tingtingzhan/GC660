

# PCKDATA
head(PCKDATA <- read.csv('data/LDS_C07_PCKDATA.csv'))
dim(PCKDATA)

shapiro.test(PCKDATA$A)
shapiro.test(PCKDATA$B)

# this data is organized in a poor manner
# According to the question, A and B should be two pyschological situations (in the same patient)
# Ideally we would prefer to have one column of endpoint and another column of predictor (i.e. situation)
# We use \code{reshape2::melt} to re-organize the data
head(PCK_melted <- reshape2::melt(PCKDATA, id.vars = 'SUBJ', value.name = 'PCK', variable.name = 'Group'))
dim(PCK_melted)

boxplot(PCK ~ Group, data = PCK_melted)
t.test(PCK ~ Group, data = PCK_melted, alternative = 'greater', paired = TRUE)
t.test(PCK ~ Group, data = PCK_melted, alternative = 'less', paired = TRUE)
t.test(PCKDATA$A, PCKDATA$B, alternative = 'greater', paired = TRUE)



# PROTHROM
# BAD way of organizing data!!!
head(PROTHROM_raw <- read.csv('data/LDS_C07_PROTHROM.csv', header = FALSE))
# `header = FALSE` means 'the first row of csv file is NOT the variable names'
dim(PROTHROM_raw)
PROTHROM_raw[[1L]]
class(PROTHROM_raw[[2L]])

PROTHROM0 = PROTHROM_raw[-c(1L, 502L), ]
names(PROTHROM0) = c('Subj', 'Time')
class(PROTHROM0$Time)
PROTHROM = within(PROTHROM0, expr = {
  Time = as.numeric(Time) # R previouly mistook Time as 'character' due to the poor data organization
  Group = rep(c('FULLSUB', 'PRESUB'), each = 500L) # add a column to correctly identify the groups
})
head(PROTHROM)

boxplot(Time ~ Group, data = PROTHROM)
t.test(Time ~ Group, data = PROTHROM)



# HEADCIRC
head(HEADCIRC <- read.csv('data/LDS_C07_HEADCIRC.csv'))
dim(HEADCIRC)

boxplot(HEADCIRC$SCA - HEADCIRC$NC, main = 'Difference of SCA - NC')
t.test(HEADCIRC$SCA, HEADCIRC$NC, alternative = 'less', paired = TRUE)



# HEMOGLOB
# BAD way of organizing data!!!
head(HEMOGLOB_raw <- read.csv('data/LDS_C07_HEMOGLOB.csv', header = FALSE))
dim(HEMOGLOB_raw)

HEMOGLOB0 = HEMOGLOB_raw[-c(1L, 502L), ]
names(HEMOGLOB0) = c('Subj', 'Hb')
HEMOGLOB = within(HEMOGLOB0, expr = {
  Hb = as.numeric(Hb)
  Group = structure(rep(1:2, each = 500L), levels = c('A', 'B'), class = 'factor')
})
head(HEMOGLOB)

boxplot(Hb ~ Group, data = HEMOGLOB)
t.test(Hb ~ Group, data = HEMOGLOB)




# MANDEXT
# BAD way of organizing data!!!
head(MANDEXT_raw <- read.csv('data/LDS_C07_MANDEXT.csv', header = FALSE))
dim(MANDEXT_raw)
MANDEXT_raw[[1L]]

MANDEXT = MANDEXT_raw[-c(1L, 502L), ]
names(MANDEXT) = c('Subj', 'SCORE')
MANDEXT = within(MANDEXT, expr = {
  SCORE = as.numeric(SCORE)
  Group = structure(rep(1:2, each = 500L), levels = c('LD', 'NORM'), class = 'factor')
})
head(MANDEXT)

# illustrating the use of \code{base::subset}
sLD = subset(MANDEXT, subset = (Group == 'LD')) # 'LD' means 'learning disabled'
dim(MANDEXT)
dim(sLD)
head(sLD)
# end of illustrating the use of \code{base::subset}

ids = c(sample(1:500, size = 10L, replace = FALSE), sample(501:1e3, size = 15L, replace = FALSE))
sp_MANDEXT = MANDEXT[ids, ]
dim(sp_MANDEXT)

boxplot(SCORE ~ Group, data = MANDEXT)
t.test(SCORE ~ Group, data = MANDEXT, alternative = 'less')

t.test(SCORE ~ Group, data = sp_MANDEXT, alternative = 'less')


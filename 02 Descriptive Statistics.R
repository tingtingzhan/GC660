

# Windows
# Important!!  Right click -> Extract all
# Ctrl + Enter: send a line to Console
# Ctrl + L: clear Console
# Ctrl + S: Save your R file *on the fly*
# 

# Mac
# Command + Enter: send a line to Console
# Ctrl + L: clear Console
# Command + S: Save your R file
# Command + Shift + N: Create new .R file (for exams, for example)

# Rearrange Panels: Tools -> Global Options -> Pane Layout

# RStudio Shortcuts (Windows & Mac)
# https://support.rstudio.com/hc/en-us/articles/200711853-Keyboard-Shortcuts


# Integrated Development Environment (IDE)



# Simple Random Sampling
# Example 1.4.1
d141 = read.csv('data/EXA_C01_S04_01.csv') # relative directory, only works if you set up R project correctly
d141 = read.csv('~/Dropbox/Document/Papers/GC660/GC660 R Lab/data/EXA_C01_S04_01.csv') # absolute directory
?read.csv # R 'function'
class(d141)
dim(d141)
nrow(d141)
ncol(d141)
head(d141, n = 8L)
tail(d141)
names(d141) # faster
colnames(d141)
d141$SUBJ
d141$AGE

if (FALSE) { # optional
  class(1)
  class(1L)
  class(TRUE)
  class(FALSE)
  # class(false) # gives you error
  
  # two ways of defining variable
  x = 1 # assigning (casual)
  y <- 1 # assigning (stringent)
  x == 2 # equal
}

if (FALSE) { # textbook solution
  subject <- seq(1,189,1)
  sample(subject,10)
}

# our solution
source('shared_functions.R')
sampleRow(d141, size = 10L, replace = FALSE)




# Optional: Example 1.4.2 (systematic sample)
d141[seq.int(from = 4L, to = 166L, by = 18L), ]



# Example 2.2.1
class(d141$AGE)
sort(d141$AGE) # in R, we call this 'ordered vector' (not 'ordered array')


# Example 2.3.1
age = d141$AGE
class(age)
(ageC = cut(age, breaks = c(30,40,50,60,70,80,90), include.lowest = TRUE, right = FALSE)) # `include.lowest` actually means 'include.highest'
class(ageC) # 'factor', i.e. unordered categorical variable
print.default(ageC)
table(ageC) # tabulate
sprintf(fmt = '%.1f%%', 100 * table(ageC)/length(ageC)) # relative frequencies
sprintf(fmt = '%.1f%%', 100 * cumsum(table(ageC)/length(ageC))) # cumulative relative frequencies
if (FALSE) { # optional: understand `include.lowest` and `right`
  table(cut(age, breaks = c(30,40,50,60,70,80,90), include.lowest = FALSE, right = FALSE))
  table(cut(age, breaks = c(30,40,50,60,70,80,90), include.lowest = TRUE, right = TRUE))
  table(cut(age, breaks = c(30,40,50,60,70,80,90), include.lowest = FALSE, right = TRUE))
}
hist(age)



# Example 2.4.1
mean(age)


# Example 2.4.2
(d242 = c(88.9, 97.1, 79.0, 92.0, 83.9, 81.2, 86.2, 89.8)/100)
sprintf(fmt = '%.1f%%', 100 * mean(d242))



# Example 2.4.3-2.4.6
median(age)
sprintf(fmt = '%.1f%%', 100 * median(d242))
if (FALSE) { # correct but may be confusing
  library(pracma)
  Mode(age)
}
pracma::Mode(age)
e1071::skewness(age)



# Example 2.5.1-2.5.3
range(age)
diff(range(age))
var(d242)
sd(age)
coef_variation(age)
quantile(age, probs = c(0, .25, .5, .75, 1)) # percentile, quartile, etc are special cases of 'quantiles'
rm(age)



# 10th ed, P50, Example 2.5.5
head(datA_255 <- read.csv('data/EXA_C02_S05_05.csv')) # R thinks {dat_255 <- read.csv('data/EXA_C02_S05_05.csv')} as argument
# head(datA_255 = read.csv('data/EXA_C02_S05_05.csv')) # R thinks `dat_255` as parameter of head function
dim(datA_255)
boxplot(datA_255) # newer of R can do this
boxplot(datA_255$GRF) # best practice


# 10th ed, P31, Exercise 2.3.2 
head(datR_232 <- read.csv('data/EXR_C02_S03_02.csv'))
dim(datR_232)
sizes_e232 = datR_232$sizes
(n_e232 = length(sizes_e232))
hist(sizes_e232)

sizes_e232_f = cut(sizes_e232, breaks = seq(from = 0, to = 30, by = 5), include.lowest = TRUE)
table(sizes_e232_f) # frequency
table(sizes_e232_f) / n_e232 # relative frequency
cumsum(table(sizes_e232_f)) # cumulative frequency
cumsum(table(sizes_e232_f)) / n_e232 # relative cumulative frequency

# explaning Boolean operators
TRUE & FALSE # FALSE
TRUE & TRUE # TRUE
TRUE | FALSE # TRUE
FALSE | TRUE # TRUE
FALSE | FALSE # FALSE
NA # missing
class(NA) # logical
TRUE & NA # NA
FALSE & NA # FALSE
TRUE | NA # TRUE
FALSE | NA # NA
# end of explaning Boolean operators

table((sizes_e232 >= 10) & (sizes_e232 <= 14.9)) / n_e232
mean((sizes_e232 >= 10) & (sizes_e232 <= 14.9)) # since TRUE is 1 and FALSE is 0
table(sizes_e232 < 20) / n_e232
table(sizes_e232 >= 25) / n_e232
table((sizes_e232 < 10) | (sizes_e232 >= 19.95)) / n_e232


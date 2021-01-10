

library(survival)


# Page 769, Example 14.5.1
head(datA_1451_raw <- read.csv('data/EXA_C14_S05_01.csv'))

datA_1451 = within(datA_1451_raw, expr = {
  edp = Surv(time, status)
  rm(time, status)
  drug = relevel(structure(drug, levels = c('Opiate', 'Other'), class = 'factor'), ref = 'Other')
})
head(datA_1451)
class(datA_1451$edp)

summary(model1_1451 <- coxph(edp ~ drug + age, data = datA_1451))
confint(model1_1451)

summary(model2_1451 <- coxph(edp ~ drug, data = datA_1451))
confint(model2_1451)

# library(ggfortify)
# autoplot(survfit(edp ~ drug, data = datA_1451))

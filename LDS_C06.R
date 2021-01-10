

source('shared_functions.R')



# CHOLEST
head(CHOLEST <- read.csv('data/LDS_C06_CHOLEST.csv'))
dim(CHOLEST)

(sp15 = sample(CHOLEST$CHOLEST, size = 15L))
CI_t(sp15)
t.test(sp15)

sp50 = sample(CHOLEST$CHOLEST, size = 50L)
CI_CLT_p(x = sum(sp50 > 225), n = length(sp50), level = .95)





# BABYWGTS
head(BABYWGTS <- read.csv('data/LDS_C06_BABYWGTS.csv'))
dim(BABYWGTS)

sp20 = sample(BABYWGTS$WGT, size = 20L)
(ci20 = CI_t(sp20))
t.test(sp20)
diff(ci20)

sp35 = sample(BABYWGTS$WGT, size = 35L)
(ci35 = CI_t(sp35))
t.test(sp35)
diff(ci35)





# BOYHGTS

head(BOYHGTS <- read.csv('data/LDS_C06_BOYHGTS.csv'))
dim(BOYHGTS)

sp15 = sample(BOYHGTS$HGT, size = 15L)
(ci15 = CI_t(sp15))
t.test(sp15)
# CI_z(sp15)
diff(ci15)

sp35 = sample(BOYHGTS$HGT, size = 35L)
(ci35 = CI_t(sp35))
t.test(sp35)
# CI_z(sp35)
diff(ci35)





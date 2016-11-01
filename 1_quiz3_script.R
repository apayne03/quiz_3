library (tidyverse)
library(apaTables)
library(cocor)
library(predictionInterval)

bfi2 <- read_csv("bfi2.csv")

# create correlation table
apa.cor.table(bfi2)

# Question 1
cocor(~A1+C1|E1+O1, data=as.data.frame(bfi2))

# Question 2 
cocor(~A1+C1|A1+E1, data=as.data.frame(bfi2))

# Question 3
bfi2_men <- bfi2 %>% filter(gender==1) %>% select(-gender)
bfi2_women <- bfi2 %>% filter(gender==2) %>% select(-gender)

apa.cor.table(bfi2_men)
apa.cor.table(bfi2_women)

bfi2_men <- as.data.frame(bfi2_men)
bfi2_women <- as.data.frame(bfi2_women)

cocor(~A1+E1|A1+E1, data=list(bfi2_men, bfi2_women))

# Question 4 determine correlation between rating-raises and rating-critical correlation
r.jk <- .59 
r.jh <- .16 
r.kh <- .38 
n <- 30

cocor.dep.groups.overlap(r.jk, r.jh, r.kh, n)


# Question 5 determine correlation between rating-complaint and raises-critical correlation
r.jk <- .59
r.hm <- .83
r.jh <- .16
r.jm <- .67
r.kh <- .38
r.km <- .19
n <- 30

cocor.dep.groups.nonoverlap(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n)


# Question 6

r1.jk <- -.24
r2.hm <- .03
n1=30
n2=3000

cocor.indep.groups(r1.jk, r2.hm, n1, n2)

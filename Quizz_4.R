# Question 1

antes <- c(140, 138, 150, 148, 135)
depois <- c(132, 135, 151,146, 130)
tteste <- t.test(antes, depois, paired = TRUE)
round(tteste$p.value,3)


#Question 2

round(1100 +c(-1,1)*qt(0.975,8)*30/sqrt(9),0)


# Question 3

min_value = 2
trial = 4
prob = 0.5

pbinom(min_value, size = trial, prob = prob, lower.tail = FALSE)

# Question 4

n_rate <- 1/100
n_obs <- 1787
infec_count <- 10

ppois(infec_count, n_rate*n_obs)


# Question 5 
# Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo.
# Subjects' body mass indices (BMIs) were measured at a baseline and again after 
# having received the treatment or placebo for four weeks. The average difference from 
# follow-up to the baseline (followup - baseline) was ???3 kg/m2 for the treated group 
# and 1 kg/m2 for the placebo group. The corresponding standard deviations of the differences 
# was 1.5 kg/m2 for the treatment group and 1.8 kg/m2 for the placebo group. Does the change 
# in BMI appear to differ between the treated and placebo groups? 
# Assuming normality of the underlying data and a common population variance, 
# give a pvalue for a two sided t test.

# Formulando hipótese
# H0: mbg = mpg
# H1: mbg <> mpg

d =0
n1 <- 9
n2 <- 9
m1 <- -3           # Mean for baseline group
m2 <- 1            # Mean for placebo group
sd1 <- 1.5         # Standard Deviation for base line group
sd2 <- 1.8         # Standard Deviation for placebo group

num <-  sd1^2/n1 + sd2^2/n2
den <- (sd1^2/n1)^2/(n1-1)+(sd2^2/n2)^2/(n2-1)

SE <- sqrt(num)

DF <- ceiling(num^2/den)
t = (m1-m2-d)/SE

p_value <- 2*pt(t, DF, lower.tail = TRUE)

# Teste com exemplo

d =0
n1 <- 30

# ----- Exemplo da internet


n2 <- 25
m1 <- 78           # Mean for baseline group
m2 <- 85            # Mean for placebo group
sd1 <- 10         # Standard Deviation for base line group
sd2 <- 15         # Standard Deviation for placebo group

num <-  sd1^2/n1 + sd2^2/n2
den <- (sd1^2/n1)^2/(n1-1)+(sd2^2/n2)^2/(n2-1)

SE <- sqrt(num)

DF <- num^2/den 
t = (m1-m2-d)/SE

p_value <- pt(t,40-1, lower.tail = TRUE)



# Questão 7 

# Researchers would like to conduct a study of 100 healthy adults to detect a four year 
# mean brain volume loss of .01~mm^3.
# Assume that the standard deviation of four year volume loss in this population is .04~mm^3.
# About what would be the power of the study for a 5% one sided test versus a null hypothesis
# of no volume loss?

alpha = 0.05
z = qnorm(1-alpha)

mu0 = -0.01
mua = 0
sigma = 0.04
n = 100

pnorm(mu0 + z*sigma/sqrt(n), mean = mua , sd = sigma/sqrt(n), lower.tail = FALSE)

power.t.test(n = 100, delta = 0.01, sd = 0.04, type = "one.sample", alt = "one.sided")$power

# Questão 8

# Researchers would like to conduct a study of n n healthy adults to detect a four year mean 
# brain volume loss of .01~mm^3.  
# Assume that the standard deviation of four year volume loss in this population is .04~mm^3.
# About what would be the value of n needed for 90% power of type one error rate of 5% one sided
# test versus a null hypothesis of no volume loss?

power.t.test(sig.level = 0.05, delta = 0.01, sd = 0.04, type = "one.sample", alt = "one.sided", power = 0.9)$n

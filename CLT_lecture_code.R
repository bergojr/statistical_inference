# Arquivo com os códigos utilizados on estudo sobre CLT 
# Instruções do estudo
# In this project you will investigate the exponential distribution in R and compare 
# it with the Central Limit Theorem. The exponential distribution can be simulated in R 
# with rexp(n, lambda) where lambda is the rate parameter. The mean of exponential 
# distribution is 1/lambda and the standard deviation is also 1/lambda. Set lambda = 0.2 
# for all of the simulations. You will investigate the distribution of averages 
# of 40 exponentials. Note that you will need to do a thousand simulations.

# Illustrate via simulation and associated explanatory text the properties of the 
# distribution of the mean of 40 exponentials. You should

# 1. Show the sample mean and compare it to the theoretical mean of the distribution.
# 2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
# 3. Show that the distribution is approximately normal.

# In point 3, focus on the difference between the distribution of a large collection 
# of random exponentials and the distribution of a large collection of averages of 40 exponentials.

# As a motivating example, compare the distribution of 1000 random uniforms

hist(runif(1000))

# and the distribution of 1000 averages of 40 random uniforms

mns = NULL
for (i in 1:1000) mns = c(mns, mean(runif(40)))
hist(mns)

# This distribution looks far more Gaussian than the original uniform distribution!
  
# This exercise is asking you to use your knowledge of the theory given in class 
# to relate the two distributions.

# Confused? Try re-watching video lecture 07 for a starter on how to complete this 
# project.

# Sample Project Report Structure

# Of course, there are multiple ways one could structure a report to address the 
# requirements above. However, the more clearly you pose and answer each question, 
# the easier it will be for reviewers to clearly identify and evaluate your work.

# A sample set of headings that could be used to guide the creation of your report 
# might be:
  
# - Title (give an appropriate title) and Author Name
# - Overview: In a few (2-3) sentences explain what is going to be reported on.
# - Simulations: Include English explanations of the simulations you ran, with the accompanying R code. Your explanations should make clear what the R code accomplishes.
# - Sample Mean versus Theoretical Mean: Include figures with titles. In the figures, highlight the means you are comparing. Include text that explains the figures and what is shown on them, and provides appropriate numbers.
# - Sample Variance versus Theoretical Variance: Include figures (output from R) with titles. Highlight the variances you are comparing. Include text that explains your understanding of the differences of the variances.
# - Distribution: Via figures and text, explain how one can tell the distribution is approximately normal.

library(dplyr)
lambda = 0.2
nosim = 1000
n1 = 10
n2 = 20
n3 = 40
set.seed(63)

data1 <- rexp(nosim*n1,lambda)
data2 <- rexp(nosim*n2,lambda)
data3 <- rexp(nosim*n3,lambda)

teste2 <- cbind(data1[1:100], data2[1:100], data3 [1:100])



data <- data.frame(
  x = c(data1, data2, data3),
  size = factor(rep(c(n1,n2,n3),c(length(data1),length(data2),length(data3))))
)

g <- ggplot(data, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..count..))
g + facet_grid(. ~ size)


medias <- data.frame(
  x = c(apply(matrix(data1, nosim), 1, mean),
        apply(matrix(data2, nosim), 1, mean),
        apply(matrix(data3, nosim), 1, mean)
  ),
  size = factor(rep(c(n1, n2, n3), rep(nosim, 3)))))

g <- (ggplot(medias, aes(x = x, fill = size)) 
      + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..count..)) 
      + geom_vline(xintercept = 1/lambda, linetype = "dotted", size = 1.5, color = "blue")
      + annotate("text", label = "Theoretical \n Mean", size = 4, x = 1.4*(1/lambda), y = 200)
      )
g + facet_grid(. ~ size)


variances <- data.frame(
  x = c(apply(matrix(data1, nosim), 1, var),
        apply(matrix(data2, nosim), 1, var),
        apply(matrix(data3, nosim), 1, var)
  ),
  size = factor(rep(c(n1, n2, n3), rep(nosim, 3)))) 

g <- (ggplot(variances, aes(x = x, fill = size)) 
      + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..count..)) 
      + geom_vline(xintercept = 1/lambda^2, linetype = "dotted", size = 1.5, color = "blue")
      + annotate("text", label = "Theoretical \n Variance", size = 4, x = 3*(1/lambda^2), y = 20)
)
g + facet_grid(. ~ size)

teor_var1 <- (1/lambda^2)/n1
teor_var2 <- (1/lambda^2)/n2
teor_var3 <- (1/lambda^2)/n3
sample_var1 <- var(medias$x[medias$size==n1])
sample_var2 <- var(medias$x[medias$size==n2])
sample_var3 <- var(medias$x[medias$size==n3])
conf_1 <- t.test(medias$x[medias$size==n1])$conf.int
conf_2 <- t.test(medias$x[medias$size==n2])$conf.int
conf_3 <- t.test(medias$x[medias$size==n2])$conf.int

lista_1 <- list(n1, teor_var1, sample_var1, paste(round(conf_1[1], 3),"-",round(conf_1[2],3)))

var_table <- tbl_df(rbind(c(n1, teor_var1, sample_var1),
                   cbind(n2, teor_var2,sample_var2),
                   cbind(n3, teor_var3, sample_var3)))

names(var_table) <- c("Size", "Theor. Variance", "Sample Variance", "Mean Conf. Interval")

cfunc <- function(x, n) sqrt(n) * (mean(x) - 1/lambda) / (1/lambda)

dat <- data.frame(
  x = c(apply(matrix(data1, nosim), 1, cfunc, n1),
        apply(matrix(data2, nosim), 1, cfunc, n2),
        apply(matrix(data3, nosim), 1, cfunc, n3)
  ),
  size = factor(rep(c(n1, n2, n3), rep(nosim, 3))))


g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)




# 
# hist(dist_exp1)
# mean_exp1 <- apply(matrix(dist_exp1,nosim),1,mean)
# hist(mean_exp1)
# sd_exp1 <- apply(matrix(dist_exp1, nosim),1, sd)
# hist(sd_exp1)
# var_exp1 <- apply(matrix(dist_exp1, nosim),1,var)
# hist(var_exp1)
# 
# #teste = matrix(rexp(nosim*n,lambda),nosim)
# #matrix(sample(1 : 6, nosim * 10, replace = TRUE),nosim)

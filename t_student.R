# homework questions

#  Questions 7 and 9

data(mtcars)

cyl4_ <- mtcars$cyl==4
cyl6_ <- mtcars$cyl==6
cyl4 <- mtcars$mpg[cyl4_]
cyl6 <- mtcars$mpg[cyl6_]

n1 <- length(cyl4)
n2 <- length(cyl6)

sp <- sqrt(((n1-1)*sd(cyl4)^2+(n2-1)*sd(cyl6)^2)/(n1+n2-2))

md <- mean(cyl4) - mean(cyl6)

semd <- sp * sqrt(1/n1+1/n2)

md +c(-1,1)*qt(0.975,n1+n2-2)*semd
 t.test(cyl4, cyl6, paired = FALSE, var.equal = TRUE)$conf
 
# Question 10
 
 var_exc10 <- ((9-1)*1.5^2+(9-1)*1.8^2)/((9+9-2))
 (1.8^2+1.5^2)/27

# homework Hypothesis Test
 
# Question 3

data(mtcars)
attach(mtcars)
n <- length(mpg)
mn <- mean(mpg)
s <- sd(mpg)
z <- qnorm(.05)
mu0 <- mn - z*s/sqrt(n)

# Question 4

cyl4 <- mtcars$mpg[mtcars$cyl==4]
cyl6 <- mtcars$mpg[mtcars$cyl==6]

t.test(cyl4,cyl6)


# Question 5

3 + c(-1,1)*qnorm(1-0.05/2)*1.1/sqrt(100)

# Quizz 3
 
# t=(X'-mu)/(s/sqrt(n)
#X' +/- t_(n-1)*s/sqrt(n)
 
1100 +c(-1,1)*qt(0.975,8)*30/sqrt(9)
 
-2 +c(-1,1)*qt(0.975,8)*2.6/sqrt(9)
-2 +c(-1,1)*qt(0.975,8)*1.5/sqrt(9)
-2 +c(-1,1)*qt(0.975,8)*2.1/sqrt(9)
-2 +c(-1,1)*qt(0.975,8)*0.3/sqrt(9)

# questao 4

n1 <- 10
n2 <- 10

sp <- sqrt(((n1-1)*0.6+(n2-1)*0.68)/(n1+n2-2))

md <- 3 -5 

semd <- sp * sqrt(1/n1+1/n2)

md +c(-1,1)*qt(1-0.05/2,n1+n2-2)*semd
t.test(cyl4, cyl6, paired = FALSE, var.equal = TRUE)$conf

#questão 6


6-4+c(-1,1)*qnorm(0.975)*sqrt((0.5^2/100+2^2/100))*1.1/sqrt(100)


#Questao 7

n1 <- 9
n2 <- 9

sp <- sqrt(((n1-1)*(1.5^2)+(n2-1)*(1.8^2))/(n1+n2-2))

md <- -3 -1 

semd <- sp * sqrt(1/n1+1/n2)

md +c(-1,1)*qt(1-0.10/2,n1+n2-2)*semd
t.test(cyl4, cyl6, paired = FALSE, var.equal = TRUE)$conf


# QUIZZ 4

# Questão 1



# Questão 2

1100 +c(-1,1)*qt(0.975,8)*30/sqrt(9)

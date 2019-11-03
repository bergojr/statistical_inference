n <- 100
pvals <- seq(0.1,0.9, by= 0.05)
nosim <- 1000

coverage <- sapply(pvals, function(p){ 
     phats <- rbinom(nosim,prob = p, size=n)/n
     ll <- phats - qnorm(0.975)*sqrt(phats*(1-phats)/n)
     ul <- phats + qnorm(0.975)*sqrt(phats*(1-phats)/n)
     mean( ll <p & ul>p)
     })
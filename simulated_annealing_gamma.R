## simulated annealling to estimate gamma coefficients

library(tidyverse)
library(optimization)

set.seed(42)

## first, assuming homogeneity and no effects between covariates
## we have a dgm A -> B -> C -> Y
## define additive effects and baselines

n <- 5000

y_when_all_0 <- 0.3
c_when_all_0 <- 0.2
b_when_all_0 <- 0.3
a_probability <- 0.6

effect_c_on_y <-  0.2

effect_b_on_c <-  0
effect_b_on_y <-  0.3

effect_a_on_b <-  0.0
effect_a_on_c <-  0.0
effect_a_on_y <-  0.1

# values should be:
# beta = 0.3
# gamma_a = 1/7
# gamma_b = 3/7
# gamma_c = 2/7

a <- rbinom(n, 1, a_probability)

data <- data.frame(a = a, b = 0, c = 0, y = 0)

for(i in 1:n){
  data$b[i] <- rbinom(1, 1, b_when_all_0 + data$a[i] * effect_a_on_b)
  data$c[i] <- rbinom(1, 1, c_when_all_0 + data$b[i] * effect_b_on_c + data$a[i] * effect_a_on_c)
  data$y[i] <- rbinom(1, 1, y_when_all_0 + data$c[i] * effect_c_on_y + data$b[i] * effect_b_on_y + data$a[i] * effect_a_on_y)
}

# define functions for optimization

## the PTO is def as

pto <- function(p, R){
  if(R > 0){
    return(1 - (1-p)*(1-R))
  }
  else if(R < 0){
    return(p * (1 + R))
  }
  else return(p)
  
}

# plot pto for possible inputs 

m <- matrix(nrow = 51, ncol = 101)

for(p in 1:51){
  for(R in 1:101){
    m[p, R] <- pto(p = (p - 1) * 0.02, 
                   R = -1 + (R-1) * 0.02) 
  }
}

d <- m %>% as.data.frame()
d$p <- 1:51
d <- pivot_longer(d, cols = 1:101, names_to = "R")
d$R <- str_sub(d$R, start = 2) %>% as.numeric()
d$R <- -1 + (d$R-1) * 0.02
d$p <- (d$p - 1) * 0.02 

ggplot(data = d, aes(x=p, y=R, fill=value)) + 
  geom_tile() +
  theme_classic() +
  scale_fill_viridis_c()

## define inverse pto

pto_inv <- function(p, q){
  if(q > p){
    return(1 - (1-q)/(1-p))
  }
  else if(q < p){
    return(-1 + (q/p))
  }
  else return(0)
}

# define fg

f_g <- function(p, V, gamma){
  if(V == 1){
    return(pto(p, gamma))
  }
  else{
    return(p)
  }
}

# individual likelihood

prob_y1 <- function(A, B, C, g_a, g_b, g_c, beta){
  f_g(beta, A, g_a) %>% 
    f_g(B, g_b) %>% 
    f_g(C, g_c)
}

# total likelihood given vector of parameters

a <- data$a
b <- data$b
c <- data$c
y <- data$y

full_ll <- function(gamma_vector){
  ll <- list()
  
  for(i in 1:n){
    l <- prob_y1(a[i], b[i], c[i], gamma_vector[1], gamma_vector[2], gamma_vector[3], gamma_vector[4])
    l2 <- l^y[i] * (1-l)^(1 - y[i])
    ll[[i]] <- log(l2)
  }
  return(do.call(sum, ll))
}

## simulated annealing for optimization

gamma_upper <- 1 - 1e-05
gamma_lower <- -1 + 1e-05
beta_upper <- 1 - 1e-05
beta_lower <- 1e-05

opt <- optim_sa(fun = full_ll, maximization = T,
         start = c(runif(3, gamma_lower, gamma_upper), runif(1, beta_lower, beta_upper)),
         lower = c(gamma_lower, gamma_lower, gamma_lower, beta_lower),
         upper = c(gamma_upper, gamma_upper, gamma_upper, beta_upper),
         trace = TRUE, 
         control = list(dyn_rf = FALSE,
                        rf = 1.6,
                        t0 = 100,
                        nlimit = 400,
                        r = 0.6,
                        t_min = 0.1))

opt$par

# computationally intensive, but gives parameter values: 
# beta = 0.2975
# gamma_a = 0.155
# gamma_b = 0.430
# gamma_c = 0.200

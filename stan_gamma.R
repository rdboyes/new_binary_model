# stan fit

library(rstan)
library(tidyverse)

options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')
rstan_options(auto_write = TRUE)

set.seed(42)

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

stan_data <- list(N = n,
                  a = data$a,
                  b = data$b,
                  c = data$c,
                  y = data$y)

fit <- stan("stan_model.stan", data = stan_data, control = list(adapt_delta = 0.95))

plot(fit)

### ---- with different coefficients

set.seed(43)

n <- 5000

y_when_all_0 <- 0.3
c_when_all_0 <- 0.2
b_when_all_0 <- 0.3
a_probability <- 0.6

effect_c_on_y <-  -0.2

effect_b_on_c <-  0
effect_b_on_y <-  0.3

effect_a_on_b <-  0.0
effect_a_on_c <-  0.0
effect_a_on_y <-  0.0

# values should be:
# beta = 0.3
# gamma_a = 0
# gamma_b = 3/7
# gamma_c = -2/3

a <- rbinom(n, 1, a_probability)

data <- data.frame(a = a, b = 0, c = 0, y = 0)

for(i in 1:n){
  data$b[i] <- rbinom(1, 1, b_when_all_0 + data$a[i] * effect_a_on_b)
  data$c[i] <- rbinom(1, 1, c_when_all_0 + data$b[i] * effect_b_on_c + data$a[i] * effect_a_on_c)
  data$y[i] <- rbinom(1, 1, y_when_all_0 + data$c[i] * effect_c_on_y + data$b[i] * effect_b_on_y + data$a[i] * effect_a_on_y)
}

stan_data2 <- list(N = n,
                  a = data$a,
                  b = data$b,
                  c = data$c,
                  y = data$y)

fit2 <- stan("stan_model.stan", data = stan_data2, control = list(adapt_delta = 0.95))

fit2

plot(fit2)


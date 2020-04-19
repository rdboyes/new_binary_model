## stan?

## beginning a stan attempt

library(rstan)

options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')
rstan_options(auto_write = TRUE)



stan_data <- list(N = n,
                  a = data$a,
                  b = data$b,
                  c = data$c,
                  y = data$y)

fit <- stan(model_code = stan_model, data = stan_data, control = list(adapt_delta = 0.99))




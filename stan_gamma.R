# stan fit

library(rstan)
library(tidyverse)
library(patchwork)

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

### ---- with positive, negative, and zero true gamma + relationships between covariates

set.seed(43)

n <- 5000

y_when_all_0 <- 0.3
c_when_all_0 <- 0.2
b_when_all_0 <- 0.3
a_probability <- 0.6

effect_c_on_y <-  -0.2

effect_b_on_c <-  0.1
effect_b_on_y <-  0.3

effect_a_on_b <-  -0.1
effect_a_on_c <-  0.1
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

fit2 <- stan("stan_model.stan", data = stan_data2, iter = 4000, control = list(adapt_delta = 0.95))

# are stan's confidence intervals are too optimistic?
# fit 100 models on 100 datasets generated with the same parameters

fit_list <- list()

for(model in 1:100){
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

  fit_list[[model]] <- stan("stan_model.stan", data = stan_data2, iter = 4000, control = list(adapt_delta = 0.95))
}

summary <- summary(fit_list[[1]])$summary %>% as.data.frame() %>% rownames_to_column()
summary$iter <- 1

for (i in 2:100){
  temp <- summary(fit_list[[i]])$summary %>% as.data.frame() %>% rownames_to_column()
  temp$iter <- i
  summary <- rbind(summary, temp)
}

mean_a <- mean(summary$mean[summary$rowname == "gamma_a"])

summary %>% filter(rowname == "gamma_a") %>% 
  ggplot(aes(x = iter, ymin = `2.5%`, ymax =`97.5%`)) +
  geom_errorbar() + geom_hline(yintercept = 0, color = 'red') +
  ggtitle("Average values show bias away from true values") +
  geom_hline(yintercept = mean(summary$mean[summary$rowname == "gamma_a"]), linetype = "dotted") -> plot_gamma_a

summary %>% filter(rowname == "gamma_b") %>% 
  ggplot(aes(x = iter, ymin = `2.5%`, ymax =`97.5%`)) +
  geom_errorbar() + geom_hline(yintercept = 3/7, color = 'red') +
  geom_hline(yintercept = mean(summary$mean[summary$rowname == "gamma_b"]), linetype = "dotted") -> plot_gamma_b

summary %>% filter(rowname == "gamma_c") %>% 
  ggplot(aes(x = iter, ymin = `2.5%`, ymax =`97.5%`)) +
  geom_errorbar() + geom_hline(yintercept = -2/3, color = 'red') +
  geom_hline(yintercept = mean(summary$mean[summary$rowname == "gamma_c"]), linetype = "dotted") -> plot_gamma_c

ggsave(plot_gamma_a/plot_gamma_b/plot_gamma_c, file = "bias.png") 


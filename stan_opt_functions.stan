functions{
  real pto(real p, real R){
    if(R > 0){
      return 1 - (1-p)*(1-R);
    }
    else if(R < 0){
      return p * (1 + R);
    }
    else{
      return p;
    } 
  }
  
  real fg(real p, real V, real gamma){
    if(V == 1){
      return pto(p, gamma);
    }
    else{
      return p;
    }
  }
  
  real prob_y1(real A, real B, real C, real g_a, real g_b, real g_c, real beta){
    return fg(fg(fg(beta, A, g_a), B, g_b), C, g_c);
  }
}

data {
  int<lower=0> N;
  vector[N] y;
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  y ~ normal(mu, sigma);
}


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
  
  real fg(real p, int V, real gamma){
    if(V == 1){
      return pto(p, gamma);
    }
    else{
      return p;
    }
  }
  
  real prob_y1(int y, int A, int B, int C, real g_a, real g_b, real g_c, real beta){
    if (y == 1){
      return log(fg(fg(fg(beta, A, g_a), B, g_b), C, g_c));
    }
    else{
      return log1m(fg(fg(fg(beta, A, g_a), B, g_b), C, g_c));
    }
  }
}

data{
  int<lower=0> N;
  int a[N];
  int b[N];
  int c[N];
  int y[N];
}

parameters{
  real<lower=-1, upper=1> gamma_a;
  real<lower=-1, upper=1> gamma_b;
  real<lower=-1, upper=1> gamma_c;
  real<lower=0, upper=1> beta;
}

model{
  for (i in 1:N) 
    target += prob_y1(y[i], a[i], b[i], c[i], gamma_a, gamma_b, gamma_c, beta);
}

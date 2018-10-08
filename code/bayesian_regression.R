# October, 2018
# Bayesian Regression ---------------------------
library(tidyverse)

# Simulating  data ----------------------------------
set.seed(2018) # reprodutibility

x <- rnorm(n = 100, mean = 3, sd = 1.5)
e <- rnorm(n = 100)

# Priors of parameters
betas <- rnorm(n = 2, mean = 0, sd = sqrt(10))

# The regression model
y <- betas[1] + (betas[2] * x) + e

da <- data.frame(y, x)

da %>% 
  ggplot(aes(x, y)) +
  geom_point(colour = 'skyblue', size = 2, alpha = 0.75) +
  geom_smooth(method = 'lm', colour = 'grey', linetype = 'dotted') +
  theme_bw() +
  labs(x = 'Independent variable', y = 'Dependent variable')


# The classical regression model
# With functions:
lm(y ~ x) %>% summary()

# Vanilla flavour:
mm <- model.matrix( ~ x, data = da)
k <-  ncol(mm)
n <- nrow(mm)

# Estimating betas
v <- solve(t(mm) %*% mm)
betas_hat <- c(v %*% t(mm) %*% y)

# y_hat
y_hat <- mm %*% betas

# Residual sum of squares
rss <- sum((y - y_hat)^2)

# Mean sum of errors
mse <- mean((y - y_hat)^2)

# Estimating the variance of the parameters
var_betas <- solve(t(mm) %*% mm) * mse

# t-values
betas_hat[2]/sqrt(var_betas[2, 2]) 
betas_hat[1]/sqrt(var_betas[1, 1])

# # p-values
# pt(-5.144, n - k)
# dt(-70.721, n - k)

# The R's - Multiple correlation coefficients
R <- sum((y_hat - mean(y))^2)/sum((y - mean(y))^2)
R_adj <- 1 - (1 - R)*((n-1)/(n-k))

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Bayesian model

# p(betas / sigma^2) = normal(m, sigma^2*V)
# standard non-informative prior 
# p(betas, sigma^2 / y) proportional to 1/sigma^2
# posterior parameters

mm <- model.matrix(~x, data = da)
k <-  ncol(mm)
n <- nrow(mm)
v <- solve(t(mm) %*% mm)
betas <- v %*% t(mm) %*% y

y_hat <- mm %*% betas
da$res <- y - y_hat

da %>% 
  ggplot(aes(res)) +
  geom_density(colour = 'skyblue', size = 1.2, alpha = 0.75) +
  geom_vline(xintercept = 0, linetype = 'dotted') +
  theme_bw() +
  labs(x = 'residual')

# Estimated s2
s2 <- (t(da$res) %*% da$res)/(n-k)

# Simulations -------------------------------------------------------------
sim <- 10000
gamma <- rgamma(sim, shape = (n-k)/2, 
                rate = (n-k)*s2/2)

sigma <- 1/gamma
err <- sigma * MASS::mvrnorm(n = sim, mu = c(0, 0), v)
beta_sim <- rep(c(betas), each = 1000) + c(err[,1], err[,2])


beta_sim %>% 
  as.data.frame() %>% 
  slice(1:1000) %>% 
  ggplot(aes(.)) +
  geom_density(colour = 'skyblue', size = 1.2, alpha = 0.75) +
  geom_vline(xintercept = betas[1], linetype = 'dotted') +
  theme_bw()

beta_sim %>% 
  as.data.frame() %>% 
  slice(1001:2000) %>% 
  ggplot(aes(.)) +
  geom_density(colour = 'skyblue', size = 1.2, alpha = 0.75) +
  geom_vline(xintercept = betas[2], linetype = 'dotted') +
  theme_bw()

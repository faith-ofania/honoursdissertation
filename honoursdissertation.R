# Data
data = list(N=48, 
             
             M=0,
             
             C=c(15.9, 25.7, 28.5, 23.7, 24.9, 33.2, 28.2, 19.7, 17.6, 19.4, 21.5, 23.1,
                 22.6, 22.9, 24.0, 29.7, 14.9, 14.6, 31.1, 37.3, 40.6, 30.2, 27.2, 28.7, 
                 26.0, 36.6, 32.8, 35.3, 27.6, 28.4, 28.0, 30.6, 27.7, 31.4, 38.8, 31.7, 
                 28.0, 22.5, 18.9, 24.5, 20.3, 18.9, 22.3, 19.2, 24.1, 25.1, 19.3, 13.7),
             
             I=c(801.35, 791.72, 743.27, 599.7, 629.15, 415.4, 319.23, 343.32, 405.04, 374.69, 449.76, 
                 384.15, 352.29, 361.37, 321.2, 301.33, 294.83, 368.2, 300.71, 295.3, 263.46, 195.45,
                 165.61,177.62, 197.55, 214.57, 218.2, 282.08, 276.07, 292.35, 308.64, 281.76, 197.97, 
                 174.6, 218.39, 175.16, 153.28, 202.27, 253.89, 202.06, 239.13, 248.35, 262.41, 286.47, 
                 240.72, 250.93, 270.34,180.64)*0.1 )

# schaefer model
stan_mod <- stan_model("model.stan")

# fit schaefer model
fit_schaefer_default <- sampling(stan_mod, data = data)

# model checking set up
lp_ncp <- log_posterior(fit_schaefer_default)
np_ncp <- nuts_params(fit_schaefer_default)
color_scheme_set("mix-brightblue-gray")

# marginal posteriors
mcmc_pairs(fit_schaefer_defaultb, pars = c("K","r","q","sigma2","tau2"), off_diag_args = list(size = 0.75))

# tau traceplot
mcmc_trace(fit_schaefer_default, pars = c("tau2"), np = np_ncp) + xlab("Post-warmup iteration")

# rhat assess
rhats <- rhat(fit_schaefer_default, pars = c("K","r","q","sigma2","tau2"))
mcmc_rhat(rhats) + yaxis_text(hjust = 0)

# ess assess
neff_ncp <- neff_ratio(fit_schaefer_default, pars = c("K","r","q","sigma2","tau2"))
mcmc_neff(neff_ncp)

# look at other diagnostics in ShinyStan
launch_shinystan(fit_schaefer_default)

# fit better schaefer model
fit_schaefer <- sampling(stan_mod, data = data, control=list(adapt_delta=0.99,max_treedepth=15), iter = 20000,thin=5)

# model checking set up
lp_ncp <- log_posterior(fit_schaefer)
np_ncp <- nuts_params(fit_schaefer)

# marginal posteriors
mcmc_pairs(fit_schaefer, np = np_ncp, pars = c("K","r","q","sigma2","tau2"), off_diag_args = list(size = 0.75))

# traceplots
mcmc_trace(fit_schaefer, pars = c("K"), np = np_ncp) + xlab("Post-warmup iteration")
mcmc_trace(fit_schaefer, pars = c("r"), np = np_ncp) + xlab("Post-warmup iteration")
mcmc_trace(fit_schaefer, pars = c("q"), np = np_ncp) + xlab("Post-warmup iteration")
mcmc_trace(fit_schaefer, pars = c("sigma2"), np = np_ncp) + xlab("Post-warmup iteration")
mcmc_trace(fit_schaefer, pars = c("tau2"), np = np_ncp) + xlab("Post-warmup iteration")

# rhats assess
rhats <- rhat(fit_schaefer,pars = c("K","r","q","sigma2","tau2"))
mcmc_rhat(rhats) + yaxis_text(hjust = 0)

# ess assess 
neff_ncp <- neff_ratio(fit_schaefer, pars = c("K","r","q","sigma2","tau2"))
mcmc_neff(neff_ncp)

# look at other diagnostics in shiny stan
launch_shinystan(fit_schaefer)

# fox model
stan_mod_fox <- stan_model("fox_model.stan")
fit_fox <- sampling(stan_mod_fox, data = data, control=list(adapt_delta=0.99,max_treedepth=15), iter = 20000,thin=5)

# model checking set up
lp_ncp <- log_posterior(fit_fox)
np_ncp <- nuts_params(fit_fox)

# marginal posteriors
color_scheme_set("blue")
mcmc_pairs(fit_fox, np = np_ncp, pars = c("K","r","q","sigma2","tau2"), off_diag_args = list(size = 0.75))

# traceplots
color_scheme_set("mix-brightblue-gray")
mcmc_trace(fit_fox, pars = c("K"), np = np_ncp) + xlab("Post-warmup iteration")
mcmc_trace(fit_fox, pars = c("r"), np = np_ncp) + xlab("Post-warmup iteration")
mcmc_trace(fit_fox, pars = c("q"), np = np_ncp) + xlab("Post-warmup iteration")
mcmc_trace(fit_fox, pars = c("sigma2"), np = np_ncp) + xlab("Post-warmup iteration")
mcmc_trace(fit_fox, pars = c("tau2"), np = np_ncp) + xlab("Post-warmup iteration")

# rhats assess
rhats <- rhat(fit_fox)
mcmc_rhat(rhats) + yaxis_text(hjust = 0)

# ess assess 
neff_ncp <- neff_ratio(fit_fox, pars = c("K","r","q","sigma2","tau2"))
mcmc_neff(neff_ncp)

# look at other diagnostics in shiny stan
launch_shinystan(fit_fox)

#### fox and shaefer with P[1] = 0.9 ####

stan_mod_p1 <- stan_model("model_p1_0.9.stan")
stan_mod_fox_p1 <- stan_model("fox_model_p1_0.9.stan")

# schaefer model
fit_schaefer_p1 <- sampling(stan_mod_p1, data = data, control=list(adapt_delta=0.99,max_treedepth=15), iter = 20000,thin=5)

# model checking set up
lp_ncp <- log_posterior(fit_schaefer_p1)
np_ncp <- nuts_params(fit_schaefer_p1)

# marginal posteriors
color_scheme_set("blue")
mcmc_pairs(fit_schaefer_p1, np = np_ncp, pars = c("K","r","q","sigma2","tau2"), off_diag_args = list(size = 0.75))

# traceplots
color_scheme_set("mix-brightblue-gray")
mcmc_trace(fit_schaefer_p1, pars = c("K"), np = np_ncp) + xlab("Post-warmup iteration")
mcmc_trace(fit_schaefer_p1, pars = c("r"), np = np_ncp) + xlab("Post-warmup iteration")
mcmc_trace(fit_schaefer_p1, pars = c("q"), np = np_ncp) + xlab("Post-warmup iteration")
mcmc_trace(fit_schaefer_p1, pars = c("sigma2"), np = np_ncp) + xlab("Post-warmup iteration")
mcmc_trace(fit_schaefer_p1, pars = c("tau2"), np = np_ncp) + xlab("Post-warmup iteration")

# rhats assess
rhats <- rhat(fit_schaefer_p1, pars = c("K","r","q","sigma2","tau2"))
mcmc_rhat(rhats) + yaxis_text(hjust = 0)

# ess assess 
neff_ncp <- neff_ratio(fit_schaefer_p1, pars = c("K","r","q","sigma2","tau2"))
mcmc_neff(neff_ncp)

# look at other diagnostics in shiny stan
launch_shinystan(fit_schaefer_p1)

# fox model
fit_fox_p1 <- sampling(stan_mod_fox_p1, data = data, control=list(adapt_delta=0.99,max_treedepth=15), iter = 20000,thin=5)

# model checking set up
lp_ncp <- log_posterior(fit_fox_p1)
np_ncp <- nuts_params(fit_fox_p1)

# marginal posteriors
color_scheme_set("blue")
mcmc_pairs(fit_fox_p1, np = np_ncp, pars = c("K","r","q","sigma2","tau2"), off_diag_args = list(size = 0.75))

# traceplots
color_scheme_set("mix-brightblue-gray")
mcmc_trace(fit_fox_p1, pars = c("K"), np = np_ncp) + xlab("Post-warmup iteration")
mcmc_trace(fit_fox_p1, pars = c("r"), np = np_ncp) + xlab("Post-warmup iteration")
mcmc_trace(fit_fox_p1, pars = c("q"), np = np_ncp) + xlab("Post-warmup iteration")
mcmc_trace(fit_fox_p1, pars = c("sigma2"), np = np_ncp) + xlab("Post-warmup iteration")
mcmc_trace(fit_fox_p1, pars = c("tau2"), np = np_ncp) + xlab("Post-warmup iteration")

# rhats assess
rhats <- rhat(fit_fox_p1)
mcmc_rhat(rhats) + yaxis_text(hjust = 0)

# ess assess 
neff_ncp <- neff_ratio(fit_fox_p1, pars = c("K","r","q","sigma2","tau2"))
mcmc_neff(neff_ncp)

# look at other diagnostics in shiny stan
launch_shinystan(fit_fox_p1)

### use loo for model comparison ###

# schaefer loo
par(mfrow=c(1,2))

loo_1 <- loo(fit_schaefer)
print(loo_1)
plot(loo_1)

loo_1b <- loo(fit_schaefer, moment_match = TRUE)
print(loo_1b)
plot(loo_1b)

# fox loo
loo_2 <- loo(fit_fox)
print(loo_2)
plot(loo_2)

loo_2b <- loo(fit_fox, moment_match = TRUE)
print(loo_2b)
plot(loo_2b)

# schaefer p1=0.9 loo
loo_3 <- loo(fit_schaefer_p1)
print(loo_3)
plot(loo_3)

loo_3b <- loo(fit_schaefer_p1, moment_match = TRUE)
print(loo_3b)
plot(loo_3b)

# fox p1=0.9 loo
loo_4 <- loo(fit_fox_p1)
print(loo_4)
plot(loo_4)

loo_4b <- loo(fit_fox_p1, moment_match = TRUE)
print(loo_4b)
plot(loo_4b)

# use loo to compare the 4 models
loo_compare(loo_1,loo_2,loo_3,loo_4)
s = loo_compare(loo_1b,loo_2b,loo_3b,loo_4b)
print(s,simplify = F)


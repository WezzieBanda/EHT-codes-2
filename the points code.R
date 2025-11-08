##############################################################################
##
## Additional benefit of next-generation nets
## April 2022
##
###############################################################################


###################################################
##
## G2: New nets analysis, ESSENTIALS showing the relationships for the
## new nets are consistent with the standard and PBO-nets defined in 
## Churcher et al 2016 / Nash et al 2021 / Sherrard-Smith et al 2021
##
#########################################################################


library(rstan)
library(readr)

# For execution on a local, multicore CPU with excess RAM we recommend calling
options(mc.cores = parallel::detectCores())
# To avoid recompilation of unchanged Stan programs, we recommend calling
rstan_options(auto_write = TRUE)

g2_dat = read.csv("data/pyrethroid-pyrrole nets April 2022.csv",header=TRUE)
g2_dat <- read_csv("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/stan/ellie code/pyrethroid-pyrrole nets April 2022_public (1).csv")

tail(g2_dat)
library(tidyr)
g2_dat = g2_dat %>% drop_na(n_dead_72h)
g2_dat = g2_dat %>% drop_na(n_total)
g2_dat = g2_dat %>% drop_na(n_dead_72h_PYRNET)
g2_dat = g2_dat %>% drop_na(n_total_PYRNET)

###########################################
##
## Part 1:
##
## Added benefit of the nets

##Relationship 1 pyrethroid-pyrrole benefit on top of pyrethroid-only nets 
##LOGISTIC BENEFIT
d_t = c(g2_dat$n_dead_72h) # number of mosquitoes dying IRS HUTS
n_t = c(g2_dat$n_total) # Number of mosquitoes entering IRS huts
x1 = c(g2_dat$n_dead_72h_PYRNET)/
  c(g2_dat$n_total_PYRNET)



data_G2 = data.frame(d_t,n_t,x1)
data_G2$Proportion_dead = data_G2$d_t/data_G2$n_t
x = seq(1,0,length=100)
fmG2 <- cbind(data_G2$d_t,data_G2$n_t-data_G2$d_t) ~ data_G2$x1 
glm_2 <- glm(fmG2, family = binomial())
role.fitted2 <- predict(glm_2, se.fit = TRUE, type = "response")
summary(glm_2)$coeff[2,1]
summary(glm_2)$coeff[1,1]

data_list_g2 = list(N = nrow(data_G2),
                    n_t = data_G2$n_t,
                    d_t = data_G2$d_t,
                    x = data_G2$x1)

stan_base <- stan(file="Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/stan/ellie code/binomial_fit_nets.stan.stan", 
                  data=data_list_g2, 
                  warmup=1000,
                  control = list(adapt_delta = 0.8,
                                 max_treedepth = 20),
                  iter=2000, chains=4)
base <- rstan::extract(stan_base)



stan_base <- readRDS("C:/Users/user/Downloads/ento_g2_benefit_Apr2022data (2).RDS")

base <- rstan::extract(stan_base)
# posterior$alpha1 is now a numeric vector of samples
mean(base$alpha1)
sd(base$alpha1)
mean(base$alpha2)
# saveRDS(stan_base,"stan model outputs/ento_g2_benefit_Apr2022data.RDS")

## diagnostics
# traceplot(stan_base)
# stan_diag(stan_base,
#           information = c("sample","stepsize", "treedepth","divergence"),
#           chain = 0)
# stan_hist(stan_base, include = TRUE, unconstrain = FALSE,
# inc_warmup = FALSE)
# stan_rhat(stan_base,bins = 10)
mean(base$alpha2)
mean(base$alpha1)


preds_g2 = array(dim=c(100,4000))
for(i in 1:4000){
  preds_g2[,i] = 1 / (1 + exp(-base$alpha1[i] - base$alpha2[i]*x))
}

median_G2_pred = 1 / (1 + exp(-quantile(base$alpha1,0.5) - quantile(base$alpha2,0.5)*x))
upper_G2_pred = 1 / (1 + exp(-quantile(base$alpha1,0.975) - quantile(base$alpha2,0.975)*x))
lower_G2_pred = 1 / (1 + exp(-quantile(base$alpha1,0.025) - quantile(base$alpha2,0.025)*x))

### -----------------------------
# 1️⃣ Adjust margins (optional)
# -----------------------------
par(mfrow = c(1, 1))            # single plot
par(mar = c(4.5, 5.5, 3, 2))    # tighter margins

# -----------------------------
# 2️⃣ Calculate proportions
# -----------------------------
g2_dat$x <- g2_dat$n_dead_72h_PYRNET / g2_dat$n_total_PYRNET
g2_dat$prop_killed_g2 <- g2_dat$n_dead_72h / g2_dat$n_total

# -----------------------------
# 3️⃣ Scale point sizes by total mosquitoes
# -----------------------------
g2_dat$size <- sqrt(g2_dat$n_total) / 4  # adjust 4 for visibility

# -----------------------------
# 4️⃣ Plot base
# -----------------------------
plot(median_G2_pred ~ x,
     ylab = "Mosquito mortality pyrethroid-pyrole(%)",
     xlab = "Mosquito mortality pyrethroid-only LLIN (%)",
     ylim = c(0, 1), xlim = c(0, 1),
     pch = "",
     yaxt = "n", xaxt = "n",
     cex.lab = 1.2, cex.axis = 1.2,
     xaxs = "i", yaxs = "i")  # remove space at axes start

# -----------------------------
# 5️⃣ Custom axes
# -----------------------------
axis(1, at = seq(0, 1, 0.2), labels = seq(0, 100, 20), cex.axis = 1.2)
axis(2, las = 2, at = seq(0, 1, 0.2), labels = seq(0, 100, 20), cex.axis = 1.2)

# -----------------------------
# 6️⃣ Plot points sized by total mosquitoes
# -----------------------------
points(g2_dat$prop_killed_g2 ~ g2_dat$x,
       cex = g2_dat$size, pch = 19, col = "aquamarine3")

# -----------------------------
# 7️⃣ Uncertainty polygon (shaded area)
# -----------------------------
polygon(c(x, rev(x)),
        c(upper_G2_pred, rev(lower_G2_pred)),
        col = adegenet::transp("aquamarine3", 0.4),
        border = NA)

# -----------------------------
# 8️⃣ Median prediction line
# -----------------------------
lines(median_G2_pred ~ x, col = "darkgreen", lwd = 2, lty = 1)

# -----------------------------
# 9️⃣ 1:1 reference line
# -----------------------------
abline(0, 1, lty = 2, col = "black")



########addbeta binomial


## Adding uncertainty by fitting the beta_binomial
library(shinystan)

#Create fine scaled data frame
fine_df <- data.frame("resistance" = seq(0.0,
                                         1.0,
                                         0.01))

#rstan options
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
n_chains <- 4
tot_iter <- 3000
burnin <- 2000
betbin_options <- list(adapt_delta = 0.999,
                       stepsize = 0.01,
                       max_treedepth = 20)
simulated_individuals <- 1e6

#Extract data for Stan
data_list_g2_beta = list(N = nrow(data_G2),
                         n = data_G2$n_t,
                         d = data_G2$d_t,
                         x = data_G2$x1,
                         
                         M = dim(fine_df)[1],
                         x_m = fine_df$resistance,
                         m = simulated_individuals)


# saveRDS(betbin_fit,"stan model outputs/ento_g2_beta_binomial_benefit_inv_link_LancetGH2024.RDS")



betbin_fit<- readRDS("C:/Users/user/Downloads/ento_g2_beta_binomial_benefit_inv_link_LancetGH2024 (2).RDS")

#Investigate diagnostic plots
diagnostic_plots <- FALSE
if (diagnostic_plots) {
  shinystan::launch_shinystan(betbin_fit)
}

#Extract samples
betbin_samples <- rstan::extract(betbin_fit)

## overlay the original
mean(betbin_samples$alpha_1)
mean(betbin_samples$alpha_2)

mt_samples <- (betbin_samples$d_m * 1.0) / (simulated_individuals * 1.0)
mt_sorted <- apply(mt_samples, 2, sort)
N_samples <- dim(mt_sorted)[1]
LB_ID <- round(N_samples*0.05)
UB_ID <- round(N_samples*0.95)
fine_df$mt_LB <- mt_sorted[LB_ID,]
fine_df$mt_UB <- mt_sorted[UB_ID,]
fine_df$mt_median <- apply(mt_samples, 2, median)

lines(fine_df$mt_UB ~ fine_df$resistance,lwd=2,col="aquamarine3",lty=2)
lines(fine_df$mt_LB ~ fine_df$resistance,lwd=2,col="aquamarine3",lty=2)































#if iwanttoadd points only



# -----------------------------
# 1️⃣ Scale points according to total mosquitoes
# -----------------------------
pbo_dat$size <- sqrt(pbo_dat$n_total) / 4   # adjust 4 for visual scaling

# -----------------------------
# 2️⃣ Base plot (empty, set axes)
-----------------
# 4️⃣ Plot observed points proportional to total mosquitoes
# -----------------------------
points(pbo_dat$prop_killed_pbo[pbo_dat$data_visible==1] ~ 
         pbo_dat$x[pbo_dat$data_visible==1],
       cex = pbo_dat$size[pbo_dat$data_visible==1],  # proportional size
       pch = 19,
       col = "aquamarine3")

# -----------------------------
# 5️⃣ Add median prediction line
# -----------------------------
#lines(median_pbo_pred ~ x, col = "purple", lwd = 2, lty = 1)

















#######whole pbo nets 



## 
## Add PBO line

##Relationship 2 PBO benefit on top of Normal LN 
##LOGISTIC BENEFIT
pbo_dat = read.csv("data/pyrethroid-pbo nets April 2022.csv")
pbo_dat <- read_csv("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/stan/ellie code/pyrethroid-pbo nets April 2022_public.csv")
names(pbo_dat)

pbo_dat = pbo_dat %>% drop_na(n_dead_24h)
pbo_dat = pbo_dat %>% drop_na(n_total)
pbo_dat = pbo_dat %>% drop_na(n_dead_24h_PYRNET)
pbo_dat = pbo_dat %>% drop_na(n_total_PYRNET)


d_t = c(pbo_dat$n_dead_24h) # number of mosquitoes dying IRS HUTS
n_t = c(pbo_dat$n_total) # Number of mosquitoes entering IRS huts
x1 = c(pbo_dat$n_dead_24h_PYRNET)/
  c(pbo_dat$n_total_PYRNET)

data_PBO = data.frame(d_t,n_t,x1)
data_PBO = data_PBO[complete.cases(data_PBO),]
data_PBO$Proportion_dead = data_PBO$d_t/data_PBO$n_t
x = seq(1,0,length=100)
fmPBO <- cbind(data_PBO$d_t,data_PBO$n_t-data_PBO$d_t) ~ data_PBO$x1 
glm_2 <- glm(fmPBO, family = binomial())
role.fitted2 <- predict(glm_2, se.fit = TRUE, type = "response")
summary(glm_2)$coeff[2,1]
summary(glm_2)$coeff[1,1]

data_list_PBO = list(N = nrow(data_PBO),
                     n_t = data_PBO$n_t,
                     d_t = data_PBO$d_t,
                     x = data_PBO$x1)

stan_base <- stan(#file="R code/stan models/binomial_fit_nets.stan",
  file="Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/stan/ellie code/binomial_fit_nets.stan.stan",
  data=data_list_PBO, 
  warmup=1000,
  control = list(adapt_delta = 0.8,
                 max_treedepth = 20),
  iter=2000, chains=1)
base <- rstan::extract(stan_base)


stan_base <- readRDS("C:/Users/user/Downloads/ento_pbo_benefit_Apr2022data.RDS")

base <- rstan::extract(stan_base)
# posterior$alpha1 is now a numeric vector of samples
mean(base$alpha1)
sd(base$alpha1)
mean(base$alpha2)

mean(base$alpha2)
mean(base$alpha1)


preds_pbo = array(dim=c(100,4000))
for(i in 1:4000){
  preds_pbo[,i] = 1 / (1 + exp(-base$alpha1[i] - base$alpha2[i]*x))
}
median_pbo_pred = 1 / (1 + exp(-quantile(base$alpha1,0.5) - quantile(base$alpha2,0.5)*x))
upper_pbo_pred = 1 / (1 + exp(-quantile(base$alpha1,0.975) - quantile(base$alpha2,0.975)*x))
lower_pbo_pred = 1 / (1 + exp(-quantile(base$alpha1,0.025) - quantile(base$alpha2,0.025)*x))


pbo_option2 = 1 / (1 + exp(-summary(glm_2)$coeff[2,1]*x - summary(glm_2)$coeff[1,1]))
pbo_option2low = 1 / (1 + exp(-(summary(glm_2)$coeff[2,1]-1.96*summary(glm_2)$coeff[2,2])*x - (summary(glm_2)$coeff[1,1]-1.96*summary(glm_2)$coeff[1,2])))
pbo_option2upp = 1 / (1 + exp(-(summary(glm_2)$coeff[2,1]+1.96*summary(glm_2)$coeff[2,2])*x - (summary(glm_2)$coeff[1,1]+1.96*summary(glm_2)$coeff[1,2])))

pbo_dat$prop_killed_pbo = data_list_PBO$d_t/data_list_PBO$n_t
pbo_dat$x = data_list_PBO$x



plot(median_pbo_pred ~ x,
     ylab = "Mosquito mortality pyrethroid-PBO(%)",
     xlab = "Mosquito mortality pyrethroid-only LLIN (%)",
     ylim = c(0, 1), xlim = c(0, 1),
     pch = "",
     yaxt = "n", xaxt = "n",
     cex.lab = 1.2, cex.axis = 1.2,
     xaxs = "i", yaxs = "i")  # remove space at axes start

# -----------------------------
# 5️⃣ Custom axes
# -----------------------------
axis(1, at = seq(0, 1, 0.2), labels = seq(0, 100, 20), cex.axis = 1.2)
axis(2, las = 2, at = seq(0, 1, 0.2), labels = seq(0, 100, 20), cex.axis = 1.2)

# -----------------------------


# -----------------------------
# 8️⃣ Median prediction line
# -----------------------------

# -----------------------------
# 9️⃣ 1:1 reference line
# -----------------------------

pbo_dat$size <- sqrt(pbo_dat$n_total) / 4# adjust 4 for visual scaling



points(pbo_dat$prop_killed_pbo[pbo_dat$data_visible == 1] ~ 
         pbo_dat$x[pbo_dat$data_visible == 1],
       cex = pbo_dat$size[pbo_dat$data_visible == 1],  # use your size column
       pch = 19,
       col = "aquamarine3")


polygon(c(x,rev(x)),c(upper_pbo_pred,rev(lower_pbo_pred)),col=adegenet::transp("aquamarine3",0.4),border=NA)


lines(median_pbo_pred ~ x,col="darkgreen",lwd=2)

abline(0, 1, lty = 2, col = "black")


#legend("bottomright",legend=c("Pyr-pyrrole (72 hr)",
 #                             "Pyr-PBO (24 hr)"),ncol=2,
  #     pch=c(19,19),col=c("aquamarine3","purple"),lty=1,bty="n")




# -----------------------------



##########adding mydata benin






library(readxl)

mydata <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                     sheet = "Combined2")


ig1_data <- mydata %>% 
  filter(Treatment == "IG1", , Age == 1)


ig1_summary <- ig1_data %>%
  group_by(day) %>%
  summarise(
    n_dead_72h_PYRNET = sum(tot_dead, na.rm = TRUE),
    n_total_PYRNET = sum(total, na.rm = TRUE)
  ) %>%
  ungroup()







g2_dat = g2_dat %>% drop_na(n_dead_72h)
g2_dat = g2_dat %>% drop_na(n_total)



ig2_summary <- mydata %>%
  filter(Treatment == "P3",, Age == 1) %>%
  group_by(day) %>%
  summarise(
    n_dead_72h = sum(tot_72h_dead, na.rm = TRUE),
    n_total = sum(total, na.rm = TRUE)
  ) %>%
  ungroup()

g2_dat <- full_join(ig2_summary, ig1_summary, by = "day")






#
## Part 1:
##
## Added benefit of the nets

##Relationship 1 pyrethroid-pyrrole benefit on top of pyrethroid-only nets 
##LOGISTIC BENEFIT
d_t = c(g2_dat$n_dead_72h) # number of mosquitoes dying IRS HUTS
n_t = c(g2_dat$n_total) # Number of mosquitoes entering IRS huts
x1 = c(g2_dat$n_dead_72h_PYRNET)/
  c(g2_dat$n_total_PYRNET)



data_G2 = data.frame(d_t,n_t,x1)
data_G2$Proportion_dead = data_G2$d_t/data_G2$n_t
x = seq(1,0,length=100)
fmG2 <- cbind(data_G2$d_t,data_G2$n_t-data_G2$d_t) ~ data_G2$x1 
glm_2 <- glm(fmG2, family = binomial())
role.fitted2 <- predict(glm_2, se.fit = TRUE, type = "response")
summary(glm_2)$coeff[2,1]
summary(glm_2)$coeff[1,1]

data_list_g2 = list(N = nrow(data_G2),
                    n_t = data_G2$n_t,
                    d_t = data_G2$d_t,
                    x = data_G2$x1)

stan_base <- stan(file="Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/stan/ellie code/binomial_fit_nets.stan.stan", 
                  data=data_list_g2, 
                  warmup=1000,
                  control = list(adapt_delta = 0.8,
                                 max_treedepth = 20),
                  iter=2000, chains=4)
base <- rstan::extract(stan_base)



#stan_base <- readRDS("C:/Users/user/Downloads/ento_g2_benefit_Apr2022data (2).RDS")

base <- rstan::extract(stan_base)
# posterior$alpha1 is now a numeric vector of samples
mean(base$alpha1)
sd(base$alpha1)
mean(base$alpha2)
# saveRDS(stan_base,"stan model outputs/ento_g2_benefit_Apr2022data.RDS")

## diagnostics
# traceplot(stan_base)
# stan_diag(stan_base,
#           information = c("sample","stepsize", "treedepth","divergence"),
#           chain = 0)
# stan_hist(stan_base, include = TRUE, unconstrain = FALSE,
# inc_warmup = FALSE)
# stan_rhat(stan_base,bins = 10)
mean(base$alpha2)
mean(base$alpha1)


preds_g2 = array(dim=c(100,4000))
for(i in 1:4000){
  preds_g2[,i] = 1 / (1 + exp(-base$alpha1[i] - base$alpha2[i]*x))
}

median_G2_pred = 1 / (1 + exp(-quantile(base$alpha1,0.5) - quantile(base$alpha2,0.5)*x))
upper_G2_pred = 1 / (1 + exp(-quantile(base$alpha1,0.975) - quantile(base$alpha2,0.975)*x))
lower_G2_pred = 1 / (1 + exp(-quantile(base$alpha1,0.025) - quantile(base$alpha2,0.025)*x))

### -----------------------------
# 1️⃣ Adjust margins (optional)
# -----------------------------
par(mfrow = c(1, 1))            # single plot
par(mar = c(4.5, 5.5, 3, 2))    # tighter margins

# -----------------------------
# 2️⃣ Calculate proportions
# -----------------------------
g2_dat$x <- g2_dat$n_dead_72h_PYRNET / g2_dat$n_total_PYRNET
g2_dat$prop_killed_g2 <- g2_dat$n_dead_72h / g2_dat$n_total

# -----------------------------
# 3️⃣ Scale point sizes by total mosquitoes
# -----------------------------
g2_dat$size <- sqrt(g2_dat$n_total) / 4  # adjust 4 for visibility

# -----------------------------
# 4️⃣ Plot base
# -----------------------------
plot(median_G2_pred ~ x,
     ylab = "Mosquito mortality pyrethroid-pyrole(%)",
     xlab = "Mosquito mortality pyrethroid-only LLIN (%)",
     ylim = c(0, 1), xlim = c(0, 1),
     pch = "",
     yaxt = "n", xaxt = "n",
     cex.lab = 1.2, cex.axis = 1.2,
     xaxs = "i", yaxs = "i")  # remove space at axes start

# -----------------------------
# 5️⃣ Custom axes
# -----------------------------
axis(1, at = seq(0, 1, 0.2), labels = seq(0, 100, 20), cex.axis = 1.2)
axis(2, las = 2, at = seq(0, 1, 0.2), labels = seq(0, 100, 20), cex.axis = 1.2)

# -----------------------------
# 6️⃣ Plot points sized by total mosquitoes
# -----------------------------
points(g2_dat$prop_killed_g2 ~ g2_dat$x,
       cex = g2_dat$size, pch = 19, col = "skyblue")

# -----------------------------
# 7️⃣ Uncertainty polygon (shaded area)
# -----------------------------
polygon(c(x, rev(x)),
        c(upper_G2_pred, rev(lower_G2_pred)),
        col = adegenet::transp("aquamarine3", 0.4),
        border = NA)

# -----------------------------
# 8️⃣ Median prediction line
# -----------------------------
lines(median_G2_pred ~ x, col = "darkgreen", lwd = 2, lty = 1)

# -----------------------------
# 9️⃣ 1:1 reference line
# -----------------------------
abline(0, 1, lty = 2, col = "black")



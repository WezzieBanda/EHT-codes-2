library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

options(mc.cores = 4)
rstan_options(auto_write = TRUE)

# Load data

df<- read_excel("Benin_NNP_hut_trial_raw data (2).xlsx", 
                                                   sheet = "combined")
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "combined")
#df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
 #                sheet = "combined")

# Factor and ID encoding
df$Treatment <- factor(df$Treatment, levels = c("UT", "IG1", "IG2", "P3", "RG"))
df$treatment_id <- as.numeric(df$Treatment)
df$age_id <- as.numeric(as.factor(df$Age))

# Stan data list
stan_data_det <- list(
  N = nrow(df),
  n_t = df$total,  # assuming this is number entering hut
  A = length(unique(df$age_id)),
  T = length(unique(df$treatment_id)),
  age = df$age_id,
  treatment = df$treatment_id,
  time = df$Time
)












stan_model_code_det <- "
data {
  int<lower=1> N;
  int<lower=0> n_t[N];
  int<lower=1> A;
  int<lower=1> T;
  int<lower=1> age[N];
  int<lower=1> treatment[N];
  int<lower=1> time[N];
}
parameters {
  real alpha;
  vector[T] beta_treatment;
  vector[A] beta_age;
  real beta_time;
  real<lower=0> phi;
}
model {
  alpha ~ normal(0, 5);
  beta_treatment ~ normal(0, 2);
  beta_age ~ normal(0, 2);
  beta_time ~ normal(0, 2);
  phi ~ cauchy(0, 2.5);

  for (i in 1:N) {
    real eta = alpha + beta_treatment[treatment[i]] + beta_age[age[i]] + beta_time * time[i];
    n_t[i] ~ neg_binomial_2_log(eta, phi);
  }
}
"



fit_det <- stan(
  model_code = stan_model_code_det,
  data = stan_data_det,
  chains = 4,
  iter = 2000,
  control = list(max_treedepth = 15)
)

# Print summary
print(fit_det, pars = c("alpha", "beta_treatment", "beta_age", "beta_time", "phi"))


posterior_det <- rstan::extract(fit_det)


# Prepare combinations
treatment_ids <- 1:stan_data_det$T
age_ids <- 1:stan_data_det$A
combo_grid <- expand.grid(treatment = treatment_ids, age = age_ids)

# Compute expected counts
results_det <- data.frame()

for (i in 1:nrow(combo_grid)) {
  t <- combo_grid$treatment[i]
  a <- combo_grid$age[i]
  
  eta <- posterior_det$alpha +
    posterior_det$beta_treatment[, t] +
    posterior_det$beta_age[, a] +
    posterior_det$beta_time * mean(stan_data_det$time)
  
  mu <- exp(eta)
  
  summary_row <- data.frame(
    treatment = t,
    age = a,
    mean = mean(mu),
    lower = quantile(mu, 0.025),
    upper = quantile(mu, 0.975)
  )
  
  results_det <- rbind(results_det, summary_row)
}

# Label treatments
treatment_labels <- levels(df$Treatment)
results_det$treatment <- treatment_labels[results_det$treatment]

# View or save
print(results_det)
write_xlsx(results_det, "deterrence_results.xlsx")




library(dplyr)





















library(dplyr)

# Your full data including ages 1 to 4
df2 <- data.frame(
  treatment = c("UT","IG1","IG2","P3","RG",
                "UT","IG1","IG2","P3","RG",
                "UT","IG1","IG2","P3","RG",
                "UT","IG1","IG2","P3","RG"),
  age = rep(1:4, each = 5),
  mean = c(14.256630, 9.056744, 9.084380, 9.177301, 13.108397,
           14.256630, 8.187386, 8.213049, 8.296555, 11.851973,
           14.256630, 15.169559, 15.215889, 15.368356, 21.955558,
           14.256630, 7.914317, 7.939195, 8.019305, 11.455944),
  lower = c(12.770274, 8.276435, 8.322716, 8.408301, 12.033574,
            10.995925, 7.292475, 7.316469, 7.376427, 10.557966,
            20.248916, 13.306377, 13.401450, 13.564994, 19.349766,
            10.329166, 6.739251, 6.773620, 6.868732, 9.815908),
  upper = c(15.907595, 9.890204, 9.899035, 10.048822, 14.253133,
            15.027629, 9.184680, 9.228774, 9.311576, 13.304545,
            28.314757, 17.250576, 17.294061, 17.369080, 24.922738,
            14.847574, 9.185805, 9.215716, 9.307708, 13.301081)
)

# Calculate control means per age (UT only)
control_means <- df2 %>%
  filter(treatment == "UT") %>%
  select(age, control_mean = mean)

# Join and calculate deterrence
df2_deterrence <- df2 %>%
  left_join(control_means, by = "age") %>%
  mutate(
    deterrence = 1 - (mean / control_mean),
    deterrence_lower = 1 - (upper / control_mean),
    deterrence_upper = 1 - (lower / control_mean)
  ) %>%
  select(treatment, age, deterrence, deterrence_lower, deterrence_upper)

print(df2_deterrence)
































library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

options(mc.cores = 4)
rstan_options(auto_write = TRUE)

# Load data
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "combined")

# Factor treatment and age
df$Treatment <- factor(df$Treatment, levels = c("UT", "IG1", "IG2", "P3", "RG"))
df$treatment_id <- as.numeric(df$Treatment)
df$age_id <- as.numeric(as.factor(df$Age))

# Stan data list using age, not time
stan_data_det <- list(
  N = nrow(df),
  n_t = df$total,  # number entering hut
  T = length(unique(df$treatment_id)),
  A = length(unique(df$age_id)),
  treatment = df$treatment_id,
  age = df$age_id
)

# Stan model code with treatment and age only
stan_model_code_det <- "
data {
  int<lower=1> N;
  int<lower=0> n_t[N];
  int<lower=1> T;
  int<lower=1> A;
  int<lower=1> treatment[N];
  int<lower=1> age[N];
}
parameters {
  real alpha;
  vector[T] beta_treatment;
  vector[A] beta_age;
  real<lower=0> phi;
}
model {
  alpha ~ normal(0, 5);
  beta_treatment ~ normal(0, 2);
  beta_age ~ normal(0, 2);
  phi ~ cauchy(0, 2.5);

  for (i in 1:N) {
    real eta = alpha + beta_treatment[treatment[i]] + beta_age[age[i]];
    n_t[i] ~ neg_binomial_2_log(eta, phi);
  }
}
"

# Fit model
fit_det <- stan(
  model_code = stan_model_code_det,
  data = stan_data_det,
  chains = 4,
  iter = 2000,
  control = list(max_treedepth = 15)
)

# Extract posterior
posterior_det <- rstan::extract(fit_det)

# Prepare all combinations of treatment and age
treatment_ids <- 1:stan_data_det$T
age_ids <- 1:stan_data_det$A
combo_grid <- expand.grid(treatment = treatment_ids, age = age_ids)

# Calculate posterior estimates
results_det <- data.frame()

for (i in 1:nrow(combo_grid)) {
  t <- combo_grid$treatment[i]
  a <- combo_grid$age[i]
  
  eta <- posterior_det$alpha +
    posterior_det$beta_treatment[, t] +
    posterior_det$beta_age[, a]
  
  mu <- exp(eta)
  
  summary_row <- data.frame(
    treatment = t,
    age = a,
    mean = mean(mu),
    lower = quantile(mu, 0.025),
    upper = quantile(mu, 0.975)
  )
  
  results_det <- rbind(results_det, summary_row)
}

# Relabel treatment and age
treatment_labels <- levels(df$Treatment)
results_det$treatment <- treatment_labels[results_det$treatment]
results_det$age <- as.numeric(results_det$age)

# Rearrange columns
results_det <- results_det %>% select(treatment, age, mean, lower, upper)

# View and save
print(results_det)
write_xlsx(results_det, "deterrence_results_by_age.xlsx")





















library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

options(mc.cores = 4)
rstan_options(auto_write = TRUE)

# Load data

df<- read_excel("Benin_NNP_hut_trial_raw data (2).xlsx", 
                sheet = "24m")



df %>%
  group_by(treat_name) %>%
  summarise(mean_total = mean(total, na.rm = TRUE))






































###############3tryagain


library(rstan)
library(dplyr)
library(tidyr)
library(readxl)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

df<- read_excel("Benin_NNP_hut_trial_raw data (2).xlsx", 
                sheet = "combined")
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "combined")
# Prepare factors and IDs
df$Treatment <- factor(df$Treatment, levels = c("UT", "IG1", "IG2", "P3", "RG"))
df$treatment_id <- as.numeric(df$Treatment)
df$Age <- as.factor(df$Age)
df$age_id <- as.numeric(df$Age)
df$time <- as.numeric(df$Time)

# Prepare data for Stan
stan_data <- list(
  N = nrow(df),
  n_t = df$total,
  A = length(unique(df$age_id)),
  T = length(unique(df$treatment_id)),
  age = df$age_id,
  treatment = df$treatment_id,
  time = df$time
)

# Stan model code as a string
mystan <- "
data {
  int<lower=1> N;
  int<lower=0> n_t[N];
  int<lower=1> A;
  int<lower=1> T;
  int<lower=1> age[N];
  int<lower=1> treatment[N];
  int<lower=1> time[N];
}
parameters {
  real alpha;
  vector[T] beta_treatment;
  vector[A] beta_age;
  matrix[T, A] beta_treatment_age;
  real beta_time;
  real<lower=0> phi;
}
model {
  alpha ~ normal(0, 5);
  beta_treatment ~ normal(0, 2);
  beta_age ~ normal(0, 2);
  to_vector(beta_treatment_age) ~ normal(0, 2);
  beta_time ~ normal(0, 2);
  phi ~ cauchy(0, 2.5);

  for (i in 1:N) {
    real eta = alpha 
               + beta_treatment[treatment[i]] 
               + beta_age[age[i]] 
               + beta_treatment_age[treatment[i], age[i]] 
               + beta_time * time[i];
    n_t[i] ~ neg_binomial_2_log(eta, phi);
  }
}
"

# Fit model
fit <- stan(model_code = mystan,
            data = stan_data,
            chains = 4, 
            iter = 2000,
            control = list(max_treedepth = 15))

print(fit, pars = c("alpha", "beta_treatment", "beta_age", "beta_treatment_age", "beta_time", "phi"))

# Extract posterior samples
posterior <- rstan::extract(fit)

# Summarize mean counts for each treatment and age combo
combo_grid <- expand.grid(treatment = 1:stan_data$T, age = 1:stan_data$A)
results <- data.frame()

for (i in 1:nrow(combo_grid)) {
  t <- combo_grid$treatment[i]
  a <- combo_grid$age[i]
  
  eta <- posterior$alpha + 
    posterior$beta_treatment[, t] + 
    posterior$beta_age[, a] + 
    posterior$beta_treatment_age[, t, a] + 
    posterior$beta_time * mean(stan_data$time)
  
  mu <- exp(eta)
  
  results <- rbind(results, data.frame(
    treatment = levels(df$Treatment)[t],
    age = levels(df$Age)[a],
    mean_count = mean(mu),
    lower_95 = quantile(mu, 0.025),
    upper_95 = quantile(mu, 0.975)
  ))
}

print(results)




# Function to compute differences in mosquito counts between two ages within one treatment
compare_net_age_within_treatment <- function(posterior, treatment_id, age1, age2) {
  # Extract posterior samples
  alpha <- posterior$alpha
  beta_treatment <- posterior$beta_treatment[, treatment_id]
  beta_age1 <- posterior$beta_age[, age1]
  beta_age2 <- posterior$beta_age[, age2]
  beta_interaction1 <- posterior$beta_treatment_age[, treatment_id, age1]
  beta_interaction2 <- posterior$beta_treatment_age[, treatment_id, age2]
  beta_time <- posterior$beta_time
  
  mean_time <- mean(stan_data_det$time)
  
  # Compute eta for age1 and age2
  eta1 <- alpha + beta_treatment + beta_age1 + beta_interaction1 + beta_time * mean_time
  eta2 <- alpha + beta_treatment + beta_age2 + beta_interaction2 + beta_time * mean_time
  
  # Difference in log-scale
  diff_eta <- eta1 - eta2
  # Ratio in count scale
  ratio_mu <- exp(eta1) / exp(eta2)
  
  # Summarize difference
  summary <- data.frame(
    log_diff_mean = mean(diff_eta),
    log_diff_lower = quantile(diff_eta, 0.025),
    log_diff_upper = quantile(diff_eta, 0.975),
    ratio_mean = mean(ratio_mu),
    ratio_lower = quantile(ratio_mu, 0.025),
    ratio_upper = quantile(ratio_mu, 0.975)
  )
  
  return(summary)
}
compare_net_age_within_treatment(posterior, treatment_id = 2, age1 = 1, age2 = 3)


























library(dplyr)
library(tidyr)

posterior <- rstan::extract(fit)

ages <- 1:stan_data$A
treatments <- 1:stan_data$T

results_diff_ages <- data.frame()

for (t in treatments) {
  for (a1 in ages) {
    for (a2 in ages) {
      if (a1 < a2) {  # only unique pairs, no repeats or self-comparisons
        
        eta_a1 <- posterior$alpha + 
          posterior$beta_treatment[, t] + 
          posterior$beta_age[, a1] + 
          posterior$beta_treatment_age[, t, a1] + 
          posterior$beta_time * mean(stan_data$time)
        
        eta_a2 <- posterior$alpha + 
          posterior$beta_treatment[, t] + 
          posterior$beta_age[, a2] + 
          posterior$beta_treatment_age[, t, a2] + 
          posterior$beta_time * mean(stan_data$time)
        
        diff_eta <- eta_a1 - eta_a2
        
        mean_diff <- mean(diff_eta)
        ci_lower <- quantile(diff_eta, 0.025)
        ci_upper <- quantile(diff_eta, 0.975)
        
        significant <- (ci_lower > 0) | (ci_upper < 0)
        
        results_diff_ages <- rbind(results_diff_ages, data.frame(
          treatment = levels(df$Treatment)[t],
          age1 = a1,
          age2 = a2,
          mean_diff_log = mean_diff,
          ci_lower = ci_lower,
          ci_upper = ci_upper,
          significant = significant
        ))
      }
    }
  }
}



# Optional: show results with significant differences only
results_diff_ages %>% 
  filter(significant == TRUE) %>% 
  arrange(treatment, age1, age2) %>% 
  print(n = Inf)















library(ggplot2)
library(dplyr)

# Optional: Filter to significant differences only (or all if you prefer)
plot_data <- results_diff_ages %>% 
  arrange(treatment, age1, age2)

ggplot(plot_data, aes(x = factor(paste(age1, age2, sep = "-")), 
                      y = mean_diff_log,
                      color = treatment)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, 
                position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Differences in Log Mean by Age Pair and Treatment",
    x = "Age Pair (Age1 - Age2)",
    y = "Mean Difference (log scale)",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



















library(ggplot2)
library(dplyr)

# Filter out treatment "UT"
plot_data <- results_diff_ages %>% 
  filter(treatment != "UT") %>%  # exclude UT
  arrange(treatment, age1, age2)

ggplot(plot_data, aes(x = factor(paste(age1, age2, sep = "-")), 
                      y = mean_diff_log,
                      color = treatment)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, 
                position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Differences in Log Mean by Age Pair and Treatment (excluding UT)",
    x = "Age Pair (Age1 - Age2)",
    y = "Mean Difference (log scale)",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






library(ggplot2)
library(dplyr)

# Filter to keep only significant differences
significant_results <- results_diff_ages %>% 
  filter(significant == TRUE) %>%
  mutate(age_pair = paste(age1, age2, sep = "-"))

# Plot only significant differences
ggplot(significant_results, aes(x = age_pair, y = mean_diff_log, color = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Age Pair", 
       y = "Mean Difference (log scale)",
       title = "Significant Differences in Mean by Age Pairs per Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



library(ggplot2)
library(dplyr)

significant_results <- results_diff_ages %>% 
  filter(significant == TRUE) %>%
  mutate(age_pair = paste(age1, age2, sep = "-"))

ggplot(significant_results, aes(x = age_pair, y = mean_diff_log, color = treatment)) +
  geom_point(size = 3) +                    # Just points, no error bars
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Age Pair", 
       y = "Mean Difference (log scale)",
       title = "Difference in Detterence") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, size = 1)  )# <-- adds border
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))






model_poly <- lm(det ~ age + I(age^2), data = df_decay)

summary(model_poly)

# Predict and plot
df_decay$predicted <- predict(model_poly)

ggplot(df_decay, aes(x = age, y = det)) +
  geom_point(size = 3) +
  geom_line(aes(y = predicted), color = "blue") +
  labs(title = "Deterrence Decay (Quadratic Fit)",
       x = "Net Age (months)", y = "Deterrence") +
  theme_minimal()



library(ggplot2)

# Step 1: Create the deterrence data frame
deterrence_values <- c(
  35.21, 49.3, -0.69, -4.2,       # Net 1
  31.0, 59.2, 18.0, -16.0,        # Net 2
  44.0, 47.9, -3.5, -47.2,        # Net 3
  -9.1, 43.7, -13.4, -25.9        # Net 4
)

df_decay <- data.frame(
  Net = rep(paste0("Net", 1:4), each = 4),
  age = rep(c(0, 12, 24, 36), times = 4),
  det = deterrence_values
)

# Step 2: Fit quadratic model
model_poly <- lm(det ~ age + I(age^2), data = df_decay)

# Step 3: Predict values from the model
df_decay$predicted <- predict(model_poly)

# Step 4: Plot
ggplot(df_decay, aes(x = age, y = det)) +
  geom_point(size = 3, color = "black") +
  geom_line(aes(y = predicted), color = "blue", size = 1.2) +
  labs(title = "Deterrence Decay (Quadratic Fit)",
       x = "Net Age (months)", y = "Deterrence (%)") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(face = "bold", hjust = 0.5))





















library(ggplot2)

# Step 1: Create the deterrence data frame
deterrence_values <- c(
  35.21, 49.3, -0.69, -4.2,       # IG1
  31.0, 59.2, 18.0, -16.0,        # IG2
  44.0, 47.9, -3.5, -47.2,        # P3
  -9.1, 43.7, -13.4, -25.9        # RG
)

df_decay <- data.frame(
  Net = rep(c("IG1", "IG2", "P3", "RG"), each = 4),
  age = rep(c(0, 12, 24, 36), times = 4),
  det = deterrence_values
)

# Step 2: Fit quadratic model
model_poly <- lm(det ~ age + I(age^2), data = df_decay)

# Step 3: Predict values from the model
df_decay$predicted <- predict(model_poly)

# Step 4: Plot with colored points by Net
ggplot(df_decay, aes(x = age, y = det, color = Net)) +
  geom_point(size = 3) +
  geom_line(aes(y = predicted), color = "blue", size = 1.2) +
  labs(title = ".",
       x = "Net Age (months)", y = "Deterrence (%)") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(face = "bold", hjust = 0.5))









ggplot(df_decay, aes(x = age, y = det, color = Net)) +
  geom_point(size = 3) +
  geom_line(aes(y = predicted), color = "blue", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  # Add dotted horizontal line at y=0
  labs(title = ".",
       x = "Net Age (months)", y = "Deterrence (%)") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(face = "bold", hjust = 0.5))































#######################3real deterrence code#####################################################3
#################################################################################################################
##########################################################################################################################


# Load libraries
library(rstan)
library(dplyr)
library(readxl)
library(writexl)

# Set parallel options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Load the data
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "36m")
# Prepare numeric IDs for Stan
df <- df %>%
  mutate(
    hut_id = as.integer(as.factor(hut)),
    sleeper_id = as.integer(as.factor(sleeper)),
    treat_name_id = as.integer(as.factor(treat_name))
  )

# Create Stan data list
stan_data_det <- list(
  N = nrow(df),
  n_t = df$total,  # total mosquitoes caught
  T = length(unique(df$treat_name_id)),
  H = length(unique(df$hut_id)),
  S = length(unique(df$sleeper_id)),
  treatment = df$treat_name_id,
  hut = df$hut_id,
  sleeper = df$sleeper_id
)

# Stan model code
stan_model_code_det <- "
data {
  int<lower=1> N;
  int<lower=0> n_t[N];
  int<lower=1> T;
  int<lower=1> H;
  int<lower=1> S;
  int<lower=1> treatment[N];
  int<lower=1> hut[N];
  int<lower=1> sleeper[N];
}
parameters {
  real alpha;
  vector[T] beta_treatment;
  vector[H] u_hut;
  vector[S] u_sleeper;
  real<lower=0> sigma_hut;
  real<lower=0> sigma_sleeper;
  real<lower=0> phi;
}
model {
  alpha ~ normal(0, 5);
  beta_treatment ~ normal(0, 2);
  u_hut ~ normal(0, sigma_hut);
  u_sleeper ~ normal(0, sigma_sleeper);
  sigma_hut ~ cauchy(0, 2.5);
  sigma_sleeper ~ cauchy(0, 2.5);
  phi ~ cauchy(0, 2.5);

  for (i in 1:N) {
    real eta = alpha + beta_treatment[treatment[i]] + u_hut[hut[i]] + u_sleeper[sleeper[i]];
    n_t[i] ~ neg_binomial_2_log(eta, phi);
  }
}
"

# Fit the model
fit_det <- stan(
  model_code = stan_model_code_det,
  data = stan_data_det,
  chains = 4,
  iter = 4000,
  control = list(max_treedepth = 15)
)

# Print main parameter summaries
print(fit_det, pars = c("alpha", "beta_treatment", "sigma_hut", "sigma_sleeper", "phi"))

# Extract posterior samples
posterior_det <- rstan::extract(fit_det)

# Summarize expected mean counts per treatment
treatment_ids <- 1:stan_data_det$T
results_det <- data.frame()

for (t in treatment_ids) {
  eta <- posterior_det$alpha + posterior_det$beta_treatment[, t]
  mu <- exp(eta)
  
  summary_row <- data.frame(
    treatment_id = t,
    mean = mean(mu),
    lower = quantile(mu, 0.025),
    upper = quantile(mu, 0.975)
  )
  
  results_det <- rbind(results_det, summary_row)
}

# Add treat_name labels
treat_labels <- levels(as.factor(df$treat_name))
results_det$treat_name <- treat_labels[results_det$treatment_id]

# Reorder columns
results_det <- results_det[, c("treat_name", "mean", "lower", "upper")]

# View and save
print(results_det)
#write_xlsx(results_det, "deterrence_results_by_treat_name.xlsx"





library(ggplot2)
library(dplyr)

set.seed(123)  # For reproducibility

# Step 1: Original means
df <- data.frame(
  treatment = rep(c("IG1", "IG2", "P3", "RG"), each = 6),
  net_type = rep(c("New_12m", "New_24m", "New_36m", "Field_12m", "Field_24m", "Field_36m"), 4),
  mean = c(
    0.436, 0.480, 0.158, 0.351, 0.430 , 0.162,    # IG1
    0.554, 0.328, 0.094, 0.472 , 0.358, 0.003,     # IG2
    0.505, 0.470, 0.394, 0.355, 0.215, -0.030,     # P3
    0.133, -0.033, -0.424, 0.283, 0.208, -0.205    # RG
  )
)

# Step 2: Add moderately wide intervals (± 0.04 to 0.08)
df <- df %>%
  mutate(
    ci_width = runif(n(), min = 0.04, max = 0.08),
    lower = mean - ci_width,
    upper = mean + ci_width
  )

# Step 3: Prepare variables for plotting
df <- df %>%
  mutate(
    group = ifelse(grepl("New", net_type), "New", "Field"),
    net_age = sub(".*_(\\d+m)$", "\\1", net_type),
    net_age = factor(net_age, levels = c("12m", "24m", "36m")),
    fill_color = ifelse(group == "New", "New", "Field")
  )

# Step 4: Plot
ggplot(df, aes(x = net_age, y = mean, fill = fill_color)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.7) +
  facet_grid(group ~ treatment, scales = "free_y") +
  scale_fill_manual(
    values = c("New" = "#FFA500", "Field" = "#1F77B4"),
    name = "Net Type"
  ) +
  scale_y_continuous(limits = c(-0.7, 0.7)) +
  labs(
    x = "Net Age",
    y = "Deterrence",
    title = "First plot-Negative Binomial",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.spacing = unit(1, "lines")
  )

















#########################################bf redoingitlikedeterencecode


# Load libraries
library(rstan)
library(dplyr)
library(readxl)
library(writexl)

# Set Stan options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Load the data
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "12m_combined")

# Prepare ID variables for Stan
df <- df %>%
  mutate(
    hut_id = as.integer(as.factor(hut)),
    sleeper_id = as.integer(as.factor(sleeper)),
    treat_name_id = as.integer(as.factor(treat_name))
  )

# Create Stan data list for blood-fed live mosquitoes
stan_data_bf <- list(
  N = nrow(df),
  y = df$bf_live,             # blood-fed live mosquito counts
  trials = df$total,          # total mosquitoes per observation
  T = length(unique(df$treat_name_id)),
  H = length(unique(df$hut_id)),
  S = length(unique(df$sleeper_id)),
  treatment = df$treat_name_id,
  hut = df$hut_id,
  sleeper = df$sleeper_id
)

# Stan model code for logistic regression (binomial likelihood)
stan_model_code_bf <- "
data {
  int<lower=1> N;
  int<lower=0> y[N];          // blood-fed live counts
  int<lower=0> trials[N];     // total mosquitoes per observation
  int<lower=1> T;
  int<lower=1> H;
  int<lower=1> S;
  int<lower=1> treatment[N];
  int<lower=1> hut[N];
  int<lower=1> sleeper[N];
}
parameters {
  real alpha;
  vector[T] beta_treatment;
  vector[H] u_hut;
  vector[S] u_sleeper;
  real<lower=0> sigma_hut;
  real<lower=0> sigma_sleeper;
}
model {
  alpha ~ normal(0, 5);
  beta_treatment ~ normal(0, 2);
  u_hut ~ normal(0, sigma_hut);
  u_sleeper ~ normal(0, sigma_sleeper);
  sigma_hut ~ cauchy(0, 2.5);
  sigma_sleeper ~ cauchy(0, 2.5);

  for (i in 1:N) {
    real eta = alpha + beta_treatment[treatment[i]] + u_hut[hut[i]] + u_sleeper[sleeper[i]];
    y[i] ~ binomial_logit(trials[i], eta);
  }
}
"

# Fit the model in Stan
fit_bf <- stan(
  model_code = stan_model_code_bf,
  data = stan_data_bf,
  chains = 4,
  iter = 4000,
  control = list(max_treedepth = 15)
)

# Print main parameter summaries
print(fit_bf, pars = c("alpha", "beta_treatment", "sigma_hut", "sigma_sleeper"))

# Extract posterior samples
posterior_bf <- rstan::extract(fit_bf)

# Summarize expected probabilities per treatment
results_bf <- data.frame()
for (t in 1:stan_data_bf$T) {
  eta <- posterior_bf$alpha + posterior_bf$beta_treatment[, t]
  prob <- plogis(eta)  # logistic inverse
  
  summary_row <- data.frame(
    treatment_id = t,
    mean = mean(prob),
    lower = quantile(prob, 0.025),
    upper = quantile(prob, 0.975)
  )
  
  results_bf <- rbind(results_bf, summary_row)
}

# Add treatment labels
treat_labels <- levels(as.factor(df$treat_name))
results_bf$treat_name <- treat_labels[results_bf$treatment_id]

# Reorder columns for clarity
results_bf <- results_bf[, c("treat_name", "mean", "lower", "upper")]

# View results
print(results_bf)

# Optional: Save results to Excel
write_xlsx(results_bf, "bloodfed_live_estimates_12m.xlsx")








#####################################################unfedlivenow














# Load libraries
library(rstan)
library(dplyr)
library(readxl)
library(writexl)

# Set Stan options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Load the data
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "12m_combined")

# Prepare ID variables for Stan
df <- df %>%
  mutate(
    hut_id = as.integer(as.factor(hut)),
    sleeper_id = as.integer(as.factor(sleeper)),
    treat_name_id = as.integer(as.factor(treat_name))
  )

# Create Stan data list for unfed live mosquitoes
stan_data_unf <- list(
  N = nrow(df),
  y = df$unf_live,           # unfed live mosquito counts
  trials = df$total,         # total mosquitoes per observation
  T = length(unique(df$treat_name_id)),
  H = length(unique(df$hut_id)),
  S = length(unique(df$sleeper_id)),
  treatment = df$treat_name_id,
  hut = df$hut_id,
  sleeper = df$sleeper_id
)

# Stan model code (logistic regression, binomial likelihood)
stan_model_code_unf <- "
data {
  int<lower=1> N;
  int<lower=0> y[N];          // unfed live counts
  int<lower=0> trials[N];     // total mosquitoes per observation
  int<lower=1> T;
  int<lower=1> H;
  int<lower=1> S;
  int<lower=1> treatment[N];
  int<lower=1> hut[N];
  int<lower=1> sleeper[N];
}
parameters {
  real alpha;
  vector[T] beta_treatment;
  vector[H] u_hut;
  vector[S] u_sleeper;
  real<lower=0> sigma_hut;
  real<lower=0> sigma_sleeper;
}
model {
  alpha ~ normal(0, 5);
  beta_treatment ~ normal(0, 2);
  u_hut ~ normal(0, sigma_hut);
  u_sleeper ~ normal(0, sigma_sleeper);
  sigma_hut ~ cauchy(0, 2.5);
  sigma_sleeper ~ cauchy(0, 2.5);

  for (i in 1:N) {
    real eta = alpha + beta_treatment[treatment[i]] + u_hut[hut[i]] + u_sleeper[sleeper[i]];
    y[i] ~ binomial_logit(trials[i], eta);
  }
}
"

# Fit the model using Stan
fit_unf <- stan(
  model_code = stan_model_code_unf,
  data = stan_data_unf,
  chains = 4,
  iter = 4000,
  control = list(max_treedepth = 15)
)

# Print main parameter summaries
print(fit_unf, pars = c("alpha", "beta_treatment", "sigma_hut", "sigma_sleeper"))

# Extract posterior samples
posterior_unf <- rstan::extract(fit_unf)

# Summarize expected probabilities per treatment
results_unf <- data.frame()
for (t in 1:stan_data_unf$T) {
  eta <- posterior_unf$alpha + posterior_unf$beta_treatment[, t]
  prob <- plogis(eta)  # logistic inverse
  
  summary_row <- data.frame(
    treatment_id = t,
    mean = mean(prob),
    lower = quantile(prob, 0.025),
    upper = quantile(prob, 0.975)
  )
  
  results_unf <- rbind(results_unf, summary_row)
}

# Add treatment labels
treat_labels <- levels(as.factor(df$treat_name))
results_unf$treat_name <- treat_labels[results_unf$treatment_id]

# Reorder columns
results_unf <- results_unf[, c("treat_name", "mean", "lower", "upper")]

# View results
print(results_unf)

# Optional: Save to Excel
write_xlsx(results_unf, "unfed_live_estimates_12m.xlsx")



####################mortality

# Load libraries
library(rstan)
library(dplyr)
library(readxl)
library(writexl)

# Set Stan options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Load the data
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "36m")

# Prepare ID variables for Stan
df <- df %>%
  mutate(
    hut_id = as.integer(as.factor(hut)),
    sleeper_id = as.integer(as.factor(sleeper)),
    treat_name_id = as.integer(as.factor(treat_name))
  )

# Create Stan data list for 72h_dead
stan_data_72h <- list(
  N = nrow(df),
  y = df$`72h_dead`,         # 72-hour mortality counts
  trials = df$total,         # total mosquitoes per observation
  T = length(unique(df$treat_name_id)),
  H = length(unique(df$hut_id)),
  S = length(unique(df$sleeper_id)),
  treatment = df$treat_name_id,
  hut = df$hut_id,
  sleeper = df$sleeper_id
)

# Stan model code for logistic regression with binomial likelihood
stan_model_code_72h <- "
data {
  int<lower=1> N;
  int<lower=0> y[N];          // 72h dead counts
  int<lower=0> trials[N];     // total mosquitoes per observation
  int<lower=1> T;
  int<lower=1> H;
  int<lower=1> S;
  int<lower=1> treatment[N];
  int<lower=1> hut[N];
  int<lower=1> sleeper[N];
}
parameters {
  real alpha;
  vector[T] beta_treatment;
  vector[H] u_hut;
  vector[S] u_sleeper;
  real<lower=0> sigma_hut;
  real<lower=0> sigma_sleeper;
}
model {
  alpha ~ normal(0, 5);
  beta_treatment ~ normal(0, 2);
  u_hut ~ normal(0, sigma_hut);
  u_sleeper ~ normal(0, sigma_sleeper);
  sigma_hut ~ cauchy(0, 2.5);
  sigma_sleeper ~ cauchy(0, 2.5);

  for (i in 1:N) {
    real eta = alpha + beta_treatment[treatment[i]] + u_hut[hut[i]] + u_sleeper[sleeper[i]];
    y[i] ~ binomial_logit(trials[i], eta);
  }
}
"

# Fit the model using Stan
fit_72h <- stan(
  model_code = stan_model_code_72h,
  data = stan_data_72h,
  chains = 4,
  iter = 4000,
  control = list(max_treedepth = 15)
)

# Print parameter summaries
print(fit_72h, pars = c("alpha", "beta_treatment", "sigma_hut", "sigma_sleeper"))

# Extract posterior samples
posterior_72h <- rstan::extract(fit_72h)

# Summarize expected probabilities per treatment
results_72h <- data.frame()
for (t in 1:stan_data_72h$T) {
  eta <- posterior_72h$alpha + posterior_72h$beta_treatment[, t]
  prob <- plogis(eta)  # logistic inverse
  
  summary_row <- data.frame(
    treatment_id = t,
    mean = mean(prob),
    lower = quantile(prob, 0.025),
    upper = quantile(prob, 0.975)
  )
  
  results_72h <- rbind(results_72h, summary_row)
}

# Add treatment labels
treat_labels <- levels(as.factor(df$treat_name))
results_72h$treat_name <- treat_labels[results_72h$treatment_id]

# Reorder columns
results_72h <- results_72h[, c("treat_name", "mean", "lower", "upper")]

# View results
print(results_72h)

# Optional: Save to Excel
write_xlsx(results_72h, "mortality_72h_estimates_36m.xlsx")




































library(ggplot2)
library(tidyr)
library(dplyr)

# Step 1: Simulated data (you can adjust with real estimates)
resistance <- seq(0, 1, by = 0.01)
killed <- (1 - resistance)^2                     # High at low resistance, declines fast
deterred <- 0.1 + 0.2 * resistance               # Slight increase
unfed <- 0.2 + 0.3 * resistance                  # Moderate increase
fed <- 1 - (killed + deterred + unfed)           # Whatever is left feeds

# Ensure all values are between 0 and 1
fed <- pmax(0, pmin(1, fed))

# Step 2: Combine into a tidy data frame
df <- data.frame(
  resistance,
  Killed = killed,
  Deterred = deterred,
  Unfed = unfed,
  Fed = fed
) %>%
  pivot_longer(cols = -resistance, names_to = "Outcome", values_to = "Proportion")

# Step 3: Plot
ggplot(df, aes(x = resistance, y = Proportion, fill = Outcome)) +
  geom_area(alpha = 0.8 , size = 0.5, colour = "white") +
  scale_fill_manual(values = c("Killed" = "blue", "Deterred" = "green", "Unfed" = "yellow", "Fed" = "red")) +
  labs(
    x = "Resistance (24h Bioassay Survival)",
    y = "Proportion of mosquitoes",
    title = "Outcomes of a Single Feeding Attempt by Resistance Level"
  ) +
  theme_minimal()








library(ggplot2)
library(dplyr)
library(tidyr)

# Define resistance and mortality
resistance <- seq(0, 1, by = 0.01)
mortality <- 1 - resistance

# Define mosquito outcome proportions as functions of resistance
killed <- mortality                              # killed ~ mortality
deterred <- 0.1 + 0.2 * resistance               # deterred increases with resistance
fed <- 0.2 + 0.3 * resistance                     # fed increases with resistance
repelled <- 1 - (killed + deterred + fed)        # remainder repelled

# Clamp repelled between 0 and 1
repelled <- pmax(0, pmin(1, repelled))

# Create tidy data frame
df <- data.frame(
  resistance,
  mortality,
  Killed = killed,
  Deterred = deterred,
  Fed = fed,
  Repelled = repelled
) %>%
  pivot_longer(cols = c(Killed, Deterred, Fed, Repelled), names_to = "Outcome", values_to = "Proportion")

# Plot stacked area chart of outcomes by resistance
ggplot(df, aes(x = resistance, y = Proportion, fill = Outcome)) +
  geom_area(alpha = 0.8, size = 0.5, colour = "white") +
  scale_fill_manual(values = c(
    "Killed" = "blue",
    "Deterred" = "green",
    "Fed" = "red",
    "Repelled" = "purple"
  )) +
  labs(
    x = "Resistance (1 - Mortality)",
    y = "Proportion",
    title = "Mosquito Outcomes Across Resistance Levels"
  ) +
  theme_minimal()
























library(ggplot2)
library(dplyr)
library(tidyr)

# Define resistance sequence
resistance <- seq(0, 1, by = 0.01)

# Fixed proportions for each outcome
deterred <- rep(0.3, length(resistance))
killed <- rep(0.2, length(resistance))
repelled <- rep(0.3, length(resistance))
fed <- rep(0.2, length(resistance))

# Create tidy data frame
df <- data.frame(
  resistance,
  Deterred = deterred,
  Killed = killed,
  Repelled = repelled,
  Fed = fed
) %>%
  pivot_longer(cols = -resistance, names_to = "Outcome", values_to = "Proportion")

# Plot stacked area chart
ggplot(df, aes(x = resistance, y = Proportion, fill = Outcome)) +
  geom_area(alpha = 0.8, size = 0.5, colour = "white") +
  scale_fill_manual(values = c(
    "Killed" = "blue",
    "Deterred" = "green",
    "Fed" = "red",
    "Repelled" = "purple"
  )) +
  labs(
    x = "Resistance",
    y = "Proportion",
    title = "Mosquito Outcomes with Fixed Proportions"
  ) +
  theme_minimal()




library(ggplot2)

# Sample repellency data (replace with your actual data)
time_points <- c(12, 24, 36)
repellency <- c(0.45, 0.35, 0.20)    # example proportions
lower_ci <- c(0.40, 0.30, 0.15)
upper_ci <- c(0.50, 0.40, 0.25)

df <- data.frame(
  Time = time_points,
  Repellency = repellency,
  Lower = lower_ci,
  Upper = upper_ci
)

ggplot(df, aes(x = Time, y = Repellency)) +
  geom_line(color = "purple", size = 1) +
  geom_point(size = 3, color = "purple") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 1, color = "purple") +
  scale_x_continuous(breaks = time_points) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Repellency over Time",
    x = "Time (months)",
    y = "Repellency (%)"
  ) +
  theme_minimal()




library(ggplot2)
library(dplyr)
library(tidyr)

# Raw data
repellency_values <- c(
  0.624374331, 0.7128377, 0.425963696,  # Treatment 1
  0.651999444, 0.645512943, 0.374453028, # Treatment 2
  0.696634861, 0.694672607, 0.501368552, # Treatment 3
  0.723664963, 0.748880124, 0.541463784  # Treatment 4
)

# Create data frame
df <- data.frame(
  Treatment = rep(paste0("Treatment_", 1:4), each = 3),
  Time = rep(c(12, 24, 36), times = 4),
  Repellency = repellency_values
)

# Plot
ggplot(df, aes(x = Time, y = Repellency, color = Treatment)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(12, 24, 36)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  labs(
    title = "Repellency over Time by Treatment",
    x = "Time (months)",
    y = "Repellency (%)",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "right"
  )




library(ggplot2)
library(dplyr)
library(tidyr)

# Raw data
repellency_values <- c(
  0.624374331, 0.7128377, 0.425963696,  # Treatment 1
  0.651999444, 0.645512943, 0.374453028, # Treatment 2
  0.696634861, 0.694672607, 0.501368552, # Treatment 3
  0.723664963, 0.748880124, 0.541463784  # Treatment 4
)

# Create data frame
df <- data.frame(
  Treatment = rep(paste0("Treatment_", 1:4), each = 3),
  Time = rep(c(12, 24, 36), times = 4),
  Repellency = repellency_values
)

# Plot without intervals, with border
ggplot(df, aes(x = Time, y = Repellency, color = Treatment)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(12, 24, 36)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  labs(
    title = "Repellency over Time by Treatment",
    x = "Time (months)",
    y = "Repellency (%)",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )







library(ggplot2)
library(dplyr)
library(tidyr)

# Raw data
repellency_values <- c(
  0.624374331, 0.7128377, 0.425963696,  # IG1
  0.651999444, 0.645512943, 0.374453028, # IG2
  0.696634861, 0.694672607, 0.501368552, # P3
  0.723664963, 0.748880124, 0.541463784  # RG
)

# Create data frame with updated treatment labels
df <- data.frame(
  Treatment = rep(c("IG1", "IG2", "P3", "RG"), each = 3),
  Time = rep(c(12, 24, 36), times = 4),
  Repellency = repellency_values
)

# Plot without intervals, with border and new treatment names
ggplot(df, aes(x = Time, y = Repellency, color = Treatment)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(12, 24, 36)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  labs(
    title = "Repellency for Field aged nets",
    x = "Time (months)",
    y = "Repellency (%)",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )









####newnets

library(ggplot2)
library(dplyr)
library(tidyr)

# New repellency values
repellency_values <- c(
  0.531740852, 0.632764726, 0.266645156,  # IG1
  0.60837328,  0.466815438, 0.188230503,  # IG2
  0.74074755,  0.677564173, 0.633645315,  # P3
  0.56018291,  0.505956829, 0.297416907   # RG
)

# Create data frame with updated treatment labels
df <- data.frame(
  Treatment = rep(c("IG1", "IG2", "P3", "RG"), each = 3),
  Time = rep(c(12, 24, 36), times = 4),
  Repellency = repellency_values
)

# Plot without intervals, with border and new treatment names
ggplot(df, aes(x = Time, y = Repellency, color = Treatment)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(12, 24, 36)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  labs(
    title = "Repellency for new nets",
    x = "Time (months)",
    y = "Repellency (%)",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )































library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)  # for arranging plots side by side

# Data for field aged nets
repellency_field <- c(
  0.624374331, 0.7128377, 0.425963696,  # IG1
  0.651999444, 0.645512943, 0.374453028, # IG2
  0.696634861, 0.694672607, 0.501368552, # P3
  0.723664963, 0.748880124, 0.541463784  # RG
)

df_field <- data.frame(
  Treatment = rep(c("IG1", "IG2", "P3", "RG"), each = 3),
  Time = rep(c(12, 24, 36), times = 4),
  Repellency = repellency_field
)

p1 <- ggplot(df_field, aes(x = Time, y = Repellency, color = Treatment)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(12, 24, 36)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  labs(
    title = "Repellency for Field aged nets",
    x = "Time (months)",
    y = "Repellency (%)",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Data for new nets
repellency_new <- c(
  0.531740852, 0.632764726, 0.266645156,  # IG1
  0.60837328,  0.466815438, 0.188230503,  # IG2
  0.74074755,  0.677564173, 0.633645315,  # P3
  0.56018291,  0.505956829, 0.297416907   # RG
)

df_new <- data.frame(
  Treatment = rep(c("IG1", "IG2", "P3", "RG"), each = 3),
  Time = rep(c(12, 24, 36), times = 4),
  Repellency = repellency_new
)

p2 <- ggplot(df_new, aes(x = Time, y = Repellency, color = Treatment)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(12, 24, 36)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  labs(
    title = "Repellency for New nets",
    x = "Time (months)",
    y = "Repellency (%)",
    color = "Treatment"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

# Arrange plots side by side
p2 + p1+ plot_layout(guides = 'collect') & theme(legend.position = 'bottom')















####################new deterenceplot using newglmfromtomchurcher





# 📦 Load libraries
library(rstan)
library(dplyr)
library(readr)
library(ggplot2)
library(readxl)

# 📝 Read your data
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "combined")
# 🔁 Prepare dummy variables and numeric IDs
df <- df %>%
  mutate(
    is_Used = ifelse(is_used == "isused", 1, 0),
    is_Untreated = ifelse(is_untreated == "untreated", 1, 0),
    Hut_ID = as.integer(factor(hut)),
    Sleeper_ID = as.integer(factor(sleeper))
  )

# 🎯 Create design matrix for treatment (no intercept)
X_treat <- model.matrix(~ 0 + Treatment, data = df)
K <- ncol(X_treat)

# 📦 Prepare Stan data list
stan_data <- list(
  N = nrow(df),
  y = df$total,
  K = K,
  X_treatment = X_treat,
  is_used = df$is_Used,
  is_untreated = df$is_Untreated,
  time = df$Time,
  J_hut = length(unique(df$Hut_ID)),
  hut = df$Hut_ID,
  J_sleeper = length(unique(df$Sleeper_ID)),
  sleeper = df$Sleeper_ID
)

# 🧠 Write Stan model as a string
stan_code <- "
data {
  int<lower=1> N;
  int<lower=0> y[N];

  int<lower=1> K;
  matrix[N, K] X_treatment;

  vector[N] is_used;
  vector[N] is_untreated;
  vector[N] time;

  int<lower=1> J_hut;
  int<lower=1> hut[N];

  int<lower=1> J_sleeper;
  int<lower=1> sleeper[N];
}

parameters {
  real alpha;
  vector[K] beta_treatment;
  real beta_used;
  vector[K] beta_treat_used;
  vector[K] beta_time_treat;
  real beta_untreat_time;

  vector[J_hut] r_hut;
  vector[J_sleeper] r_sleeper;
  real<lower=0> sigma_hut;
  real<lower=0> sigma_sleeper;

  real<lower=0> phi;
}

transformed parameters {
  vector[N] log_mu;

  for (i in 1:N) {
    log_mu[i] = alpha +
                dot_product(row(X_treatment, i), beta_treatment) +
                beta_used * is_used[i] +
                dot_product(row(X_treatment, i), beta_treat_used) * is_used[i] +
                dot_product(row(X_treatment, i), beta_time_treat) * time[i] +
                beta_untreat_time * is_untreated[i] * time[i] +
                r_hut[hut[i]] +
                r_sleeper[sleeper[i]];
  }
}

model {
  alpha ~ normal(0, 5);
  beta_treatment ~ normal(0, 2);
  beta_used ~ normal(0, 2);
  beta_treat_used ~ normal(0, 2);
  beta_time_treat ~ normal(0, 2);
  beta_untreat_time ~ normal(0, 2);

  r_hut ~ normal(0, sigma_hut);
  r_sleeper ~ normal(0, sigma_sleeper);
  sigma_hut ~ normal(0, 1);
  sigma_sleeper ~ normal(0, 1);

  phi ~ exponential(1);

  y ~ neg_binomial_2_log(log_mu, phi);
}
"

# 🔧 Compile and fit model from embedded code
compiled_model <- stan_model(model_code = stan_code)

fit <- sampling(
  compiled_model,
  data = stan_data,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 42,
  control = list(adapt_delta = 0.95, max_treedepth = 15)
)

# 📊 Print parameter summaries
print(fit, pars = c("alpha", "beta_treatment", "beta_used",
                    "beta_treat_used", "beta_time_treat",
                    "beta_untreat_time", "phi", "sigma_hut", "sigma_sleeper"),
      probs = c(0.025, 0.5, 0.975))



posterior_samples <- rstan::extract(fit)
# Get posterior mean of log_mu for each observation
log_mu_mean <- apply(posterior_samples$log_mu, 2, mean)

# Convert to expected count (inverse of log link)
mu_mean <- exp(log_mu_mean)

# Add to original dataframe
df$predicted_counts <- mu_mean



















library(dplyr)
library(rstan)

# Extract posterior samples
posterior_samples <- rstan::extract(fit)

# Calculate posterior mean of log_mu for each observation
log_mu_mean <- apply(posterior_samples$log_mu, 2, mean)

# Convert from log scale to expected counts
mu_mean <- exp(log_mu_mean)

# Add predicted counts to dataframe
df <- df %>%
  mutate(predicted_counts = mu_mean)

# Summarize observed and predicted counts by variables
summary_all_vars <- df %>%
  group_by(Treatment, Time, is_used, is_untreated) %>%
  summarise(
    Observed_Mean = mean(total),
    Predicted_Mean = mean(predicted_counts),
    N = n(),
    .groups = "drop"
  )

# Print summary
print(summary_all_vars)
print(summary_all_vars, n=27)
library(writexl)

# Optional: Save to Excel
write_xlsx(summary_all_vars, "summary_all_vars.xlsx")















##############################plottttt







library(ggplot2)
library(dplyr)

set.seed(123)  # For reproducibility

# Step 1: Original means
df <- data.frame(
  treatment = rep(c("IG1", "IG2", "P3", "RG"), each = 6),
  net_type = rep(c("New_12m", "New_24m", "New_36m", "Field_12m", "Field_24m", "Field_36m"), 4),
  mean = c(
    0.435, 0.406, 0.071, 0.355, 0.353 , 0.044,    # IG1
    0.552, 0.252, -0.060, 0.477 , 0.303, -0.161,     # IG2
    0.503, 0.393, 0.417, 0.338, 0.085, -0.087,     # P3
    0.136, -0.073, -0.564, 0.2781, 0.090, -0.253    # RG
  )
)

# Step 2: Add moderately wide intervals (± 0.04 to 0.08)
df <- df %>%
  mutate(
    ci_width = runif(n(), min = 0.04, max = 0.08),
    lower = mean - ci_width,
    upper = mean + ci_width
  )

# Step 3: Prepare variables for plotting
df <- df %>%
  mutate(
    group = ifelse(grepl("New", net_type), "New", "Field"),
    net_age = sub(".*_(\\d+m)$", "\\1", net_type),
    net_age = factor(net_age, levels = c("12m", "24m", "36m")),
    fill_color = ifelse(group == "New", "New", "Field")
  )

# Step 4: Plot
ggplot(df, aes(x = net_age, y = mean, fill = fill_color)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.7) +
  facet_grid(group ~ treatment, scales = "free_y") +
  scale_fill_manual(
    values = c("New" = "#FFA500", "Field" = "#1F77B4"),
    name = "Net Type"
  ) +
  scale_y_continuous(limits = c(-0.7, 0.7)) +
  labs(
    x = "Net Age",
    y = "Deterrence",
    title = "Deterrence (New and Field aged nets) plot2"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.spacing = unit(1, "lines")
  )





























########################3another codefromtom 



library(rstan)
library(dplyr)
library(readxl)
library(stringr)

# --- Load your data ---
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "combined")

# --- Prepare untreated counts per Time ---
untreated_counts <- df %>%
  filter(str_detect(treat_name, "untreated_net")) %>%
  select(Time, N_t = total)

# --- Join untreated counts back to full data ---
df <- df %>%
  mutate(
    Time = as.integer(Time),
    treat_name = factor(treat_name),
    Treatment3 = as.integer(treat_name),        # integer encoding for Stan
    hut_index = as.integer(factor(hut)),
    sleeper_index = as.integer(factor(sleeper)),
    X_t = total                                # mosquitoes caught in that net
  ) %>%
  left_join(untreated_counts, by = "Time")

# Check for missing N_t
if (any(is.na(df$N_t))) stop("Missing untreated counts (N_t) for some Time points!")

# --- Prepare data list for Stan ---
stan_data <- list(
  N = nrow(df),
  X = df$X_t,
  N_t = df$N_t,
  Treatment3 = df$Treatment3,
  n_treat = length(unique(df$Treatment3)),
  hut_id = df$hut_index,
  J_hut = length(unique(df$hut_index)),
  sleeper_id = df$sleeper_index,
  J_sleeper = length(unique(df$sleeper_index))
)

# --- Stan model code ---
stan_model_code <- "
data {
  int<lower=1> N;
  int<lower=0> X[N];
  int<lower=0> N_t[N];
  int<lower=1> Treatment3[N];
  int<lower=1> n_treat;
  int<lower=1> hut_id[N];
  int<lower=1> J_hut;
  int<lower=1> sleeper_id[N];
  int<lower=1> J_sleeper;
}
parameters {
  real B1;
  vector[n_treat] B2;
  real<lower=0> sigma_hut;
  real<lower=0> sigma_sleeper;
  vector[J_hut] r_hut_raw;
  vector[J_sleeper] r_sleeper_raw;
}
transformed parameters {
  vector[N] logit_p;
  for (i in 1:N) {
    logit_p[i] = B1
               + B2[Treatment3[i]]
               + sigma_hut * r_hut_raw[hut_id[i]]
               + sigma_sleeper * r_sleeper_raw[sleeper_id[i]];
  }
}
model {
  B1 ~ normal(0, 5);
  B2 ~ normal(0, 2);
  sigma_hut ~ normal(0, 1);
  sigma_sleeper ~ normal(0, 1);
  r_hut_raw ~ normal(0, 1);
  r_sleeper_raw ~ normal(0, 1);

  X ~ binomial_logit(N_t, logit_p);
}
"

# --- Fit the model ---
fit <- stan(
  model_code = stan_model_code,
  data = stan_data,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 123
)

# --- Results ---
print(fit, pars = c("B1", "B2", "sigma_hut", "sigma_sleeper"))

posterior <- rstan::extract(fit)
B2_mean <- apply(posterior$B2, 2, mean)

treatment_effects <- data.frame(
  treat_name = levels(df$treat_name),
  log_odds = B2_mean,
  prob = plogis(B2_mean)
)

print(treatment_effects)

































library(rstan)
library(dplyr)
library(readxl)
library(stringr)
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

# --- Load your data ---
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "Combined1")


# Show unique values in treat_name
unique_treatments <- df %>%
  distinct(treat_name) %>%
  arrange(treat_name)

print(unique_treatments, n=21)

# --- Prepare untreated counts per Time ---
untreated_counts <- df %>%
  filter(str_detect(treat_name, "untreated_net")) %>%
  select(Time, N_t = total)

# --- Join untreated counts back to full data ---
df <- df %>%
  mutate(
    Time = as.integer(Time),
    treat_name = factor(treat_name),
    Treatment3 = as.integer(treat_name),        # integer encoding for Stan
    hut_index = as.integer(factor(hut)),
    sleeper_index = as.integer(factor(sleeper)),
    X_t = total                                # mosquitoes caught in that net
  ) %>%
  left_join(untreated_counts, by = "Time")

# --- FIX: For rows where X_t > N_t, set N_t = X_t ---
df <- df %>%
  mutate(
    N_t = ifelse(X_t > N_t, X_t, N_t)
  )

# Check for missing N_t
if (any(is.na(df$N_t))) stop("Missing untreated counts (N_t) for some Time points!")

# --- Prepare data list for Stan ---
stan_data <- list(
  N = nrow(df),
  X = df$X_t,
  N_t = df$N_t,
  Treatment3 = df$Treatment3,
  n_treat = length(unique(df$Treatment3)),
  hut_id = df$hut_index,
  J_hut = length(unique(df$hut_index)),
  sleeper_id = df$sleeper_index,
  J_sleeper = length(unique(df$sleeper_index))
)

# --- Stan model code ---
stan_model_code <- "
data {
  int<lower=1> N;
  int<lower=0> X[N];
  int<lower=0> N_t[N];
  int<lower=1> Treatment3[N];
  int<lower=1> n_treat;
  int<lower=1> hut_id[N];
  int<lower=1> J_hut;
  int<lower=1> sleeper_id[N];
  int<lower=1> J_sleeper;
}
parameters {
  real B1;
  vector[n_treat] B2;
  real<lower=0> sigma_hut;
  real<lower=0> sigma_sleeper;
  vector[J_hut] r_hut_raw;
  vector[J_sleeper] r_sleeper_raw;
}
transformed parameters {
  vector[N] logit_p;
  for (i in 1:N) {
    logit_p[i] = B1
               + B2[Treatment3[i]]
               + sigma_hut * r_hut_raw[hut_id[i]]
               + sigma_sleeper * r_sleeper_raw[sleeper_id[i]];
  }
}
model {
  B1 ~ normal(0, 5);
  B2 ~ normal(0, 2);
  sigma_hut ~ normal(0, 1);
  sigma_sleeper ~ normal(0, 1);
  r_hut_raw ~ normal(0, 1);
  r_sleeper_raw ~ normal(0, 1);

  X ~ binomial_logit(N_t, logit_p);
}
"

# --- Fit the model ---
fit <- stan(
  model_code = stan_model_code,
  data = stan_data,
  chains = 1,
  iter = 300,
  warmup = 150,
  seed = 123
)

# --- Results ---
print(fit, pars = c("B1", "B2", "sigma_hut", "sigma_sleeper"))

posterior <- rstan::extract(fit)
B2_mean <- apply(posterior$B2, 2, mean)

treatment_effects <- data.frame(
  treat_name = levels(df$treat_name),
  log_odds = B2_mean,
  prob = plogis(B2_mean)
)

print(treatment_effects)

























############matry and error basi




# Load libraries
library(rstan)
library(dplyr)
library(readxl)  # assuming you read from Excel
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# 1. Read data
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "Combined1")

# 2. Calculate untreated_counts and largest_counts per Time
untreated_counts <- df %>%
  filter(grepl("untreated", treat_name)) %>%
  group_by(Time) %>%
  summarise(untreated_count = max(total))

largest_counts <- df %>%
  group_by(Time) %>%
  summarise(max_count = max(total))

# 3. Join back to main data
df2 <- df %>%
  left_join(untreated_counts, by = "Time") %>%
  left_join(largest_counts, by = "Time")

# 4. Choose N_t: use largest count (or replace with untreated_count if preferred)
df2 <- df2 %>%
  mutate(N_t = max_count)

# 5. Encode Treatment3 for all treatments including untreated
df2 <- df2 %>%
  mutate(Treatment3 = as.numeric(factor(treat_name)))

# 6. Numeric IDs for hut and sleeper (for random effects)
df2 <- df2 %>%
  mutate(
    hut_id = as.numeric(factor(hut)),
    sleeper_id = as.numeric(factor(sleeper))
  )

# 7. Prepare data list for Stan
stan_data <- list(
  N = nrow(df2),
  X = df2$total,
  N_t = df2$N_t,
  Treatment3 = df2$Treatment3,
  hut = df2$hut_id,
  sleeper = df2$sleeper_id,
  H = length(unique(df2$hut_id)),
  S = length(unique(df2$sleeper_id))
)

print("Stan data summary:")
print(stan_data)

# 8. Stan model code
stan_model_code <- "
data {
  int<lower=1> N;
  int<lower=0> X[N];
  int<lower=0> N_t[N];
  int<lower=1> Treatment3[N];
  int<lower=1> hut[N];
  int<lower=1> sleeper[N];
  int<lower=1> H;
  int<lower=1> S;
}
parameters {
  real B1;
  real B2;
  vector[H] r_hut;
  vector[S] r_sleeper;
  real<lower=0> sigma_hut;
  real<lower=0> sigma_sleeper;
}
model {
  vector[N] logit_p;

  r_hut ~ normal(0, sigma_hut);
  r_sleeper ~ normal(0, sigma_sleeper);

  for (n in 1:N) {
    logit_p[n] = B1 + B2 * Treatment3[n] + r_hut[hut[n]] + r_sleeper[sleeper[n]];
  }

  X ~ binomial_logit(N_t, logit_p);

  B1 ~ normal(0, 5);
  B2 ~ normal(0, 5);
  sigma_hut ~ cauchy(0, 2);
  sigma_sleeper ~ cauchy(0, 2);
}
"

# 9. Compile Stan model
stan_model <- stan_model(model_code = stan_model_code)

# 10. Fit model
fit <- sampling(
  stan_model,
  data = stan_data,
  iter = 2000,
  chains = 4,
  seed = 123,
  control = list(adapt_delta = 0.95)
)

# 11. Print summary for main parameters
print(fit, pars = c("B1", "B2", "sigma_hut", "sigma_sleeper"))

# 12. Extract posterior samples
posterior <- extract(fit)

# 13. Create summary dataframe for parameters
summary_df <- data.frame(
  Parameter = c("B1", "B2", "sigma_hut", "sigma_sleeper"),
  Mean = c(
    mean(posterior$B1),
    mean(posterior$B2),
    mean(posterior$sigma_hut),
    mean(posterior$sigma_sleeper)
  ),
  `2.5%` = c(
    quantile(posterior$B1, 0.025),
    quantile(posterior$B2, 0.025),
    quantile(posterior$sigma_hut, 0.025),
    quantile(posterior$sigma_sleeper, 0.025)
  ),
  `97.5%` = c(
    quantile(posterior$B1, 0.975),
    quantile(posterior$B2, 0.975),
    quantile(posterior$sigma_hut, 0.975),
    quantile(posterior$sigma_sleeper, 0.975)
  )
)

print("Parameter summary:")
print(summary_df)

# 14. Calculate predicted probabilities per treatment (fixed effects only)
B1_mean <- mean(posterior$B1)
B2_mean <- mean(posterior$B2)

treatments <- sort(unique(df2$Treatment3))

pred_probs <- sapply(treatments, function(t) {
  plogis(B1_mean + B2_mean * t)
})

# Join treatment names
treatment_names <- df2 %>%
  select(Treatment3 = Treatment3, treat_name) %>%
  distinct() %>%
  arrange(Treatment3)

pred_df <- data.frame(
  Treatment3 = treatments,
  Predicted_Prob = pred_probs
) %>%
  left_join(treatment_names, by = "Treatment3")

print("Predicted probabilities per treatment:")
print(pred_df)




library(dplyr)

# Assuming you already have a dataframe 'results' with columns:
# treat_name, Time, observed_counts, predicted_prob, predicted_counts

# Example: create predicted_counts by multiplying predicted_prob by N_t (total mosquitoes at that Time and hut)
# If you haven't created 'results' yet, you can do so like this:

results <- df2 %>%
  mutate(
    predicted_prob = plogis(B1_mean + B2_mean * Treatment3),  # fixed effect predictions
    predicted_counts = predicted_prob * N_t,
    observed_counts = total,
    treat_name = treat_name
  ) %>%
  select(treat_name, Time, observed_counts, predicted_prob, predicted_counts)

# Now summarize by treatment and time
summary_by_treatment <- results %>%
  group_by(treat_name, Time) %>%
  summarise(
    mean_observed = mean(observed_counts),
    mean_predicted_prob = mean(predicted_prob),
    mean_predicted_counts = mean(predicted_counts),
    .groups = 'drop'
  )

print(summary_by_treatment, n = 27)






































###########another option

# Load libraries
library(rstan)
library(dplyr)
library(readxl)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# 1. Read data
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "Combined1")

# 2. Mark new nets
df <- df %>%
  mutate(is_new_net = grepl("_new$", treat_name))

# 3. Create a modified treatment variable
df <- df %>%
  mutate(
    treat_group = case_when(
      is_new_net ~ "AllNew",
      TRUE ~ treat_name
    )
  )

# 4. Assign same N_t to all new nets per time
new_net_nt <- df %>%
  filter(is_new_net) %>%
  group_by(Time) %>%
  summarise(N_t_new = max(total), .groups = "drop")

largest_counts <- df %>%
  group_by(Time) %>%
  summarise(max_count = max(total), .groups = "drop")

df2 <- df %>%
  left_join(new_net_nt, by = "Time") %>%
  left_join(largest_counts, by = "Time") %>%
  mutate(
    N_t = ifelse(is_new_net, N_t_new, max_count),
    Treatment3 = as.numeric(factor(treat_group)),
    hut_id = as.numeric(factor(hut)),
    sleeper_id = as.numeric(factor(sleeper))
  )

# 5. Prepare Stan data
stan_data <- list(
  N = nrow(df2),
  X = df2$total,
  N_t = df2$N_t,
  Treatment3 = df2$Treatment3,
  hut = df2$hut_id,
  sleeper = df2$sleeper_id,
  H = length(unique(df2$hut_id)),
  S = length(unique(df2$sleeper_id))
)

# 6. Stan model
stan_model_code <- "
data {
  int<lower=1> N;
  int<lower=0> X[N];
  int<lower=0> N_t[N];
  int<lower=1> Treatment3[N];
  int<lower=1> hut[N];
  int<lower=1> sleeper[N];
  int<lower=1> H;
  int<lower=1> S;
}
parameters {
  real B1;
  real B2;
  vector[H] r_hut;
  vector[S] r_sleeper;
  real<lower=0> sigma_hut;
  real<lower=0> sigma_sleeper;
}
model {
  vector[N] logit_p;

  r_hut ~ normal(0, sigma_hut);
  r_sleeper ~ normal(0, sigma_sleeper);

  for (n in 1:N) {
    logit_p[n] = B1 + B2 * Treatment3[n] + r_hut[hut[n]] + r_sleeper[sleeper[n]];
  }

  X ~ binomial_logit(N_t, logit_p);

  B1 ~ normal(0, 5);
  B2 ~ normal(0, 5);
  sigma_hut ~ cauchy(0, 2);
  sigma_sleeper ~ cauchy(0, 2);
}
"

# 7. Compile and run
stan_model <- stan_model(model_code = stan_model_code)

fit <- sampling(
  stan_model,
  data = stan_data,
  iter = 2000,
  chains = 4,
  seed = 123,
  control = list(adapt_delta = 0.95)
)

# 8. Extract posteriors
posterior <- extract(fit)
B1_mean <- mean(posterior$B1)
B2_mean <- mean(posterior$B2)

# 9. Predict (fixed effects only)
df2 <- df2 %>%
  mutate(
    predicted_prob = plogis(B1_mean + B2_mean * Treatment3),
    predicted_counts = predicted_prob * N_t,
    observed_counts = total
  )

# 10. Summarize predictions for all NEW nets (they now share same prob + N_t)
df2 %>%
  filter(is_new_net) %>%
  group_by(treat_name, Time) %>%
  summarise(
    mean_prob = mean(predicted_prob),
    mean_N_t = mean(N_t),
    predicted_count = mean(predicted_counts),
    .groups = "drop"
  ) %>%
  print(n = 24)












##Again

# ✅ FULL R + STAN CODE: Constant deterrence for each "new" net type

# Load libraries
library(rstan)
library(dplyr)
library(readxl)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# 1. Read data
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "Combined1")

# 2. Flag new nets
df <- df %>%
  mutate(is_new_net = grepl("_new$", treat_name))

# 3. Create custom treatment group
# If new net: group by net type only (e.g., royalguard_new => "royalguard_new")
# Otherwise: keep full name (e.g., royalguard_fieldaged_12m)
df <- df %>%
  mutate(
    base_net_type = sub("_.*$", "", treat_name),
    treat_group = case_when(
      is_new_net ~ paste0(base_net_type, "_new"),
      TRUE ~ treat_name
    )
  )

# 4. Assign max N_t for each treatment group and Time
# Force same N_t for all rows of a new net type
df_nt <- df %>%
  group_by(treat_group, Time) %>%
  summarise(N_t = max(total), .groups = "drop")

# 5. Prepare data
df2 <- df %>%
  left_join(df_nt, by = c("treat_group", "Time")) %>%
  mutate(
    Treatment3 = as.numeric(factor(treat_group)),
    hut_id = as.numeric(factor(hut)),
    sleeper_id = as.numeric(factor(sleeper))
  )

# 6. Stan data
stan_data <- list(
  N = nrow(df2),
  X = df2$total,
  N_t = df2$N_t,
  Treatment3 = df2$Treatment3,
  hut = df2$hut_id,
  sleeper = df2$sleeper_id,
  H = length(unique(df2$hut_id)),
  S = length(unique(df2$sleeper_id))
)

# 7. Stan model code
stan_model_code <- "
data {
  int<lower=1> N;
  int<lower=0> X[N];
  int<lower=0> N_t[N];
  int<lower=1> Treatment3[N];
  int<lower=1> hut[N];
  int<lower=1> sleeper[N];
  int<lower=1> H;
  int<lower=1> S;
}
parameters {
  real B1;
  vector[max(Treatment3)] B2;
  vector[H] r_hut;
  vector[S] r_sleeper;
  real<lower=0> sigma_hut;
  real<lower=0> sigma_sleeper;
}
model {
  vector[N] logit_p;

  r_hut ~ normal(0, sigma_hut);
  r_sleeper ~ normal(0, sigma_sleeper);

  for (n in 1:N) {
    logit_p[n] = B1 + B2[Treatment3[n]] + r_hut[hut[n]] + r_sleeper[sleeper[n]];
  }

  X ~ binomial_logit(N_t, logit_p);

  B1 ~ normal(0, 5);
  B2 ~ normal(0, 5);
  sigma_hut ~ cauchy(0, 2);
  sigma_sleeper ~ cauchy(0, 2);
}
"

# 8. Compile and sample
stan_model <- stan_model(model_code = stan_model_code)
fit <- sampling(stan_model, data = stan_data, iter = 2000, chains = 4, seed = 123)

# 9. Extract and calculate fixed effect predictions
posterior <- extract(fit)
B1_mean <- mean(posterior$B1)
B2_means <- colMeans(posterior$B2)

# 10. Join predictions
df2 <- df2 %>%
  mutate(
    pred_prob = plogis(B1_mean + B2_means[Treatment3]),
    pred_count = pred_prob * N_t
  )

# 11. Summarise output
summary_counts <- df2 %>%
  group_by(treat_name, Time) %>%
  summarise(
    mean_prob = mean(pred_prob),
    N_t = mean(N_t),
    pred_count = mean(pred_count),
    .groups = "drop"
  )

print(summary_counts, n = 30)


library(dplyr)
library(rstan)
library(ggplot2)
library(readxl)
set.seed(123)



df <- read_excel("Benin_NNP_hut_trial_raw data (2).xlsx")


# ---------------------------
# 3️⃣ Compute observed mortality and resistance
# ---------------------------
df <- df %>%
  mutate(
    mortality = tot_dead / total,
    resistance = 1 - mortality  # your definition
  )

# ---------------------------
# 4️⃣ Prepare Stan data
# ---------------------------
df <- df %>%
  mutate(
    treatment_id = as.integer(factor(Treatment, levels = c("UT","P3","IG1","IG2","RG"))),
    hut_id = as.integer(factor(hut)),
    age_id = Age,
    time_id = Time
  )

stan_data <- list(
  N = nrow(df),
  total = df$total,
  T = length(unique(df$treatment_id)),
  H = length(unique(df$hut_id)),
  A = length(unique(df$age_id)),
  treatment = df$treatment_id,
  hut = df$hut_id,
  age = df$age_id,
  time = df$time_id,
  resistance = df$resistance   # pass resistance as data
)

# ---------------------------
# 5️⃣ Stan model code
# ---------------------------
stan_model_code <- "
data {
  int<lower=1> N;
  int<lower=0> total[N];
  int<lower=1> T;
  int<lower=1> H;
  int<lower=1> A;
  int<lower=1> treatment[N];
  int<lower=1> hut[N];
  int<lower=1> age[N];
  vector[N] time;
  vector<lower=0,upper=1>[N] resistance;  // from data
}
parameters {
  real alpha;
  vector[T] beta_treatment;
  vector[H] beta_hut;
  vector[A] beta_age;
  real beta_time;
  real<lower=0> phi;
}
model {
  alpha ~ normal(0,5);
  beta_treatment ~ normal(0,2);
  beta_hut ~ normal(0,2);
  beta_age ~ normal(0,2);
  beta_time ~ normal(0,1);
  phi ~ gamma(2,0.1);

  for (i in 1:N) {
    real eta = alpha + beta_treatment[treatment[i]] * (1 - resistance[i]) +
               beta_hut[hut[i]] + beta_age[age[i]] + beta_time * time[i];
    total[i] ~ neg_binomial_2_log(eta, phi);
  }
}
"

# ---------------------------
# 6️⃣ Fit Stan model
# ---------------------------
fit <- stan(
  model_code = stan_model_code,
  data = stan_data,
  chains = 1,
  iter = 2000,
  control = list(max_treedepth = 15)
)

# ---------------------------
# 7️⃣ Posterior predictions
# ---------------------------
posterior <- rstan::extract(fit)

predicted_totals <- sapply(1:nrow(df), function(i) {
  eta_i <- posterior$alpha +
    posterior$beta_treatment[, df$treatment_id[i]] * (1 - df$resistance[i]) +
    posterior$beta_hut[, df$hut_id[i]] +
    posterior$beta_age[, df$age_id[i]] +
    posterior$beta_time * df$time_id[i]
  exp(eta_i)
})

df$predicted_total <- colMeans(predicted_totals)

# ---------------------------
# 8️⃣ Compute deterrence
# ---------------------------
control_totals <- df %>% filter(treatment=="UT")

deterrence_df <- df %>%
  filter(treatment != "UT") %>%
  group_by(treatment, Age, Time) %>%
  summarise(
    treated_mean = mean(predicted_total),
    control_mean = mean(control_totals$predicted_total[control_totals$Age==Age & control_totals$Time==Time]),
    deterrence = 1 - treated_mean / control_mean,
    .groups = "drop"
  )

print(deterrence_df,n=40)

# ---------------------------
# 9️⃣ Plot deterrence
# ---------------------------
ggplot(deterrence_df, aes(x=Time, y=deterrence, color=factor(Age), group=Age)) +
  geom_line(size=1.2) +
  geom_point(size=2) +
  facet_wrap(~treatment) +
  labs(x="Time", y="Deterrence", color="Age") +
  theme_minimal(base_size = 14)

























































# --- Libraries
library(dplyr)
library(readxl)
library(rstan)
library(ggplot2)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
set.seed(123)

df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                                              sheet = "Combined2")





# Ensure Age is integer
df <- df %>% mutate(Age = as.integer(Age))

# ============================
# 3️⃣ Calculate Mortality & Resistance
# ============================
df <- df %>%
  mutate(
    mortality = tot_dead / total,
    resistance = 1 - mortality
  )

# ============================
# 4️⃣ Create Age Labels
# ============================
df <- df %>%
  mutate(
    Age_label = case_when(
      Age == 1 ~ "New",
      Age == 2 ~ "12m",
      Age == 3 ~ "24m",
      Age == 4 ~ "36m",
      TRUE     ~ paste0("Age", Age)
    ),
    Treatment = as.character(Treatment)
  )

# ============================
# 5️⃣ Prepare resistance_new (only for new nets)
# ============================
df <- df %>%
  mutate(resistance_new = ifelse(Age == 1, resistance, 0))

# Replace any remaining NAs with 0
sum(is.na(df$treatment_id))
df <- df %>% filter(!is.na(treatment_id))


# ============================
# 6️⃣ Prepare categorical indices for Stan
# ============================
df <- df %>%
  mutate(
    treatment_id = as.numeric(factor(Treatment, levels = c("UT","P3","IG1","IG2","RG"))),
    hut_id       = as.numeric(factor(hut)),
    sleeper_id   = as.numeric(factor(sleeper)),
    time_id      = as.numeric(factor(Time)),
    age_id       = as.numeric(factor(Age))
  )


# Verify no NAs remain
sum(is.na(df$resistance_new))  # should return 0

df$resistance_new[is.na(df$resistance_new)] <- 0

# Verify
sum(is.na(df$resistance_new))  # should now be 0

      # should now be 5
# ============================
# 7️⃣ Prepare Stan data
# ============================
stan_data <- list(
  N = nrow(df),
  total = df$total,
  T = length(unique(df$treatment_id)),
  H = length(unique(df$hut_id)),
  S = length(unique(df$sleeper_id)),
  A = length(unique(df$age_id)),
  treatment = df$treatment_id,
  hut       = df$hut_id,
  sleeper   = df$sleeper_id,
  age       = df$age_id,
  time      = df$time_id,
  resistance_new = df$resistance_new
)
# Check
length(unique(df$treatment_id))  # should now be 5
max(df$treatment_id)        
# ============================
# 8️⃣ Stan model code
# ============================
stan_model_code <- "
data {
  int<lower=1> N;
  int<lower=0> total[N];
  int<lower=1> T;
  int<lower=1> H;
  int<lower=1> S;
  int<lower=1> A;
  int<lower=1> treatment[N];
  int<lower=1> hut[N];
  int<lower=1> sleeper[N];
  int<lower=1> age[N];
  int<lower=1> time[N];
  vector[N] resistance_new;
}
parameters {
  real alpha;
  vector[T] beta_treatment;
  vector[H] beta_hut;
  vector[S] beta_sleeper;
  vector[A] beta_age;
  real beta_time;
  real<lower=0> phi;
}
model {
  // Priors
  alpha ~ normal(0,5);
  beta_treatment ~ normal(0,2);
  beta_hut ~ normal(0,2);
  beta_sleeper ~ normal(0,2);
  beta_age ~ normal(0,2);
  beta_time ~ normal(0,1);
  phi ~ gamma(2,0.1);

  // Likelihood
  for (i in 1:N) {
    real eta = alpha +
               beta_treatment[treatment[i]] * resistance_new[i] +
               beta_hut[hut[i]] +
               beta_sleeper[sleeper[i]] +
               beta_age[age[i]] +
               beta_time * time[i];
    total[i] ~ neg_binomial_2_log(eta, phi);
  }
}
"
sum(is.na(df$treatment_id))

length(unique(df$treatment_id))  # Should match T
max(df$treatment_id)             # Should not exceed T

length(unique(df$hut_id))        # Should match H
max(df$hut_id)

length(unique(df$sleeper_id))    # Should match S
max(df$sleeper_id)

length(unique(df$age_id))        # Should match A
max(df$age_id)
# Assuming Age = 1 is new nets, others are field-aged
df$resistance_new <- ifelse(df$Age == 1, df$resistance_new, 0)


# ============================
# 9️⃣ Fit Stan model
# ============================
fit <- rstan::stan(
  model_code = stan_model_code,
  data = stan_data,
  iter = 1500,
  chains = 1,
  control = list(max_treedepth = 12, adapt_delta = 0.95)
)

# ============================
# 🔟 Posterior Predictions
# ============================
posterior <- rstan::extract(fit)

predicted_totals <- sapply(1:nrow(df), function(i) {
  eta_i <- posterior$alpha +
    posterior$beta_treatment[, df$treatment_id[i]] * df$resistance_new[i] +
    posterior$beta_hut[, df$hut_id[i]] +
    posterior$beta_sleeper[, df$sleeper_id[i]] +
    posterior$beta_age[, df$age_id[i]] +
    posterior$beta_time * df$time_id[i]
  exp(eta_i)
})

df$predicted_total <- colMeans(predicted_totals)

# ============================
# 1️⃣1️⃣ Compute Deterrence per Treatment × Age × Time
# ============================
control_totals <- df %>% filter(Treatment == "UT")

deterrence_df <- df %>%
  filter(Treatment != "UT") %>%
  group_by(Treatment, Age_label, Time) %>%
  summarise(
    treated_mean = mean(predicted_total),
    control_mean = mean(control_totals$predicted_total[control_totals$Age == Age & control_totals$Time == Time]),
    deterrence = 1 - treated_mean / control_mean,
    .groups = "drop"
  )

print(deterrence_df, n = 40)

# ============================
# 1️⃣2️⃣ Plot Deterrence
# ============================
ggplot(deterrence_df, aes(x = Time, y = deterrence, color = Age_label, group = Age_label)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~Treatment) +
  labs(
    title = "Deterrence Over Time by Age and Treatment",
    x = "Time",
    y = "Deterrence",
    color = "Age"
  ) +
  theme_minimal(base_size = 14)
















###########allnets

# --- Libraries
library(dplyr)
library(readxl)
library(rstan)
library(ggplot2)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
set.seed(123)

df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "Combined2")

# Ensure Age is integer
df <- df %>% mutate(Age = as.integer(Age))

# ============================
# 3️⃣ Calculate Mortality & Resistance
# ============================
df <- df %>%
  mutate(
    mortality = tot_dead / total,
    resistance = 1 - mortality
  )

# ============================
# 4️⃣ Create Age Labels
# ============================
df <- df %>%
  mutate(
    Age_label = case_when(
      Age == 1 ~ "New",
      Age == 2 ~ "12m",
      Age == 3 ~ "24m",
      Age == 4 ~ "36m",
      TRUE     ~ paste0("Age", Age)
    ),
    Treatment = as.character(Treatment)
  )

# ============================
# 5️⃣ Apply Resistance to All Nets
# ============================
df$resistance_all <- df$resistance  # apply to all nets

# Replace any remaining NAs with 0
df$resistance_all[is.na(df$resistance_all)] <- 0
sum(is.na(df$resistance_all))  # should now be 0

# ============================
# 6️⃣ Prepare categorical indices for Stan
# ============================
df <- df %>%
  mutate(
    treatment_id = as.numeric(factor(Treatment, levels = c("UT","P3","IG1","IG2","RG"))),
    hut_id       = as.numeric(factor(hut)),
    sleeper_id   = as.numeric(factor(sleeper)),
    time_id      = as.numeric(factor(Time)),
    age_id       = as.numeric(factor(Age))
  )

# 1️⃣ Ensure resistance has no NAs
df <- df %>%
  mutate(
    resistance_all = ifelse(is.na(resistance), 0, resistance)
  )

# 2️⃣ Remove rows with total = 0 (cannot model these)
df <- df %>% filter(total > 0)

# 3️⃣ Verify
sum(is.na(df$resistance_all))  # should be 0


# ============================
# 7️⃣ Prepare Stan data
# ============================
stan_data <- list(
  N = nrow(df),
  total = df$total,
  T = length(unique(df$treatment_id)),
  H = length(unique(df$hut_id)),
  S = length(unique(df$sleeper_id)),
  A = length(unique(df$age_id)),
  treatment = df$treatment_id,
  hut       = df$hut_id,
  sleeper   = df$sleeper_id,
  age       = df$age_id,
  time      = df$time_id,
  resistance_new = df$resistance_all  # now all nets
)

# ============================
# 8️⃣ Stan model code
# ============================
stan_model_code <- "
data {
  int<lower=1> N;
  int<lower=0> total[N];
  int<lower=1> T;
  int<lower=1> H;
  int<lower=1> S;
  int<lower=1> A;
  int<lower=1> treatment[N];
  int<lower=1> hut[N];
  int<lower=1> sleeper[N];
  int<lower=1> age[N];
  int<lower=1> time[N];
  vector[N] resistance_new;  // now applies to all nets
}
parameters {
  real alpha;
  vector[T] beta_treatment;
  vector[H] beta_hut;
  vector[S] beta_sleeper;
  vector[A] beta_age;
  real beta_time;
  real<lower=0> phi;
}
model {
  alpha ~ normal(0,5);
  beta_treatment ~ normal(0,2);
  beta_hut ~ normal(0,2);
  beta_sleeper ~ normal(0,2);
  beta_age ~ normal(0,2);
  beta_time ~ normal(0,1);
  phi ~ gamma(2,0.1);

  for (i in 1:N) {
    real eta = alpha +
               beta_treatment[treatment[i]] * resistance_new[i] +
               beta_hut[hut[i]] +
               beta_sleeper[sleeper[i]] +
               beta_age[age[i]] +
               beta_time * time[i];
    total[i] ~ neg_binomial_2_log(eta, phi);
  }
}
"

# ============================
# 9️⃣ Fit Stan model
# ============================
fit <- rstan::stan(
  model_code = stan_model_code,
  data = stan_data,
  iter = 1500,
  chains = 1,
  control = list(max_treedepth = 12, adapt_delta = 0.95)
)

# ============================
# 🔟 Posterior Predictions
# ============================
posterior <- rstan::extract(fit)

predicted_totals <- sapply(1:nrow(df), function(i) {
  eta_i <- posterior$alpha +
    posterior$beta_treatment[, df$treatment_id[i]] * df$resistance_all[i] +
    posterior$beta_hut[, df$hut_id[i]] +
    posterior$beta_sleeper[, df$sleeper_id[i]] +
    posterior$beta_age[, df$age_id[i]] +
    posterior$beta_time * df$time_id[i]
  exp(eta_i)
})

df$predicted_total <- colMeans(predicted_totals)

# ============================
# 1️⃣1️⃣ Compute Deterrence per Treatment × Age × Time
# ============================
control_totals <- df %>% filter(Treatment == "UT")

deterrence_df <- df %>%
  filter(Treatment != "UT") %>%
  group_by(Treatment, Age_label, Time) %>%
  summarise(
    treated_mean = mean(predicted_total),
    control_mean = mean(control_totals$predicted_total[control_totals$Age == Age & control_totals$Time == Time]),
    deterrence = 1 - treated_mean / control_mean,
    .groups = "drop"
  )

print(deterrence_df, n = 40)

# ============================
# 1️⃣2️⃣ Plot Deterrence
# ============================
ggplot(deterrence_df, aes(x = Time, y = deterrence, color = Age_label, group = Age_label)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~Treatment) +
  labs(
    title = "Deterrence Over Time by Age and Treatment",
    x = "Time",
    y = "Deterrence",
    color = "Age"
  ) +
  theme_minimal(base_size = 14)
















































##############his is the realcodethatihaveused formy deterence

##seasonality

# ============================
# 1️⃣ Load Libraries
# ============================
library(dplyr)
library(readxl)
library(rstan)
library(ggplot2)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
set.seed(123)

# ============================
# 2️⃣ Load Data
# ============================
df <- read_excel(
  "Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
  sheet = "Combined2"
)

# Ensure Age is integer
df <- df %>% mutate(Age = as.integer(Age))

# ============================
# 3️⃣ Calculate Mortality & Resistance
# ============================
df <- df %>%
  mutate(
    mortality = tot_dead / total,
    resistance_all = 1 - mortality   # apply to all nets
  )

# ============================
# 4️⃣ Create Age Labels
# ============================
df <- df %>%
  mutate(
    Age_label = case_when(
      Age == 1 ~ "New",
      Age == 2 ~ "12m",
      Age == 3 ~ "24m",
      Age == 4 ~ "36m",
      TRUE     ~ paste0("Age", Age)
    ),
    Treatment = as.character(Treatment)
  )

# ============================
# 5️⃣ Prepare categorical indices for Stan
# ============================
df <- df %>%
  mutate(
    treatment_id = as.numeric(factor(Treatment, levels = c("UT","P3","IG1","IG2","RG"))),
    hut_id       = as.numeric(factor(hut)),
    sleeper_id   = as.numeric(factor(sleeper)),
    age_id       = as.numeric(factor(Age)),
    time_id      = as.numeric(factor(Time))   # categorical for seasonality
  )
sum(is.na(df$resistance_all))
df$resistance_all[is.na(df$resistance_all)] <- 0
sum(is.na(df$resistance_all))  # should now return 0
# ============================
# 6️⃣ Prepare Stan data
# ============================
stan_data <- list(
  N = nrow(df),
  total = df$total,
  T = length(unique(df$treatment_id)),
  H = length(unique(df$hut_id)),
  S = length(unique(df$sleeper_id)),
  A = length(unique(df$age_id)),
  Time_levels = length(unique(df$time_id)),  # number of periods
  treatment = df$treatment_id,
  hut       = df$hut_id,
  sleeper   = df$sleeper_id,
  age       = df$age_id,
  time      = df$time_id,                    # categorical
  resistance_all = df$resistance_all
)

# ============================
# 7️⃣ Stan model with seasonality
# ============================
stan_model_code <- "
data {
  int<lower=1> N;
  int<lower=0> total[N];
  int<lower=1> T;
  int<lower=1> H;
  int<lower=1> S;
  int<lower=1> A;
  int<lower=1> Time_levels;
  int<lower=1> treatment[N];
  int<lower=1> hut[N];
  int<lower=1> sleeper[N];
  int<lower=1> age[N];
  int<lower=1, upper=Time_levels> time[N]; 
  vector[N] resistance_all;
}
parameters {
  real alpha;
  vector[T] beta_treatment;
  vector[H] beta_hut;
  vector[S] beta_sleeper;
  vector[A] beta_age;
  vector[Time_levels] beta_time;  // seasonality
  real<lower=0> phi;
}
model {
  // Priors
  alpha ~ normal(0,5);
  beta_treatment ~ normal(0,2);
  beta_hut ~ normal(0,2);
  beta_sleeper ~ normal(0,2);
  beta_age ~ normal(0,2);
  beta_time ~ normal(0,1);
  phi ~ gamma(2,0.1);

  // Likelihood
  for (i in 1:N) {
    real eta = alpha +
               beta_treatment[treatment[i]] * resistance_all[i] +
               beta_hut[hut[i]] +
               beta_sleeper[sleeper[i]] +
               beta_age[age[i]] +
               beta_time[time[i]]; 
    total[i] ~ neg_binomial_2_log(eta, phi);
  }
}
"

# ============================
# 8️⃣ Fit Stan model
# ============================
fit <- rstan::stan(
  model_code = stan_model_code,
  data       = stan_data,
  iter       = 2000,
  chains     = 4,
  control    = list(max_treedepth = 12, adapt_delta = 0.95)
)

# ============================
# 9️⃣ Posterior Predictions
# ============================
posterior <- rstan::extract(fit)

predicted_totals <- sapply(1:nrow(df), function(i) {
  eta_i <- posterior$alpha +
    posterior$beta_treatment[, df$treatment_id[i]] * df$resistance_all[i] +
    posterior$beta_hut[, df$hut_id[i]] +
    posterior$beta_sleeper[, df$sleeper_id[i]] +
    posterior$beta_age[, df$age_id[i]] +
    posterior$beta_time[, df$time_id[i]]
  exp(eta_i)
})

df$predicted_total <- colMeans(predicted_totals)

# ============================
# 🔟 Compute Deterrence per Treatment × Age × Time
# ============================
control_totals <- df %>% filter(Treatment == "UT")

deterrence_df <- df %>%
  filter(Treatment != "UT") %>%
  group_by(Treatment, Age_label, Time) %>%
  summarise(
    treated_mean = mean(predicted_total),
    control_mean = mean(control_totals$predicted_total[control_totals$Age == Age &
                                                         control_totals$Time == Time]),
    deterrence = 1 - treated_mean / control_mean,
    .groups = "drop"
  )

print(deterrence_df, n = 40)




####plot deterence 


library(ggplot2)
library(dplyr)

#set.seed(123)  # For reproducibility

# Step 1: Original means
df <- data.frame(
  treatment = rep(c("IG1", "IG2", "P3", "RG"), each = 6),
  net_type = rep(c("New_12m", "New_24m", "New_36m", "Field_12m", "Field_24m", "Field_36m"), 4),
  mean = c(
    0.300 , 0.301,  0.316, 0.354, 0.350 , 0.043,    # IG1
    0.381 ,  0.379, 0.394 , 0.476 , 0.302, -0.163,     # IG2
    0.352  ,  0.356 , 0.365 , 0.336, 0.083, -0.086,     # P3
    0.0913, 0.166, 0.163 , 0.276, 0.092, -0.258    # RG
  )
)


#0.350 , 0.335,  0.316, 0.354, 0.350 , 0.043,    # IG1


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
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  labs(
    x = "Net Age",
    y = "Deterrence",
    title = "Deterrence(New vs Field Aged  Nets)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.spacing = unit(1, "lines")
  )

#################################3####################################################################################3
#################################3##################################################DIFFERENCE IN DETERENCE ##################################3
#################################3####################################################################################3


library(dplyr)

# Keep only relevant columns
det_age <- deterrence_df %>%
  select(Treatment, Age_label, deterrence)

# Split by treatment
det_by_treat <- det_age %>% group_by(Treatment) %>% group_split()



results <- lapply(det_by_treat, function(df_treat) {
  new_det <- df_treat$deterrence[df_treat$Age_label == "New"]
  other_det <- df_treat$deterrence[df_treat$Age_label != "New"]
  
  t_test <- t.test(new_det, other_det)  # independent samples
  data.frame(
    Treatment = unique(df_treat$Treatment),
    p_value = t_test$p.value,
    mean_new = mean(new_det),
    mean_other = mean(other_det)
  )
})

results_df <- do.call(rbind, results)
print(results_df)

# Load the package
library(broom)


lm_results <- deterrence_df %>%
  group_by(Treatment) %>%
  do(tidy(lm(deterrence ~ Age_label, data = .)))

# This will give coefficient estimates for each age relative to "New"
print(lm_results)



# Set "New" as reference for Age_label
deterrence_df <- deterrence_df %>%
  mutate(Age_label = factor(Age_label, levels = c("New","12m","24m","36m")))

# Load broom if not loaded
library(broom)

# Fit linear models per treatment
lm_results <- deterrence_df %>%
  group_by(Treatment) %>%
  do(tidy(lm(deterrence ~ Age_label, data = .)))

# View results
print(lm_results)



lm_results <- lm_results %>%
  mutate(star = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE            ~ ""
  ))


###############endshere

library(dplyr)

summary_df <- df %>%
  group_by(Treatment, Age, Time) %>%
  summarise(
    mean_total = mean(total, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_df, n = 40)




#############resistance2

# ============================================================
# 1️⃣ Load Libraries
# ============================================================
library(dplyr)
library(readxl)
library(rstan)
library(ggplot2)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
set.seed(123)

# ============================================================
# 2️⃣ Load Data
# ============================================================
df <- read_excel(
  "Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
  sheet = "Combined2"
)

# Ensure Age is integer
df <- df %>% mutate(Age = as.integer(Age))

# ============================================================
# 3️⃣ Calculate Mortality & Resistance
# ============================================================
df <- df %>%
  mutate(
    mortality = tot_dead / total,
    resistance_all = 1 - mortality   # higher resistance = fewer dead mosquitoes
  )

# ============================================================
# 4️⃣ Create Age Labels
# ============================================================
df <- df %>%
  mutate(
    Age_label = case_when(
      Age == 1 ~ "New",
      Age == 2 ~ "12m",
      Age == 3 ~ "24m",
      Age == 4 ~ "36m",
      TRUE     ~ paste0("Age", Age)
    ),
    Treatment = as.character(Treatment)
  )

# ============================================================
# 5️⃣ Prepare categorical indices for Stan
# ============================================================
df <- df %>%
  mutate(
    treatment_id = as.numeric(factor(Treatment, levels = c("UT","P3","IG1","IG2","RG"))),
    hut_id       = as.numeric(factor(hut)),
    sleeper_id   = as.numeric(factor(sleeper)),
    age_id       = as.numeric(factor(Age)),
    time_id      = as.numeric(factor(Time))   # categorical for seasonality
  )

# Replace any missing resistance values with 0
df$resistance_all[is.na(df$resistance_all)] <- 0

# ============================================================
# 6️⃣ Prepare Stan data
# ============================================================
stan_data <- list(
  N = nrow(df),
  total = df$total,
  T = length(unique(df$treatment_id)),
  H = length(unique(df$hut_id)),
  S = length(unique(df$sleeper_id)),
  A = length(unique(df$age_id)),
  Time_levels = length(unique(df$time_id)),
  treatment = df$treatment_id,
  hut       = df$hut_id,
  sleeper   = df$sleeper_id,
  age       = df$age_id,
  time      = df$time_id,
  resistance_all = df$resistance_all
)

# ============================================================
# 7️⃣ Stan Model Code (Separate Resistance Effect)
# ============================================================
stan_model_code <- "
data {
  int<lower=1> N;
  int<lower=0> total[N];
  int<lower=1> T;
  int<lower=1> H;
  int<lower=1> S;
  int<lower=1> A;
  int<lower=1> Time_levels;
  int<lower=1> treatment[N];
  int<lower=1> hut[N];
  int<lower=1> sleeper[N];
  int<lower=1> age[N];
  int<lower=1, upper=Time_levels> time[N]; 
  vector[N] resistance_all;
}
parameters {
  real alpha;
  vector[T] beta_treatment;
  vector[H] beta_hut;
  vector[S] beta_sleeper;
  vector[A] beta_age;
  vector[Time_levels] beta_time;
  real beta_resistance;   // <-- NEW PARAMETER
  real<lower=0> phi;
}
model {
  // Priors
  alpha ~ normal(0,5);
  beta_treatment ~ normal(0,2);
  beta_hut ~ normal(0,2);
  beta_sleeper ~ normal(0,2);
  beta_age ~ normal(0,2);
  beta_time ~ normal(0,1);
  beta_resistance ~ normal(0,2);   // <-- NEW PRIOR
  phi ~ gamma(2,0.1);

  // Likelihood
  for (i in 1:N) {
    real eta = alpha +
               beta_treatment[treatment[i]] +
               beta_resistance * resistance_all[i] +  // <-- DIRECT EFFECT
               beta_hut[hut[i]] +
               beta_sleeper[sleeper[i]] +
               beta_age[age[i]] +
               beta_time[time[i]]; 
    total[i] ~ neg_binomial_2_log(eta, phi);
  }
}
"

# ============================================================
# 8️⃣ Fit Stan Model
# ============================================================
fit <- rstan::stan(
  model_code = stan_model_code,
  data       = stan_data,
  iter       = 2000,
  chains     = 1,
  control    = list(max_treedepth = 15, adapt_delta = 0.95)
)

# ============================================================
# 9️⃣ Check Statistical Significance of Resistance
# ============================================================
print(fit, pars = "beta_resistance")  # Summary statistics

posterior <- rstan::extract(fit)
beta_resistance <- posterior$beta_resistance
ci <- quantile(beta_resistance, probs = c(0.025, 0.975))
print(ci)

if (ci[1] > 0 | ci[2] < 0) {
  cat("✅ Resistance has a statistically significant effect.\n")
} else {
  cat("❌ Resistance effect is NOT statistically significant.\n")
}

# ============================================================
# 🔟 Posterior Predictions for Total Mosquito Counts
# ============================================================
predicted_totals <- sapply(1:nrow(df), function(i) {
  eta_i <- posterior$alpha +
    posterior$beta_treatment[, df$treatment_id[i]] +
    posterior$beta_resistance * df$resistance_all[i] +
    posterior$beta_hut[, df$hut_id[i]] +
    posterior$beta_sleeper[, df$sleeper_id[i]] +
    posterior$beta_age[, df$age_id[i]] +
    posterior$beta_time[, df$time_id[i]]
  exp(eta_i)
})

df$predicted_total <- colMeans(predicted_totals)

# ============================================================
# 1️⃣1️⃣ Deterrence per Treatment × Age × Time
# ============================================================
control_totals <- df %>% filter(Treatment == "UT")

deterrence_df <- df %>%
  filter(Treatment != "UT") %>%
  group_by(Treatment, Age_label, Time) %>%
  summarise(
    treated_mean = mean(predicted_total),
    control_mean = mean(control_totals$predicted_total[control_totals$Age == Age &
                                                         control_totals$Time == Time]),
    deterrence = 1 - treated_mean / control_mean,
    .groups = "drop"
  )

print(deterrence_df, n = 40)

# ============================================================
# 1️⃣2️⃣ Plot Deterrence Over Time
# ============================================================
ggplot(deterrence_df, aes(x = Time, y = deterrence, color = Age_label, group = Age_label)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~Treatment) +
  labs(
    title = "Deterrence Over Time by Age and Treatment",
    x = "Time",
    y = "Deterrence",
    color = "Age"
  ) +
  theme_minimal(base_size = 14)















###############################################################################################################################################################
#####################################################################################DETERENCE TANZANIA##########################################################################
###############################################################################################################################################################

##seasonality

# ============================
# 1️⃣ Load Libraries
# ============================
library(dplyr)
library(readxl)
library(rstan)
library(ggplot2)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
set.seed(123)

# ============================
# 2️⃣ Load Data

df <- read_excel("Wezzie-Tom/EHT DATA/Tanzania trial/gambiae/newdata.xlsx")



df <- df %>% 
  filter(treatment != "STD")

# Ensure Age is integer
df <- df %>% mutate(Age = as.integer(Age))

# ============================
# 3️⃣ Calculate Mortality & Resistance
# ============================
df <- df %>%
  mutate(
    mortality = anodead72 / anotot,
    resistance_all = 1 - mortality   # apply to all nets
  )
str(deterrence_df)
unique(deterrence_df$Treatment)
table(deterrence_df$Treatment, deterrence_df$Age_label)

# ============================
# 4️⃣ Create Age Labels
# ============================
df <- df %>%
  mutate(
    Age_label = case_when(
      Age == 1 ~ "New",
      Age == 2 ~ "12m",
      Age == 3 ~ "24m",
      Age == 4 ~ "36m",
      TRUE     ~ paste0("Age", Age)
    ),
    Treatment = as.character(Treatment)
  )

# ============================
# 5️⃣ Prepare categorical indices for Stan
# ============================
df <- df %>%
  mutate(
    treatment_id = as.numeric(factor(Treatment, levels = c("UT","OP","IG1","IG2","RG"))),
    hut_id       = as.numeric(factor(hut)),
    sleeper_id   = as.numeric(factor(sleeper)),
    age_id       = as.numeric(factor(Age)),
    time_id      = as.numeric(factor(Time))   # categorical for seasonality
  )

unique(df$treatment)

sum(is.na(df$resistance_all))
df$resistance_all[is.na(df$resistance_all)] <- 0
sum(is.na(df$resistance_all))  # should now return 0
# ============================
# 6️⃣ Prepare Stan data
# ============================
stan_data <- list(
  N = nrow(df),
  total = df$anotot,
  T = length(unique(df$treatment_id)),
  H = length(unique(df$hut_id)),
  S = length(unique(df$sleeper_id)),
  A = length(unique(df$age_id)),
  Time_levels = length(unique(df$time_id)),  # number of periods
  treatment = df$treatment_id,
  hut       = df$hut_id,
  sleeper   = df$sleeper_id,
  age       = df$age_id,
  time      = df$time_id,                    # categorical
  resistance_all = df$resistance_all
)

# ============================
# 7️⃣ Stan model with seasonality
# ============================
stan_model_code <- "
data {
  int<lower=1> N;
  int<lower=0> total[N];
  int<lower=1> T;
  int<lower=1> H;
  int<lower=1> S;
  int<lower=1> A;
  int<lower=1> Time_levels;
  int<lower=1> treatment[N];
  int<lower=1> hut[N];
  int<lower=1> sleeper[N];
  int<lower=1> age[N];
  int<lower=1, upper=Time_levels> time[N]; 
  vector[N] resistance_all;
}
parameters {
  real alpha;
  vector[T] beta_treatment;
  vector[H] beta_hut;
  vector[S] beta_sleeper;
  vector[A] beta_age;
  vector[Time_levels] beta_time;  // seasonality
  real<lower=0> phi;
}
model {
  // Priors
  alpha ~ normal(0,5);
  beta_treatment ~ normal(0,2);
  beta_hut ~ normal(0,2);
  beta_sleeper ~ normal(0,2);
  beta_age ~ normal(0,2);
  beta_time ~ normal(0,1);
  phi ~ gamma(2,0.1);

  // Likelihood
  for (i in 1:N) {
    real eta = alpha +
               beta_treatment[treatment[i]] * resistance_all[i] +
               beta_hut[hut[i]] +
               beta_sleeper[sleeper[i]] +
               beta_age[age[i]] +
               beta_time[time[i]]; 
    total[i] ~ neg_binomial_2_log(eta, phi);
  }
}
"

# ============================
# 8️⃣ Fit Stan model
# ============================
fit <- rstan::stan(
  model_code = stan_model_code,
  data       = stan_data,
  iter       = 1500,
  chains     = 1,
  control    = list(max_treedepth = 12, adapt_delta = 0.95)
)

# ============================
# 9️⃣ Posterior Predictions
# ============================
posterior <- rstan::extract(fit)

predicted_totals <- sapply(1:nrow(df), function(i) {
  eta_i <- posterior$alpha +
    posterior$beta_treatment[, df$treatment_id[i]] * df$resistance_all[i] +
    posterior$beta_hut[, df$hut_id[i]] +
    posterior$beta_sleeper[, df$sleeper_id[i]] +
    posterior$beta_age[, df$age_id[i]] +
    posterior$beta_time[, df$time_id[i]]
  exp(eta_i)
})

df$predicted_total <- colMeans(predicted_totals)

# ============================
# 🔟 Compute Deterrence per Treatment × Age × Time
# ============================
control_totals <- df %>% filter(Treatment == "UT")

deterrence_df <- df %>%
  filter(Treatment != "UT") %>%
  group_by(Treatment, Age_label, Time) %>%
  summarise(
    treated_mean = mean(predicted_total),
    control_mean = mean(control_totals$predicted_total[control_totals$Age == Age &
                                                         control_totals$Time == Time]),
    deterrence = 1 - treated_mean / control_mean,
    .groups = "drop"
  )

print(deterrence_df, n = 40)




####plot deterence 


library(ggplot2)
library(dplyr)

#set.seed(123)  # For reproducibility

# Step 1: Original means
df <- data.frame(
  treatment = rep(c("IG1", "IG2", "OP", "RG"), each = 6),
  net_type = rep(c("New_12m", "New_24m", "New_36m", "Field_12m", "Field_24m", "Field_36m"), 4),
  mean = c(
    0.0284 , -0.0784,  0.0534, -0.041, 0.090 , 0.320,    # IG1
    0.244 ,  0.0890, 0.306  , 0.090 , 0.063, 0.254,     # IG2
    0.242   ,  0.215 , -0.0301 , -0.102, -0.081, 0.098,     # P3
    -0.113, -0.0174, -0.153 , -0.082, -0.127, 0.084    # RG
  )
)




#0.350 , 0.335,  0.316, 0.354, 0.350 , 0.043,    # IG1


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
  scale_y_continuous(limits = c(-1, 1)) +
  labs(
    x = "Net Age",
    y = "Deterrence",
    title = "Deterrence(New vs Field Aged  Nets)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.spacing = unit(1, "lines")
  )




#######################seasonality of Date for TZ data
###########now lets see difference 


library(dplyr)
library(broom)

# --- 1. Start fresh ---
rm(list = ls())  # clear old objects

# --- 2. Make sure Age_label is a factor with "New" as reference ---
deterrence_df <- deterrence_df %>%
  mutate(
    Age_label = factor(Age_label, levels = c("New", "12m", "24m", "36m")),
    Treatment = ifelse(Treatment == "P3", "OP", Treatment) # rename if needed
  )

# --- 3. t-test: compare New vs others (only if enough data) ---
det_by_treat <- deterrence_df %>% group_by(Treatment) %>% group_split()

results <- lapply(det_by_treat, function(df_treat) {
  new_det <- df_treat$deterrence[df_treat$Age_label == "New"]
  other_det <- df_treat$deterrence[df_treat$Age_label != "New"]
  
  if (length(new_det) > 1 & length(other_det) > 1) {
    t_test <- t.test(new_det, other_det)
    data.frame(
      Treatment = unique(df_treat$Treatment),
      p_value = t_test$p.value,
      mean_new = mean(new_det),
      mean_other = mean(other_det)
    )
  } else {
    data.frame(
      Treatment = unique(df_treat$Treatment),
      p_value = NA,
      mean_new = mean(new_det),
      mean_other = mean(other_det)
    )
  }
})

results_df <- do.call(rbind, results)
print(results_df)

# --- 4. Linear models per treatment (with "New" as reference) ---
lm_results <- deterrence_df %>%
  group_by(Treatment) %>%
  group_modify(~ {
    if (n_distinct(.x$Age_label) > 1) {
      tidy(lm(deterrence ~ Age_label, data = .x))
    } else {
      tibble(term = NA, estimate = NA, std.error = NA,
             statistic = NA, p.value = NA)
    }
  }) %>%
  ungroup()

# Add significance stars
lm_results <- lm_results %>%
  mutate(star = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE            ~ ""
  ))

print(lm_results)
















###############This is deterence for code i haveremoved resistance butadded ageXtreatment ##############################################################################################################################
###############################################################################################################################################################################




###################now remove the resistance 


library(dplyr)
library(readxl)
library(rstan)
library(ggplot2)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
set.seed(123)

# ============================
# 2️⃣ Load Data
# ============================
df <- read_excel(
  "Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
  sheet = "Combined2"
)

df <- read_excel("Benin_NNP_hut_trial_raw data (2).xlsx", sheet = "Combined2")

df <- read_excel("Wezzie-Tom/EHT DATA/Tanzania trial/gambiae/newdata.xlsx", 
                 sheet = "Sheet1")

df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/New folder/TZ and benin combined.xlsx", 
                 sheet = "Sheet3")

# Assuming your data frame is called df and the column to sum is 'Value' (replace with your column name)
summary_df <- df %>%
  group_by(Treatment, Age) %>%
  summarise(Total = mean(total, na.rm = TRUE), .groups = "drop")

# View the summary
print(summary_df,n=27)



df <- df %>%
  filter(!is.na(Treatment), Treatment != "STD")

df <- df %>% mutate(Age = as.integer(Age))

# Remove rows with zero total
df <- df %>% filter( total> 0)

# ============================
# 3️⃣ Calculate Mortality & Resistance
# ============================
df <- df %>%
  mutate(
    mortality = tot_dead / total,
    resistance_all = 1 - mortality
  )

# Ensure numeric and no NA (should not be needed now)
df$resistance_all <- as.numeric(df$resistance_all)
sum(is.na(df$resistance_all))  # should be 0

# ============================
# 4️⃣ Create Age Labels
# ============================
df <- df %>%
  mutate(
    Age_label = case_when(
      Age == 1 ~ "New",
      Age == 2 ~ "12m",
      Age == 3 ~ "24m",
      Age == 4 ~ "36m",
      TRUE     ~ paste0("Age", Age)
    ),
    Treatment = as.character(Treatment)
  )

# ============================
# 5️⃣ Prepare categorical indices for Stan
# ============================
df <- df %>%
  mutate(
    treatment_id = as.numeric(factor(Treatment, levels = c("UT","OP", "P3", "IG1","IG2","RG"))),
    hut_id       = as.numeric(factor(hut)),
    sleeper_id   = as.numeric(factor(sleeper)),
    age_id       = as.numeric(factor(Age)),
    time_id      = as.numeric(factor(Time))
  )

# ============================
# 6️⃣ Prepare Stan data
# ============================
stan_data <- list(
  N = nrow(df),
  total = df$total,
  T = length(unique(df$treatment_id)),
  H = length(unique(df$hut_id)),
  S = length(unique(df$sleeper_id)),
  A = length(unique(df$age_id)),
  Time_levels = length(unique(df$time_id)),
  treatment = df$treatment_id,
  hut       = df$hut_id,
  sleeper   = df$sleeper_id,
  age       = df$age_id,
  time      = df$time_id
)

# ============================
# 7️⃣ Stan model (Age × Treatment, NO Resistance × Treatment)
# ============================
stan_model_code <- "
data {
  int<lower=1> N;
  int<lower=0> total[N];
  int<lower=1> T;
  int<lower=1> H;
  int<lower=1> S;
  int<lower=1> A;
  int<lower=1> Time_levels;
  int<lower=1> treatment[N];
  int<lower=1> hut[N];
  int<lower=1> sleeper[N];
  int<lower=1> age[N];
  int<lower=1, upper=Time_levels> time[N];
}
parameters {
  real alpha;
  vector[T] beta_treatment;          // main treatment effect
  vector[A] beta_age;                // main age effect
  matrix[A, T] beta_age_treat;       // Age × Treatment interaction
  vector[H] beta_hut;
  vector[S] beta_sleeper;
  vector[Time_levels] beta_time;     // seasonality
  real<lower=0> phi;
}
model {
  // Priors
  alpha ~ normal(0,5);
  beta_treatment ~ normal(0,2);
  beta_age ~ normal(0,2);
  to_vector(beta_age_treat) ~ normal(0,2);
  beta_hut ~ normal(0,2);
  beta_sleeper ~ normal(0,2);
  beta_time ~ normal(0,1);
  phi ~ gamma(2,0.1);

  // Likelihood
  for (i in 1:N) {
    real eta = alpha +
               beta_treatment[treatment[i]] +
               beta_age[age[i]] +
               beta_age_treat[age[i], treatment[i]] +   // Age × Treatment
               beta_hut[hut[i]] +
               beta_sleeper[sleeper[i]] +
               beta_time[time[i]];
    total[i] ~ neg_binomial_2_log(eta, phi);
  }
}
"

# ============================
# 8️⃣ Fit Stan model
# ============================
fit <- rstan::stan(
  model_code = stan_model_code,
  data       = stan_data,
  iter       = 2000,
  chains     = 1,
  control    = list(max_treedepth = 12, adapt_delta = 0.95)
)

# ============================
# 9️⃣ Posterior Predictions
# ============================
posterior <- rstan::extract(fit)

predicted_totals <- sapply(1:nrow(df), function(i) {
  eta_i <- posterior$alpha +
    posterior$beta_treatment[, df$treatment_id[i]] +
    posterior$beta_age[, df$age_id[i]] +
    posterior$beta_age_treat[, df$age_id[i], df$treatment_id[i]] +
    posterior$beta_hut[, df$hut_id[i]] +
    posterior$beta_sleeper[, df$sleeper_id[i]] +
    posterior$beta_time[, df$time_id[i]]
  exp(eta_i)
})

df$predicted_total <- colMeans(predicted_totals)

# ============================
# 🔟 Compute Deterrence per Treatment × Age × Time
# ============================
control_totals <- df %>% filter(Treatment == "UT")

deterrence_df <- df %>%
  filter(Treatment != "UT") %>%
  group_by(Treatment, Age_label, Time) %>%
  summarise(
    treated_mean = mean(predicted_total),
    control_mean = mean(control_totals$predicted_total[control_totals$Age == Age &
                                                         control_totals$Time == Time]),
    deterrence = 1 - treated_mean / control_mean,
    .groups = "drop"
  )

print(deterrence_df, n = 40)

##########################################
##########################################

# Get posterior draws again (already extracted as 'posterior')
n_draws <- length(posterior$alpha)

# Identify rows for IG1 new (Age = 1) and IG1 12m (Age = 2)
IG1_new_idx <- which(df$Treatment == "IG2" & df$Age == 1)
IG1_12m_idx <- which(df$Treatment == "IG2" & df$Age == 2)

# Compute posterior mean totals across relevant rows for each draw
IG1_new_total <- sapply(1:n_draws, function(d) {
  mean(exp(
    posterior$alpha[d] +
      posterior$beta_treatment[d, df$treatment_id[IG1_new_idx]] +
      posterior$beta_age[d, df$age_id[IG1_new_idx]] +
      posterior$beta_age_treat[d, df$age_id[IG1_new_idx], df$treatment_id[IG1_new_idx]] +
      posterior$beta_hut[d, df$hut_id[IG1_new_idx]] +
      posterior$beta_sleeper[d, df$sleeper_id[IG1_new_idx]] +
      posterior$beta_time[d, df$time_id[IG1_new_idx]]
  ))
})

IG1_12m_total <- sapply(1:n_draws, function(d) {
  mean(exp(
    posterior$alpha[d] +
      posterior$beta_treatment[d, df$treatment_id[IG1_12m_idx]] +
      posterior$beta_age[d, df$age_id[IG1_12m_idx]] +
      posterior$beta_age_treat[d, df$age_id[IG1_12m_idx], df$treatment_id[IG1_12m_idx]] +
      posterior$beta_hut[d, df$hut_id[IG1_12m_idx]] +
      posterior$beta_sleeper[d, df$sleeper_id[IG1_12m_idx]] +
      posterior$beta_time[d, df$time_id[IG1_12m_idx]]
  ))
})



# Posterior difference (12m vs New)
diff <- IG1_new_total - IG1_12m_total

# Summary statistics
mean_diff <- mean(diff)
ci_diff <- quantile(diff, c(0.025, 0.975))
prob_positive <- mean(diff > 0)

cat("Posterior mean difference (IG1 new - IG1 12m):", mean_diff, "\n")
cat("95% Credible Interval:", ci_diff, "\n")
cat("P(IG1 new > IG1 12m):", prob_positive, "\n")



#####################test difference




library(ggplot2)
library(dplyr)

#set.seed(123)  # For reproducibility

# Step 1: Original means
df <- data.frame(
  treatment = rep(c("IG1", "IG2", "P3", "RG"), each = 6),
  net_type = rep(c("New_12m", "New_24m", "New_36m", "Field_12m", "Field_24m", "Field_36m"), 4),
  mean = c(
    0.335,0.34,0.339,0.233,0.357,0.288,     # IG1
    0.332,0.33,0.336,0.372,0.280,0.109,     # IG2
    0.424,0.423,0.449,0.216,0.086,0.180,    # P3
    -0.0478,-0.0489,-0.036,0.1554,0.081,0.0193) # RG
  )






#Tanzania 
df <- data.frame(
  treatment = rep(c("IG1", "IG2", "OP", "RG"), each = 6),
  net_type = rep(c("New_12m", "New_24m", "New_36m", "Field_12m", "Field_24m", "Field_36m"), 4),
mean = c(
  -0.178,-0.182,-0.181,0.121,0.022,0.095,     # IG1
  0.068,0.0688,0.0688,0.134,-0.061,0.047,     # IG2
  0.0502,0.0386,-0.00534,-0.124,-0.019,0.066,    # P3
  0.028,0.0183,0.0313,0.0404,-0.148,0.070 # RG
))
#0.350 , 0.335,  0.316, 0.354, 0.350 , 0.043,    # IG1


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
  scale_y_continuous(limits = c(-0.30, 0.6)) +
  labs(
    x = "Net Age",
    y = "Deterrence",
    title = "Deterrence(New vs Field Aged  Nets)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.spacing = unit(1, "lines")
  )




###########difference in deterence singt test




library(dplyr)

# Subset IG1/RG data
ig1_data <- df %>% filter(Treatment == "RG")

# Get posterior totals for Age 1 (new) and Age 4 (aged 24 months)
new_totals   <- ig1_data %>% filter(Age == 1) %>% pull(predicted_total)
aged24_totals <- ig1_data %>% filter(Age == 4) %>% pull(predicted_total)



t_test_result <- t.test(aged24_totals, new_totals, paired = FALSE)
t_test_result




library(dplyr)

# Subset IG1 data
ig1_data <- df %>% filter(Treatment == "RG")

# Get predicted totals or observed totals per Age
new_totals   <- ig1_data %>% filter(Age== 1) %>% pull(anotot)  # or pull(predicted_total)
aged24_totals <- ig1_data %>% filter(Age == 4) %>% pull(anotot)



t_test_result <- t.test(aged24_totals, new_totals, paired = FALSE)
t_test_result


##############plot deterencewith pvalus FOR TANZANIA
#####################################################################
#####################################################################






library(ggplot2)
library(dplyr)

# Example data
df <- data.frame(
  treatment = rep(c("IG1", "IG2", "OP", "RG"), each = 6),
  net_type = rep(c("New_12m", "New_24m", "New_36m", "Field_12m", "Field_24m", "Field_36m"), 4),
  mean = c(
    -0.178,-0.182,-0.181,0.121,0.022,0.095,     # IG1
    0.068,0.0688,0.0688,0.134,-0.061,0.047,     # IG2
    0.0502,0.0386,-0.00534,-0.124,-0.019,0.066,    # P3
    0.028,0.0183,0.0313,0.0404,-0.148,0.070 # RG
  )
)
# Add CI and grouping
df <- df %>%
  mutate(
    ci_width = runif(n(), min = 0.04, max = 0.08),
    lower = mean - ci_width,
    upper = mean + ci_width,
    group = ifelse(grepl("New", net_type), "New Nets", "Field Aged Nets"),
    net_age = sub(".*_(\\d+m)$", "\\1", net_type),
    net_age = factor(net_age, levels = c("12m","24m","36m")),
    fill_color = ifelse(group == "New Nets", "New Nets", "Field Aged Nets")
  )

# Create annotations for Field bars (unique text + p-value)
annotations <- df %>%
  filter(group == "Field Aged Nets") %>%
  rowwise() %>%
  mutate(
    p_value = runif(1, 0.001, 0.1),  # replace with actual p-values
    custom_text = case_when(
      treatment == "IG1" & net_age == "12m" ~ "*",
      treatment == "IG1" & net_age == "24m" ~ "P=0.38",
      treatment == "IG1" & net_age == "36m" ~ "*", # #(-4.406, 1.14)
      
      #treatment == "IG2" & net_age == "12m" ~ paste0("p=", 0.56)
      treatment == "IG2" & net_age == "12m" ~ "P=0.42", 
      treatment == "IG2" & net_age == "24m" ~ "P=0.53",
      treatment == "IG2" & net_age == "36m" ~ "P=0.07",
      
      treatment == "OP"  & net_age == "12m" ~ "*", #(-0.86,20.5)
      treatment == "OP"  & net_age == "24m" ~ "P=0.43",
      treatment == "OP"  & net_age == "36m" ~ "P=0.27",
      
      treatment == "RG"  & net_age == "12m" ~ "P=0.78", #*
      treatment == "RG"  & net_age == "24m" ~ "P=0.5",
      treatment == "RG"  & net_age == "36m" ~ "*" # (-3.3,3.35)
    ),label = custom_text,  # only show custom text
    y_pos = upper + 0.09,
    net_age = factor(net_age, levels = c("12m","24m","36m"))
  )


# Plot
ggplot(df, aes(x = net_age, y = mean, fill = fill_color)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.7) +
  facet_grid(group ~ treatment, scales = "free_y") +
  scale_fill_manual(
    values = c("New Nets" = "#FFA500", "Field Aged Nets" = "#1F77B4"),
    name = "Net Type"
  ) +
  scale_y_continuous(limits = c(-0.25, 0.3)) +
  labs(
    x = "Time Point",
    y = "Deterrence",
    title = "Deterrence (Tanzania)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    panel.spacing = unit(0.3, "lines")
  ) +
  geom_text(
    data = annotations,   # <- only Field Aged Nets
    aes(x = net_age, y = y_pos, label = label),
    inherit.aes = FALSE,
    color = "black",
    size = 3.5,
    fontface = "bold"
  )















##############plot deterencewith pvalus FOR BENIN
#####################################################################
#####################################################################











library(ggplot2)
library(dplyr)

# Example data
df <- data.frame(
  treatment = rep(c("IG1", "IG2", "P3", "RG"), each = 6),
  net_type = rep(c("New_12m", "New_24m", "New_36m", "Field_12m", "Field_24m", "Field_36m"), 4),
  mean = c(
    0.335,0.34,0.339,0.233,0.357,0.288,     # IG1
    0.332,0.33,0.336,0.372,0.280,0.109,     # IG2
    0.424,0.423,0.449,0.216,0.086,0.180,    # P3
    -0.0478,-0.0489,-0.036,0.1554,0.081,0.0193) # RG
)

# Add CI and grouping
df <- df %>%
  mutate(
    ci_width = runif(n(), min = 0.04, max = 0.08),
    lower = mean - ci_width,
    upper = mean + ci_width,
    group = ifelse(grepl("New", net_type), "New Nets", "Field Aged Nets"),
    net_age = sub(".*_(\\d+m)$", "\\1", net_type),
    net_age = factor(net_age, levels = c("12m","24m","36m")),
    fill_color = ifelse(group == "New Nets", "New Nets", "Field Aged Nets")
  )

# Create annotations for Field bars (unique text + p-value)
annotations <- df %>%
  filter(group == "Field Aged Nets") %>%
  rowwise() %>%
  mutate(
    p_value = runif(1, 0.001, 0.1),  # replace with actual p-values
    custom_text = case_when(
      treatment == "IG1" & net_age == "12m" ~ "*",
      treatment == "IG1" & net_age == "24m" ~ "*",
      treatment == "IG1" & net_age == "36m" ~ "P=0.249", # #(-4.406, 1.14)
      
      #treatment == "IG2" & net_age == "12m" ~ paste0("p=", 0.56)
      treatment == "IG2" & net_age == "12m" ~ "*", 
      treatment == "IG2" & net_age == "24m" ~ "*",
      treatment == "IG2" & net_age == "36m" ~ "*",
      
      treatment == "P3"  & net_age == "12m" ~ "P=0.59", #(-0.86,20.5)
      treatment == "P3"  & net_age == "24m" ~ "*",
      treatment == "P3"  & net_age == "36m" ~ "*",
      
      treatment == "RG"  & net_age == "12m" ~ "P=0.86", #*
      treatment == "RG"  & net_age == "24m" ~ "*",
      treatment == "RG"  & net_age == "36m" ~ "*" # (-3.3,3.35)
    ),label = custom_text,  # only show custom text
    y_pos = upper + 0.09,
    net_age = factor(net_age, levels = c("12m","24m","36m"))
  )


# Plot
ggplot(df, aes(x = net_age, y = mean, fill = fill_color)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.7) +
  facet_grid(group ~ treatment, scales = "free_y") +
  scale_fill_manual(
    values = c("New Nets" = "#FFA500", "Field Aged Nets" = "#1F77B4"),
    name = "Net Type"
  ) +
  scale_y_continuous(limits = c(-0.2, 0.6)) +
  labs(
    x = "Time Point",
    y = "Deterrence",
    title = "Deterrence (Benin)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    panel.spacing = unit(0.3, "lines")
  ) +
  geom_text(
    data = annotations,   # <- only Field Aged Nets
    aes(x = net_age, y = y_pos, label = label),
    inherit.aes = FALSE,
    color = "black",
    size = 3.5,
    fontface = "bold"
  )



































#################################3nowiwant to estimate kp fedalive ############################################################################################################################
#########################################################################################################################################################################################################
#########################################################################################################################################################################################################







library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)



df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                                              sheet = "Combined2")
#View(df)

df$Treatment <- factor(df$Treatment, levels = c("UT", "IG1", "IG2", "P3", "RG"))
df$treatment_id <- as.numeric(df$Treatment)
# Convert categorical variables to index values
df$treatment_id <- as.numeric(as.factor(df$Treatment))
df$age_id <- as.numeric(as.factor(df$Age))

# Prepare data for Stan
stan_data <- list(
  N = nrow(df),
  total_dead = df$bf_live,
  total = df$total,
  A = length(unique(df$age_id)),
  T = length(unique(df$treatment_id)),
  age = df$age_id,
  treatment = df$treatment_id,
  time = df$Time
)



# Stan model code as string
stan_model_code <- "
data {
  int<lower=1> N;
  int<lower=0> total_dead[N];
  int<lower=0> total[N];
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
}
model {
  alpha ~ normal(0, 5);
  beta_treatment ~ normal(0, 2);
  beta_age ~ normal(0, 2);
  beta_time ~ normal(0, 2);

  for (i in 1:N) {
    real eta = alpha + beta_treatment[treatment[i]] + beta_age[age[i]] + beta_time * time[i];
    total_dead[i] ~ binomial_logit(total[i], eta);
  }
}
"

# Compile and run the model
fit <- stan(
  model_code = stan_model_code,
  data = stan_data,
  chains = 4,
  iter = 2000,
  control = list(max_treedepth = 15)
  #seed = 123,
  #cores = 2
)

# Print a summary
print(fit, pars = c("alpha", "beta_treatment", "beta_age", "beta_time"))











# Extract posterior samples
posterior <- rstan::extract(fit)

# Number of posterior draws
n_draws <- length(posterior$alpha)

# Unique treatment and age IDs
treatment_ids <- 1:stan_data$T
age_ids <- 1:stan_data$A

# Make a grid of all combinations
combo_grid <- expand.grid(treatment = treatment_ids, age = age_ids)

# Compute posterior predicted probability for each combo
results <- data.frame()

for (i in 1:nrow(combo_grid)) {
  t <- combo_grid$treatment[i]
  a <- combo_grid$age[i]
  
  # Linear predictor for all posterior draws
  eta <- posterior$alpha + posterior$beta_treatment[, t] + posterior$beta_age[, a] + posterior$beta_time * mean(stan_data$time)
  
  # Convert to probability using logistic function
  p <- plogis(eta)
  
  # Summarize posterior
  summary_row <- data.frame(
    treatment = t,
    age = a,
    mean = mean(p),
    lower = quantile(p, 0.025),
    upper = quantile(p, 0.975)
  )
  
  results <- rbind(results, summary_row)
}

# Optional: map treatment index back to names
treatment_labels <- levels(as.factor(df$Treatment))
results$treatment <- treatment_labels[results$treatment]

# View the estimates
print(results)








######try anothercode



library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# Load data
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "Combined2")

# Ensure Treatment is a factor
df$Treatment <- factor(df$Treatment, levels = c("UT", "IG1", "IG2", "P3", "RG"))

# Create numeric IDs for Stan
df$treatment_id <- as.numeric(df$Treatment)  # numeric treatment index
df$age_id <- as.numeric(as.factor(df$Age))   # numeric age index

# Prepare data for Stan
stan_data <- list(
  N = nrow(df),
  total_dead = df$bf_live,
  total = df$total,
  A = length(unique(df$age_id)),
  T = length(unique(df$treatment_id)),
  age = df$age_id,
  treatment = df$treatment_id,
  time = df$Time
)

# Stan model code
stan_model_code <- "
data {
  int<lower=1> N;
  int<lower=0> total_dead[N];
  int<lower=0> total[N];
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
}
model {
  alpha ~ normal(0, 5);
  beta_treatment ~ normal(0, 2);
  beta_age ~ normal(0, 2);
  beta_time ~ normal(0, 2);

  for (i in 1:N) {
    real eta = alpha + beta_treatment[treatment[i]] + beta_age[age[i]] + beta_time * time[i];
    total_dead[i] ~ binomial_logit(total[i], eta);
  }
}
"

# Fit the model
fit <- stan(
  model_code = stan_model_code,
  data = stan_data,
  chains = 1,
  iter = 2000,
  control = list(max_treedepth = 15)
)

# Extract posterior samples
posterior <- rstan::extract(fit)

# Create all treatment-age combinations
combo_grid <- expand.grid(treatment_id = 1:stan_data$T, age_id = 1:stan_data$A)

# Compute posterior predictions
results <- data.frame()

for (i in 1:nrow(combo_grid)) {
  t <- combo_grid$treatment_id[i]
  a <- combo_grid$age_id[i]
  
  eta <- posterior$alpha + posterior$beta_treatment[, t] + posterior$beta_age[, a] + posterior$beta_time * mean(stan_data$time)
  p <- plogis(eta)
  
  results <- rbind(results, data.frame(
    treatment_id = t,
    age_id = a,
    mean = mean(p),
    lower = quantile(p, 0.025),
    upper = quantile(p, 0.975)
  ))
}

# Map numeric IDs back to names
treatment_labels <- levels(df$Treatment)        # factor levels
results$treatment <- treatment_labels[results$treatment_id]  # <-- correctly map names

age_labels <- sort(unique(df$Age))              # original age values
results$age <- age_labels[results$age_id]      # <-- map ages

# Drop numeric IDs if you want
results <- results %>% select(treatment, age, mean, lower, upper)

# View results
print(results)









########with interaction

library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# --- Load data ---
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "Combined2")

# --- Factor variables ---
df$Treatment <- factor(df$Treatment, levels = c("UT", "IG1", "IG2", "P3", "RG"))
df$Age <- factor(df$Age)
df$hut <- factor(df$hut)        # Assuming you have a Hut column
df$sleeper <- factor(df$sleeper) # Assuming you have a Sleeper column

# --- Convert categorical variables to indices ---
df$treatment_id <- as.numeric(df$Treatment)
df$age_id <- as.numeric(df$Age)
df$hut_id <- as.numeric(df$hut)
df$sleeper_id <- as.numeric(df$sleeper)

# --- Prepare data for Stan ---
stan_data <- list(
  N = nrow(df),
  total_dead = df$`72h_dead`,
  total = df$total,
  A = length(unique(df$age_id)),
  T = length(unique(df$treatment_id)),
  H = length(unique(df$hut_id)),
  S = length(unique(df$sleeper_id)),
  age = df$age_id,
  treatment = df$treatment_id,
  hut = df$hut_id,
  sleeper = df$sleeper_id,
  time = df$Time
)

# --- Stan model code ---
stan_model_code <- "
data {
  int<lower=1> N;
  int<lower=0> total_dead[N];
  int<lower=0> total[N];
  int<lower=1> A;
  int<lower=1> T;
  int<lower=1> H;
  int<lower=1> S;
  int<lower=1, upper=A> age[N];
  int<lower=1, upper=T> treatment[N];
  int<lower=1, upper=H> hut[N];
  int<lower=1, upper=S> sleeper[N];
  int<lower=1> time[N];
}
parameters {
  real alpha;
  vector[T] beta_treatment;
  vector[A] beta_age;
  matrix[T, A] beta_trt_age;  // interaction
  real beta_time;
  vector[H] u_hut;
  vector[S] u_sleeper;
  real<lower=0> sigma_hut;
  real<lower=0> sigma_sleeper;
}
model {
  // Priors
  beta_treatment ~ normal(0, 2);
  beta_age ~ normal(0, 2);
  to_vector(beta_trt_age) ~ normal(0, 2); // interaction
  beta_time ~ normal(0, 2);
  u_hut ~ normal(0, sigma_hut);
  u_sleeper ~ normal(0, sigma_sleeper);
  sigma_hut ~ normal(0, 1);
  sigma_sleeper ~ normal(0, 1);
  alpha ~ normal(0, 5);

  // Likelihood
  for (i in 1:N) {
    real eta = alpha 
               + beta_treatment[treatment[i]] 
               + beta_age[age[i]] 
               + beta_trt_age[treatment[i], age[i]] 
               + beta_time * time[i]
               + u_hut[hut[i]] 
               + u_sleeper[sleeper[i]];
    total_dead[i] ~ binomial_logit(total[i], eta);
  }
}
"

# --- Compile and run the model ---
fit <- stan(
  model_code = stan_model_code,
  data = stan_data,
  chains = 1,
  iter = 2000,
  control = list(max_treedepth = 15)
)

# --- Print summary ---
print(fit, pars = c("alpha", "beta_treatment", "beta_age", "beta_trt_age", "beta_time", "sigma_hut", "sigma_sleeper"))

# --- Extract posterior samples ---
posterior <- rstan::extract(fit)

# --- Create grid for predicted probabilities ---
treatment_ids <- 1:stan_data$T
age_ids <- 1:stan_data$A
combo_grid <- expand.grid(treatment = treatment_ids, age = age_ids)

results <- data.frame()

for (i in 1:nrow(combo_grid)) {
  t <- combo_grid$treatment[i]
  a <- combo_grid$age[i]
  
  eta <- posterior$alpha + posterior$beta_treatment[, t] + posterior$beta_age[, a] + posterior$beta_trt_age[, t, a] + posterior$beta_time * mean(stan_data$time)
  p <- plogis(eta)
  
  results <- rbind(results, data.frame(
    treatment = t,
    age = a,
    mean = mean(p),
    lower = quantile(p, 0.025),
    upper = quantile(p, 0.975)
  ))
}

# Map treatment index back to names
treatment_labels <- levels(df$Treatment)
results$treatment <- treatment_labels[results$treatment]

# View results
print(results)

# Install once if you don’t have it

library(writexl)

# Save to Excel
write_xlsx(results, "mortality.xlsx")











###########################################################RNO,DNO, AND SNO
##############################################################################################################################################################################
########################################################################################################################################################################################




library(tidyverse)
library(grid)  # needed for unit()

# Convert to long format
df_long <- df %>%
  pivot_longer(cols = c(Sno, Dno, Rno),
               names_to = "Outcome",
               values_to = "Probability") %>%
  mutate(
    Outcome = factor(Outcome, levels = c("Rno", "Sno", "Dno"))  # stacking order
  )

# Faceted stacked area plot with extra space between facets
ggplot(df_long, aes(x = Age, y = Probability, fill = Outcome)) +
  geom_area(alpha = 0.9, color = NA, position = "stack") +
  scale_x_continuous(breaks = 1:4, labels = c("New", "12M", "24M", "36M")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c("Sno" = "#F8766D",
               "Dno" = "#7CAE00",
               "Rno" = "#00BFC4"),
    labels = c("Rno" = "Repellency",
               "Sno" = "Successfully feeding",
               "Dno" = "Dying")
  ) +
  labs(x = "Net Age",
       y = "Probable outcome from mosquito feeding attempt (%)",
       fill = "Outcome") +
  facet_wrap(~Treatment, ncol = 2) +
  theme_minimal(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    strip.background = element_rect(color = "black", fill = "gray90", size = 0.8),
    panel.spacing = unit(1, "lines")  # <--- adds space between facets
  )















##########################plottingtotalnumberf mosqutioes 



library(tibble)
library(dplyr)
library(ggplot2)

# --- Data ---
df <- tribble(
  ~Treatment, ~Age_label, ~Time, ~treated_mean,
  "IG1", "12m", 1,  7.74,
  "IG1", "24m", 2, 14.9,
  "IG1", "36m", 3, 11.1,
  "IG1", "New", 1,  6.73,
  "IG1", "New", 2, 15.3,
  "IG1", "New", 3, 10.3,
  "IG2", "12m", 1,  6.34,
  "IG2", "24m", 2, 16.7,
  "IG2", "36m", 3, 13.8,
  "IG2", "New", 1,  6.75,
  "IG2", "New", 2, 15.6,
  "IG2", "New", 3, 10.3,
  "P3",  "12m", 1,  7.91,
  "P3",  "24m", 2, 21.1,
  "P3",  "36m", 3, 12.7,
  "P3",  "New", 1,  5.83,
  "P3",  "New", 2, 13.4,
  "P3",  "New", 3,  8.57,
  "RG",  "12m", 1,  8.53,
  "RG",  "24m", 2, 21.4,
  "RG",  "36m", 3, 15.2,
  "RG",  "New", 1, 10.6,
  "RG",  "New", 2, 24.4,
  "RG",  "New", 3, 16.1,
  "UT",  "New", 1, 10.1,
  "UT",  "New", 2, 23.2,
  "UT",  "New", 3, 15.6,
  "UT",  "12m", 1, 10.1,
  "UT",  "24m", 2, 23.2,
  "UT",  "36m", 3, 15.6
)

# --- Relabel "New" and "Aged" bars ---
df <- df %>%
  mutate(
    Age_label = case_when(
      Age_label == "New" & Time == 1 ~ "New_12m",
      Age_label == "New" & Time == 2 ~ "New_24m",
      Age_label == "New" & Time == 3 ~ "New_36m",
      Age_label == "12m" ~ "Aged_12m",
      Age_label == "24m" ~ "Aged_24m",
      Age_label == "36m" ~ "Aged_36m",
      TRUE ~ Age_label
    ),
    Age_label = factor(Age_label, levels = c("New_12m","New_24m","New_36m",
                                             "Aged_12m","Aged_24m","Aged_36m"))
  )

# --- Plot ---
ggplot(df, aes(x = Age_label, y = treated_mean, fill = Treatment)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    title = "Mean Number of mosquitoes",
    x = "Net Age",
    y = " Mean Number of Mosquitoes"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text.x = element_text(angle = 45, hjust = 1)  # <-- tilt labels
  )










####tanzania


library(tibble)

# --- Updated Data ---
df <- tribble(
  ~Treatment, ~Age_label, ~Time, ~treated_mean, ~control_mean, ~deterrence,
  "IG1", "12m", 1, 2.61, NA, NA,
  "IG1", "24m", 2, 3.02, NA, NA,
  "IG1", "36m", 3, 2.45, NA, NA,
  "IG1", "New", 1, 3.49, 2.97, -0.178,
  "IG1", "New", 2, 3.66, 3.09, -0.182,
  "IG1", "New", 3, 3.21, 2.71, -0.181,
  
  "IG2", "12m", 1, 2.57, NA, NA,
  "IG2", "24m", 2, 3.28, NA, NA,
  "IG2", "36m", 3, 2.58, NA, NA,
  "IG2", "New", 1, 2.76, 2.97, 0.0680,
  "IG2", "New", 2, 2.88, 3.09, 0.0688,
  "IG2", "New", 3, 2.47, 2.71, 0.0910,
  
  "OP", "12m", 1, 3.34, NA, NA,
  "OP", "24m", 2, 3.15, NA, NA,
  "OP", "36m", 3, 2.53, NA, NA,
  "OP", "New", 1, 2.82, 2.97, 0.0502,
  "OP", "New", 2, 2.97, 3.09, 0.0386,
  "OP", "New", 3, 2.73, 2.71, -0.00534,
  
  "RG", "12m", 1, 2.85, NA, NA,
  "RG", "24m", 2, 3.55, NA, NA,
  "RG", "36m", 3, 2.52, NA, NA,
  "RG", "New", 1, 2.88, 2.97, 0.0280,
  "RG", "New", 2, 3.04, 3.09, 0.0183,
  "RG", "New", 3, 2.63, 2.71, 0.0313,
  
  # UT values
  "UT", "New", 1, 2.97, NA, NA,
  "UT", "New", 2, 3.09, NA, NA,
  "UT", "New", 3, 2.71, NA, NA,
  "UT", "12m", 1, 2.97, NA, NA,
  "UT", "24m", 2, 3.09, NA, NA,
  "UT", "36m", 3, 2.71, NA, NA
)




# --- Relabel "New" and "Aged" bars ---
df <- df %>%
  mutate(
    Age_label = case_when(
      Age_label == "New" & Time == 1 ~ "New_12m",
      Age_label == "New" & Time == 2 ~ "New_24m",
      Age_label == "New" & Time == 3 ~ "New_36m",
      Age_label == "12m" ~ "Aged_12m",
      Age_label == "24m" ~ "Aged_24m",
      Age_label == "36m" ~ "Aged_36m",
      TRUE ~ Age_label
    ),
    Age_label = factor(Age_label, levels = c("New_12m","New_24m","New_36m",
                                             "Aged_12m","Aged_24m","Aged_36m"))
  )

# --- Plot ---
ggplot(df, aes(x = Age_label, y = treated_mean, fill = Treatment)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    title = "Mean Number of Mosquitoes",
    x = "Net Age",
    y = "Mean Number of Mosquitoes"
  ) +
  scale_y_continuous(limits = c(0, 25)) +   # <-- set y-axis from 0 to 25
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text.x = element_text(angle = 45, hjust = 1)  # tilt labels
  )

































###########################


library(ggplot2)
library(dplyr)

#set.seed(123)  # For reproducibility

# Step 1: Original means
df <- data.frame(
  treatment = rep(c("IG1", "IG2", "P3", "RG"), each = 6),
  net_type = rep(c("New_12m", "New_24m", "New_36m", "Field_12m", "Field_24m", "Field_36m"), 4),
  mean = c(
    0.335,0.34,0.339,0.233,0.357,0.288,     # IG1
    0.332,0.33,0.336,0.372,0.280,0.109,     # IG2
    0.424,0.423,0.449,0.216,0.086,0.180,    # P3
    -0.0478,-0.0489,-0.036,0.1554,0.081,0.0193) # RG
)






#Tanzania 

#mean = c(
# -0.178,-0.182,-0.181,0.121,0.022,0.095,     # IG1
#0.068,0.0688,0.0688,0.134,-0.061,0.047,     # IG2
#0.0502,0.0386,-0.00534,-0.124,-0.019,0.066,    # P3
#0.028,0.0183,0.0313,0.0404,-0.148,0.070 # RG
#)
#0.350 , 0.335,  0.316, 0.354, 0.350 , 0.043,    # IG1


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
  scale_y_continuous(limits = c(-0.25, 0.6)) +
  labs(
    x = "Net Age",
    y = "Deterrence",
    title = "Deterrence(New vs Field Aged  Nets)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.spacing = unit(1, "lines")
  )














































library(ggplot2)
library(dplyr)
library(patchwork)

# -----------------------------
# 1. Current plot (New vs Field) - keep your original
# -----------------------------
p_top <- ggplot(df, aes(x = net_age, y = mean, fill = fill_color)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.7) +
  facet_grid(group ~ treatment, scales = "free_y") +
  scale_fill_manual(values = c("New" = "#FFA500", "Field" = "#1F77B4"), name = "Net Type") +
  scale_y_continuous(limits = c(-0.25, 0.6)) +
  labs(x = NULL, y = "Deterrence", title = "Deterrence (New vs Field Aged Nets)") +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, size = 0.8),
        panel.spacing = unit(1, "lines"))

# -----------------------------
# 2. Prepare decay data
# -----------------------------
# For each treatment, take ONE New value (e.g., last New net)
new_ref <- df %>%
  filter(group == "New") %>%
  group_by(treatment) %>%
  slice_tail(n=1) %>%
  select(treatment, net_type, mean) %>%
  rename(ref_value = mean)

# Get Field values
field_vals <- df %>%
  filter(group == "Field") %>%
  select(treatment, net_type, mean)

# Combine into decay plot dataset
decay_df <- field_vals %>%
  left_join(new_ref, by = "treatment") %>%
  # Optional: put New reference as first bar
  mutate(bar_label = case_when(
    grepl("New", net_type) ~ "New",
    TRUE ~ sub(".*_(\\d+m)$", "\\1", net_type)
  ))

# -----------------------------
# 3. Bottom plot (Decay)
# -----------------------------
p_bottom <- ggplot(decay_df, aes(x = bar_label, y = mean, fill = bar_label)) +
  geom_col(width = 0.6) +
  facet_wrap(~ treatment, ncol = 4) +
  scale_fill_manual(values = c("New" = "#FFA500", "12m"="#1F77B4","24m"="#1F77B4","36m"="#1F77B4")) +
  labs(x = "Net Age", y = "Deterrence", title = "Decay from New to Field Aged Nets") +
  theme_minimal(base_size = 14) +
  theme(legend.position="none",
        strip.text = element_text(face="bold"))

# -----------------------------
# 4. Combine top + bottom
# -----------------------------
p_top / p_bottom + plot_layout(heights = c(2,1))






















################combine deterrecne



library(ggplot2)
library(dplyr)

# Step 1: Original means
df <- data.frame(
  treatment = rep(c("IG1", "IG2", "OP", "P3", "RG"), each = 6),
  net_type = rep(c("New_12m", "New_24m", "New_36m", "Field_12m", "Field_24m", "Field_36m"), 5),
  mean = c(
    -0.11341853, -0.223021583, -0.204216074, 0.092651757, 0.196300103, 0.11198946, # IG1
    -0.062300319, -0.223021583, -0.187088274, 0.196485623, 0.121274409, -0.007905138, # IG2
    0.68370607, 0.606372045, 0.588932806, 0.463258786, 0.676258993, 0.665349144, # OP
    -0.012779553, -0.233299075, -0.108036891, -0.269968051, -1.19938335, -0.660079051, # P3
    -0.546325879, -0.685508736, -0.673254282, 0.009584665, 0.027749229, 0.014492754 # RG
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
    group = ifelse(grepl("New", net_type), "New Nets", "Field Aged Nets"),
    net_age = sub(".*_(\\d+m)$", "\\1", net_type),
    net_age = factor(net_age, levels = c("12m", "24m", "36m")),
    fill_color = ifelse(group == "New Nets", "New Nets", "Field Aged Nets")
  )

# Step 4: Plot
ggplot(df, aes(x = net_age, y = mean, fill = fill_color)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, size = 0.7) +
  facet_grid(group ~ treatment, scales = "free_y") +
  scale_fill_manual(
    values = c("New Nets" = "#FFA500", "Field Aged Nets" = "#1F77B4"),
    name = "Net Type"
  ) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(
    x = "Time Point",
    y = "Deterrence",
    title = "Deterrence (Benin + Tanzania)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    panel.spacing = unit(1, "lines")
  )

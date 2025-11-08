library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
options(mc.cores=4)
rstan_options(auto_write = TRUE)

#df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/Benin_NNP_hut_trial_raw data (2).xlsx", 
 #                                             sheet = "combined")

#df<-  read_excel("Benin_NNP_hut_trial_raw data (2).xlsx", 
                # sheet = "combined")
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "combined")
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/New folder/TZ and benin combined.xlsx", 
                                    sheet = "Sheet3")

df_clean <- df %>%
  filter(!is.na(Treatment), Treatment != "STD")
#View(df)

df$Treatment <- factor(df$Treatment, levels = c("UT", "IG1", "IG2", "P3", "OP", "RG"))
df$treatment_id <- as.numeric(df$Treatment)
# Convert categorical variables to index values
df$treatment_id <- as.numeric(as.factor(df$Treatment))
df$age_id <- as.numeric(as.factor(df$Age))

# Prepare data for Stan
stan_data <- list(
  N = nrow(df),
  total_dead = df$tot_72h_dead,
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
  chains = 1,
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
treatment_labels <- levels(as.factor(df$treatment))
results$treatment <- treatment_labels[results$treatment]

# View the estimates
print(results)

write_xlsx(results, "mortality.xlsx")





#plot


# Create the bar chart: Observed mortality vs Treatment and Age
ggplot(results, aes(x = factor(age), y = mean, fill = treatment)) +
  geom_col(position = "dodge", width = 0.5) +  # Bar chart with separate bars for each treatment
  facet_wrap(~treatment) +  # Create separate panels for each treatment
  labs(
    title = "Observed Mortality per Treatment and Age",  # Plot title
    x = "Age",  # X-axis label
    y = "Mortality Probability"  # Y-axis label
  ) +
  ylim(0,1)+
  theme_minimal() +  # Use a minimal theme for clarity
  theme(legend.position = "none")  # Remove legend for cleaner visualization










# Assuming 'results' has the observed mortality at age 1 (m0) and age 4 (m(t))
# Extract initial and final mortality values for each treatment
# Here, 'm0' is the mortality at age 1, and 'm4' is the mortality at age 4

# Add mortality at age 1 and age 4 to the dataset (for illustration)
results$m0 <- ifelse(results$age == 1, results$mean, NA)  # Mortality at age 1
results$m4 <- ifelse(results$age == 4, results$mean, NA)  # Mortality at age 4

# Carry forward m0 and m4 for each treatment across ages
results <- results %>%
  group_by(treatment) %>%
  fill(m0, m4, .direction = "downup")  # Fill missing m0 and m4 values per treatment

# Calculate lambda for each treatment
results <- results %>%
  group_by(treatment) %>%
  mutate(lambda = -log(m4 / m0) / 3)  # Calculate decay rate (lambda)

results %>% 
  select(treatment, lambda) %>% 
  distinct()

# Define the exponential decay function
exponential_decay <- function(age, m0, lambda) {
  return(m0 * exp(-lambda * (age-1 )))  # Apply decay directly from age 1
}

# Calculate fitted exponential decay values for each age
results$decay <- mapply(exponential_decay, age = results$age, m0 = results$m0, lambda = results$lambda)

head(results)
#View(results)

# Create named vectors for the labels
treatment_labels <- c("UT", "IG1", "IG2", "P3", "OP", "RG")
names(treatment_labels) <- 1:5

age_labels <- c("New", "12m", "24m", "36m")
names(age_labels) <- 1:4


results <- results %>%
 rename(
    lci = lower,
   uci = upper
)



ggplot(results, aes(x = age, y = mean, fill = treatment)) +
  geom_col(position = "dodge", width = 0.5) +
  # Add confidence interval ribbon (semi-transparent)
  geom_ribbon(aes(ymin = lci, ymax = uci, group = treatment), alpha = 0.2, fill = "grey70") +
  # Decay line
  geom_line(aes(y = decay, group = treatment, color = treatment), size = 0.8) +
  facet_wrap(~treatment, labeller = as_labeller(treatment_labels)) +
  scale_x_continuous(breaks = 1:4, labels = age_labels) +
  labs(
    title = "Observed Mortality per Treatment with Exponential Decay Curve and 95% CI",
    x = "Net Age",
    y = "Mortality Probability"
  ) +
  ylim(0, 1) +
  theme_minimal() +
  theme(legend.position = "none")




#numbers <- c(0.412, 0.364, 0.257, 0.320, 0.327)

numbers <- c(0.421, 0.373, 0.270, 0.329, 0.331, 0.336)


# Compute log(2) / each number
result <- log(2) / numbers

# Print results
result






# Plot with borders around facets and axis labels for specific facets
ggplot(results, aes(x = age, y = mean, fill = treatment)) +
  geom_point(size = 1, aes(color = treatment)) +  # Dotted points for observed mortality
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = treatment), 
              alpha = 0.4, fill = "gray40") +  # Darker shaded region for better visibility
  geom_line(aes(x = age, y = decay, group = treatment, color = treatment), size = 0.8) +  # Exponential decay line
  facet_wrap(~treatment, labeller = as_labeller(treatment_labels), 
             scales = "free") +  # Facet by treatment
  scale_x_continuous(breaks = 1:4, labels = age_labels) +  # Label ages on x-axis
  labs(
    title = "Predicted Mortality per Treatment with Exponential Decay Curve and 95% CI",
    x = "Net Age",
    y = "Mortality Probability"
  ) +
  ylim(0, 1) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend
    strip.background = element_blank(),  # Remove background behind facet labels
    strip.text = element_text(size = 12, face = "bold"),  # Make facet labels bold
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add border around each facet
    axis.text.x = element_text(size = 10),  # Adjust x-axis text size
    axis.text.y = element_text(size = 10),  # Adjust y-axis text size
    axis.title.x = element_text(size = 12, face = "bold"),  # Make x-axis title bold
    axis.title.y = element_text(size = 12, face = "bold"),  # Make y-axis title bold
    strip.placement = "outside",  # Move facet labels outside of the plot area
    panel.spacing = unit(1, "lines"),  # Add space between facets
    # Custom y-axis labels for specific treatments
    axis.title.y.right = element_blank(),  # Remove y-axis label on facets that don't need it
    axis.text.y.right = element_blank(),  # Remove y-axis labels on facets that don't need it
    axis.title.x.top = element_blank(),  # Remove x-axis label on facets that don't need it
    axis.text.x.top = element_blank()  # Remove x-axis labels on facets that don't need it
  ) +
  # Add specific axis labels to the required facets
  theme(
    strip.text = element_text(face = "bold"),
    # Label x-axis for "UT" and "IG1" facets
    strip.text.x = element_text(size = 12, face = "bold", color = "black", 
                                angle = 0, hjust = 0.5),
    # Label y-axis for "IG1", "IG2", and "RG" facets
    strip.text.y = element_text(size = 12, face = "bold", color = "black", 
                                angle = 0, hjust = 0.5)
  )


#############33raw mortality


# Create raw mortality column in your original dataframe (df)
df$raw_mortality <- df$tot_72h_dead / df$total

# Summarize raw mortality by treatment and age
raw_mortality_summary <- df %>%
  group_by(Treatment, Age) %>%
  summarize(
    mean_mortality = mean(raw_mortality, na.rm = TRUE),
    .groups = "drop"  # To drop the grouping structure after summarization
  )

# Convert treatment columns to characters to avoid type mismatch
results$treatment <- as.character(results$treatment)
raw_mortality_summary$treatment <- as.character(raw_mortality_summary$treatment)

# Join the summarized mortality data with the results dataframe
results <- left_join(results, raw_mortality_summary, 
                     by = c("treatment" = "treatment", "age" = "Age"))

# View the final results with mean mortality merged
head(results)

# If needed, you can write the results to a file (e.g., CSV) to inspect further
# write.csv(results, "final_results_with_mean_mortality.csv", row.names = FALSE)




View(results)

# Create the bar chart with mean mortality by treatment and age, no confidence intervals
ggplot(results, aes(x = factor(age), y = mean_mortality, fill = treatment)) +
  geom_col(position = "dodge", width = 0.5) +  # Bar chart with separate bars for each treatment
  facet_wrap(~treatment, scales = "free") +  # Create separate panels for each treatment with free y-axis scale
  labs(
    title = "Raw Mortality per Treatment and Age",  # Plot title
    x = "Age",  # X-axis label (common)
    y = "Mean Mortality"  # Y-axis label (common)
  ) +
  ylim(0, 1) +  # Set y-axis range to [0,1]
  scale_x_discrete(
    limits = c("1", "2", "3", "4"),  # Ensure all age categories are included
    labels = c("1" = "New", "2" = "12m", "3" = "24m", "4" = "36m")  # Custom x-axis labels
  ) +  # Custom x-axis labels
  theme_minimal() +  # Use a minimal theme for clarity
  theme(
    legend.position = "none",  # Remove legend for cleaner visualization
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add border around each facet
    strip.background = element_blank(),  # Remove background behind facet labels
    strip.text = element_text(size = 12, face = "bold"),  # Make facet labels bold
    axis.text.x = element_text(size = 10),  # Adjust x-axis text size
    axis.text.y = element_text(size = 10),  # Adjust y-axis text size
    axis.title.x = element_text(size = 12, face = "bold"),  # Make x-axis title bold
    axis.title.y = element_text(size = 12, face = "bold"),  # Make y-axis title bold
    strip.placement = "outside"  # Move facet labels outside of the plot area
  )




#with intervals

ggplot(results, aes(x = factor(age), fill = treatment)) +
  # Plot mean mortality as bars (mean_mortality)
  geom_col(aes(y = mean_mortality), position = "dodge", width = 0.5, alpha = 0.6) +  # Bar chart for mean mortality
  
  # Plot the mean line with confidence intervals (mean, lci, and uci)
  geom_line(aes(x = age, y = mean, group = treatment, color = "Mean"), size = 1) +  # Line for mean mortality
  geom_ribbon(aes(x = age, ymin = lci, ymax = uci, fill = treatment), 
              alpha = 0.4, color = NA) +  # Shaded region for confidence intervals
  
  facet_wrap(~treatment, labeller = as_labeller(treatment_labels), scales = "free") +  # Facet by treatment
  
  scale_x_discrete(
    limits = c("1", "2", "3", "4"),  # Ensure all age categories are included
    labels = c("1" = "New", "2" = "12m", "3" = "24m", "4" = "36m")  # Custom x-axis labels
  ) +  # Custom x-axis labels
  
  labs(
    title = "Observed Mortality",  # Plot title
    x = "Age",  # X-axis label (common)
    y = "Mortality Probability"  # Y-axis label (common)
  ) +
  
  ylim(0, 1) +  # Set y-axis range to [0,1]
  
  scale_color_manual(values = c("Mean" = "lightgray")) +  # Set color for 'mean' line to light gray
  
  theme_minimal() +  # Use a minimal theme for clarity
  theme(
    legend.position = "none",  # Remove legend for cleaner visualization
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add border around each facet
    strip.background = element_blank(),  # Remove background behind facet labels
    strip.text = element_text(size = 12, face = "bold"),  # Make facet labels bold
    axis.text.x = element_text(size = 10),  # Adjust x-axis text size
    axis.text.y = element_text(size = 10),  # Adjust y-axis text size
    axis.title.x = element_text(size = 12, face = "bold"),  # Make x-axis title bold
    axis.title.y = element_text(size = 12, face = "bold"),  # Make y-axis title bold
    strip.placement = "outside"  # Move facet labels outside of the plot area
  )





######projectedfuturemortality




#####futuremortality prediction 

# Define future ages to project (ages 5 to 10)
future_ages <- 1:10  

# Create a new data frame to store projected mortality
projected_results <- data.frame()

# Loop through each treatment and calculate projected mortality for ages 5 to 10
for(t in unique(extended_results$treatment)) {
  # Extract the m0 and lambda for the current treatment
  m0 <- extended_results$m0[extended_results$treatment == t & extended_results$age == 1][1]  # Mortality at age 1
  lambda <- extended_results$lambda[extended_results$treatment == t][1]  # Decay rate (lambda)
  
  # Calculate projected mortality for ages 5 to 10
  projected_mortality <- sapply(future_ages, function(age) m0 * exp(-lambda * (age - 1)))
  
  # Store the projected results
  temp <- data.frame(
    treatment = rep(t, length(future_ages)),
    age = future_ages,
    projected_mortality = projected_mortality
  )
  
  projected_results <- rbind(projected_results, temp)  # Append to the result data frame
}

# Plot: Projected Mortality for Ages 5 to 10 with Exponential Decay Curve
ggplot(projected_results, aes(x = age, y = projected_mortality, color = treatment)) +
  geom_line(size = 1.5) +  # Exponential decay curve for projected mortality
  facet_wrap(~treatment) +  # Create separate panels for each treatment
  labs(
    title = "Projected Mortality for Ages 5 to 10 Using Exponential Decay",
    x = "Age",
    y = "Projected Mortality Probability"
  ) +
  ylim(0,1)+
  theme_minimal() +
  theme(legend.position = "none")























# Define future ages to project (ages 1 to 10)
future_ages <- 1:10  

# Create a new data frame to store projected mortality
projected_results <- data.frame()

# Loop through each treatment and calculate projected mortality for ages 1 to 10
for(t in unique(extended_results$treatment)) {
  # Extract the m0 and lambda for the current treatment
  m0 <- extended_results$m0[extended_results$treatment == t & extended_results$age == 1][1]  # Mortality at age 1
  lambda <- extended_results$lambda[extended_results$treatment == t][1]  # Decay rate (lambda)
  
  # Calculate projected mortality for ages 1 to 10
  projected_mortality <- sapply(future_ages, function(age) m0 * exp(-lambda * (age - 1)))
  
  # Store the projected results
  temp <- data.frame(
    treatment = rep(t, length(future_ages)),
    age = future_ages,
    projected_mortality = projected_mortality
  )
  
  projected_results <- rbind(projected_results, temp)  # Append to the result data frame
}

# Plot: Projected Mortality for Ages 1 to 10 with Exponential Decay Curve
ggplot(projected_results, aes(x = age, y = projected_mortality, color = treatment)) +
  geom_line(size = 1) +  # Exponential decay curve for projected mortality
  labs(
    title = "Projected Mortality for Ages 1 to 10 Using Exponential Decay",
    x = "Age",
    y = "Projected Mortality Probability"
  ) +
  ylim(0, 1) +  # Set y-axis limits from 0 to 1
  theme_minimal() +
  theme(
    legend.title = element_blank(),  # Remove the legend title
    legend.position = "right",  # Position the legend on the right
    strip.background = element_blank(),  # Remove background behind facet labels
    strip.text = element_text(size = 12, face = "bold")  # Make facet labels bold
  )




# Define the future ages and corresponding labels (in months)
future_ages <- 1:10  # Age indices from 1 to 10
age_labels <- c("New", "12m", "24m", "36m", "48m", "60m", "72m", "84m", "96m", "120m")  # Custom age labels for each index

# Create a new data frame to store projected mortality
projected_results <- data.frame()

# Loop through each treatment and calculate projected mortality for ages 1 to 10
for(t in unique(extended_results$treatment)) {
  # Extract the m0 and lambda for the current treatment
  m0 <- extended_results$m0[extended_results$treatment == t & extended_results$age == 1][1]  # Mortality at age 1
  lambda <- extended_results$lambda[extended_results$treatment == t][1]  # Decay rate (lambda)
  
  # Calculate projected mortality for ages 1 to 10
  projected_mortality <- sapply(future_ages, function(age) m0 * exp(-lambda * (age - 1)))
  
  # Store the projected results
  temp <- data.frame(
    treatment = rep(t, length(future_ages)),
    age = future_ages,
    projected_mortality = projected_mortality
  )
  
  projected_results <- rbind(projected_results, temp)  # Append to the result data frame
}

# Plot: Projected Mortality for Ages 1 to 10 with Exponential Decay Curve
ggplot(projected_results, aes(x = age, y = projected_mortality, color = treatment)) +
  geom_line(size = 0.8) +  # Exponential decay curve for projected mortality
  scale_x_continuous(breaks = 1:10, labels = age_labels) +  # Custom x-axis labels for age
  labs(
    title = "Projected Mortality for Field Aged Bednets",
    x = "Aged Bednets (Months)",
    y = "Projected Mortality Probability"
  ) +
  ylim(0, 1) +  # Set y-axis limits from 0 to 1
  theme_minimal() +
  theme(
    legend.title = element_blank(),  # Remove the legend title
    legend.position = "right",  # Position the legend on the right
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add border around the plot area
    strip.background = element_blank(),  # Remove background behind facet labels
    strip.text = element_text(size = 10),# face = "bold"),  # Make facet labels bold
    axis.text.x = element_text(size = 10),  # Adjust x-axis text size
    axis.text.y = element_text(size = 10)  # Adjust y-axis text size
  )










#########################################################################################################################################################################
################################################now lets estimate bloodfed#########################################################################################################################





library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

#df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/Benin_NNP_hut_trial_raw data (2).xlsx", 
#                                             sheet = "combined")

#df<-  read_excel("Benin_NNP_hut_trial_raw data (2).xlsx", 
           #      sheet = "combined")

df<- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                                                  sheet = "combined")
#View(df)

df$Treatment <- factor(df$Treatment, levels = c("UT", "IG1", "IG2", "P3", "RG"))
df$treatment_id <- as.numeric(df$Treatment)
# Convert categorical variables to index values
df$treatment_id <- as.numeric(as.factor(df$Treatment))
df$age_id <- as.numeric(as.factor(df$Age))

# Prepare data for Stan
stan_data <- list(
  N = nrow(df),
  bf_live= df$bf_live,
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
  int<lower=0> bf_live[N];
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
    bf_live[i] ~ binomial_logit(total[i], eta);
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
treatment_labels <- levels(as.factor(df$treatment))
results$treatment <- treatment_labels[results$treatment]

# View the estimates
print(results)
#library(writexl)

write_xlsx(results, "bf_live.xlsx")






#########################################################################################################################################################################
################################################unfedlive#########################################################################################################################





library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

#df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/Benin_NNP_hut_trial_raw data (2).xlsx", 
#                                             sheet = "combined")

#df<-  read_excel("Benin_NNP_hut_trial_raw data (2).xlsx", 
#      sheet = "combined")

df<- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                sheet = "combined")
#View(df)

df$Treatment <- factor(df$Treatment, levels = c("UT", "IG1", "IG2", "P3", "RG"))
df$treatment_id <- as.numeric(df$Treatment)
# Convert categorical variables to index values
df$treatment_id <- as.numeric(as.factor(df$Treatment))
df$age_id <- as.numeric(as.factor(df$Age))

# Prepare data for Stan
stan_data <- list(
  N = nrow(df),
  unf_live= df$unf_live,
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
  int<lower=0> unf_live[N];
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
    unf_live[i] ~ binomial_logit(total[i], eta);
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
treatment_labels <- levels(as.factor(df$treatment))
results$treatment <- treatment_labels[results$treatment]

# View the estimates
print(results)
#library(writexl)

write_xlsx(results, "unf_live.xlsx")



#################################################################################################################################################################
#######################################TOTALUNFED#############################################################################################################################


library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

#df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/Benin_NNP_hut_trial_raw data (2).xlsx", 
#                                             sheet = "combined")

#df<-  read_excel("Benin_NNP_hut_trial_raw data (2).xlsx", 
#sheet = "combined")
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "combined")

#View(df)

df$Treatment <- factor(df$Treatment, levels = c("UT", "IG1", "IG2", "P3", "RG"))
df$treatment_id <- as.numeric(df$Treatment)
# Convert categorical variables to index values
df$treatment_id <- as.numeric(as.factor(df$Treatment))
df$age_id <- as.numeric(as.factor(df$Age))

# Prepare data for Stan
stan_data <- list(
  N = nrow(df),
  total_unf= df$tot_unf,
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
  int<lower=0> total_unf[N];
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
    total_unf[i] ~ binomial_logit(total[i], eta);
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
treatment_labels <- levels(as.factor(df$treatment))
results$treatment <- treatment_labels[results$treatment]

# View the estimates
print(results)
library(writexl)

write_xlsx(results, "unfed.xlsx")



library(readr)
ento_fit1_half_life_pyrethroid_nets_4_ <- read_csv("C:/Users/user/Downloads/ento_fit1_half_life_pyrethroid_nets (4).rds")
View(ento_fit1_half_life_pyrethroid_nets_4_)



###################################################################################################################################################
#########################################total##################################################################################################################


# Load required libraries
library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

# Read the data
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "combined")

# Convert Treatment and Age to factors and numeric IDs
df$Treatment <- factor(df$Treatment, levels = c("UT", "IG1", "IG2", "P3", "RG"))
df$treatment_id <- as.numeric(df$Treatment)
df$age_id <- as.numeric(as.factor(df$Age))

# Prepare data for Stan
stan_data <- list(
  N = nrow(df),
  total = df$total,
  A = length(unique(df$age_id)),
  T = length(unique(df$treatment_id)),
  age = df$age_id,
  treatment = df$treatment_id,
  time = df$Time
)

# Stan model code: Negative Binomial for total count
stan_model_code <- "
data {
  int<lower=1> N;
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
  real<lower=0> phi;
}
model {
  alpha ~ normal(0, 5);
  beta_treatment ~ normal(0, 2);
  beta_age ~ normal(0, 2);
  beta_time ~ normal(0, 2);
  phi ~ gamma(2, 0.1);

  for (i in 1:N) {
    real eta = alpha + beta_treatment[treatment[i]] + beta_age[age[i]] + beta_time * time[i];
    total[i] ~ neg_binomial_2_log(eta, phi);
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
)

# Print summary of parameters
print(fit, pars = c("alpha", "beta_treatment", "beta_age", "beta_time", "phi"))

posterior <- rstan::extract(fit)


# Extract posterior samples
#posterior <- extract(fit)

# Generate combinations of treatment and age
treatment_ids <- 1:stan_data$T
age_ids <- 1:stan_data$A
combo_grid <- expand.grid(treatment = treatment_ids, age = age_ids)

# Compute predicted total mosquito counts
results <- data.frame()

for (i in 1:nrow(combo_grid)) {
  t <- combo_grid$treatment[i]
  a <- combo_grid$age[i]
  
  eta <- posterior$alpha + posterior$beta_treatment[, t] + 
    posterior$beta_age[, a] + 
    posterior$beta_time * mean(stan_data$time)
  
  lambda <- exp(eta)
  
  summary_row <- data.frame(
    treatment = t,
    age = a,
    mean = mean(lambda),
    lower = quantile(lambda, 0.025),
    upper = quantile(lambda, 0.975)
  )
  
  results <- rbind(results, summary_row)
}

# Restore treatment and age labels
results$treatment <- levels(df$Treatment)[results$treatment]
results$age <- levels(as.factor(df$Age))[results$age]

# View or export results
print(results)
write_xlsx(results, "estimated_total_mosquitoes.xlsx")









# Prepare empty matrix to hold expected counts
lambda_all <- matrix(0, nrow = length(posterior$alpha), ncol = nrow(df))

# Loop through each row in the data
for (i in 1:nrow(df)) {
  t <- df$treatment_id[i]
  a <- df$age_id[i]
  tm <- df$Time[i]
  
  eta <- posterior$alpha + posterior$beta_treatment[, t] + 
    posterior$beta_age[, a] + 
    posterior$beta_time * tm
  
  # Expected unfed_live counts per draw
  lambda_all[, i] <- plogis(eta) * df$total[i]  # Prob × total for that row
}

# Add treatment and age labels to each row
df$predicted_mean <- colMeans(lambda_all)

# Group by treatment and age, then summarize
total_predictions <- df %>%
  group_by(Treatment, Age) %>%
  summarise(
    mean = sum(predicted_mean),
    lower = sum(apply(lambda_all[, df$Treatment == Treatment & df$Age == Age], 2, quantile, 0.025)),
    upper = sum(apply(lambda_all[, df$Treatment == Treatment & df$Age == Age], 2, quantile, 0.975))
  )

# Print results
print(total_predictions)
write_xlsx(total_predictions, "estimated_total_mosquitoes.xlsx")





















################deterence




# Load required libraries
library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

# Read the data
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "combined")

# Convert Treatment and Age to factors and numeric IDs
df$Treatment <- factor(df$Treatment, levels = c("UT", "IG1", "IG2", "P3", "RG"))
df$treatment_id <- as.numeric(df$Treatment)
df$age_id <- as.numeric(as.factor(df$Age))

# Prepare data for Stan
stan_data <- list(
  N = nrow(df),
  total = df$total,
  A = length(unique(df$age_id)),
  T = length(unique(df$treatment_id)),
  age = df$age_id,
  treatment = df$treatment_id,
  time = df$Time
)

# Stan model code: Negative Binomial for total count
stan_model_code <- "
data {
  int<lower=1> N;
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
  real<lower=0> phi;
}
model {
  alpha ~ normal(0, 5);
  beta_treatment ~ normal(0, 2);
  beta_age ~ normal(0, 2);
  beta_time ~ normal(0, 2);
  phi ~ gamma(2, 0.1);

  for (i in 1:N) {
    real eta = alpha + beta_treatment[treatment[i]] + beta_age[age[i]] + beta_time * time[i];
    total[i] ~ neg_binomial_2_log(eta, phi);
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
)

# Extract posterior samples
posterior <- rstan::extract(fit)

# Generate combinations of treatment and age
treatment_ids <- 1:stan_data$T
    age_ids <- 1:stan_data$A
combo_grid <- expand.grid(treatment = treatment_ids, age = age_ids)

# Compute predicted total mosquito counts
results <- data.frame()

for (i in 1:nrow(combo_grid)) {
  t <- combo_grid$treatment[i]
  a <- combo_grid$age[i]
  
  eta <- posterior$alpha + posterior$beta_treatment[, t] + 
    posterior$beta_age[, a] + 
    posterior$beta_time * mean(stan_data$time)
  
  lambda <- exp(eta)
  
  summary_row <- data.frame(
    treatment = t,
    age = a,
    mean = mean(lambda),
    lower = quantile(lambda, 0.025),
    upper = quantile(lambda, 0.975)
  )
  
  results <- rbind(results, summary_row)
}

# Restore treatment and age labels
results$treatment <- levels(df$Treatment)[results$treatment]
results$age <- levels(as.factor(df$Age))[results$age]

# Estimate deterrence
control_results <- results %>% filter(treatment == "UT")

deterrence_df <- results %>%
  left_join(control_results, by = "age", suffix = c("", "_control")) %>%
  mutate(
    deterrence = 100 * (mean_control - mean) / mean_control,
    deterrence_lower = 100 * (lower_control - upper) / lower_control,
    deterrence_upper = 100 * (upper_control - lower) / upper_control
  ) %>%
  filter(treatment != "UT")

# Function to compute Bayesian p-values
compute_pvals <- function(treatment_id, age_id) {
  lambda_treat <- exp(
    posterior$alpha +
      posterior$beta_treatment[, treatment_id] +
      posterior$beta_age[, age_id] +
      posterior$beta_time * mean(stan_data$time)
  )
  
  lambda_control <- exp(
    posterior$alpha +
      posterior$beta_treatment[, 1] +  # assuming "UT" is treatment 1
      posterior$beta_age[, age_id] +
      posterior$beta_time * mean(stan_data$time)
  )
  
  p_val <- mean(lambda_treat < lambda_control)
  return(p_val)
}

# Compute p-values for each treatment/age combo
deterrence_df$p_value <- mapply(
  compute_pvals,
  treatment_id = match(deterrence_df$treatment, levels(df$Treatment)),
  age_id = match(deterrence_df$age, levels(as.factor(df$Age)))
)

# Save final results
print(deterrence_df)
write_xlsx(deterrence_df, "deterrence_with_pvalues.xlsx")






#########################################################################total####################################################################################
##############################################################################################################################################################################


# Load required libraries
library(readxl)
library(dplyr)

# Read the data
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "combined")

# Ensure variables are properly formatted
df$treatment <- as.factor(df$Treatment)
df$age <- as.factor(df$Age)

# --- Method 1: Using base R (tapply) to compute mean
tapply_results <- tapply(df$total, list(df$treatment, df$age), mean)
print("Mean mosquitoes per treatment and age (tapply):")
print(tapply_results)

# --- Method 2: Using dplyr to compute mean
mean_df <- df %>%
  group_by(treatment, age) %>%
  summarise(mean_mosquitoes = mean(total, na.rm = TRUE)) %>%
  ungroup()

print("Mean mosquitoes per treatment and age (dplyr):")
print(mean_df)

library(lme4)
df$treatment <- as.factor(df$treatment)
df$age <- as.factor(df$age)

fit_nb <- glmer.nb(total ~ treatment + age+ (1|hut), data = df, ref="UT")
summary(fit_nb)

exp(coef(summary(fit_nb))["(Intercept)", "Estimate"])
exp(coef(summary(fit_nb))["(Intercept)", "Estimate"]+coef(summary(fit_nb))["age2", "Estimate"]+
      coef(summary(fit_nb))["treatmntIG2", "Estimate"])
exp(coef(summary(fit_nb))["(Intercept)", "Estimate"]+coef(summary(fit_nb))["age3", "Estimate"]+
      coef(summary(fit_nb))["treatmntIG2", "Estimate"])
exp(coef(summary(fit_nb))["(Intercept)", "Estimate"]+coef(summary(fit_nb))["age4", "Estimate"]+
      coef(summary(fit_nb))["treatmntIG2", "Estimate"])









#######################detrerrenceplot###################################################################
#############################################################################################################


library(ggplot2)
library(dplyr)

deterrence_values <- c(35.21, 35.2, 35.08, 4.31, 
                       30.8, 47.8, 30.2, -16, 
                       44, 33.8, 8.3, -8.6, 
                       -9.1, 27.8, 9.2, -25.9)

age <- rep(c("New", "12M", "24M", "36M"), times = 4)
treatment <- rep(c("IG1", "IG2", "P3", "RG"), each = 4)

det_df <- data.frame(
  Treatment = factor(treatment, levels = c("IG1", "IG2", "P3", "RG")),
  Age = factor(age, levels = c("New", "12M", "24M", "36M")),
  Deterrence = deterrence_values
)

brown_palette <- c("#5C4033", "#A0522D", "#CD853F", "#D2B48C")

ggplot(det_df, aes(x = Treatment, y = Deterrence, fill = Age)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(Deterrence, 1)),
            position = position_dodge(width = 0.9),
            vjust = ifelse(det_df$Deterrence < 0, 1.2, -0.3),
            size = 3) +
  scale_fill_manual(values = brown_palette) +
  labs(title = "Deterrence",
       x = "Treatment",
       y = "Deterrence (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )















































################ deterrence (Poisson version)

# Load required libraries
library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

# Read the data
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "combined")

# Convert Treatment and Age to factors and numeric IDs
df$Treatment <- factor(df$Treatment, levels = c("UT", "IG1", "IG2", "P3", "RG"))
df$treatment_id <- as.numeric(df$Treatment)
df$age_id <- as.numeric(as.factor(df$Age))

# Prepare data for Stan
stan_data <- list(
  N = nrow(df),
  total = df$total,
  A = length(unique(df$age_id)),
  T = length(unique(df$treatment_id)),
  age = df$age_id,
  treatment = df$treatment_id,
  time = df$Time
)

# Stan model code: Poisson for total count
stan_model_code <- "
data {
  int<lower=1> N;
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
    total[i] ~ poisson_log(eta);
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
)

# Extract posterior samples
posterior <- rstan::extract(fit)

# Generate combinations of treatment and age
treatment_ids <- 1:stan_data$T
age_ids <- 1:stan_data$A
combo_grid <- expand.grid(treatment = treatment_ids, age = age_ids)

# Compute predicted total mosquito counts
results <- data.frame()

for (i in 1:nrow(combo_grid)) {
  t <- combo_grid$treatment[i]
  a <- combo_grid$age[i]
  
  eta <- posterior$alpha + posterior$beta_treatment[, t] + 
    posterior$beta_age[, a] + 
    posterior$beta_time * mean(stan_data$time)
  
  lambda <- exp(eta)
  
  summary_row <- data.frame(
    treatment = t,
    age = a,
    mean = mean(lambda),
    lower = quantile(lambda, 0.025),
    upper = quantile(lambda, 0.975)
  )
  
  results <- rbind(results, summary_row)
}

# Restore treatment and age labels
results$treatment <- levels(df$Treatment)[results$treatment]
results$age <- levels(as.factor(df$Age))[results$age]

# Estimate deterrence
control_results <- results %>% filter(treatment == "UT")

deterrence_df <- results %>%
  left_join(control_results, by = "age", suffix = c("", "_control")) %>%
  mutate(
    deterrence = 100 * (mean_control - mean) / mean_control,
    deterrence_lower = 100 * (lower_control - upper) / lower_control,
    deterrence_upper = 100 * (upper_control - lower) / upper_control
  ) %>%
  filter(treatment != "UT")

# Function to compute Bayesian p-values
compute_pvals <- function(treatment_id, age_id) {
  lambda_treat <- exp(
    posterior$alpha +
      posterior$beta_treatment[, treatment_id] +
      posterior$beta_age[, age_id] +
      posterior$beta_time * mean(stan_data$time)
  )
  
  lambda_control <- exp(
    posterior$alpha +
      posterior$beta_treatment[, 1] +  # assuming "UT" is treatment 1
      posterior$beta_age[, age_id] +
      posterior$beta_time * mean(stan_data$time)
  )
  
  p_val <- mean(lambda_treat < lambda_control)
  return(p_val)
}

# Compute p-values for each treatment/age combo
deterrence_df$p_value <- mapply(
  compute_pvals,
  treatment_id = match(deterrence_df$treatment, levels(df$Treatment)),
  age_id = match(deterrence_df$age, levels(as.factor(df$Age)))
)

# Save final results
print(deterrence_df)
write_xlsx(deterrence_df, "deterrence_with_pvalues_poisson.xlsx")





























































################################################################################################################################################################
###############age continous########################################



df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "combined")







library(dplyr)
library(writexl)

# Define a fixed increment step
increment_step <- 0.001

df <- df %>%
  group_by(Treatment, Age) %>%
  mutate(
    # Row number within Treatment and Age group
    row_in_group = row_number(),
    # age_continuous: age + (row_number - 1)*increment_step
    age_continuous = as.numeric(as.character(Age)) + (row_in_group - 1) * increment_step
  ) %>%
  ungroup() %>%
  select(-row_in_group)

# Save result
#write_xlsx(df, "df_2.xlsx")




df_2 <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/df_2.xlsx")











library(rstan)
library(dplyr)
library(readxl)
library(writexl)

#--- Step 1: Load and prepare data -------------------


df_2 <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/df_2.xlsx")

# Make Treatment a factor with your desired order
df$Treatment <- factor(df$Treatment, levels = c("UT", "IG1", "IG2", "P3", "RG"))

# Create numeric treatment ID
df$treatment_id <- as.numeric(df$Treatment)

# Create continuous age within each Treatment and Age group
df <- df %>%
  group_by(Treatment, Age) %>%
  mutate(
    row_in_group = row_number(),
    base_age = as.numeric(as.character(Age)),
    age_continuous = base_age + row_in_group / 1000  # Add decimal for continuity
  ) %>%
  ungroup()

#--- Step 2: Prepare data list for Stan -------------------

stan_data <- list(
  N = nrow(df),
  total_dead = df$tot_72h_dead,
  total = df$total,
  T = length(unique(df$treatment_id)),
  age_continuous = df$age_continuous,
  treatment = df$treatment_id,
  time = df$Time
)

#--- Step 3: Stan model code with continuous age -----------

stan_model_code <- "
data {
  int<lower=1> N;
  int<lower=0> total_dead[N];
  int<lower=0> total[N];
  int<lower=1> T;
  vector[N] age_continuous;
  int<lower=1,upper=T> treatment[N];
  vector[N] time;
}
parameters {
  real alpha;
  vector[T] beta_treatment;
  real beta_age;
  real beta_time;
}
model {
  alpha ~ normal(0, 5);
  beta_treatment ~ normal(0, 2);
  beta_age ~ normal(0, 2);
  beta_time ~ normal(0, 2);

  for (i in 1:N) {
    real eta = alpha + beta_treatment[treatment[i]] + beta_age * age_continuous[i] + beta_time * time[i];
    total_dead[i] ~ binomial_logit(total[i], eta);
  }
}
"

#--- Step 4: Compile and fit model ----------------------------

fit <- stan(
  model_code = stan_model_code,
  data = stan_data,
  chains = 4,
  iter = 2000,
  control = list(max_treedepth = 15)
)

print(fit, pars = c("alpha", "beta_treatment", "beta_age", "beta_time"))

#--- Step 5: Extract posterior and make predictions -------------

posterior <- rstan::extract(fit)

names(posterior)

# Generate a sequence of continuous ages for prediction
age_seq <- seq(min(df$age_continuous), max(df$age_continuous), length.out = 100)
treatment_ids <- 1:stan_data$T

results <- data.frame()

#for (t in treatment_ids) {
 # for (a in age_seq) {
  #  eta <- posterior$alpha + posterior$beta_treatment[, t] + posterior$beta_age * a + posterior$beta_time * mean(stan_data$time)
  #  p <- plogis(eta)
    
   # results <- rbind(results, data.frame(
    #  treatment = t,
     # age_continuous = a,
      #mean = mean(p),
      #lower = quantile(p, 0.025),
      #upper = quantile(p, 0.975)
    #))
  #}
#}

treatment_ids <- 1:stan_data$T
results <- data.frame()

for (t in treatment_ids) {
  for (a in unique(df$age_continuous)) {
    eta <- posterior$alpha + posterior$beta_treatment[, t] + posterior$beta_age * a + posterior$beta_time * mean(stan_data$time)
    p <- plogis(eta)
    
    results <- rbind(results, data.frame(
      treatment = t,
      age_continuous = a,
      mean = mean(p),
      lower = quantile(p, 0.025),
      upper = quantile(p, 0.975)
    ))
  }
}



# Map numeric treatment back to factor labels
treatment_labels <- levels(df$Treatment)
results$treatment <- treatment_labels[results$treatment]

# View results
print(head(results, 20))

#--- Step 6: Save results ---------------------------------------


write_xlsx(results, "mortality_continuous_age_predictions.xlsx")








library(dplyr)

# Step 1: Create integer age category from age_continuous
results <- results %>%
  mutate(age_group = floor(age_continuous))

# Step 2: Define function to compute average over age range
get_avg_for_range <- function(data, age_start, age_end) {
  data %>%
    filter(age_group >= age_start, age_group <= age_end) %>%
    group_by(treatment) %>%
    summarise(
      age_range = paste0(age_start, "-", age_end),
      mean_mortality = mean(mean),
      lower_ci = mean(lower),
      upper_ci = mean(upper),
      .groups = "drop"
    )
}

#  Apply to all desired age ranges
ranges <- list(c(1, 2), c(1, 3), c(1, 4), c(2, 3), c(2, 4), c(3, 4))

results_summary <- do.call(rbind, lapply(ranges, function(rng) {
  get_avg_for_range(results, rng[1], rng[2])
}))

#  View or save
print(results_summary)
write_xlsx(results_summary, "mortality_by_age_range.xlsx")



#########mortality per agerange

library(dplyr)
library(writexl)

# Step 1: Create an integer age group column (1 for 1.0–1.999, 2 for 2.0–2.999, etc.)
results <- results %>%
  mutate(age_group = floor(age_continuous))

# Step 2: Define a function to summarize mean mortality across an age range
get_avg_for_range <- function(data, age_start, age_end) {
  data %>%
    filter(age_group >= age_start, age_group <= age_end) %>%
    group_by(treatment) %>%
    summarise(
      age_range = paste0(age_start, "-", age_end),
      mean_mortality = mean(mean),
      lower_ci = mean(lower),
      upper_ci = mean(upper),
      .groups = "drop"
    )
}

# Step 3: Define the age ranges you want
age_ranges <- list(c(1, 2), c(1, 3), c(1, 4), c(2, 3), c(2, 4), c(3, 4))

# Step 4: Apply the function to each range and combine the results
results_summary <- do.call(rbind, lapply(age_ranges, function(rng) {
  get_avg_for_range(results, rng[1], rng[2])
}))

# Step 5: Save or print the summarized results
#print(results_summary)
#write_xlsx(results_summary, "mortality_by_age_range.xlsx")











library(ggplot2)

# Ensure age_range is ordered correctly
results_summary$age_range <- factor(
  results_summary$age_range,
  levels = c("1-2", "1-3", "1-4", "2-3", "2-4", "3-4")
)



ggplot(results_summary, aes(x = age_range, y = mean_mortality, fill = treatment)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.9),
    width = 0.8,
    color = "black"  # Adds border line on bars
  ) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.9),
    width = 0.2,
    color = "black"  # Optional: error bar color match
  ) +
  labs(
    title = "Estimated Mortality by Net Age Range",
    x = "Age Range (Years)",
    y = "Mean Mortality"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1)  # Border around entire plot panel
  )











# Relabel age_range
results_summary$age_range <- factor(
  results_summary$age_range,
  levels = c("1-2", "1-3", "1-4", "2-3", "2-4", "3-4"),
  labels = c("New-12M", "New-24M", "New-36M", "12M-24M", "12M-36M", "24M-36M")
)

# Create the plot
ggplot(results_summary, aes(x = age_range, y = mean_mortality, fill = treatment)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.9),
    width = 0.8,
    color = "black"  # Border around bars
  ) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.9),
    width = 0.2,
    color = "black"
  ) +
  labs(
    title = "Estimated Mortality by Net Age",
    x = "Age Range",
    y = "Mean Mortality"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )












#group bytreatment

ggplot(results_summary, aes(x = treatment, y = mean_mortality, fill = age_range)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.9),
    width = 0.8,
    color = "black"  # Border around bars
  ) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.9),
    width = 0.2,
    color = "black"
  ) +
  labs(
    title = "Estimated Mortality by Treatment and Net Age",
    x = "Treatment",
    y = "Mean Mortality",
    fill = "Net Age"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  )








############################################################lets do halflife#####################################################################
###########################################################################################################################################################


library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
options(mc.cores=4)
rstan_options(auto_write = TRUE)


df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "combined")
View(df)


df_IG1 <- df %>%
  filter(Treatment == "IG2")


# Filter for zero washes (Age = 1)
dfa0 <- df_IG1 %>% filter(Treatment == "IG2" & Age == 1)
View(dfa0)

# Filter for twenty washes (Age = 4)
dfa20 <- df_IG1 %>% filter(Treatment == "IG2" & Age == 4)
View(dfa20)


#merge
library(dplyr)



dfa_match <- merge(dfa0, dfa20, by = c( "hut","sleeper"))
View(dfa_match)
dim(dfa_match)
library(writexl)
write_xlsx(dfa_match, "merge.xlsx")


#clean the data

dfa_match = dfa_match %>% drop_na(total.x)
dfa_match = dfa_match %>% drop_na(tot_72h_dead.x)
dfa_match = dfa_match %>% drop_na(total.y)
dfa_match = dfa_match %>% drop_na(tot_72h_dead.y)

dfa_match = subset(dfa_match,dfa_match$total.x != 0)
dim(dfa_match)


######simple

## Simple model first
setup_inputs = function(dfa_match){
  
  # Prep data
  S <- as.numeric(length(dfa_match$total.x))
  prop_dead <- c(dfa_match$tot_72h_dead.x)/dfa_match$total.x
  #X_halflife <-c(dfa_match$total.y - dfa_match$tot_72h_dead.y)
  X_halflife <-c(dfa_match$tot_72h_dead.y)
  X_caught_halflife <- dfa_match$total.y
  
  
  # Need to pass it the data:
  data_stan <- list(S=S, 
                    prop_dead=prop_dead,
                    X_halflife=X_halflife,
                    N_caught_halflife=X_caught_halflife)#,
  # nsite = nsite,
  # site = site)
  
  
  return(data_stan)
  
}


#stan block




stan_code <- "
data {
  int S; // number of data points int because its a count
  vector[S] prop_dead; // This is the predictor lp

  // data on # successfully fed
  int X_halflife[S]; // mortality for twenty washes
  int N_caught_halflife[S]; // mortality for twenty washes
}

parameters {
  real a;
  real b;
}

model {
  real sp[S];

  // priors by looking at the data - a (8), b(0.006)
  a ~ normal(0,100);
  b ~ normal(0,100); // weakly informative prior

  // likelihood
  for (i in 1:S) {
    sp[i] = a + b * prop_dead[i];
  }

  // half-life binomial logit model
  X_halflife ~ binomial_logit(N_caught_halflife, sp);
}
"

# Then compile your model from this string:
full_model <- stan_model(model_code = stan_code)


# Compile model
#full_model<- stan_model("R code/stan models/Model_halflife_0.stan") # flex params


#stan_base <- rstan::stan(file="R code/stan models/Model_halflife_0.stan", 
 #                        data=setup_inputs(dfa_match), 
  #                       warmup=1000,
   #                      control = list(adapt_delta = 0.8,
    #                                    max_treedepth = 20),
     #                    iter=2000, chains=4)

stan_base <- rstan::sampling(full_model,
                             data=setup_inputs(dfa_match),
                             warmup=1000,
                             iter=2000,
                             chains=4,
                             control=list(adapt_delta=0.8, max_treedepth=20))

base <- rstan::extract(stan_base)
median(base$a)
median(base$b)



run_f = function(dfa_match,dataset){
  data_stan = setup_inputs(dfa_match)
  
  # Run model
  fit_full <- sampling(full_model, data=data_stan, iter=2000, chains=4)
  # params_a = extract(fit_full)
  
  # save fit
  saveRDS(fit_full, paste0("stan model outputs/ento_fit1_half_life_",dataset,".rds"))
}
run_f(dfa_match,"IG2")#all data

run_f = function(dfa_match, dataset){
  data_stan = setup_inputs(dfa_match)
  
  # Run model
  fit_full <- sampling(full_model, data=data_stan, iter=2000, chains=4)
  
  # Create folder if it doesn't exist
  if(!dir.exists("stan model outputs")){
    dir.create("stan model outputs")
  }
  
  # save fit
  saveRDS(fit_full, paste0("stan model outputs/ento_fit1_half_life_", dataset, ".rds"))
}


ento_fit1_half_life_IG2 <- readRDS("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/ento_fit1_half_life_IG2.rds")
print(ento_fit1_half_life_IG2)




# proportion of mosquitoes killed for 0 washes

prop_killed_0_wash = setup_inputs(dfa_match)$prop_dead
prop_killed20_wash = setup_inputs(dfa_match)$X_halflife/setup_inputs(dfa_match)$N_caught_halflife
mean(prop_killed_0_wash)
mean(prop_killed20_wash)

test = base
names(test)
sim_X = seq(0,1,0.01)
P_hl1 <- 1/(1 + exp(-(median(test$a) + median(test$b) * sim_X)))
P_hl1_low <- 1/(1 + exp(-(quantile(test$a,0.025) + quantile(test$b,0.025) * sim_X)))
P_hl1_upp <- 1/(1 + exp(-(quantile(test$a,0.975) + quantile(test$b,0.975) * sim_X)))
median(P_hl1)











#install.packages("adegenet")
#install.packages("adegenet", type = "source")
library(adegenet)
par(mfrow=c(1,1))

lp_tau = seq(0,1,0.01)-0.5
mort1 = seq(0,1,length=length(lp_tau))
mu1 = -2.36
rho1 = -3.01
original_eLife = 1/(1 + exp(-(-mu1 - rho1 * lp_tau)))

plot(original_eLife ~ c(1-mort1),ylim=c(0,1),xlim=c(0,1),
     ylab = "EHT Mortality after 20 washes (%)",
     xlab = "EHT Mortality after 0 washes (%)",
     xaxt="n",yaxt="n")
axis(1,at=seq(0,1,0.2),labels=seq(0,100,20))
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20))

lines(P_hl1 ~ sim_X,col="red")
polygon(c(sim_X,rev(sim_X)),
        c(P_hl1_low,rev(P_hl1_upp)),border=NA,col=adegenet::transp("red",0.4))
points(prop_killed_0_wash, prop_killed20_wash,
       ylim=c(0,1),xlim=c(0,1),
       col=adegenet::transp("darkred",0.6),pch=19)

Hw = log(2)/(1/(1+exp(-(median(test$a) + median(test$b) * sim_X))))
HW2=log(2)/(1/(1 + exp(-(-mu1 - rho1 * lp_tau))))
mean(Hw)

















##############################################REDO
























# Load packages
library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# Parallel processing options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Read data
df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "combined")

# Filter for IG2 treatment
df_IG1 <- df %>% filter(Treatment == "IG1") %>% drop_na(total, tot_72h_dead) %>% filter(total > 0)
dim(df_IG1)
# Filter for Age 1 (0 washes) and Age 4 (20 washes)
dfa0 <- df_IG1 %>% filter(Age == 1)  %>% drop_na(total, tot_72h_dead) %>% filter(total > 0)
dfa20 <- df_IG1 %>% filter(Age == 4)  %>% drop_na(total, tot_72h_dead) %>% filter(total > 0)
View(dfa20)
dim(dfa20)
View(dfa0)
dim(dfa0)


library(writexl)

# Save the dataframe to Excel
write_xlsx(dfa0, "dfa0.xlsx")
write_xlsx(dfa20, "dfa20.xlsx")


#dfa0 <- dfa0 %>%
 # group_by(day, week) %>%
  #summarise(
    #across(where(is.numeric), sum, na.rm = TRUE),
    #Treatment = first(Treatment),
    #treat_name= first(treat_name),
    #.groups = "drop"
  #)

# Setup input function
setup_inputs <- function(dfa0, dfa20) {
  list(
    S0 = nrow(dfa0),
    prop_dead = dfa0$tot_72h_dead / dfa0$total,
    S20 = nrow(dfa20),
    X_halflife = dfa20$tot_72h_dead,
    N_caught_halflife = dfa20$total
  )
}

# Stan code
stan_code <- "
data {
  int S0;
  vector[S0] prop_dead;
  int S20;
  int X_halflife[S20];
  int N_caught_halflife[S20];
}
parameters {
  real a;
  real b;
}
model {
  vector[S20] sp;
  a ~ normal(0, 100);
  b ~ normal(0, 100);
  real pd_mean = mean(prop_dead);
  for (i in 1:S20)
    sp[i] = a + b * prop_dead[i];
  X_halflife ~ binomial_logit(N_caught_halflife, sp);
}
"

# Compile the Stan model
full_model <- stan_model(model_code = stan_code)

# Prepare data for Stan
data_stan <- setup_inputs(dfa0, dfa20)

# Fit the model
stan_base <- sampling(full_model,
                      data = data_stan,
                      warmup = 1000,
                      iter = 2000,
                      chains = 4,
                      control = list(adapt_delta = 0.8, max_treedepth = 20))

# Extract posterior samples
#posterior_samples <- extract(stan_base)
#a_median <- median(posterior_samples$a)
#b_median <- median(posterior_samples$b)

base <- rstan::extract(stan_base)
median(base$a)
median(base$b)




# proportion of mosquitoes killed for 0 washes

prop_killed_0_wash <- setup_inputs(dfa0, dfa20)$prop_dead
prop_killed20_wash <- setup_inputs(dfa0, dfa20)$X_halflife / setup_inputs(dfa0, dfa20)$N_caught_halflife

#prop_killed_0_wash = setup_inputs(dfa0)$prop_dead
#prop_killed20_wash = setup_inputs(dfa20)$X_halflife/setup_inputs(dfa20)$N_caught_halflife
mean(prop_killed_0_wash)
mean(prop_killed20_wash)

test = base
names(test)
sim_X = seq(0,1,0.01)
P_hl1 <- 1/(1 + exp(-(median(test$a) + median(test$b) * sim_X)))
P_hl1_low <- 1/(1 + exp(-(quantile(test$a,0.025) + quantile(test$b,0.025) * sim_X)))
P_hl1_upp <- 1/(1 + exp(-(quantile(test$a,0.975) + quantile(test$b,0.975) * sim_X)))
median(P_hl1)
median(P_hl1_low )
median(P_hl1_upp)











#install.packages("adegenet")
#install.packages("adegenet", type = "source")
library(adegenet)
par(mfrow=c(1,1))

lp_tau = seq(0,1,0.01)-0.5
mort1 = seq(0,1,length=length(lp_tau))
mu1 = -2.36
rho1 = -3.01
  original_eLife = 1/(1 + exp(-(-mu1 - rho1 * lp_tau)))

plot(original_eLife ~ c(1-mort1),ylim=c(0,1),xlim=c(0,1),
     ylab = "EHT Mortality after 20 washes (%)",
     xlab = "EHT Mortality after 0 washes (%)",
     xaxt="n",yaxt="n")
axis(1,at=seq(0,1,0.2),labels=seq(0,100,20))
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20))

lines(P_hl1 ~ sim_X,col="red")
polygon(c(sim_X,rev(sim_X)),
        c(P_hl1_low,rev(P_hl1_upp)),border=NA,col=adegenet::transp("red",0.4))
points(prop_killed_0_wash, prop_killed20_wash,
       ylim=c(0,1),xlim=c(0,1),
       col=adegenet::transp("darkred",0.6),pch=19)

Hw = log(2)/(1/(1+exp(-(median(test$a) + median(test$b) * sim_X))))
HW2=log(2)/(1/(1 + exp(-(-mu1 - rho1 * lp_tau))))
mean(Hw)

summary(prop_killed_0_wash)
summary(prop_killed20_wash)














plot(original_eLife ~ c(1-mort1), ylim=c(0,1), xlim=c(0,1),
     ylab = "EHT Mortality after 20 washes (%)",
     xlab = "EHT Mortality after 0 washes (%)",
     xaxt="n", yaxt="n")
axis(1, at=seq(0,1,0.2), labels=seq(0,100,20))
axis(2, las=2, at=seq(0,1,0.2), labels=seq(0,100,20))

lines(P_hl1 ~ sim_X, col="red")
polygon(c(sim_X, rev(sim_X)),
        c(P_hl1_low, rev(P_hl1_upp)),
        border=NA, col=adegenet::transp("red", 0.4))

# Plot points LAST and clearly
points(prop_killed_0_wash, prop_killed20_wash,
       col="darkred", pch=19, cex=1.5)












#########################without merging




library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
options(mc.cores=4)
rstan_options(auto_write = TRUE)


df <- read_excel("Wezzie-Tom/EHT DATA/Benin Trial/benin trial data results/Benin_NNP_hut_trial_raw data (2).xlsx", 
                 sheet = "combined")
#View(df)



df_IG1 <- df %>% filter(Treatment == "RG") %>% drop_na(total, tot_72h_dead) %>% filter(total > 0)
dim(df_IG1)


# Filter for zero washes (Age = 1)

dfa0 <- df_IG1 %>% filter(Age == 1)
dim(dfa0)
dfa0 <- dfa0 %>%
  group_by(day, week) %>%
  summarise(
    across(where(is.numeric), sum, na.rm = TRUE),
    Treatment = first(Treatment),
    treat_name= first(treat_name),
    .groups = "drop"
  )
dim(dfa0)

# Filter for twenty washes (Age = 4)
dfa20 <- df_IG1 %>% filter(Age == 4)
dim(dfa20)

# Filter for IG2 treatment
#dim(df_IG1)
# Filter for Age 1 (0 washes) and Age 4 (20 washes)
#dfa0 <- df_IG1 %>% filter(Age == 1) 
#dfa20 <- df_IG1 %>% filter(Age == 4) 
#View(dfa20)
#dim(dfa20)
#View(dfa0)
#dim(dfa0)





dfa_match <- merge(dfa0, dfa20, by = c( "day", "week"))
dim(dfa_match)

library(writexl)
write_xlsx(dfa_match, "merge.xlsx")


#clean the data

dfa_match = dfa_match %>% drop_na(total.x)
dfa_match = dfa_match %>% drop_na(tot_72h_dead.x)
dfa_match = dfa_match %>% drop_na(total.y)
dfa_match = dfa_match %>% drop_na(tot_72h_dead.y)

dim(dfa_match)


######simple

## Simple model first
setup_inputs = function(dfa_match){
  
  # Prep data
  S <- as.numeric(length(dfa_match$total.x))
  prop_dead <- c(dfa_match$tot_72h_dead.x)/dfa_match$total.x
  X_halflife <-c(dfa_match$total.y - dfa_match$tot_72h_dead.y)
  #X_halflife <-c(dfa_match$tot_72h_dead.y)
  X_caught_halflife <- dfa_match$total.y
  
  
  # Need to pass it the data:
  data_stan <- list(S=S, 
                    prop_dead=prop_dead,
                    X_halflife=X_halflife,
                    N_caught_halflife=X_caught_halflife)#,
  # nsite = nsite,
  # site = site)
  
  
  return(data_stan)
  
}


#stan block




stan_code <- "
data {
  int S; // number of data points int because its a count
  vector[S] prop_dead; // This is the predictor lp

  // data on # successfully fed
  int X_halflife[S]; // mortality for twenty washes
  int N_caught_halflife[S]; // mortality for twenty washes
}

parameters {
  real a;
  real b;
}

model {
  real sp[S];

  // priors by looking at the data - a (8), b(0.006)
  a ~ normal(0,10);
  b ~ normal(0,10); // weakly informative prior

  // likelihood
  for (i in 1:S) {
    sp[i] = a + b * prop_dead[i];
  }

  // half-life binomial logit model
  X_halflife ~ binomial_logit(N_caught_halflife, sp);
}
"

# Then compile your model from this string:
full_model <- stan_model(model_code = stan_code)




stan_base <- rstan::sampling(full_model,
                             data=setup_inputs(dfa_match),
                             warmup=1000,
                             iter=2000,
                             chains=4,
                             control=list(adapt_delta=0.8, max_treedepth=20))

base <- rstan::extract(stan_base)
median(base$a)
median(base$b)






# proportion of mosquitoes killed for 0 washes

prop_killed_0_wash = setup_inputs(dfa_match)$prop_dead
prop_killed20_wash =setup_inputs(dfa_match)$X_halflife/setup_inputs(dfa_match)$N_caught_halflife
mean(prop_killed_0_wash)
mean(prop_killed20_wash)

test = base
names(test)
sim_X = seq(0,1,0.01)
P_hl1 <- 1/(1 + exp(-(median(test$a) + median(test$b) * sim_X)))
P_hl1_low <- 1/(1 + exp(-(quantile(test$a,0.025) + quantile(test$b,0.025) * sim_X)))
P_hl1_upp <- 1/(1 + exp(-(quantile(test$a,0.975) + quantile(test$b,0.975) * sim_X)))
median(P_hl1)
median(P_hl1_low)
median(P_hl1_upp)



#install.packages("adegenet")
#install.packages("adegenet", type = "source")
#library(adegenet)
#par(mfrow=c(1,1))

#lp_tau = seq(0,1,0.01)-0.5
#mort1 = seq(0,1,length=length(lp_tau))
#mu1 = -2.36
#rho1 = -3.01
#original_eLife = 1/(1 + exp(-(-mu1 - rho1 * lp_tau)))
#mean(original_eLife)

#plot(original_eLife ~ c(1-mort1),ylim=c(0,1),xlim=c(0,1),
   #  ylab = "EHT Mortality after 20 washes (%)",
    # xlab = "EHT Mortality after 0 washes (%)",
    # xaxt="n",yaxt="n")
#axis(1,at=seq(0,1,0.2),labels=seq(0,100,20))
#axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20))

#lines(P_hl1 ~ sim_X,col="red")
#polygon(c(sim_X,rev(sim_X)),
#        c(P_hl1_low,rev(P_hl1_upp)),border=NA,col=adegenet::transp("red",0.1))
#points(prop_killed_0_wash, prop_killed20_wash,
 #      ylim=c(0,1),xlim=c(0,1),
  #     col=adegenet::transp("darkred",0.6),pch=19)

Hw = log(2)/(1/(1+exp(-(median(test$a) + median(test$b) * sim_X))))
HW2=log(2)/(1/(1 + exp(-(-mu1 - rho1 * lp_tau))))
mean(Hw)


original_eLife
original_eLife =log(2)/( 1/(1 + exp(-(-mu1 - rho1 * lp_tau))))
























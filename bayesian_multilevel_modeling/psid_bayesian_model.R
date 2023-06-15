##### Coding sample - Renée Hagen

### Modeling the effect of a first birth on total labor income by gender, accounting for age and market effects

# This script analyses PSID income data to investigate the temporal effect of having a first child on parents' labor income.
# I use a Bayesian hierarchical model with a hurdle-lognormal link function, as labor income is exponentially distributed 
# but also includes zeroes. These zeroes are important, because parents (and especially mothers) may leave the labor market 
# when they have children. 
#
# The model accounts for the complex survey design of the PSID, as well as for attrition in two ways: 1) I weigh the likelihood
# contribution of each row in the data by the survey weights provided by the PSID, and 2) I use information on sampling strata
# and clusters within strata as varying intercepts
#
# Lastly, I am interested in the effect of having children after accounting for variation in income across the lifespan and 
# variation due to market-wide effects. Income increases with age but flattens after age ~50, so I account for this with polynomials.
# I account for market fluctuations with varying intercepts per year.


###### Modeling total labor income dependent on age, gender and years since first birth

# Load packages
# install.packages(c("tidyverse", "brms", "emmeans", "tidybayes"))
library(tidyverse)
library(brms)
library(emmeans)
library(tidybayes)

### Load data and filter rows

# Load data
psid <- read.csv("https://raw.githubusercontent.com/reneehagen/coding_samples/main/psid_clean.csv")

# Filter rows 
d <- psid %>%
  filter(birth_oldest_year >= 2008 & # include only parents of children born after 2008
           years_since_first_birth > -11 & years_since_first_birth <= 6 &  # include 10 years before and 6 years after t = 0
           age >= 25 & # exclude ages most affected by college-attendance since this strongly affects labor income
           longitudinal_weights > 0 # weight data according to 2019 population structure, exclude rows with zero weight to increase model speed
  ) %>% 
  drop_na(total_labor_income) # drop cases with missing outcome


### Visualize data descriptives
theme_new <- theme_bw(base_size = 14,
                      base_rect_size = 0,
                      base_line_size = 0)

# Histograms of income data (raw data and logged data)
hist1 <- d %>%
  mutate(is_zero = total_labor_income == 0,
         total_labor_income = ifelse(is_zero, -0.1, total_labor_income)) %>% 
  ggplot(aes(x = total_labor_income)) +
  geom_histogram(aes(fill = is_zero), color = "white", linewidth = 1.5,
                 binwidth = 15000, boundary = 0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(limits = c(-25000, 2e5)) +
  scale_fill_manual(values = c("#a1d76a", "#e9a3c9"), 
                    guide = guide_legend(reverse = TRUE)) +
  labs(x = "Total labor income", y = "Number of observations", fill = "No income?", subtitle = "Original scale") +
  theme_new +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank())

hist2 <- d %>%
  mutate(is_zero = total_labor_income == 0,
         total_labor_income_log = ifelse(is_zero, -0.1, log(total_labor_income))) %>% 
  ggplot(aes(x = total_labor_income_log)) +
  geom_histogram(aes(fill = is_zero), color = "white", linewidth = 1.5,
                 binwidth = 1, boundary = 0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_manual(values = c("#a1d76a", "#e9a3c9"), 
                    guide = guide_legend(reverse = TRUE),
                    labels = c("Non-zero income","No income")) +
  labs(x = "Total labor income (logged)", y = NULL, 
       subtitle = "Exponential scale",
       fill = NULL) +
  theme_new +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank())

legend <- cowplot::get_legend(
  hist2 + theme(legend.position = "bottom")
)

# save as PNG 
png("histograms.png", height = 500, width = 1500)
cowplot::plot_grid(cowplot::plot_grid(hist1, hist2, align = "hv", axis = "tblr"),
                   legend, nrow = 2, rel_heights = c(1, .1))
dev.off()


### Statistical model

# Set model parameters
iter <- 5000
warmup <- 1000
chains <- 4
cores <- 4
seed <- 11

# Set weakly regularizing priors
priors <- c(prior(normal(10, 5), class = Intercept),
            prior(normal(0, 10), class = Intercept, dpar = "hu"),
            prior(normal(0, 5), class = b),
            prior(cauchy(0, 2), class = sd),
            prior(student_t(3, 0, 2.5), class = sds),
            prior(exponential(1), class = sigma))

# Start model
message("Start modeling...")

t1 <- Sys.time()

fit <- brm(data = d,
         family = hurdle_lognormal(),
         formula = bf(
           
           # This part models the effects on total labor income when income is not zero, with a lognormal link function since 
           # non-zero income is exponentially distributed.
           # I use PSID survey weights to weigh the likelihood contribution of each row
           total_labor_income | weights(longitudinal_weights, scale = TRUE) ~ 
             
             # This is a multilevel model with varying intercepts per individual and per year
             # Varying intercepts per year allow the model to account for year-to-year fluctuations in the labor market
             # Varying intercepts per ID capture variation between individuals
             1 + (1 | id + year) + 
             
             # I also use sampling strata and clusters nested within strata as varying intercepts 
             (1 | sampling_stratum / sampling_cluster) +
             
             # Fixed effects for age and age squared (both centered around 0) account for age-effects on total labor income
             age_centered + age_2_centered + 
             
             # B-splines (with 7 knots) to model the effect of time since first birth on income per gender
             s(years_since_first_birth, bs = "bs", k = 7, by = gender) + gender, 
           
           # This part uses a logit link function to estimate the effects on the binary outcome of having any income vs no income.
           # Besides the link function and outcome, this model is the same as the mu-part (no need to add survey weights in this part)
           hu ~
             1 + (1 | id + year) +
             (1 | sampling_stratum / sampling_cluster) +
             age_centered + age_2_centered +
             s(years_since_first_birth, bs = "bs", k = 7, by = gender) + gender
         ), 
         
         prior = priors,
         
         iter = iter, warmup = warmup, chains = chains, cores = chains,
         seed = seed,
         control = list(adapt_delta = .95),
         file = "results/fit"
)

t2 <- Sys.time()

message(paste("Model run time: ", round(difftime(t2, t1, units='mins'), digits = 2), " minutes.", sep = ""))


######## Visualize model results

# set plot theme
theme_new <- theme_bw(base_size = 14,
                      base_rect_size = 0,
                      base_line_size = 0)

# Some posterior predictive checks
pred <- posterior_predict(fit, re_formula = NA)
density <- bayesplot::ppc_dens_overlay(y = log1p(fit$data$total_labor_income),
                                           yrep = log1p(pred[1:10,])) 

dens_plot <- density +
  labs(subtitle = "2008 - 2018 Model: Posterior predictive check", x = 'Total labor income (logged)', y = "Density") +
  scale_color_manual(labels=c('PSID data', 'Predicted'), values = c("#276419", "#a1d76a"), name = NULL) +
  theme_new + 
  theme(text = element_text(family = 'Arial'))

png("results/plot_posterior_density.png", height = 400, width = 850)
dens_plot
dev.off()

# Other pp checks
plot(fit)
pairs(fit)


### Plotting hurdle model's conditional effects of years since first birth per gender

# Summarize model
summary(fit)

# Plot mixed model
ce_mixed <- conditional_effects(fit, effects = "years_since_first_birth:gender",  # conditional_effects shows median and 95% interval
                                    conditions = data.frame(age_centered = 0,
                                                            age_2_centered = 0))
ce_mixed_plot <- plot(ce_mixed,
                points = F,
                plot = F)[[1]] +
  geom_vline(aes(xintercept = 0.25), color = 'red') +
  labs(subtitle = "Combined hurdle model",
       x = "Years since first birth", y = "Predicted labor income") +
  scale_y_continuous(limits = c(0, 70000), breaks = seq(0, 60000, length.out = 4), labels=scales::dollar_format()) +
  scale_x_continuous(limits = c(-9, 6), breaks = seq(-9, 6)) +
  scale_fill_manual(labels=c('Men', 'Women'), values = c("#f1a340", "#998ec3"), aesthetics = c("colour", "fill"), name = NULL) +
  theme_new + theme(legend.position = "bottom")

# Plot hu-only model
ce_hu <- conditional_effects(fit, effects = "years_since_first_birth:gender", dpar = "hu", 
                             conditions = data.frame(age_centered = 0,
                                                     age_2_centered = 0))

ce_hu_plot <- plot(ce_hu,
                   points = F,
                   plot = F)[[1]] +
  geom_vline(aes(xintercept = 0.25), color = 'red') +
  labs(subtitle = "Hurdle part only: predicting the probability of having no income",
       x = "Years since first birth", y = "Probability") +
  scale_y_continuous(limits = c(0, .20), breaks = seq(0, .20, by = .05)) +
  scale_x_continuous(limits = c(-9, 6), breaks = seq(-9, 6)) +
  scale_fill_manual(labels=c('Men', 'Women'), values = c("#f1a340", "#998ec3"), aesthetics = c("colour", "fill"), name = NULL) +
  theme_new +
  theme(legend.position = "none")

# Plot mu-only model 
ce_mu <- fit %>% 
  # I use the emmeans package here because brms::conditional_effects does not allow for the isolation of the mu part
  emmeans::emmeans(~ years_since_first_birth:gender, dpar = "mu",
                   regrid = "response", tran = "log", type = "response",
                   at = list(years_since_first_birth = seq(-9, 6, by = 1), age_centered = 0, age_2_centered = 0)
                   ) %>%
  gather_emmeans_draws()

ce_mu_plot <- ce_mu %>%
  mutate(.value = exp(.value)) %>%
  ggplot(aes(x = years_since_first_birth, y = .value, fill = gender, color = gender)) +
  stat_lineribbon(size = 1, alpha = .4, .width = 0.95) +
  stat_lineribbon(size = 1, .width = 0) + # add this so that line is not transparent
  geom_vline(aes(xintercept = 0.25), color = 'red') +
  scale_x_continuous(limits = c(-9, 6), breaks = seq(-9, 6)) +
  scale_y_continuous(limits = c(0, 70000), breaks = seq(0, 60000, length.out = 4), labels=scales::dollar_format()) +
  scale_fill_manual(labels=c('Men', 'Women'), values = c("#f1a340", "#998ec3"), aesthetics = c("colour", "fill"), name = NULL) +
  labs(subtitle = "Mu part only: predicting non-zero incomes",
       x = "Years since first birth", y = "Predicted labor income") +
  theme_new +
  theme(legend.position = "none")


# Plot combined, mu and hu models

legend <- cowplot::get_legend(ce_mixed_plot)

ce_mixed_plot <- ce_mixed_plot + theme(legend.position = "none")

label <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "2008 - 2018 Model",
    fontfamily = theme_new$family,
    fontface = theme_new$face,
    size = 16
  )

# save as PNG 
png("results/plot_conditionaleffects.png", height = 1300, width = 1000)
cowplot::plot_grid(label, 
                   cowplot::plot_grid(ce_mixed_plot, ce_mu_plot, ce_hu_plot, align = "hv", axis = "tblr", nrow = 3),
                   legend, nrow = 3, rel_heights = c(.05, .95, .05))
dev.off()

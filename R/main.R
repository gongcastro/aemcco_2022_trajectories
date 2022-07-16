library(dplyr)
library(arrow)
library(brms)
library(tidybayes)
library(ggplot2)
library(bayesplot)
library(here)
library(ggsci)
library(ggrepel)
library(stringr)
library(tibble)
library(job)

df <- read_ipc_stream(here("data", "main.parquet"))

# model prior
model_prior <- c(
    prior(normal(-0.25, 0.1), class = "Intercept"),
    prior(normal(1, 0.1), class = "sd", group = "te"),
    prior(normal(1, 0.1), class = "sd", group = "id"),
    prior(lkj(2), class = "cor"),
    prior(normal(1, 0.1), class = "b", coef = "age_std"),
    prior(normal(0, 0.1), class = "b", coef = "freq_std"),
    prior(normal(0, 0.1), class = "b", coef = "n_phon_std"),
    prior(normal(0, 0.1), class = "b", coef = "doe_std"),
    prior(normal(0, 0.1), class = "b", coef = "age_std:doe_std"),
    
)

model_fit_4 <- brm(
    formula = bf(
        response ~ age_std + freq_std + n_phon_std + doe_std +
            (1 + age_std + freq_std + n_phon_std + doe_std | id) + 
            (1 + age_std + freq_std + n_phon_std + doe_std | te) ,
        family = cratio(link = "logit") # cumulative, continuation ratio
    ), 
    data = df,
    prior = model_prior[1:8, ],
    sample_prior = "yes", # for faster computation of Bayes Factors and LOO
    iter = 4000,
    chains = 2,
    cores = 4,
    threads = threading(4, grainsize = 100),
    init = 0, # where to initialise MCMCs
    seed = 888, # for reproducibility
    backend = "cmdstanr", # for faster, less problematic compilation in C++
    file = here("results", "fit_4.rds"), # save model as file
    # file_refit = "always", # should model be refitted or loaded from file?
    control = list(
        adapt_delta = 0.9, # for better convergence of MCMCs
        max_treedepth = 15
    ),
    save_model = here("stan", "fit_4.stan") # save Stan code
)


job(
    import = c("model_prior", "df"),
    packages = c("brms", "here", "job"),
    title = "model_fit_5",
    model_fit_5 = {
        model_fit_5 <- brm(
            formula = bf(
                response ~ age_std + freq_std + n_phon_std + doe_std + age_std:doe_std +
                    (1 + age_std + freq_std + n_phon_std + doe_std + age_std:doe_std | id) + 
                    (1 + age_std + freq_std + n_phon_std + doe_std + age_std:doe_std | te) ,
                family = cratio(link = "logit") # cumulative, continuation ratio
            ), 
            data = df,
            prior = model_prior,
            sample_prior = "yes", # for faster computation of Bayes Factors and LOO
            iter = 1000,
            chains = 8,
            cores = 8,
            init = 0, # where to initialise MCMCs
            seed = 888, # for reproducibility
            backend = "cmdstanr", # for faster, less problematic compilation in C++
            file = here("results", "fit_5.rds"), # save model as file
            # file_refit = "always", # should model be refitted or loaded from file?
            control = list(
                adapt_delta = 0.9, # for better convergence of MCMCs
                max_treedepth = 15
            ),
            save_model = here("stan", "fit_5.stan") # save Stan code
        )
        export(c(model_fit_5))
    }
)



model_fit_4_prior <- brm(
    formula = bf(
        response ~ age_std + freq_std + n_phon_std + doe_std + 
            (1 + age_std + freq_std + n_phon_std + doe_std | id) + 
            (1 + age_std + freq_std + n_phon_std + doe_std | te) ,
        family = cratio(link = "logit") # cumulative, continuation ratio
    ), 
    data = df,
    prior = model_prior,
    sample_prior = "only", # for faster computation of Bayes Factors and LOO
    iter = 4000,
    chains = 2,
    init = 0, # where to initialise MCMCs
    seed = 888, # for reproducibility
    backend = "cmdstanr", # for faster, less problematic compilation in C++
    file = here("results", "fit_4_prior.rds"), # save model as file
    # file_refit = "always", # should model be refitted or loaded from file?
    control = list(
        adapt_delta = 0.9, # for better convergence of MCMCs
        max_treedepth = 15
    ),
    save_model = here("stan", "fit_4_prior.stan") # save Stan code
)

# diagnose model ----
# see https://mc-stan.org/bayesplot/articles/visual-mcmc-diagnostics.html#general-mcmc-diagnostics-1
post <- gather_draws(
    model_fit_4,
    `b_.*`,
    `sd_.*`,
    `cor_.*`,
    regex = TRUE
) %>% 
    mutate(.chain = as.factor(.chain))

write_ipc_stream(post, here("results", "posterior_draws.parquet"))

# R-hat diagnostic of chain convergence (Gelman-Rubin)
rhats <- model_fit_4 %>% 
    rhat() %>% 
    data.frame(.rhat = .) %>% 
    rownames_to_column(".variable") %>% 
    as_tibble() 

write_ipc_stream(rhats, here("results", "rhats.parquet"))

# effective sample size
neffs <- neff_ratio(model_fit_4) %>% 
    data.frame(.neff = .) %>% 
    rownames_to_column(".variable") %>% 
    as_tibble()

write_ipc_stream(neffs, here("results", "neffs.parquet"))


# coefficients ----
fix_coefs <- post %>% 
    filter(str_detect(.variable, "b_")) %>% 
    mutate(
        .value = case_when(
            str_detect(.variable, "Intercept") ~ inv_logit_scaled(.value),
            TRUE ~ .value/4
        ),
        .variable = .variable %>% 
            factor(
                levels = c(
                    "b_Intercept[2]",
                    "b_Intercept[1]",
                    "b_age_std",
                    "b_freq_std",
                    "b_n_phon_std",
                    "b_doe_std"
                ),
                labels = c(
                    "Intercept 1 (Comprehension)",
                    "Intercept 2 (Production)",
                    "Age (+1 SD)",
                    "Frequency (+1 SD)",
                    "# Phonemes (+1 SD)",
                    "Exposure (+1 SD)"
                )
            )
    ) 

write_ipc_stream(fix_coefs, here("results", "fixed_coefs.parquet"))


# group-level coefficients (SD)
rand_coefs <- post %>% 
    filter(str_detect(.variable, "sd_")) %>% 
    mutate(
        .value = inv_logit_scaled(.value),
        group = ifelse(str_detect(.variable, "_te__"), "TE", "ID"),
        .variable = .variable %>% 
            factor(
                levels = c(
                    "sd_te__Intercept",
                    "sd_te__age_std",
                    "sd_te__freq_std",
                    "sd_te__n_phon_std",
                    "sd_te__doe_std",
                    "sd_id__Intercept",
                    "sd_id__age_std",
                    "sd_id__freq_std",
                    "sd_id__n_phon_std",
                    "sd_id__doe_std"
                ),
                labels = c(
                    "Intercept",
                    "Age (+1 SD)",
                    "Frequency (+1 SD)",
                    "# Phonemes (+1 SD)",
                    "Exposure (+1 SD)",
                    "Intercept",
                    "Age (+1 SD)",
                    "Frequency (+1 SD)",
                    "# Phonemes (+1 SD)",
                    "Exposure (+1 SD)"
                )
            )
    )

write_ipc_stream(rand_coefs, here("results", "rand_coefs.parquet"))

# posterior predictions ----

# expand dataset to get predictions with larger resolution
df_preds <- expand_grid(
    age_std = seq(-4, 4, 0.1),
    freq_std = 0,
    n_phon_std = 0,
    doe_std = c(-1, 0, 1)
) 

# expected posterior predictions 
preds_age <- add_epred_draws(
    newdata = df_preds, 
    object = model_fit_4,
    ndraws = 25,
    re_formula = NA
) %>% 
    pivot_wider(
        names_from = .category,
        values_from = .epred
    ) %>% 
    mutate(
        Understands = Understands + `Understands and Says`
    ) %>% 
    pivot_longer(
        c(No, Understands, `Understands and Says`),
        names_to = ".category",
        values_to = ".epred"
    ) %>% 
    # get curves of interest
    filter(.category %in% c("Understands", "Understands and Says")) %>% 
    # more informative labels
    mutate(
        .category = case_when(
            .category=="Understands" ~ "Comprehension",
            .category=="Understands and Says" ~ "Production"
        ),
        # see R/utils.R
        age = rescale_variable(age_std, mean = mean(df$age), sd = sd(df$age))
    ) 

write_ipc_stream(preds_age, here("results", "posterior_predictions-population.parquet"))

# empirical response cumulative probabilities
aoas <- get_aoa(preds_age, .category, doe_std) %>% 
    mutate(doe_std = paste0(doe_std, " SD")) 

write_ipc_stream(aoas, here("results", "posterior_aoas-population.parquet"))


# predictions by frequency ----
preds_freq <- get_posterior_predictions(
    model_fit_4, 
    freq_std,
    step = 0.1,
    ndraws = 50, 
    re_formula = NA
) %>%
    mutate(
        freq = rescale_variable(
            freq_std,
            mean = mean(df$freq), 
            sd = sd(df$freq)
        )
    ) 


write_ipc_stream(preds_freq, here("results", "posterior_predictions-frequency.parquet"))


# predictions by n_phon ----
preds_n_phon <- get_posterior_predictions(
    model_fit_4, 
    n_phon_std,
    step = 0.5,
    ndraws = 50, 
    re_formula = NA
) %>% 
    mutate(
        n_phon = rescale_variable(
            n_phon_std,
            mean = mean(df$n_phon), 
            sd = sd(df$n_phon)
        )
    )


write_ipc_stream(preds_n_phon, here("results", "posterior_predictions-n_phon.parquet"))

# predictions by doe ----
preds_n_doe <- get_posterior_predictions(
    model_fit_4, 
    doe_std,
    ndraws = 50, 
    step = 0.2,
    re_formula = NA
) %>% 
    mutate(
        doe = rescale_variable(
            doe_std,
            mean = mean(df$doe), 
            sd = sd(df$doe)
        )
    )

write_ipc_stream(preds_n_doe, here("results", "posterior_predictions-doe.parquet"))


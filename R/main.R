library(dplyr)
library(arrow)
# library(brms)
library(tidyr)
library(tidybayes)
library(ggplot2)
library(bayesplot)
library(here)
library(ggsci)
library(ggrepel)
library(stringr)
library(tibble)
library(job)
library(tidytext)
library(scales)
library(patchwork)

df <- read_ipc_stream(here("data", "main.parquet"))
participants <- read_ipc_stream(here("data", "participants.parquet"))
items <- read_ipc_stream(here("data", "items.parquet"))
# model_fit_4 <- readRDS(here("results", "fit_4.rds"))
post <- read_ipc_stream(here("results", "posterior_draws.parquet"))
rhats <- read_ipc_stream(here("results", "rhats.parquet"))
neffs <- read_ipc_stream(here("results", "neffs.parquet"))
fix_coefs <- read_ipc_stream(here("results", "fixed_coefs.parquet"))
rand_coefs <- read_ipc_stream(here("results", "rand_coefs.parquet"))
preds_age <- read_ipc_stream(here("results", "posterior_predictions-population.parquet"))
preds_freq <- read_ipc_stream(here("results", "posterior_predictions-frequency.parquet"))
preds_n_phon <- read_ipc_stream(here("results", "posterior_predictions-n_phon.parquet"))
preds_doe <- read_ipc_stream(here("results", "posterior_predictions-doe.parquet"))
aoas <- read_ipc_stream(here("results", "posterior_aoas-population.parquet"))
wordbank_aoas <- read_ipc_stream(here("data", "wordbank-aoa.parquet"))
preds_age_te <- read_ipc_stream(here("results", "posterior_predictions-groups.parquet"))

source(here("R", "utils.R"))


# participants ----
plot_age <- participants %>% 
    mutate(age = floor(age)) %>% 
    count(lp, age) %>% 
    ggplot() +
    aes(
        x = age,
        y = n,
        fill = lp,
        colour = lp
    ) +
    geom_col() +
    labs(
        x = "Edad (meses)",
        y = "# respuestas",
        colour = "Grupo",
        fill = "Grupo"
    ) +
    scale_fill_d3() +
    scale_color_d3() +
    scale_x_continuous(
        breaks = seq(
            floor(min(participants$age)), 
            floor(max(participants$age)),
            2
        ),
        sec.axis = dup_axis(
            name = "Edad (años)",
            labels = months_to_years(
                seq(
                    floor(min(participants$age)), 
                    floor(max(participants$age)),
                    2
                )
            )
        ) 
    ) +
    theme(
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
        
    ) 

plot_doe <- participants %>% 
    mutate(
        dominant_language = if_else(
            doe_catalan >= doe_spanish,
            "Catalan",
            "Spanish"
        ),
        doe_dominant = if_else(
            dominant_language== "Catalan",
            doe_catalan,
            doe_spanish
        ),
        doe_dominant = cut(
            doe_dominant*100,
            breaks = seq(40, 100, 10), 
            ordered_results = TRUE, 
            right = FALSE,
            include.lowest = TRUE
        ),
        doe_dominant = gsub("([0-9.]+)", "\\1%", doe_dominant)
    ) %>% 
    count(lp, dominant_language, doe_dominant) %>% 
    ggplot() +
    aes(
        x = doe_dominant,
        y = n,
        fill = lp,
        colour = lp
    ) +
    geom_col() +
    labs(
        x = "Exposición acumulada a lengua dominante",
        y = "# respuestas",
        fill = "Grupo",
        colour = "Grupo"
    ) +
    scale_fill_d3() +
    scale_color_d3() 

plot_time <- participants %>% 
    count(lp, time) %>% 
    ggplot() +
    aes(
        x = as.factor(time),
        y = n,
        colour = lp,
        fill = lp
    ) +
    geom_col() +
    labs(
        x = "Orden de respuesta",
        y = "# respuestas",
        colour = "Grupo",
        fill = "Grupo"
    ) +
    scale_color_d3() +
    scale_fill_d3()


plot_age /
    (
        plot_doe + plot_time +
            plot_layout(
                ncol = 2,
                widths = c(0.6, 0.4)
            )
    ) +
    plot_layout(
        guides = "collect"
        
    ) &
    theme_custom() &
    theme(
        legend.position = "right",
        legend.title = element_blank()
    )

ggsave(here("img", "participants.png"), width = 13, height = 5)



# model prior
model_prior <- c(
    prior(normal(-0.25, 0.1), class = "Intercept"),
    prior(normal(1, 0.1), class = "sd", group = "te"),
    prior(normal(1, 0.1), class = "sd", group = "id"),
    prior(lkj(2), class = "cor"),
    prior(normal(1, 0.1), class = "b", coef = "age_std"),
    prior(normal(0, 0.1), class = "b", coef = "freq_std"),
    prior(normal(0, 0.1), class = "b", coef = "n_phon_std"),
    prior(normal(0, 0.1), class = "b", coef = "doe_std")
    
)

ggsave(here("img", "participants.png"))

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

#### aoa wordbank ----
aoa_summary <- wordbank_aoas %>% 
    group_by(measure) %>% 
    summarise(
        mean_aoa = mean(aoa),
        sd_aoa = sd(aoa),
        .groups = "drop"
    ) 

wordbank_aoas %>% 
    mutate(language_reordered = reorder_within(language, aoa, measure)) %>% 
    ggplot() +
    aes(
        x = aoa,
        y = reorder(language_reordered, aoa),
        xmin = .lower,
        xmax = .upper
    ) +
    facet_wrap(~measure, scales = "free_y") +
    geom_interval() +
    geom_rect(
        data = aoa_summary,
        aes(
            xmin = mean_aoa - sd_aoa,
            xmax = mean_aoa + sd_aoa,
            ymin = -Inf,
            ymax = Inf,
        ) ,
        fill = pal_d3()(2)[2],
        alpha = 0.5,
        inherit.aes = FALSE
    ) +
    geom_point(
        shape = 4,
        size = 2,
        stroke = 0.75,
        colour = "white"
    ) +
    geom_vline(
        data = aoa_summary,
        aes(
            xintercept = mean_aoa,
        ),
        size = 0.75,
        colour = pal_d3()(2)[2]
    ) +
    labs(
        x = "Word age of acquisition (acquired by 50% of sample)",
        y = "Language",
        colour = "Percentile",
        fill = "Percentile",
        title = "Distribution of ages of acquisition across languages",
        subtitle = "Orange lines and intervals indicate grand mean and SD",
        caption = "Source: Wordbank (see https://mikabr.io/aoa-prediction/aoa_estimation.html)"
    ) +
    scale_color_brewer() +
    scale_y_reordered() +
    theme(
        legend.position = "top",
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0)
    )

ggsave(here("img", "wordbank-aoas.png"))


#### observed responses ----
df %>% 
    mutate(
        age = cut(age, seq(10, 36, 2), include.lowest = TRUE),
        doe = cut(doe, seq(0, 1, 0.25), include.lowest = TRUE)
    ) %>% 
    count(age, doe, response) %>% 
    rename(`Language exposure` = doe) %>% 
    ggplot() +
    aes(
        age,
        n,
        fill = response
    ) +
    facet_wrap(
        ~`Language exposure`,
        labeller = label_both
    ) +
    geom_col(
        position = position_fill()
    ) +
    geom_hline(
        yintercept = 0.5,
        size = 1,
        colour = "grey"
    ) +
    labs(
        x = "Age (months)",
        y = "% responses",
        fill = "Response"
    ) +
    scale_fill_d3() +
    scale_y_continuous(labels = percent) +
    scale_x_discrete(
        guide = guide_axis(n.dodge = 2)
    ) +
    theme(
        legend.position = "top",
        panel.grid = element_blank()
    )


ggsave(here("img", "observed-responses.png"))


#### posterior distribution ----
fix_coefs %>% 
    ggplot() +
    aes(
        x = .value,
        y = reorder(.variable, desc(.variable))
    ) +
    geom_vline(
        xintercept = 0,
        size = 1,
        colour = "grey"
    ) +
    stat_slab(
        aes(
            fill = stat(cut_cdf_qi(cdf, labels = percent))
        ),
        colour = "white"
    ) +
    stat_pointinterval(
        colour = "black"
    ) +
    scale_fill_manual(
        values = c(pal_d3()(1)[1], "#57a9e2", "#aed6f1")
    ) +
    labs(
        x = "Sampling space",
        y = "Posterior likelihood density",
        fill = "Credible Interval"
    ) +
    scale_x_continuous(
        limits = c(-0.2, 1),
        labels = percent
    ) +
    theme(
        legend.position = "top",
        panel.grid.major.y = element_line(colour = "grey"),
        axis.title.y = element_blank()
    )

ggsave(here("img", "posterior_distribution.png"))

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
    ndraws = 50,
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
aoas <- get_aoa(preds, .category, doe_std) %>%
    mutate(doe_std = paste0(doe_std, " SD"))


observations <- df %>%
    mutate(
        doe_std = cut(
            doe_std,
            c(-1, -0.5, 0.5, 1.5),
            labels = c("-1 SD", "0 SD", "1 SD")
        ),
        age_std = cut(
            age_std,
            breaks = unique(preds$age_std),
            labels = unique(preds$age_std)[-1]
        ) %>%
            as.character() %>%
            as.numeric()
    ) %>%
    drop_na(doe_std) %>%
    group_by(doe_std, age_std) %>%
    summarise(
        yes_Production = sum(response=="Understands and Says"),
        yes_Comprehension = sum(response %in% c("Understands", "Understands and Says")),
        n = n(),
        .groups = "drop"
    ) %>%
    pivot_longer(
        starts_with("yes"),
        names_to = ".category",
        values_to = "sum",
        names_prefix = "yes_"
    ) %>%
    mutate(
        prop = prop_adj(sum, n),
        prop_se = prop_adj_se(sum, n)
    ) %>%
    # group_by(age_std, doe_std, .category) %>%
    # summarise(
    #     prop = mean(prop),
    #     n = n(),
    #     .groups = "drop"
    # ) %>%
    mutate(
        age = rescale_variable(
            age_std,
            mean = mean(df$age),
            sd = sd(df$age)
        )
    )


plot_curves <- preds_age %>%
    mutate(doe_std = paste0(doe_std, " SD")) %>%
    ggplot() +
    aes(
        x = age,
        y = .epred,
        colour = doe_std,
        fill = doe_std,
        shape = .category,
        linetype = .category,
        group = interaction(doe_std, .category)
    ) +
    # linea de referencia (50% suele ser considerado el punto de adquisición)
    geom_hline(
        yintercept = 0.5,
        colour = "grey",
        alpha = 1
    ) +
    # posterior predictions for each individual posterior draw
    # stat_summary(
    #     fun.data = mean_qi,
    #     geom = "ribbon",
    #     colour = NA,
    #     alpha = 0.5
    # ) +
    geom_line(
        aes(group = interaction(.draw, doe_std, .category , sep = " / ")),
        alpha = 0.10,
        size = 0.8,
        linetype = "solid"
    ) +
    stat_summary(
        fun = mean,
        geom = "line",
        size  = 1
    ) +
    # geom_point(
    #     data = observations,
    #     aes(y = prop),
    #     size = 2,
    #     alpha = 0.5
    # ) +
    # geom_line(
    #     data = observations,
    #     aes(y = prop, group = interaction(.category, doe_std)),
    #     size = 0.5,
    #     alpha = 0.5
# ) +
labs(
    x = "Age (months)",
    y = "P(acquisition | age, dominance)",
    colour = "Language exposure",
    fill = "Language exposure",
    shape = "Response",
    linetype = "Response",
    title = "Acquisition curves",
    subtitle = "Each line corresponds to a posterior prediction"
) +
    scale_color_d3() +
    scale_fill_d3() +
    scale_y_continuous(
        labels = percent
    ) +
    scale_x_continuous(
        breaks = seq(0, 45, 5)
    ) +
    theme(
        panel.grid.major.x = element_line(colour = "grey", linetype = "dotted"),
        axis.text.x.top = element_text(),
        axis.title.x.top = element_text(colour = "top"),
        axis.ticks.x.top = element_line(),
        axis.title.x.bottom = element_blank(),
        axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        axis.line.x = element_blank()
    )

plot_aoas <- aoas %>%
    filter(.category != "No") %>%
    ggplot() +
    aes(
        x = aoa,
        y = 0,
        colour = doe_std,
        shape = .category
    ) +
    annotate(
        geom = "text",
        x = c(7, 40),
        y = -0.1,
        vjust = -1,
        hjust = c(1, 0),
        label = c("Acquired earlier", "Acquired later")
    ) +
    annotate(
        geom = "segment",
        x = c(7, 40),
        xend = c(0, 45),
        y = -0.1,
        yend = -0.1,
        arrow = arrow(angle = 30, length = unit(0.2, "cm"))
    ) +
    stat_pointinterval(
        position = position_dodge(width = 0.5),
        point_interval = mean_qi
    ) +
    labs(
        x = "Age (months)",
        y = "Response",
        title = "Age of acquisition",
        shape = "Response",
        linetype = "Response"
    ) +
    guides(colour = "none") +
    scale_x_continuous(
        limits = c(0, 45),
        breaks = seq(0, 45, 5)
    ) +
    scale_color_d3() +
    scale_fill_d3() +
    theme(
        legend.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    )

plot_curves +
    plot_aoas +
    plot_layout(
        ncol = 1,
        heights = c(0.8, 0.2),
        guides = "collect"
    ) &
    plot_annotation(
        tag_levels = "A"
    ) &
    theme(
        legend.position = "right"
    )

ggsave(here("img", "posterior_predictions.png"), height = 6, width = 10)

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


preds_freq_observed <- df %>%
    left_join(select(items, item, ipa_flat)) %>% 
    filter(
        between(age_std, -0.5, 0.5),
        between(doe_std, -0.5, 0.5),
        item %in% c(
            "spa_casa",
            "spa_jirafa",
            "cat_girafa",
            "spa_silla",
            "cat_planta",
            "spa_cangrejo",
            "cat_fatigos"
        )
    ) %>%
    group_by(item, ipa_flat, freq) %>% 
    summarise(
        yes_Production = sum(response=="Understands and Says"),
        yes_Comprehension = sum(response %in% c("Understands", "Understands and Says")),
        n = n(),
        .groups = "drop"
    ) %>% 
    pivot_longer(
        starts_with("yes"),
        names_to = ".category",
        values_to = "sum",
        names_prefix = "yes_"
    ) %>% 
    mutate(
        prop = prop_adj(sum, n),
        prop_se = prop_adj_se(sum, n)
    )

plot_preds_freq <- preds_freq %>% 
    ggplot()+
    aes(
        x = freq,
        y = .epred,
        colour = .category
    ) +
    geom_line(
        aes(group = interaction(.draw, .category)),
        alpha = 0.15
    ) +
    stat_summary(
        aes(group = interaction(.category)),
        fun = mean,
        geom = "line",
        size = 3,
        alpha = 0.75,
        colour = "white"
    ) +
    stat_summary(
        aes(group = interaction(.category)),
        fun = mean,
        geom = "line",
        size = 1
    ) +
    # geom_label_repel(
    #     data = preds_freq_observed %>%
    #         group_by(.category, te, item, ipa_flat, freq) %>%
    #         summarise(
    #             prop = max(prop),
    #             .groups = "drop"
    #         ),
    #     box.padding = 1,
    #     min.segment.length = 10,
    #     aes(
    #         y = prop,
#         label = paste0("/", ipa_flat, "/")
#     ),
#     size = 5,
#     alpha = 0.65,
#     label.size = 0,
#     label.r = 0,
#     fill = "white"
# ) +
geom_label(
    data = preds_freq_observed %>%
        group_by(.category, item, ipa_flat, freq) %>%
        summarise(
            prop = max(prop),
            .groups = "drop"
        ),       
    aes(
        y = prop,
        label = paste0("/", ipa_flat, "/")
    ),
    size = 4,
    fill = "white",
    alpha = 0.65,
    label.size = 0,
    show.legend = FALSE,
    label.r = unit(0, "lines")
) +
    labs(
        x = "Frequency (Zipf score)",
        y = "P(acquisition | frequency)",
        colour = "Category",
        fill = "Category"
        
    ) +
    scale_color_d3() +
    scale_y_continuous(
        labels = percent,
        limits = c(0, 1)
    ) +
    theme(
        legend.title = element_blank()
    )


preds_n_phon_observed <- df %>%
    left_join(select(items, item, ipa_flat)) %>% 
    filter(
        item %in% c(
            "spa_sol",
            "spa_vaso",
            "spa_camiseta",
            "cat_tovalloletes",
            "cat_excavadora",
            "cat_cremallera"
        )
    ) %>% 
    group_by(item, ipa_flat, n_phon) %>% 
    summarise(
        yes_Production = sum(response=="Understands and Says"),
        yes_Comprehension = sum(response %in% c("Understands", "Understands and Says")),
        n = n(),
        .groups = "drop"
    ) %>% 
    pivot_longer(
        starts_with("yes"),
        names_to = ".category",
        values_to = "sum",
        names_prefix = "yes_"
    ) %>% 
    mutate(
        prop = prop_adj(sum, n),
        prop_se = prop_adj_se(sum, n)
    )

plot_preds_n_phon <- preds_n_phon %>% 
    ggplot()+
    aes(
        x = n_phon,
        y = .epred,
        colour = .category
    ) +
    geom_line(
        aes(group = interaction(.draw, .category)),
        alpha = 0.15
    ) +
    stat_summary(
        aes(group = interaction(.category)),
        fun = mean,
        geom = "line",
        size = 3,
        alpha = 0.75,
        colour = "white"
    ) +
    stat_summary(
        aes(group = .category),
        fun = mean,
        geom = "line",
        size = 1
    ) +
    geom_label(
        data = preds_n_phon_observed %>%
            group_by(.category, item, ipa_flat, n_phon) %>%
            summarise(
                prop = max(prop),
                .groups = "drop"
            ),       
        aes(
            y = prop,
            label = paste0("/", ipa_flat, "/")
        ),
        size = 4,
        fill = "white",
        alpha = 0.65,
        label.size = 0,
        show.legend = FALSE,
        label.r = unit(0, "lines")
    ) +
    labs(
        x = "# phonemes",
        y = "P(acquisition | # phonemes)",
        colour = "Category",
        fill = "Category"
    ) +
    scale_color_d3() +
    scale_y_continuous(
        labels = percent,
        limits = c(0, 1)
    ) +
    scale_x_continuous(
        breaks = seq(2, 14, 2)
    ) +
    theme(
        legend.title = element_blank()
    )


preds_doe_observed <- df %>%
    left_join(select(items, item, ipa_flat)) %>% 
    filter(
        item %in% c(
            "spa_mesa"
        )
    ) %>% 
    group_by(item, ipa_flat, doe) %>% 
    summarise(
        yes_Production = sum(response=="Understands and Says"),
        yes_Comprehension = sum(response %in% c("Understands", "Understands and Says")),
        n = n(),
        .groups = "drop"
    ) %>% 
    pivot_longer(
        starts_with("yes"),
        names_to = ".category",
        values_to = "sum",
        names_prefix = "yes_"
    ) %>% 
    mutate(
        prop = prop_adj(sum, n),
        prop_se = prop_adj_se(sum, n)
    )

plot_preds_doe <- preds_doe %>% 
    ggplot()+
    aes(
        x = doe,
        y = .epred,
        colour = .category
    ) +
    geom_line(
        aes(group = interaction(.draw, .category)),
        alpha = 0.15
    ) +
    stat_summary(
        aes(group = .category),
        fun = mean,
        geom = "line",
        size = 3,
        alpha = 0.75,
        colour = "white"
    ) +
    stat_summary(
        aes(group = .category),
        fun = mean,
        geom = "line",
        size = 1
    ) +
    geom_line(
        data = preds_doe_observed,
        aes(
            y = prop,
            group = interaction(.category, item)
        ),
        size = 0.5,
        alpha = 0.5
    ) +
    geom_label(
        data = preds_doe_observed %>%
            group_by(.category, item, ipa_flat, doe) %>%
            summarise(
                prop = max(prop),
                .groups = "drop"
            ),       
        aes(
            x = 0.8,
            y = ifelse(.category=="Comprehension", 0.80, 0.25),
            label = paste0("/", ipa_flat, "/")
        ),
        show.legend = FALSE,
        size = 4,
        fill = "white",
        alpha = 0.65,
        label.size = 0,
        label.r = unit(0, "lines")
    ) +    
    labs(
        x = "Language exposure",
        y = "P(acquisition | % language exposre)",
        colour = "Category",
        fill = "Category"
    ) +
    scale_color_d3() +
    scale_y_continuous(
        labels = percent,
        limits = c(0, 1)
    ) +
    scale_x_continuous(
        breaks = seq(min(df$doe), max(df$doe), 0.25),
        labels = percent
    ) +
    theme(
        legend.title = element_blank(),
        panel.grid.minor = element_blank()
    )


plot_preds_freq +
    plot_preds_n_phon +
    plot_preds_doe +
    plot_layout(
        guides = "collect"
    ) &
    theme(
        legend.position = "top"
    )


ggsave(here("img", "posterior_conditional-effects.png"), width = 12, height = 6)

# expected posterior predictions by (te)

items_interest  <- c(
    "spa_casa", 
    "spa_gato", 
    "cat_casa", 
    "cat_gat",
    "spa_cocodrilo",
    "cat_cocodrilo",
    "spa_mano",
    "cat_ma2"
    ) 

df_preds_te <- expand_grid(
    age_std = seq(-4, 4, 0.1),
    doe_std = c(-1, 0, 1),
    te = unique(df$te[df$item %in% items_interest])
) %>% 
    left_join(
        distinct(df, te, item, freq_std, n_phon_std) %>% 
            filter(te %in%  df$te[df$item %in% items_interest]
            )
    )

preds_age_te <- add_epred_draws(
    newdata = df_preds_te, 
    object = model_fit_4,
    ndraws = 25,
    re_formula = ~ 1 | te
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
    ) %>% 
    left_join(
        distinct(df, te, item, n_phon, freq) %>% 
            filter(te %in%  df$te[df$item %in% items_interest]
            )
    )


write_ipc_stream(preds_age_te, here("results", "posterior_predictions-groups.parquet"))


aoas_te <- get_aoa(preds_age_te, .category, doe_std, te, item, freq, n_phon) %>% 
    mutate(doe_std = paste0(doe_std, " SD")) 

plot_curves_te <- preds_age_te %>%
    filter(te %in% items$te[items$item=="cat_casa"]) %>% 
    mutate(
        doe_std = paste0(doe_std, " SD"),
        item = paste0(
            item, 
            " / freq = ", round(freq, 2),
            " / n_phon = ", n_phon
        ) 
    ) %>% 
    ggplot() +
    facet_wrap(~item) +
    aes(
        x = age,
        y = .epred,
        colour = doe_std,
        fill = doe_std,
        linetype = .category
    ) +
    geom_line(
        aes(group = interaction(.draw, doe_std, .category , sep = " / ")),
        alpha = 0.10,
        size = 0.8,
        linetype = "solid"
    ) +
    stat_summary(
        fun = mean,
        geom = "line",
        colour = "white",
        aes(group = interaction(doe_std, .category)),
        size = 1,
        linetype = "solid"
    ) +
    stat_summary(
        geom = "line",
        fun = mean,
        size = 0.95,
    ) +
    labs(
        x = "Age (months)",
        y = "Posterior P(Acquisition | model)",
        colour = "Language exposure",
        fill = "Language exposure"
    ) +
    scale_color_d3() +
    scale_y_continuous(labels = scales::percent) +
    guides(linetype = guide_legend(title = NULL)) +
    theme(
        legend.position = "top"
    )

plot_aoas_te <- aoas_te %>%
    filter(te %in% items$te[items$item=="cat_casa"]) %>% 
    filter(.category != "No") %>%
    ggplot() +
    aes(
        x = aoa,
        y = 0,
        colour = doe_std,
        shape = .category
    ) +
    facet_wrap(~item) +
    stat_pointinterval(
        position = position_dodge(width = 0.5),
        point_interval = mean_qi
    ) +
    labs(
        x = "Age (months)",
        y = "Response",
        title = "Age of acquisition",
        shape = "Response",
        linetype = "Response"
    ) +
    guides(colour = "none") +
    scale_x_continuous(
        limits = c(0, 45),
        breaks = seq(0, 45, 5)
    ) +
    scale_color_d3() +
    scale_fill_d3() +
    theme(
        legend.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    )

plot_curves_te +
    plot_aoas_te +
    plot_layout(
        ncol = 1,
        heights = c(0.8, 0.2),
        guides = "collect"
    ) &
    plot_annotation(
        tag_levels = "A"
    ) &
    theme(
        legend.position = "right"
    )

ggsave(here("img", "posterior_predictions-group_casa.png"), width = 12, height = 6)

# gato ----
aoas_te <- get_aoa(preds_age_te, .category, doe_std, te, item, freq, n_phon) %>% 
    mutate(doe_std = paste0(doe_std, " SD")) 

plot_curves_te <- preds_age_te %>%
    filter(te %in% items$te[items$item=="cat_gat"]) %>% 
    mutate(
        doe_std = paste0(doe_std, " SD"),
        item = paste0(
            item, 
            " / freq = ", round(freq, 2),
            " / n_phon = ", n_phon
        ) 
    ) %>% 
    ggplot() +
    facet_wrap(~item) +
    aes(
        x = age,
        y = .epred,
        colour = doe_std,
        fill = doe_std,
        linetype = .category
    ) +
    geom_line(
        aes(group = interaction(.draw, doe_std, .category , sep = " / ")),
        alpha = 0.10,
        size = 0.8,
        linetype = "solid"
    ) +
    stat_summary(
        fun = mean,
        geom = "line",
        colour = "white",
        aes(group = interaction(doe_std, .category)),
        size = 1,
        linetype = "solid"
    ) +
    stat_summary(
        geom = "line",
        fun = mean,
        size = 0.95,
    ) +
    labs(
        x = "Age (months)",
        y = "Posterior P(Acquisition | model)",
        colour = "Language exposure",
        fill = "Language exposure"
    ) +
    scale_color_d3() +
    scale_y_continuous(labels = scales::percent) +
    guides(linetype = guide_legend(title = NULL)) +
    theme(
        legend.position = "top"
    )

plot_aoas_te <- aoas_te %>%
    filter(te %in% items$te[items$item=="cat_gat"]) %>% 
    filter(.category != "No") %>%
    ggplot() +
    aes(
        x = aoa,
        y = 0,
        colour = doe_std,
        shape = .category
    ) +
    facet_wrap(~item) +
    stat_pointinterval(
        position = position_dodge(width = 0.5),
        point_interval = mean_qi
    ) +
    labs(
        x = "Age (months)",
        y = "Response",
        title = "Age of acquisition",
        shape = "Response",
        linetype = "Response"
    ) +
    guides(colour = "none") +
    scale_x_continuous(
        limits = c(0, 45),
        breaks = seq(0, 45, 5)
    ) +
    scale_color_d3() +
    scale_fill_d3() +
    theme(
        legend.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    )

plot_curves_te +
    plot_aoas_te +
    plot_layout(
        ncol = 1,
        heights = c(0.8, 0.2),
        guides = "collect"
    ) &
    plot_annotation(
        tag_levels = "A"
    ) &
    theme(
        legend.position = "right"
    )

ggsave(here("img", "posterior_predictions-group_gato.png"), width = 12, height = 6)


plot_curves_te <- preds_age_te %>%
    filter(te %in% items$te[items$item=="spa_cocodrilo"]) %>% 
    mutate(
        doe_std = paste0(doe_std, " SD"),
        item = paste0(
            item, 
            " / freq = ", round(freq, 2),
            " / n_phon = ", n_phon
        ) 
    ) %>% 
    ggplot() +
    facet_wrap(~item) +
    aes(
        x = age,
        y = .epred,
        colour = doe_std,
        fill = doe_std,
        linetype = .category
    ) +
    geom_line(
        aes(group = interaction(.draw, doe_std, .category , sep = " / ")),
        alpha = 0.10,
        size = 0.8,
        linetype = "solid"
    ) +
    stat_summary(
        fun = mean,
        geom = "line",
        colour = "white",
        aes(group = interaction(doe_std, .category)),
        size = 1,
        linetype = "solid"
    ) +
    stat_summary(
        geom = "line",
        fun = mean,
        size = 0.95,
    ) +
    labs(
        x = "Age (months)",
        y = "Posterior P(Acquisition | model)",
        colour = "Language exposure",
        fill = "Language exposure"
    ) +
    scale_color_d3() +
    scale_y_continuous(labels = scales::percent) +
    guides(linetype = guide_legend(title = NULL)) +
    theme(
        legend.position = "top"
    )

plot_aoas_te <- aoas_te %>%
    filter(te %in% items$te[items$item=="spa_cocodrilo"]) %>% 
    filter(.category != "No") %>%
    ggplot() +
    aes(
        x = aoa,
        y = 0,
        colour = doe_std,
        shape = .category
    ) +
    facet_wrap(~item) +
    stat_pointinterval(
        position = position_dodge(width = 0.5),
        point_interval = mean_qi
    ) +
    labs(
        x = "Age (months)",
        y = "Response",
        title = "Age of acquisition",
        shape = "Response",
        linetype = "Response"
    ) +
    guides(colour = "none") +
    scale_x_continuous(
        limits = c(0, 45),
        breaks = seq(0, 45, 5)
    ) +
    scale_color_d3() +
    scale_fill_d3() +
    theme(
        legend.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    )

plot_curves_te +
    plot_aoas_te +
    plot_layout(
        ncol = 1,
        heights = c(0.8, 0.2),
        guides = "collect"
    ) &
    plot_annotation(
        tag_levels = "A"
    ) &
    theme(
        legend.position = "right"
    )

ggsave(here("img", "posterior_predictions-group_cocodrilo.png"), width = 12, height = 6)


#### convergencia de cadenas ----

post %>% 
    mutate(
        .variable_name = str_replace_all(
            .variable,
            c(
                "b_" = "\u03b2: ",
                "sd_te__" = paste0("TE \u03c3: "),
                "sd_id__" = paste0("ID \u03c3: "),
                "cor_te__" = paste0("TE \u03c1: "),
                "cor_id__" = paste0("ID \u03c1: "),
                "__" = " \u00d7 "
            )
            
        )
    ) %>% 
    ggplot() +
    aes(
        x = .iteration,
        y = .value,
        colour = .chain
    ) +
    facet_wrap(
        ~.variable_name,
        scales = "free_y"
    ) + 
    geom_line() +
    labs(
        x = "Iteration",
        y = "Value",
        colour = "Chain"
    ) +
    scale_color_d3()  +
    scale_x_continuous(
        labels = function(x) format(x, big.mark = ",")
    ) +
    theme(
        legend.position = "top",
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 6)
    )


ggsave(here("img", "diagnostics_traceplots.png"))


#### rhats ----
rhats %>% 
    ggplot() +
    aes(
        x = .rhat
    ) +
    # geom_vline(xintercept = 1.1, colour = "grey") +
    geom_histogram(
        fill = pal_d3()(3)[1],
        colour = "white"
    ) +
    scale_y_continuous(
        labels = function(x) format(x, big.mark = ",")
    ) +
    labs(
        title = "Chain convergence",
        subtitle = "Values should not be larger than 1.1",
        x = "Gelman-Rubin statistic (R-hat)",
        y = "Number of samples"
    ) 

ggsave(here("img", "diagnostics_rhats.png"))


#### neffs ----

neffs %>% 
    ggplot() +
    aes(
        x = .neff
    ) +
    geom_histogram(
        fill = pal_d3()(1)[1],
        colour = "white"
    ) +
    geom_vline(
        xintercept = 1,
        colour = pal_d3()(2)[2],
        size = 1
    ) +
    labs(
        title = "Effective sample size",
        subtitle = "Overall, values should be larger than 1",
        x = "Effective sample size ratio (N eff.)",
        y = "Number of samples"
    ) +
    scale_y_continuous(
        labels = function(x) format(x, big.mark = ",")
    ) +
    theme(
        axis.title.y = element_blank()
    ) 

ggsave(here("img", "diagnostics_neffs.png"))





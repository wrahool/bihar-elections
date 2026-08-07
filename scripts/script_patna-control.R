library(glue)
library(tidyverse)
library(MatchIt)
library(cobalt)
library(modelsummary)
library(tidymodels)
library(effsize)
library(pandoc)
library(patchwork)
library(estimatr)

rescale0to1 <- function(x) {
  rng <- range(x, na.rm = TRUE)   # compute min and max ignoring NAs
  (x - rng[1]) / (rng[2] - rng[1])
}

data_folder <- ".//data//"

survey_dat <- read_csv(glue('{data_folder}//clean-merged-survey-data-revised.csv'))

# correct data entry errors
survey_dat <- survey_dat |>
  mutate(Constituency = case_when(
    Constituency == 'Valmiki Nagar' ~ 'Valmikinagar',
    Constituency == 'Narkatiya' ~ 'Narkatia',
    TRUE ~ Constituency
  ))

votes_dat <- read_csv(glue('{data_folder}//votes-data.csv'))

# there are two Kalyanpurs. Remove the Kalyanpur from Samastipur district
# because there's no one in the survey from this Kalyanpur
# and we want to match the survey data with the correct Kalyanpur in the votes data
# same with Pipra, Purba Champaran
votes_dat <- votes_dat |>
  filter(!(`AC Name` == "Kalyanpur" & District == "Samastipur")) |>
  filter(!(`AC Name` == "Pipra" & District == "Purba Champaran"))

merged_dat <- survey_dat |>
  left_join(votes_dat, by = c('Constituency' = 'AC Name'))

replace_with <- NA

merged_dat <- merged_dat |>
  mutate(RecirculatedNDA = ifelse(RecirculatedNDA %in% c(88, 99), replace_with, RecirculatedNDA),
         RecirculatedUPA = ifelse(RecirculatedUPA %in% c(88, 99), replace_with, RecirculatedUPA),
         CreatedNDA = ifelse(CreatedNDA %in% c(88, 99), replace_with, CreatedNDA),
         CreatedUPA = ifelse(CreatedUPA %in% c(88, 99), replace_with, CreatedUPA),
         CommentedNDA = ifelse(CommentedNDA %in% c(88, 99), replace_with, CommentedNDA),
         CommentedUPA = ifelse(CommentedUPA %in% c(88, 99), replace_with, CommentedUPA),
         PostedNDA = ifelse(PostedNDA %in% c(88, 99), replace_with, PostedNDA),
         PostedUPA = ifelse(PostedUPA %in% c(88, 99), replace_with, PostedUPA),
         RalliesNDA = ifelse(RalliesNDA %in% c(88, 99), replace_with, RalliesNDA),
         RalliesUPA = ifelse(RalliesUPA %in% c(88, 99), replace_with, RalliesUPA)
         # MeetingsNDA = ifelse(MeetingsNDA %in% c(88, 99), replace_with, MeetingsNDA),
         # MeetingsUPA = ifelse(MeetingsUPA %in% c(88, 99), replace_with, MeetingsUPA)
  )

# Impute missing DV values

vars <- c(
  "RecirculatedNDA", "RecirculatedUPA",
  "CreatedNDA", "CreatedUPA",
  "CommentedNDA", "CommentedUPA",
  "PostedNDA", "PostedUPA",
  "RalliesNDA", "RalliesUPA"
  # "MeetingsNDA", "MeetingsUPA"
)

imputed_means <- lapply(vars, function(v) {
  flooded <- merged_dat %>%
    filter(Flooding == 1) %>%
    summarize(mean_value = mean(.data[[v]], na.rm = TRUE)) %>%
    pull(mean_value)
  
  nonflooded <- merged_dat %>%
    filter(Flooding == 0) %>%
    summarize(mean_value = mean(.data[[v]], na.rm = TRUE)) %>%
    pull(mean_value)
  
  tibble(variable = v,
         mean_flooded = flooded,
         mean_nonflooded = nonflooded)
}) %>%
  bind_rows()

merged_dat <- merged_dat %>%
  group_by(Flooding) %>%
  mutate(across(all_of(vars),
                ~ ifelse(is.na(.x),
                         mean(.x, na.rm = TRUE),
                         # 1,
                         .x))) %>%
  ungroup()


# plot descriptives

plot_dv <- function(data, base_var) {
  var1 <- paste0(base_var, "NDA")
  var2 <- paste0(base_var, "UPA")
  
  long_dat <- data %>%
    select(Flooding, all_of(c(var1, var2))) %>%
    pivot_longer(cols = c(all_of(var1), all_of(var2)),
                 names_to = "Variable",
                 values_to = "Value")
  
  summary_dat <- long_dat %>%
    group_by(Variable, Flooding) %>%
    summarise(
      mean_y = mean(Value, na.rm = TRUE),
      sd_y   = sd(Value, na.rm = TRUE),
      n      = n(),
      se_y   = sd_y / sqrt(n),
      .groups = "drop"
    ) %>%
    mutate(Variable = gsub("^.*?(NDA|UPA)$", "\\1", Variable))
  
  ggplot(summary_dat, aes(x = Variable,
                          y = mean_y,
                          fill = factor(Flooding))) +
    geom_col(position = position_dodge(width = 0.8), width = 0.6) +
    geom_errorbar(aes(ymin = mean_y - se_y, ymax = mean_y + se_y),
                  position = position_dodge(width = 0.8),
                  width = 0.2) +
    labs(
      x = NULL,
      y = paste("Mean", base_var),
      fill = "Flooding"
    ) +
    theme_bw() +
    scale_x_discrete(labels = c("NDA" = "in support of NDA", "UPA" = "in support of MGB")) +
    scale_fill_manual(values = c("0" = "#1f78b4", "1" = "#b2df8a"),
                      labels = c("0" = "No Flooding", "1" = "Flooding"))
}

vars <- c("Recirculated", "Created", "Commented", "Posted", "Rallies")

plots <- lapply(vars, function(v) plot_dv(merged_dat, v))

# Arrange in a grid, 2 per row, with a single shared legend
final_plot <- wrap_plots(plots, ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")  # put legend below

ggsave("figures/descriptives-new.svg",
       plot = final_plot,
       width = 6,
       height = 6,
       dpi = 300, device = "svg")


# Propensity Score Matching

# group columns thematically

demographic_cols <- c("Age", "Sex", "Caste", "Religion", "HouseholdIncome")

past_voting_cols <- c("VotedLokSabha2019", "VotedAssembly2015")

reqd_covariates <- c(demographic_cols, past_voting_cols,
                     "OrganisationMember", "UsedPhone")



# Create the formula dynamically
formula <- as.formula(paste("Flooding ~", paste(reqd_covariates, collapse = " + ")))

# Perform propensity score matching

match_model <- matchit(formula, 
                       data = merged_dat, 
                       method = "cardinality") # this gives the best covariate balance


# balance plot
svg("figures/balance_plot-new.svg", width = 8, height = 8)
plot(summary(match_model))
dev.off()

summary(match_model)

matched <- match.data(match_model)

# effect of flooding on participation

matched <- matched |> 
  mutate(
    total_participation = RecirculatedNDA + RecirculatedUPA +
      CreatedNDA + CreatedUPA +
      CommentedNDA + CommentedUPA +
      PostedNDA + PostedUPA +
      RalliesNDA + RalliesUPA,
    # MeetingsNDA + MeetingsUPA,
    
    NDA_participation = RecirculatedNDA + CreatedNDA +
      CommentedNDA + PostedNDA +
      RalliesNDA, # + MeetingsNDA,
    
    UPA_participation = RecirculatedUPA + CreatedUPA +
      CommentedUPA + PostedUPA +
      RalliesUPA, # + MeetingsUPA,
    
    total_recirculated = RecirculatedNDA + RecirculatedUPA,
    total_created = CreatedNDA + CreatedUPA,
    total_commented = CommentedNDA + CommentedUPA,
    total_posted = PostedNDA + PostedUPA,
    total_rallies = RalliesNDA + RalliesUPA
    # total_meetings = MeetingsNDA + MeetingsUPA
  )

# Regressions are run with match.data()'s `weights` column and HC3 robust SEs.
# Cardinality matching (no mahvars) doesn't form pairs/subclasses, so
# cluster-robust SEs aren't needed -- just robust (see MatchIt's
# "Estimating Effects After Matching" vignette / ?MatchIt::method_cardinality).

print(summary(lm_robust(rescale0to1(total_participation) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))
print(summary(lm_robust(rescale0to1(NDA_participation) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))
print(summary(lm_robust(rescale0to1(UPA_participation) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))

print(summary(lm_robust(rescale0to1(total_recirculated) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))
print(summary(lm_robust(rescale0to1(RecirculatedNDA) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))
print(summary(lm_robust(rescale0to1(RecirculatedUPA) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))

print(summary(lm_robust(rescale0to1(total_created) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))
print(summary(lm_robust(rescale0to1(CreatedNDA) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))
print(summary(lm_robust(rescale0to1(CreatedUPA) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))

print(summary(lm_robust(rescale0to1(total_commented) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))
print(summary(lm_robust(rescale0to1(CommentedNDA) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))
print(summary(lm_robust(rescale0to1(CommentedUPA) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))

print(summary(lm_robust(rescale0to1(total_posted) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))
print(summary(lm_robust(rescale0to1(PostedNDA) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))
print(summary(lm_robust(rescale0to1(PostedUPA) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))

print(summary(lm_robust(rescale0to1(total_rallies) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))
print(summary(lm_robust(rescale0to1(RalliesNDA) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))
print(summary(lm_robust(rescale0to1(RalliesUPA) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))

# print(summary(lm_robust(rescale0to1(total_meetings) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))
# print(summary(lm_robust(rescale0to1(MeetingsNDA) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))
# print(summary(lm_robust(rescale0to1(MeetingsUPA) ~ Flooding + patna, data = matched, weights = weights, se_type = "HC3")))


# Function to run regression (Flooding, controlling for Patna) and extract results
# Uses match.data()'s weights and HC3 robust SEs to account for the matched
# design (see note above the diagnostic regressions).
run_regression <- function(var) {
  model <- lm_robust(rescale0to1(matched[[var]]) ~ Flooding + patna,
                     data = matched,
                     weights = weights,
                     se_type = "HC3")
  
  # Use broom's tidy()/glance() rather than pulling fields off the model
  # object directly -- these are estimatr's documented, stable accessors
  # and avoid depending on exact internal list-field names.
  tidied <- tidy(model)
  glanced <- glance(model)
  
  flooding_row <- tidied[tidied$term == "Flooding", ]
  patna_row    <- tidied[tidied$term == "patna", ]
  
  return(c(
    Mean_Treated = mean(rescale0to1(matched[[var]])[matched$Flooding == 1], na.rm = TRUE),
    Mean_Control = mean(rescale0to1(matched[[var]])[matched$Flooding == 0], na.rm = TRUE),
    Flooding_b    = flooding_row$estimate,
    Flooding_SE   = flooding_row$std.error,
    p_value       = flooding_row$p.value,
    Patna_b       = patna_row$estimate,
    Patna_SE      = patna_row$std.error,
    Patna_p       = patna_row$p.value,
    Adj_R2        = glanced$adj.r.squared,
    N             = glanced$nobs
  ))
}

# List of all variables you tested
variables <- c("total_participation", "NDA_participation", "UPA_participation",
               "total_recirculated", "RecirculatedNDA", "RecirculatedUPA",
               "total_created", "CreatedNDA", "CreatedUPA",
               "total_commented", "CommentedNDA", "CommentedUPA",
               "total_posted", "PostedNDA", "PostedUPA",
               "total_rallies", "RalliesNDA", "RalliesUPA")
# "total_meetings", "MeetingsNDA", "MeetingsUPA")

# variables <- c("NDA_participation", "UPA_participation",
#                "RecirculatedNDA", "RecirculatedUPA",
#                "CreatedNDA", "CreatedUPA",
#                "CommentedNDA", "CommentedUPA",
#                "PostedNDA", "PostedUPA",
#                "RalliesNDA", "RalliesUPA",
#                "MeetingsNDA", "MeetingsUPA")

# Run all regressions and store results
results_matrix <- sapply(variables, run_regression)

# Convert to a data frame
results <- as.data.frame(t(results_matrix))
colnames(results) <- c("Mean_Treated", "Mean_Control",
                       "Flooding_b", "Flooding_SE", "p_value",
                       "Patna_b", "Patna_SE", "Patna_p",
                       "Adj_R2", "N")

# Adjust p-values using Holm and Benjamini-Hochberg (BH) and Bonferroni corrections
results$Holm_p <- p.adjust(results$p_value, method = "holm")
results$BH_p <- p.adjust(results$p_value, method = "BH")
results$Bonferroni_p <- p.adjust(results$p_value, method = "bonferroni")

# Sort results by raw p-values for easy interpretation
results <- results[order(results$p_value), ]

results <- signif(results, 2) |>
  mutate(orig_sig = ifelse(p_value < 0.05, "*", ""),
         holm_sig = ifelse(Holm_p < 0.05, "*", ""),
         BH_sig = ifelse(BH_p < 0.05, "*", ""),
         Bonferroni_sig = ifelse(Bonferroni_p < 0.05, "*", ""))

results |>
  mutate(Variable = rownames(results)) |>
  select(Variable, everything()) |>
  write_csv(glue("model output/regression-results_{match_model$info$method}-new.csv"))


# Moderation based on who they voted for
# VotedLokSabha2019, VotedAssembly2015 = 1 for NDA, 2 for MGB  

matched_LSsubset <- matched |>
  filter(VotedLokSabha2019 %in% c(1, 2))  |>
  mutate(VotedLokSabha2019 = factor(VotedLokSabha2019, levels = c(1, 2), labels = c("NDA", "MGB")))

matched_LAsubset <- matched |>
  filter(VotedAssembly2015 %in% c(1,2)) |>
  mutate(VotedAssembly2015 = factor(VotedAssembly2015, levels = c(1, 2), labels = c("NDA", "MGB")))

# Moderation by voting in Lok Sabha 2019
part_all1 <- lm_robust(total_participation ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3") 
part_NDA1 <- lm_robust(NDA_participation ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3") 
part_UPA1 <- lm_robust(UPA_participation ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3") 

recirc_all1 <- lm_robust(total_recirculated ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")
recirc_NDA1 <- lm_robust(RecirculatedNDA ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")
recirc_UPA1 <- lm_robust(RecirculatedUPA ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")

create_all1 <- lm_robust(total_created ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")
create_NDA1 <- lm_robust(CreatedNDA ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")
create_UPA1 <- lm_robust(CreatedUPA ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")

comment_all1 <- lm_robust(total_commented ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")
comment_NDA1 <- lm_robust(CommentedNDA ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")
comment_UPA1 <- lm_robust(CommentedUPA ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")

posted_all1 <- lm_robust(total_posted ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")
posted_NDA1 <- lm_robust(PostedNDA ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")
posted_UPA1 <- lm_robust(PostedUPA ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")

rallies_all1 <- lm_robust(total_rallies ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")
rallies_NDA1 <- lm_robust(RalliesNDA ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")
rallies_UPA1 <- lm_robust(RalliesUPA ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")

# meetings_all1 <- lm_robust(total_meetings ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")
# meetings_NDA1 <- lm_robust(MeetingsNDA ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")
# meetings_UPA1 <- lm_robust(MeetingsUPA ~ Flooding*VotedLokSabha2019 + patna, data = matched_LSsubset, weights = weights, se_type = "HC3")

modelsummary(list("Overall participation" = part_all1,
                  "Overall recirculating" = recirc_all1,
                  "Overall creation" = create_all1,
                  "Overall commenting" = comment_all1,
                  "Overall posting" = posted_all1,
                  "Overall rallies" = rallies_all1),
             # "Overall meetings" = meetings_all1),
             statistic = "{estimate} ({std.error})",
             stars = TRUE,
             output = glue("model output/moderation_loksabha_overall_{match_model$info$method}-new.docx"))

modelsummary(list("NDA participation" = part_NDA1,
                  "NDA recriculation" = recirc_NDA1,
                  "NDA creation" = create_NDA1,
                  "NDA commenting" = comment_NDA1,
                  "NDA posting" = posted_NDA1,
                  "NDA rallies" = rallies_NDA1),
             # "NDA meetings" = meetings_NDA1),
             statistic = "{estimate} ({std.error})",
             stars = TRUE,
             output = glue("model output/moderation_loksabha_BJP_{match_model$info$method}-new.docx"))

modelsummary(list("UPA participation" = part_UPA1,
                  "UPA recriculation" = recirc_UPA1,
                  "UPA creation" = create_UPA1,
                  "UPA commenting" = comment_UPA1,
                  "UPA posting" = posted_UPA1,
                  "UPA rallies" = rallies_UPA1),
             # "UPA meetings" = meetings_UPA1),
             statistic = "{estimate} ({std.error})",
             stars = TRUE,
             output = glue("model output/moderation_loksabha_UPA_{match_model$info$method}-new.docx"))

# Moderation by voting in Assembly 2015
part_all2 <- lm_robust(total_participation ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3") 
part_NDA2 <- lm_robust(NDA_participation ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3") 
part_UPA2 <- lm_robust(UPA_participation ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3") 

recirc_all2 <- lm_robust(total_recirculated ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")
recirc_NDA2 <- lm_robust(RecirculatedNDA ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")
recirc_UPA2 <- lm_robust(RecirculatedUPA ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")

create_all2 <- lm_robust(total_created ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")
create_NDA2 <- lm_robust(CreatedNDA ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")
create_UPA2 <- lm_robust(CreatedUPA ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")

comment_all2 <- lm_robust(total_commented ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")
comment_NDA2 <- lm_robust(CommentedNDA ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")
comment_UPA2 <- lm_robust(CommentedUPA ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")

posted_all2 <- lm_robust(total_posted ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")
posted_NDA2 <- lm_robust(PostedNDA ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")
posted_UPA2 <- lm_robust(PostedUPA ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")

rallies_all2 <- lm_robust(total_rallies ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")
rallies_NDA2 <- lm_robust(RalliesNDA ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")
rallies_UPA2 <- lm_robust(RalliesUPA ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")

# meetings_all2 <- lm_robust(total_meetings ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")
# meetings_NDA2 <- lm_robust(MeetingsNDA ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")
# meetings_UPA2 <- lm_robust(MeetingsUPA ~ Flooding*VotedAssembly2015 + patna, data = matched_LAsubset, weights = weights, se_type = "HC3")

modelsummary(list("Overall participation" = part_all2,
                  "Overall recirculating" = recirc_all2,
                  "Overall creation" = create_all2,
                  "Overall commenting" = comment_all2,
                  "Overall posting" = posted_all2,
                  "Overall rallies" = rallies_all2),
             # "Overall meetings" = meetings_all2),
             statistic = "{estimate} ({std.error})",
             stars = TRUE,
             output = glue("model output/moderation_assembly_overall_{match_model$info$method}-new.docx"))

modelsummary(list("NDA participation" = part_NDA2,
                  "NDA recriculation" = recirc_NDA2,
                  "NDA creation" = create_NDA2,
                  "NDA commenting" = comment_NDA2,
                  "NDA posting" = posted_NDA2,
                  "NDA rallies" = rallies_NDA2),
             # "NDA meetings" = meetings_NDA2),
             statistic = "{estimate} ({std.error})",
             stars = TRUE,
             output = glue("model output/moderation_assembly_NDA_{match_model$info$method}-new.docx"))

modelsummary(list("UPA participation" = part_UPA2,
                  "UPA recriculation" = recirc_UPA2,
                  "UPA creation" = create_UPA2,
                  "UPA commenting" = comment_UPA2,
                  "UPA posting" = posted_UPA2,
                  "UPA rallies" = rallies_UPA2),
             # "UPA meetings" = meetings_UPA2),
             statistic = "{estimate} ({std.error})",
             stars = TRUE,
             output = glue("model output/moderation_assembly_UPA_{match_model$info$method}-new.docx"))




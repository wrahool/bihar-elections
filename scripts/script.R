library(glue)
library(tidyverse)
library(MatchIt)
library(cobalt)
library(modelsummary)
library(tidymodels)
library(effsize)
library(pandoc)
library(patchwork)

rescale0to1 <- function(x) {
  rng <- range(x, na.rm = TRUE)   # compute min and max ignoring NAs
  (x - rng[1]) / (rng[2] - rng[1])
}

data_folder <- ".//data//"

survey_dat <- read_csv(glue('{data_folder}//clean-survey-data.csv'))
survey_colnames <- read_csv(glue('{data_folder}//survey-column-names.csv'))

names(survey_dat) <- survey_colnames$RenamedColumn

flood_dat <- read_csv(glue('{data_folder}//flood-data.csv'))

# correct data entry errors
survey_dat <- survey_dat |>
  mutate(Constituency = case_when(
    Constituency == 'Barahra' ~ 'Barhara',
    Constituency == 'Arrah bhojpur' ~ 'Arrah Bhojpur',
    Constituency == 'Atri Bidhan Sabha' ~ 'Atri',
    Constituency == 'Pipra, Supaul' ~ 'Pipra',
    Constituency == 'Ramnagar, Paschim Champaran' ~ 'Ramnagar',
    Constituency == 'Patna sahib' ~ 'Patna Sahib',
    TRUE ~ Constituency
  ))

# Missing in flood data 
survey_dat$Constituency |> setdiff(flood_dat$Constituency)

survey_dat <- survey_dat |>
  inner_join(flood_dat, by = 'Constituency')

survey_dat |> write_csv(glue('{data_folder}//clean-merged-survey-data-revised.csv'))

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
         RalliesUPA = ifelse(RalliesUPA %in% c(88, 99), replace_with, RalliesUPA),
         MeetingsNDA = ifelse(MeetingsNDA %in% c(88, 99), replace_with, MeetingsNDA),
         MeetingsUPA = ifelse(MeetingsUPA %in% c(88, 99), replace_with, MeetingsUPA)
  )

# Impute missing DV values

vars <- c(
  "RecirculatedNDA", "RecirculatedUPA",
  "CreatedNDA", "CreatedUPA",
  "CommentedNDA", "CommentedUPA",
  "PostedNDA", "PostedUPA",
  "RalliesNDA", "RalliesUPA",
  "MeetingsNDA", "MeetingsUPA"
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

vars <- c("Recirculated", "Created", "Commented", "Posted", "Rallies", "Meetings")

plots <- lapply(vars, function(v) plot_dv(merged_dat, v))

# Arrange in a grid, 2 per row, with a single shared legend
final_plot <- wrap_plots(plots, ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")  # put legend below

ggsave("figures/descriptives.svg",
       plot = final_plot,
       width = 6,
       height = 6,
       dpi = 300, device = "svg")


# Propensity Score Matching

# group columns thematically

demographic_cols <- c("Age", "Sex", "Caste", "Religion", "HouseholdIncome")

past_voting_cols <- c("VotedLokSabha2019", "VotedAssembly2015")

party_opinion_cols <- c("CentralGovtPerformance", "StateGovtPerformance",
                        "ImportantProblem", "GovtResponsibleProblem",
                        "PartySolveProblem", "StateStatus", "StateHeaded")

bjp_policy_opinion_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "BJPPolicy")) |>
  pull(RenamedColumn)

performance_leader_opinion_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "PerformanceLeader")) |>
  pull(RenamedColumn)

performance_party_opinion_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "PerformanceParty")) |>
  pull(RenamedColumn)

# this is general  phone usage, unrelated to elections
used_phone_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "UsedPhone")) |>
  pull(RenamedColumn)

# columns about being contacted by parties?
social_media_contact_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "SocialMediaContact")) |>
  pull(RenamedColumn)

phone_contact_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "PhoneContact")) |>
  pull(RenamedColumn)

sms_contact_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "SMSContact")) |>
  pull(RenamedColumn)

whatsapp_contact_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "WhatsappContact")) |>
  pull(RenamedColumn)

f2f_contact_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "F2F")) |>
  pull(RenamedColumn)

roadshow_contact_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "RoadshowContact")) |>
  pull(RenamedColumn)

email_contact_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "EmailContact")) |>
  pull(RenamedColumn)

# columns about people doing things (readings news, sharing news, attending meetings)
news_consumption_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "NewsConsumption")) |>
  pull(RenamedColumn)

news_sharing_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "NewsSharing")) |>
  pull(RenamedColumn)

news_attention_cols <- "AttentiontoNews"

bjp_negative_campaign_cols <- "BJPNegativeCampaign"

recirculated_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "Recirculated")) |>
  pull(RenamedColumn)

created_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "Created")) |>
  pull(RenamedColumn)

commented_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "Commented")) |>
  pull(RenamedColumn)

posted_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "Posted")) |>
  pull(RenamedColumn)

rallies_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "Rallies")) |>
  pull(RenamedColumn)

meetings_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "Meetings")) |>
  pull(RenamedColumn)

whatsapp_act_cols <- survey_colnames |> 
  filter(str_starts(RenamedColumn, "Whatsapp")) |>
  filter(!str_detect(RenamedColumn,  "Contact")) |>
  pull(RenamedColumn)

# party contact columns
BJPcontact_cols <- survey_colnames |>
  filter(str_ends(RenamedColumn, "ContactBJP") | str_ends(RenamedColumn, "ContactNDA")) |>
  pull(RenamedColumn)

UPAcontact_cols <- survey_colnames |>
  filter(str_ends(RenamedColumn, "ContactUPA")) |>
  pull(RenamedColumn)

LJPcontact_cols <- survey_colnames |>
  filter(str_ends(RenamedColumn, "ContactLJP")) |>
  pull(RenamedColumn)

Otherscontact_cols <- survey_colnames |>
  filter(str_ends(RenamedColumn, "ContactOthers")) |>
  pull(RenamedColumn)

Independentcontact_cols <- survey_colnames |>
  filter(str_ends(RenamedColumn, "ContactIndependent")) |>
  pull(RenamedColumn)

reqd_covariates <- c(demographic_cols, past_voting_cols,
                     "OrganisationMember", "UsedPhone")



# Create the formula dynamically
formula <- as.formula(paste("Flooding ~", paste(reqd_covariates, collapse = " + ")))

# Perform propensity score matching

match_model <- matchit(formula, 
                       data = merged_dat, 
                       method = "cardinality") # this gives the best covariate balance


# balance plot
svg("figures/balance_plot.svg", width = 8, height = 8)
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
      RalliesNDA + RalliesUPA +
      MeetingsNDA + MeetingsUPA,
    
    NDA_participation = RecirculatedNDA + CreatedNDA +
      CommentedNDA + PostedNDA +
      RalliesNDA + MeetingsNDA,
    
    UPA_participation = RecirculatedUPA + CreatedUPA +
      CommentedUPA + PostedUPA +
      RalliesUPA + MeetingsUPA,
    
    total_recirculated = RecirculatedNDA + RecirculatedUPA,
    total_created = CreatedNDA + CreatedUPA,
    total_commented = CommentedNDA + CommentedUPA,
    total_posted = PostedNDA + PostedUPA,
    total_rallies = RalliesNDA + RalliesUPA,
    total_meetings = MeetingsNDA + MeetingsUPA
  )

t.test(rescale0to1(total_participation) ~ Flooding, data = matched) 
t.test(rescale0to1(NDA_participation) ~ Flooding, data = matched) 
t.test(rescale0to1(UPA_participation) ~ Flooding, data = matched) 

t.test(rescale0to1(total_recirculated) ~ Flooding, data = matched) 
t.test(rescale0to1(RecirculatedNDA) ~ Flooding, data = matched) 
t.test(rescale0to1(RecirculatedUPA) ~ Flooding, data = matched) 

t.test(rescale0to1(total_created) ~ Flooding, data = matched) 
t.test(rescale0to1(CreatedNDA) ~ Flooding, data = matched) 
t.test(rescale0to1(CreatedUPA) ~ Flooding, data = matched) 

t.test(rescale0to1(total_commented) ~ Flooding, data = matched) 
t.test(rescale0to1(CommentedNDA) ~ Flooding, data = matched) 
t.test(rescale0to1(CommentedUPA) ~ Flooding, data = matched) 

t.test(rescale0to1(total_posted) ~ Flooding, data = matched) 
t.test(rescale0to1(PostedNDA) ~ Flooding, data = matched) 
t.test(rescale0to1(PostedUPA) ~ Flooding, data = matched) 

t.test(rescale0to1(total_rallies) ~ Flooding, data = matched) 
t.test(rescale0to1(RalliesNDA) ~ Flooding, data = matched) 
t.test(rescale0to1(RalliesUPA) ~ Flooding, data = matched) 

t.test(rescale0to1(total_meetings) ~ Flooding, data = matched) 
t.test(rescale0to1(MeetingsNDA) ~ Flooding, data = matched) 
t.test(rescale0to1(MeetingsUPA) ~ Flooding, data = matched) 


# Function to run t-test and extract results
run_t_test <- function(var) {
  test <- t.test(rescale0to1(matched[[var]]) ~ matched$Flooding)
  effect_size <- cohen.d(rescale0to1(matched[[var]]) ~ matched$Flooding)$estimate
  
  return(c(
    Mean_Treated = mean(rescale0to1(matched[[var]])[matched$Flooding == 1], na.rm = TRUE),
    Mean_Control = mean(rescale0to1(matched[[var]])[matched$Flooding == 0], na.rm = TRUE),
    p_value = test$p.value,
    Cohen_d = effect_size,
    SE = (test$conf.int[2] - test$conf.int[1]) / (2 * 1.96)  # Approximate SE from CI
  ))
}

# List of all variables you tested
variables <- c("total_participation", "NDA_participation", "UPA_participation",
               "total_recirculated", "RecirculatedNDA", "RecirculatedUPA",
               "total_created", "CreatedNDA", "CreatedUPA",
               "total_commented", "CommentedNDA", "CommentedUPA",
               "total_posted", "PostedNDA", "PostedUPA",
               "total_rallies", "RalliesNDA", "RalliesUPA",
               "total_meetings", "MeetingsNDA", "MeetingsUPA")

# variables <- c("NDA_participation", "UPA_participation",
#                "RecirculatedNDA", "RecirculatedUPA",
#                "CreatedNDA", "CreatedUPA",
#                "CommentedNDA", "CommentedUPA",
#                "PostedNDA", "PostedUPA",
#                "RalliesNDA", "RalliesUPA",
#                "MeetingsNDA", "MeetingsUPA")

# Run all t-tests and store results
results_matrix <- sapply(variables, run_t_test)

# Convert to a data frame
results <- as.data.frame(t(results_matrix))
colnames(results) <- c("Mean_Treated", "Mean_Control", "p_value", "Cohen_d", "SE")

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
  write_csv(glue("model output/t-test-results_{match_model$info$method}.csv"))


# Moderation based on who they voted for
# VotedLokSabha2019, VotedAssembly2015 = 1 for NDA, 2 for MGB  

matched_LSsubset <- matched |>
  filter(VotedLokSabha2019 %in% c(1, 2))  |>
  mutate(VotedLokSabha2019 = factor(VotedLokSabha2019, levels = c(1, 2), labels = c("NDA", "MGB")))

matched_LAsubset <- matched |>
  filter(VotedAssembly2015 %in% c(1,2)) |>
  mutate(VotedAssembly2015 = factor(VotedAssembly2015, levels = c(1, 2), labels = c("NDA", "MGB")))

# Moderation by voting in Lok Sabha 2019
part_all1 <- lm(total_participation ~ Flooding*VotedLokSabha2019, data = matched_LSsubset) 
part_NDA1 <- lm(NDA_participation ~ Flooding*VotedLokSabha2019, data = matched_LSsubset) 
part_UPA1 <- lm(UPA_participation ~ Flooding*VotedLokSabha2019, data = matched_LSsubset) 

recirc_all1 <- lm(total_recirculated ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)
recirc_NDA1 <- lm(RecirculatedNDA ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)
recirc_UPA1 <- lm(RecirculatedUPA ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)

create_all1 <- lm(total_created ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)
create_NDA1 <- lm(CreatedNDA ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)
create_UPA1 <- lm(CreatedUPA ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)

comment_all1 <- lm(total_commented ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)
comment_NDA1 <- lm(CommentedNDA ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)
comment_UPA1 <- lm(CommentedUPA ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)

posted_all1 <- lm(total_posted ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)
posted_NDA1 <- lm(PostedNDA ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)
posted_UPA1 <- lm(PostedUPA ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)

rallies_all1 <- lm(total_rallies ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)
rallies_NDA1 <- lm(RalliesNDA ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)
rallies_UPA1 <- lm(RalliesUPA ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)

meetings_all1 <- lm(total_meetings ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)
meetings_NDA1 <-lm(MeetingsNDA ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)
meetings_UPA1 <- lm(MeetingsUPA ~ Flooding*VotedLokSabha2019, data = matched_LSsubset)

modelsummary(list("Overall participation" = part_all1,
                  "Overall recirculating" = recirc_all1,
                  "Overall creation" = create_all1,
                  "Overall commenting" = comment_all1,
                  "Overall posting" = posted_all1,
                  "Overall rallies" = rallies_all1,
                  "Overall meetings" = meetings_all1),
             statistic = "{estimate} ({std.error})",
             stars = TRUE,
             output = glue("model output/moderation_loksabha_overall_{match_model$info$method}.docx"))

modelsummary(list("NDA participation" = part_NDA1,
                  "NDA recriculation" = recirc_NDA1,
                  "NDA creation" = create_NDA1,
                  "NDA commenting" = comment_NDA1,
                  "NDA posting" = posted_NDA1,
                  "NDA rallies" = rallies_NDA1,
                  "NDA meetings" = meetings_NDA1),
             statistic = "{estimate} ({std.error})",
             stars = TRUE,
             output = glue("model output/moderation_loksabha_BJP_{match_model$info$method}.docx"))

modelsummary(list("UPA participation" = part_UPA1,
                  "UPA recriculation" = recirc_UPA1,
                  "UPA creation" = create_UPA1,
                  "UPA commenting" = comment_UPA1,
                  "UPA posting" = posted_UPA1,
                  "UPA rallies" = rallies_UPA1,
                  "UPA meetings" = meetings_UPA1),
             statistic = "{estimate} ({std.error})",
             stars = TRUE,
             output = glue("model output/moderation_loksabha_UPA_{match_model$info$method}.docx"))

# Moderation by voting in Assembly 2015
part_all2 <- lm(total_participation ~ Flooding*VotedAssembly2015, data = matched_LAsubset) 
part_NDA2 <- lm(NDA_participation ~ Flooding*VotedAssembly2015, data = matched_LAsubset) 
part_UPA2 <- lm(UPA_participation ~ Flooding*VotedAssembly2015, data = matched_LAsubset) 

recirc_all2 <- lm(total_recirculated ~ Flooding*VotedAssembly2015, data = matched_LAsubset)
recirc_NDA2 <- lm(RecirculatedNDA ~ Flooding*VotedAssembly2015, data = matched_LAsubset)
recirc_UPA2 <- lm(RecirculatedUPA ~ Flooding*VotedAssembly2015, data = matched_LAsubset)

create_all2 <- lm(total_created ~ Flooding*VotedAssembly2015, data = matched_LAsubset)
create_NDA2 <- lm(CreatedNDA ~ Flooding*VotedAssembly2015, data = matched_LAsubset)
create_UPA2 <- lm(CreatedUPA ~ Flooding*VotedAssembly2015, data = matched_LAsubset)

comment_all2 <- lm(total_commented ~ Flooding*VotedAssembly2015, data = matched_LAsubset)
comment_NDA2 <- lm(CommentedNDA ~ Flooding*VotedAssembly2015, data = matched_LAsubset)
comment_UPA2 <- lm(CommentedUPA ~ Flooding*VotedAssembly2015, data = matched_LAsubset)

posted_all2 <- lm(total_posted ~ Flooding*VotedAssembly2015, data = matched_LAsubset)
posted_NDA2 <- lm(PostedNDA ~ Flooding*VotedAssembly2015, data = matched_LAsubset)
posted_UPA2 <- lm(PostedUPA ~ Flooding*VotedAssembly2015, data = matched_LAsubset)

rallies_all2 <- lm(total_rallies ~ Flooding*VotedAssembly2015, data = matched_LAsubset)
rallies_NDA2 <- lm(RalliesNDA ~ Flooding*VotedAssembly2015, data = matched_LAsubset)
rallies_UPA2 <- lm(RalliesUPA ~ Flooding*VotedAssembly2015, data = matched_LAsubset)

meetings_all2 <- lm(total_meetings ~ Flooding*VotedAssembly2015, data = matched_LAsubset)
meetings_NDA2 <-lm(MeetingsNDA ~ Flooding*VotedAssembly2015, data = matched_LAsubset)
meetings_UPA2 <- lm(MeetingsUPA ~ Flooding*VotedAssembly2015, data = matched_LAsubset)

modelsummary(list("Overall participation" = part_all2,
                  "Overall recirculating" = recirc_all2,
                  "Overall creation" = create_all2,
                  "Overall commenting" = comment_all2,
                  "Overall posting" = posted_all2,
                  "Overall rallies" = rallies_all2,
                  "Overall meetings" = meetings_all2),
             statistic = "{estimate} ({std.error})",
             stars = TRUE,
             output = glue("model output/moderation_assembly_overall_{match_model$info$method}.docx"))

modelsummary(list("NDA participation" = part_NDA2,
                  "NDA recriculation" = recirc_NDA2,
                  "NDA creation" = create_NDA2,
                  "NDA commenting" = comment_NDA2,
                  "NDA posting" = posted_NDA2,
                  "NDA rallies" = rallies_NDA2,
                  "NDA meetings" = meetings_NDA2),
             statistic = "{estimate} ({std.error})",
             stars = TRUE,
             output = glue("model output/moderation_assembly_NDA_{match_model$info$method}.docx"))

modelsummary(list("UPA participation" = part_UPA2,
                  "UPA recriculation" = recirc_UPA2,
                  "UPA creation" = create_UPA2,
                  "UPA commenting" = comment_UPA2,
                  "UPA posting" = posted_UPA2,
                  "UPA rallies" = rallies_UPA2,
                  "UPA meetings" = meetings_UPA2),
             statistic = "{estimate} ({std.error})",
             stars = TRUE,
             output = glue("model output/moderation_assembly_UPA_{match_model$info$method}.docx"))




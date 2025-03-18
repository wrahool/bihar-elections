library(glue)
library(tidyverse)
library(MatchIt)
library(cobalt)
library(modelsummary)
library(tidymodels)

rescale0to1 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data_folder <- 'C://Users//Subhayan//NUS Dropbox//Subhayan Mukerjee//Research Projects//Bihar Floods//data'

survey_dat <- read_csv(glue('{data_folder}//clean-survey-data.csv'))
survey_colnames <- read_csv(glue('{data_folder}//survey-column-names.csv'))

names(survey_dat) <- survey_colnames$RenamedColumn

flood_dat <- read_csv(glue('{data_folder}//flood-data.csv'))

survey_dat <- survey_dat |>
  inner_join(flood_dat, by = 'Constituency')

survey_dat |> write_csv(glue('{data_folder}//clean-merged-survey-data.csv'))

# correct data entry errors
survey_dat <- survey_dat |>
  mutate(Constituency = case_when(
    Constituency == 'Patna sahib' ~ 'Patna Sahib',
    Constituency == 'Valmiki Nagar' ~ 'Valmikinagar',
    Constituency == 'Narkatiya' ~ 'Narkatia',
    TRUE ~ Constituency
  ))

votes_dat <- read_csv(glue('{data_folder}//votes-data.csv'))

# there are two Kalyanpurs. Remove the Kalyanpur from Samastipur district
# because there's no one in the survey from this Kalyanpur
# and we want to match the survey data with the correct Kalyanpur in the votes data
votes_dat <- votes_dat |>
  filter(!(`AC Name` == "Kalyanpur" & District == "Samastipur"))

merged_dat <- survey_dat |>
  left_join(votes_dat, by = c('Constituency' = 'AC Name'))

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
                       method = "optimal", distance = "logit")

plot(summary(match_model))

match_model <- matchit(formula, 
                       data = merged_dat, 
                       method = "cardinality")

# balance plot
plot(summary(match_model))

summary(match_model)

matched <- match.data(match_model)

# effect of flooding on participation

# replace 88 and 99 with 0
matched <- matched |>
  mutate(RecirculatedNDA = ifelse(RecirculatedNDA %in% c(88, 99), 0, RecirculatedNDA),
         RecirculatedUPA = ifelse(RecirculatedUPA %in% c(88, 99), 0, RecirculatedUPA),
         CreatedNDA = ifelse(CreatedNDA %in% c(88, 99), 0, CreatedNDA),
         CreatedUPA = ifelse(CreatedUPA %in% c(88, 99), 0, CreatedUPA),
         CommentedNDA = ifelse(CommentedNDA %in% c(88, 99), 0, CommentedNDA),
         CommentedUPA = ifelse(CommentedUPA %in% c(88, 99), 0, CommentedUPA),
         PostedNDA = ifelse(PostedNDA %in% c(88, 99), 0, PostedNDA),
         PostedUPA = ifelse(PostedUPA %in% c(88, 99), 0, PostedUPA),
         RalliesNDA = ifelse(RalliesNDA %in% c(88, 99), 0, RalliesNDA),
         RalliesUPA = ifelse(RalliesUPA %in% c(88, 99), 0, RalliesUPA),
         MeetingsNDA = ifelse(MeetingsNDA %in% c(88, 99), 0, MeetingsNDA),
         MeetingsUPA = ifelse(MeetingsUPA %in% c(88, 99), 0, MeetingsUPA)
  )

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


t.test(rescale0to1(total_participation) ~ Flooding, data = matched) # yes
t.test(rescale0to1(NDA_participation) ~ Flooding, data = matched) # yes
t.test(rescale0to1(UPA_participation) ~ Flooding, data = matched) # no

t.test(rescale0to1(total_recirculated) ~ Flooding, data = matched) # yes
t.test(rescale0to1(RecirculatedNDA) ~ Flooding, data = matched) # yes
t.test(rescale0to1(RecirculatedUPA) ~ Flooding, data = matched) # no

t.test(rescale0to1(total_created) ~ Flooding, data = matched) # no
t.test(rescale0to1(CreatedNDA) ~ Flooding, data = matched) # no
t.test(rescale0to1(CreatedUPA) ~ Flooding, data = matched) # no

t.test(rescale0to1(total_commented) ~ Flooding, data = matched) # yes
t.test(rescale0to1(CommentedNDA) ~ Flooding, data = matched) # yes
t.test(rescale0to1(CommentedUPA) ~ Flooding, data = matched) # yes

t.test(rescale0to1(total_posted) ~ Flooding, data = matched) # yes
t.test(rescale0to1(PostedNDA) ~ Flooding, data = matched) # yes
t.test(rescale0to1(PostedUPA) ~ Flooding, data = matched) # no

t.test(rescale0to1(total_rallies) ~ Flooding, data = matched) # no
t.test(rescale0to1(RalliesNDA) ~ Flooding, data = matched) # no
t.test(rescale0to1(RalliesUPA) ~ Flooding, data = matched) # no

t.test(rescale0to1(total_meetings) ~ Flooding, data = matched) # no
t.test(rescale0to1(MeetingsNDA) ~ Flooding, data = matched) # no
t.test(rescale0to1(MeetingsUPA) ~ Flooding, data = matched) # no


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

# Run all t-tests and store results
results_matrix <- sapply(variables, run_t_test)

# Convert to a data frame
results <- as.data.frame(t(results_matrix))
colnames(results) <- c("Mean_Treated", "Mean_Control", "p_value", "Cohen_d", "SE")

# Adjust p-values using Holm and Benjamini-Hochberg (BH) corrections
results$Holm_p <- p.adjust(results$p_value, method = "holm")
results$BH_p <- p.adjust(results$p_value, method = "BH")
results$Bonferroni_p <- p.adjust(results$p_value, method = "bonferroni")

# Sort results by raw p-values for easy interpretation
results <- results[order(results$p_value), ]

# Display the results
print(results)

results <- results |>
  mutate(orig_sig = ifelse(p_value < 0.05, "*", ""),
         holm_sig = ifelse(Holm_p < 0.05, "*", ""),
         BH_sig = ifelse(BH_p < 0.05, "*", ""),
         Bonferroni_sig = ifelse(Bonferroni_p < 0.05, "*", ""))

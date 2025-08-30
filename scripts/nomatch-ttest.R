library(glue)
library(tidyverse)
library(MatchIt)
library(cobalt)
library(modelsummary)
library(tidymodels)
library(effsize)
library(pandoc)

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

# Simple t-test analsis (before matching)

# replace 88 and 99 with 0
merged_dat_rep <- merged_dat |>
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

merged_dat_rep <- merged_dat_rep |> 
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

t.test(rescale0to1(total_participation) ~ Flooding, data = merged_dat_rep) # no
t.test(rescale0to1(NDA_participation) ~ Flooding, data = merged_dat_rep) # no
t.test(rescale0to1(UPA_participation) ~ Flooding, data = merged_dat_rep) # yes

t.test(rescale0to1(total_recirculated) ~ Flooding, data = merged_dat_rep) # yes
t.test(rescale0to1(RecirculatedNDA) ~ Flooding, data = merged_dat_rep) # yes
t.test(rescale0to1(RecirculatedUPA) ~ Flooding, data = merged_dat_rep) # yes

t.test(rescale0to1(total_created) ~ Flooding, data = merged_dat_rep) # yes
t.test(rescale0to1(CreatedNDA) ~ Flooding, data = merged_dat_rep) # no
t.test(rescale0to1(CreatedUPA) ~ Flooding, data = merged_dat_rep) # no

t.test(rescale0to1(total_commented) ~ Flooding, data = merged_dat_rep) # no
t.test(rescale0to1(CommentedNDA) ~ Flooding, data = merged_dat_rep) # no
t.test(rescale0to1(CommentedUPA) ~ Flooding, data = merged_dat_rep) # no

t.test(rescale0to1(total_posted) ~ Flooding, data = merged_dat_rep) # yes
t.test(rescale0to1(PostedNDA) ~ Flooding, data = merged_dat_rep) # no
t.test(rescale0to1(PostedUPA) ~ Flooding, data = merged_dat_rep) # yes

t.test(rescale0to1(total_rallies) ~ Flooding, data = merged_dat_rep) # no
t.test(rescale0to1(RalliesNDA) ~ Flooding, data = merged_dat_rep) # no
t.test(rescale0to1(RalliesUPA) ~ Flooding, data = merged_dat_rep) # no

t.test(rescale0to1(total_meetings) ~ Flooding, data = merged_dat_rep) # no
t.test(rescale0to1(MeetingsNDA) ~ Flooding, data = merged_dat_rep) # no
t.test(rescale0to1(MeetingsUPA) ~ Flooding, data = merged_dat_rep) # no

# Function to run t-test and extract results
run_t_test <- function(var) {
  test <- t.test(rescale0to1(merged_dat_rep[[var]]) ~ merged_dat_rep$Flooding)
  effect_size <- cohen.d(rescale0to1(merged_dat_rep[[var]]) ~ merged_dat_rep$Flooding)$estimate
  
  return(c(
    Mean_Treated = mean(rescale0to1(merged_dat_rep[[var]])[merged_dat_rep$Flooding == 1], na.rm = TRUE),
    Mean_Control = mean(rescale0to1(merged_dat_rep[[var]])[merged_dat_rep$Flooding == 0], na.rm = TRUE),
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

# Adjust p-values using Holm and Benjamini-Hochberg (BH) and Bonferroni corrections
results$Holm_p <- p.adjust(results$p_value, method = "holm")
results$BH_p <- p.adjust(results$p_value, method = "BH")
results$Bonferroni_p <- p.adjust(results$p_value, method = "bonferroni")

# Sort results by raw p-values for easy interpretation
results <- results[order(results$p_value), ]

# Display the results
print(signif(results, 2))

results <- results |>
  mutate(orig_sig = ifelse(p_value < 0.05, "*", ""),
         holm_sig = ifelse(Holm_p < 0.05, "*", ""),
         BH_sig = ifelse(BH_p < 0.05, "*", ""),
         Bonferroni_sig = ifelse(Bonferroni_p < 0.05, "*", ""))

print(results)

results |>
  mutate(Variable = rownames(results)) |>
  select(Variable, everything()) |>
  write_csv("model output/unmatched-t-test-results.csv")

tar_load(cleaned_yield_files)
tar_load(cleaned_lead_sheets)
tar_load(cleaned_test_weight)


cleaned_yield_files %<>%
  mutate(loc = toupper(loc))

cleaned_test_weight %<>%
  filter(!is.na(test), rep <= 4)

# Semi join test weight data to yield data to find test weight samples with a match
has_match <- semi_join(cleaned_test_weight, cleaned_yield_files, by = c("test", "loc", "genotype", "rep"))

genotype_corrections <- read_csv(here("tests", "data", "twt_no_match_corrections.csv")) %>%
  mutate(genotype = correct_genotype_name) %>%
  select(-correct_genotype_name) %>%
  mutate(rep = as.character(rep))

all_twt_corrected <- bind_rows(has_match, genotype_corrections)

#write_csv(no_match, here("tests", "data", "twt_no_match.csv"))

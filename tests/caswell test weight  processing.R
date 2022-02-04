tar_load(cleaned_lead_sheets)
tar_load(nir_masterfile)
tar_load(cleaned_yield_files)


masterfile <- read_csv(nir_masterfile) %>%
  select(NIR_No,
         test,
         loc,
         plant_no,
         color) %>%
  rename(genotype = plant_no,
         rep = color) %>%
  mutate(rep = as.character(rep))

# Get the number of reps to measure twt for
twt_reps <- cleaned_lead_sheets$`Merged tables`$`Data to collect` %>%
  filter(trait == "sdwt") %>%
  select(-trait) %>%
  rename(test = test_name) %>%
  mutate(test = str_remove(test, "2021 ")) %>%
  mutate(test = recode(test,
                       "LU 5E-1"  ="LU 5 Early-1",
                       "LU 5E-2"  ="LU 5 Early-2",
                       "LU 5L-1"  = "LU 5 Late-1",
                       "LU 5L-2"  = "LU 5 Late-2",
                       "LU 6E-21" = "LU 6 Early-21",
                       "LU 6L-21" = "LU 6 Late-21"))

# Get the plots that were grown at caswell
cas_yield <- cleaned_yield_files %>%
  left_join(twt_reps, by = "test") %>%
  filter(rep <= reps_to_measure) %>%
  filter(loc == "CAS") %>%
  select(genotype, loc, test, year, rep) %>%
  mutate(genotype = ifelse(test == "LO-HP 5", str_replace(genotype, "RM", "N"), genotype),
         genotype = ifelse(test == "LO-HP 5", str_replace(genotype, "V14", "N19"), genotype)) %>%
  filter(!(test %in% c("HIF 5", "LU 5 Early-1", "LU 5 Early-2")))


# Read in and clean the caswell test weight data
cas_twt <- clean_test_weight(here("data", "test_weight", "Test_weight_3.xlsx")) %>%
  filter(loc == "CAS") %>%
  mutate(genotype = ifelse(test == "LU 6 Late-21", str_replace(genotype, "N19-NC-Dilday", "NC-Dilday"), genotype),
         genotype = ifelse(test == "LU 6 Late-21", str_replace(genotype, "N19-Dunphy", "Dunphy"), genotype),)

# Join with the sdwt data from the cleaned lead sheets to get the expected number of reps
cas_twt_clean <- twt_reps %>%
  right_join(cas_twt, by = "test") %>%
  filter(rep <= reps_to_measure) %>%
  group_by(test, genotype, loc, rep) %>%
  top_n(1, twt_date_time) %>%
  right_join(cas_yield, by = c("test", "loc", "genotype", "rep", "year"))


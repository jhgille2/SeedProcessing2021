##################################################
## Project: 2021 Seed Processing
## Script purpose: Working on functions to clean up/analyze the combined 2020/2021
## yield data
## Date: 2022-03-07
## Author: Jay Gillenwater
##################################################

# Load the combined data
library(here)
source(here("packages.R"))

tar_load(c(multiyear_data,
           cleaned_lead_sheets))

# Want to fit a model of the form:
#
# y = mu + G + L + Y(L) + R(L:Y) + GL + GY(L) + error
#
# Where y = phenotype value, mu = global mean, g = genotype effect (fixed)
# L = location effect (random), Y(L) = nested effect of year within location
# R(L:Y) is the effect of rep nested within a year:loc combination, GL is
# the interaction of a genotype and a location, GY(L) is the effect of a genotype by year interaction nested within a location,
# and error is the random error term
#
# (see https://www.fao.org/3/y4391e/y4391e07.htm#TopOfPage)


# First, clean up the lead sheets from 2020
cleaned_lead_sheets_2020 <- clean_lead_sheets(files = list.files(here("data", "leadsheets_2020"), full.names = TRUE))

# A function to clean up the "data to collect" portion of the leadsheets so that it can be joined to the rest of the data
clean_data_to_collect <- function(leadsheets){
  # Get just the number of reps to measure for protein/oil
  # and use this to make a new table with explicit observations
  # for protein and oil seperately
  prot_oil_table <- leadsheets$`Merged tables`$`Data to collect` %>% dplyr::filter(trait == "protein/oil")

  new_prot_oil_table <- tibble(trait = c(rep("oil_dry_basis", nrow(prot_oil_table)), rep("protein_dry_basis", nrow(prot_oil_table)), rep("po", nrow(prot_oil_table))),
                               reps_to_measure = rep(prot_oil_table$reps_to_measure, 3),
                               test_name = rep(prot_oil_table$test_name, 3))

  new_sdwt_table <- leadsheets$`Merged tables`$`Plot techniques` %>%
    filter(component == "reps") %>%
    mutate(value = as.numeric(value) - 1,
           component = "sdwt") %>%
    rename(trait = component,
           reps_to_measure = value)

  new_sq_table <- leadsheets$`Merged tables`$`Data to collect` %>%
    filter(trait == "seed quality") %>%
    mutate(reps_to_measure = 2)

  # Also add test weight to the traits to measure (same number of reps as sdwt)
  twt_table <- leadsheets$`Merged tables`$`Data to collect` %>% dplyr::filter(trait == "sdwt") %>%
    dplyr::mutate(trait = "twt_weight")

  # Add these two tables to the "data to collect" table and recode some of the trait names
  # to match the ones that are used in the merged data
  lead_sheet_trait_table <- leadsheets$`Merged tables`$`Data to collect` %>%
    dplyr::filter(trait != "protein/oil") %>%
    dplyr::filter(trait != "sdwt") %>%
    dplyr::filter(trait != "seed quality") %>%
    dplyr::bind_rows(new_prot_oil_table) %>%
    dplyr::bind_rows(new_sdwt_table) %>%
    dplyr::bind_rows(twt_table) %>%
    dplyr::bind_rows(new_sq_table) %>%
    dplyr::mutate(trait = recode(trait,
                                 "lodging"      = "lod",
                                 "height"       = "ht",
                                 "seed quality" = "sq",
                                 "flowor color" = "fc",
                                 "maturity"     = "md",
                                 "pubescence"   = "pub"),
                  year = str_sub(test_name, start = 1, end = 4),
                  test_name = purrr::map_chr(test_name, function(x) stringr::str_remove(x, "2021 ")),
                  test_name = purrr::map_chr(test_name, function(x) stringr::str_remove(x, "2020 ")),
                  test_name = recode(test_name,
                                     "LU 5E-1"  = "LU 5 Early-1",
                                     "LU 5E-2"  = "LU 5 Early-2",
                                     "LU 5L-1"  = "LU 5 Late-1",
                                     "LU 5L-2"  = "LU 5 Late-2",
                                     "LU 6E-21" = "LU 6 Early-21",
                                     "LU 6L-21" = "LU 6 Late-21")) %>%
    dplyr::rename(test = test_name)

  return(lead_sheet_trait_table)
}

data_to_collect_2020 <- clean_data_to_collect(cleaned_lead_sheets_2020)
data_to_collect_2021 <- clean_data_to_collect(cleaned_lead_sheets)

combined_data_to_collect <- bind_rows(data_to_collect_2020, data_to_collect_2021)

# The traits I want to fit a model on
measurement_variables <- c("md",
                           "ht",
                           "lod",
                           "yield",
                           "sdwt",
                           "sq",
                           "protein_dry_basis",
                           "oil_dry_basis",
                           "po")

# Pivot the data by these measurement variables and then join to the combined data
# to collect tables so the expected number of reps can be added as an
# additional column
pivoted_multiyear_data <- multiyear_data %>%
  pivot_longer(cols = measurement_variables, names_to = "trait") %>%
  left_join(combined_data_to_collect, by = c("trait", "test", "year")) %>%
  filter(as.numeric(rep) <= reps_to_measure) %>%
  select(-reps_to_measure) %>%
  group_by(trait) %>%
  nest()

# And here is a function to fit the model
combined_model <- function(Data){

  tryCatch(
    {
      # Make sure that genotype, location, and rep are factors
      Data %<>%
        select(value, genotype, loc, rep, year) %>%
        mutate(genotype = as.factor(genotype),
               loc      = as.factor(loc),
               rep      = as.factor(rep),
               year     = as.factor(year))

      # y = mu + G + L + Y(L) + R(L:Y) + GL + GY(L) + error

      # Fit the model
      Model <- with(Data, lme4::lmer(value ~ genotype  + (1|loc)+ (1|loc:year) + (1|year:loc:rep) + (1|genotype:loc) + (1|genotype:year:loc)))

      # Get the genotype marginal means from the model
      Model %>%
        emmeans("genotype") %>%
        as.data.frame() %>%
        dplyr::select(genotype, emmean) %>%
        mutate(emmean = round(emmean, 1)) %>%
        rename(LSMean = emmean)
    },

    # If the is an error when the model is being fit, return a NA value for
    # each genotype marginal means to make it easier to find problematic data
    # without clogging up the rest iof the analysis
    error = function(cnd) {
      Data %>%
        dplyr::select(genotype) %>%
        group_by(genotype) %>%
        sample_n(1) %>%
        ungroup() %>%
        mutate(LSMean = NA) -> EmptyData

      return(EmptyData)
    }
  )

}


library(furrr)
library(future)

plan("multisession", workers = 5)

combined_ememans <- pivoted_multiyear_data %>%
  mutate(pheno_emmean = future_map(data, combined_model))

pheno_key <- c(ht                = "Height (inches)",
               yield             = "Yield (bu/acre)",
               sdwt              = "Seed weight (grams)",
               oil_dry_basis     = "Oil (dry basis)",
               protein_dry_basis = "Protein (dry basis)",
               po                = "Protein + Oil",
               lod               = "Lodging",
               md                = "Maturity Date",
               sq                = "Seed quality")

combined_lsmeans_data <- combined_ememans %>%
  select(-data) %>%
  unnest(pheno_emmean) %>%
  mutate(trait = recode(trait, !!!pheno_key)) %>%
  pivot_wider(names_from = trait, values_from = LSMean) %>%
  arrange(desc(`Yield (bu/acre)`))


openxlsx::write.xlsx(combined_lsmeans_data, here("exports", "combined_lsmeans.xlsx"))

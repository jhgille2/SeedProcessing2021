#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param phenotype_data
calculate_lsmeans <- function(phenotype_data = pivoted_phenotype_data) {

  ## Section: LSMean functions
  ##################################################


  # These two functions fit the two models and then extract the genotype marginal
  # means from them

  # A function to fit a linear model within each location once the data has been grouped by
  # the location and the phenotype
  Model_ByLoc <- function(LocationData){

    tryCatch(
      {

        # Make sure that genotype, location, and rep are factors
        LocationData %<>%
          select(value, genotype, rep) %>%
          mutate(genotype = as.factor(genotype),
                 rep      = as.factor(rep))

        # Fit the model
        Model <- with(LocationData, lm(value ~ genotype + rep))

        # Get the genotype marginal means from the model
        Model %>%
          emmeans::emmeans("genotype") %>%
          as.data.frame() %>%
          dplyr::select(genotype, emmean) %>%
          mutate(emmean = round(emmean, 1)) %>%
          rename(LSMean = emmean)
      },

      # If the is an error when the model is being fit, return a NA value for
      # each genotype marginal means to make it easier to find problematic data
      # without clogging up the rest iof the analysis
      error = function(cnd) {
        LocationData %>%
          dplyr::select(genotype) %>%
          group_by(genotype) %>%
          sample_n(1) %>%
          ungroup() %>%
          mutate(LSMean = NA) -> EmptyData

        return(EmptyData)
      }
    )

  }


  Model_Overall <- function(OverallData){

    tryCatch(
      {
        # Make sure that genotype, location, and rep are factors
        OverallData %<>%
          select(value, genotype, loc, rep) %>%
          mutate(genotype = as.factor(genotype),
                 loc      = as.factor(loc),
                 rep      = as.factor(rep))

        # Fit the model
        Model <- with(OverallData, lme4::lmer(value ~ genotype + (1|loc/rep) + (1|loc:genotype)))

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
        OverallData %>%
          dplyr::select(genotype) %>%
          group_by(genotype) %>%
          sample_n(1) %>%
          ungroup() %>%
          mutate(LSMean = NA) -> EmptyData

        return(EmptyData)
      }
    )

  }

  ## Section: Applying the functions and cleaning up
  ##################################################

  # Here's how the by-location models can be fit to the nested data
  by_loc_means <-  phenotype_data %>%
    mutate(by_loc_means = map(data, Model_ByLoc))

  # To fit the overall means model, the data has to be unnested, grouped by just test
  # and phenotype, and then nested again so that the location variable can be included
  # in the "data" column
  overall_means <- phenotype_data %>%
    unnest(data) %>%
    group_by(test) %>%
    mutate(n_locs = length(unique(loc))) %>%
    ungroup() %>%
    filter(n_locs > 1) %>%
    group_by(test, trait) %>%
    nest() %>%
    mutate(overall_means = map(data, Model_Overall))

  # Since this is also pretty close to the end of the analysis, I also want to
  # make the phenotype names more "human friendly". I'll use the recode function
  # from dplyr to do this

  # Make a key to rename phenotypes
  pheno_key <- c(ht                = "Height (inches)",
                 yield             = "Yield (grams)",
                 sdwt              = "Seed weight (grams)",
                 oil_dry_basis     = "Oil (dry basis)",
                 protein_dry_basis = "Protein (dry basis)",
                 twt_weight        = "Test weight",
                 p_o               = "Protein + Oil",
                 lod               = "Lodging",
                 md                = "Maturity Date")

  # The cleaned by-location marginal means
  by_loc_clean <- by_loc_means %>%
    select(loc, test, trait, by_loc_means) %>%
    mutate(trait = recode(trait, !!!pheno_key)) %>%
    unnest(by_loc_means) %>%
    pivot_wider(names_from  = c(loc, trait),
                names_sep   = " - ",
                names_sort  = TRUE,
                values_from = LSMean)

  # The cleaned overall means
  overall_clean <- overall_means %>%
    select(test, trait, overall_means) %>%
    mutate(trait = recode(trait, !!!pheno_key)) %>%
    unnest(overall_means) %>%
    pivot_wider(names_from  = trait,
                names_sep   = " - ",
                names_sort  = TRUE,
                values_from = LSMean)

  # And finally, the two datasets can be joined by test and genotype
  lsmeans_final <- left_join(by_loc_clean, overall_clean, by = c("test", "genotype")) %>%
    rename(Genotype = genotype)

  return(lsmeans_final)
}

install.packages("jsonlite", repos = "https://cloud.r-project.org")

library(optparse) # make_option OptionParser parse_args
library(jsonlite) # fromJSON
library(here)
library(tidyverse)
library(magrittr)

# --------- Get the output from GHA ---------

# Look for the data_in argument
option_list <- list(
  optparse::make_option(
    c("--data_in_2024_b1"),
    type = "character",
    default = NULL,
    help = "2024 Sheet Results (json, batch 1)",
  ),
  optparse::make_option(
    c("--data_in_2024_b2"),
    type = "character",
    default = NULL,
    help = "2024 Sheet Results (json, batch 2)"
  ),
  optparse::make_option(
    c("--data_in_2024_b3"),
    type = "character",
    default = NULL,
    help = "2024 Sheet Results (json, batch 3)"
  ),
  optparse::make_option(
    c("--data_in_2025"),
    type = "character",
    default = NULL,
    help = "2025 Sheet Results (json)"
  )
  #add more optparse::make_option's here for each year of results
)


# Read the results provided as command line argument
opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)
jsonResults_2024b1 <- opt$data_in_2024_b1
jsonResults_2024b2 <- opt$data_in_2024_b2
jsonResults_2024b3 <- opt$data_in_2024_b3
jsonResults_2025 <- opt$data_in_2025
#add more opt$data_in_{}_{} assignments here for each year (and batch) of results

get_combine_dfs <- function(jsonResultsVec){
  if (length(jsonResultsVec) > 1){
    full_df = data.frame()
  }

  for (i in 1:length(jsonResultsVec)){
    df <- fromJSON(jsonResultsVec[i])
    df <- df$results$results$formatted[[2]]
    if (i == 1){
      colnames(df) <- df[1,] #colnames taken from first row of data
      df <- df[-1, ] #remove the first row of data (original column names)
      message(dim(df))
    }
    df <- tibble::as_tibble(df)
    if (i > 1){
      full_df <- rbind(full_df, df)
    }
  }

  if (length(jsonResultsVec) > 1){
  message(dim(full_df))
    return(full_df)
  } else { return(df)}
}

wrangle_data <- function(df, year){
  print(paste0("Raw results dimension ", year, ": ", dim(df)))

  # --------- Any analysis/tidying you want to do ---------

  # ------- Tidying Step: simplify column names ----------
  simplifiedColNames <-
    read_delim(here(paste0("data/", year, "/codebook.txt")),
                delim = "\t",
                col_select = SimplifiedColNames)
  df <-
    df %>% `colnames<-`(unlist(simplifiedColNames))

  # ------- Tidying Step: Drop any column which contains all NAs ----

  df <- df[, colSums(is.na(df)) != nrow(df)]
  print(paste0("Number of columns after renaming and dropping ", year, ": ", ncol(df)))

  # ------ Tidying step: If an email is duplicated, keep the latest response -----

  df %<>% mutate(Email = str_replace(Email, ".eud", ".edu"))

  print(paste0("Number of NA emails ", year, ": ", sum(is.na(df$Email))))

  tabulatedEmails <- table(df$Email, useNA = "no")

  if (sum(tabulatedEmails > 1) > 0) {
    duplicatedEmails <-
      names(tabulatedEmails)[which(tabulatedEmails > 1)]
    IDXs_to_remove <-
      unlist(lapply(1:length(duplicatedEmails), function(x)
        head(
          which(df$Email == duplicatedEmails[x]),-1
        )))
    print(paste0("Removing __ IDs for year ", year, ": ", length(IDXs_to_remove)))
    df <- df[-IDXs_to_remove, ]
  }
  #should I drop the email column?

  print(paste0(year, ": ", nrow(df)))

  # ----- Tidying step: Change typeof is list columns to not be lists (unnesting) and recode rankings as integers-------

  df %<>% mutate(across(starts_with(c(
      "ReturningAnVILTech", "AllTech"
  )), as.character)) %>%
     unnest(starts_with(c("ReturningAnVILTech", "AllTech")), keep_empty = TRUE) %>%
     mutate(across(starts_with(c(
       "ReturningAnVILTech", "AllTech"
     )), ~ parse_factor(
       .,
       levels = c(
         "Don't know it",
         "Not at all comfortable",
         "Slightly comfortable",
         "Somewhat comfortable",
         "Moderately comfortable",
         "Extremely comfortable"
       )
     ))) %>%
     mutate(across(
       starts_with(c("ReturningAnVILTech", "ALLTech")),
       ~ case_when(
         . == "Don't know it" ~ 0,
         . == "Not at all comfortable" ~ 1,
         . == "Slightly comfortable" ~ 2,
         . == "Somewhat comfortable" ~ 3,
         . == "Moderately comfortable" ~ 4,
         . == "Extremely comfortable" ~ 5
       )
       ,
       .names = "Score_{.col}"
     ))

  df %<>%
    mutate(across(starts_with(c(
      "PotentialRank", "ReturningRank"
    )), as.character)) %>%
    unnest(starts_with(c("PotentialRank", "ReturningRank")), keep_empty = TRUE) %>%
    mutate(across(
      starts_with(c("PotentialRank", "ReturningRank")),
      ~ recode(
        .x,
        "1 (Most important in this list)" = "1",
        "6 (Least important in this list)" = "6",
        "NULL" = NA_character_
      )
    )) %>%
    mutate(across(starts_with(c(
      "PotentialRank", "ReturningRank"
    )), as.integer))

  df %<>%
    mutate(across(starts_with(c(
      "PotentialPay", "ReturningPay"
    )), as.character)) %>%
    unnest(starts_with(c("PotentialPay", "ReturningPay")), keep_empty = TRUE) %>%
    mutate(across(
      starts_with(c("PotentialPay", "ReturningPay")),
      ~ recode(
        .x,
        "1 (Most preferred in this list)" = "1",
        "3 (Least preferred in this list)" = "3",
        "NULL" = NA_character_
      )
    )) %>%
    mutate(across(starts_with(c(
      "PotentialPay", "ReturningPay"
    )), as.integer))

  df %<>%
    mutate(across(starts_with(
      "LearningMethod"
    ), as.character)) %>%
    unnest(starts_with("LearningMethod"), keep_empty = TRUE) %>%
    mutate(across(
      starts_with("LearningMethod"),
      ~ recode(
        .x,
        "1 (Most preferred in this list)" = "1",
        "7 (Least preferred in this list)" = "7",
        "NULL" = NA_character_
      )
    )) %>%
    mutate(across(starts_with(
      "LearningMethod"
    ), as.integer))

  df %<>%
    mutate(across(starts_with(
      "AnVILTrainingWorkshops"), as.character)) %>%
    unnest(starts_with("AnVILTrainingWorkshops"), keep_empty = TRUE) %>%
    mutate(across(
      starts_with("AnVILTrainingWorkshops"),
      ~ recode(
        .x,
        "1 (Most preferred in this list)" = "1",
        "5 (Least preferred in this list)" = "5",
        "NULL" = NA_character_
      )
    )) %>%
    mutate(across(starts_with("AnVILTrainingWorkshop"), as.integer))


  # ----- Tidying step: replace certain strings with a recoding ----

  df %<>%
    mutate(
      InstitutionalAffiliation =
        recode(
          InstitutionalAffiliation,
          "Broad" = "Broad Institute",
          "broad institute" = "Broad Institute",
          "CUNY School of Public Health; Roswell Park Comprehensive Cancer Center" = "City University of New York",
          "harvard" = "Harvard University",
          "Harvard Public Health" = "Harvard University",
          "Johns hopkins" = "Johns Hopkins",
          "Johns Hopkins University" = "Johns Hopkins",
          "OHSU" = "Oregon Health & Science University",
          "OHSU (Knight Center)" = "Oregon Health & Science University",
          "The Ohio State University" = "Ohio State University",
          "UCSC" = "University of California Santa Cruz",
          "univ. ca. santa cruz" = "University of California Santa Cruz",
          "university of California santa cruz" = "University of California Santa Cruz",
          "UMASS Chan Medical School" = "UMass Chan Medical School",
          "Umass Chan Medical School" = "UMass Chan Medical School",
          "Washington University in St Louis" = "Washington University in St. Louis",
          "yikongene" = "Yikon Genomics",
          "v" = "Unknown"
        )
    )

  # ----- Tidying step: Categorize specific answers into groups -----

  ## Adds column `UserType`
  ### `UserType` will only be "Returning User" or "Potential User" in 2024. Adds "Past and Potential User as an option in 2025"
  df %<>%
    mutate(
      UserType = case_when(
        str_detect(tolower(CurrentUsageDescription), "ongoing|long-term|short-term") ~ "Returning User",
        str_detect(tolower(CurrentUsageDescription), "have in the past|never used|never heard") ~ "Potential User",
        str_detect(tolower(CurrentUsageDescription), "before") ~ "Past and Potential User"
      )
    ) %>%
    mutate(UserType = factor(UserType,
                            levels = c("Potential User", "Returning User", "Past and Potential User")
                            )
          )

  ## Adds columns `clinicalFlag`, `humanGenomicFlag`, and `nonHumanGenomicFlag`
  ### Each of these columns will have a value of TRUE for poll responses that report being either "Moderately" or "Extremely" experienced in the relevant research category. FALSE otherwise.
  df %<>%
    mutate(
    clinicalFlag = case_when(
             HumanClinicalExperience == "Moderately experienced" | HumanClinicalExperience == "Extremely experienced" ~ TRUE,
             .default = FALSE
           ),
    humanGenomicFlag = case_when(
             HumanGenomicExperience == "Moderately experienced" | HumanGenomicExperience == "Extremely experienced" ~ TRUE,
             .default = FALSE
           ),
    nonHumanGenomicFlag = case_when(NonHumanGenomicExperience == "Moderately experienced" | NonHumanGenomicExperience == "Extremely experienced" ~ TRUE,
            .default = FALSE)
    )

  ## Adds columns `AnVILDemoAwareness` and `AnVILDemoUse`
  ### `AnVILDemoAwareness` recodes raw responses from `AnVILDemo` to "Aware of" or "Not Aware of"
  ### `AnVILDemoUse` recodes raw responses from `AnVILDemo` to "Have/will utilize" or "Have not utilized"
  df %<>%
    mutate(AnVILDemo = factor(AnVILDemo, levels = c("Yes, multiple", "Yes, one", "Not yet, but am registered to", "No, but aware of", "No, didn't know of")),
          AnVILDemoAwareness = factor(case_when(
            AnVILDemo == "Yes, multiple" ~ "Aware of",
            AnVILDemo == "Yes, one" ~ "Aware of",
            AnVILDemo == "Not yet, but am registered to" ~ "Aware of",
            AnVILDemo == "No, but aware of" ~ "Aware of",
            AnVILDemo == "No, didn't know of" ~ "Not Aware of"
          ), levels = c("Not Aware of", "Aware of")),
          AnVILDemoUse = factor(case_when(
            AnVILDemo == "Yes, multiple" ~ "Have/will utilize",
            AnVILDemo == "Yes, one" ~ "Have/will utilize",
            AnVILDemo == "Not yet, but am registered to" ~ "Have/will utilize",
            AnVILDemo == "No, but aware of" ~ "Have not utilized",
            AnVILDemo == "No, didn't know of" ~ "Have not utilized"
          ), levels = c("Have not utilized", "Have/will utilize"))
  )

  ## Adds columns `forumAwareness` and `forumUse`
  df %<>%
    mutate(forumAwareness = factor(
      case_when(
        str_detect(AnVILSupportForum, "aware of|Answered|Posted|Read") ~ "Aware of",
        str_detect(AnVILSupportForum, "didn't know of") ~ "Not Aware of"
      ), levels = c("Not Aware of", "Aware of")),
           forumUse = factor(
      case_when(
        str_detect(AnVILSupportForum, "Answered|Posted|Read") ~ "Have/will utilize",
        str_detect(AnVILSupportForum, "No, ") ~ "Have not utilized"
      ), levels = c("Have not utilized", "Have/will utilize"))
  )

  ## Adds column InstitutionalType from the institution specific codebook
  institutionCodeBook <- read_delim(here(paste0("data/", year, "/institution_codebook.txt")),
                                    delim="\t",
                                    col_select = c("InstitutionalAffiliation", "InstitutionalType"))

  df <- left_join(df, institutionCodeBook, by = "InstitutionalAffiliation")

  ## Adds column FurtherSimplifiedInstitutionalType

  df %<>%
    mutate(FurtherSimplifiedInstitutionalType =
             case_when(
               InstitutionalType == "R1 University" ~ "Research Intensive",
               InstitutionalType == "Research Center" ~ "Research Intensive",
               InstitutionalType == "Medical Center or School" ~ "Research Intensive",
               InstitutionalType == "NIH" ~ "Research Intensive",
               InstitutionalType == "R2 University" ~ "Education Focused",
               InstitutionalType == "Community College" ~ "Education Focused",
               InstitutionalType == "Industry" ~ "Industry & Other",
               InstitutionalType == "International Location" ~ "Industry & Other",
               InstitutionalType == "Unknown" ~ "Industry & Other",
               .default = NA_character_
             )
           )

  df %<>%
    mutate(
      Degrees =
        factor(recode(Degrees, "PhD, MD" = "MD/PhD"), levels = c("High School or equivalent", "Bachelor's degree", "Master's degree in progress", "Master's degree", "PhD in progress", "PhD", "MD in progress", "MD", "MD/PhD")),
      FurtherSimplifiedDegrees = recode(Degrees,
                                        "Master's degree in progress" = "Master's degree (or in progress)",
                                        "Master's degree" = "Master's degree (or in progress)",
                                        "PhD in progress" = "PhD (or in progress)",
                                        "PhD" = "PhD (or in progress)",
                                        "MD/PhD" = "MD (MD, MD/PhD, or in progress)",
                                        "MD in progress" = "MD (MD, MD/PhD, or in progress)",
                                        "MD" = "MD (MD, MD/PhD, or in progress)"
                                        )
    )

  # ----- Tidying step: replace certain strings with NA ------

  df %<>% mutate(Timestamp = as.factor(Timestamp)) #must change type from double so next replacements run without error

  df[df == 'n/a'] <- NA
  df[df == 'NULL'] <- NA

  return(df)
}

poll_results <- list(
  df2024 = wrangle_data(get_combine_dfs(c(jsonResults_2024b1, jsonResults_2024b2, jsonResults_2024b3)), "2024"),
  df2025 = wrangle_data(get_combine_dfs(c(jsonResults_2025)), "2025")
  #add more calls to the function for each year of results here
)

# --------- Render web pages from Rmd docs ---------

rmarkdown::render_site(
  input = "pages",
  envir = new.env(parent = globalenv()) # enable use of 'poll_results' list inside the Rmd being rendered
)

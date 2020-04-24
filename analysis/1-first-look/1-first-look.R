# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(ggplot2)
library(dplyr)
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("readr")   # data input
requireNamespace("tidyr")   # data manipulation
requireNamespace("dplyr")   # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")  # For asserting conditions meet expected patterns.
requireNamespace("corrplot")  # For asserting conditions meet expected patterns.
# requireNamespace("car")     # For it's `recode()` function.

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graph-presets.R") # fonts, colors, themes
source("./scripts/graphing/graph-missing.R")
baseSize = 8
# ---- declare-globals ---------------------------------------------------------
config <- config::get()
#set default ggplot theme
ggplot2::theme_set(ggplot2::theme_bw())

describe_item <- function(d, varname){
  # d <- ds %>% select(id, Q9)
  # d %>% glimpse()
  # varname <- "Q9"

  (variable_label <- labelled::var_label(d[,varname])[[1]])
  d %>% histogram_discrete(varname)+labs(title = paste0(varname,": ",variable_label))


  # d1 <- d %>%
  #   dplyr::rename(temp = varname ) %>%
  #   dplyr::mutate(
  #     temp = as.numeric(factor(temp)),
  #     temp = ifelse(temp %in% c(1:5), temp, NA)
  #   ) %>%
  #   plyr::rename(c("temp" = varname))
  #
  # d1 %>% group_by(temp) %>% summarize(n = n())
  #
  # psych::summary.psych(d)
  # d1 %>% histogram_continuous(varname)


  # cat("\n")
  # cat("\nMean: ",round(mean( as.numeric( factor(d[,varname]) ),na.rm = T),2),"\n")
  # cat("\nSD: ", round(sd(as.numeric(d[,varname]), na.rm = T),2),"\n")
  # cat("\nMissing: ",sum(is.na(d[,varname])),"\n")

}

demographic <- c(
  "Q2"   = "institution"
  ,"Q16" = "class_standing"  # What is your class standing?
  ,"Q17" = "age"  # What is your age?
  ,"Q18" = "race"  # What best describes your race/ethnicity? Mark all that apply.
  ,"Q19" = "gender"  # What best describes your gender?
  ,"Q20" = "political"  # What best describes your political leanings?
  ,"Q21" = "religion"  # How important is religion or spirituality to you?
  ,"Q22" = "student_type"  # Mark all that apply to you.
  ,"Q23" = "field_of_study"  # Field of study (max = 2)?
)
# demographic <- c(
#     "institution"
#   ,"class_standing"  # What is your class standing?
#   ,"age"  # What is your age?
#   ,"race"  # What best describes your race/ethnicity? Mark all that apply.
#   ,"gender"  # What best describes your gender?
#   ,"political"  # What best describes your political leanings?
#   ,"religion"  # How important is religion or spirituality to you?
#   ,"student_type"  # Mark all that apply to you.
#   ,"field_of_study"  # Field of study (max = 2)?
# )
methadone <- c(
  "Q7_1"  = "md_replace"
  ,"Q7_2" = "md_safe"
  ,"Q7_3" = "md_side_eff"
  ,"Q7_4" = "md_not_recov"
  ,"Q7_5" = "md_bad_phys"
  ,"Q7_6" = "md_get_high"
  ,"Q7_7" = "md_cravings"
  ,"Q7_8" = "md_from_high"
)

buprenorphine <- c(
  "Q8_1"  =  "br_replace"
  ,"Q8_2" =  "br_safe"
  ,"Q8_3" =  "br_side_eff"
  ,"Q8_4" =  "br_not_recov"
  ,"Q8_5" =  "br_bad_phys"
  ,"Q8_6" =  "br_get_high"
  ,"Q8_7" =  "br_cravings"
  ,"Q8_8" =  "br_from_high"
)

naltrexone <- c(
  "Q9_1"  = "nt_replace"
  ,"Q9_2" = "nt_safe"
  ,"Q9_3" = "nt_side_eff"
  ,"Q9_4" = "nt_not_recov"
  ,"Q9_5" = "nt_bad_phys"
  ,"Q9_6" = "nt_get_high"
  ,"Q9_7" = "nt_cravings"
  ,"Q9_8" = "nt_from_high"

)
# ---- load-data ---------------------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
ds0 <- readr::read_csv(config$oud_survey)
meta <- readr::read_csv(config$survey_items)

# ---- inspect-data -------------------------------------------------------------
ds0 %>% TabularManifest::histogram_discrete("Status")
ds0 %>% TabularManifest::histogram_continuous("Progress")
ds0$`Duration (in seconds)` %>% summary()
ds0 %>% TabularManifest::histogram_discrete("Finished")


# ---- print-meta-1 ---------------------------
varname_scale <- c(methadone, buprenorphine, naltrexone)

# ---- tweak-data --------------------------------------------------------------
ds1 <- ds0 %>%
  dplyr::filter(
    Status == "IP Address"
    ,Finished == "TRUE"
  ) %>%
  dplyr::select(
    c(
      "ResponseId", "Status", "Progress", "Finished"
      ,names(demographic)
      ,names(varname_scale)
    )
  ) %>%
  dplyr::rename(
    "institution"     = "Q2"
    ,"class_standing" = "Q16"  # What is your class standing?
    ,"age"            = "Q17"  # What is your age?
    ,"race"           = "Q18"  # What best describes your race/ethnicity? Mark all that apply.
    ,"gender"         = "Q19"  # What best describes your gender?
    ,"political"      = "Q20"  # What best describes your political leanings?
    ,"religion"       = "Q21"  # How important is religion or spirituality to you?
    ,"student_type"   = "Q22"  # Mark all that apply to you.
    ,"field_of_study" = "Q23"  # Field of study (max = 2)?

    ,"md_replace"   = "Q7_1" # Treatment with methadone is replacing one addiction with another
    ,"md_safe"      = "Q7_2" # Treatment with methadone is safe
    ,"md_side_eff"  = "Q7_3" # Methadone has dangerous side effects
    ,"md_not_recov" = "Q7_4" # People in methadone treatment are not actually in recovery
    ,"md_bad_phys"  = "Q7_5" # Treatment with methadone is bad for you physically
    ,"md_get_high"  = "Q7_6" # Most people in methadone treatment use it to get high
    ,"md_cravings"  = "Q7_7" # Methadone helps prevent cravings for opioids
    ,"md_from_high" = "Q7_8" # Methadone helps prevent individuals from getting high

    ,"br_replace"   = "Q8_1" # Treatment with methadone is replacing one addiction with another
    ,"br_safe"      = "Q8_2" # Treatment with methadone is safe
    ,"br_side_eff"  = "Q8_3" # Methadone has dangerous side effects
    ,"br_not_recov" = "Q8_4" # People in methadone treatment are not actually in recovery
    ,"br_bad_phys"  = "Q8_5" # Treatment with methadone is bad for you physically
    ,"br_get_high"  = "Q8_6" # Most people in methadone treatment use it to get high
    ,"br_cravings"  = "Q8_7" # Methadone helps prevent cravings for opioids
    ,"br_from_high" = "Q8_8" # Methadone helps prevent individuals from getting high
    ,"nt_replace"   = "Q9_1" # Treatment with methadone is replacing one addiction with another
    ,"nt_safe"      = "Q9_2" # Treatment with methadone is safe
    ,"nt_side_eff"  = "Q9_3" # Methadone has dangerous side effects
    ,"nt_not_recov" = "Q9_4" # People in methadone treatment are not actually in recovery
    ,"nt_bad_phys"  = "Q9_5" # Treatment with methadone is bad for you physically
    ,"nt_get_high"  = "Q9_6" # Most people in methadone treatment use it to get high
    ,"nt_cravings"  = "Q9_7" # Methadone helps prevent cravings for opioids
    ,"nt_from_high" = "Q9_8" # Methadone helps prevent individuals from getting high
  )
ds1 %>% glimpse()
ds <- ds1
# ---- basic-table --------------------------------------------------------------


# ---- basic-graph --------------------------------------------------------------

# ---- survey-response -------------------------



# ---- demographics -----------------------------------------
cat("\n Sample size: ")
ds$ResponseId %>% length() %>% unique()

cat("\n\n")
cat("## Sample characteristics\n")
ds %>% describe_item("institution")
ds %>% describe_item("class_standing")
ds %>% describe_item("age")
ds %>% describe_item("race")
ds %>% describe_item("gender")
ds %>% describe_item("political")
ds %>% describe_item("religion")
ds %>% describe_item("student_type")
ds %>% describe_item("field_of_study")

# ---- substance-use ---------------------
cat("\n\n")
cat("## Concerned about use\n")
ds %>% describe_item("Q9")

cat("\n\n")
cat("## Met my goal\n")
ds %>% describe_item("Q11")

# item Q12
cat("\n\n")
cat("## What helped - items \n")
ds %>% describe_item("Q12_1")
ds %>% describe_item("Q12_2")
ds %>% describe_item("Q12_3")
ds %>% describe_item("Q12_4")
ds %>% describe_item("Q12_5")
ds %>% describe_item("Q12_6")
ds %>% describe_item("Q12_7")
ds %>% describe_item("Q12_8")
ds %>% describe_item("Q12_9")
ds %>% describe_item("Q12_10")
ds %>% describe_item("Q12_11")
ds %>% describe_item("Q12_12")
ds %>% describe_item("Q12_13")
ds %>% describe_item("Q12_14")

cat("\n\n")
cat("## What helped - summary\n")
vars_helped_goal <- paste0("Q12_",1:14)
d1 <- ds %>%
  dplyr::select(c("id",vars_helped_goal)) %>%
  # dplyr::filter(id == "R_0Oi2kFZHx1kSxMd") %>%
  # dplyr::filter(id %in% c("R_0Oi2kFZHx1kSxMd","R_0GMDW5Vmy3q3fHj")) %>%
  tidyr::gather(key = "item", value = "response", vars_helped_goal) %>%
  dplyr::select(-item) %>%
  dplyr::distinct() %>%
  dplyr::arrange(id) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate( n_responses = n() ) %>%
  dplyr::ungroup()
# d1
only_missing_response <- d1 %>%
  dplyr::filter( n_responses == 1 & response == "(Missing)")
only_nonmissing_response <- d1 %>%
  dplyr::filter( n_responses > 1) %>%
  dplyr::filter(!response == "(Missing)")

d_q12 <- only_nonmissing_response %>%
  dplyr::group_by(response) %>%
  dplyr::summarize(n_freq = n()) %>%
  dplyr::arrange(desc(n_freq))
factor_levels <- d_q12$response %>% as.character()
d_q12 <- d_q12 %>% dplyr::mutate(
  response = factor(response, levels = factor_levels),
  response = factor(response, levels = rev(levels(response)))
  )

d_q12 %>%
  ggplot(aes(x = response, y = n_freq)) +
  geom_bar(stat = "identity", alpha =  .5, fill = "salmon")+
  geom_text(aes(label = n_freq))+
  coord_flip()+
  theme_bw()+
  labs(title = paste0("Q12: ", labelled::var_label(ds$Q12_1), "\n (frequency of non-unique responses)") )

cat("\n\n")
cat("## What helped - Comments\n")
# comments to Q12
ds %>% dplyr::distinct(Q14) %>% neat()


# item Q12

ds %>% describe_item("Q13_1")
ds %>% describe_item("Q13_2")
ds %>% describe_item("Q13_3")
ds %>% describe_item("Q13_4")
ds %>% describe_item("Q13_5")
ds %>% describe_item("Q13_6")
ds %>% describe_item("Q13_7")
ds %>% describe_item("Q13_8")
ds %>% describe_item("Q13_9")
ds %>% describe_item("Q13_10")
ds %>% describe_item("Q13_11")
ds %>% describe_item("Q13_12")
ds %>% describe_item("Q13_13")

cat("\n\n")
cat("## What hindered - Summary\n")
vars_hindered_goal <- paste0("Q13_",1:13)
d1 <- ds %>%
  dplyr::select(c("id",vars_hindered_goal)) %>%
  # dplyr::filter(id == "R_0Oi2kFZHx1kSxMd") %>%
  # dplyr::filter(id %in% c("R_0Oi2kFZHx1kSxMd","R_0GMDW5Vmy3q3fHj")) %>%
  tidyr::gather(key = "item", value = "response", vars_hindered_goal) %>%
  dplyr::select(-item) %>%
  dplyr::distinct() %>%
  dplyr::arrange(id) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate( n_responses = n() ) %>%
  dplyr::ungroup()
# d1
only_missing_response <- d1 %>%
  dplyr::filter( n_responses == 1 & response == "(Missing)")
only_nonmissing_response <- d1 %>%
  dplyr::filter( n_responses > 1) %>%
  dplyr::filter(!response == "(Missing)")

d_q13 <- only_nonmissing_response %>%
  dplyr::group_by(response) %>%
  dplyr::summarize(n_freq = n()) %>%
  dplyr::arrange(desc(n_freq))
factor_levels <- d_q13$response %>% as.character()
d_q13 <- d_q13 %>% dplyr::mutate(
  response = factor(response, levels = factor_levels),
  response = factor(response, levels = rev(levels(response)))
)

d_q13 %>%
  ggplot(aes(x = response, y = n_freq)) +
  geom_bar(stat = "identity", alpha =  .5, fill = "lightblue")+
  geom_text(aes(label = n_freq))+
  coord_flip()+
  theme_bw()+
  labs(title = paste0("Q13: ", labelled::var_label(ds$Q13_1), "\n (frequency of non-unique responses)") )

cat("\n\n")
cat("## What hindered - Comments\n")
# comments to Q13
ds %>% dplyr::distinct(Q15) %>% neat()

cat("\n\n")
cat("## Craving (New)\n")
# Q16
ds %>% describe_item("Q16")


# ---- q40 ------------------
# ds %>% distinct(Q40)
# cat("\n\n")
# cat("## Craving (old)\n")
# custom_levels <- c(
#   "Moderate urge"
#   ,"None at all"
#   ,"MIld urge"
#   ,"Slight, that is, a very mild urge"
#   ,"Strong urge, but easily controlled"
#   ,"Strong urge, would have used if available"
#   ,"Strong urge and difficult to control"
#   ,"(Missing)"
# )
# var_label <- labelled::var_label(ds$Q40)
# ds <- ds %>%
#   dplyr::mutate(
#      Q40 = factor(Q40, levels = custom_levels)
#     ) %>%
#   dplyr::filter(!is.na(Q40))
# labelled::var_label(ds$Q40) <- var_label

ds %>% describe_item("Q40")


# ----- publisher --------------------
path <- "./analysis/1-first-look/demographics-substance.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # ,"word_document"
  ),
  clean=TRUE
)

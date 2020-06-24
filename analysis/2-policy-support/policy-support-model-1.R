
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-packages -----------------------------------------------------------
library(magrittr) # enables piping : %>%
library(ggplot2)
library(dplyr)
requireNamespace("tidyr")# data manipulation
requireNamespace("car")  # For it's `recode()` function.

# ---- load-sources ------------------------------------------------------------
source("./scripts/common-functions.R") # used in multiple reports
# source("./scripts/graph-presets.R") # fonts, colors, themes
source("./scripts/graphing/graph-missing.R")
baseSize = 8
# ---- declare-globals ------------------------------------------------------
config <- config::get()
#set default ggplot theme
ggplot2::theme_set(ggplot2::theme_bw())

describe_item <- function(d, varname){
  # browser()
  # d <- ds1
  # varname <- "Q2"
  variable_label <- ds_meta %>%
    dplyr::filter(q_name == varname ) %>%
    dplyr::pull(item_label)
  g <- d %>%
    TabularManifest::histogram_discrete(varname)+
    labs(
      title = paste0(varname," : ", variable_label)
    )
  return(g)
}

make_corr_matrix <- function(d,metaData,item_names){
  # d <- ds_opioid
  # metaData <- ds_meta
  # item_names <- c(q4_varnames,"total_opioid")
  #
  # browser()
  # d %>% glimpse()
  # d <- ds %>% dplyr::select(foc_01:foc_49)
  d1 <- d %>% dplyr::select(item_names)
  d2 <- d1[complete.cases(d1),] %>%
    dplyr::mutate(
      total_score = rowSums(.)
    )
  # d2 %>% glimpse()
  rownames <- metaData %>%
    dplyr::filter(q_name %in% item_names) %>%
    dplyr::mutate(display_name = paste0(q_name,"\n",q_label))

  rownames <- rownames[,"display_name"]
  rownames[nrow(rownames)+1,1]<- "total\nscore"
  rownames <- rownames %>% as.list() %>% unlist() %>% as.character()

  d3 <- sapply(d2, as.numeric)
  # d3 %>% glimpse()
  cormat <- cor(d3)
  colnames(cormat) <- rownames; rownames(cormat) <- rownames
  return(cormat)
}

make_corr_plot <- function (
  corr,
  lower="number",
  upper="circle",
  tl.pos=c("d","lt", "n"),
  diag=c("n", "l", "u"),
  bg="white",
  addgrid.col="gray", ...
){

  diag <- match.arg(diag)
  tl.pos <- match.arg(tl.pos)
  n <- nrow(corr)
  # corrplot::corrplot(corr, type="upper", method=upper, diag=TRUE, tl.pos=tl.pos, ...)
  corrplot::corrplot(corr, type="upper", method=upper, diag=TRUE, tl.pos=tl.pos)
  # corrplot::corrplot(corr, add=TRUE, type="lower", method=lower, diag=(diag == "l"), tl.pos="n", cl.pos="n", ...)
  corrplot::corrplot(corr, add=TRUE, type="lower", method=lower, diag=(diag == "l"), tl.pos="n", cl.pos="n")
  if (diag == "n" & tl.pos != "d") {
    symbols(1:n, n:1, add=TRUE, bg=bg, fg=addgrid.col,  inches=FALSE, squares=rep(1, n))
  }
}

rundown <- function(d, qn){
  # d <- ds2
  # qn = "Q4_1"
  d %>% TabularManifest::histogram_discrete(
    qn
    ,main_title = paste0( ds_meta %>% filter(q_name == qn) %>% pull(section),"\n",
                          qn, " : ", ds_meta %>% filter(q_name == qn) %>% pull(item_label)
    )
  )
}

# must provide a  recode guide to  put reseponses on a numeric scale
compute_total_score <- function(d, id_name = "ResponseId", rec_guide){
  # d <- ds_opioid
  # id_name <- "ResponseId"
  varname_scale <- setdiff(names(d),"ResponseId")
  d_out <- d %>%
    dplyr::mutate_at(varname_scale, rec_guide) %>%
    dplyr::mutate_at(varname_scale, as.character) %>%
    dplyr::mutate_at(varname_scale, as.integer) %>%
    dplyr::mutate(
      allna   = rowSums(is.na(.[varname_scale]))== length(varname_scale)
      ,anyna  = rowSums(is.na(.[varname_scale])) > 0L
      ,total_score = rowSums(.[varname_scale],na.rm = TRUE)
      ,total_score = ifelse(allna,NA,total_score)
    ) %>%
    dplyr::filter(!anyna) %>%
    dplyr::select(-allna,-anyna)
  return(d_out)
}
# ---- load-data ---------------------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
ds0 <- readr::read_rds(config$oud_first_look)
ds_meta <- readr::read_csv(config$survey_meta)

# ---- inspect-data -------------------------------------------------------------

# ---- add-new-names -----------------
demographic <- c(
  "Q2"   = "institution"
  ,"Q16" = "class_standing"  # What is your class standing?
  ,"Q17" = "age"  # What is your age?
  ,"Q18" = "race"  # What best describes your race/ethnicity? Mark all that apply.
  ,"Q19" = "sex"  # What best describes your gender?
  ,"Q20" = "political"  # What best describes your political leanings?
  ,"Q21" = "religion"  # How important is religion or spirituality to you?
  ,"Q22" = "student_type"  # Mark all that apply to you.
  ,"Q23" = "field_of_study"  # Field of study (max = 2)?
)
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
varname_scale <- c(methadone, buprenorphine, naltrexone)
selected_varnames <- c(demographic, varname_scale)

# add renaming convention
ds_rename_guide <- tibble::tibble(
  q_name = selected_varnames %>% names()
  ,item_name = selected_varnames
)
ds_meta <- dplyr::left_join(
  ds_meta
  ,ds_rename_guide
  ,by = c("q_name" = "q_name")
) %>%
  dplyr::select(q_name, item_name, dplyr::everything())

# ---- tweak-data -----------------------------------------------------------
ds2 <- ds0

ds_demo <- ds2 %>%
  select(c("ResponseId", names(demographic)))
names(ds_demo) <- c("ResponseId", demographic)


recode_opioid <- function(x){
  car::recode(var = x, recodes =
                "
  ;'Very knowledgeable'                    = 2
  ;'Somewhat knowledgeable'                = 1
  ;'I have never heard of this treatment'  = 0
  ;'I choose not to answer'                = NA
  "
  )
}
ds_opioid <- ds2 %>%
  select(c("ResponseId", starts_with("Q4_")) ) %>%
  compute_total_score(rec_guide = recode_opioid) %>%
  rename("knowledge_oud_tx" = "total_score")

recode_support <- function(x){
  car::recode(var = x, recodes =
                "
  ;'Strongly support'      = 2
  ;'Somewhat support'      = 1
  ;'Neutral/no opinion'    = 0
  ;'Somewhat oppose'       = -1
  ;'Strongly oppose'       = -2
  ;'Unsure'                                   = NA
  ;'I choose not to answer'                   = NA
  ;'I do not know what this policy is/means'  = NA
  "
  )
}

ds_support <- ds2 %>%
  select(c("ResponseId", starts_with("Q15_")) ) %>%
  compute_total_score(rec_guide = recode_support) %>%
  rename("hr_support" = "total_score")

# ---- q15-1 -------------
cat("\n SECTION Q15 \n"
    , ds_meta %>% filter(q_name == "Q15_1") %>% pull(section)
)
cat("\n From the total of ", ds2 %>% n_distinct("ResponseId"), "respondents, only ",
    ds_support %>% n_distinct("RespondentId"), " have provided complete and meaningful responses to all of the questions in section Q15")

# Responses `Unsure`, `I choose not to answer` and `I do not know what this policy is/means` have been converted to NA values in order to compute the overal score of HR policy support.

ds_support %>% TabularManifest::histogram_continuous(
  "hr_support"
  ,main_title = paste0("Total score for supporting harm reduction policies")
  ,sub_title = paste0("\n (+2)Strongly Support, (+1)Support, (0)Neutral, (-1)Oppose, (-2)Strongly Oppose"), x_title = "Support for HR policies"
  ,bin_width = 1
)+labs(caption = paste0("Based on complete and meaningful responses, N = ",ds_support %>%  n_distinct("RespondentId"),"\n value is on the interval [-14, 14]"))
skimr::skim(ds_support)

# ----- create-ds-for-model -----------------

ds_model <-
  ds_support %>% select(ResponseId, hr_support) %>%
  dplyr::left_join(ds_opioid %>% select(ResponseId,knowledge_oud_tx)) %>%
  dplyr::left_join(ds_demo ) %>%
  tidyr::drop_na()

# ----- groom-predictors --------------------

ds_model %>% group_by(institution) %>% count()
ds_model %>% group_by(sex) %>% count()
ds_model %>% group_by(class_standing) %>% count()

ds_model <- ds_model %>%
  dplyr::mutate(
    institution = forcats::fct_recode(
      institution,
      "UCF" = "University of Central Florida"
      ,"IUB" = "Indiana University-Bloomington"

    )
  )




# ---- support-1 --------
# Sex
ds_model %>%
  dplyr::left_join(ds_demo ) %>%
  filter(sex %in% c("Male", "Female")) %>%
  ggplot(aes(x = hr_support, y = ..density.., fill = sex ))+
  facet_wrap(~sex, ncol =1)+
  geom_histogram(binwidth = 1)+
  geom_density(alpha = .2)+
  labs(title = "Support of policies by sex")


ds_model %>%
  dplyr::left_join(ds_demo ) %>%
  filter(sex %in% c("Male", "Female")) %>%
  ggplot(aes(x = sex, y = hr_support, fill = sex))+
  geom_boxplot(width = .1, outlier.color = "black", color = "black")+
  # stat_summary(fun.y = median, geom = "point",  shape = 3, size = 7, color = "red")+
  stat_summary(fun.y = mean, geom = "point",  shape = 21, size = 3, fill = "white")+
  labs(title = "Support of policies by sex")

# ---- support-2 --------
ds_model %>%
  dplyr::left_join(ds_demo ) %>%
  filter(class_standing %in% c("Freshman", "Sophomore","Junior","Senior")) %>%
  ggplot(aes(x = hr_support, y = ..density.., fill = class_standing ))+
  facet_wrap(~class_standing, ncol =1)+
  geom_histogram(binwidth = 1)+
  geom_density(alpha = .2)+
  labs(title = "Support of policies by class standing")


ds_model %>%
  dplyr::left_join(ds_demo ) %>%
  filter(class_standing %in% c("Freshman", "Sophomore","Junior","Senior")) %>%
  ggplot(aes(x = class_standing, y = hr_support, fill = class_standing))+
  geom_boxplot(width = .1, outlier.color = "black", color = "black")+
  # stat_summary(fun.y = median, geom = "point",  shape = 3, size = 7, color = "red")+
  stat_summary(fun.y = mean, geom = "point",  shape = 21, size = 3, fill = "white")+
  labs(title = "Support of policies by class standing")

# ---- support-3 --------
ds_demo %>% group_by(religion) %>% count()

ds_model %>%
  dplyr::left_join(ds_demo ) %>%
  filter(religion %in% c("Very important", "Moderately important","Not important")) %>%
  ggplot(aes(x = hr_support, y = ..density.., fill = religion ))+
  facet_wrap(~religion, ncol =1)+
  geom_histogram(binwidth = 1)+
  geom_density(alpha = .2)+
  labs(title = "Support of policies by religious views")


d1 %>%
  dplyr::left_join(ds_demo ) %>%
  filter(religion %in% c("Very important", "Moderately important","Not important")) %>%
  ggplot(aes(x = religion, y = hr_support, fill = religion))+
  geom_boxplot(width = .1, outlier.color = "black", color = "black")+
  # stat_summary(fun.y = median, geom = "point",  shape = 3, size = 7, color = "red")+
  stat_summary(fun.y = mean, geom = "point",  shape = 21, size = 3, fill = "white")+
  labs(title = "Support of policies by religious views")


# ---- q4 ---------------

cat("\n SECTION Q4 \n"
    , ds_meta %>% filter(q_name == "Q4_1") %>% pull(section)
)

ds_opioid %>% TabularManifest::histogram_continuous(
  "knowledge_oud_tx"
  ,main_title = paste0("Total score: 1 - Somewhat, 2 - Very Knowledgable (Max = 18)")
  ,bin_width = 1
)




# ---- knowledge-1 -----------------

# Knowledge for OUD treatment does not appear to differ by sex
ds_opioid %>%
  dplyr::left_join(ds_demo ) %>%
  filter(sex %in% c("Male", "Female")) %>%
  ggplot(aes(x = knowledge_oud_tx, y = ..density.., fill = sex ))+
  facet_wrap(~sex, ncol =1)+
  geom_histogram(binwidth = 1)+
  geom_density(alpha = .2)+
  labs(title = "Knowledg of OUT treatment by sex")

ds_opioid %>%
  dplyr::left_join(ds_demo ) %>%
  filter(sex %in% c("Male", "Female")) %>%
  ggplot(aes(x = sex, y = knowledge_oud_tx, fill = sex))+
  geom_boxplot(width = .1, outlier.color = "black", color = "black")+
  # stat_summary(fun.y = median, geom = "point",  shape = 3, size = 7, color = "red")+
  stat_summary(fun.y = mean, geom = "point",  shape = 21, size = 3, fill = "white")+
  labs(title = "Knowledg of OUT treatment by sex")


# ---- knowledge-2 --------
ds_opioid %>%
  dplyr::left_join(ds_demo ) %>%
  filter(class_standing %in% c("Freshman", "Sophomore","Junior","Senior")) %>%
  ggplot(aes(x = knowledge_oud_tx, y = ..density.., fill = class_standing ))+
  facet_wrap(~class_standing, ncol =1)+
  geom_histogram(binwidth = 1)+
  geom_density(alpha = .2)+
  labs(title = "Knowledge of OUD treatments by class standing")


ds_opioid %>%
  dplyr::left_join(ds_demo ) %>%
  filter(class_standing %in% c("Freshman", "Sophomore","Junior","Senior")) %>%
  ggplot(aes(x = class_standing, y = knowledge_oud_tx, fill = class_standing))+
  geom_boxplot(width = .1, outlier.color = "black", color = "black")+
  # stat_summary(fun.y = median, geom = "point",  shape = 3, size = 7, color = "red")+
  stat_summary(fun.y = mean, geom = "point",  shape = 21, size = 3, fill = "white")+
  labs(title = "Knowledge of OUD treatments by class standing")


# ---- knowledge-3 --------
ds_demo %>% group_by(religion) %>% count()

ds_opioid %>%
  dplyr::left_join(ds_demo ) %>%
  filter(religion %in% c("Very important", "Moderately important","Not important")) %>%
  ggplot(aes(x = knowledge_oud_tx, y = ..density.., fill = religion ))+
  facet_wrap(~religion, ncol =1)+
  geom_histogram(binwidth = 1)+
  geom_density(alpha = .2)+
  labs(title = "Knowledge of OUD treatments by religious views")


ds_opioid %>%
  dplyr::left_join(ds_demo ) %>%
  filter(religion %in% c("Very important", "Moderately important","Not important")) %>%
  ggplot(aes(x = religion, y = knowledge_oud_tx, fill = religion))+
  geom_boxplot(width = .1, outlier.color = "black", color = "black")+
  # stat_summary(fun.y = median, geom = "point",  shape = 3, size = 7, color = "red")+
  stat_summary(fun.y = mean, geom = "point",  shape = 21, size = 3, fill = "white")+
  labs(title = "Knowledge of OUD treatments by religious views")







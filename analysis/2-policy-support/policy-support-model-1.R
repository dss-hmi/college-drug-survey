
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
  # d <- ds1
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


# Function for exploring relationship between two categorical variables
make_bi_bar_graph <- function(d, var1, var2, label1, label2, labels=F){
  # d <- dsm2
  # var1 <- "sex"
  # var2 <- "sex"
  #
  d1 <- d %>%
    group_by(.dots = c(var1, var2) )%>%
    summarize(
      n_people = n()
    ) %>%
    ungroup() %>%
    mutate(
      total = sum(n_people, na.rm =T)
    ) %>%
    group_by(.dots = var1) %>%
    mutate(
      total_1 = sum(n_people, na.rm = T)
      ,pct_1 = scales::label_percent()(total_1/total)
      ,pct_12 = scales::label_percent()(n_people/total_1)
    )
  n_total = d1 %>% pull(total) %>% unique()

  g1 <- d1 %>%
    ggplot(aes_string(x = var1, y = "n_people", fill = var2 ))+
    geom_col(position = position_dodge())+
    geom_text(aes(label = n_people),position = position_dodge(.9), vjust = 1.5, color = "white", size = 5 )+
    scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = "plasma")+
    labs( title = paste0("Sample size, N = ", n_total))
    # coord_flip()

  if(var1 == var2){
    g1 <- g1 +
      geom_text(aes_string(label = "pct_1"),position = position_dodge(.9), vjust = -.5, color = "black", size = 4)
  }else{
    g1 <- g1 +
      geom_text(aes_string(label = "pct_12"),position = position_dodge(.9), vjust = -.5, color = "black", size = 4)
  }

  g1

}
# How to use
# dsm2 %>% make_bi_bar_graph("sex","class_standing")
# dsm2 %>% make_bi_bar_graph("class_standing","sex")
# dsm2 %>% make_bi_bar_graph("sex","sex")
# dsm2 %>% make_bi_bar_graph("class_standing","class_standing")


make_bi_mosaic <- function(d, var1, var2){

  # d <- dsm2 %>% select(sex, class_standing)
  # var1 <- "class_standing"
  # var2 <- "sex"

  d1 <- d %>%
    dplyr::rename(
      "v1" = var1
      ,"v2" = var2
    ) %>%
    mutate(
      v1 = forcats::fct_drop(v1)
      ,v2 = forcats::fct_drop(v2)
    )

  mosaicplot(~v1 + v2, data = d1,
             main = paste0("Bivariate distribution between (", var1, ") and (", var2,")")
             ,xlab = var1, y = var2
             ,shade = TRUE)
}
# How to use:
# dsm2 %>% make_bi_mosaic("sex", "class_standing")
# dsm2 %>% make_bi_mosaic("sex", "over21")


# Function to conduct an independence test between two categorical variables
test_independence <- function(d, var1, var2){
  d1 <- d %>%
    dplyr::rename(
      "v1" = var1
      ,"v2" = var2
    ) %>%
    mutate(
      v1 = forcats::fct_drop(v1)
      ,v2 = forcats::fct_drop(v2)
    )

  d1 %>%
    dplyr::select(v1, v2) %>%
    sjPlot::sjtab(
      fun = "xtab"
      ,var.labels=c(var1, var2)
      ,show.row.prc=T
      ,show.col.prc=T
      ,show.summary=T
      ,show.exp=T
      ,show.legend=T
    )
  # see interpretation of Phi and V
  # http://www.people.vcu.edu/~pdattalo/702SuppRead/MeasAssoc/NominalAssoc.html
}
# How to use
# dsm2 %>% test_independence("sex", "religion")
# dsm2 %>% test_independence("religion", "class_standing")

scatter_by_groups <- function(
  d,xvar, yvar, groupvar, jitterwidth=0, jitterheight=0,  xlabel = xvar, ylabel=yvar, grouplabel=groupvar
  ){
  # d <- dsm2
  # xvar = "knowledge_oud_tx"
  # xvar = "n_tx_helpful"
  # yvar = "hr_support"
  # groupvar = "sex"

  g1 <- d %>%
  ggplot(aes_string(x = xvar, y = yvar, color = groupvar))+

    geom_point(shape = 21, size = 3, alpha = .6,
    position = position_jitter(width=jitterwidth,height = jitterheight, seed = 42))+
    scale_color_viridis_d(begin = 0, end = .6, option = "plasma")+
    geom_smooth(method="lm", se = F)+
    ggpmisc::stat_poly_eq(formula = y ~ + x ,
                          aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                          parse = TRUE)+
    labs(
      title = paste0("Relationship between (", yvar,") and (", xvar,") for different levels of (",groupvar,")"),
      caption = paste0("N = ", nrow(d)),
      x = xlabel,
      y = ylabel,
      color = grouplabel
    )
    return(g1)

}
# How to use:
# dsm2 %>% scatter_by_groups("knowledge_oud_tx","hr_support","sex")
# dsm2 %>% scatter_by_groups("knowledge_oud_tx","hr_support","sex",1,0)
# dsm2 %>% scatter_by_groups("knowledge_oud_tx","hr_support","sex",1,0,"Knowledge of OUD Tx","HR Policy Support", "Sex")
# ---- load-data ---------------------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
ds0 <- readr::read_rds(config$oud_first_look)
ds_meta <- readr::read_csv(config$survey_meta)


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
# Recode variables
ds1 <- ds0

ds_demo <- ds1 %>%
  select(c("ResponseId", names(demographic)))
names(ds_demo) <- c("ResponseId", demographic)

# - Q4 - kNOWLEDGE OF OUD TREATMENTS
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
ds_opioid <- ds1 %>%
  select(c("ResponseId", starts_with("Q4_")) ) %>%
  compute_total_score(rec_guide = recode_opioid) %>%
  rename("knowledge_oud_tx" = "total_score")


# Q6 - BELIEF IN USEFULNESS OF TH TREATMENT
recode_helpful <- function(x){
  car::recode(var = x, recodes =
                "
  ;'Very helpful'            = 1
  ;'Somewhat helpful'        = 1
  ;'Neutral'                 = 0
  ;'Not very helpful'        = 0
  ;'Not helpful at all'      = 0
  ;'Unsure'                  = NA
  ;'I choose not to answer'  = NA
  "
  )
}

varname_scale <- ds1 %>% select(starts_with("Q6_")) %>% names()
ds_helpful <- ds1 %>%
  select(c("ResponseId", starts_with("Q6_")) ) %>%
  dplyr::mutate_at(varname_scale, recode_helpful) %>%
  dplyr::mutate_at(varname_scale, as.character) %>%
  dplyr::mutate_at(varname_scale, as.integer) %>%
  mutate(
    # the number of Tx the respondent believed to be helpful (i.e "Very Helpful" or "Somewhat Helpful")
    n_tx_helpful = rowSums(.[varname_scale], na.rm =T)
    ,tx_helpful = (n_tx_helpful > 0) %>% factor()
    ,tx_helpful = relevel(tx_helpful, ref = "FALSE")
  ) %>%
  select(ResponseId, n_tx_helpful, tx_helpful)

# ds_helpful %>% group_by(n_tx_helpful) %>% count()
# ds_helpful %>% group_by(tx_helpful) %>% count()

# Q5 - Experience with Treatments
ds_exp_tx <- readr::read_rds("./data-unshared/derived/ds_exp_tx.rds") %>%
  select(ResponseId, n_tx_experience ) %>%
  mutate(
    n_tx_experience = as.integer(n_tx_experience)
  )


ds_experience <- ds1 %>%
  mutate(
    exp_w_peer_group = stringr::str_detect(Q5, "peer support group")
    ,exp_w_peer_group = tidyr::replace_na(exp_w_peer_group,0) %>% as.logical() %>% factor()
    ,exp_w_peer_group = relevel(exp_w_peer_group, ref = "FALSE")
  ) %>%
  select(ResponseId, exp_w_peer_group) %>%
  dplyr::left_join(ds_exp_tx, by = "ResponseId")
rm(ds_exp_tx)
# ds1 %>% distinct(Q5,exp_w_peer_group) %>% View()
# ds_experience %>% group_by(exp_w_peer_group) %>% count()
# ds_experience %>% group_by(n_tx_experience) %>% count()

# q15 - POLICY SUPPORT
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

# subset ds1 with Q15 questions on Policy Support
ds_support <- ds1 %>%
  select(c("ResponseId", starts_with("Q15_")) ) %>%
  compute_total_score(rec_guide = recode_support) %>%
  rename("hr_support" = "total_score")

# Assemble the predictors to be used in modeling:
# Keep only th individuals for whom the total HM_SUPPORT score could be computed (N = 725)
dsm0 <-
  ds_support %>% select(ResponseId, hr_support) %>%
  dplyr::left_join(ds_demo ) %>%
  dplyr::left_join(ds_opioid %>% select(ResponseId,knowledge_oud_tx)) %>%
  dplyr::left_join(ds_helpful) %>%
  dplyr::left_join(ds_experience)

# dsm0 %>% glimpse()
# skimr::skim(dsm0)
# dsm0 %>% make_bi_bar_graph("tx_helpful","sex")
# ----- groom-predictors --------------------
# create the dataset for modeling

dsm0 %>% group_by(institution) %>% count()
dsm0 %>% group_by(sex) %>% count()
dsm0 %>% group_by(class_standing) %>% count()
dsm0 %>% group_by(age) %>% count()
dsm0 %>% group_by(race) %>% count()
dsm0 %>% group_by(political) %>% count()
dsm0 %>% group_by(religion) %>% count()
dsm0 %>% group_by(student_type) %>% count()

dsm1 <- dsm0 %>%
  dplyr::mutate(
    institution = forcats::fct_recode(
      institution,
      "UCF" = "University of Central Florida"
      ,"IUB" = "Indiana University-Bloomington"
    )
    ,over21 = forcats::fct_recode(age
                                  ,"TRUE" = "31-40 years old"
                                  ,"TRUE" = "21-30 years old"
                                  ,"TRUE" = "51-60 years old"
                                  ,"TRUE" = "61+ years old"
                                  ,"FALSE" = "Under 20 years old"
    )
    ,over21 = relevel(over21, ref = "FALSE")
    ,nonwhite = ifelse(race == "White/Caucasian", FALSE, TRUE) %>% as.factor()
    ,leaning = forcats::fct_recode(
      political
      , "left" = "Democrat"
      , "left" = "Very liberal"
      , "left" = "Somewhat liberal"
      , "right" = "Republican"
      , "right" = "Very conservative"
      , "right" = "Somewhat conservative"
      , "middle" = "Independent/moderate"
      , "other" = "Libertarian"
      , "other" = "Other"
      , "other" = "Unsure"
      , "other" = "I choose not to answer"
    )
    ,leaning = forcats::fct_relevel(leaning, "right","middle", "left", "other")
    # ,full_time = stringr::str_detect(student_type,"I am a full-time student") %>% factor()
    ,greek = stringr::str_detect(student_type, "I am a fraternity or sorority member") %>% factor()
    ,greek = relevel(greek, ref = "FALSE")
    ,health_major = stringr::str_detect(field_of_study,"Psychology|Health sciences" ) %>% factor()
    ,health_major = relevel(health_major, ref = "FALSE")
  )
# dsm1 %>% glimpse()
# dsm1 %>% skimr::skim()
# outcome = hr_support
# predictors
# the list of predictors we want to use
predictors_000 <- c(
  "knowledge_oud_tx"
  ,"institution"
  ,"class_standing"
  ,"over21"
  ,"nonwhite"
  ,"sex"
  ,"leaning"
  ,"religion"
  ,"greek"
  ,"health_major"
  ,"n_tx_experience"          #
  ,"exp_w_peer_group"  # use this or `exp_w_1plus_oudtx`
  ,"n_tx_helpful"         # use this or `tx_helpful`
  ,"tx_helpful"        # use this or `n_tx_helpful`
)

dsm1 %>% group_by(institution) %>% count()
dsm1 %>% group_by(class_standing) %>% count()
dsm1 %>% group_by(over21) %>% count()
dsm1 %>% group_by(nonwhite) %>% count()
dsm1 %>% group_by(sex) %>% count()
dsm1 %>% group_by(leaning) %>% count()
dsm1 %>% group_by(political, leaning) %>% count() %>% arrange(leaning)

dsm1 %>% group_by(religion) %>% count()
dsm1 %>% group_by(greek) %>% count()
dsm1 %>% group_by(health_major) %>% count()

dsm1 %>% group_by(n_tx_helpful) %>% count()
dsm1 %>% group_by(tx_helpful) %>% count()


dsm1 %>% group_by(n_tx_experience) %>% count()
dsm1 %>% group_by(exp_w_peer_group) %>% count()
table(dsm1$n_tx_experience, dsm1$exp_w_peer_group)

# ---- explanatory-variables -----------------------
# the dataset with no missing values on any of the predictors
dsm2 <- dsm1 %>%
  filter(!is.na(knowledge_oud_tx)) %>%
  filter(sex %in% c("Male","Female") ) %>%
  filter(class_standing %in% c("Freshman", "Sophomore","Junior","Senior")) %>%
  filter(religion %in% c("Very important", "Moderately important", "Not important")) %>%
  mutate(
    sex = forcats::fct_drop(sex)
    ,class_standing = forcats::fct_drop(class_standing)
    ,religion = forcats::fct_drop(religion)
  ) %>%
  select("ResponseId","hr_support",predictors_000)
skimr::skim(dsm2)

# ----- explanatory-variables-1 --------------------
skimr::skim(dsm1)

# ---- target-outcome -------------
cat("\n SECTION Q15 \n"
    , ds_meta %>% filter(q_name == "Q15_1") %>% pull(section)
)
cat("\n From the total of ", ds1 %>% n_distinct("ResponseId"), "respondents, only ",
    ds_support %>% n_distinct("RespondentId"), " have provided responses to all of the questions in section Q15.\n
    Responses `Unsure`, `I choose not to answer` and `I do not know what this policy is/means` have been converted to NA values in order to compute the overall score of HR policy support.")

# Responses `Unsure`, `I choose not to answer` and `I do not know what this policy is/means` have been converted to NA values in order to compute the overall score of HR policy support.

ds_support %>% TabularManifest::histogram_continuous(
  "hr_support"
  ,main_title = paste0("Total score for supporting harm reduction policies")
  ,sub_title = paste0("\nResponse values: (-2)Strongly Oppose, (-1)Oppose, (0)Neutral, (+1)Support, (+2)Strongly Support"), x_title = "Support for HR policies"
  ,bin_width = 1
)+labs(caption = paste0("Based on complete and meaningful responses, N = ",ds_support %>%  n_distinct("RespondentId"),"\n value is on the interval [-14, 14]"))
# skimr::skim(ds_support)
# ds_model %>% distinct(health_major, field_of_study) %>% View()

varname_scale <- ds1 %>% select(starts_with("Q15_")) %>% names()
ds_support_full <- ds1 %>%
  select(ResponseId, varname_scale) %>%
  mutate_at(varname_scale, recode_support) %>%
  mutate_at(varname_scale, as.integer) %>%
  dplyr::left_join(ds_support %>% select(ResponseId, hr_support))

# ds_meta %>% filter(q_name %in% varname_scale) %>% pull(q_label)
# ds_support_full %>% names()
skimr::skim(ds_support_full)


# ---- predictors-0 ---------------------------
# some predictors exist almost on a numeric scale

dsm2 %>% scatter_by_groups("n_tx_experience", "hr_support", "sex")
dsm2 %>% scatter_by_groups("n_tx_helpful", "hr_support", "sex")


# ----- ---------
dsm2 %>% make_bi_bar_graph("institution", "sex")
dsm2 %>% make_bi_mosaic("institution", "sex")
dsm2 %>% test_independence("institution", "sex")


dsm2 %>% make_bi_bar_graph("class_standing", "sex")
dsm2 %>% make_bi_bar_graph("sex", "class_standing")
dsm2 %>% make_bi_mosaic("class_standing", "sex")
dsm2 %>% test_independence("class_standing", "sex")
# ----- define-modeling-functions --------------------

run_regression <- function(d,p){
  # d <- dsm2
  p <- predictors_00
  # outcome <- "hr_support ~ "
  #
  # browser()
  ls_out <- list()
  eq_formula <- as.formula(paste0(outcome, paste(p, collapse = " + ") ) )

  model <- stats::glm(
    formula = eq_formula
    # ,family = "binomial"
    ,family=gaussian(link="identity")
    ,data = d %>%
      select(-ResponseId )
  )

  ls_out[["equation"]] <- eq_formula
  ls_out[["model"]] <- model

    return(ls_out)
}
# How to use
# lsm00 <- ds_modeling %>% run_regression(predictors_00)
# model <- lsm00$model

tabulate_coefficients <- function(
  model_object
){
  # model_object <- lsm00$model
  # browser()
  (cf <- summary(model_object)$coefficients)
  # (cf <- model_object$coefficients)
  # (ci <- exp(cbind(coef(model_object), confint(model_object))))
  # (ci <- exp(cbind(coef(model_object), confint(model_object))))

  # if(ncol(ci)==2L){
  #   (ci <- t(ci)[1,])
  #   ds_table <- cbind.data.frame("coef_name" = rownames(cf), cf,"V1"=NA,"2.5 %" = ci[1], "97.5 %"=ci[2])
  # }else{
  # ds_table <- cbind.data.frame("coef_name" = rownames(cf), cf,ci)
  # }
  ds_table <- cbind.data.frame("coef_name" = rownames(cf), cf)
  row.names(ds_table) <- NULL
  # ds_table <- plyr::rename(ds_table, replace = c(
  #   "Estimate" = "estimate",
  #   "Std. Error"="sderr",
  #   # "z value" ="zvalue",
  #   "t value" ="tvalue",
  #   # "Pr(>|z|)"="pvalue"
  #   "Pr(>|t|)"="pvalue"
  #   # "V1"="odds",
  #   # "2.5 %"  = "ci95_low",
  #   # "97.5 %"  ="ci95_high"
  # ))
  # ds_table$sign_ <- cut(
  #   x = ds_table$`Pr(>|t|)`,
  #   breaks = c(-Inf, .001, .01, .05, .10, Inf),
  #   labels = c("<=.001", "<=.01", "<=.05", "<=.10", "> .10"), #These need to coordinate with the color specs.
  #   right = TRUE, ordered_result = TRUE
  # )
  ds_table$sign <- cut(
    x = ds_table$`Pr(>|t|)`,
    breaks = c(-Inf, .001, .01, .05, .10, Inf),
    labels = c("***", "**", "*", ".", " "), #These need to coordinate with the color specs.
    right = TRUE, ordered_result = TRUE
  )
  # prepare for display
  ds_table$`Estimate` <- gsub("^([+-])?(0)?(\\.\\d+)$", "\\1\\3", round(ds_table$`Estimate`, 2))
  ds_table$`Std. Error` <- gsub("^([+-])?(0)?(\\.\\d+)$", "\\1\\3", round(ds_table$`Std. Error`, 2))
  ds_table$`t value` <- gsub("^([+-])?(0)?(\\.\\d+)$", "\\1\\3", round(ds_table$`t value`, 3))
  # ds_table$z <- gsub("^([+-])?(0)?(\\.\\d+)$", "\\1\\3", round(ds_table$zvalue, 3))
  ds_table$`Pr(>|t|)` <- gsub("^([+-])?(0)?(\\.\\d+)$", "\\1\\3", round(ds_table$`Pr(>|t|)`, 4))
  # ds_table$est <- gsub("^([+-])?(0)?(\\.\\d+)$", "\\1\\3", round(ds_table$estimate, 2))
  # ds_table$se <- gsub("^([+-])?(0)?(\\.\\d+)$", "\\1\\3", round(ds_table$sderr, 2))
  # ds_table$t <- gsub("^([+-])?(0)?(\\.\\d+)$", "\\1\\3", round(ds_table$tvalue, 3))
  # # ds_table$z <- gsub("^([+-])?(0)?(\\.\\d+)$", "\\1\\3", round(ds_table$zvalue, 3))
  # ds_table$p <- gsub("^([+-])?(0)?(\\.\\d+)$", "\\1\\3", round(ds_table$pvalue, 4))
  # ds_table$p <- as.numeric(round(ds_table$pvalue, 4))
  # ds_table$odds <- gsub("^([+-])?(0)?(\\.\\d+)$", "\\1\\3", round(ds_table$odds, 2))
  # ds_table$odds_ci <- paste0("(",
  #                            gsub("^([+-])?(0)?(\\.\\d+)$", "\\1\\3", round(ds_table$ci95_low,2)), ",",
  #                            gsub("^([+-])?(0)?(\\.\\d+)$", "\\1\\3", round(ds_table$ci95_high,2)), ")"
  # )

  # ds_table$sign_ <- cut(
  #   x = ds_table$`Pr(>|t|)`,
  #   breaks = c(-Inf, .001, .01, .05, .10, Inf),
  #   labels = c("<=.001", "<=.01", "<=.05", "<=.10", "> .10"), #These need to coordinate with the color specs.
  #   right = TRUE, ordered_result = TRUE
  # )
  # ds_table$sign <- cut(
  #   x = ds_table$`Pr(>|t|)`,
  #   breaks = c(-Inf, .001, .01, .05, .10, Inf),
  #   labels = c("***", "**", "*", ".", " "), #These need to coordinate with the color specs.
  #   right = TRUE, ordered_result = TRUE
  # )
  # ds_table$display_odds <- paste0(ds_table$odds," ",ds_table$sign , "\n",  ds_table$odds_ci)

  # ds_table <- ds_table %>%
  #   dplyr::select_(
  #     "sign",
  #     "coef_name",
  #     # "odds",
  #     # "odds_ci",
  #     "est",
  #     "se",
  #     "p",
  #     "sign_"
  #   )

  return(ds_table)
}
# How to use
# model %>% table_of_coefficients()

get_rsquared <- function(m){
  cat("R-Squared, Proportion of Variance Explained = ",
      scales::percent((1 - (summary(m)$deviance/summary(m)$null.deviance)),accuracy = .01)
      , "\n")
}
# How to use
# model_1glm %>% get_rsquared()

get_model_fit <- function(m){
  cat("MODEL FIT",
      "\nChi-Square = ", with(m, null.deviance - deviance),
      "\ndf = ", with(m, df.null - df.residual),
      "\np-value = ", with(m, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)),"\n"
  )
}
# How to use
# model_1glm %>% get_model_fit()

# ---- m00 ------------------
outcome <- "hr_support ~ "
predictors_00 <- c(
  "knowledge_oud_tx"
  ,"institution"
  ,"class_standing"
  ,"over21"
  ,"nonwhite"
  ,"sex"
  ,"leaning"
  ,"religion"
  ,"greek"
  ,"health_major"
  ,"n_tx_experience"
  ,"exp_w_peer_group"
  ,"n_tx_helpful"
  ,"tx_helpful"
)
lsm00 <- dsm2 %>% run_regression(predictors_00)
model <- lsm00$model
print(model$formula, showEnv = F)
model %>% tabulate_coefficients() %>% arrange(`Pr(>|t|)`)
model %>% get_rsquared()
model %>% get_model_fit()
summary(model) %>% print()
exp(cbind(coef = coef(model), confint(model))) #%>% neat()
# summary(model)$coefficients %>% neat(output_format = "pandoc") %>% print()
# https://datascienceplus.com/perform-logistic-regression-in-r/
anova(model, test="Chisq") %>% print()


# ----- individual-predictors --------------------
outcome <- "hr_support ~"
# modeling the outcome using a single predictor

l <- dsm2 %>% run_regression("knowledge_oud_tx")
print(l$model$formula, showEnv = F)
l$model %>% get_rsquared() # 0.55%
l$model %>% tabulate_coefficients()

l <- dsm2 %>% run_regression("institution")
print(l$model$formula, showEnv = F)
l$model %>% get_rsquared() # 0.45%
l$model %>% tabulate_coefficients()

l <- ds_model %>%
  filter(class_standing %in% c("Freshman", "Sophomore","Junior","Senior")) %>%
  mutate(
    class_standing = forcats::fct_drop(class_standing)
  ) %>%
  mutate(
      class_standing = forcats::fct_relevel(
        class_standing,
        "Freshman", "Sophomore","Junior","Senior"
      )
  ) %>%
  run_regression("class_standing")
l$model %>% get_rsquared() # 3.07%
l$model %>% make_result_table()

l <- ds_model %>% run_regression("over21")
l$model %>% get_rsquared() # 0.45%
l$model %>% make_result_table()


l <- ds_model %>% run_regression("nonwhite")
l$model %>% get_rsquared() # 0.29%
l$model %>% make_result_table()
ds_model %>% group_by(nonwhite) %>% count()

l <- ds_model %>%
  filter(sex %in% c("Male","Female") ) %>%
  run_regression("sex")
l$model %>% get_rsquared() # 0.64%
l$model %>% make_result_table()

l <- ds_model %>%
  # filter(religion %in% c("Male","Female") ) %>%
  run_regression("religion")
l$model %>% get_rsquared() # 0.64%
l$model %>% make_result_table()

ds_model %>% group_by(religion) %>% count()

# ---- m0 ------------------
outcome <- "hr_support ~ "
predictors_0 <- c(
  "knowledge_oud_tx"
)
lsm0 <- ds_model %>% run_regression(predictors_0)
model <- lsm0$model
print(model$formula, showEnv = F)
model %>% get_rsquared()
model %>% get_model_fit()
summary(model) %>% print()
summary(model)$coefficients %>% neat(output_format = "pandoc") %>% print()
# https://datascienceplus.com/perform-logistic-regression-in-r/
anova(model, test="Chisq") %>% print()

# ---- m1 ------------------
ds_m1 <- ds_model %>%
  filter(sex  %in% c("Male", "Female")) %>%
  mutate(
    sex = relevel(sex , ref = "Male")
  )
outcome <- "hr_support ~ "
predictors_1 <- c(
  "knowledge_oud_tx"
  ,"sex"
)
lsm1 <- ds_m1 %>% run_regression(predictors_1)
model <- lsm1$model
print(model$formula, showEnv = F)
model %>% get_rsquared()
model %>% get_model_fit()
summary(model) %>% print()
# summary(model)$coefficients %>% neat(output_format = "pandoc") %>% print()
# https://datascienceplus.com/perform-logistic-regression-in-r/
anova(model, test="Chisq") %>% print()



# ---- model-1 ----------------------
model_1glm <- stats::glm(
  formula = hr_support ~ knowledge_oud_tx
  ,family = gaussian(link = "identity")
  , data = ds_model
)

model_1lm <- stats::lm(hr_support ~ knowledge_oud_tx,data=ds_model)
model_1 %>% get_regression_table()
model_1 %>% summary()



model_1glm

model_object <- model_1glm





ds_model <- ds_model %>%
  filter(sex %in% c("Female","Male")) %>%
  filter(class_standing %in% c("Freshman", "Sophomore","Junior","Senior"))



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








# ----- publisher --------------------
path <- "./analysis/2-policy-support/policy-support-model-1.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # ,"word_document"
  ),
  clean=TRUE
)

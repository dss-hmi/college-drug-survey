
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
    dplyr::filter(item_name == varname ) %>%
    dplyr::pull(item)
  g <- d %>%
    TabularManifest::histogram_discrete(varname)+
    labs(
      title = paste0(varname," : ", variable_label)
    )
  return(g)


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

make_corr_matrix <- function(d,metaData,item_names){
  # d <- dsn
  # metaData <- dto$metaData
  # item_names <- varname_n_scale
  #
  # d %>% glimpse()
  # d <- ds %>% dplyr::select(foc_01:foc_49)
  d1 <- d %>% dplyr::select_(.dots=item_names)
  d2 <- d1[complete.cases(d1),] %>%
    dplyr::mutate(
      total_score = rowSums(.)
    )
  # d2 %>% glimpse()
  rownames <- metaData %>%
    dplyr::filter(item_name %in% item_names) %>%
    dplyr::mutate(display_name = paste0(item_name,"\n",item_label))

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
# ---- load-data ---------------------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
ds0 <- readr::read_csv(config$oud_survey)
ds_meta <- readr::read_csv(config$survey_items)

# ---- inspect-data -------------------------------------------------------------

# ---- add-new-names -----------------
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
# recode into proper factors
# Q4
lvl_knowledge <- c(
   "Very knowledgeable"
  ,"Somewhat knowledgeable"
  ,"I've never heard of this treatment"
  ,"I choose not to answer"
)
# Q13, Q14
lvl_knowledge2 <- c(
  "Very knowledgeable"
  ,"Somewhat knowledgeable"
  ,"Not very knowledgeable"
  ,"Never heard of it"
  ,"I choose not to answer"
)
# Q6
lvl_helpful <- c(
    "Very helpful"
  ,"Somewhat helpful"
  ,"Neutral"
  ,"Not very helpful"
  ,"Not helpful at all"
  ,"Unsure"
  ,"I choose not to answer"
)

# Q11, Q12
lvl_common <- c(
  "Very common"
  ,"Somewhat common"
  ,"Rare"
  ,"None existent"
  ,"Unsure"
  ,"I choose not to answer"
)

# Q7, Q8, Q9, Q10
lvl_agreement <- c(
  "Strongly agree"
  ,"Somewhat agree"
  ,"Neutral"
  ,"Somewhat disagree"
  ,"Strongly disagree"
  ,"Unsure"
  ,"I choose not to answer"
)
# Q15
lvl_support <- c(
   "Strongly support"
  ,"Somewhat support"
  ,"Neutral/no opinion"
  ,"Somewhat oppose"
  ,"Strongly oppose"
  ,"Unsure"
  ,"I choose not to answer"
  ,"I don't know what this policy is/means"
)
#Q16
lvl_class_standing <- c(
  "Graduate student/Professional student"
  ,"Senior"
  ,"Junior"
  ,"Sophomore"
  ,"Freshman"
  ,"Non-degree seeking"
  ,"I choose not to answer"
)
lvl_age <- c(
   "61+ years old"
  ,"51-60 years old"
  ,"31-40 years old"
  ,"21-30 years old"
  ,"Under 20 years old"
)
lvl_gender <- c(
  "Female"
  ,"Male"
  ,"Other"
  ,"I choose not to answer"
)
lvl_political <- c(
   "Democrat"
  ,"Republican"
  ,"Independent/moderate"
  ,"Libertarian"
  ,"Very conservative"
  ,"Very liberal"
  ,"Somewhat conservative"
  ,"Somewhat liberal"
  ,"Other"
  ,"Unsure"
  ,"I choose not to answer"
)
lvl_religion <- c(
   "Very important"
  ,"Moderately important"
  ,"Not important"
  ,"I choose not to answer"
  ,"Unsure"
)
lvl_institution <- c(
  "Indiana University-Bloomington"
  ,"University of Central Florida"
  ,"Other"
)
ds1 <- ds0 %>%
  dplyr::mutate_at(vars(starts_with("Q4"))  ,~factor(.,levels = lvl_knowledge)) %>%
  dplyr::mutate_at(vars(starts_with("Q13")) ,~factor(.,levels = lvl_knowledge2)) %>%
  dplyr::mutate_at(vars(starts_with("Q14")) ,~factor(.,levels = lvl_knowledge2)) %>%
  dplyr::mutate_at(vars(starts_with("Q6"))  ,~factor(.,levels = lvl_helpful)) %>%
  dplyr::mutate_at(vars(starts_with("Q11")) ,~factor(.,levels = lvl_common)) %>%
  dplyr::mutate_at(vars(starts_with("Q12")) ,~factor(.,levels = lvl_common)) %>%
  dplyr::mutate_at(vars(starts_with("Q7"))  ,~factor(.,levels = lvl_agreement)) %>%
  dplyr::mutate_at(vars(starts_with("Q8"))  ,~factor(.,levels = lvl_agreement)) %>%
  dplyr::mutate_at(vars(starts_with("Q9"))  ,~factor(.,levels = lvl_agreement)) %>%
  dplyr::mutate_at(vars(starts_with("Q10")) ,~factor(.,levels = lvl_agreement)) %>%
  dplyr::mutate_at(vars(starts_with("Q15")) ,~factor(.,levels = lvl_support)) %>%
  dplyr::mutate(
     Q2 = factor(Q2, levels = lvl_institution)
    ,Q16 = factor(Q16, levels = lvl_class_standing)
    ,Q17 = factor(Q17, levels = lvl_age)
    ,Q19 = factor(Q19, levels = lvl_gender)
    ,Q20 = factor(Q20, levels = lvl_political)
    ,Q21 = factor(Q21, levels = lvl_religion)

  )
# too granular
# ds1 %>% group_by(Q5)  %>% count() # too granular to factorize, needs grouping
# ds1 %>% group_by(Q18) %>% count() # too granular to factorize, needs grouping
# ds1 %>% group_by(Q21) %>% count() # too granular to factorize, needs grouping
# ds1 %>% group_by(Q22) %>% count() # too granular to factorize, needs grouping
# ds1 %>% group_by(Q23) %>% count() # too granular to factorize, needs grouping
#
# ds1 %>% glimpse()

# ---- tweak-data-2 ---------------

# ds2 <- ds1 %>%
#   dplyr::select(
#     c(
#       "ResponseId", "Status", "Progress", "Finished"
#       ,names(demographic)
#       ,names(varname_scale)
#     )
#   )

# ---- basic-table --------------------------------------------------------------


# ---- basic-graph --------------------------------------------------------------


# ---- survey-response  -------------------------
cat("Initial responses, N = ", ds0 %>% n_distinct("ResponseId"))
ds0 %>% group_by(Status) %>% count() %>% neat()
ds0 <- ds0 %>% filter(Status == "IP Address")
cat("After keeping only `IP Address`\n",
    "Remaining responses, N =", ds0 %>% n_distinct("ResponseId"))


ds0 %>% group_by(Finished) %>% count() %>% neat()
ds0 <- ds0 %>% filter(Finished)
cat("After keeping only those that finished the survey\n",
    "Remaining responses, N =", ds0 %>% n_distinct("ResponseId"))

ds0 %>% group_by(UserLanguage) %>% count() %>% neat()

ds_meta %>% filter(q_name == "Q1") %>% pull(item_label)
ds0 %>% group_by(Q1) %>% count() %>% neat()
ds0 <- ds0 %>% filter(Q1 == "Yes")
cat("After keeping only those older than 18 years of age\n",
    "Remaining responses, N =", ds0 %>% n_distinct("ResponseId"))

ds0 %>%
  mutate(
    date = lubridate::date(RecordedDate)
  ) %>%
  ggplot(aes(x = date) )+
  geom_bar()+
  labs(
    title = "Date of response collection"
    ,x = "2019"
    ,y = "Number of responses"
  )

d <- ds0 %>%
  # arrange(`Duration (in seconds)`) %>%
  mutate(
    minutes = `Duration (in seconds)`/60
    ,hours = `Duration (in seconds)`/60 / 60
    ,days = `Duration (in seconds)`/60 / 60 / 24
  ) %>%
  arrange(minutes) %>%
  select(minutes, hours, days) %>%
  dplyr::mutate(
    id = row_number()
  )
d %>% filter(hours > 1) %>%
  TabularManifest::histogram_continuous(
    "hours"
    ,bin_width = 1
    ,main_title = paste0(
      "Repondends who took more than 1 hour to complete the survey ( N = "
      ,d %>% filter(hours  > 1 ) %>% count() %>% pull(n), " )"
    )
  )
d %>% filter(hours < 1) %>%
  TabularManifest::histogram_continuous(
    "minutes"
    ,main_title = paste0(
      "Repondends who completed the survey within 1 hour or less ( N = "
      ,d %>% filter(hours <= 1) %>% count() %>% pull(n), " )"
    )
  )

ds0 <- ds0 %>% filter(`Duration (in seconds)` < 60*60 )
cat("After keeping only those who completed the survey within 1 hour\n",
    "Remaining responses, N =", ds0 %>% n_distinct("ResponseId"))



stem(ds0$`Duration (in seconds)`)

# ---- demographics -----------------------------------------
cat("\n Sample size: ")
ds1$ResponseId %>% length() %>% unique()

cat("\n\n")
cat("## Sample characteristics\n")

ds1 %>% describe_item("Q2")  #  = "institution"
ds1 %>% describe_item("Q16") # = "class_standing"
ds1 %>% describe_item("Q17") # = "age"

cat("\n",
  "Q18: ", (ds_meta %>% filter(item_name == "Q18") %>% pull(item)), "\n"
)
ds1 %>% dplyr::group_by(Q18) %>% count() %>% arrange(desc(n)) %>% neat()
ds1 %>% dplyr::group_by(Q18) %>% count() %>% arrange(desc(n)) %>%
  dplyr::mutate(
    race = ifelse(n > 10, Q18, "Other")
  ) %>%
  dplyr::group_by(race) %>%
  dplyr::summarize(
    n = sum(n, na.rm = T)
  ) %>% dplyr::ungroup() %>% dplyr::arrange(desc(n)) %>%

  dplyr::filter(n>7) %>%
  ggplot(aes(x=reorder(race,n) ,y=n ) )+
  geom_col(fill = "salmon", alpha = .3, color = "black")+
  coord_flip()
ds1 %>% describe_item("Q19") # = "gender"
ds1 %>% describe_item("Q20") # = "political"
ds1 %>% describe_item("Q21") # = "religion"
ds1 %>% dplyr::group_by(Q22) %>% count() %>% arrange(desc(n))%>% neat()# = "student_type"
cat("\n",
    "Q23: ", (ds_meta %>% filter(item_name == "Q23") %>% pull(item)), "\n"
)
ds1 %>% dplyr::group_by(Q23) %>% count() %>% arrange(desc(n))%>% neat()# = "student_type"


# ---- methadone ---------------------
# cat("\n\n# Item Analysis: Methadone")
# # for(item_i in varname_e_scale[1:3]){
# for(item_i in names(methadone) ){
#   # item_i <- "Q7_1"
#   item_label <- ds_meta %>%
#     dplyr::filter(item_name == item_i ) %>%
#     dplyr::pull(short_label)
#   item_description <- ds_meta %>%
#     dplyr::filter(item_name == item_i ) %>%
#     dplyr::pull(item)
#
#   cat("\n\n")
#   cat("## ", item_i," - ", item_label)
#   # labelled::var_label(ds[item_i])
#   cat("\n\n")
#   item_description %>% print()
#   cat("\n\n")
#   ds1 %>% describe_item(item_i) %>% print()
#   cat("\n\n")
# }



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

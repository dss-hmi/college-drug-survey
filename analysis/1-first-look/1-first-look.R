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
# ds0 %>% TabularManifest::histogram_discrete("Status")
# ds0 %>% TabularManifest::histogram_continuous("Progress")
# # ds0$`Duration (in seconds)` %>% summary()
# ds0 %>% TabularManifest::histogram_discrete("Finished")

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
q_vars <- grep("^Q7",names(ds0), value = T)
d1 <- ds0 %>% select(q_vars) %>% filter(!is.na(Q7_1))

lvl_agreement <- c(
  "2"   =  "Strongly agree"
  ,"1"  = "Somewhat agree"
  ,"0"  = "Neutral"
  ,"-1" = "Somewhat disagree"
  ,"-2" = "Strongly disagree"
  ,"99" = "Unsure"
  ,"98" = "I choose not to answer"
)


assign_levels <- function(x, lvl_codes){
  x <- factor(x, levels = lvl_codes)
  x <- forcats::fct_recode(x, lvl_agreement)
}

d2 <- d1 %>%
  dplyr::mutate_at(vars(q_vars), funs(assign_levels))
d2 %>% glimpse()


lvl_agreement_recode <-
"
'Strongly agree'          = '2'
;'Somewhat agree'         = '1'
;'Neutral'                = '0'
;'Somewhat disagree'      = '-1'
;'Strongly disagree'      = '-2'
;'Unsure'                 = '99'
;'I choose not to answer' = '98'
"
lvl_agreement <- c(
    "Strongly agree"         = "2"
  , "Somewhat agree"         = "1"
  , "Neutral"                = "0"
  , "Somewhat disagree"      = "-1"
  , "Strongly disagree"      = "-2"
  , "Unsure"                 = "99"
  , "I choose not to answer" = "98"
)
recode_values <- function(v, rec_guide){
  car::recode(v,rec_guide)
}
recode_levels <- function(v, lvl_guide){
  # factor(v, levels = as.integer(lvl_guide), labels = names(lvl_guide))
  factor(v, levels = lvl_guide, labels = names(lvl_guide))
}
q_vars <- grep("^Q7",names(ds0), value = T)
ds1 <- ds0 %>%
  dplyr::select(q_vars) %>%
  dplyr::filter(!is.na(Q7_1)) %>%
  dplyr::mutate_at(q_vars, ~recode_values(., rec_guide = lvl_agreement_recode) ) %>%
  dplyr::mutate_at(q_vars, as.character) %>%
  dplyr::mutate_at(q_vars, ~recode_levels(., lvl_guide = lvl_agreement))

d <- ds1 %>% select(Q7_1, Q7_2) %>%
  dplyr::mutate(
    q71 = as.integer(Q7_1)
  )

for(i in q_vars){
  ds_meta %>% filter(q_name == i) %>% pull(item_label) %>% print()
  ds0 %>% group_by(.dots = i) %>% count() %>% print()
}
q_vars <- grep("^Q4",names(ds0), value = T)
ds2 <- ds1 %>%
  # dplyr::mutate_at(vars(starts_with("Q4")) ,~factor(.,levels = lvl_knowledge))
  dplyr::mutate_at(vars(starts_with("Q7")) ,~factor(.,levels = lvl_agreement))


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
# ---- -------------

# ---- tweak-data-2 ---------------

ds2 <- ds1 %>%
  dplyr::select(
    c(
      "ResponseId", "Status", "Progress", "Finished"
      ,names(demographic)
      ,names(varname_scale)
    )
  )

a <- c("aa","bb", "cc")
rec <- " 'aa' = 'AA' "
car::recode(a,rec )




d <- ds2 %>% select(names(methadone) )
d <- ds2 %>% ryouready::recode2(vars = c("Q7_1","Q7_2"), recodes = recode_guide)
library(car)
library(ryouready)
recode2(ds2,vars = 12:13, recodes = recode_guide )

dd$Q7_1 <- car::recode(dd$Q7_1 , recodes = recode_guide)

region,
"
      'central'  ='CN'
      ;'southeast'='SE'
      ;'northeast'='NE  '
      "
)

a <- attitude
rec <- "0:50=1; 51:70=2; 60:100=3; else=NA"
recode2(a, recodes=rec)
recode2(a, vars=1:2, recodes=rec)
recode2(a, vars=c("rating", "complaints"), recodes=rec)


for(i in vv ){
  dd[,i] <- car::recode(dd[,i],  `Strongly agree` = "1")
}
dplyr::recode("Strongly agree" = "1")

  ds2 %>% glimpse()
  #
  # dplyr::rename(
  #   "institution"     = "Q2"
  #   ,"class_standing" = "Q16"  # What is your class standing?
  #   ,"age"            = "Q17"  # What is your age?
  #   ,"race"           = "Q18"  # What best describes your race/ethnicity? Mark all that apply.
  #   ,"gender"         = "Q19"  # What best describes your gender?
  #   ,"political"      = "Q20"  # What best describes your political leanings?
  #   ,"religion"       = "Q21"  # How important is religion or spirituality to you?
  #   ,"student_type"   = "Q22"  # Mark all that apply to you.
  #   ,"field_of_study" = "Q23"  # Field of study (max = 2)?
  #
  #   ,"md_replace"   = "Q7_1" # Treatment with methadone is replacing one addiction with another
  #   ,"md_safe"      = "Q7_2" # Treatment with methadone is safe
  #   ,"md_side_eff"  = "Q7_3" # Methadone has dangerous side effects
  #   ,"md_not_recov" = "Q7_4" # People in methadone treatment are not actually in recovery
  #   ,"md_bad_phys"  = "Q7_5" # Treatment with methadone is bad for you physically
  #   ,"md_get_high"  = "Q7_6" # Most people in methadone treatment use it to get high
  #   ,"md_cravings"  = "Q7_7" # Methadone helps prevent cravings for opioids
  #   ,"md_from_high" = "Q7_8" # Methadone helps prevent individuals from getting high
  #
  #   ,"br_replace"   = "Q8_1" # Treatment with methadone is replacing one addiction with another
  #   ,"br_safe"      = "Q8_2" # Treatment with methadone is safe
  #   ,"br_side_eff"  = "Q8_3" # Methadone has dangerous side effects
  #   ,"br_not_recov" = "Q8_4" # People in methadone treatment are not actually in recovery
  #   ,"br_bad_phys"  = "Q8_5" # Treatment with methadone is bad for you physically
  #   ,"br_get_high"  = "Q8_6" # Most people in methadone treatment use it to get high
  #   ,"br_cravings"  = "Q8_7" # Methadone helps prevent cravings for opioids
  #   ,"br_from_high" = "Q8_8" # Methadone helps prevent individuals from getting high
  #
  #   ,"nt_replace"   = "Q9_1" # Treatment with methadone is replacing one addiction with another
  #   ,"nt_safe"      = "Q9_2" # Treatment with methadone is safe
  #   ,"nt_side_eff"  = "Q9_3" # Methadone has dangerous side effects
  #   ,"nt_not_recov" = "Q9_4" # People in methadone treatment are not actually in recovery
  #   ,"nt_bad_phys"  = "Q9_5" # Treatment with methadone is bad for you physically
  #   ,"nt_get_high"  = "Q9_6" # Most people in methadone treatment use it to get high
  #   ,"nt_cravings"  = "Q9_7" # Methadone helps prevent cravings for opioids
  #   ,"nt_from_high" = "Q9_8" # Methadone helps prevent individuals from getting high
  # )
ds2 %>% glimpse()

# ---- basic-table --------------------------------------------------------------


# ---- basic-graph --------------------------------------------------------------
# q5 v q15

d <- ds1 %>%
  dplyr::select
  ggplot(x =)


# ---- survey-response -------------------------


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
cat("\n\n# Item Analysis: Methadone")
# for(item_i in varname_e_scale[1:3]){
for(item_i in names(methadone) ){
  # item_i <- "Q7_1"
  item_label <- ds_meta %>%
    dplyr::filter(item_name == item_i ) %>%
    dplyr::pull(short_label)
  item_description <- ds_meta %>%
    dplyr::filter(item_name == item_i ) %>%
    dplyr::pull(item)

  cat("\n\n")
  cat("## ", item_i," - ", item_label)
  # labelled::var_label(ds[item_i])
  cat("\n\n")
  item_description %>% print()
  cat("\n\n")
  ds1 %>% describe_item(item_i) %>% print()
  cat("\n\n")
}



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

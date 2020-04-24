# this script imports the raw data described in this shared document
# https://drive.google.com/file/d/10idMxy8eX8nTHr6wr2Q40x4XOP3Y5ck7/view
# and prepares a state of data used as a standard point of departure for any subsequent reproducible analytics

# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(
#   script = "./manipulation/0-greeter.R",
#   output = "./manipulation/stitched-output/0-greeter.md"
# )
# this command is typically executed by the ./manipulation/governor.R

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ---------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes

path_file <- "data-unshared/raw/College Student Survey regarding Opioid Use Disorder Treatment_May 6, 2019_10.04.csv"
# ---- declare-globals -----------------------------------------------

# ---- load-data -----------------------------------------------------
ds0 <- readr::read_csv(path_file,skip = 3, col_names = F)
d_names <- readr::read_csv(path_file,col_names = F, n_max = 2)
# ---- define-utility-functions ---------------
ds_names <- d_names %>% t %>% tibble::as_tibble() %>%
  dplyr::rename(
    "q_name" = "V1"
    ,"item_full_name" = "V2"
  ) %>%
  dplyr::mutate(
    item_full_name = gsub("\\n"," ",item_full_name)
    ,section = gsub("(.+) - (.+)","\\1",item_full_name)
    ,item_label = gsub("(.+) - (.+)","\\2",item_full_name)
  ) %>%
  dplyr::select(
    q_name, item_label, section, item_full_name
  )
names(ds0) <- ds_names %>% dplyr::pull(item_name)


# ---- tweak-data ---------------------

#
# recode_guide <-
#   "
# 'Strongly agree'          = '2'
# ;'Somewhat agree'         = '1'
# ;'Neutral'                = '0'
# ;'Somewhat disagree'      = '1'
# ;'Strongly disagree'      = '2'
# ;'Unsure'                 = '99'
# ;'I choose not to answer' = '98'
# "
# recode_levels <- function(v){car::recode(v,recode_guide)}
# ds2 <- ds2 %>%
#   dplyr::mutate_all(recode_levels)
#



# ---- save-to-disk ----------------------------
ds_names %>% readr::write_csv("./data-public/metadata/survey_items.csv")
ds0 %>% readr::write_csv("./data-unshared/derived/oud_survey.csv")

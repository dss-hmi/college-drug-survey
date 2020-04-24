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
    "item_name" = "V1"
    ,"item_label" = "V2"
  ) %>%
  dplyr::mutate(
    item_label = gsub("\\n"," ",item_label)
    ,section = gsub("(.+) - (.+)","\\1",item_label)
    ,item = gsub("(.+) - (.+)","\\2",item_label)
  ) %>%
  dplyr::select(
    item_name, item, section, item_label
  ) %>%
  readr::write_csv("./data-unshared/derived/survey_items.csv")


names(ds0) <- ds_names %>% dplyr::pull(item_name)


# ---- save-to-disk ----------------------------

ds <- read.csv(path_file)

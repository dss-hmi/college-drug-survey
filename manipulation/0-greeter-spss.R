rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
cat("\f") # clear console when working in RStudio
# ---- load-sources ------------------------------------------------------------
source("./scripts/common-functions.R")
# ---- load-packages -----------------------------------------------------------
library("magrittr") #Pipes
library("ggplot2")  # graphs
library("dplyr")

# ---- declare-globals ---------------------------------------------------------
path_input <- "data-unshared/raw/College Student Survey regarding Opioid Use Disorder Treatment_May 15, 2020_13.09.sav"

#set default ggplot theme
ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey90", color = NA)
    )
)

# ---- load-data ---------------------------------------------------------------
ds0 <- haven::read_sav(path_input)
meta_items <- ds0 %>% names_labels()
# meta_items %>% readr::write_csv("./data-public/metadata/meta_items.csv")
meta_values <- labelled::val_labels(ds0)

# ---- inspect-data -------------------------------------------------------------
# ds0 %>% glimpse(50)
# ds0 %>% distinct(ResponseId) %>% count()
# ---- derive-meta --------------------------

# -----tweak-data ---------------------------------------------------------------

# ---- survey-response -------------------------
cat("Initial responses, N = ", ds0 %>% n_distinct("ResponseId"))
ds0 %>% group_by(Status) %>% count() #%>% neat()
ds1 <- ds0 %>% filter(Status == 0)
cat("After keeping only valid IP Address\n",
    "Remaining responses, N =", ds1 %>% n_distinct("ResponseId"))


ds1 %>% group_by(Finished) %>% count() #
cat("\n Among the ", ds1 %>% filter(Finished==0) %>% count() %>% pull(n), " respondents who did not complete the survey", ds1 %>% filter(Finished==0, !Q2 %in% c(NA, -99)) %>% count() %>% pull(n), " have indicated the corporation, so we keep these records to salvage the partial information they have provided" )
ds1 %>% filter(Finished==0) %>% group_by(Q2) %>% count()
ds1 %>% filter(Finished==0,!Q2 %in% c(NA, -99)) %>% select(Q2, Progress)

ds1 <- ds1 %>% filter(Finished == 1 |!Q2 %in% c(NA, -99) )
cat("After keeping only those who indicated the corporation, N =", ds1 %>% n_distinct("ResponseId"))

ds1 %>% group_by(UserLanguage) %>% count() #%>% neat()

ds1 %>%
  mutate(
    date = lubridate::date(RecordedDate)
  ) %>%
  ggplot(aes(x = date) )+
  geom_bar()+
  labs(
    title = paste0("Date of response collection (N ="
                   ,ds1 %>% n_distinct("ResponseId")
                   , ")"
    )
    ,x = "2019"
    ,y = "Number of responses"
  )

cat("\n Duration of Survey")
d <- ds1 %>%
  # arrange(`Duration (in seconds)`) %>%
  mutate(
    minutes = Duration__in_seconds_/60
    ,hours = Duration__in_seconds_/60 / 60
    ,days = Duration__in_seconds_/60 / 60 / 24
  ) %>%
  arrange(minutes) %>%
  select(minutes, hours, days) %>%
  dplyr::mutate(
    id = row_number()
  )
d %>% head()
d %>% tail()


# ---- inspect-data-2 -------------------------------------------------------------
# d <- ds0 %>% select(ResponseId, Finished)
# d %>% glimpse()
# d <- d %>%
#   mutate(
#     finished2 = factor(Finished, levels = meta_values$Finished, labels = names(meta_values$Finished))
#   )
# d %>% TabularManifest::histogram_discrete("finished2")


# ---- experience-with-tx ---------------------

recode_experience <- function(x){
  car::recode(var = x, recodes =
  "
  ;'-99'            = NA
  "
  )
}

varname_scale <- ds1 %>% select(starts_with("Q5_")) %>% names() %>% setdiff("Q5_11")

ds_exp_tx <- ds1 %>%
  select(c("ResponseId", varname_scale) ) %>%
  mutate_at(varname_scale, as.integer) %>%
  mutate_at(varname_scale, recode_experience) %>%
  mutate(
    # the number of Tx the respondent  had experiences with)
    n_tx_experience = rowSums(.[varname_scale], na.rm =T)
   , ResponseId = as.character(ResponseId)
  )

ds_exp_tx %>% group_by(n_exp_tx) %>% count()

ds_exp_tx %>% saveRDS("./data-unshared/derived/ds_exp_tx.rds")



# ---- save-to-disk -------------------------------------------------------------
dto <- list(
  "raw0"     = ds0
  ,"microData" = ds2
  # ,"metaData" = ds_meta
)
dto %>% saveRDS("./data-unshared/derived/dto.rds")
names(ds2)

# ----- publisher --------------------
path <- "./analysis/0-greeter/0-greeter.Rmd"
rmarkdown::render(
  input = path ,
  output_format=c(
    "html_document"
    # ,"word_document"
  ),
  clean=TRUE
)



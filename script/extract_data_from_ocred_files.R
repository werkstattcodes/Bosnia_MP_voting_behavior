# setup -------------------------------------------------------------------
library(rvest)
library(tidyverse)
library(paletteer)
library(glue)
library(furrr)
plan(multiprocess)
library(pdftools)


wdr <- getwd() 



# >> file list ------------------------------------------------------------

file_list  <- list.files(path=paste0(wdr,"/data/voting_records/"), 
                         pattern=".pdf$",
                         all.files=T,
                         full.names = T) 




# > *function identifiying house from scrapped pdf -----------------------

fn_identify_house <- function(files){
  
  df_which_house <- files %>% 
    map(., pdftools::pdf_ocr_text) %>% 
    map(., glue_collapse, sep="\n") %>% 
    enframe(name=NULL, value="raw_text") 
}

df_houses <- file_list %>%  
  set_names() %>% 
  future_map_dfr(., fn_identify_house, .id="link") %>% 
  mutate(raw_text=map_chr(raw_text, ~unlist(.))) %>% 
  mutate(raw_text=iconv(raw_text, from="UTF-8", to="windows-1253")) %>% 
  mutate(house=case_when(str_detect(raw_text, regex("Predstav*", ignore_case = T)) ~ "HoR",
                         str_detect(raw_text, "naroda") ~ "HoP",
                         TRUE ~ as.character("missing")))

table(df_houses$house, useNA=c("always")) 

#285 files could not be scrapped; #hence ocr needed;



# conitinue with ocr results  ----------------------------------------------

df_ocr_results <- readr::read_csv2(file = paste0(wdr, "/data/df_ocr_results.csv")) # from script ocr_files.R

df_ocr_results <- df_ocr_results %>%
  mutate(house_indicator=str_extract(raw_text, "Predstav*|naroda")) %>% 
  mutate(house = case_when(
    str_detect(house_indicator, regex("Predstav*", ignore_case = T)) & !str_detect(house_indicator, regex("naroda", ignore_case = T)) ~ "HoR",
    str_detect(house_indicator, "naroda") ~ "HoP",
    TRUE ~ as.character("missing")
  ))

table(df_ocr_results$house, useNA = c("always")) # hop 308, hor485, no missing results; data good to go

df_ocr_results <- df_ocr_results %>%
  mutate(raw_text2 = stringr::str_split(raw_text, "\n"))
# 
# df_raw_text2<- df_ocr_results %>% 
#   filter(str_detect(link, "108778|123161")) %>% 
#   mutate(raw_text3=map(raw_text2, enframe, name="row_number", value="text")) %>% 
#   mutate(raw_text3=map(raw_text3, ~filter(., row_number<4))) %>% #take first 3 rows from pdf; check for house
#   mutate(raw_text4=paste0(raw_text3$text, collapse=";"))
#   mutate(house = case_when(map(raw_text3, ~str_detect(., "Predstav")) ~ "HoR",
#                            map(raw_text3, ~str_detect(., "naorda")) ~ "HoP",
#                            TRUE ~ as.character("missing")))
                           
                           
df_ocr_results <- df_ocr_results %>%
  mutate(raw_text = stringr::str_squish(raw_text)) %>%
  mutate(session_no = stringr::str_squish(raw_text) %>% stringr::str_extract(., "[0-9]+(?=\\.?\\s?sjednic)")) %>%
  mutate(vote_no = stringr::str_extract(raw_text, "(?<=(Glasanje br:\\s?|Redni broj.{0,20}))[0-9]+")) %>% 
  mutate(session_date = str_extract(raw_text, "[0-9]{1,2}/[0-9]{1,2}/20[0-9]{2}|[0-9]{1,2}.[0-9]{2}.20[0-9]{2}|[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}|[0-9]{1,2}.[:alpha:]+20[1-9]{2}."))

check_session_date  <- df_ocr_results %>%  filter(is.na(session_date)) #1 HoP file different than all others
check_vote_no  <- df_ocr_results %>%  filter(is.na(vote_no)) #1 HoP file different than all others


df_ocr_results <- df_ocr_results %>%
  mutate(delegate_votes = map(raw_text2, ~ str_subset(., regex(" Za| Nije prisutan| PROTIV| UKUPNO| SUZDRZAN| NIJE GLASAO",
    ignore_case = T
  )) %>%
    str_subset(., " FBiH | RS | Federacija | Republika Srpska ") %>%
    str_remove(., "^[^[:alpha:]]+") %>% # remove every non-alpha before the frist alpha.
    str_squish(.) %>%
    enframe(name = NULL, value = "delegate_raw"))) %>%
  mutate(delegate_votes = map(delegate_votes, ~ .x %>%
    mutate(
      delegate_name = stringr::word(delegate_raw, 1, 2),
      entity = case_when(
        str_detect(delegate_raw, "FBiH|Federacija") ~ "FBiH",
        str_detect(delegate_raw, "RS|Republika Srpska") ~ "RS",
        TRUE ~ as.character("missing")
      ),
      vote = case_when(str_detect(delegate_raw, regex(" za", ignore_case = T)) ~ "yes",  #space before is important; attention suzdrZAn;
                           str_detect(delegate_raw, regex(" protiv", ignore_case = T)) ~ "no",
                           str_detect(delegate_raw, regex(" suzdrzan", ignore_case = T)) ~ "abstained", #reserved
                           str_detect(delegate_raw, regex(" nije glasao", ignore_case = T)) ~ "no vote", 
                           str_detect(delegate_raw, regex(" nije prisutan", ignore_case = T)) ~ "not present",
                       TRUE ~ as.character("missing")
                     ) %>% stringr::str_to_lower()
      ))) %>%
  mutate(delegate_num = map_int(delegate_votes, nrow)) %>% 
  mutate(law_id=str_extract(link, "(?<=law_id_)[0-9]+"),
         record_id=str_extract(link, "(?<=record_id_)[0-9]+")) #pos. look-behind
         

wrong_HoR<- df_ocr_results %>% 
  filter(house=="HoR") %>% 
  filter(delegate_num!=42) 
nrow(wrong_HoR)

wrong_HoP<- df_ocr_results %>% 
  filter(house=="HoP") %>% 
  filter(delegate_num!=15) 
nrow(wrong_HoP)

#unnest delegates
df_ocr_results_unnested <- df_ocr_results %>% 
  unnest(delegate_votes)

#correct/standardize some names; almost all errors relate to one document ("07.10.2015"))
df_ocr_results_unnested<- df_ocr_results_unnested %>% 
  mutate(delegate_name = case_when(delegate_name=="Zaim Backovib" ~ "Zaim Backovic",
                                   str_detect(delegate_name, "Aleksandra Pandu") ~ "Aleksandra Pandurevic",
                                   str_detect(delegate_name, "Amir Fazl") ~ "Amir Fazlic",
                                   str_detect(delegate_name, "Asim Sarajlib") ~ "Asim Sarajlic",
                                   str_detect(delegate_name, "Borislav Boji") ~ "Borislav Bojic",
                                   str_detect(delegate_name, "Ljiulja Zovko") ~ "Ljilja Zovko",
                                   str_detect(delegate_name, "D. Nermina") ~ "Nermina Kapetanovic",
                                   str_detect(delegate_name, "D. Safer") ~ "Safer Demirovic",
                                   str_detect(delegate_name, "Magazinovic") ~ "Sasa Magazinovic",
                                   TRUE ~ as.character(delegate_name))) %>% 
  mutate(delegate_name_2=paste(str_remove(delegate_name, "^[:alpha:]+\\s"),
                               word(delegate_name, 1), sep=", ")) %>% 
  mutate(delegate_name_family=stringr::word(delegate_name, -1))

table(df_ocr_results_unnested$vote, useNA = c("always"))

df_ocr_results_unnested %>% 
  group_by(house, vote) %>% 
  summarise(n_vote=n()) %>% 
  group_by(house) %>% 
  mutate(n_rel=n_vote/sum(n_vote)*100) %>% 
  arrange(desc(n_rel), .by_group=T) %>% 
  pivot_wider(id_cols=house, names_from=vote, values_from=c(n_vote, n_rel))
            

#number of MPs per house
df_ocr_results_unnested %>% 
  group_by(house) %>% 
  summarise(n_unique=length(unique(delegate_name)))
#rather remarkable; high fluctuation among MPs; 136 HoR, 28 HoP



# import party affiliation ------------------------------------------------

df_members_parliament <- read_csv2(paste0(wdr,"/data/df_members_parliament.csv")) %>% 
  select(-X1) 

df_members_parliament <- df_members_parliament %>% 
  filter(period %in% c("2010-2014", "2006-2010", "2002-2006", "2014-2018")) %>% 
  group_by(name) %>% 
  arrange(desc(period), .by_group=T) %>% 
  slice(1) #takes MPs


df_members_parliament_distinct <- df_members_parliament %>% 
  distinct(name, party)

df_members_parliament_distinct <- df_members_parliament %>% 
 # distinct(name, party) %>% 
  group_by(name) %>% 
  summarise(n=n())
  
df_ocr_results_unnested<- df_ocr_results_unnested %>% 
  #filter(house=="HoR") %>% 
  filter(session_date!="07.10.2015") %>%  #most inconsistencies with names are related to one document form 7.10.2015
  left_join(., df_members_parliament %>% select(name, party),
            by=c("delegate_name"="name"))

#check number of delegates per vote and house
df_ocr_results_unnested %>% 
  group_by(law_id) %>% 
  summarise(n_votes_per_law=length(unique(record_id)))

votes_check <- df_ocr_results_unnested %>% 
  group_by(house, record_id, entity) %>% 
  summarise(n.delegates=n()) %>% 
  group_by(house, entity, n.delegates) %>% 
  summarise(n_obs=n(),
            record_ids=list(unique(record_id))) %>% 
  ungroup()
votes_check


#assign ethnicity to party
df_ocr_results_unnested <- df_ocr_results_unnested %>% 
  mutate(ethnicity=case_when(str_detect(party, "A-SDA|BPS|SBB|SDA|SBiH") ~ "Bosniak",
                             str_detect(party, "SDP|DF") ~ "civic",
                             party %in% c("DNS", "PDP-NDP", "SDS", "SNSD") ~ "Serb",
                             party %in% c("independent") ~ "independent",
                             str_detect(party, "HDZ|HSS|HSP") ~ "Croat",
                             TRUE ~ NA_character_))

table(df_ocr_results_unnested$ethnicity, useNA = c("always")) #few parties not yet assigned

df_ocr_results_unnested %>% 
  filter(is.na(ethnicity)) %>% 
  distinct(party, ethnicity)

# export file -------------------------------------------------------------
write_csv2(df_ocr_results_unnested %>% 
             select(-raw_text2), path=paste0(wdr, "/data/df_ocr_results_unnested.csv")) #raw_text2 class list

summary(df_ocr_results_unnested)

MPs_without_party <- df_ocr_results_unnested_m %>% 
  filter(is.na(party)) %>% 
  group_by(delegate_name) %>% 
  #summarise(session_dates=paste0(session_date, collapse=", "))
  distinct(delegate_name)
nrow(MPs_without_party)


#checks
delegate_party <- df_ocr_results_unnested %>% 
  distinct(delegate_name, party)

table(delegate_party$party, useNA = c("always"))

length(unique(df_ocr_results_unnested$law_id))
#151 law ids but 162 laws?! pending?




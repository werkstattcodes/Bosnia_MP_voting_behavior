# setup -------------------------------------------------------------------
library(rvest)
library(tidyverse)
library(paletteer)
library(glue)
library(furrr)
plan(multiprocess)
library(pdftools)


wdr <- getwd() 

# scrap links for all laws ------------------------------------------------

#link where you get list on all laws with all statuses
#http://parlament.ba/oLaw/GetOLawsByStatus?SearchTerm=&MandateId=4&Status=-1

seq_page<- seq(1:17)
seq_page_links <- glue("http://parlament.ba/oLaw/GetOLawsByStatus?page={seq_page}&MandateId=4&Status=-1") %>% 
  enframe(name=NULL)

pb <- progress_estimated(nrow(seq_page_links))

fn_scrap <- function(x){
  
  pb$tick()$print()
  
  x %>% 
    read_html() %>% 
    html_nodes("a") %>% 
    html_attr(.,"href") %>% 
    enframe(name=NULL) %>% 
    filter(str_detect(value, "OLawDetails")) %>% 
    mutate(links_to_law_details=paste0("http://parlament.ba", value)) 
  
}

df_links_to_laws <- seq_page_links$value %>% 
  set_names() %>% 
  map_dfr(.,  possibly(fn_scrap, otherwise=NULL), .id="seq_page_links")

write_csv2(df_links_to_laws, paste0(wdr, "/data/df_links_to_laws.csv"))

df_links_to_laws <- read_csv2(file=paste0(wdr, "/data/df_links_to_laws.csv"))



# scrap law details based on link list ------------------------------------

pb <- dplyr::progress_estimated(nrow(df_links_to_laws)) #define length for progress bar

#scrap law details
fn_scrap_law_details <- function(x) { 
  
  pb$tick()$print()
  
  df.contents <- x %>% 
    read_html() %>% 
    html_nodes("td") %>% 
    html_text(.,trim=T) %>% 
    enframe(name=NULL, value="content") %>% 
    slice(c(1:13))
  
  df.names<- x %>% 
    read_html() %>% 
    html_nodes("th") %>% 
    html_text(.,trim=T) %>% 
    enframe(name=NULL, value="heading") %>% 
    slice(c(1:13))
  
  bind_cols(df.names, df.contents) %>% 
    mutate(house=case_when(row_number()>7 ~ "DP",    #House of Peoples
                             TRUE ~ "PS"))           #House of Representatives
  
}

#Scrap overview of law status 
fn_get_law_status <- function(x) {
  x %>%
    read_html() %>%
    html_nodes("body > section > div.container > div > div.col-md-8 > article > div > div:nth-child(2) > table") %>%
    html_table() %>%
    enframe(name = NULL) %>%
    map_df(., bind_rows) %>%
    rename(
      heading = X1,
      content = X2
    ) %>%
    mutate(heading = case_when(
      lag(heading) %>% str_detect(., "Status u DNPSBiH") ~
        paste("DNPSBiH -", heading),
      lag(heading) %>% str_detect(., "Status u PDPSBiH") ~
        paste("PDPSBiH -", heading),
      TRUE ~ as.character(heading)
    ))
}


df_details_all_status<- df_links_to_laws$links_to_law_details %>% 
  set_names() %>% 
  future_map_dfr(.,  fn_get_law_status, 
                 .progress=T,
                 .id="seq_page_links")

stringi::stri_enc_list()

#*****
#encoding checks
encoding_tests <- df_details_all_status %>% 
  mutate(content=str_trim(content, side=c("both"))) %>% 
  filter(str_detect(heading, coll("status", ignore_case = T))) %>% 
  mutate(content_encoding=Encoding(content)) %>% 
  mutate(content2=stringr::str_conv(content, encoding = "UTF-8")) %>% 
  mutate(content2_encoding=Encoding(content2)) %>% 
  mutate(content3=iconv(content, from="UTF-8", to="windows-1253")) %>%
  mutate(content3_encoding=Encoding(content3)) %>% 
  #filter_at(vars(contains("content2")), any_vars(.=="Ceka"))
  filter_at(.vars = vars(contains("content")),
           any_vars(str_detect(. , "Ceka")))
encoding_tests
#*****

df_details_all_status_wide <- df_details_all_status %>% 
  mutate(content=str_trim(content, side=c("both"))) %>% 
  mutate(content=iconv(content, from="UTF-8", to="windows-1253")) %>% #removes diacrits; makes it easier to work with
  pivot_wider(id_cols = "seq_page_links",
              names_from = "heading",
              values_from = "content") %>% 
  mutate(law_id=str_extract(seq_page_links, "[0-9]+$")) %>% 
  rename(doc_name ="Naziv dokumenta",
         period = Saziv,
         HoR_draft_law_number_date  = "Broj i datum Prijedloga zakona u PDPSBiH",
         HoR_tabler = `Predlagač u PDPSBiH`,
         HoR_status = "Status u PDPSBiH",
         HoR_order_number_session_date_agenda_item = `PDPSBiH - Red. br. i datum sjednice - tačka dnevnog reda`,
         HoP_draft_law_number_date= `Broj i datum Prijedloga zakona u DNPSBiH`,
         HoP_tabler =`Predlagač u DNPSBiH`,
         HoP_status =`Status u DNPSBiH`,
         HoP_order_number_session_date_agenda_item =`DNPSBiH - Red. br. i datum sjednice - tačka dnevnog reda`,
         BiH_Parl_final_status=`Konačni status u PSBiH`,
         ended_tabler_session =`Utvrđen na sjednici predlagača`) %>% 
  separate(col=HoR_draft_law_number_date, 
           into=c("HoR_draft_law_number", "HoR_draft_law_date"),
           sep="od",
           remove=F) %>% 
  separate(col=HoP_draft_law_number_date, 
           into=c("HoP_draft_law_number", "HoP_draft_law_date"),
           sep="od",
           remove=F) %>% 
  mutate_at(vars(contains("draft_law_date")), ~str_remove(., ".$") %>% 
              lubridate::dmy(.)) %>% 
  mutate_at(vars(ends_with("draft_law_number")), ~str_trim(., side=c("both")) %>% 
            str_remove(., ",$") %>% str_replace(., ",","-")) %>% 
  mutate(HoP_order_number_session_date_agenda_item=ifelse(HoP_order_number_session_date_agenda_item=="",
                                                          NA, HoP_order_number_session_date_agenda_item)) %>% 
  mutate(HoP_order_number=stringr::str_extract_all(HoP_order_number_session_date_agenda_item,
                                                   "^[0-9]+") %>% as.numeric) %>% 
  mutate(HoP_session_date=stringr::str_extract_all(HoP_order_number_session_date_agenda_item,
                                                   "[:digit:]+\\.[:digit:]+\\.[:digit:]{4}") %>% 
           map_chr(., paste, collapse="; ")) %>% 
  mutate(HoP_agenda_item=stringr::str_extract(HoP_order_number_session_date_agenda_item,
                                              "Ad\\. [:digit:]+")) %>% 
#  select(-HoP_order_number_session_date_agenda_item) %>% 
  mutate(HoR_order_number_session_date_agenda_item=ifelse(HoR_order_number_session_date_agenda_item=="",
                                                          NA, HoR_order_number_session_date_agenda_item)) %>% 
  mutate(HoR_order_number=stringr::str_extract_all(HoR_order_number_session_date_agenda_item,
                                                   "^[0-9]+") %>% as.numeric) %>% 
  mutate(HoR_session_date=stringr::str_extract_all(HoR_order_number_session_date_agenda_item,
                                                   "[:digit:]+\\.[:digit:]+\\.[:digit:]{4}") %>% 
           map_chr(., paste, collapse="; ")) %>% 
  mutate(HoR_agenda_item=stringr::str_extract(HoR_order_number_session_date_agenda_item,
                                              "Ad\\. [:digit:]+")) %>% 
#  select(-HoR_order_number_session_date_agenda_item) %>% 
  mutate_at(vars(contains("status")),
            .funs=list(~case_when(
              .=="Usvojen"~"adopted",
              .=="Procedura - nije preuzet" ~ "procedure - not started",  #?
              .=="Procedura" ~ "procedure",
              str_detect(., regex("Povucen")) ~ "withdrawn",  #Povucen encoding issue
              .=="Donesen i objavljen" ~ "submitted and published",
              .=="Odbijen" ~ "rejected",
              .=="Obustavljen zakonodavni postupak" ~ "legislative process suspended",
              str_detect(., "Ceka na pokretanje procedure") ~ "waiting for the procedure to start", #encoding issue
              .=="Nije razmatran" ~ "not considered",
              is.na(.) ~ "missing",
              TRUE ~ .)))

df_details_all_status_wide %>% 
  select(contains("status")) %>% 
  mutate(index=1) %>% 
  pivot_longer(cols=-index, names_to="names", values_to="status") %>% 
  distinct(status)  

library(googleLanguageR)
gl_auth("C:/Users/Roland/Google Drive/Events - Projects/R-Projects/GoogleAPI/BiH-Laws-49dca6b0c199.json")

df_details_all_status_wide<- df_details_all_status_wide %>% 
  mutate(doc_name_eng=googleLanguageR::gl_translate(doc_name, target="en", format=c("text"),
                                                    source="bs")$translatedText) %>%
  select(doc_name, doc_name_eng, everything())

write_csv2(df_details_all_status_wide, path=paste0(wdr, "/data/df_details_all_status_wide.csv"))




#run with only 1 law to test
fn_scrap_law_details(df_links_to_laws$links_to_law_details[1])

#run fn with all laws/gets details of all laws (function without furrr)
# tic()
#   df_details_all_laws<- df_links_to_laws$links_to_law_details %>%
#     set_names() %>%
#     map_dfr(.,  possibly(fn_scrap_law_details, otherwise=NULL), .id="seq_page_links")
# toc()

library(tictoc)
library(furrr)
plan(multiprocess)
tic()
df_details_all_laws<- df_links_to_laws$links_to_law_details %>% 
  set_names() %>% 
  future_map_dfr(.,  fn_scrap_law_details, 
                 .progress=T,
                 .id="seq_page_links")
toc()

# df_details_all_laws <- df_details_all_laws %>% 
#   group_by(seq_page_links) %>% 
#   mutate(house=case_when(row_number()>7 ~ "DP",
#                            TRUE ~ "PS"))
  
write.csv2(df_details_all_laws, paste0(wdr,"/data/df_details_all_laws.csv"),
           row.names = F,
           fileEncoding = "latin1")

df_details_all_laws <- read.csv2(file=paste0(wdr,"/data/df_details_all_laws.csv"),
           fileEncoding = "latin1",
           stringsAsFactors = F)


df_details_all_laws <- df_details_all_laws %>% 
  mutate(heading=case_when(str_detect(content, "pdf$") ~ "filename",
                         TRUE ~ as.character(heading))) %>% 
  mutate_if(is.character, stringr::str_trim, side=c("both") %>% str_squish)


df_details_all_laws_wide <- df_details_all_laws %>% 
 # filter(seq_page_links=="http://parlament.ba/olaw/OLawDetails?lawId=61794") %>% 
  pivot_wider(id_cols = c(seq_page_links, house),
              names_from = heading, 
              values_from = content,
              names_repair = c("unique"),
              values_fn = list(content = length))


larger_1<- df_details_all_laws_wide %>% 
  filter_at(vars(3:12), any_vars(.>1))


df_details_all_laws_wide$`Naziv dokumenta`[1]

df_details_all_laws <- readr::read_csv2(paste0(wdr, "/data/df_details_all_laws.csv"))

# (pending) law timeline ------------------------------------------------------------

df_links_to_laws <-
  readr::read_csv2(paste0(wdr, "/data/2014_2018_law_links.csv"))

pb <- dplyr::progress_estimated(nrow(df_links_to_laws))


fn_case.details <- function(x)  {
  pb$tick()$print()
  
  x %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table(., trim = T, fill = T) %>%
    
    map(., ~ map(., as_tibble)) %>%
    map(., bind_cols) %>%
    bind_rows()
  
}

df_details_all_laws <- df_links_to_laws$links %>%
  set_names() %>%
  map_dfr(.,  possibly(fn_case.details, otherwise = NULL), .id = "seq_page_links")

#write_csv2(df_details_all_laws, paste0(wdr,"/data/df_details_all_laws.csv"))




# get links to pdfs -------------------------------------------------------

# > single case testing ---------------------------------------------------

#df_details_all_laws <- readr::read_csv2(paste0(wdr,"/data/2014_2018_law_details.csv"))

df_links_to_laws <-
  readr::read_csv2(paste0(wdr, "/data/2014_2018_law_links.csv"))

# > functions -------------------------------------------------------------

fn_scrap_links_to_voting_records <- function(x) {

    x %>% 
    read_html() %>% 
    html_nodes(xpath="//a[contains(text(), 'Listing')]") %>%  #filters links based on text/name of links
    html_attr('href') %>%  #extracts links
    enframe(name=NULL) %>% 
    mutate(link_to_voting_record=paste0("http://parlament.ba", value))
  
}

df_voting_results_links <- df_links_to_laws$links_to_law_details %>% 
  set_names() %>% 
  future_map_dfr(.,  possibly(fn_scrap_links_to_voting_records, otherwise=NULL), 
                 .progress = T, .id="seq_page_links") %>% 
  select(-value) %>% 
  mutate(law_id=str_extract(seq_page_links, "[0-9]+$"),
         record_id=str_extract(link_to_voting_record, "(?<=documentId=)[0-9]+")) #pos. look-behind


votes_per_bill <- df_voting_results_links %>% 
  group_by(seq_page_links) %>% 
  summarise(n_votes_per_bill=n()) %>% 
  arrange(desc(n_votes_per_bill))
votes_per_bill


# >> ex/import ------------------------------------------------------------------
write_csv2(df_voting_results_links, path=paste0(wdr, "/data/df_voting_results_links.csv"))
df_voting_results_links <- readr::read_csv2(paste0(wdr, "/data/df_voting_results_links.csv"))


# download voting records (pdfs) ------------------------------------------
pdf_destination <- glue("{wdr}/data/voting_records/law_id_{df_voting_results_links$law_id}-record_id_{df_voting_results_links$record_id}.pdf")

furrr::future_walk2(df_voting_results_links$link_to_voting_record, pdf_destination, download.file, mode = "wb") #downloads files


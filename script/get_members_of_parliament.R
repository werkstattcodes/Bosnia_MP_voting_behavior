library(tidyverse)
library(rvest)
library(glue)

wdr <- getwd()


# Members of House of REPRESENTATIVES ---------------------------------------------

page_index <- seq(1,5,1)
mandate_no <- c(6, 7,8, 9)
df_ids<- crossing(page_index, mandate_no)

MP_links <- glue("https://www.parlament.ba/Representative/List?page={df_ids$page_index}&mandateId={df_ids$mandate_no}")

pb <- dplyr::progress_estimated(length(MP_links))

fn_scrap_MPs <- function(x) {
  
  pb$tick()$print()
  
  x  %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(.,trim=T, fill=T) %>% 
    map_dfr(., as_tibble)
}


df_members_HoRepresentatives <- MP_links %>% 
  set_names() %>% 
  map_dfr(.,  possibly(fn_scrap_MPs, otherwise = NULL), .id = "MP_links")

df_members_HoRepresentatives<- df_members_HoRepresentatives %>% 
  rename(name=X2,
         party2=X3,
         party=X4,
         constituency=X5) %>% 
  #  mutate(name=map(., iconv, from="UTF-8", to="window-1253")) %>% 
  tidyr::separate(name, c("family_name", "first_name"), sep=", ") %>% 
  mutate(party2=stringr::str_remove(party2, "Klup poslanika ")) %>% 
  mutate_at(vars(contains("name"), party), iconv, from="UTF-8", to="windows-1253")


df_members_HoRepresentatives<- df_members_HoRepresentatives %>% 
  select(-X1) %>% 
  mutate(first_name=stringr::str_squish(first_name)) %>% #removes space between double names
  unite(name, c("first_name","family_name"), sep=" " , remove=FALSE) %>% 
  mutate(house="House of Representatives") %>% 
  mutate(entity=case_when(str_detect(constituency, "RS") ~ "RS",
                          str_detect(constituency, "FBiH") ~ "FBiH",
                          TRUE ~ as.character(constituency))) %>% 
  mutate(party.original=party) %>% 
  mutate(party=case_when(party.original=="Samostalni poslanik" ~ "independent",
                         str_detect(party.original, "SBB") ~ "SBB",
                         str_detect(party.original, "BPS") ~ "BPS",
                         str_detect(party.original, "fronta") ~ "DF",
                         str_detect(party.original, "Za evropsku") ~ "A-SDA",
                         str_detect(party.original, "Koalicija HDZ") ~ "HDZ Coalition",
                         TRUE ~ as.character(party.original))) %>% 
  mutate(period=str_extract(MP_links, "(?<=mandateId=)[0-9]+")) %>% 
  mutate(period=case_when(period=="6" ~ "2006-2010",
                          period=="7" ~ "2010-2014",
                          period=="8" ~ "2014-2018",
                          period=="9" ~ "2018-2022",
                     TRUE ~ NA_character_)) %>%
  mutate(house="HoR") %>% 
  mutate_at(vars(contains("name"), party), iconv, from="UTF-8", to="windows-1253") %>% 
  mutate(name=case_when(name=="Nermina Zaimovic - Uzunovic" ~ "Nermina Zaimovic-Uzunovic",
                        name=="Mladen Ivankovic Lijanovic" ~ "Mladen Ivankovic-Lijanovic",
                        name=="Vesna Krstovic – Spremo" ~ "Vesna Krstovic-Spremo",
                        TRUE ~ as.character(name)))


write.csv2(df_members_HoRepresentatives, paste0(wdr,"/data/df_members_HoRepresentatives.csv" ),
           fileEncoding = "windows-1253")




# Members of House of PEOPLES ---------------------------------------------

page_index <- seq(1,5,1)
mandate_no <- c(6, 7, 8, 9)
df_ids<- crossing(page_index, mandate_no)

MP_links <- glue("https://www.parlament.ba/Delegate/List?page={df_ids$page_index}&mandateId={df_ids$mandate_no}")



pb <- dplyr::progress_estimated(length(MP_links))

fn_scrap_MPs <- function(x) {
  
  pb$tick()$print()
  
  y <- x  %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table(.,trim=T, fill=T) %>% 
    map_dfr(., as_tibble)
}


df_members_HoPeoples <- MP_links %>% 
  set_names() %>% 
  map_dfr(.,  possibly(fn_scrap_MPs, otherwise = NULL), .id = "MP_links")

df_members_HoPeoples<- df_members_HoPeoples %>% 
  rename(name=X2,
         party2=X3,
         party=X4,
         constituency=X5) %>% 
  tidyr::separate(name, c("family_name", "first_name"), sep=", ") %>% 
  mutate(party2=stringr::str_remove(party2, "Klup poslanika ")) %>% 
  mutate_at(vars(contains("name"), party), iconv, from="UTF-8", to="windows-1253") 

df_members_HoPeoples<- df_members_HoPeoples %>%
  select(-X1) %>% 
  mutate(first_name=stringr::str_squish(first_name)) %>% #removes space between double names
  unite(name, c("first_name","family_name"), sep=" " , remove=FALSE) %>% 
  mutate(house="House of Peoples") %>% 
  rename(entity=constituency) %>% 
  mutate(party.original=party) %>% 
  mutate(party=stringr::word(party, 1)) %>% 
  mutate(period=str_extract(MP_links, "(?<=mandateId=)[0-9]+")) %>% 
  mutate(period=case_when(period=="6" ~ "2006-2010",
                          period=="7" ~ "2010-2014",
                          period=="8" ~ "2014-2018",
                          period=="9" ~ "2018-2022",
                          TRUE ~ NA_character_)) %>% 
  mutate(house="HoP")



write.csv2(df_members_HoPeoples, paste0(wdr,"/data/df_members_HoPeoples.csv"),
           fileEncoding = "windows-1253")

df_members_parliament <- bind_rows(df_members_HoPeoples, df_members_HoRepresentatives)  
write.csv2(df_members_parliament, paste0(wdr,"/data/df_members_parliament.csv"),
           fileEncoding = "windows-1253")




# Did some MPs change their party affiliation over time -------------------

x <- df_members_parliament %>% 
  group_by(house, name) %>% 
  summarise(n=n(),
            periods=paste(period, collapse=","),
            party=paste(party, collapse=",")) %>% 
  arrange(desc(n))






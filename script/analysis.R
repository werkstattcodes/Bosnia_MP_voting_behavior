# setup -------------------------------------------------------------------
library(tidyverse)
library(paletteer)
library(glue)
library(extrafont)
library(paletteer)
loadfonts(device = "win", quiet = T)
extrafont::fonts()
library(hrbrthemes)
library(paletteer)
library(ggtext)
library(scales)
library(patchwork)
library(padr)

wdr <- getwd() 

my_caption <- c("Data: parlament.ba", "Analysis: Roland Schmidt | @zoowalk | <span style='color:black'>**werk.statt.codes**</span>")


df_ocr_results_unnested <- readr::read_csv2(file=paste0(wdr, "/data/df_ocr_results_unnested.csv"))

df_ocr_results_unnested <- df_ocr_results_unnested %>% 
  mutate_if(is.character, as.factor) 
  
#parse_date_time order allows to specify different formats and their priority
#could be moved to other R script
df_ocr_results_unnested <- df_ocr_results_unnested %>% 
  mutate(session_date=as.character(session_date)) %>% 
  mutate(session_date=lubridate::parse_date_time(session_date,
                                                 orders=c("dmy","ymd", "mdy")) %>% 
           lubridate::as_date(.)) 



# _ ----------------------------------------------------------------------
# ANALYSIS ----------------------------------------------------------------

#Overview

df_details_all_status_wide <- read_csv2(paste0(wdr, "/data/df_details_all_status_wide.csv")) %>% 
  mutate_at(vars(contains("status")), as.factor)


#standardize colors across plots
colors_outcome <- paletteer_d("ggsci::default_jco", n = 10) %>% as.character() 
names(colors_outcome) <- unique(c(levels(df_details_all_status_wide$HoP_status), 
                                  levels(df_details_all_status_wide$HoR_status), 
                                  levels(df_details_all_status_wide$BiH_Parl_final_status)))

paletteer::palettes_d_names %>% 
  as.data.frame() %>% 
  filter(length>9)


df_laws_outcome <- df_details_all_status_wide %>% 
  group_by(BiH_Parl_final_status) %>% 
  summarise(n_obs=n()) %>% 
  ungroup() %>% 
  mutate(n_rel=n_obs/sum(n_obs)) 

df_laws_outcome %>% 
  ggplot()+
  labs(title="Outcome of bills",
       subtitle="Outcome of bills considered during legislative session 2014-2018; Total number: 163.",
       caption=my_caption)+
  geom_bar(aes(x=reorder(BiH_Parl_final_status, n_rel),
               y=n_obs,
               fill=BiH_Parl_final_status),
           stat="identity",
           position=position_dodge())+
  geom_text(aes(x=reorder(BiH_Parl_final_status, n_rel),
                y=80,
                label=paste0(n_obs, " (",
                  percent(n_rel, accuracy = 1),
                  ")")),
            family = "Roboto Condensed",
            color = "grey30",
            hjust = 1,
           # group = name,
            stat="identity")+
  scale_fill_manual(values=colors_outcome)+
  #scale_fill_paletteer_d("ggsci::default_jama")+
  scale_y_continuous(breaks=seq(0,60, 20))+
  hrbrthemes::theme_ipsum_rc() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title=element_blank(),
        plot.title = element_text(size = 12, face="bold.italic", margin=margin(b=0, unit="cm")),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 12, color = "grey30"),
        plot.caption.position = "plot",
        plot.caption = element_markdown(color = "grey30", hjust = c(0, 1)))+
  coord_flip()


#in which house are bills rejected

df_outcome_chambers <- df_details_all_status_wide %>% 
  select(contains("status"), -contains("final")) %>% 
  pivot_longer(cols=contains("status"), names_to="house", values_to="outcome") %>% 
  filter(outcome!="missing") %>%
  mutate(outcome=fct_drop(outcome, only="missing")) %>%
  mutate(outcome=fct_infreq(outcome) %>% fct_rev) %>% 
  group_by(house, outcome, .drop=F) %>% 
  summarise(n_obs=n()) %>% 
  group_by(house) %>% 
  mutate(n_rel=n_obs/sum(n_obs)) %>% 
  arrange(desc(n_obs), .by_group=T) %>% 
  mutate(cumsum_n_obs=cumsum(n_obs)) %>% 
  ungroup() %>% 
  mutate(outcome_label = case_when(house=="HoR_status" ~ paste0(outcome, "\n(", n_obs, ")"),
                                   house=="HoP_status" ~ paste0(outcome, "(", n_obs, ")")))

plot_df_outcome_chambers <- df_outcome_chambers %>% 
  # mutate(outcome=fct_drop(outcome, only="missing")) %>% 
  # filter(outcome!="missing") %>% 
  group_split(house) %>% 
  map(~ggplot(.)+
  geom_bar(aes(x=1,
               y=n_obs,
               fill=outcome,
               group=outcome),
           stat="identity",
           position = position_stack())+
  geom_text(aes(x=1,
                y = n_obs,
                label=ifelse(n_obs>0,
                             n_obs,
                             ""),
                group=outcome),
            family = "Roboto Condensed",
            color = "white",
            position = position_stack(vjust= 0.5))+
  geom_text(aes(x=1,
                y = n_obs,
                label=ifelse(n_obs>10,
                             paste0("\n\n", scales::percent(n_rel)),
                             ""),
                group=outcome),
            family = "Roboto Condensed",
            color = "white",
            position = position_stack(vjust= 0.5))+  
  labs(y="Number of bills",
       title=ifelse(unique(.$house)=="HoP_status", 
                    "House of Peoples",
                    "House of Representatives"))+
      #subtitle="Bills considered in 2014 - 2018 legislative period.",
      # caption=my_caption)+
  scale_fill_manual(values=colors_outcome)+
  # scale_y_continuous(expand=expansion(mult=c(0,0.1)),
  #                    breaks=df_outcome_chambers %>% 
  #                      filter(house==.$house) %>% 
  #                      pull(cumsum_n_obs))+
  scale_y_continuous(expand=expansion(mult=c(0,0.1)),
                     limits=c(0, max(df_outcome_chambers$cumsum_n_obs)),
                     breaks=.$cumsum_n_obs,
                     labels=ifelse(.$n_obs<10, 
                                   ifelse(.$cumsum_n_obs==max(.$cumsum_n_obs), 
                                          unique(max(.$cumsum_n_obs)), ""),
                                          .$cumsum_n_obs))+
  coord_flip()+
  # lemon::facet_rep_wrap(vars(house),
  #                       repeat.tick.labels = F,
  #                       nrow=2,
  #                       labeller = labeller(house=c("HoP_status"="House of Peoples",
  #                                                "HoR_status"="House of Representatives")))+
  theme_ipsum_rc()+
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")+
  guides(fill=guide_legend(reverse = T, 
                           nrow=1)))

(patchwork::wrap_plots(plot_df_outcome_chambers))+
  plot_layout(ncol=1, guides="collect") &
  theme(legend.position = "bottom",
        legend.justification = "left")

df_outcome_chambers_combined <- df_details_all_status_wide %>% 
  mutate_at(vars(contains("status")), as.factor) %>% 
  group_by_at(vars(contains("status"), -contains("final")),
              .drop=F) %>% 
  summarise(n_obs=n()) %>% 
  ungroup() %>% 
  mutate(n_rel=n_obs/sum(n_obs)) %>% 
  group_by(HoR_status) %>% 
  mutate(HoR_outcome_label=sum(n_obs) %>% paste0(HoR_status, " (", ., ")")) %>% 
  group_by(HoP_status) %>% 
  mutate(HoP_outcome_label=sum(n_obs) %>% paste0(HoP_status, " (", ., ")")) %>% 
  ungroup() %>%  #ungroup before converting to factor
  mutate_at(vars(contains("label")), as_factor)
  


#to duplicate axis labels scale has to be numeric; convert to numeric, but
#us character labels;

plot_df_outcome_chambers_combined <- df_outcome_chambers_combined  %>% 
  ggplot()+
  labs(title="Outcome of bills by house",
       x="final status in House of Representatives",
       y=str_wrap("final status in House of Peoples", 20),
       caption=my_caption)+
  geom_tile(aes(x=as.numeric(HoR_outcome_label),
                y=as.numeric(HoP_outcome_label),
                fill=n_obs))+
  geom_text(data=. %>% filter(n_obs!=0),
              aes(x=as.numeric(HoR_outcome_label),
                y=as.numeric(HoP_outcome_label),
                label=paste0(n_obs, " (",
                             percent(n_rel, accuracy = 0.1),
                             ")")),
            family = "Roboto Condensed",
            color = "grey30",
            hjust = .5,
            # group = name,
            stat="identity")+
  scale_fill_gradient(low="white", high="steelblue",
                      na.value="grey")+
  scale_x_continuous(labels=levels(df_outcome_chambers_combined$HoR_outcome_label) %>% map_chr(., label_wrap(20)),
                     breaks=seq(1, length(unique(df_outcome_chambers_combined$HoR_outcome_label))),
                     sec.axis = dup_axis())+
  scale_y_continuous(labels=levels(df_outcome_chambers_combined$HoP_outcome_label) %>% map_chr(., label_wrap(20)),
                     breaks=seq(1, length(unique(df_outcome_chambers_combined$HoP_outcome_label))),
                     sec.axis = dup_axis(),
                     trans="reverse")+
  theme_ipsum_rc()+
  theme(axis.text.x.top=element_blank(),
        axis.title.x.top = element_text(hjust=0, size=11),
        axis.title.x.bottom = element_blank(),
        axis.text.y.left=element_blank(),
        axis.title.y.left = element_text(hjust=0, size=11),
        axis.title.y.right = element_blank(),
        #axis.text.x.bottom=element_text(angle=0, hjust=.5),
        panel.grid = element_blank(),
        legend.position = "none",
        panel.border = element_rect(color="grey30", fill=NA),
        axis.title.y = element_text(angle=0),
        plot.title = element_text(size = 12, face="bold.italic", margin=margin(b=0, unit="cm")),
        plot.title.position = "panel",
        plot.subtitle = element_text(size = 12, color = "grey30"),
        plot.caption.position = "panel",
        plot.caption = element_markdown(color = "grey30", hjust = c(0, 1)))


plot_df_outcome_chambers_combined



# time line ---------------------------------------------------------------



#number of votes per week
#use floor_date to aggregate witin a week
#use pdr to thicken to weeks, then pad for missing weeks, then plot;

# padr::thicken(., 
#               interval="week",
#               colname="week",
#               by="session_date") %>% 
#   



df_ocr_results_unnested %>% 
  mutate(month=lubridate::floor_date(session_date, unit="month")) %>% 
  group_by(house, month) %>% 
  summarise(n_votes=n_distinct(record_id)) %>%
  pad(., interval="month",
      start_val=min(.$month),
      end_val=max(.$month)
      ) %>% 
  padr::fill_by_value() %>% 
  mutate(year=lubridate::year(month)) %>% 
  #filter(lubridate::year(month)==2014) %>% 
  ggplot()+
  geom_bar(aes(y=n_votes,
               x=month),
           stat="identity")+
  scale_x_date(date_breaks = "1 month",
               labels=scales::label_date(format="%b"))+  #prints year only when year changes
  facet_grid(house~year,
             scales="free_x",
             space="free_x",
             shrink=F)+
  theme_ipsum_rc()+
  theme(axis.text.x = element_text(angle=90))


#check session number and dates

df_sessions <- df_ocr_results_unnested %>% 
  select(house, session_date, session_no) %>% 
  distinct() %>% 
  arrange(house, session_date) %>% 
  mutate(new_parlament=case_when(session_no<lag(session_no) ~ "start",
                                 TRUE ~ NA_character_)) 


#identify start of new legislative session
date_new_parliament <- df_ocr_results_unnested %>% 
  filter(str_detect(delegate_name_2, "^Ahmetovic")) %>% 
  filter(session_date==min(session_date)) %>% 
  distinct(session_date) %>% 
  pull(session_date)

voting_colors <- c("no"="red",
                   "no vote" = "grey",
                   "not present" = "brown",
                   "yes" = "green",
                   "restrained"="orange")


# tile graph --------------------------------------------------------------
library(ggforce)
library(ggiraph)

plots_tile <- df_ocr_results_unnested %>% 
  filter(house=="HoP") %>% 
  filter(session_date >= as.Date(date_new_parliament)) %>% 
  mutate(tooltip=paste(session_date, law_id, record_id)) %>% 
  select(tooltip, record_id, session_date, delegate_name_2, vote) %>% 
  ggplot()+
  geom_tile(aes(x=reorder(record_id, session_date),
                                     y=fct_rev(delegate_name_2),
                                     fill=vote))+
  scale_fill_manual(values=voting_colors)+
  theme_ipsum_rc() +
  theme(axis.text.x=element_blank(),
        panel.grid = element_blank())

plots_tile

#harmonize names in HoR


  
df.sum <- df_ocr_results_unnested %>% 
  mutate_at(vars(law_id, entity, vote), as.factor) %>% 
  group_by(law_id, entity, vote, .drop = F) %>% 
  summarise(votes=n()) %>% 
  group_by(law_id, entity, .drop=F) %>% 
  mutate(votes.casted=sum(votes[vote=="against"|vote=="yes"])) %>% 
  mutate(votes.rel=votes/votes.casted) %>% 
  mutate(entity.voting=case_when(votes>9 & vote=="against" ~ "entity veto",
                                 votes>18 & vote=="against" ~ "entity veto",
                                 TRUE ~ as.character("no entity veto"))) %>% 
  mutate(entity.voting2=case_when(votes.rel>0.66 & vote=="against" ~ "entity veto",
                                  votes.rel>0.66 & vote=="against" ~ "entity veto",
                                  TRUE ~ as.character("no entity veto"))) %>% 
  group_by(law_id) %>% 
  mutate(votes.total=sum(votes)) %>% 
  ungroup() %>% 
  mutate(house=case_when(votes.total==42 ~ "HoR",
                         votes.total==15 ~ "HoP",
                         TRUE ~ NA_character_))


df.x <- df.sum %>% 
  filter(vote=="against") %>% 
  arrange(law_id, entity) %>% 
  select(law_id, entity, entity.voting2) %>% 
  spread(key=entity, value=entity.voting2) %>% 
  mutate(entity.voting=case_when(FBiH=="entity veto"  & RS=="entity veto" ~ "both veto",
                                 FBiH!="entity veto" & RS=="entity veto" ~ "RS veto",
                                 FBiH=="entity veto" & RS!="entity veto" ~ "FBiH veto",
                                 TRUE ~ as.character("no veto")))

df.sum %>% 
  filter(vote=="yes") %>% 
  select(law_id, entity, votes.rel) %>% 
  spread(key=entity, value=votes.rel) %>% 
  ggplot()+
  geom_jitter(aes(x=FBiH, y=RS))+
  hrbrthemes::theme_ipsum_rc(base_family="ArialNarrow")



df.x %>% 
  ggplot()+
  geom_bar(aes(x=entity.voting,
               fill=entity.voting),
           stat="count",
           position=position_dodge())+
  hrbrthemes::theme_ipsum_tw() +
  nord::scale_fill_nord(palette="algoma_forest")+
  scale_y_continuous(expand=expand_scale(add=c(0.4 ,0.02)))



# jitter graph ------------------------------------------------------------


delegate.voting <- df_ocr_results_unnested %>% 
  group_by(entity, party, delegate_name, vote, .drop=F) %>% 
  summarise(nobs=n()) %>%
  mutate(total.votes=sum(nobs)) %>% 
  mutate(perc=nobs/total.votes*100)

delegate.voting %>% 
  filter(entity=="FBiH" & vote=="yes") %>% 
  arrange(perc)



delegate.voting %>% 
  filter(vote=="yes") %>% 
  ggplot()+
  geom_boxplot(aes(x=entity, y=perc,
                   fill=entity))+
  # geom_beeswarm(aes(x=entity, y=perc),
  #               color="firebrick",
  #               size=3)+
  
  geom_jitter(aes(x=entity, y=perc),
              width=0.03)+
  labs(subtitle="Each dot represents a member of HoR")+
  hrbrthemes::theme_ipsum_tw()+
  theme(legend.position=c(0.95,1),
        legend.direction = "horizontal",
        legend.title = element_blank())+
  scale_fill_paletteer_d("ggsci::default_jama")+
  scale_color_manual(guide=FALSE)



skimr::skim(df_ocr_results_unnested$vote)
table(df_ocr_results_unnested$vote, df_ocr_results_unnested$entity, useNA = "always")

# density chart -----------------------------------------------------------
library(ggridges)


df_ocr_results_unnested %>% 
  group_by(entity, delegate_name, vote) %>% 
  summarise(nobs=n()) %>%
  mutate(total.votes=sum(nobs)) %>% 
  mutate(perc=nobs/total.votes*100) %>% 
  filter(vote=="yes") %>% 
  ggplot()+
  geom_density_ridges(aes(x=perc,
                          y=entity,
                          group=entity,
                          fill=entity))+
  scale_x_continuous(limits=c(0,100))+
  nord::scale_fill_nord(palette="silver_mine")



# bar chart voting behavior each representative ---------------------------

df_ocr_results_unnested %>% 
  select(delegate_name, entity) %>% 
  distinct() %>% 
  count(entity)
#15 RS; possibly change of delegate
#49 FBiH delegates!

delegaes <- df_ocr_results_unnested %>% 
  distinct(delegate_name, entity) %>% 
  arrange(entity, delegate_name)


df_ocr_results_unnested %>% 
  group_by(entity, party, delegate_name, vote) %>% 
  summarise(votes.cat=n()) %>% 
  ggplot()+
  geom_bar(aes(x=delegate_name, y=votes.cat,
               fill=vote),
           stat="identity",
           position="fill")+
  #facet_wrap(vars(entity), scales="free_x", shrink=T)+
  facet_grid(~entity, space="free", scales="free_x")+
  theme_minimal()+
  scale_fill_viridis_d()+
  theme(axis.text.x=element_text(angle=90, hjust=1))


df_ocr_results_unnested %>% 
  group_by(entity, party, delegate_name, vote) %>% 
  summarise(votes.cat=n()) %>% 
  ggplot()+
  geom_bar(aes(x=delegate_name, y=votes.cat,
               fill=vote),
           stat="identity",
           position="fill")+
  #facet_wrap(vars(entity), scales="free_x", shrink=T)+
  facet_grid(~party, space="free", scales="free_x")+
  theme_minimal()+
  scale_fill_viridis_d()+
  theme(axis.text.x=element_text(angle=90, hjust=1))





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

#order factor of vote
df_ocr_results_unnested <- df_ocr_results_unnested %>% 
  mutate(vote=forcats::fct_infreq(vote))


voting_colors <- c("no"="red",
                   "no vote" = "orange",
                   "not present" = "grey",
                   "yes" = "green",
                   "abstained"="orange")


# _ ----------------------------------------------------------------------

# FILTER ------------------------------------------------------------------

#identify start of new legislative session
# date_new_parliament <- df_ocr_results_unnested %>% 
#   filter(str_detect(delegate_name_2, "^Ahmetovic")) %>% 
#   filter(session_date==min(session_date)) %>% 
#   distinct(session_date) %>% 
#   pull(session_date)

#only votes after elections

df_ocr_results_unnested_truncated <- df_ocr_results_unnested %>% 
  filter(session_date > lubridate::dmy("12/10/2014")) %>% 
  mutate(party=forcats::fct_drop(party)) %>% #remove empty party factor levels
  mutate_at(vars(contains("name")), ~fct_drop(.))

levels(df_ocr_results_unnested_truncated$delegate_name_2)


# ANALYSIS ----------------------------------------------------------------

#Overview

df_details_all_status_wide <- read_csv2(paste0(wdr, "/data/df_details_all_status_wide.csv")) %>% 
  mutate_at(vars(contains("status")), as.factor)


#standardize colors across plots
colors_outcome <- paletteer_d("ggsci::default_jco", n = 10) %>% as.character() 
names(colors_outcome) <- unique(c(levels(df_details_all_status_wide$HoP_status), 
                                  levels(df_details_all_status_wide$HoR_status), 
                                  levels(df_details_all_status_wide$BiH_Parl_final_status)))

colors_outcome_2 <- c("rejected"="firebrick2", 
                      "submitted and published"="green4", 
                      "legislative process suspended"="orange", 
                      "withdrawn"="red", 
                      "waiting for the procedure to start"="azure2",
                      "procedure - not started"="azure2",
                      "procedure"="azure2",
                      "not considered"="grey40",
                      "adopted"="green4")


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
  scale_fill_manual(values=colors_outcome_2)+
  #scale_fill_paletteer_d("ggsci::default_jama")+
  scale_y_continuous(breaks=seq(0,60, 20),
                     expand=expansion(mult=c(0)))+
  hrbrthemes::theme_ipsum_rc() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title=element_blank(),
        plot.margin=margin(l=0, t=0.5, b=0.5, unit="cm"),
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
  scale_fill_manual(values=colors_outcome_2)+
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
  plot_annotation(caption="Analysis: Roland Schmidt | @zoowalk | https://werk.statt.codes",
                  theme = theme_ipsum_rc()) &
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



# Legislative votes over time  ---------------------------------------------------------------

#number of votes per week
#use floor_date to aggregate witin a week
#use pdr to thicken to weeks, then pad for missing weeks, then plot;

election_dates <- c("07/10/2018", "12/10/2014") %>% 
  enframe(., 
          value="election_date",
          name=NULL) %>% 
  mutate(election_date=lubridate::dmy(election_date)) %>% 
  mutate(election_month=lubridate::floor_date(election_date, unit="month"))

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
  ggplot()+
  labs(subtitle="Number of items voted on per month.",
       title="Legislative activity",
       caption=my_caption)+
  geom_bar(aes(y=n_votes,
               x=month),
           fill="steelblue",
           stat="identity")+
  geom_vline(data=election_dates,
             aes(xintercept=election_month,
                 color="election"))+
  scale_x_date(date_breaks = "1 month",
               labels=scales::label_date(format="%b"))+  #prints year only when year changes
  scale_color_manual(values=c("election"="orange"))+
  facet_grid(house~year,
             scales="free_x",
             space="free_x",
             shrink=F)+
  theme_ipsum_rc()+
  theme(axis.text.x = element_text(angle=90, size=10),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.caption = element_markdown(hjust=c(0,1)),
        panel.spacing.x = unit(0, "cm"))+
  guides(color=guide_legend(title=NULL))


#check session number and dates

df_sessions <- df_ocr_results_unnested %>% 
  select(house, session_date, session_no) %>% 
  distinct() %>% 
  arrange(house, session_date) %>% 
  mutate(new_parlament=case_when(session_no<lag(session_no) ~ "start",
                                 TRUE ~ NA_character_)) 



# tile graph --------------------------------------------------------------
library(ggforce)
library(ggiraph)

n_record_ids <- df_ocr_results_unnested_truncated %>% 
  filter(house=="HoR") %>% 
  pull(record_id) %>% 
  n_distinct()

unique(df_ocr_results_unnested_truncated$vote)

plots_tile <- df_ocr_results_unnested_truncated %>% 
  filter(house=="HoR") %>% 
  mutate(vote=forcats::fct_collapse(vote, abstained=c("abstained", "no vote", "restrained"))) %>% 
  ggplot()+
  labs(x="item voted at",
       y="MP",
       title="Vote by MP and item",
       subtitel=my_caption)+
  geom_tile(aes(x=reorder(record_id, session_date),
                y=fct_rev(delegate_name_family),
                fill=vote))+
  # scale_y_discrete(label=delegate_name_2)+
  scale_fill_manual(values=voting_colors)+
  theme_ipsum_rc() +
  theme(axis.text.x=element_blank(),
        panel.grid = element_blank(),
        plot.margin=margin(l=0, t=0.5, b=0.5, unit="cm"),
        legend.position = "bottom",
        legend.justification = "right")+
  facet_wrap(vars(party),
             scales="free_y")

plots_tile




# tile graph new --------------------------------------------------------------
library(ggforce)
library(ggiraph)

df_delegate_index <- df_ocr_results_unnested_truncated %>% 
  filter(house=="HoR") %>% 
  mutate(delegate_name_3=str_extract(delegate_name_2, "[:alpha:]+,\\s[:alpha:]?")) %>% 
  distinct(party, delegate_name_3) %>% 
  arrange(party) %>% 
  mutate(delegate_index_seq=row_number()) %>% 
  group_by(party) %>% 
  mutate(delegate_index=row_number()) %>% 
  arrange(party)

df_tile <- df_ocr_results_unnested_truncated %>% 
  filter(house=="HoR") %>% 
  mutate(delegate_name_3=str_extract(delegate_name_2, "[:alpha:]+,\\s[:alpha:]?")) %>% 
  left_join(., df_delegate_index) 

df_tile %>% 
  ggplot()+
  labs(x="item voted at",
       y="MP",
       title="Vote by MP and item",
       subtitel=my_caption)+
  geom_tile(aes(x=reorder(record_id, session_date),
                                     y=delegate_index,
                                     fill=vote))+
  scale_y_continuous(limits=c(6, 1),
                     # labels=df_tile %>%
                     #   group_by(party) %>% pull(delegate_name_3),
                     trans="reverse")+
  scale_fill_manual(values=voting_colors)+
  theme_ipsum_rc() +
  theme(axis.text.x=element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.justification = "right")+
  facet_wrap(vars(party),
             scales="free")

plots_tile


df_ocr_results_unnested_truncated %>% 
  filter(str_detect(delegate_name, "Beci"))

# bargraph party ----------------------------------------------------------------

df_votes_delegates <- df_ocr_results_unnested_truncated %>% 
  filter(house=="HoR") %>% 
  group_by(entity, party, delegate_name_2, delegate_name_family, vote) %>% 
  summarise(n_votes_cat=n()) %>% 
  group_by(party, delegate_name_2, delegate_name_family) %>% 
  mutate(n_votes_total=sum(n_votes_cat)) %>% 
  mutate(n_votes_cat_rel=n_votes_cat/n_votes_total) %>% 
  ungroup()
  
df_votes_delegates %>%  
  ggplot()+
  geom_bar(aes(x=delegate_name_family, 
               y=n_votes_cat,
               fill=vote),
          # width=0.3,
          position=position_stack(),
          stat="identity")+
  #coord_flip()+
  # facet_wrap(vars(party),
  #          #  space="free",
  #            scales="free")+
  facet_grid(vote~party,
             #ncol=3,
             space="free",
             scales="free")+
  scale_fill_manual(values=voting_colors)+
  theme_ipsum_rc()+
  theme(axis.text.x=element_text(angle=90, hjust=1))

df_votes_delegates %>% 
  ggplot()+
  # geom_boxplot_interactive(aes(x=party,
  #                              y=n_votes_cat_rel))+
  geom_point_interactive(aes(x=party,
                               y=n_votes_cat_rel))+
  facet_col(~vote,
            scales="free_y")



# # MP ranking per vote category  -----------------------------------------


library(tidytext) #includes reorder_within function
library(lemon)

df_votes_delegates %>% 
  mutate(delegate_name_3=str_extract(delegate_name_2, "[:alpha:]+,\\s[:alpha:]?")) %>% 
  ggplot()+
  labs(title="MP's votes by category (in %)",
       subtitle ="In how many % of all her votes did the MP vote/was.... Absolute number in category and total in brackets",
       caption=my_caption)+
  geom_bar(aes(x=reorder_within(paste0(delegate_name_3, " (", party, ")"), n_votes_cat_rel, vote),
               y=n_votes_cat_rel,
               fill=entity),
           stat="identity",
           position=position_stack())+
  geom_text(aes(x=reorder_within(paste0(delegate_name_3, " (", party, ")"), n_votes_cat_rel, vote),
                y=1.2,
                label=paste0(scales::percent(n_votes_cat_rel, accuracy =0.1), " (", n_votes_cat, "/", n_votes_total, ")")),
            family = "Roboto Condensed",
            color = "grey30",
            hjust = 1,
            stat="identity")+
  scale_fill_paletteer_d("ggsci::default_jama")+
  scale_x_reordered()+
  scale_y_continuous(minor_breaks = NULL,
                     labels=scales::label_percent(accuracy=1),
                     breaks=seq(0,0.8,.1))+
  theme_ipsum_rc()+
  theme(axis.text.x = element_text(angle=90),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing = unit(0, "cm"),
        strip.text = element_text(face="bold"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_markdown(hjust=c(0,1)),
        axis.title.y = element_blank(),
        axis.title.x = element_text(hjust=0),
        legend.position = "none")+
  lemon::facet_rep_wrap(~vote,
                        repeat.tick.labels = T,
                        ncol=2,
                        scales="free_y")+
  coord_flip()


# Boxplot: per party per decision per vote type ---------------------------

x <- df_ocr_results_unnested_truncated %>%
  filter(house=="HoR") %>% 
 # mutate(record_id=as_factor(record_id)) %>% 
  group_by(party, record_id, vote, .drop=F) %>%   #important: create entry for every vote type even if not used; for %
  summarise(n_obs=n()) 
check
 x %>% 
   group_by(party) %>% 
   summarise(n_obs=n())

x %>% 
  arrange(party, record_id, vote, .by_group=T) %>% 
  group_by(party, record_id) %>% 
  mutate(n_votes_record=sum(n_obs)) %>% 
  mutate(n_votes_record_rel=n_obs/n_votes_record) %>% 
  filter(n_votes_record!=1) %>% 
  ggplot()+
  geom_boxplot(aes(y=n_votes_record_rel,
                    x=party))+
  geom_point(aes(y=n_votes_record_rel,
                   x=party),
             stat="identity",
             position="jitter")+
  facet_wrap(.~vote)


library(ggridges)
x %>% 
  arrange(party, record_id, vote, .by_group=T) %>% 
  group_by(party, record_id) %>% 
  mutate(n_votes_record=sum(n_obs)) %>% 
  mutate(n_votes_record_rel=n_obs/n_votes_record) %>% 
  filter(n_votes_record!=1) %>% 
  ggplot()+
  geom_density_ridges(aes(x=n_votes_record_rel,
                 y=party),
                 rel_min_height = 0.005)+
  facet_wrap(.~vote)+
  theme_ridges()


#check
df_ocr_results_unnested_truncated %>% 
  filter(house=="HoR") %>% 
  group_by(party) %>% 
  summarise(unique_record_ids=n_distinct(record_id))

unique_record_ids <- length(unique(df_ocr_results_unnested_truncated$record_id))
unique_party <- length(unique(df_ocr_results_unnested_truncated$party))
unique_vote <- length(unique(df_ocr_results_unnested_truncated$vote))
unique_record_ids  * unique_party * unique_vote



# delegates and party -----------------------------------------------------

colors_ethnicity=c("Bosniak"="green",
                   "Croat"="blue",
                   "Serb"="red",
                   "civic"="orange",
                   "independent"="grey")

unique(df_ocr_results_unnested_truncated$ethnicity)

df_delegate_voting <- df_ocr_results_unnested_truncated %>% 
  filter(house=="HoR") %>% 
  mutate(ethnicity=forcats::fct_relevel(ethnicity, "Bosniak", "Croat", "Serb", "civic", "independent")) %>% 
  group_by(ethnicity, party, delegate_name, vote, .drop=T) %>% 
  summarise(nobs=n()) %>%
  mutate(total.votes=sum(nobs, na.rm = T)) %>% 
  mutate(perc=nobs/total.votes) %>% 
  ungroup()

plot_delegates <- df_delegate_voting %>% 
  group_split(vote) %>% 
  map(~ggplot(., aes(x=reorder_within(party, desc(perc), ethnicity),
                     y=perc,
                     group=party,
                     tooltip=delegate_name,
                     color=ethnicity))+
        geom_jitter_interactive(width=0.03)+
        geom_boxplot()+
        labs(title=paste(unique(.$vote)))+
        hrbrthemes::theme_ipsum_rc()+
        theme(legend.position="none",
              axis.title = element_blank(),
              axis.text.x=element_text(angle=45, 
                                       hjust=0.5),
              strip.text.y = element_text(angle=180, 
                                          vjust=1,
                                          hjust=1),
              plot.margin = margin(t=0, b=0, unit="cm"),
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.spacing.x = unit(0.3, "cm"),
              panel.spacing.y = unit(0.3, "cm"))+
        scale_y_continuous(labels=scales::label_percent(accuracy=1),
                           minor_breaks = NULL,
                           limits=c(0,NA),
                           position = "right")+
        scale_x_reordered()+
        scale_color_manual(values=colors_ethnicity, na.value="grey")+
        #scale_color_paletteer_d("ggsci::default_jama")+
        facet_row(~ethnicity,
                  # switch = "y",
                  scales="free_x",
                  space="free"))

wrap_plot_delegats <- wrap_plots(plot_delegates, ncol = 1)

girafe(ggobj=wrap_plot_delegats,
       pointsize=6,
       width_svg=10, height_svg=15)



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



            
            
# entity voting (jitter)-----------------------------------------------------------

df_entity_voting <- df_ocr_results_unnested_truncated %>% 
  filter(house=="HoR") %>% 
  group_by(house, entity, law_id, record_id, vote, .drop=F) %>%  #drop to include all vote types
  summarise(votes_n=n()) %>% 
  filter(vote!="not present") %>% #of those present? PENDING!
  mutate(entity_delegates=sum(votes_n)) %>% 
  mutate(votes_rel=votes_n/entity_delegates) %>% 
  filter(vote=="yes") %>% 
  ungroup() %>% 
  arrange(law_id, record_id) %>% 
  #select(entity, record_id, votes_rel) %>% 
  pivot_wider(id_cols=c(law_id, record_id), names_from=entity, values_from = c(entity_delegates, votes_rel, votes_n)) %>% 
  mutate(votes_yes_total_abs = votes_n_FBiH + votes_n_RS) %>% 
  mutate(votes_yes_total_rel = votes_yes_total_abs/(entity_delegates_FBiH+entity_delegates_RS)) %>% 
  mutate(overall_majority = ifelse(votes_yes_total_rel>0.5, "yes", "no")) %>% 
  mutate(RS_veto = ifelse(votes_rel_RS<0.3, "yes", "no"),
         FBiH_veto = ifelse(votes_rel_FBiH<0.3, "yes", "no"))

library(here)
df_voting_results_links <- readr::read_csv2(here("data", "df_voting_results_links.csv"))

df_entity_voting <- df_entity_voting %>% 
  left_join(., df_voting_results_links) %>% 
  left_join(., df_details_all_status_wide %>% 
              select(doc_name_eng,
                     seq_page_links))

df_ocr_results_unnested_truncated %>% 
  filter(is.na(record_id))

#one observation wiht NA
df_entity_voting %>%
  filter_at(vars(votes_rel_FBiH, votes_rel_RS, overall_majority),
            any_vars(is.na(.)))

plot_entity_voting <- df_entity_voting %>% 
  ggplot()+
  labs(x="yes vote-share among FBiH representatives",
       subtitle="Legislative period 2014-2019. House of Representatives only. Click on
       dots to open voting record.",
       title="Vote results by entity support in BiH HoR (incl. entity vetoes)",
       caption=my_caption,
       y="yes vote-share among RS representatives")+
  geom_rect(data=. %>% slice(1),
            aes(xmin=1/3, xmax=Inf,
                ymin=-Inf, ymax=1/3),
            alpha=0.3,
            fill="#DF8F44FF")+
  geom_rect(data=. %>% slice(1),
            aes(xmin=-Inf, xmax=1/3,
                ymin=1/3, ymax=1),
            alpha=0.7,
            fill=paletteer_d("ggsci::default_jama")[1])+
  geom_rect(data=. %>% slice(1),
            aes(xmin=-Inf, xmax=1/3,
                ymin=-Inf, ymax=1/3),
            alpha=0.3,
            fill="grey")+
  geom_text(data=. %>% slice(1), aes(x=.02,
                                     y=.2),
            check_overlap = T,
            hjust=0,
            size=4,
            fontface="italic",
            family = "Roboto Condensed",
            color = "grey30",
            label="both entities with vetoes")+
  geom_text(data=. %>% slice(1), aes(x=.02,
                                     y=.7),
            check_overlap = T,
            hjust=0,
            size=4,
            fontface="italic",
            family = "Roboto Condensed",
            color = "white",
            label="Fed entity  veto")+
  geom_text(data=. %>% slice(1), aes(x=.55,
                                     y=.20),
            check_overlap = T,
            hjust=0,
            size=4,
            fontface="italic",
            family = "Roboto Condensed",
            color = "grey30",
            label="RS entity  veto")+
  geom_jitter_interactive(aes(x=votes_rel_FBiH,
                              y=votes_rel_RS,
                              data_id=record_id,
                              color=overall_majority,
                              shape=overall_majority,
                              tooltip=paste(str_wrap(doc_name_eng, 30), "\n",
                                        #    "record link", "<a href='", link_to_voting_record, 
                                        #    "'>", record_id, "</a>\n",
                                            "total yes:", percent(votes_yes_total_rel, accuracy=0.01),"\n",
                                            "Fed:", percent(votes_rel_FBiH, accuracy=0.01),"\n",
                                            "RS:", percent(votes_rel_RS, accuracy=0.01), "\n","\n",
                                        "Click on dot to open pertaining voting record"),
                              onclick=paste0('window.open("', link_to_voting_record,'")')),
                          size=2) +
  geom_vline(xintercept = c(1/3))+
  geom_hline(yintercept = c(1/3))+
  theme_ipsum_rc()+
  theme(legend.position = "bottom",
        legend.justification = "right",
        legend.title=element_text(size=10),
        plot.caption=element_markdown(hjust=c(0, 1)),
        axis.text=element_text(size=8),
        axis.title = element_text(size=12),
        panel.grid = element_blank())+
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     minor_breaks = NULL,
                     breaks=c(seq(0, 1, .25), 1/3),
                     expand=expansion(mult=0.02))+
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     breaks=c(seq(0, 1, .25), 1/3),
                     minor_breaks = NULL,
                     expand=expansion(mult=0.02))+
  scale_color_manual(values=c("yes"="seagreen",
                              "no"="firebrick"),
                     na.translate=F,
                     name="Overall majority:")+
  scale_shape_manual(values=c("yes"=19, "no"=1), #not all symbols can be used in ggiraph
                     na.translate=F,
                     name="Overall majority:")

plot_entity_voting
girafe(ggobj=plot_entity_voting,
       options=list(opts_tooltip(css = "background-color:lightgray; font-family:Roboto Condensed;",
                                 delay_mouseout = 5000)),
       pointsize=6,
       width_svg = 8,
       height_svg = 8)


x <- df_entity_voting %>% 
  select(link_to_voting_record) %>% 
  slice(1)
u <- "record_id"

paste0("<a href='", x, "'>", u, "</a>")



# entity voting (bar) --------------------------------------------------

df_overall_majority<- df_entity_voting %>% 
  filter(overall_majority=="no") %>% 
  group_by(overall_majority) %>% 
  summarise(n_obs=n()) %>% 
  mutate(type="no overall majority")

df_entity_vetoes<- df_entity_voting %>% 
  filter(overall_majority=="yes") %>% 
  select(contains("veto")) %>% 
  group_by_all() %>% 
  summarise(n_obs=n()) %>% 
  mutate(type=case_when(RS_veto=="yes" & FBiH_veto=="no" ~ "RS entity veto only",
                        RS_veto=="yes" & FBiH_veto=="yes" ~ "RS and FBiH entity veto",
                        RS_veto=="no" & FBiH_veto=="yes" ~ "FBiH enttiy veto",
                        RS_veto=="no" & FBiH_veto=="no" ~ "passed"))

df_entity_vetoes <- bind_rows(df_overall_majority,
          df_entity_vetoes) %>% 
  select(n_obs, type) %>% 
  mutate(type=fct_rev(fct_relevel(type, "passed", "no overall majority", "RS entity veto only"))) %>% 
  arrange(desc(type)) %>% 
  mutate(n_obs_cum=cumsum(n_obs)) %>% 
  mutate(n_obs_total=sum(n_obs)) %>% 
  mutate(n_obs_rel=n_obs/n_obs_total)

df_entity_vetoes %>% 
  ggplot()+
  labs(y="total number of votes",
       title="Outcome of votes in BiH House of Representatives",
       caption=my_caption)+
  geom_bar(aes(x=as.factor(1),
               y=n_obs,
               fill=type),
           width=0.4,
           position=position_stack(),
           stat="identity")+
  geom_text(aes(x=as.factor(1),
                label=paste(n_obs,
                            "\n\n", 
                            scales::percent(n_obs_rel)),
                y=n_obs),
           position=position_stack(vjust = 0.5),
           family = "Roboto Condensed",
           color = "white",
           stat="identity")+
  coord_flip()+
  hrbrthemes::theme_ipsum_rc()+
  theme(panel.grid = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        plot.title.position="plot",
        plot.caption=element_markdown(hjust=c(0, 1)),
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.justification = "right")+
  scale_fill_paletteer_d("ggsci::default_jama",
                         name="Vote result")+
  scale_y_continuous(breaks=df_entity_vetoes$n_obs_cum,
                     expand=expansion(mult=c(0)))+
  scale_x_discrete(expand=expansion(mult=c(0)))+
  guides(fill=guide_legend(reverse = T))


# composition of Hor ------------------------------------------------------

df_ocr_results_unnested_truncated %>% 
  filter(house=="HoR") %>% 
  group_by(record_id, party) %>% 
  summarise(n_obs=n()) %>% 
  ungroup() %>% 
  select(-record_id) %>% 
  distinct() %>% 
  arrange(desc(n_obs)) %>% 
  mutate(length_party=nchar(as.character(party))) %>% 
  mutate(sum_n_obs=sum(n_obs)) #46 MPs instead of 42, indicates MPs changing parties

df_ocr_results_unnested_truncated %>% 
  filter(house=="HoR") %>% 
  group_by(record_id, party, session_date) %>% 
  summarise(n_delegates_party=n()) %>% 
  ggplot() +
  geom_bar(aes(x=reorder(record_id, session_date),
               y=n_delegates_party,
               fill=party),
           stat="identity",
           position=position_stack())

#check: there are always exact 42 MPs   
df_ocr_results_unnested_truncated %>% 
  filter(house=="HoR") %>% 
  group_by(record_id, session_date) %>% 
  summarise(delegates_n_total=n()) %>% 
  ungroup() %>% 
  ggplot()+
  geom_line(aes(x=reorder(record_id, session_date),
                y=delegates_n_total,
                group=delegates_n_total))
  

#check: n delegates by party per decision

#check: there are always exact 42 MPs   
df_ocr_results_unnested_truncated %>% 
  #filter(str_detect(party, "SDP")) %>% 
  filter(house=="HoR") %>% 
  group_by(record_id, session_date, party) %>% 
  summarise(delegates_n_total=n()) %>% 
  ungroup() %>% 
  ggplot()+
  geom_line(aes(x=reorder(record_id, session_date),
                y=delegates_n_total,
                group=party,
                color=party))+
  theme(axis.text.x = element_blank())+
  facet_wrap(~party,
             scales="free_y")

#big IF: party affiliation as indicatated on the website of the parliament; it
#does not account for potential party swaps of MPs during the 4 years; composition
#of parliament in data and composition stated on wiki are not identical; did members
#change party affiliation after election; inauguration of parliament?


# party cohesion ----------------------------------------------------------

df_party_cohesion <- df_ocr_results_unnested_truncated %>% 
  filter(house=="HoR") %>% 
  select(law_id, record_id, session_date, party, ethnicity, vote, delegate_name_2) %>% 
  group_by(law_id, record_id, session_date, party, ethnicity, vote) %>%  #drop doesn't work; would nest votes in all other factors levels
  summarise(votes_n=n()) #%>% 
  
levels(df_party_cohesion$record_id)
levels(df_party_cohesion$party)
levels(df_party_cohesion$vote)

df_party_fragmentation <- df_party_cohesion %>% 
  mutate(party_delegates_n=sum(votes_n)) %>% 
  mutate(votes_rel_sq=(votes_n/party_delegates_n)^2) %>% 
  group_by(ethnicity, session_date, party, record_id) %>% 
  summarise(fragmentation_record=1/sum(votes_rel_sq),
            party_delegates_n=sum(votes_n)) %>% 
  ungroup()

n_vote_options <- length(unique(df_ocr_results_unnested_truncated$vote))
n_vote_options

#max fragmentation per party; baseline
df_fragmentation_max <-   df_ocr_results_unnested_truncated %>% 
  select(delegate_name_2, party, record_id) %>% 
  group_by(party, record_id) %>% 
  summarise(n_delegates=n()) %>% 
  mutate(n_vote_options=n_vote_options) %>% 
  mutate(fragmentation_max=case_when(n_delegates<=n_vote_options ~ 1/(((1/n_delegates)^2)*n_delegates),
                                     n_delegates>n_vote_options ~ 1/( 
                                       ((1/n_delegates)^2*(n_vote_options-1)) +
                                       ((1/n_delegates)*(n_delegates-n_vote_options+1))^2),
                                     TRUE ~ as.numeric(0)))

df_party_fragmentation<- df_party_fragmentation %>% 
  left_join(., df_fragmentation_max %>% select(party, record_id, fragmentation_max),
            by=c("record_id"="record_id", "party"="party")) %>% 
  mutate(fragmentation_rel=fragmentation_record/fragmentation_max) #%>% 
  

df_party_fragmentation %>% 
  ggplot()+
  geom_boxplot(aes(x=party,
               y=fragmentation_rel))












ggplot()+
  geom_line(aes(x=reorder(record_id, session_date),
                y=fragmentation_rel,
                group=party))+
  facet_col(vars(party))



df_party_cohesion %>% 
  group_split(party_delegates_n) %>% 
  map(~ggplot(.)+
        geom_line(aes(x=reorder(record_id, session_date),
                y=cohesion_record,
                group=party))+
        facet_wrap(vars(party),
                   ncol = 3))

wrap_plots(plot_x)+plot_layout(ncol=1)  
  
df_party_cohesion %>% 
  ggplot()+
  geom_line(aes(x=reorder(record_id, session_date),
                      y=cohesion_record,
                      group=party))+
  facet_col(vars(party_delegates_n, party))
  





# old ---------------------------------------------------------------------




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


# Tablers -----------------------------------------------------------------

df_tablers <- df_details_all_status_wide %>% 
  filter(!is.na(HoR_tabler)) %>% 
  select(doc_name_eng, 
         contains("HOR"),
         law_id,
         -HoR_draft_law_number_date,
         -HoR_order_number_session_date_agenda_item) %>% 
  mutate(HoR_tabler=str_split(HoR_tabler, 
                              pattern=",",
                              simplify = F)) %>% 
  unnest(HoR_tabler) %>% 
  mutate(HoR_tabler=str_trim(HoR_tabler, 
                             side=c("both")))

df_tablers %>% 
  mutate(HoR_tabler=fct_rev(fct_infreq(HoR_tabler))) %>% 
  group_by(HoR_tabler, HoR_status) %>% 
  summarise(n_obs=n()) %>% 
  ggplot()+
  geom_bar(aes(x=HoR_tabler,
               y=n_obs,
               fill=HoR_status),
           stat="identity",
           position="stack")+
  coord_flip()


# Duration of Bills -------------------------------------------------------

x <- df_details_all_status_wide %>% 
  select(contains("date"),
         contains("HoR"),
         -contains("HoP"),
         law_id,
         seq_page_links,
         -HoR_draft_law_number_date,
         -HoR_order_number_session_date_agenda_item,
         -HoP_order_number_session_date_agenda_item)

names(x)
x

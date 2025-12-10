#########-----------------Summary Stats for all Agencies------------################
##This file creates scatter plots, skittles plots and scalar relationships 

library(GGally)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(forcats)
library(tidyr)
library(PerformanceAnalytics)
library(correlation)
library(plotly)



# set the working directory to the root of the repo 
setwd(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))))
getwd()

##############################
###prepare analysis dataset###
##############################


#Load the cell data
cell <- read.csv("./data/combined_airsage/national_model_Airsage_combined.csv") %>%
  mutate(Agency = case_when(Category %in% c("USFS ownership",
                                            "USFS Administrative") ~ "USFS",
                            TRUE ~ Category))


##load the social media data
social <- read.csv("./data/combined_smedia/monthly-socmed_20231207.csv")

#set the year and month columns to numeric
social$year=year(social$d2p)
social$month=month(social$d2p)

##load the observed data
observed <- read.csv("./data/observed_data/combined_monthly_observed.csv") 

#combine observed and cell data
combined_obs_cell <- merge(cell, observed, by = c("year", "month", "siteid"))

#combine observed, cell and social media data
combined_obs_cell_sm <- merge(combined_obs_cell, social, by = c("year", "month", "siteid"))

#cut to the columns we need, remove NAs, and add log columns 

combined <- combined_obs_cell_sm %>%
  select("year", "month", "siteid", "Unit_Name" = "UNIT_NAME",
         "Agency",
         "Airsage_ud" = "userdays", "visits",
         "Flickr_ud" = "pud", "Twitter_ud" = "tud",
         "AllTrails_ud" = "aud",
         "EBird_ud" = "eud") %>%
  na.omit() %>%
  mutate(across(c(-siteid, -Unit_Name, -Agency, -year, -month),
                list(log = log1p)))

##########################
###create summary stats###
##########################

# Part 1:
#create pearson correlation estimates between rec visits and each source:
#userdays, pud, tud, aud, eud, overall and by year/site
#make tables, correlation matrices, and plots (only tables for by-site, since there are too many sites to plot)

# Overall correlations
correlations_overall <- combined %>%
  select(ends_with("_log")) %>%                   
  correlation(., redundant = TRUE) %>%
  filter(Parameter1 == "visits_log",
         Parameter2 != "visits_log") %>%
  mutate(Group = "Overall")

# Overall correlations 2018 and 2019 only
correlations_overall_1819 <- combined %>%
  filter(year %in% c(2018, 2019)) %>%
  select(ends_with("_log")) %>%                   
  correlation(., redundant = TRUE) %>%
  filter(Parameter1 == "visits_log",
         Parameter2 != "visits_log") %>%
  mutate(Group = "Overall")


# Correlations by year
correlations_byyear <- combined %>%
  group_by(year) %>%
  select(ends_with("_log")) %>%
  correlation(., redundant = TRUE) %>%
  filter(Parameter1 == "visits_log",
         Parameter2 != "visits_log") 

# Correlations by agency
correlations_byagency <- combined %>%
  group_by(Agency) %>%
  select(ends_with("_log")) %>%
  correlation(., redundant = TRUE) %>%
  filter(Parameter1 == "visits_log",
         Parameter2 != "visits_log") 

# Correlations by agency for 2018 and 2019 only
correlations_byagency_1819 <- combined %>%
  filter(year %in% c(2018, 2019)) %>%
  group_by(Agency) %>%
  select(ends_with("_log")) %>%
  correlation(., redundant = TRUE) %>%
  filter(Parameter1 == "visits_log",
         Parameter2 != "visits_log") 

# Correlations by site
correlations_bysite <- combined %>%
  group_by(Unit_Name) %>%
  select(ends_with("_log")) %>%
  correlation(., redundant = TRUE) %>%
  filter(Parameter1 == "visits_log",
         Parameter2 != "visits_log") 

# Correlations by site for 2018 and 2019 only
correlations_bysite_1819 <- combined %>%
  filter(year %in% c(2018, 2019)) %>%
  group_by(Unit_Name) %>%
  select(ends_with("_log")) %>%
  correlation(., redundant = TRUE) %>%
  filter(Parameter1 == "visits_log",
         Parameter2 != "visits_log") 



# Create correlation matrix plot showing the correlations calculated above
pdf(file = "./figs/combined/combined_correlation_matrix.pdf",
    width = 9.3,
    height = 7)
chart.Correlation(combined[,c("visits_log", "Airsage_ud_log", "Flickr_ud_log",
                              "Twitter_ud_log", "AllTrails_ud_log",
                              "EBird_ud_log")])
dev.off()

#Same correlation matrix as above but showing years
pdf(file = "./figs/combined/combined_correlation_matrix_by_year.pdf",
    width = 9.3,
    height = 7)
ggpairs(combined,
        columns = c("visits_log", "Airsage_ud_log", "Flickr_ud_log",
                    "Twitter_ud_log", "AllTrails_ud_log",
                    "EBird_ud_log"),
        ggplot2::aes(colour=as.character(year)))
dev.off()




#correlation plots using UW template code

# first create a long dataset so we can just filter for whatever variable we want

combined_long <- combined %>%
  pivot_longer(cols = c("Airsage_ud", "Flickr_ud",
                        "Twitter_ud", "AllTrails_ud",
                        "EBird_ud"),
               names_to = "data_source",
               values_to = "value") %>%
  select(-c(Airsage_ud_log, visits_log, Flickr_ud_log,
            Twitter_ud_log, AllTrails_ud_log, EBird_ud_log)) %>%
  mutate(data_source = case_when(data_source == "Airsage_ud" ~ "AirSage",
                                 data_source == "Flickr_ud" ~ "Flickr",
                                 data_source == "Twitter_ud" ~ "Twitter",
                                 data_source == "AllTrails_ud" ~ "AllTrails",
                                 data_source == "EBird_ud" ~ "eBird"))


#now plot correlations between Rec Visits and all userday measures
overall_cor <- ggplot(combined_long,
                      aes(x = (value+1), y = (visits))) +
  geom_point(colour = "darkgoldenrod3", alpha = 0.25, aes(text = Unit_Name)) +
  xlab("Userdays (monthly, log)") +
  ylab("Visitors (monthly, log)") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  geom_smooth(method = "lm", se = F, col = "#1F78B4") +
  ggpubr::stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)), label.x.npc = .6 ) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(vars(data_source), scales = 'free')

ggsave("./figs/combined/combined_overall_cor_plot.pdf", plot = overall_cor)

#same as above but using colors for each site, just to see how they cluster when
#using with plotly
ggplotly(ggplot(combined_long,
                aes(x = (value+1), y = (visits))) +
           geom_point(alpha = 0.25, aes(text = Agency, colour = Agency)) +
           xlab("Userdays (monthly, log)") +
           ylab("Visitors (monthly, log)") +
           scale_x_log10(labels = scales::comma) +
           scale_y_log10(labels = scales::comma) +
           geom_smooth(method = "lm", se = F, col = "#1F78B4") +
           ggpubr::stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)), label.x.npc = .6 ) +
           theme_bw(base_size = 20) +
           theme(axis.text.x = element_text(angle = 45, hjust = 1),
                 legend.position = "none") +
           facet_wrap(vars(data_source), scales = 'free'), tooltip = c("text"))



#same things as above by year (line style); correlations between Rec Visits and all userday measures
yearly_cor_line <- ggplot(combined_long) +
  scale_color_manual(values = c(hcl.colors(n = 5, palette = "viridis"), "black"),
                     name = "Year") +
  geom_point(alpha = 0.03, aes(x = (value+1), y = (visits),
                               color = as.character(year))) +
  xlab("Userdays (monthly, log)") +
  ylab("NFS visitors (monthly, log)") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  geom_smooth(method = "lm", se = F,
              aes(x = (value+1), y = (visits),
                  color = as.character(year))) +
  geom_smooth(method = "lm", se = F, linetype = "twodash",
              aes(x = (value+1), y = (visits), color = "Overall")) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(vars(data_source), scales = 'free')

# Below is the same plot but without the overall line included
# ggplot(combined_long, aes(x = (value+1), y = (RecreationVisits),
#                             color = as.character(year))) +
# scale_color_manual(values = c("cornflowerblue", "indianred3"),
# name = "Year",
# breaks = c("2018", "2019")) +
# geom_point(alpha = 0.03) +
# xlab("Userdays (monthly, log)") +
# ylab("NFS visitors (monthly, log)") +
# scale_x_log10(labels = scales::comma) +
# scale_y_log10(labels = scales::comma) +
# geom_smooth(method = "lm", se = F) +
# theme_bw(base_size = 20) +
# theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
# facet_wrap(vars(data_source), scales = 'free')

#ggsave("./figs/NPS/nps_lineyear_cor_plot.pdf", plot = yearly_cor_line)

#same things as above by agency (line style); correlations between Visits and all userday measures
agency_cor_line <- ggplot(combined_long) +
  scale_linetype_manual(values = c(rep("solid", 3), "twodash"),
                        name = "Agency",
                        breaks = c("FWS", "NPS","USFS", "Overall")) +
  scale_color_manual(values = c("red2", "blue1", "goldenrod", "black"),
                     name = "Agency",
                     breaks = c("FWS", "NPS", "USFS", "Overall")) +
  geom_point(alpha = 0.03, aes(x = (value+1), y = (visits),
                               color = Agency)) +
  xlab("Userdays (monthly, log scale)") +
  ylab("Visitation (monthly, log scale)") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  geom_smooth(method = "lm", se = F,
              aes(x = (value+1), y = (visits),
                  color = Agency, linetype = Agency)) +
  geom_smooth(method = "lm", se = F, aes(x = (value+1), y = (visits),
                                         color = "Overall", linetype = "Overall")) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill="white")) +
  facet_wrap(vars(data_source), scales = 'free')

ggsave("./figs/combined/combined_agency_cor_plot.pdf", plot = agency_cor_line)

#Same plot but filtered to years 2018 and 2019 to match with the data used in the models

#same things as above by agency (line style); correlations between Visits and all userday measures
agency_cor_line_1819 <- ggplot(filter(combined_long, year %in% c(2018, 2019))) +
  scale_linetype_manual(values = c(rep("solid", 3), "twodash"),
                        name = "Agency",
                        breaks = c("FWS", "NPS","USFS", "Overall")) +
  scale_color_manual(values = c("red2", "blue1", "goldenrod", "black"),
                     name = "Agency",
                     breaks = c("FWS", "NPS", "USFS", "Overall")) +
  geom_point(alpha = 0.03, aes(x = (value+1), y = (visits),
                               color = Agency)) +
  xlab("Userdays (monthly, log scale)") +
  ylab("Visitation (monthly, log scale)") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  geom_smooth(method = "lm", se = F,
              aes(x = (value+1), y = (visits),
                  color = Agency, linetype = Agency)) +
  geom_smooth(method = "lm", se = F, aes(x = (value+1), y = (visits),
                                         color = "Overall", linetype = "Overall")) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill="white")) +
  facet_wrap(vars(data_source), scales = 'free')

ggsave("./figs/combined/combined_agency_cor_plot_1819.pdf",
       plot = agency_cor_line_1819,
       width = 1500,
       height = 900,
       units = "px",
       dpi = 100)




# Part 2:
#create scale estimates between RecreationVisits and each source:
#userdays, pud, tud, aud, eud, and Pop, overall and by year/site


# Overall Scale
scale_overall <- combined %>%
  summarise(scale_userdays=mean(Airsage_ud)/mean(visits),
            scale_pud=mean(Flickr_ud)/mean(visits),
            scale_tud=mean(Twitter_ud)/mean(visits),
            scale_aud=mean(AllTrails_ud)/mean(visits),
            scale_eud=mean(EBird_ud)/mean(visits),
            nobs=n())   

#by site
#create scale relationships for each site and year using dplyr by summing by site id

scale_bysite <- combined %>% 
  group_by(siteid) %>% 
  summarise(scale_userdays=mean(Airsage_ud)/mean(visits),
            scale_pud=mean(Flickr_ud)/mean(visits),
            scale_tud=mean(Twitter_ud)/mean(visits),
            scale_aud=mean(AllTrails_ud)/mean(visits),
            scale_eud=mean(EBird_ud)/mean(visits),
            nobs=n(),
            UNIT_NAME=first(Unit_Name))


scale_yearly <- combined %>% 
  group_by(year) %>% 
  summarise(scale_userdays=mean(Airsage_ud)/mean(visits),
            scale_pud=mean(Flickr_ud)/mean(visits),
            scale_tud=mean(Twitter_ud)/mean(visits),
            scale_aud=mean(AllTrails_ud)/mean(visits),
            scale_eud=mean(EBird_ud)/mean(visits),
            nobs=n())




# Part 3:
#skittle plots using UW template code, just playing and getting a feel for it


# By site skittles plot (plotted in two layers because the above way wasn't plotting in the correct order using plotly)
colors_site = c("black", hcl.colors(n = n_distinct(correlations_byagency$Group), palette = "hawaii"))
names(colors_site) = c("Overall", unique(correlations_byagency$Group))

skittles_agency <- ggplot() +
  geom_point(correlations_byagency, size = 5, shape = 21, colour = "grey33", alpha = 0.5,
             mapping = aes(x = r, y = fct_rev(Parameter2), col = Group, fill = Group, text = Group)) +
  geom_point(correlations_overall, size = 8, color = 'black', alpha = 0.5,
             mapping = aes(x = r, y = fct_rev(Parameter2), text = "Overall")) +
  geom_vline(xintercept = 0) +
  scale_fill_manual(values = colors_site, name = "Agency") +
  scale_x_continuous(limits = c(NA, 1), breaks = seq(-.25, 1, by = .25)) +
  scale_y_discrete(labels=c("Airsage_ud_log" = "Airsage (log)",
                            "AllTrails_ud_log" = "All Trails (log)",
                            "EBird_ud_log" = "eBird (log)",
                            "Flickr_ud_log" = "Flickr (log)",
                            "Twitter_ud_log" = "Twitter (log)")) +
  ylab(NULL) +
  theme_classic(base_size = 20) +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab("Correlation with Visits (log)")

#add agency to correlations_by_site
site_cor <- as.data.frame(correlations_bysite) %>%
  left_join((cell %>% select(UNIT_NAME, Agency) %>% distinct()), join_by(Group == UNIT_NAME))

agency_cor <- as.data.frame(correlations_byagency)

#skittles plot with each site shown as a small point and each agency overall shown
#as a larger point
skittles_site <- ggplot() +
  geom_point(site_cor, size = 2.5, alpha = 0.5, shape = 16,
             mapping = aes(x = r, y = fct_rev(Parameter2), 
                           color = Agency),
             position = position_dodge(width = -.5)) +
  geom_point(agency_cor, size = 5,  alpha = 1, shape = 21, col = "grey30",
             mapping = aes(x = r, y = fct_rev(Parameter2), fill = Group),
             position = position_dodge(width = -.5)) +
  geom_vline(xintercept = 0) +
  scale_fill_manual(values = c("red2", "blue1",  "goldenrod"),
                     name = "Agency",
                     breaks = c("FWS", "NPS", "USFS")) +
  scale_color_manual(values = c("red2", "blue1",  "goldenrod"),
                    name = "Agency",
                    breaks = c("FWS", "NPS", "USFS")) +
  #scale_fill_manual(values = colors_site, name = "Agency") +
  scale_x_continuous(limits = c(NA, 1), breaks = seq(-.25, 1, by = .25)) +
  scale_y_discrete(labels=c("Airsage_ud_log" = "AirSage",
                            "AllTrails_ud_log" = "AllTrails",
                            "EBird_ud_log" = "eBird",
                            "Flickr_ud_log" = "Flickr",
                            "Twitter_ud_log" = "Twitter")) +
  ylab(NULL) +
  theme_classic(base_size = 20) +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab("Correlation")

ggsave("./figs/combined/combined_skittles.pdf", plot = skittles_site)

#same as above but for 2018 and 2019 only

site_cor_1819 <- as.data.frame(correlations_bysite_1819) %>%
  left_join((cell %>% select(UNIT_NAME, Agency) %>% distinct()), join_by(Group == UNIT_NAME))

agency_cor_1819 <- as.data.frame(correlations_byagency_1819)


skittles_site_1819 <- ggplot() +
  geom_point(site_cor_1819, size = 2.5, alpha = 0.5, shape = 16,
             mapping = aes(x = r, y = fct_rev(Parameter2), 
                           color = Agency),
             position = position_dodge(width = -.5)) +
  geom_point(agency_cor_1819, size = 5,  alpha = 1, shape = 21, col = "grey30",
             mapping = aes(x = r, y = fct_rev(Parameter2), fill = Group),
             position = position_dodge(width = -.5)) +
  geom_vline(xintercept = 0) +
  scale_fill_manual(values = c("red2", "blue1",  "goldenrod"),
                    name = "Agency",
                    breaks = c("FWS", "NPS", "USFS")) +
  scale_color_manual(values = c("red2", "blue1",  "goldenrod"),
                     name = "Agency",
                     breaks = c("FWS", "NPS", "USFS")) +
  #scale_fill_manual(values = colors_site, name = "Agency") +
  scale_x_continuous(limits = c(NA, 1), breaks = seq(-.25, 1, by = .25)) +
  scale_y_discrete(labels=c("Airsage_ud_log" = "AirSage",
                            "AllTrails_ud_log" = "AllTrails",
                            "EBird_ud_log" = "eBird",
                            "Flickr_ud_log" = "Flickr",
                            "Twitter_ud_log" = "Twitter")) +
  ylab(NULL) +
  theme_classic(base_size = 20) +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab("Correlation")

ggsave("./figs/combined/combined_skittles_1819.pdf",
       plot = skittles_site_1819,
       width = 1300,
       height = 900,
       units = "px",
       dpi = 100)





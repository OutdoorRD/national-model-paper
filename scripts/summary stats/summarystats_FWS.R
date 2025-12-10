#########-----------------Summary Stats for FWS------------################
##This file creates scatter plots, skittles plots and scalar relationships for FWS 

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
cell <- read.csv("./data/combined_airsage/national_model_Airsage_combined.csv")
#cut to forest service data
cell <- cell[which(cell$Category == "FWS"),]

##load the social media data
social <- read.csv("./data/combined_smedia/monthly-socmed_20231207.csv")

#set the year and month columns to numeric
social$year=year(social$d2p)
social$month=month(social$d2p)

##load the FWS observed data
observed <- read.csv("./data/observed_data/FWS_monthly_observed.csv") %>%
  rename(year = Year,
         month = Month) %>%
  mutate(month = match(month, month.abb))
#missing Baskett Slough National Wildlife Refuge, Ridgefield National Wildlife Refuge, and Tijuana Slough National Wildlife Refuge
#which are in the cell data


#combine observed and cell data
combined_obs_cell <- merge(cell, observed, by = c("year", "month", "siteid"))

#combine observed, cell and social media data
combined_obs_cell_sm <- merge(combined_obs_cell, social, by = c("year", "month", "siteid"))

#cut to the columns we need, remove NAs, and add log columns 

combined <- combined_obs_cell_sm %>%
  select("year", "month", "siteid", "Unit_Name" = "UNIT_NAME.x",
         "Airsage_ud" = "userdays", "visitors",
         "Flickr_ud" = "pud", "Twitter_ud" = "tud",
         "AllTrails_ud" = "aud",
         "EBird_ud" = "eud") %>%
  na.omit() %>%
  mutate(across(c(-siteid, -Unit_Name,
                  -year, -month), list(log = log1p)))

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
  filter(Parameter1 == "visitors_log",
         Parameter2 != "visitors_log") %>%
  mutate(Group = "Overall")


# Correlations by year
correlations_byyear <- combined %>%
  group_by(year) %>%
  select(ends_with("_log")) %>%
  correlation(., redundant = TRUE) %>%
  filter(Parameter1 == "visitors_log",
         Parameter2 != "visitors_log") 

# Correlations by site
correlations_bysite <- combined %>%
  group_by(Unit_Name) %>%
  select(ends_with("_log")) %>%
  correlation(., redundant = TRUE) %>%
  filter(Parameter1 == "visitors_log",
         Parameter2 != "visitors_log") 

#the warnings in the above table are due to missing data (zeros) for Wolf Creek
#for Flickr and Twitter


# Create correlation matrix plot showing the correlations calculated above
pdf(file = "./figs/FW/fw_correlation_matrix.pdf",
    width = 9.3,
    height = 7)
chart.Correlation(combined[,c("visitors_log", "Airsage_ud_log", "Flickr_ud_log",
                              "Twitter_ud_log", "AllTrails_ud_log",
                              "EBird_ud_log")])
dev.off()

#Same correlation matrix as above but showing years
pdf(file = "./figs/FW/fw_correlation_matrix_by_year.pdf",
    width = 9.3,
    height = 7)
ggpairs(combined,
        columns = c("visitors_log", "Airsage_ud_log", "Flickr_ud_log",
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
               values_to = "value") 


#now plot correlations between Rec Visits and all userday measures
overall_cor <- ggplot(combined_long,
                      aes(x = (value+1), y = (visitors))) +
  geom_point(colour = "darkgoldenrod3", alpha = 0.25, aes(text = Unit_Name)) +
  xlab("Userdays (monthly, log)") +
  ylab("FWS visitors (monthly, log)") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  geom_smooth(method = "lm", se = F, col = "#1F78B4") +
  ggpubr::stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)), label.x.npc = .6 ) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(vars(data_source), scales = 'free')

ggsave("./figs/FW/fw_overall_cor_plot.pdf", plot = overall_cor)

#same as above but using colors for each site, just to see how they cluster when
#using with plotly
ggplotly(ggplot(combined_long,
                aes(x = (value+1), y = (visitors))) +
           geom_point(alpha = 0.25, aes(text = Unit_Name, colour = Unit_Name)) +
           xlab("Userdays (monthly, log)") +
           ylab("FWS visitors (monthly, log)") +
           scale_x_log10(labels = scales::comma) +
           scale_y_log10(labels = scales::comma) +
           geom_smooth(method = "lm", se = F, aes(colour  = Unit_Name)) +
           ggpubr::stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)), label.x.npc = .6 ) +
           theme_bw(base_size = 20) +
           theme(axis.text.x = element_text(angle = 45, hjust = 1),
                 legend.position = "none") +
           facet_wrap(vars(data_source), scales = 'free'), tooltip = c("text"))



#same things as above by year (line style); correlations between visitors and all userday measures
yearly_cor_line <- ggplot(combined_long) +
  scale_color_manual(values = c(hcl.colors(n = 5, palette = "viridis"), "black"),
                     name = "Year") +
  geom_point(alpha = 0.2, aes(x = (value+1), y = (visitors),
                               color = as.character(year))) +
  xlab("Userdays (monthly, log)") +
  ylab("FWS visitors (monthly, log)") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  geom_smooth(method = "lm", se = F,
              aes(x = (value+1), y = (visitors),
                  color = as.character(year))) +
  geom_smooth(method = "lm", se = F, linetype = "twodash",
              aes(x = (value+1), y = (visitors), color = "Overall")) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(vars(data_source), scales = 'free')

# Below is the same plot but without the overall line included
# ggplot(combined_long, aes(x = (value+1), y = (visitors),
#                             color = as.character(year))) +
# scale_color_manual(values = hcl.colors(n = 5, palette = "viridis"),
# name = "Year") +
# geom_point(alpha = 0.03) +
# xlab("Userdays (monthly, log)") +
# ylab("NFS visitors (monthly, log)") +
# scale_x_log10(labels = scales::comma) +
# scale_y_log10(labels = scales::comma) +
# geom_smooth(method = "lm", se = F) +
# theme_bw(base_size = 20) +
# theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
# facet_wrap(vars(data_source), scales = 'free')



ggsave("./figs/FW/fw_lineyear_cor_plot.pdf", plot = yearly_cor_line)




# Part 2:
#create scale estimates between RecreationVisits and each source:
#userdays, pud, tud, aud, eud, and Pop, overall and by year/site


# Overall Scale
scale_overall <- combined %>%
  summarise(scale_userdays=mean(Airsage_ud)/mean(visitors),
            scale_pud=mean(Flickr_ud)/mean(visitors),
            scale_tud=mean(Twitter_ud)/mean(visitors),
            scale_aud=mean(AllTrails_ud)/mean(visitors),
            scale_eud=mean(EBird_ud)/mean(visitors),
            nobs=n())   

#by site
#create scale relationships for each site and year using dplyr by summing by site id

scale_bysite <- combined %>% 
  group_by(siteid) %>% 
  summarise(scale_userdays=mean(Airsage_ud)/mean(visitors),
            scale_pud=mean(Flickr_ud)/mean(visitors),
            scale_tud=mean(Twitter_ud)/mean(visitors),
            scale_aud=mean(AllTrails_ud)/mean(visitors),
            scale_eud=mean(EBird_ud)/mean(visitors),
            nobs=n(),
            UNIT_NAME=first(Unit_Name))


scale_yearly <- combined %>% 
  group_by(year) %>% 
  summarise(scale_userdays=mean(Airsage_ud)/mean(visitors),
            scale_pud=mean(Flickr_ud)/mean(visitors),
            scale_tud=mean(Twitter_ud)/mean(visitors),
            scale_aud=mean(AllTrails_ud)/mean(visitors),
            scale_eud=mean(EBird_ud)/mean(visitors),
            nobs=n())




# Part 3:
#skittle plots using UW template code, just playing and getting a feel for it


# Make a dataset for the by year skittles plot
yr <- rbind(correlations_overall, correlations_byyear) %>%
  arrange(Group) # so that overall is at the bottom and plotted last

#Set colors
colors = c("black", hcl.colors(n = n_distinct(yr$Group) -1, palette = "hawaii"))
names(colors) = c("Overall", unique(yr$Group)[!unique(yr$Group) %in% c("Overall")])

#Set sizes
sz = c(8, rep(5, times = n_distinct(yr$Group) -1))
names(sz) = c("Overall", unique(yr$Group)[!unique(yr$Group) %in% c("Overall")])

skittles_year <- ggplot(yr) +
  geom_point(alpha = 0.5, 
             aes(x = r, y = fct_rev(Parameter2), col = Group, size = Group)) +
  geom_vline(xintercept = 0) +
  scale_color_manual(values = colors, name = "Year") +
  scale_size_manual(values = sz, name = "Year") +
  scale_x_continuous(limits = c(NA, 1), breaks = seq(-.25, 1, by = .25)) +
  scale_y_discrete(labels=c("Airsage_ud_log" = "Airsage (log)",
                            "AllTrails_ud_log" = "All Trails (log)",
                            "EBird_ud_log" = "eBird (log)",
                            "Flickr_ud_log" = "Flickr (log)",
                            "Twitter_ud_log" = "Twitter (log)")) +
  ylab(NULL) +
  theme_classic(base_size = 20) +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab("Correlation with Fish and Wildlife Visits (log)")


ggsave("./figs/FW/fw_skittles_year.pdf", plot = skittles_year)

# By site skittles plot (plotted in two layers because the above way wasn't plotting in the correct order using plotly)
#Set colors
colors_site = c("black", hcl.colors(n = n_distinct(correlations_bysite$Group), palette = "hawaii"))
names(colors_site) = c("Overall", unique(correlations_bysite$Group))

skittles_site <- ggplot() +
  geom_point(correlations_bysite, size = 5, shape = 21, colour = "grey33",
             mapping = aes(x = r, y = fct_rev(Parameter2), col = Group, fill = Group, text = Group)) +
  geom_point(correlations_overall, size = 8, color = 'black',
             mapping = aes(x = r, y = fct_rev(Parameter2), text = "Overall")) +
  geom_vline(xintercept = 0) +
  scale_fill_manual(values = colors_site) +
  scale_x_continuous(limits = c(NA, 1), breaks = seq(-.25, 1, by = .25)) +
  scale_y_discrete(labels=c("Airsage_ud_log" = "Airsage (log)",
                            "AllTrails_ud_log" = "All Trails (log)",
                            "EBird_ud_log" = "eBird (log)",
                            "Flickr_ud_log" = "Flickr (log)",
                            "Twitter_ud_log" = "Twitter (log)")) +
  ylab(NULL) +
  theme_classic(base_size = 20) +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none") +
  xlab("Correlation with Fish and Wildlife Visits (log)") +
  ggtitle("Correlation by Park")
#ignore the warning about ignoring unknown aesthetics--this is just for plotting
#in plotly


#The below is another way to plot using the same colour for each site
# ggplot() +
#   geom_point(correlations_bysite,
#              size = 5, colour = "cornflowerblue", alpha = 0.5, shape = 16,
#              mapping = aes(x = r, y = fct_rev(Parameter2), text = Group)) +
#   geom_point(correlations_overall, size = 8, color = 'black',
#              mapping = aes(x = r, y = fct_rev(Parameter2), text = "Overall")) +
#   geom_vline(xintercept = 0) +
#   scale_x_continuous(limits = c(NA, 1), breaks = seq(-.25, 1, by = .25)) +
#   scale_y_discrete(labels=c("Airsage_ud_log" = "Airsage (log)",
#                             "AllTrails_ud_log" = "All Trails (log)",
#                             "EBird_ud_log" = "eBird (log)",
#                             "Flickr_ud_log" = "Flickr (log)",
#                             "Twitter_ud_log" = "Twitter (log)")) +
#   ylab(NULL) +
#   theme_classic(base_size = 20) +
#   theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
#         legend.position = "none") +
#   xlab("Correlation with Fish and Wildlife Visits (log)") +
#   ggtitle("Correlation by Park")

ggplotly(skittles_site, tooltip = c("text"))

ggsave("./figs/FW/fw_skittles_site.pdf", plot = skittles_site)


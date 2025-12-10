#########-----------------Summary Stats for USFS------------################
##This file creates scatter plots, skittles plots and scalar relationships for the USFS 

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
cell <- cell[which(cell$Category %in% c("USFS ownership", "USFS Administrative")),]

##load the social media data
social <- read.csv("./data/combined_smedia/monthly-socmed_20231207.csv")

#set the year and month columns to numeric
social$year=year(social$d2p)
social$month=month(social$d2p)

##load the USFS observed data
observed <- read.csv("./data/observed_data/FS_monthly_observed.csv")

##load the population data for admin boundaries
pop <- read.csv("./data/population/FS_admin_boundary_pop.csv")

#combine observed and cell data
combined_obs_cell <- merge(cell, observed, by = c("year", "month", "siteid"))

#combine observed, cell and social media data
combined_obs_cell_sm <- merge(combined_obs_cell, social, by = c("year", "month", "siteid"))

#combine observed, cell, social media, and population data,
#cut to the columns we need, remove NAs, and add log columns 

combined <- merge(combined_obs_cell_sm, pop, by = c("siteid"), suffixes = c(".xx", ".yy")) %>%
  select("year", "month", "siteid", "Unit_Name" = "UNIT_NAME.x",
         "Airsage_ud" = "userdays", "month_nfv",
         "Flickr_ud" = "pud", "Twitter_ud" = "tud",
         "AllTrails_ud" = "aud",
         "EBird_ud" = "eud", "Population"= "Pop") %>%
  na.omit() %>%
  mutate(across(c(-siteid, -Unit_Name,
                  -year, -month), list(log = log1p)))

##########################
###create summary stats###
##########################

# Part 1:
#create pearson correlation estimates between month_nfv and each source:
#userdays, pud, tud, aud, eud, and Pop, overall and by year/site
#make tables, correlation matrices, and plots (only tables for by-site, since there are too many sites to plot)

# Overall correlations
correlations_overall <- combined %>%
  select(ends_with("_log")) %>%                   
  correlation(., redundant = TRUE) %>%
  filter(Parameter1 == "month_nfv_log",
         Parameter2 != "month_nfv_log") %>%
  mutate(Group = "Overall")


# Correlations by year
correlations_byyear <- combined %>%
  group_by(year) %>%
  select(ends_with("_log")) %>%
  correlation(., redundant = TRUE) %>%
  filter(Parameter1 == "month_nfv_log",
         Parameter2 != "month_nfv_log") 

# Correlations by site
correlations_bysite <- combined %>%
  group_by(Unit_Name) %>%
  select(ends_with("_log")) %>%
  correlation(., redundant = TRUE) %>%
  filter(Parameter1 == "month_nfv_log",
         Parameter2 != "month_nfv_log") 

#don't worry about warnings for the above table, I think it's just due to each 
#site only having one population estimate so a correlation can't be computed


# Create correlation matrix plot showing the correlations calculated above
pdf(file = "./figs/USFS/usfs_correlation_matrix.pdf",
    width = 9.3,
    height = 7)
chart.Correlation(combined[,c("month_nfv_log", "Airsage_ud_log", "Flickr_ud_log",
                              "Twitter_ud_log", "AllTrails_ud_log",
                              "EBird_ud_log", "Population_log")])
dev.off()

#Same correlation matrix as above but showing years
pdf(file = "./figs/USFS/usfs_correlation_matrix_by_year.pdf",
    width = 9.3,
    height = 7)
ggpairs(combined,
        columns = c("month_nfv_log", "Airsage_ud_log", "Flickr_ud_log",
                    "Twitter_ud_log", "AllTrails_ud_log",
                    "EBird_ud_log", "Population_log"),
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


#now plot correlations between NFV and all userday measures
overall_cor <- ggplot(combined_long,
                      aes(x = (value+1), y = (month_nfv))) +
  geom_point(aes(col = Population)) +
  scale_color_gradientn(colours = hcl.colors(n = 7, palette = "hawaii", rev = TRUE)) +
  xlab("Userdays (monthly, log)") +
  ylab("NFS visitors (monthly, log)") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  geom_smooth(method = "lm", se = F, col = "#1F78B4") +
  ggpubr::stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)), label.x.npc = .6 ) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(vars(data_source), scales = 'free')

ggsave("./figs/USFS/usfs_overall_cor_plot.pdf", plot = overall_cor)

#same thing as above but by year (grid style); correlations between NFV and all userday measures
yearly_cor_grid <- ggplot(combined_long,
                      aes(x = (value+1), y = (month_nfv))) +
  geom_point(colour = "#D55E00", alpha = 0.3) +
  xlab("Userdays (monthly, log)") +
  ylab("NFS visitors (monthly, log)") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  geom_smooth(method = "lm", se = F, col = "#1F78B4") +
  ggpubr::stat_cor(cor.coef.name = "r", aes(label = after_stat(r.label)), label.x.npc = .6 ) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(year ~ data_source, scales = 'free')

ggsave("./figs/USFS/usfs_gridyear_cor_plot.pdf", plot = yearly_cor_grid)

#same things as above by year (line style); correlations between NFV and all userday measures
yearly_cor_line <- ggplot(combined_long) +
  scale_color_manual(values = c(hcl.colors(n = 5, palette = "viridis"), "black"),
                     name = "Year") +
  geom_point(alpha = 0.03, aes(x = (value+1), y = (month_nfv),
                               color = as.character(year))) +
  xlab("Userdays (monthly, log)") +
  ylab("NFS visitors (monthly, log)") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  geom_smooth(method = "lm", se = F,
              aes(x = (value+1), y = (month_nfv),
                  color = as.character(year))) +
  geom_smooth(method = "lm", se = F, linetype = "twodash",
              aes(x = (value+1), y = (month_nfv), color = "Overall")) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(vars(data_source), scales = 'free')

# Below is the same plot but without the overall line included
# ggplot(combined_long, aes(x = (value+1), y = (month_nfv),
#                             color = as.character(year))) +
# scale_color_manual(values = hcl.colors(n = 5, palette = "viridis"),
#                    name = "Year") +
# geom_point(alpha = 0.03) +
# xlab("Userdays (monthly, log)") +
# ylab("NFS visitors (monthly, log)") +
# scale_x_log10(labels = scales::comma) +
# scale_y_log10(labels = scales::comma) +
# geom_smooth(method = "lm", se = F) +
# theme_bw(base_size = 20) +
# theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
# facet_wrap(vars(data_source), scales = 'free')



ggsave("./figs/USFS/usfs_lineyear_cor_plot.pdf", plot = yearly_cor_line)




# Part 2:
#create scale estimates between month_nfv and each source:
#userdays, pud, tud, aud, eud, and Pop, overall and by year/site


# Overall Scale
scale_overall <- combined %>%
    summarise(scale_userdays=mean(Airsage_ud)/mean(month_nfv),
                scale_pud=mean(Flickr_ud)/mean(month_nfv),
                scale_tud=mean(Twitter_ud)/mean(month_nfv),
                scale_aud=mean(AllTrails_ud)/mean(month_nfv),
                scale_eud=mean(EBird_ud)/mean(month_nfv),
                nobs=n())   

#by site
#create scale relationships for each site and year using dplyr by summing by site id

scale_bysite <- combined %>% 
  group_by(siteid) %>% 
  summarise(scale_userdays=mean(Airsage_ud)/mean(month_nfv),
            scale_pud=mean(Flickr_ud)/mean(month_nfv),
            scale_tud=mean(Twitter_ud)/mean(month_nfv),
            scale_aud=mean(AllTrails_ud)/mean(month_nfv),
            scale_eud=mean(EBird_ud)/mean(month_nfv),
            nobs=n(),
            UNIT_NAME=first(Unit_Name))


scale_yearly <- combined %>% 
  group_by(year) %>% 
  summarise(scale_userdays=mean(Airsage_ud)/mean(month_nfv),
            scale_pud=mean(Flickr_ud)/mean(month_nfv),
            scale_tud=mean(Twitter_ud)/mean(month_nfv),
            scale_aud=mean(AllTrails_ud)/mean(month_nfv),
            scale_eud=mean(EBird_ud)/mean(month_nfv),
            nobs=n())




# Part 3:
#skittle plots using UW template code, just playing and getting a feel for it


# Make a dataset for the by year skittles plot
yr <- rbind(correlations_overall, correlations_byyear) %>%
  filter(Parameter2 != "Population_log") %>%
  arrange(Group) # so that overall is at the bottom and plotted last

#Set colors
colors = c("black", hcl.colors(n = n_distinct(yr$Group) -1, palette = "hawaii"))
names(colors) = c("Overall", unique(yr$Group)[!unique(yr$Group) %in% c("Overall")])

#Set sizes
sz = c(8, rep(5, times = n_distinct(yr$Group) -1))
names(sz) = c("Overall", unique(yr$Group)[!unique(yr$Group) %in% c("Overall")])

skittles_year <- ggplot(yr) +
  geom_point(aes(x = r, y = fct_rev(Parameter2), col = Group, size = Group)) +
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
  xlab("Correlation with National Forest Visits (log)")


ggsave("./figs/USFS/usfs_skittles_year.pdf", plot = skittles_year)

# By site skittles plot (plotted in two layers because the above way wasn't plotting in the correct order using plotly)


skittles_site <- ggplot() +
  geom_point(filter(correlations_bysite,
                    Parameter2 != "Population_log"),
             size = 5, colour = "cornflowerblue", alpha = 0.25, shape = 16,
             mapping = aes(x = r, y = fct_rev(Parameter2), text = Group)) +
  geom_point(filter(correlations_overall,
                    Parameter2 != "Population_log"), size = 8, color = 'black',
             mapping = aes(x = r, y = fct_rev(Parameter2), text = "Overall")) +
  geom_vline(xintercept = 0) +
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
  xlab("Correlation with National Forest Visits (log)")

#The below is another way to plot using different colors for each site

#Set colors
# colors_site = c("black", hcl.colors(n = n_distinct(correlations_bysite$Group), palette = "hawaii"))
# names(colors_site) = c("Overall", unique(correlations_bysite$Group))
# 
# skittles_site <- ggplot() +
#   geom_point(filter(correlations_bysite,
#                     Parameter2 != "Population_log"), size = 5, shape = 21, colour = "grey33",
#              mapping = aes(x = r, y = fct_rev(Parameter2), col = Group, fill = Group, text = Group)) +
#   geom_point(filter(correlations_overall,
#                     Parameter2 != "Population_log"), size = 8, color = 'black',
#              mapping = aes(x = r, y = fct_rev(Parameter2), text = "Overall")) +
#   geom_vline(xintercept = 0) +
#   scale_fill_manual(values = colors_site) +
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
#   xlab("Correlation with National Forest Visits (log)")
#ignore the warning about ignoring unknown aesthetics--this is just for plotting
#in plotly

ggplotly(skittles_site, tooltip = c("text"))

ggsave("./figs/USFS/usfs_skittles_site.pdf", plot = skittles_site)


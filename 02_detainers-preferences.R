rm(list = ls())
options(scipen = 999)

# Working directory

# wd <- "~/Dropbox/immig_sheriffs"  #Asya
wd <- "D:/Dropbox/Projects/immigration/immigsheriffs/"  #Angelo
setwd(wd)



# Load packages

library(tidyverse)
library(fixest)


# Plot theme setting

library(ggridges)
theme_plot <- function () { 
  theme_linedraw(base_size=10) %+replace% 
    theme( 
      panel.grid.minor=element_blank(),
      panel.grid.major = element_line(color = "grey95"),
      strip.text=element_text(color="Black",size=10,margin=margin(0,0,8,0)),
      strip.background=element_rect(fill=NA,color=NA),
      plot.title=element_text(size=12,margin=margin(0,0,10,0)),
      axis.title.x=element_text(size=10,margin=margin(10,0,0,0)),
      axis.title.y=element_text(size=10,angle=90,margin=margin(0,10,0,0)))
}
theme_set(theme_plot())





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import data ----


# Ideal point estimates of immigration policy preferences [CES]
# Detainer requests
# Sheriff elections

full <- read_csv("cleaned_data/detainers_ip.csv") |>
  mutate(
    detained_per1000 = ndet_notdenied/(foreign/1000),
    compliance = ndet_notdenied/detainees,
    theta_all_rescale = scales::rescale(theta_all),
    theta_cit_rescale = scales::rescale(theta_cit),
    prop_unemp = unemp_imp/100) |>
  filter(county != "Dade County") #duplicate from TRAC data?







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Descriptive analyses ----


# ... Ideal points ####

# ...... Distribution of theta  ####

#Long dataset to use
full_popstrat_long <- full |>
  select(year, state, county_fips, theta_all_rescale, theta_cit_rescale) |>
  pivot_longer(
    cols = theta_all_rescale:theta_cit_rescale, 
    names_to = "pop_poststrat", 
    values_to = "theta") |>
  mutate(pop_poststrat = if_else(
    grepl("all", pop_poststrat), 
    "Total", 
    "Citizen"))

#Theta distribution (all,) for all years and counties
full_popstrat_long |>
  ggplot() +
  geom_histogram(
    aes(x = theta, fill = pop_poststrat),  
    col = "white", alpha = .4, position = "stack", binwidth = .025) +
  xlim(0, 1) +
  scale_fill_manual(values = c("grey70", "black")) +
  theme(legend.positio = "bottom") +
  labs(
    title = "Ideal point distribution",
    x = expression(theta),
    y = "Frequency",
    fill = "Post-stratification population")
ggsave(filename = "draft/figures/hist_ideal-points.pdf", plot = last_plot(),
       width = 2, height = 2, units = "in", scale = 2.5)

#Distribution by year
full_popstrat_long |> 
  ggplot() + 
  geom_histogram(
    aes(x = theta, fill = pop_poststrat),  
    col = "white", alpha = .4, position = "stack", binwidth = .025) +
  facet_wrap(. ~ year, ncol = 5, scales = "free_y") +
  xlim(0, 1) +
  scale_fill_manual(values = c("grey60", "black")) +
  theme(legend.position = "bottom") +
  labs(
    title = "Ideal point distribution, by year",
    x = expression(theta),
    y = "Frequency",
    fill = "Post-stratification population")
ggsave(filename = "draft/figures/hist_ideal-points_year.pdf", plot = last_plot(),
       width = 6, height = 3, units = "in", scale = 1.5)


# ...... Distribution by region ####

#Southwest
full_popstrat_long |> 
  filter(state %in% c("AZ", "CA", "CO", "FL", "NM", "TX")) |>
  ggplot() + 
  geom_histogram(
    aes(x = theta, fill = pop_poststrat),  
    col = "white", alpha = .4, position = "stack", binwidth = .025) +
  facet_wrap(. ~ state, ncol = 3, scales = "free_y") +
  xlim(0, 1) +
  scale_fill_manual(values = c("grey60", "black")) +
  theme(legend.position = "bottom") +
  labs(
    title = "Ideal point distribution, by year: Southwest",
    x = expression(theta),
    y = "Frequency",
    fill = "Post-stratification population")
ggsave(filename = "draft/figures/hist_ideal-points_region-southwest.pdf", plot = last_plot(),
       width = 6, height = 4.5, units = "in", scale = 1.5)

#South
full_popstrat_long |> 
  filter(state %in% c("AL", "GA", "LA", "MS", "NC", "SC")) |>
  ggplot() + 
  geom_histogram(
    aes(x = theta, fill = pop_poststrat),  
    col = "white", alpha = .4, position = "stack", binwidth = .025) +
  facet_wrap(. ~ state, ncol = 3, scales = "free_y") +
  xlim(0, 1) +
  scale_fill_manual(values = c("grey60", "black")) +
  theme(legend.position = "bottom") +
  labs(
    title = "Ideal point distribution, by year: South",
    x = expression(theta),
    y = "Frequency",
    fill = "Post-stratification population")
ggsave(filename = "draft/figures/hist_ideal-points_region-south.pdf", plot = last_plot(),
       width = 6, height = 4.5, units = "in", scale = 1.5)

#Midwest
full_popstrat_long |> 
  filter(state %in% c("IA", "IL", "IN", "MI", "MN", "WI")) |>
  ggplot() + 
  geom_histogram(
    aes(x = theta, fill = pop_poststrat),  
    col = "white", alpha = .4, position = "stack", binwidth = .025) +
  facet_wrap(. ~ state, ncol = 3, scales = "free_y") +
  xlim(0, 1) +
  scale_fill_manual(values = c("grey60", "black")) +
  theme(legend.position = "bottom") +
  labs(
    title = "Ideal point distribution, by year: Midwest",
    x = expression(theta),
    y = "Frequency",
    fill = "Post-stratification population")
ggsave(filename = "draft/figures/hist_ideal-points_region-midwest.pdf", plot = last_plot(),
       width = 6, height = 4.5, units = "in", scale = 1.5)

#Northest/Midatlantic
full_popstrat_long |> 
  filter(state %in% c("CT", "MA", "MD", "PA", "NJ", "NY")) |>
  ggplot() + 
  geom_histogram(
    aes(x = theta, fill = pop_poststrat),  
    col = "white", alpha = .4, position = "stack", binwidth = .025) +
  facet_wrap(. ~ state, ncol = 3, scales = "free_y") +
  xlim(0, 1) +
  scale_fill_manual(values = c("grey60", "black")) +
  theme(legend.position = "bottom") +
  labs(
    title = "Ideal point distribution, by year: Northeast and Mid-Atlantic",
    x = expression(theta),
    y = "Frequency",
    fill = "Post-stratification population")
ggsave(filename = "draft/figures/hist_ideal-points_region-northeast.pdf", plot = last_plot(),
       width = 6, height = 4.5, units = "in", scale = 1.5)




# ... Top 10 least and most restrictive ####

#Table: Top 10 max
theta_max10 <- full |> 
  group_by(county_fips, county, state) |>
  summarize(theta_all_rescale = mean(theta_all_rescale, na.rm = TRUE)) |>
  ungroup() |> slice_max(order_by = theta_all_rescale, n = 10) 

#Table: Top 10 min
theta_min10 <- full |> 
  group_by(county_fips, county, state) |>
  summarize(theta_all_rescale = mean(theta_all_rescale, na.rm = TRUE)) |>
  ungroup() |> slice_min(order_by = theta_all_rescale, n = 10) 

#Plot by year
bind_rows(
  full |> filter(county_fips %in% theta_min10$county_fips) |> mutate(least_most = "Least restrictive"),
  full |> filter(county_fips %in% theta_max10$county_fips) |> mutate(least_most = "Most restrictive")) |>
  ggplot(aes(x = as.factor(year), y = theta_all_rescale, group = county_fips, linetype = least_most)) +
  geom_line(col = "grey60") +
  ylim(0, 1) +
  theme(legend.position = "bottom") +
  scale_linetype_manual(values = c("dashed", "solid")) +
  labs(
    title = "Ideal points of top 10 least and most restrictive counties, by year",
    x = "Year",
    y = expression(theta["all"]),
    linetype = "")
ggsave(filename = "draft/figures/line_ideal-points_least-most.pdf", plot = last_plot(),
       width = 2, height = 2, units = "in", scale = 3)




# ... Detainer-reliant enforcement measures ####

full |>
  filter(!is.infinite(detained_per1000)) |>
  ggplot(aes(x = compliance, y = detained_per1000)) +
  geom_point(shape = 19, size = .1) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  scale_y_log10() +
  labs(
    title = "Detainer measures",
    x = "Detainer request compliance",
    y = "Detained per 1000 foreigners")
ggsave(filename = "draft/figures/scatter_compliance-detained.pdf", plot = last_plot(),
       width = 2, height = 2, units = "in", scale = 2)


# ...... Detainers: Compliance ####

#Table: Top 10 max and min compliance rates
# compliance_max10 <- full |> 
#   group_by(state) |>
#   summarize(compliance = mean(compliance, na.rm = TRUE)) |>
#   ungroup() |> slice_max(order_by = compliance, n = 10) 

compliance_min10 <- full |> 
  group_by(county_fips, county, state) |>
  summarize(compliance = mean(compliance, na.rm = TRUE)) |>
  ungroup() |> slice_min(order_by = compliance, n = 10) 

#Compliance requests < 100% (majority at 100%)
full |>
  filter(compliance < 1) |>
  ggplot() +
  geom_histogram(
    aes(
      x = compliance, 
      y = ..density..),  
    col = "white",
    fill = "grey20",
    binwidth = .025)

#Southwestern
full |>
  filter(state %in% c("AZ", "CA", "CO", "FL", "NM", "TX")) |>
  ggplot() +
  geom_histogram(
    aes(
      x = compliance, 
      y = ..density..),  
    col = "white",
    fill = "grey20",
    binwidth = .10) +
  facet_wrap(. ~ state, ncol = 3, scale = "free_y") +
  labs(title = "Southwestern states")

#Southern
full |>
  filter(state %in% c("AL", "GA", "LA", "MS", "NC", "SC")) |>
  ggplot() +
  geom_histogram(
    aes(
      x = compliance, 
      y = ..density..),  
    col = "white",
    fill = "grey20",
    binwidth = .10) +
  facet_wrap(. ~ state, ncol = 3, scale = "free_y") +
  labs(title = "Southern states")

#Mid-Atlantic/Northeast states
full |>
  filter(state %in% c("CT", "MA", "MD", "PA", "NJ", "NY")) |>
  ggplot() +
  geom_histogram(
    aes(
      x = compliance, 
      y = ..density..),  
    col = "white",
    fill = "grey20",
    binwidth = .10) +
  facet_wrap(. ~ state, ncol = 3, scale = "free_y") +
  labs(title = "Mid-Atlantic and Northeastern states")


# ...... Detainers: Detained per 1000 foreign ####

full |> 
  select(year, county_fips, detainees, ndet_notdenied, foreign, detained_per1000) |>
  arrange(desc(detained_per1000)) %>% View()

#Table: Top 10 min and max detained per 1000
detained_max10 <- full |> 
  filter(!is.infinite(detained_per1000)) |>
  group_by(county_fips, county, state) |>
  summarize(detained_per1000 = mean(detained_per1000, na.rm = TRUE)) |>
  ungroup() |> slice_max(order_by = detained_per1000, n = 10) 
detained_min10 <- full |> 
  filter(!is.infinite(detained_per1000)) |>
  group_by(county_fips, county, state) |>
  summarize(detained_per1000 = mean(detained_per1000, na.rm = TRUE)) |>
  ungroup() |> slice_min(order_by = detained_per1000, n = 10) 

#Detainers per 1000 foreigners (log scale, linear labels)
full |>
  ggplot() +
  geom_histogram(
    aes(x = log(detained_per1000), y = ..density..),
    fill = "grey20", col = "white") +
  scale_x_log10()

#Southwestern (ridgeline density)
full |>
  filter(
    state %in% c("AZ", "CA", "CO", "FL", "NM", "TX"),
    !is.infinite(detained_per1000)) |>
  ggplot() +
  geom_density_ridges2(
    aes(x = log(detained_per1000),y = state),
    rel_min_height = 0.005, scale = 2,
    fill = "grey60",
    alpha = .5) +
  scale_x_log10() +
  scale_y_discrete(limits = rev) +
  labs(title = "Southwestern states")

#Southern (ridgeline density)
full |>
  filter(
    state %in% c("AL", "GA", "LA", "MS", "NC", "SC"),
    !is.infinite(detained_per1000)) |>
  ggplot() +
  geom_density_ridges2(
    aes(x = log(detained_per1000),y = state),
    rel_min_height = 0.005, scale = 2,
    fill = "grey60",
    alpha = .5) +
  scale_x_log10() +
  scale_y_discrete(limits = rev) +
  labs(title = "Southern states")

#Mid-Atlantic/Northeast states
full |>
  filter(
    state %in% c("CT", "MA", "MD", "PA", "NJ", "NY"),
    !is.infinite(detained_per1000)) |>
  ggplot() +
  geom_density_ridges2(
    aes(x = log(detained_per1000),y = state),
    rel_min_height = 0.005, scale = 2,
    fill = "grey60",
    alpha = .5) +
  scale_x_log10() +
  scale_y_discrete(limits = rev) +
  labs(title = "Mid-Atlantic and Northeastern states")








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Regression analyses ----




lm(
  data = full,
  compliance ~ 
    theta_all_rescale +
    prop_foreign +
    prop_unemp) |>
  summary()


lm(
  data = full,
  detained_ ~ 
    theta_all_rescale +
    prop_foreign +
    prop_unemp) |>
  summary()










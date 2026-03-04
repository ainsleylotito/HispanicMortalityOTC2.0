library(dplyr)
library(ggplot2)

# -----------------------------
# GLOBAL CONSTANTS
# -----------------------------

age_levels <- c(
  "0-4","5-9","10-14","15-19","20-24",
  "25-29","30-34","35-39","40-44",
  "45-49","50-54","55-59","60-64",
  "65-69","70-74","75-79","80-84",
  "85-89","90+"
)

set_age_order <- function(df){
  df %>% mutate(age_group=factor(age_group, levels=age_levels, ordered=TRUE))
}


# -----------------------------
# CORE RATE CALCULATION
# -----------------------------

calc_rates <- function(data, years, sex_val, cod_val){
  
  data %>%
    filter(year %in% years,
           sex == sex_val,
           cod == cod_val) %>%
    group_by(race_eth, age_group) %>%
    summarise(
      deaths = sum(deaths, na.rm=TRUE),
      pop    = sum(population, na.rm=TRUE),
      .groups="drop"
    ) %>%
    mutate(rate = deaths/pop*100000) %>%
    set_age_order()
}



# -----------------------------
# RATE RATIOS
# -----------------------------

calc_rr <- function(df, ref="white"){
  df %>%
    group_by(age_group) %>%
    mutate(
      ref_rate  = rate[race_eth==ref],
      ref_count = deaths[race_eth==ref],
      rr = rate/ref_rate,
      rr_lcl = rr*exp(-1.96*sqrt(1/deaths+1/ref_count)),
      rr_ucl = rr*exp( 1.96*sqrt(1/deaths+1/ref_count))
    ) %>%
    ungroup() %>%
    filter(race_eth!=ref)
}



# -----------------------------
# CI FUNCTIONS
# -----------------------------

ci_log <- function(rate,count){
  tibble(
    lcl = rate*exp(-1.96*sqrt(1/count)),
    ucl = rate*exp( 1.96*sqrt(1/count))
  )
}

ci_exact <- function(deaths,pop){
  tibble(
    lcl=(qchisq(.025,2*deaths)/2)/pop*100000,
    ucl=(qchisq(.975,2*(deaths+1))/2)/pop*100000
  )
}



# -----------------------------
# PLOTTING FUNCTIONS
# -----------------------------

plot_rr <- function(df, title){
  
  ggplot(df,
         aes(age_group, rr, color=race_eth, group=race_eth))+
    
    geom_hline(yintercept=1, linetype="dashed")+
    geom_line(linewidth=1)+
    geom_point(size=2)+
    geom_errorbar(aes(ymin=rr_lcl, ymax=rr_ucl), width=.15)+
    
    labs(title=title,
         x="Age group",
         y="Rate Ratio",
         color="Race/Ethnicity")+
    
    scale_color_discrete(labels = c(
      "hispanic" = "Hispanic",
      "white" = "Non-Hispanic White",
      "black" = "Non-Hispanic Black"
    ))+
    
    theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(1.2, "cm"),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1)
  )
}



plot_rate <- function(df,title,ylabel="Rate per 100,000"){
  
  ggplot(df,
         aes(age_group, rate, color=race_eth, group=race_eth))+
    
    geom_line(linewidth=1)+
    geom_point(size=2)+
    geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.15)+
    
    labs(title=title,
         x="Age group",
         y=ylabel,
         color="Race/Ethnicity")+
    
    scale_color_discrete(labels = c(
      "hispanic" = "Hispanic",
      "white" = "Non-Hispanic White",
      "black" = "Non-Hispanic Black"
    ))+
    
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_text(size = 14),
          legend.text  = element_text(size = 12),
          legend.key.size = unit(1.2, "cm"),
          legend.position = c(0.98, 0.98),
          legend.justification = c(1, 1)
    )  
}



plot_yearly <- function(df,title){
  
  pd <- position_dodge(.3)
  
  ggplot(df,aes(year, rate, color=race_eth))+
    
    geom_line(linewidth=1)+
    geom_point(size=2,position=pd)+
    geom_errorbar(aes(ymin=lcl,ymax=ucl),
                  width=.2,position=pd)+
    
    labs(title=title,
         x="Year",
         y="Rate per 100,000",
         color="Race/Ethnicity")+
    
    
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 1))+
    
    scale_color_discrete(labels = c(
      "hispanic" = "Hispanic",
      "white" = "Non-Hispanic White",
      "black" = "Non-Hispanic Black"
    ))+
    
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_text(size = 14),
          legend.text  = element_text(size = 12),
          legend.key.size = unit(1.2, "cm")
    )  
}

# ---------------------------- 
# OVERLAY FUNCTIONS 
# ---------------------------- 
plot_rate_overlay <- function(new, old, title){
  
  ggplot(new, aes(age_group, rate, color=race_eth, group=race_eth)) +
    
    geom_line(size=1) +
    geom_point(size=2) +
    geom_errorbar(aes(ymin=lcl,ymax=ucl), width=.15) +
    
    geom_line(data=old, linetype="dashed", size=1) +
    geom_point(data=old, shape=1, size=2) +
    geom_errorbar(data=old,
                  aes(ymin=lcl,ymax=ucl),
                  linetype="dashed",
                  width=.15) +
    
    labs(title=title,
         subtitle="Solid = 2007–2020, Dashed = 1999–2006",
         x="Age", y="Rate per 100k") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
} 


plot_rr_overlay <- function(new, old, title){
  
  ggplot(new, aes(age_group, rr, color=race_eth, group=race_eth)) +
    geom_hline(yintercept=1, linetype="dashed", color="gray50") +
    
    geom_line(size=1) +
    geom_point(size=2) +
    geom_errorbar(aes(ymin=rr_lcl, ymax=rr_ucl), width=.15) +
    
    geom_line(data=old, linetype="dashed", size=1) +
    geom_point(data=old, shape=1, size=2) +
    geom_errorbar(data=old,
                  aes(ymin=rr_lcl,ymax=rr_ucl),
                  linetype="dashed",
                  width=.15) +
    
    labs(title=title,
         subtitle="Solid = 2007–2020, Dashed = 1999–2006",
         x="Age", y="Rate Ratio") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

#comparison data for RRs
comparison_data <- function(df1, df2) {df1 %>%
  left_join(df2,
            by=c("age_group","race_eth"),
            suffix=c("_new","_old")) %>%
  mutate(diff = rr_new - rr_old,
         pct_change = ((rr_new - rr_old)/rr_old * 100),
         rr_ratio = rr_new / rr_old)
}

#making heatmap to compare old/new rrs
heatmap_rr_comparison <- function(df){ 
  ggplot(df, aes(age_group, race_eth, fill = rr_ratio)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 1) +
  labs(title="Change in Disparity (RR Ratio)",
       x="Age Group",
       y="Race/Ethnicity")
}

#comparing rates by age group
rate_comparison_data <- function(df_new, df_old){
  
  df_new %>%
    left_join(df_old,
              by = c("age_group","race_eth"),
              suffix = c("_new","_old")) %>%
    mutate(
      rate_diff = rate_new - rate_old,
      pct_change = ((rate_new - rate_old)/rate_old) * 100,
      rate_ratio = rate_new / rate_old
    )
}

#heatmap for rate changes by age group
heatmap_rate_comparison <- function(df){
  
  ggplot(df,
         aes(age_group, race_eth, fill = rate_ratio)) +
    geom_tile() +
    scale_fill_gradient2(midpoint = 1) +
    labs(title = "Change in Mortality Rates (Rate Ratio)",
         x = "Age Group",
         y = "Race/Ethnicity")
}


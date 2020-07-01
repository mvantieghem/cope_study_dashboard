
## related measures of distress 

# open the distress data...
load("../../clean_data/impact_disruption_cleaned.Rda")

# re-organize 
job_distress_df <- pred_vars %>%
  select(contains("job_distress"), record_id) %>%
  mutate_if(is.numeric, funs (convert_7pt_likert(.))) %>%
  drop_na()

# combine datasets and calculate% endorsed for each item.
job_distress_long <- job_distress_df %>%
  gather(key = "Factor", value = "Rating", -record_id) %>%
  mutate(Factor = ifelse(Factor ==  "current_job_distress", "Current Impact",
                         ifelse(Factor == "future_job_distress", "Future Impact",  NA))) %>% 
  # remove reduced access to social interactions, does into different category!
  filter(!is.na(Factor)) %>%
  group_by(Factor, Rating) %>%
  summarize(n = n(), 
            Percent = round(n()/nrow(distress_df)*100)) %>%
  # make sure the factors are in logical order for plots.
  mutate(Rating = fct_relevel(Rating, "No distress", "Mild distress", "Moderate distress", "High distress"))

#distress_long

# for now, plot a single one as test case.
job_distress_plot <- job_distress_long %>%
  ggplot(aes(x = Rating, y = Percent)) + theme_bw() + 
  geom_bar(stat = "identity", alpha = 0.5,aes(fill = Factor),
           color = "black", position = position_dodge()) +
  my_colors_fill + coord_flip() + 
  ylim(0, 50) + xlab("") +
  theme(axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 10), 
        legend.position = "top")

job_distress_plot
```

percent_job_future <- job_distress_long$Percent[job_distress_long$Rating == "High distress" & job_distress_long$Factor == "future Impact"]

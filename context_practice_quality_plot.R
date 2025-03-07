library(tidyverse)
library(tidylog)

raw <- read_csv("updated_adapt.csv", name_repair = "universal")
map <- read_csv("pedagogy_instruction_mapping.csv")

#' no context 0, practice 1 = practice under revealed context.
#' no context 1, practice 1 = practice without revealed context
#' N column has a bug and is showing the total n across all 4 No.Context and practice columns
#' Size column has the n values we want to use
#' Small p `practice` encodes whether or not (1/0) the teacher's initial request revealed the practice
#' Big P `Practice` has the description of the practice
#' `No.Context` encodes whether or not (0/1) the teacher revealed the context information
# Drop the no context complexity and just focus on no.context 0 - practice = 1 cases


dat <- raw |> 
  mutate(case = case_when(
    No.Context == 0 & practice == 1 ~ "Revealed Context", 
    No.Context == 1 & practice == 1 ~ "No Revealed Context",
    TRUE ~ NA
  )) |> 
  left_join(map, by = "Practice", relationship = "many-to-one") |> 
  filter(!is.na(case)) |> 
  filter(case == "Revealed Context") |> 
  select(-case) 

dat_long <- dat |> 
  pivot_longer(cols = c(Size, starts_with("Response.")), names_to = "score_type") |> 
  mutate(score_type = gsub("Response\\.", "", score_type),
         score_type = gsub("Size", "N", score_type),
         score_type = factor(score_type, 
                             levels = c("N", "Accuracy", "Relevance", "Usefulness")), 
         Context = str_wrap(Context, width = 15), # play with this number to adjust facet titles
  ) 

# By Practice
ggplot(data = dat_long |> filter(Pedagogy == "Student Engagement and Motivation"),
       aes(x = score_type, y = Practice, 
           fill = if_else(score_type == "N", NA, value))) +  # Exclude 'N' from fill
  facet_wrap(vars(Context), nrow = 1) +
  geom_tile(aes(fill = if_else(score_type == "N", NA, value)), color = "black") +  # No fill for 'N'
  geom_text(aes(label = round(value, 1), color = if_else(score_type == "N", "black", "white"))) +  # Make 'N' text black
  scale_fill_gradient(na.value = "transparent") +  # Default blue scale
  scale_color_identity() +  # Use the color identity for text (black for 'N', white for others)
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank(),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(margin = margin(t = 10)), 
        legend.position = "none") +
  labs(caption = "Low quality context-practice combinations are indicated with a darker background",
       x = NULL, y = NULL, Fill = NULL)

# By Pedagogy

# Sorry, ugly hacky code to roll up by pedagogy with the weighted average by Size
# ugh, reconvert to long, but this time keep Size as a sep column so we can use it in a weighted average
dat2 <- dat |> 
  pivot_longer(cols = c(starts_with("Response.")), names_to = "score_type") |> # remove Size
  group_by(Pedagogy, score_type, Context) |> 
  summarize(value = weighted.mean(value, Size), 
            Size = sum(Size), 
            .groups = "drop")

# Hackery to add back Size as a row (with label N)
dat2_long <- dat2 |> 
  select(-Size) |> 
  bind_rows(dat2 |> 
              filter(score_type == "Response.Accuracy") |>  # doesnt matter which one pick here, just pick one 
              select(Pedagogy, Context, value = Size) |> # drop the value column (implicitly) and rename Size to value
              mutate(score_type = "Size")) |> 
  mutate(score_type = gsub("Response\\.", "", score_type),
         score_type = gsub("Size", "N", score_type),
         score_type = factor(score_type, 
                             levels = c("N", "Accuracy", "Relevance", "Usefulness")), 
         Context = str_wrap(Context, width = 15), # play with this number to adjust facet titles
  )

ggplot(data = dat2_long,
       aes(x = score_type, y = Pedagogy, 
           fill = if_else(score_type == "N", NA, value))) +  # Exclude 'N' from fill
  facet_wrap(vars(Context), nrow = 1) +
  geom_tile(aes(fill = if_else(score_type == "N", NA, value)), color = "black") +  # No fill for 'N'
  geom_text(aes(label = round(value, 1), color = if_else(score_type == "N", "black", "white"))) +  # Make 'N' text black
  scale_fill_gradient(na.value = "transparent") +  # Default blue scale
  scale_color_identity() +  # Use the color identity for text (black for 'N', white for others)
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank(),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(margin = margin(t = 10)), 
        legend.position = "none") +
  labs(caption = "Low quality context-practice combinations are indicated with a darker background",
       x = NULL, y = NULL, Fill = NULL)



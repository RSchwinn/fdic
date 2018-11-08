library(tidyverse)
# Always Start Here (loads the data) ----
df = readRDS("../data/fdic/working_df.RDS")
definitions = read_csv("even_better_chosen_list.csv")

# Rich's sample plot ----
df %>%
    select(name, rssdhcr, fed_rssd, asset, date) %>%
    arrange(desc(asset)) %>%
    head()

# Use fed_rssd 852218 to filter by individual bank
# Only use rssdhcr if you are interested in seeing all banks under a given holding company.


df_1 = df %>%
    filter(fed_rssd == 852218)

# fixes scientific notation
options(scipen = 99)

library(scales)

ggplot(data = df_1, aes(x = dep, y = asset)) +
    geom_point(alpha = 0.5)+
    scale_x_continuous(label = comma)+
    scale_y_continuous(label = comma)+
    ggtitle(df_1$name[nrow(df_1)])
    

# Vicky's Visualization ----
recent_df = df %>% 
    filter( date == "2018-03-31")

ggplot(data = df, 
       mapping = aes(sample=asset))+
    geom_histogram()

ggplot(data = df, 
       mapping = aes(sample=asset))+
    geom_qq()

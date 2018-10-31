library(tidyverse)

# Always Start Here (loads the data) ----
df = readRDS("../data/fdic/working_df.RDS")
definitions = read_csv("even_better_chosen_list.csv")

# Rich's sample plot
df %>%
    select(name, rssdhcr, asset, date) %>%
    arrange(desc(asset)) %>%
    head()

# Use rssdhcr 1039502 to filter
df_1 = df %>%
    filter(rssdhcr == 1039502)

# fixes scientific notation
options(scipen = 99)

library(scales)

ggplot(data = df_1, aes(x = dep, y = asset)) +
    geom_point(alpha = 0.5)+
    scale_x_continuous(label = comma)+
    scale_y_continuous(label = comma)+
    ggtitle(df_1$name[1])
    
scale_y_continuous(label=comma, limits=c(min(df$Cost[df$Date > left]), 
                                         max(df$Cost[df$Date > left])))


# Vicky's Visualization ----
recent_df = df %>% 
    filter( date == "2018-03-31")

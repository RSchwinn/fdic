# Always start here (loads the data) ----
library(tidyverse)
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

ggplot(data = df_1, 
       mapping = aes(x = dep, y = asset)) +
    geom_point(alpha = 0.5)+
    geom_smooth(alpha = 0.1)+
    scale_x_continuous(label = comma)+
    scale_y_continuous(label = comma)+
    xlab("Deposits ($1000)")+
    ylab("Assets ($1000)")+
    theme_test()+
    ggtitle(df_1$name[nrow(df_1)])
    

# Vicky's visualization ----
recent_df = df %>% 
    filter( date == "2018-03-31",
            cb == 1
            )%>%
    select(name, rssdhcr, dep, fed_rssd, asset, date)

ggplot(data = recent_df, 
       mapping = aes(x=asset))+
    geom_dotplot()

ggplot(data = recent_df, 
       mapping = aes(sample=asset))+
    geom_qq()


library(plotly)

p = ggplot(data = recent_df, 
           mapping = aes(x=asset))+
    geom_histogram(bins = 100)

ggplotly(p)

p = ggplot(data = recent_df, 
       mapping = aes(x = dep, y = asset, text = paste("Bank:", name))) +
    geom_point(alpha = 0.5)+
    scale_x_continuous(label = comma)+
    scale_y_continuous(label = comma)+
    xlab("Deposits ($1000)")+
    ylab("Assets ($1000)")+
    theme_test()
ggplotly(p)



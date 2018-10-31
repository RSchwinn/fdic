require(lubridate)
require(dplyr)
require(ggplot2)
require(ggthemes)
require(scales)
require(plotly)

# Previously Useful for Choosing the Chosen ----
# start_time = Sys.time()
df = readRDS("../data/fdic/combined_FDIC.RDS")

# changes variables to lowercase
names(df) = tolower(names(df))

# converts factors to characters
i <- sapply(df, is.factor)
df[i] <- lapply(df[i], as.character)

# loads definitions file
defs = read.csv("fdic_definitions.csv")
defs$Variable = tolower(defs$Variable)

# removes duplicate variables
defs = (defs[!duplicated(defs$Variable),])
df = df[,!duplicated(names(df))]

# ensures definitions file contains only variables described in dataframe
defs = defs %>%
    filter(Variable %in% names(df))


# re-arranges df by alpha
df = df[,order(names(df))]

# makes definitions alphabetical
defs = defs[order(defs$Variable),]

# ensures variables are ordered correctly
sum(names(df) == defs$Variable)

defs = cbind(defs,matrix(NA, nrow(defs), 7))
names(defs)[6:12] = names(summary(df[,1]))

for(i in 1:nrow(defs)){
    if(is.numeric(df[,i])){
        it = summary(df[,i])
    defs[i,6:(5+length(it))] = as.numeric(summary(df[,i]))
    }
}

# # loads Vicky's selected banks
# vws = readxl::read_excel("../data/fdic/vws.xlsx")
# names(vws) = tolower(names(vws))
# vws = (vws[!duplicated(vws$variable),])
# vws = vws %>%
#     filter(variable %in% names(df), `show "r"` == "Y")
# selected = vws$variable
# 
# defs$vw_selected = defs$Variable %in% selected

# write.csv(defs, "../data/fdic/better_definitions_table.csv")



# unique(df$fed_rssd)[2]
# one_df = df %>%
#         filter(fed_rssd == unique(df$fed_rssd)[1])
# 
# # transpose
# one_df = as.data.frame(t(one_df))
# 
# # adds names
# one_df = cbind.data.frame(variable = rownames(one_df),one_df)
# 
# # selects interesting variables
# a = one_df[,c("variable","V1","V2")]
# 
# a$variable = tolower(a$variable)
# defs$Variable = tolower(defs$Variable)
# 
# defs = unique(defs[,c("Variable","Definition")])
# 
# # merges
# b = merge(a, defs, by.x = "variable", by.y = "Variable", all.x = T)
# 
# b = b[,c("variable", "Definition","V1","V2")]
# 
# c = b[!is.na(b$V1),]
# # creates a date variable
# 
# write.csv(c, "clearer_view.csv")
# 
# 
# df$date = dym(paste0(df$day, df$year,df$month))
# unique(df$name)
# 
# random_bank = as.character(df$name[sample(1:19624, 1)])


# Creates Working Dataframe ----

"
15 Geo
11 ID
361 FINANCIALS
5 TIME
"

df = readRDS("../data/fdic/combined_FDIC.RDS")
base = df 
# changes variables to lowercase
names(df) = tolower(names(df))

# converts factors to characters
i <- sapply(df, is.factor)
df[i] <- lapply(df[i], as.character)

# removes any columns with duplicate names
df = df[,!duplicated(names(df))]

# creates dates from days, months, etc.
df$date = mdy(paste(df$month,df$day,df$year))


# loads the chosen ones and reduces the nubmer of variables
the_chosen = read.csv("even_better_chosen_list.csv", stringsAsFactors = F)

# df = df[,the_chosen]
df = df %>%
    select(the_chosen$Variable)


# changes strings to dates
for(i in c("estymd", "repdte", "rundate", "insdate", "effdate")){
df[,i] = mdy(df[,i])
}

# rearranges character variables before numerical ones
df$zip = as.character(df$zip)
isf <- sapply(df, is.Date)
notf <- !isf
df = cbind(df[,isf],df[,notf])
isf <- sapply(df, is.character)
notf <- !isf
df = cbind(df[,isf],df[,notf])


# Removes left-over junk
df = df %>%
    select(-c(day, year, month))



# SAVES FILE WE WILL USE FOR EVERYTHING
saveRDS(df, "../data/fdic/working_df.RDS")


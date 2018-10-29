# data_download ----
# Data references:
# "https://cdr.ffiec.gov/public/" # The data are here
# "https://www5.fdic.gov/sdi/download_large_list_outside.asp" 

library(RCurl)
library(stringr)
library(plyr)
library(dplyr)
library(lubridate)

# Creates the download folders
dir.create("../data", showWarnings = F)
dir.create("../data/fdic", showWarnings = F)
dir.create("../data/fdic/SDI_data", showWarnings = F)
download_folder = "../data/fdic/SDI_data/source_files"
dir.create(download_folder, showWarnings = F)

# Sets up download addresses
root_path = "https://www5.fdic.gov/sdi/Resource/AllReps/All_Reports_"
years = 1992:2018
quarterly_dates = c("0331", "0630", "0930", "1231")

# Makes list of all needed (and some not needed, i.e. has extra junk) addresses 
URLs = NULL
# this "for loop" works by defining a thing called URLs. Then it adds 4 new address to URL using the c( ) command. When it reaches the end, it repeats for a later.
for(year in years){ 
    URLs = c(URLs,paste0(paste0(root_path,paste0(year,quarterly_dates),".zip"
    )))
}


# Creates a list of addresses and names to be downloaded
files_to_be_downloaded = as.data.frame(URLs, stringsAsFactors = F)
files_to_be_downloaded$file_names = str_extract(URLs,"All_.+")

# Compares the list to the "do not download" list and removes existing requests from the download list
do_not_download_list = c(c(list.files(path = download_folder)), # existing
                         "All_Reports_19920331.zip", "All_Reports_19920630.zip", "All_Reports_19920930.zip") # never existed
files_to_be_downloaded = files_to_be_downloaded[!(files_to_be_downloaded$file_names %in% do_not_download_list),] # handles comparison

# Performs data downloads
for(i in 1:nrow(files_to_be_downloaded)){
download.file(url = files_to_be_downloaded$URLs[i], 
              destfile = paste0(download_folder,"/", files_to_be_downloaded$file_names[i]))}

# Performs definitions downloads
download.file(url = "https://www5.fdic.gov/sdi/SDIAllDefinitions_CSV.zip", 
              destfile = paste0(download_folder,"/","definitions.zip"))


# data_exploration ----
# creates dataframe of the file addresses
source_files = as.data.frame(paste0(download_folder, "/", c(list.files(path = download_folder, "^A.+"))))
# creates column of names
source_files$filenames = c(list.files(path = download_folder, "^A.+"))
colnames(source_files) = c("locations", "filenames")
source_files$locations = as.character(source_files$locations)


# names folder for extractions
extracted_files_folder = "../data/fdic/SDI_data/extracted_files"

# unzips definitions
unzip(
    paste0(download_folder, "/", "definitions.zip"),
    list = F,
    exdir = paste0(extracted_files_folder, "/", "definitions")
)


extracted_files_subfolders = paste0(extracted_files_folder,
                                    "/",
                                    list.files(path = extracted_files_folder))
list.files(paste0(extracted_files_folder, "/", "definitions"))

# makes table that will hold all definitions
definitions_table = NULL

for (i in seq_along(list.files(paste0(extracted_files_folder, "/", "definitions")))) {
    a = read.csv(
        paste0(paste0(extracted_files_folder, "/", "definitions"),
               "/",
               list.files(paste0(extracted_files_folder, "/", "definitions"))[i]
        ),
        header = F,
        stringsAsFactors = F
    )[-1, -1]
    
    colnames(a) = a[1, ]
    a = a[-1, ]
    # gsub is a future best friend.
    a$ref_file = gsub(".csv",
                      "",
                      list.files(paste0(extracted_files_folder, "/", "definitions"))[i],
                      fixed = T)
    definitions_table = rbind(definitions_table,
                              a)
}

definitions_table = unique(definitions_table)
definitions_table = definitions_table[order(definitions_table$Variable), ]
write.csv(definitions_table, "definitions.csv")
# definitions_table = read.csv("definitions.csv", row.names = F)

# creates_extraction_folders_and_master_variable_list ----

for (j in 1:nrow(source_files)) {
    ## unzips_files_and_creates_file_contents_summary ----
    # names folder for file extraction
    new_folder = paste0(extracted_files_folder,
                        "/",
                        gsub(".zip", "", source_files$filenames[j], fixed = T))
    
    # creates folder for file extraction
    dir.create(new_folder, showWarnings = F)
    
    # unzips files into folder
    unzip(zipfile = source_files$locations[j],
          exdir = new_folder)
    new_files = list.files(new_folder, ".+csv$")
    
    
    ##### the next several lines simply detect which variables exist across the 61 files. 
    # Begins an empty dataframe
    variable_inclusion_table = data.frame()
    
    # caution, this is slow
    start_time = Sys.time()
    for (i in seq_along(new_files)) {
        temp_df = read.csv(paste0(new_folder, "/", new_files[i]))
        included_or_not = as.numeric(definitions_table$Variable %in% colnames(temp_df))
        variable_inclusion_table = rbind(variable_inclusion_table,
                                         included_or_not)
    }
    print(Sys.time() - start_time)
    
    colnames(variable_inclusion_table) =  definitions_table$Variable
    rownames(variable_inclusion_table) = new_files
    
    write.csv(variable_inclusion_table, "variable_inclusion.csv")
    
    # This finds the frequency of variables across files then creates histogram like data.
    f_list = NULL
    for (i in 1:ncol(variable_inclusion_table)) {
        f_list = c(f_list, sum(variable_inclusion_table[, i]))
    }
    
    f_list = as.data.frame(cbind(definitions_table$Variable, f_list),
                           stringsAsFactors = F)
    colnames(f_list) = c("variable_name", "frequency")
    f_list$frequency = as.numeric(f_list$frequency)
    table(f_list$frequency)
    
    identifiers_count = max(f_list$frequency)
    str(f_list)
    identifiers = dplyr::filter(f_list, frequency == identifiers_count)[, "variable_name"]
    
    start_time = Sys.time()
    temp_combined = read.csv(paste0(new_folder, "/", new_files[1]))
    for (i in 2:length(new_files)) {
        temp_combined = merge(x = read.csv(paste0(new_folder, "/", new_files[i])),
                              y = temp_combined)
    }
    print(Sys.time() - start_time)
    temp_combined = temp_combined[, order(names(temp_combined))]
    
    dir.create("../data/fdic/SDI_data/RDS_format", showWarnings = F)
    saveRDS(temp_combined,
            paste0(
                "../data/fdic/SDI_data/RDS_format/",
                gsub("\\.zip$", "", source_files$filenames[j]),
                ".RDS"
            ))
    unlink(new_folder, recursive = T)
}

# combines all files into one ----

RDS_folder = "../data/fdic/RDS_format/"
SDI_data = list.files(path = RDS_folder)

df = readRDS(paste0(RDS_folder,SDI_data[1]))
df$year = substr(SDI_data[1],13,16)
df$month = substr(SDI_data[1],17,18)
df$day = substr(SDI_data[1],19,20)

combined_df = df
start_time = Sys.time()
# for(i in 2:3){
for(i in 2:length(SDI_data)){
    df = readRDS(paste0(RDS_folder,SDI_data[i]))
    df$year = as.numeric(substr(SDI_data[i],13,16))
    df$month = as.numeric(substr(SDI_data[i],17,18))
    df$day = as.numeric(substr(SDI_data[i],19,20))
    combined_df = plyr::rbind.fill(combined_df,df)
}
# 
f_list = f_list[order(f_list$frequency, decreasing = T),]
new_order = colnames(combined_df)[colnames(combined_df) %in% f_list[,1]]
combined_df = combined_df[,new_order]
# combined_df = readRDS("../data/fdic/combined_FDIC.RDS")

combined_df$date = dym(paste0(combined_df$day, 
                              combined_df$year,
                              combined_df$month))
saveRDS(combined_df, "../data/fdic/combined_FDIC.RDS")
Sys.time()-start_time
# https://gallery.shinyapps.io/CDCPlot/

####---- general set up
library(revtools)
library(dplyr)
library(stringdist)
library(stringr)
library(fulltext)

#define where the output of the script should be saved
folder_script <- "C:/Users/AnneSuffel/OneDrive - London School of Hygiene and Tropical Medicine/LSHTM PhD/Data Analysis/Systematic Review/R scripts/"
#define where all the search outputs are saved
#.ris files in this folder should entail only citation and abstract, nothing else
# the files should in the follwoing order:
# Medline, Embase, Other databases from OvidSP (PsycInfo, EconLit etc)
#PubMed, Cinahl Plus, Other databases from Ebsco, Web of Science databases
#Scopus, ProQuest databases, Cochrane databases, CRD databases, Any other databases, Clinical Trials websites

folder_searches <- "C:/Users/AnneSuffel/OneDrive - London School of Hygiene and Tropical Medicine/LSHTM PhD/Data Analysis/Systematic Review/R scripts/Searches/"
folder_output <-  folder_script # place where to save final library
setwd(folder_script)


#--- different functions necessary for the procedure
###function countring_entries provides all the number of entries per dataframe
counting_entries <- function(){
  
  #counting the numbers of files from different data bases
  setwd(folder_searches)
  files <- list.files()
  n <- length(files)
  names <-  c()
  entries <- c()
  
  for(i in 1:n){
    
    names <- c(names, as.character(files[i]))
  }
  
  for(i in 1:n){
    database <- read_bibliography(files[i])
    entries <- c(entries, nrow(database))
  }
  
  database_entries <- data.frame(rbind(names, entries))
  return(database_entries)
  
}

n <- counting_entries()



####function data_glue returns all the *ris files as one data frame
data_glue <- function(){
  
  #set up for the merging function
  setwd(folder_searches)
  files <- list.files()
  n <- length(files)
  #reference database which column items will be carried through
  reference_database <- read_bibliography(files[1])
  merged_citations <- reference_database
  
  #merging function
  for(i in 2:n){

    colnames_old <-  colnames(reference_database)
    database2 <- read_bibliography(files[i])
    colnames_new <-  colnames(database2)
    
    additional_in_new <- setdiff(colnames_new, colnames_old)
    additional_in_old <- setdiff(colnames_old, colnames_new)
    
    fill <-  match(additional_in_old, colnames_old)
    drop <- match(additional_in_new, colnames_new)
    #dropping columns which are not included in the reference database
    database2 <- database2[,-drop]
    colnames_dropped <- colnames(database2)
    
    #adding NA columns for columns included in reference database but not
    # in the new dataabse
    for(i in 1:length(additional_in_old)){
      new_col <-  rep(NA, length(nrow(database2)))
      if(i==1){
        updated_database <- cbind(database2, new_col)
        colnames_new <- c(colnames_dropped, additional_in_old[i])
      }
      else{
        updated_database <- cbind(updated_database, new_col)
        colnames_new <- c(colnames_new, additional_in_old[i])
      }
      
    }
    
    #filling up the the colnames for the empty, newly added columns
    colnames(updated_database) <- colnames_new
    #merging the old data base and the new data base with exactly the same columns
    merged_citations <- rbind(merged_citations, updated_database)
    merged_citations <- data.frame(merged_citations)
    colnames(merged_citations) <- colnames_old
    
    
  }

  
  return(merged_citations)
}


data <- data_glue()
nrow(data)#11582 entires
colnames(data)


#methods of finding matches
#STEP1: deduplicating by DOI
matches1 <- find_duplicates(data, match_variable = "doi")
data_unique1 <- extract_unique_references(data, matches1)
nrow(data_unique1) #5634


#STEP2: Sorting by Journal and then deduplicating by title
#using fuzzydiz algorithm
data2 <-  data_unique1%>%
  arrange(journal)
matches2<- find_duplicates(data2,
                           match_variable = "title",
                           method = "lv",
                           threshold = 0.1,
                           to_lower = TRUE,
                           remove_punctuation = TRUE)

data_unique2<- extract_unique_references(data2, matches2)
nrow(data_unique2) #4745



#STEP3: Sorting by page number and the deduplicating by title
#using the stringdist algorithm
data3 <-  data_unique2%>%
  arrange(pages)
matches3<- find_duplicates(data3,
                           match_variable = "title",
                           method = "lv",
                           threshold = 5,
                           to_lower = TRUE,
                           remove_punctuation = TRUE)

data_unique3<- extract_unique_references(data3, matches3)
nrow(data_unique3) #4069




#exporting the final deduplicated library
setwd(folder_output)
write_bibliography(data_unique3, "compiled_bibliography.ris", format = "ris")



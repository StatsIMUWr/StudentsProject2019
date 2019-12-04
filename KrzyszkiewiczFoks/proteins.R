library(dplyr)
library(tidyr)
library(splitstackshape)
library(lubridate)

######### input data:
### file search:
path <- "C:/Users/Karolina/Desktop/UWr_R"
file <- "2.csv"
sep <- ","
### filter data:
col_1 <- "sequence"
col_2 <- "protein.id"
col_3 <- "percolator.q.value"
q_value <- 0.01
### separation of data in the column:
col <- col_2
sep_2 <- ","

### data loading function:
load_file <- function(path, file, sep){
  lst_file <- list.files(path, pattern = file)
  lst_table <- lapply(lst_file, read.table, header = TRUE, sep = sep, stringsAsFactors = FALSE)
  do.call(rbind, lst_table)
}

### filter function:
filter_table <- function(table, col_1, col_2, col_3, q_value){
  table <- filter(table, table[[col_3]] < q_value)
  table <- select(table, col_1, col_2)
}

### separation of data in the column:
separate_table <- function(table, col, sep){
  table_separate <- cSplit(table, col, sep)
}

### one table from two columns; reject "<NA>":
connect_table <- function(table_separate){
  table_protein <- table_separate[,1:2]
  for (i in seq(from = 3, to = dim(table_separate)[2])){
    table_protein <- rbind(table_protein,
                           filter(select(table_separate, 1, i),
                                  table_separate[[colnames(table_separate)[i]]] != "<NA>"),
                           use.names = FALSE)
  }
  table_protein
}

### delete duplicate rows
unique_table <- function(table){
  table <- unique(table)
}

### columns names change
change_name_cols <- function(table, name_cols){
  colnames(table) <- name_cols
  table
}

### sort table
sort_table <- function(table, name_col){
  table_sort <- table[order(table[[name_col]])]
}

### include table
create_table_0_1 <- function(table){
  table <- as.data.frame.matrix(table(table))
}

### count values peptides/proteins:
count_table <- function(table, id){ 
  if (id == 2){
    table_qty <- data.frame(colnames(table), as.vector(apply(table, 2, sum)))
    
  } else {
    table_qty <- data.frame(rownames(table), as.vector(apply(table, 1, sum)))
  }
  table_qty
}

### import function:
import_comet_percolator <- function(path, file, sep, sep_2, col_1, col_2, col_3, q_value, name_cols){
  table <- load_file(path, file, sep)
  table <- filter_table(table, col_1, col_2, col_3, q_value)
  table <- separate_table(table, col_2, sep_2)
  table <- connect_table(table)
  table <- unique_table(table)
  table <- change_name_cols(table, name_cols)
  table
}

table_proteins_peptides <- import_comet_percolator(path, file, sep, sep_2, col_1, col_2, col_3, q_value, c("peptides", "proteins"))

### manipulate function: 
manipulation_table <- function(table, name_col, id, name_cols){
  table <- sort_table(table, name_col)
  table <- create_table_0_1(table)
  table <- count_table(table, id)
  table <- change_name_cols(table, name_cols)
  table
}

table_qty_peptides <- manipulation_table(table_proteins_peptides, "proteins", 2, c("proteins", "qty_peptides") )
table_qty_proteins <- manipulation_table(table_proteins_peptides, "proteins", 1, c("peptides", "qty_proteins") )

### data summary:
head(table_qty_peptides)
head(table_qty_proteins)
head(table_proteins_peptides)





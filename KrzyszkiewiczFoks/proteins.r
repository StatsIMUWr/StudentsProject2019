library(dplyr)
library(tidyr)
library(splitstackshape)
library(lubridate)

### dane wejściowe (???):
### wyszukiwanie plików:
path <- "."
file <- "s_1.csv"
sep <- ","
header <- TRUE
### filtrowanie danych z pliku:
col_1 <- "sequence"
col_2 <- "protein.id"
col_3 <- "percolator.q.value"
q_value <- 0.01
### separowanie danych w kolumnie:
col <- col_2
sep_2 <- ","

###==(sposób F.1)=== funckja wybierająca  plik/pliki i przypisująca dane 
load_file <- function(path, file, sep, header){
  lst_file <- list.files(path, pattern = file)
  lst_table <- lapply(lst_file, read.table, header = header, sep = sep, stringsAsFactors = FALSE)
  do.call(rbind, lst_table)
}

# ##==(sposób F.2)=== funckja pozwalająca użytkownikowi wybierać jeden plik z okna i przypisująca dane
# load_file_2 <- function(sep, header){
#   file_ <- file.choose()
#   table_all <- read.table(file_, header = header, sep = sep)
# }

table_all <- load_file(path, file, sep, header)

### funckja filtruje po col_3 dla wartości mniejszej od q_value i wybierania kolumny col_1 i col_2
filter_table <- function(table_all, col_1, col_2, col_3, q_value){
  table_filtr <- filter(table_all, table_all[[col_3]] < q_value)
  table_filtr <- select(table_filtr, col_1, col_2)
}

table_filtr <- filter_table(table_all, col_1, col_2, col_3, q_value)

### funckja rozseparowuje dane w kolumnie
separate_table <- function(table, col, sep){
  table_separate <- cSplit(table, col, sep)
}

table_separate <- separate_table(table_filtr, col, sep_2)

### funckja łącząca powstałe nowe kolumny w jedną tabelkę o dwóch kolumnach
### oraz odrzuca wiersze z wartością <NA>
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

table_protein <- connect_table(table_separate)

### usuwanie powtórzonych wierszy
unique_table <- function(table_protein){
  table_protein <- unique(table_protein)
}

table_protein <- unique_table(table_protein)

### zmiana nazw kolumn
change_name_cols <- function(table, name_cols){
  colnames(table) <- name_cols
  table
}

table_protein <- change_name_cols(table_protein, c("peptides", "proteins"))

### tabela peptyd-białko posortowana po danej nazwie kolumny
sort_table <- function(table_protein, name_col){
  table_sort <- arrange(table_protein, table_protein[[name_col]])
}

table_peptide_protein <- sort_table(table_protein, "proteins")

#========================(start: sposób T.1)================================================================
### tworzenie data frame ilości występowania danego peptydu w danym białku
### zamiana klasy kolumn z factor na wektor
table_0_1 <- function(table_protein){
  table_protein[[colnames(table_protein)[1]]] <- as.vector(table_protein[[colnames(table_protein)[1]]])
  table_protein[[colnames(table_protein)[2]]] <- as.vector(table_protein[[colnames(table_protein)[2]]])
  table_protein_0_1 <- as.data.frame.matrix(table(table_protein))
}

table_protein_0_1 <- table_0_1(table_protein)

# ###======(sposób T.1.1 z macierzą 0 i 1):
# ### zamiana data frame na macierz
# matrix_0_1 <- function(table_protein_0_1){
#   matrix_protein_0_1 <- as.matrix(table_protein_0_1)
# }
# 
# matrix_protein_0_1 <- matrix_0_1(table_protein_0_1)
# 
# ### funckja obliczająca iloczyn macierzy
# matrix_diag <- function(matrix_protein_0_1){
#   t(matrix_protein_0_1) %*% matrix_protein_0_1
# }
# 
# matrix_qty_peptide <- matrix_diag(matrix_protein_0_1)
# 
# ### zamiana macierzy peptydów na tabelę ilości peptydów
# qty_table <- function(matrix_qty){
#   table_qty <- data.frame(rownames(matrix_qty), diag(matrix_qty))
#   rownames(table_qty) <- NULL
#   table_qty
# }
# 
# table_qty_peptide <- qty_table(matrix_qty_peptide)
# 
# ### zmiana nazw kolumn
# table_qty_peptide <- change_name_cols(table_qty_peptide, c("proteins", "qty_peptide"))
# 
# ### iloczyn macierzy dla ilości białek
# matrix_qty_protein <- matrix_diag(t(matrix_protein_0_1))
# 
# ### tabela ilości białek
# table_qty_protein <- qty_table(matrix_qty_protein)
# 
# ### zmiana nazw kolumn
# table_qty_protein <- change_name_cols(table_qty_protein, c("peptides", "qty_protein"))

###========(sposób T.1.2 bez macierzy 0 i 1):
### funckja tworząca tabelę ilości peptydów dla danego białka
qty_peptide <- function(table_protein_0_1){
  table_qty_peptide <- data.frame(colnames(table_protein_0_1), as.vector(apply(table_protein_0_1, 2, sum)))
}

table_qty_peptide <- qty_peptide(table_protein_0_1)

### zmiana nazw kolum
table_qty_peptide <- change_name_cols(table_qty_peptide, c("proteins", "qty_peptides"))

### funckja tworząca tabelę ilości białek dla danego peptydu
qty_protein <- function(table_protein_0_1){
  table_qty_peptide <- data.frame(rownames(table_protein_0_1), as.vector(apply(table_protein_0_1, 1, sum)))
}

table_qty_protein <- qty_protein(table_protein_0_1)

### zmiana nazw kolumn
table_qty_protein <- change_name_cols(table_qty_protein, c("peptides", "qty_proteins"))

### podsumowanie (tabele wynikowe):
head(table_qty_peptide)
head(table_qty_protein)
head(table_peptide_protein)

### funckja znajdowania wspólnych peptydów / filtrowanie po białkach ???

    ### niebardzo rozumiemy jak to ma działać

# ###=========================(koniec: sposób T.1)=======================================================
# 
# ###=========================(start: sposób T.2)========================================================
# ### sztuczne stworzenie kolumny zawierania peptydów w białkach
# include_col <- function(table_protein){
#   table_protein <- mutate(table_protein, include = rep(1, dim(table_protein)[1]))
# }
# 
# table_protein_include <- include_col(table_protein)
# 
# ### tworzenie data.frame zawierania, gdzie nazwami kolumn są nazwy białek,
# ### posortowane
# data_0_1 <-function(table_protein_include){
#   data_protein_0_1 <- as.data.frame(spread(table_protein_include, colnames(table_protein)[2], include, fill = 0))
# }
# 
# data_protein_0_1 <- data_0_1(table_protein_include)
# 
# ### usuwamy pierwszą kolumnę oraz usuwamy nazwy kolumn,
# ### aby otrzymać macierz 0 i 1 jako liczby
# matrix_0_1 <- function(data_protein_0_1){
#   matrix_protein_0_1 <- data_protein_0_1[,2:dim(data_protein_0_1)[2]]
#   colnames(matrix_protein_0_1) <- NULL
#   matrix_protein_0_1 <- as.matrix(matrix_protein_0_1)
# }
# 
# matrix_protein_0_1 <- matrix_0_1(data_protein_0_1)
# 
# 
# ### funcja iloczyn macierzy i transpozycji macierzy
# matrix_diag <- function(matrix_protein_0_1){
#   t(matrix_protein_0_1) %*% matrix_protein_0_1
# }
# 
# matrix_qty_peptide_2 <- matrix_diag(matrix_protein_0_1)
# 
# ### zamiana macierzy na tabelę ilości peptydow/ilości białek
# table_qty <- function(matrix, table_peptide_protein, name_col){
#   table_peptide_protein <- sort_table(table_peptide_protein, name_col)
#   col_sort <- table_peptide_protein[[name_col]]
#   table_qty <- data.frame(unique(col_sort), diag(matrix))
# }
# 
# table_qty_peptide_2 <- table_qty(matrix_qty_peptide_2, table_peptide_protein, "proteins")
# 
# ### zmiana nazwy kolumn
# table_qty_peptide_2 <- change_name_cols(table_qty_peptide_2, c("proteins", "qty_peptides"))
# 
# ### macierz ilości białek, korzystamy z funckji matrix_diag:
# matrix_qty_protein_2 <- matrix_diag(t(matrix_protein_0_1))
# 
# ### zamiana macierzy ilości białek na tabelę ilości białek
# table_qty_protein_2 <- table_qty(matrix_qty_protein_2, table_peptide_protein, "peptides")
# 
# ### zmiana nazwy kolumn
# table_qty_protein_2 <- change_name_cols(table_qty_protein_2, c("peptides", "qty_proteins"))
# 
# ### podsumowanie T.2 (tabele wynikowe):
# head(table_peptide_protein)
# head(table_qty_peptide_2)
# head(table_qty_protein_2)
# 
# ###============(koniec: sposób T.2)=================================================================================



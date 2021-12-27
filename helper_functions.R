library(data.table)

object.size.better <- function (x) {
  
  if ( is.character(x)==T | is.numeric(x)==T | is.integer(x)==T ) {
    x <- as.numeric(x)
  } else {
    x <- as.integer(object.size(x))
  }
  
  options(scipen = 999)
  
  if (nchar(x) < 4) { 
    so <- "b"
  } else if (nchar(x) < 7) {
    so <- "Kb"
    x <- round(x/1000, 1)
  } else if (nchar(x) < 10) {
    so <- "Mb"
    x <- round(x/1000000,1)
  } else if (nchar(x) < 13) {
    so <- "Gb"
    x <- round(x/1000000000,1)
  } else {
    so <- "Tb"
    x <- round(x/1000000000000,1)
  }

  print (paste (x, so))
  options(scipen = 0)
  
}
# object.size(dt)
# 13500000
# object.size.better(dt)
# 13.5 Mb

ifstructure <- function() {
  cat("if (x==this) { \n    #this \n} else if (x==that) { \n    #that \n} else { \n    #well \n}")
}
# ifstructure()

key.for.overlaps <- function (dt) {
  setkey (dt, chr, strand, start, end)
}
# key.for.overlaps(dt)
# foverlaps(dt, dt)

make.args <- function (x) {
  return ( strsplit (x, split = " " ) [[1]] )
}
# args <- make.args ("Rscript /dir/to/script/test.R -in seqs.fa -out seqs_qualityreport.pdf -threshold 10")

startupvol.switch <- function (x) {
  assign ( deparse ( substitute (x) ), gsub ( "groups", "Volumes", x ), env = .GlobalEnv)
}

# startupvol.switch(link)

find.nonlist.in.list <- function (x) {
  if ( class (x) == "list" ) {
    lapply ( x, find.nonlist.in.list )
  } else if ( class (x) != "list" ) {
    TRUE
  } else {
    NULL
  }
}

list.nonlist.names <- function (x) {
  return ( strsplit ( names ( unlist ( lapply ( x, find.nonlist.in.list ))), split = ".", fixed = TRUE ))
}

# list
#test <- list ( A = list ( AA = c(1,2),
#                          AB = c(2,3)),
#               B = list ( BA = c(3,4),
#                          BB = c(4,5)))

#my.vectors <- list.nonlist.names (test)
#sapply ( seq_along ( my.vectors ), function (i) { paste ( c(my.vectors [[i]] [1], my.vectors [[i]] [2], test [[ my.vectors [[i]] ]]), collapse = "_" )})
#[1] "A_AA_1_2" "A_AB_2_3" "B_BA_3_4" "B_BB_4_5"
#sapply ( seq_along ( my.vectors ), function (i) { sum ( test [[ my.vectors [[i]] ]])})
#[1] 3 5 7 9

number.with.separator <- function (x) {
  
  if ( is.character(x)==T | is.numeric(x)==T | is.integer(x)==T ) {
    x <- as.numeric(x)
  } else {
    x <- as.integer(object.size(x))
  }
  
  options(scipen = 999)
  
  x.decimal <- nchar(strsplit(as.character(x), split = "\\.")[[1]][2])
  x.decimal <- ifelse(is.na(x.decimal), 0, x.decimal)
  
  x <- format( x, big.mark="'", nsmall = x.decimal )
  
  print (x)
  options(scipen = 0)
  
}

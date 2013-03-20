data(diamonds, package = "ggplot2")

save.diamonds <- function(name, rows) {
  data(diamonds, package = "ggplot2")
  write.csv2(diamonds[rows,], file = paste("Programming Course/Data/Data collection/part", 
                                           name, ".csv", sep = ""))
}

make.char <- function(num) {
  if (grepl("[0-9][0-9][0-9]", num)) return(as.character(num))
  if(grepl("[0-9][0-9]", num)) return(paste("0", num, sep = ""))
  if (grepl("[0-9]", num)) return(paste("00", num, sep = ""))
}

row.seq <- seq(1, nrow(diamonds), 200)

for(i in length(row.seq):1) {
  end <- ifelse(is.na(row.seq[i+1]), nrow(diamonds), row.seq[i+1])
  start <- row.seq[i] + 1 
  save.diamonds(make.char(i), start:end)
}
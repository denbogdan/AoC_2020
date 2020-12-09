#collapse master file into 1 row per group (using awk on the cl)
# awk -v RS= '$1=$1' advent_day6.txt > day6_collapsed.txt
 
#read data into R
data_list <- read.delim("/Users/dbogdan/analysis/WORKDIR/Advent_2020/day6_collapsed.txt", sep="\n", header=F)

##Question 1 -----------------------------------------------------------------------
all_counts <- c()
for (i in 1:nrow(data_list))
{
  count <- sum(!!str_count(data_list$V1[i], letters))
  all_counts <- c(all_counts, count)
}
##Answer 1 -------------------------------------------------------------------------
sum(all_counts)

##Question 2 -----------------------------------------------------------------------
all_counts <- c()
for (i in 1:nrow(data_list))
{
  row <- str_split(data_list$V1[i], " ")[[1]]
  count <- length(Reduce(intersect, strsplit(row,"")))
  all_counts <- c(all_counts, count)
}
##Answer 2 -------------------------------------------------------------------------
sum(all_counts)
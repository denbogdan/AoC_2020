library(tidyverse)
library(stringr)
#parse data
seats <- read.delim("advent_day5.txt", header=F, sep="\n")

##Question 1 -----------------------------------------------------------------------

#declare number of rows and columns on the plane
rs=128
cs=8

all_ids <- c()
row_ids <- c()
col_ids <- c()
for (seat in  1:nrow(seats))
{
  s=seats$V1[seat]
  all_ids <- c(all_ids, seat_id(s)[3])
  row_ids <- c(row_ids, seat_id(s)[1])
  col_ids <- c(col_ids, seat_id(s)[2])
}


#write function that calculates seat ids
seat_id <- function(s)
{
  #split string to be able to access individual positions
  s=str_split(s, "")[[1]]
  #initialise up and down boundaries of interval to be halfed
  dn=0
  up=rs
  #initialise the row position to be updated
  fin_r=rs/2
  i=1
  #deal with the row
  while (length(s)>3)
  {
    if (s[i]=="F")
    {
      up <- fin_r
      dn <- dn
      fin_r <- dn + floor((up-dn)/2)
    }
    else 
    {
      up <- up
      dn <- fin_r
      fin_r <- dn + floor((up-dn)/2) 
    }
    #cut the letter from vect
    s=s[2:length(s)]
  }
  
  #deal with the column -same procedure
  dn=0
  up=cs
  fin_c=cs/2
  for (i in 1:3)
  {
    if (s[i]=="L")
    {
      up <- fin_c
      dn <- dn
      fin_c <- dn + floor((up-dn)/2)
    } 
    else 
    {
      up <- up
      dn <- fin_c
      fin_c <- dn + floor((up-dn)/2) 
    }
  }
  #calculate seat id
  id <- fin_r*8 + fin_c
  #return vector with ids found
  return(c(fin_r, fin_c, id))
}

##Answer 2 -------------------------------------------------------------------------
max(all_ids)

##Question 2 -----------------------------------------------------------------------

#put rows and columns in data frame
x = data.frame(row=row_ids, col=col_ids)
x = x[order(x$row),]

#collapse into counts for each row
x1 <- x %>% dplyr::group_by(row) %>% dplyr::summarise(seats = paste(row, collapse=", "), times = length(row)) 
x1 = x1[order(x1$row),]

##Answer 2 -------------------------------------------------------------------------
which(x1$times<8)
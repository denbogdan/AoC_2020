#parse data file and tidy
code <- read.delim("advent_day9.txt", header = FALSE, sep = "\n")
code <- as.numeric(code$V1)

##Question 1 -----------------------------------------------------------------------
i=26
pass=TRUE
while (pass==TRUE)
{
  #construct previous range of length 25
  previous <- code[(i-25):(i-1)]
  for (j in 1:length(previous))
  {
    value = abs(previous[j]-code[i])
    if(length(which(previous==value))>0)
    {
      pass=TRUE
      break;
    }
    else
    {
      pass=FALSE
    }
  }
  i=i+1
}

##Answer 1 -------------------------------------------------------------------------
code[i-1]

##Question 2 -----------------------------------------------------------------------
target=code[i-1]
pass=TRUE
i=1
while (pass==TRUE)
{
  sum=0
  #build sum until it exceeds the target
  #when the sum is equal to the target, break both for and while loops
  for (j in i:length(code))
  {
    sum=sum+code[j]
    if (sum==target)
    {
      pass=FALSE
      break;
    }
    else
    {
      if (sum > target)
      {
        pass=TRUE
        break;
      }
    }
  }
  i=i+1
}

#check the sum is right
sum(code[(i-1):j])==target

##Answer 1 -------------------------------------------------------------------------
min(code[(i-1):j])+max(code[(i-2):j])
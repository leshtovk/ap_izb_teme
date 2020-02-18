 
# store the data from the text file into a 'data frame'
Auto1 = read.table("Auto.data", header=TRUE, na.strings="?")
  # `header=TRUE` -- the first line of the file contains variable names
  # `na.strings="?"` -- every time "?" is encountered in the data, 
  #  it should be treated as NA

# view the data in a spreadsheet window
fix(Auto1) 


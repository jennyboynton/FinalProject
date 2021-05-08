# DSP 539: Final Project
## Goal: Observe relationships between meat and life expectancy

### Input: 
- A dataset imported as full indicator data from the Organisation for Economic Co-operation and Development (OECD) web page
- Dataset including life expectancy, GDP per capita, and population from the gapminder package in Rstudio, which can also be found in its raw form on the gapminder website (http://www.gapminder.org/data/)

### Output:
- GGplot graphs showing different relationships
- Statistics based off linear models of the data

### Prerequisites:
- R Packages:
    - "gapminder"
    - "skimr"

- R Libraries:
    
    - tidyverse
    - lubridate
    - "gapminder"
    - moderndive
    - dplyr
    - skimr  

- Software: 
  
    -  Rstudio

### How to Run the Program
- Go into Rstudio Editor and input code


### Code Description:
File: code_for_final.R
- Methods:

a. count_kmers_observed()

    - Generate a count of the number of observed kmers for a string

b. count_kmers_possible()

    - Generate a count of the number of possible kmers for a string

c. def create_panda()

    - Generate a pandas data frame for each observed and possible kmers for a string

d. calculate_LC()

    - Generate the linguistic complexity for a string

e. letter_check()

    - Checks to make sure sequence only contains A,C,T or G

f. main()

    - Creates new output files for both linguistic complexity and pandas data frame


### Output:

a. linguistic complexity:

    - Saved in a new file named after the original file and numbered(originalfile_#.txt)
    - Format:
    ATTTGGATT
    0.875

b. Pandas Dataframe:

    - Saved in a second separate new file named "string_examples_#.txt" with the number matching the number of the first file.
    - Format:
    ,k,observed kmers,possible kmers
    0,1.0,3.0,4.0
    1,2.0,5.0,8.0
    2,3.0,6.0,7.0
    3,4.0,6.0,6.0
    4,5.0,5.0,5.0
    5,6.0,4.0,4.0
    6,7.0,3.0,3.0
    7,8.0,2.0,2.0
    8,9.0,1.0,1.0
    Total,,35.0,40.0

### Code Description for Test Script:
File: test_FinalExam.py

Methods:

a. test_count_kmers_observed()

    - Confirm count_kmers_observed() function generates proper output

b. test_count_kmers_possible()

    - Confirm count_kmers_possible() function generates proper output
  
c. test_create_panda()

    - Confirm create_panda() function generates proper output
    
d. test_calculate_LC()

    - Confirm calculate_LC() function generates proper output

e. test_letter_check()

    - Confirm letter_check() function generates proper output
    
    

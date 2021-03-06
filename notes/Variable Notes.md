# Data Notes

## Ipums Data - Already Calculated

Insert Link to source with explanation of exactly which variables we pulled. Currently organized by what we have already summarized rather than a logical order.


### Race (`race`)

Original coding as follows:
* 1 = white
* 2 = black
* 3 = american indian or alaska native
* 4 = chinese
* 5 = japanese
* 6 = other asian or pacific islander
* 7 = other race
* 8 = two major races
* 9 = three or more major races

Summarized as follows:
Aggregate groups to white (1), black (2), american indian (3), asian (4,5,6), and other (7,8,9). Calculate population proportion.

### Marital Status (`marst`)
Ranges as follows:
* 1 = Married, Spouse Present
* 2 = Married, Spouse Absent
* 3 = Separated
* 4 = Divorced
* 5 = Widowed
* 6 = Never Married, Single

Summarized as follows:
Aggregated into married (1,2) and not married (3,4,5,6) categories. Calculate population proportion

### Number of Children (`nchild`)
Originally coded as follows:
* Range from 0 to 9+
* Top coded at 9

Summarized as follows:
Calculated mean (median had zero variation)

### Number of Children under 5 (`nchlt5`)
Originally coded as follows:
* Range from 0 to 9+
* Top coded at 9

Summarized as follows:
Calculated mean (median had zero variation)

### Usual Hours Worked (`uhrswork`)
Original coding as follows:
* 00 = N/A
* Range of hours
* 99 = 99 hours top code

Summarized as follows:
Calculated mean (median had zero variation)

### Education (`educ`)
Original coding as follows:
* 00 = NA or No Schooling
* 01 = Nursery to grade 4
* 02 = Grades 5-8
* 03 = Grade 9
* 04 = Grade 10
* 05 = Grade 11
* 06 = Grade 12
* 07 = 1 year college
* 08 = 2 years college
* 09 = 3 years college
* 10 = 4 years college
* 11 = 5+ years college

Summarized as follows:
* less than 6 ~ less than completing high school
* 6 ~ high school
* 7, 8, 9 ~ some college
* 10 ~ college complete
* Over 10 ~ more than college

### Employment Status (`empstat`)
Original coding as follows:
* 0 = N/A
* 1 = Employed
* 2 = Unemployed
* 3 = Not in Labor Force

Summarized as follows:
We calculate and employment proportion rate as Employed / (Employed + Unemployed + Not in Labor Force). Note that this is not a employment rate or unemployment rate, but is a metric summarizing the employment status of a state.

### (`incwage`)
https://usa.ipums.org/usa-action/variables/INCWAGE#codes_section

Summarized as follows:
We calculate the median of this for each state for each year.

## Ipums Data - To Calculate

### Hispanic (`hispan`)
Original coding as follows:
* 0 = not hispanic
* 1 = mexican
* 2 = puerto rican
* 3 = cuban
* 4 = other
* 9 = not reported

Summarized as follows:
Not Currently Done



## Other Data Source


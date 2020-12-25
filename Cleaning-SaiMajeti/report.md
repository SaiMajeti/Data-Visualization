## Sai Hymavathi Majeti
### CS 625 HW2

report.md
================
Sai Hymavathi Majeti

CS 625, Fall 2019
-----

#### Steps I took to clean the data (Pet - Names dataset):

I believe that the intention behind the collection of any kind of data
is to obtain some values or gain some insights from it. Hence, having
questions in mind that we would like to get answers for, would help us
in directing the steps we take to clean the given data.

Here, I used the questions that need to be answered in `report.md` to
direct this data cleaning task.

##### Correcting the values in each column:

  - The dataset contains 1783 rows in total and, 83 choices in `Kinds of
    pets` column to start with.

  - Trim white space before and after the values in all the columns
    using `Trasnform --> Common transformations --> trim leading and
    trailing white space`.

  - Select the `text facet` and by using the `cluster` option,
    re-cluster all the similar/duplicate/mispelled choices of each
    column.
    
      - While re-clustering using the `cluster` option, there are
        multiple curistic options in `method` and `keying function` to
        see which groups can be again grouped together into a common one
        and `re-cluster`.
    
      - Here, we need to be careful to not select the groups that
        doesn’t actually go together even the `cluster-curistics`
        suggect so.

  - Even after re-reclustering the similar groups using all the options
    availble in `cluster`, we will still have some duplicate rows or
    similar group choices which can not be identified by the clustering
    option.

  - Scrolling through the choices, we can find some that needs to be
    renamed, which are very simple or have very few rows. These can be
    changed manually using the `edit` option beside the choice name in
    the text facet.

  - In some cells of most of the columns, we will see some unecessary
    characters before and after the actual values. These can be
    identified in the text facets, select and remove using GREL
    expression `value.replace("","")`. An example of one such operation
    I performed is –\> Find “(” using text filter and Transform with
    `value.replace("(", "")`

##### Dealing with blanks:

  - There are some rows which are completely blank. Selecting the
    `blank(empty or null) facet` from the dropdown menu on the `All`
    tab, `delete all the matching rows`.

  - After this, there are still some blank cells in many columns. The
    `What Kind of pet` column can be edited basing upon the `Pet's
    breed` column by selecting text facet on both the columns
    simultaneously –\> click on the `choices` and mass edit the similar
    columns usings `Transform` and entering the value *(inside quotes if
    it is a string)* in the GREL window. Do the similar operation with
    all the other rows.

  - Now, this will mostly get rid of all the blank cells in the dataset.

  - There are some rows where only age is present. For these kinds of
    rows, I have given a value “**Unknown**” for *kind of pet*, *Pet’s
    breed* and *Pet’s names* coloumns so that the values in the *age*
    column can be used for any future statistical requirements.
    
      - While doing this, if I was unsure of any breed type that was
        mentioned in the dataset, I have googled about it and determined
        the kind of animal - some turned out to be horses\!

#### Further preparation of the data to obtain answers for our questions:

  - I changed the names of the rows to " Irrelevant" where no single
    column contained any information about pets.

  - After correcting the values in Pet’s kind using various features on
    OpenRefine, the choices narrowed down from **83** to **31**
    including *Unknown* and *Irreleveant*

##### Splitting Multi-valued cells:

  - While examining the text-facet-choices on multiple columns
    simultaneously, I have noticed that there are a couple of rows which
    contained the details of more than one pet.
    (i.e. cats/dog/dog,dog,dog,cat etc.)

**Following are the steps I have taken to split them:**

  - To include these rows, using text filter on the columns I have
    seearched with common symbols or words that can possibly specify
    multiple pets and selected them.  

  - Change the view from ***rows to records*** for best results while
    splitting (as we are going to split multiple columns - this prevents
    from forming unnecessary rows)

  - Move the single valued cells to the beginning

  - I standardized the separator in all the columns to `" , "` using
    value.replace() :
    
    `value.split(" and ").join(", ").replace("a ", "")`
    
    `value.replace("and",",").replace("Unknown", "Unknown
    ,").replace("unknown","unknown, ")`,
    
    `value.split("yrs ").join(" years, "). replace("yrs", "years")`, and
    so on for the rest of the columns.

  - Start splitting multi-valued cells from right side using `split
    multi-valued cells` option in the common column transformations. On
    the window, `select the separator as ','`.

  - Once all the columns are split, Fill down or edit the values of the
    single-valued columns.

  - Re-order the columns as they were before

(Because of this operation, the rows in my final csv document have
increased, but, to not effect the statistics of other columns they were
changed to blank/unknown. This way, the everyday names are kept intact
and used in obtaining most possible accurate answers for the questions
in the report)

**Correcting the Pet’s breed column:**

  - There are multiple similar opitions in the Pet’s breed column as
    well, even after using Text facet - clustering option.

  - Following are the steps I took to group the choices in this column:
    
    > ***Using block-quotes to represent the decisions I have made that
    > might had major effect on the final answer for the questions***
    
    >   - Cells which has the word ‘mix/and/&/x etc.,’ –\> Transformed to a group called “Some Hybrid”.
    
    >   - Cells with the values as Unsure/don’t know/rescue etc., were added to the group “**Unknown**”
    
    >   - Cells that contain multiple breed names with slashes but only
    >     single value in the rest of the columns were selected using
    >     the `text facet` and `text filter`. In these rows only the
    >     first breed name before the slash was kept using `Transform`
    >     and enetering the GREL expression `value.split("/"))[0]` -
    >     this `splits` the value using the separator `/` and only keeps
    >     the first name (as it can be the main relative breed of that
    >     pet).
    
      - There are cells which contain age instead of breed; these values
        can be shifted into the age column using either `Text facet -->
        Select choices that contain numbers` or `Numeric facet -->
        Select cells that have numeric value (from the facet displayed
        on the left side)` and using the following GREL expression in
        `Transform` on *Pet’s age column*: `cells["Pet's breed (if
        applicable)"].value`
    
      - As a result of performing the above steps, all the columns in
        the pet’s breed have a value - either the name of the breed or
        unknown.

**Correcting the Pet’s age column to obtain the age range of the pets:**

  - Convert the whole column `"To Number"`.

  - Using text facet, I have examined what types of values are there in
    this column.

  - There are `choices with "," instead of "."` ; choices with `"1/2"
    instead of .5` etc. I have corrected these values using
    `text-filter` and correcting the identified mistakes in the data
    entry.

  - Select cells that have values containing `3 "ish"` kind of words and
    mass transform them to numbers by replacing these words with blank
    using GRel expression:

`value.replace("ish","").replace("()","").replace("(rescue)", "")`.

Similarly `'yrs' with 'years'` , `'mos' with 'months'`, `'YO'with
years`, etc.

  - For values like `~3` - *I chose to keep only the number* and for
    values like `3 or 4` - *I kept the first value*.
    
      - There are GREL expressions to just scrape the numbers but, with
        the variety of values that needs to be corrected in this
        dataset, I considered using text filter and replacing them.

**Converting all the values to years**: To obtain the age range in a
single unit, I have converted the all the values in the the Pet’s age
column to years using the following GREL expressions:

  - **Weeks to months:** `toNumber(value.replace(" weeks", ""))/4.0 +"
    months"`

  - **Months to years:** `toNumber(value/12.00).splitByLengths(3)[0]`
    (3)\[0\]- to only keep 1 or two decimal points after dividing by 12.

  - There are couple of values which have `dates like '04/04/2002'` etc.
    To convert these into age, I have assumed the given year as birth
    year and subtracted it from 2019 to calculate that pet’s current
    age. I used the GREL expression: `2019 -
    toDate(value).datePart("year")` - \***datePart** keeps only the unit
    from the date we choose.

  - I searched for words like `"Dead"` etc.and added all of them to the
    category “Deceased”.

  - *Now all the cells in this column either have a year or Deceased or
    Unknown value*

***Finding the age range of a kind of pet:***

  - Select the required choice from the Text Facet of that particular
    column.

  - The numeric facet on the left side shows the ***range of all the
    choices in light blue*** and the ***highlighted choice/category in
    dark blue***

  - Using the ***slider*** on the numeric facet, I identified the max.
    and min. ages of the pets. It can also be used to determine avg.
    value of the pet.

( For this I have sepnt a whole day trying to use the *refine.stats
extension* for column statistics option - but, the OpenRefine app
wouldn’t run for me for some reason after installing it using the
given instructions on readme page of extension folder. I will hopefully
figure it out soon )

**Calculating the values for ‘Fish’:**

  - There are only values like *“Bettafish/Goldfsh/fish”* in the " What
    kind of Pet " column.

  - I transformed all the values to “Fish” in the this column and
    transformed the cell values in the “Pet’s breed” column accordingly.

  - After selecting the `text facet`: beside each choice, the number of
    values in that choice is shown - using this, the number of Goldfish
    and Bettafish can be determined.

  - And, `'sorting by count'`; the type of fish which is high in number
    can be determined.

> ***As all the above operations were driven based on the questions
> asked, the follwing are the answers that I have obtained:***

### Q & A:

-----
**1. How many types (kinds) of pets are there?**

31. (Unknown included; Selecting txt facet, shows the number of choices
    in the kinds of pets )

**2. How many dogs?**  

1151

**3. How many breeds of dogs?**

175. (“Unknown” included)

**4. What’s the most popular dog breed?**

***Some Hybrid*** group which I have created for all the mixed breed
dogs, most of which involved ***Golden Retreiver***.

**5. What’s the age range of the dogs?**

Range - 21; min. - 0.12 years(1.4 months/6weeks), max. - 22 years.

\*Using the slider on the numeric facet, the min and max values can be
determined.

**6. What’s the age range of the guinea pigs?**

Range - 4; min.- 1, max. - 5.

\*Using the slider on the numeric facet, the min and max values can be
determined

**7. What is the oldest pet?**

Cat (24 years) (assuming it is deceased and has not been mentioned; as
it exceeded the avg. life expectancy of a cat)

**8. Which are more popular, betta fish or goldfish? How many of each?**

  - *More popular fish* - Bettafish

  - Count: Bettafish - 14, Goldfish - 6

**9. What’s the most popular everyday name for a cat?**

Kitty. (`*Sorting by count*` on the text facet, gives the name with the
highest count on the top)

**10.What’s the most popular full name for a dog?**

Sadie. (`*Sorting by count*` on the text facet, gives the name with the
highest count on the top)

### References:

* [Split multi-valued-cells](https://stackoverflow.com/questions/32831582/split-multi-valued-cells-in-more-than-one-column-into-rows-open-refine?answertab=active#tab-top)

* [Open Refine video Tutorials](http://openrefine.org/)

* [Open Refine 101](https://itsmecevi.github.io/openrefine/#6_general_refine_expression_language_(grel))

* [GREL-Date-Functions](https://github.com/OpenRefine/OpenRefine/wiki/GREL-Date-Functions)

* [Github Open Refine recipes](https://github.com/OpenRefine/OpenRefine/wiki/Recipes)\

* [Open Refine walkthrough tutorial](http://web.archive.org/web/20190105063215/enipedia.tudelft.nl/wiki/OpenRefine_Tutorial)

* [NC Building permits database - referred to answer half of my doubts](https://libjohn.github.io/openrefine/start.html#start)

* [Data Carpentry - Open Refine tutorial](https://datacarpentry.org/OpenRefine-ecology-lesson/03-numbers/)

* [Open Refine Cheat sheet](https://code4libtoronto.github.io/2018-10-12-access/GoogleRefineCheatSheets.pdf)

* [Another tutorial](https://schoolofdata.org/handbook/recipes/cleaning-spending-data-open-refine/)

* [Github Documentation for Open Refine - which has answer for almost everything we need to know about Open Refine](https://github.com/OpenRefine/OpenRefine/wiki/Documentation-For-Users)

* [Open Refine Extensions](https://github.com/OpenRefine/OpenRefine/wiki/Installing-Extensions)

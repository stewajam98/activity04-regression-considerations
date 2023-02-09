Day 1 - Qualitative Explanatory Variables
================

In this repository/directory you should see two items:

- `README.md` - this document.
- `activity04.Rmd` - the file you will complete in RStudio for this
  week.

## Task 1: Open the RMarkdown document

Read these directions first, then work through them.

- In the **Files** pane of RStudio, locate and click on the
  `activity04.Rmd` file to open it.
- This file is essentially a blank document with only a `title` and
  `output` option (to produce a GitHub friendly Markdown file). You will
  follow the tasks in this `README` file and do the work (coding,
  responding, etc.) in RStudio.

![check-in](../README-img/noun-magnifying-glass.png) **Check in**

Review your `activity02-day01.Rmd` file (the one from last week) and
take note of: - What you like about its organization. - What do you want
to do differently this time?

Consider my `README` documents and how they are organized. I do not
think these are the “best” way, but they do follow typical standards
(headers, sections, code formatted text, labeled R chunks, etc.)

As you work through this activity, be descriptive in your response to
questions and even leave comments in your code to help you understand
what you are doing. These are your notes to yourself. How can you make
it easier for *future* your to remember what *current* you is
thinking/doing?

## Task 2: Load the necessary packages

We will be using two packages from Posit (formerly
[RStudio](https://posit.co/)): `{tidyverse}` and `{tidymodels}`.
Remember that [Emil Hvitfeldt](https://www.emilhvitfeldt.com/) (of
Posit) has put together a [complementary online
text](https://emilhvitfeldt.github.io/ISLR-tidymodels-labs/index.html)
for the labs in the *ISLR* text that utilize `{tidyverse}` and
`{tidymodels}` instead of base R.

- In the **Packages** pane of RStudio, check if `{tidyverse}` and
  `{tidymodels}` are installed. Be sure to check both your **User
  Library** and **System Library**. **We have used these a lot already
  so they should already be there.**

- Once you have verified that both `{tidyverse}` and `{tidymodels}` are
  installed, load these packages in the R chunk titled `setup`. Press
  Enter/Return after line 7 to add more code lines, then type the
  following:

  ``` r
  library(tidyverse)
  library(tidymodels)
  ```

- Run the `setup` code chunk or **knit**
  <img src="../README-img/knit-icon.png" alt="knit" width = "20"/> icon
  your Rmd document to verify that no errors occur.

Since we will be looking at many relationships graphically, it will be
nice to not have to code each of these individually. `{GGally}` is an
extension to `{ggplot2}` that reduces some of the complexities when
combining multiple plots. For example,
[`GGally::ggpairs`](http://ggobi.github.io/ggally/articles/ggpairs.html)
is very handy for pairwise comparisons of multiple variables.

- In the **Packages** pane of RStudio, check if `{GGally}` is already
  installed. Be sure to check both your **User Library** and **System
  Library**. **We used this last activity so it should already be
  there.**

- Once you have verified that `{GGally}` is installed, load it in the R
  chunk titled `setup`. Add another code line in this chunk, then type
  the following:

  ``` r
  library(GGally)
  ```

- Run the `setup` code chunk or **knit**
  <img src="../README-img/knit-icon.png" alt="knit" width = "20"/> icon
  your Rmd document to verify that no errors occur.

Remember to organize your RMarkdown document using your Markdown skills.

## Task 3: Load the data and

The data we’re working with is from the OpenIntro site. However, this
time we will use a **TSV** file instead of a **CSV** file (hint: not
`readr::read_csv`):
`https://www.openintro.org/data/tab-delimited/evals.txt`

- Create a new R code chunk to read in the linked TSV file.
- Rather than downloading this file, uploading to RStudio, then reading
  it in, explore how to load this file directly from the provided URL
  with the appropriate `{readr}` function (remember that `{readr}` is
  part of `{tidyverse}` so you do not need to load/`library` it
  separately).
- Assign this data set into a data frame named `evals`.

### The data

The data were gathered from end of semester student evaluations for a
large sample of professors from the University of Texas at Austin. In
addition, six students rated the professors’ physical appearance. The
result is a data frame where each row contains a different course and
columns represent variables about the courses and professors.

Review the [Professor evaluations and
beauty](https://www.openintro.org/data/index.php?data=evals) description
page. If you still have questions, review the **Source** at the bottom
of the description page. Additionally, you can use `dplyr::glimpse` to
see some meta information about the R data frame. After doing this,
answer the following questions:

1.  Is this an observational study or an experiment? The original
    research question posed in the paper is whether beauty leads
    directly to the differences in course evaluations. Given the study
    design, is it possible to answer this question as it is phrased? If
    not, rephrase the question.

2.  Describe the distribution of `score`. Is the distribution skewed?
    What does that tell you about how students rate courses? Is this
    what you expected to see? Why, or why not?

3.  Excluding `score`, select two other variables and describe their
    relationship with each other using an appropriate visualization.

## Task 4: Pairwise relationships

The data set contains several variables on the beauty score of the
professor: individual ratings from each of the six students who were
asked to score the physical appearance of the professors and the average
of these six scores. **Challenge**: Using your `{dplyr}` skills, one of
the *selection helper* (hint: `?select`) functions, and `ggpairs`,
obtain visual and numerical summaries (in one plot/figure) to explore
the relationship between all beauty rating variables (there are seven
total variables - six individual ratings and one average rating).

- Create a new R code chunk, write the code to complete the challenge,
  then run your code chunk or knit your document.

After doing this, respond to the following prompts:

4.  Describe the relationship between each pair of beauty variables.
5.  Does it make sense to include all of these variables in our model?
    Why or why not?
6.  If you said, “No,” in (5), which variable(s) do you recommend
    including in our model?

## Task 5: Multiple linear regression: one quantitative predictor, one qualitative predictor

You, hopefully, noticed that the seven variables are collinear
(correlated). Therefore, using more than one of these variables in our
model would not add much value - they essentially say the same thing.
Note: There are more advanced methods to include the variability within
a rater for our model - this is beyond STA 631. If this sounds of
interest to you, explore *generalized estimating equations* (GEE) or
*generalized linear mixed models* (GLMM)

In this Activity and with these highly-correlated predictors, it is
reasonable to use the average beauty score as the single representative
of these variables.

We wish to see if beauty is a significant predictor of professor `score`
after we account for the professor’s `gender` you can add the gender
term into the model.

- Create a new R code chunk and type the following, then run your code
  chunk or knit your document.

  ``` r
  m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
  tidy(m_bty_gen)
  ```

After doing this, respond to the following questions:

7.  *p*-values and parameter estimates should only be trusted if the
    conditions for the regression are reasonable. Verify that the
    conditions for this model are reasonable using diagnostic plots.
    Comment on how well or poorly the conditions are met.

8.  Is `bty_avg` a significant predictor of `score`? Has the addition of
    `gender` to the model changed the parameter estimate for `bty_avg`?

Note that the estimate for `gender` is now called `gendermale`. You will
see this name change whenever you introduce a categorical variable. The
reason is that R recodes `gender` from having the values of `male` and
`female` to being an indicator variable called `gendermale` that takes a
value of $0$ for female professors and a value of $1$ for male
professors. Such variables are often referred to as “indicator” (or
“dummy” - my least favorite term for models) variables.

As a result, for female professors, the parameter estimate is multiplied
by zero, leaving the intercept and slope form familiar from simple
regression.

$$
  \begin{aligned}
\widehat{\texttt{score}} &= \hat{\beta}_0 + \hat{\beta}_1 \times \texttt{bty\\_avg} + \hat{\beta}_2 \times (0) \\
&= \hat{\beta}_0 + \hat{\beta}_1 \times \texttt{bty\\_avg}
\end{aligned}
$$

9.  Write the simplified equation of the line corresponding to male
    professors. *Hint:* For male professors, the parameter estimate is
    multiplied by $1$.

10. For two professors who received the same beauty rating, which gender
    tends to have the higher course evaluation score?

The decision to call the indicator variable `gendermale` instead of
`genderfemale` has no deeper meaning. R simply codes the category that
comes first alphabetically as a $0$. Note: You can change the reference
level of a categorical variable, which is the level that is coded as a
0, using the `relevel` function. Use `?relevel` to learn more.

11. Create a new model called `m_bty_rank` with `gender` removed and
    `rank` added in. Note that the rank variable has three levels:
    `teaching`, `tenure track`, `tenured`. How does R appear to handle
    categorical variables that have more than two levels?

- Create a new R code chunk, write the code to complete this task, then
  run your code chunk or knit your document.

The interpretation of the coefficients in multiple regression is
slightly different from that of simple regression. The estimate for
`bty_avg` reflects how much higher a group of professors is expected to
score if they have a beauty rating that is one point higher *while
holding all other variables constant*. In this case, that translates
into considering only professors of the same rank with `bty_avg` scores
that are one point apart.

## What is next?

We will explore interaction terms (Section 3.3.2) and ways to work
around problems that arise in linear regression models (Section 3.3.3).

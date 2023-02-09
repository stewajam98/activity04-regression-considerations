Day 2 - Interaction terms and other considerations
================

In this repository/directory you should see one items:

- `README.md` - this document.

You will continue working in your
`day01-qualitative-explanatory/activity03.Rmd` file that you started on
[Day 1](../day01-qualitative-explanatory).

## Task 1: Pull your changes from GitHub into RStudio

You successfully updated your GitHub repo from the main
[`README`](../README), but we still need to update your RStudio files.
Read these directions first, then work through them.

- In the **Git** pane of RStudio (upper right-hand area), locate and
  click on the
  <img src="../README-img/pull-icon.png" alt="knit" width = "20"/>
  **Pull** icon.
- After a few moments (you might need to provide your GitHub username
  and PAT), your **Files** pane will be updated with the new items.

Since you are likely looking at the `README` (this page) on GitHub,
nothing of importance for your work today was brought in. However, it is
always a good habit to **pull** from GitHub before starting to work
after taking a break. For example, if you were collaborating on a
project with others, they might do work at different times than you (or
even at the same time). It is always good to solve problems on your end
before **push**ing to GitHub and causing more problems.

## Today

We will explore a MLR model with an interaction between quantitative and
qualitative explanatory variables as well as see some other methods to
assess the fit of our model. Note that the text (*ISL*) covers
interactions between two quantitative explanatory variables as well. By
including an interaction term in our model, we are relaxing the
“additive assumption” a little. However, some folk think of the additive
assumption being about the coefficients (the $\beta$s) and not the
variables.

## Task 2: $\texttt{bty\\_avg} \times \texttt{gender}$ interaction

Recall from Day 1 that we explored the model that included `bty_avg` and
`gender` to predict `score`.

$$
\widehat{\texttt{score}} = \hat{\beta}_0 + \hat{\beta}_1 \times \texttt{bty\\_avg} + \hat{\beta}_2 \times \texttt{gender}
$$

Today we will explore a similar model, except that also includes the
interaction between `bty_avg` and `geneder`. That is,

$$
\widehat{\texttt{score}} = \hat{\beta}_0 + \hat{\beta}_1 \times \texttt{bty\\_avg} + \hat{\beta}_2 \times \texttt{gender} + \hat{\beta}_3 \times (\texttt{bty\\_avg} \times \texttt{gender})
$$

- Open your `day01-qualitative-explanatory/activity04.Rmd` file and
  <img src="../README-img/knit-icon.png" alt="knit" width = "20"/>
  **knit** it to run the work you completed during Day 1 of this
  activity.

- Create a new R code chunk and type the following, then run your code
  chunk or knit your document.

  ``` r
  m_int <- lm(score ~ bty_avg * gender, data = evals)
  tidy(m_int)
  ```

Note that I shortened the model statement using `bty_avg * gender`, but
this can sometimes be confusing to read. Another way to write the
right-hand side of the equation is:
`bty_avg + gender + bty_avg * gender`.

After doing this, answer the following question:

1.  When viewing the `tidy` output, notice that the interaction term is
    listed as `bty_avg:gendermale`. Referring back to Day 1 with how R
    displays qualitative variables, interpret what this syntax means.

2.  Using page 100 of *ISL* as a reference if needed and your work from
    Day 1 (i.e., Question 9), write the simplified equation of the line
    corresponding to a) male professors and b) female professors.

3.  For two professors who received the same beauty rating, which gender
    tends to have the higher course evaluation score?

4.  Like you did in Activity 3 - Day 2, assess the fit of this model (no
    need to do any formal hypothesis testing - we will explore this
    later in the semester). How does `m_int`’s fit compare to
    `m_bty_gen`? What did you use to compare these? Why?

## Task 3: $\texttt{bty\\_avg} \times \texttt{rank}$ interaction

Following the same process (and answering the same questions) you
completed in Task 2, fit a model with `gender` removed and `rank` added
in.

## What is next?

We will set the scene for classification models and have a friendly
competition in our class meetings to apply our MLR modeling skills. On
Day 1 of Activity 5, I will provide you with some additional tools and
how to describe the output to help you assess models (i.e., studentized
residual plots, leverage points, and variance inflation factors).

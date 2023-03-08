# Workshop causal impact assesment

This webpage contains all the materials for a one-day workshop on causal impact assessment. The materials on this website are [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/) licensed.

![cc](https://mirrors.creativecommons.org/presskit/icons/cc.svg) ![by](https://mirrors.creativecommons.org/presskit/icons/by.svg)

## Course objectives

How do we assess whether a school policy intervention has had the desired effect on student performance? How do we estimate the impact a natural disaster has had on the inhabitants of affected regions? How can we determine whether a change in the maximum speed on highways has lead to fewer accidents? These types of questions are at the core of many social scientific research problems. While questions with this structure are seemingly simple, their _causal effects_ are notoriously hard to estimate, because often we cannot perform a randomized controlled experiment. 

In this course, we will deal with several advanced methods for answering such questions, with a dual focus:

- What are the causal assumptions underlying these methods?
- How can we put these methods in practice?

At the end of this workshop, participants have a firm grasp of the basics and limits of causal impact assessment, as well as the skills to start applying these methods in their own work.

## Prerequisites

We assume the following:

- you are comfortable with estimating and interpreting regression models
- you are familiar with the `R` programming language and you have a recent version installed
- it's a bonus if you are somewhat familiar with the `tidyverse` suite of packages (`readr`, `dplyr`, `ggplot`, `tibble`)
- you have installed the following `R` packages on your computer:
  - `tidyverse`
  - `sandwich`
  - `lmtest`
  - `tidysynth`
  - `rdrobust`
  - `fpp3`
  - `mice`
  - `CausalImpact`

You can use the following code to install these at once:
```r
install.packages(c("tidyverse", "sandwich", "lmtest", "tidysynth", "rdrobust", "fpp3", "mice", "CausalImpact"))
```


## Workshop schedule & materials

| Time  | Duration | Activity   | Content                            | link |
| :---: | :------: | :--------- | :--------------------------------- | :--- |
| 09:00 | 30       | Lecture    | Introduction + causal inference    | [`intro_1.pdf`](./lectures/01_introduction/intro_1.pdf) |
| 09:30 | 30       | Lecture    | running example + basic estimators | [`intro_2.pdf`](./lectures/01_introduction/intro_2.pdf) |
| 10:00 | 30       | Practical  | Data intro + post-pre + DiD        | [`intro.html`](./practicals/01_introduction/intro.html) |
| 10:30 | 15       | Break      |                                    | |
| 10:45 | 45       | Lecture    | Interrupted time series (+RDD)     | [`its.pdf`](./lectures/02_interrupted_time_series/its.pdf) |
| 11:30 | 30       | Practical  | Interrupted time series (+RDD)     | [`its.html`](./practicals/02_interrupted_time_series/its.html) | 
| 12:00 | 60       | Lunch      |                                    |
| 13:00 | 45       | Lecture    | Synthetic control                  | [`synthetic_control.pdf`](./lectures/03_synthetic_control/synthetic_control.pdf) |
| 13:45 | 45       | Practical  | Synthetic control                  | [`synthetic_control.html`](./practicals/03_synthetic_control/synthetic_control.html) |
| 14:30 | 15       | Break      |                                    |
| 14:45 | 45       | Lecture    | (synthetic) CITS and CausalImpact  | [`causal_impact.pdf`](./lectures/04_causal_impact/causal_impact.pdf) |
| 15:30 | 45       | Practical  | (synthetic) CITS and CausalImpact  | [`causal_impact.html`](./practicals/04_causal_impact/causal_impact.html) |
| 16:15 | 15       | Break      |                                    |
| 16:30 | 30       | Discussion | Conclusion + open questions        | [`discussion.pdf`](./lectures/05_discussion/discussion.pdf) |
| 17:00 |          | End        |                                    |

You can download the dataset we have prepared from here: [`proposition99.rds`](./data/proposition99.rds). Save it in a nicely accessible place, we will be using it in every practical.

## Additional links

- Course materials for [program evaluation for public service](https://evalsp23.classes.andrewheiss.com/)
- Causal inference for the Brave and True [online python book](https://matheusfacure.github.io/python-causality-handbook/landing-page.html)
- Using Synthetic Controls: Feasibility, Data Requirements, and Methodological Aspects [pdf available here](https://www.aeaweb.org/articles?id=10.1257/jel.20191450)
- Forecasting: principles and practice 3rd ed. [online R book](https://otexts.com/fpp3/)
- Application of causal impact analysis in marketing at [stitch fix](https://multithreaded.stitchfix.com/blog/2016/01/13/market-watch/)

## Contact

This project is developed and maintained by the [ODISSEI Social Data
Science (SoDa)](https://odissei-soda.nl/) team.

<img src="img/soda_logo.png" alt="SoDa logo" width="250px"/>

For questions about this course, you can contact us at [soda@odissei-data.nl](mailto:soda@odissei-data.nl).


---
title: "KIPRE STAFF TRAINING NEEDS ASSESSMENT (TNA) REPORT"
author: ""
date: "2026-01-31"
output: 
  pdf_document:
    latex_engine: pdflatex
    toc: false
    toc_depth: 2

header-includes:
  - \usepackage[table]{xcolor}
  - \usepackage{multirow}
  - \usepackage{textgreek}
  - \usepackage{setspace}
  - \onehalfspacing
  - \usepackage{array}
  - \usepackage{colortbl}
  - \usepackage[pdfpagemode=UseNone,bookmarks=false,bookmarksopen=false,hidelinks]{hyperref}
  - \hypersetup{pdfmenubar=false,pdftoolbar=false,pdfstartview={XYZ null null 0.9}}
  - \usepackage{titling}
  - \pretitle{\begin{center}\LARGE\bfseries}
  - \posttitle{\par\vspace{1em}
    {\large Prepared by Mr. Patrick Mwaura (DSAS) \newline For \newline Mrs. Stella Nyambariga, Head of HR }\par\end{center}\vskip 7em}
  - \preauthor{}\postauthor{}
  - \predate{\begin{center}\large}\postdate{\par\end{center}}

---




\definecolor{softred}{RGB}{255,200,200}
\definecolor{softyellow}{RGB}{255,255,200}
\definecolor{softgreen}{RGB}{200,255,200}
\definecolor{factorgray}{RGB}{220,220,220} 

\section*{Executive Summary}


\newpage


## Background

The primary purpose of this Training Needs Analysis (TNA) is to systematically assess institutional competency gaps and evaluate workforce preparedness. This evidence-based approach is designed to inform training priorities and guide strategic human capital development across the KIPRE.
The assessment was conducted using a multi-dimensional questionnaire structured into specific modules to capture a holistic view of the workforce. These sections included:

* Basic Information (Directorate, Age, and Sex of the employee)
* Job Role and Responsibilities
* Self Performances Assessing Questions
* Skills and Copetency Assessment
* Training History
* Identification of Training Needs
* Training Delivery Preferences
* Organizational Support
* Future Skills and Emerging Needs
* Additional Comments

By triangulating data from these domains, the analysis identifies not only current skill deficiencies but also the organizational support structures and delivery formats required to bridge them effectively.



## Methodology

The assessment was digitized via ODK Central and deployed from $20^{th}-28^{th}$ April of 2026. To ensure we captured a representative voice, we used a multi-channel approach—utilizing official email and professional WhatsApp groups. Participation was voluntary, positioned as a collaborative effort to co-design the organization’s future training roadmap.
Data Processing & Analysis
Once the collection window closed, raw data was ingested into R (v4.5.2). The analytical pipeline focused on cleaning, recoding qualitative responses for statistical depth, and aggregating the results to provide a granular view of needs across all Directorates and Divisions.



## Workforce profile  


A total of 80 employees, representing 42% of the 189 staff members currently at KIPRE, participated in this assessment. This respondent base spans 23 distinct divisions across seven primary Directorates, ensuring the training roadmap reflects both the core scientific mandate and the essential administrative backbone of the institute. The distribution reveals a workforce concentrated within key technical hubs, most notably the Animal Sciences Welfare and Ethics and Research and Product Development Directorates.
In the research domain, the Kenya Snakebite Research and Intervention Division and the Welfare and Ethics Division emerged as the most represented units. This engagement from frontline technical staff—supported by a demographic where 51.2% of respondents are aged between 25 and 44—provides a robust foundation for identifying gaps in laboratory biosafety and modern research protocols. While the gender distribution is relatively balanced (45% female, 55% male), distinct generational trends are evident: the female cadre is most concentrated in the 35–44 age bracket (36.1%), whereas the male workforce peaks in the 45–54 bracket (34.1%).
Administrative and strategic functions are equally well-mapped, with consistent representation from Corporate Services and the Office of the Director General. Notably, only 10% of respondents are aged 55 or older, indicating a predominantly mid-career workforce with significant long-term growth potential. This demographic profile highlights a strategic opportunity to implement cross-cutting training programmes that standardise administrative excellence and ensure the institution is prepared for future succession and evolving digital demands.





``` r
library(tidyverse)
library(janitor)
```

```
## 
## Attaching package: 'janitor'
```

```
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```

``` r
# 1. Summary by Directorate and Division
org_summary <- data %>%
  count(section_a.directorate, section_a.division_section, name = "Staff_Count") %>%
  mutate(
    Directorate = str_to_title(str_replace_all(section_a.directorate, "_", " ")),
    Division = str_to_title(str_replace_all(section_a.division_section, "_", " "))
  ) %>%
  select(Directorate, Division, Staff_Count)

# 2. Gender and Age Distribution
demographic_summary <- data %>%
  tabyl(section_a.gender, section_a.age_bracket) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns() # This shows both Count and Percentage (%)

# 3. Job Grade and Service Length
grade_summary <- data %>%
  count(section_a.job_grade, section_a.length_service) %>%
  pivot_wider(names_from = section_a.length_service, values_from = n, values_fill = 0)

# Print results to console
print("--- Staff Count by Division ---")
```

```
## [1] "--- Staff Count by Division ---"
```

``` r
print(org_summary)
```

```
## # A tibble: 23 x 3
##    Directorate                                              Division Staff_Count
##    <chr>                                                    <chr>          <int>
##  1 Animal Sciences Welfare And Ethics Directorate           Kenya S~           1
##  2 Animal Sciences Welfare And Ethics Directorate           Laborat~           1
##  3 Animal Sciences Welfare And Ethics Directorate           Nationa~           4
##  4 Animal Sciences Welfare And Ethics Directorate           Veterin~           9
##  5 Animal Sciences Welfare And Ethics Directorate           Welfare~          11
##  6 Capacity Building Partnership And Grant Management Dire~ Human R~           1
##  7 Capacity Building Partnership And Grant Management Dire~ Partner~           3
##  8 Corporate Services Directorate                           Corpora~           4
##  9 Corporate Services Directorate                           Finance~           4
## 10 Corporate Services Directorate                           Human R~           5
## # i 13 more rows
```

``` r
print("--- Gender by Age Bracket (%) ---")
```

```
## [1] "--- Gender by Age Bracket (%) ---"
```

``` r
print(demographic_summary)
```

```
##  section_a.gender 25_34_years 35_44_years 45_54_years 55+_years under_25_years
##            female  22.2%  (8)  36.1% (13)  27.8% (10)  5.6% (2)       8.3% (3)
##              male  29.5% (13)  15.9%  (7)  34.1% (15) 13.6% (6)       6.8% (3)
##             Total  26.2% (21)  25.0% (20)  31.2% (25) 10.0% (8)       7.5% (6)
##        Total
##  100.0% (36)
##  100.0% (44)
##  100.0% (80)
```

``` r
print("--- Job Grade by Length of Service ---")
```

```
## [1] "--- Job Grade by Length of Service ---"
```

``` r
print(grade_summary)
```

```
## # A tibble: 11 x 6
##    section_a.job_grade `1_3_years` `16+_years` `4_7_years` `8_15_years`
##    <chr>                     <int>       <int>       <int>        <int>
##  1 nm1                           1           0           0            0
##  2 nm10                          1           5           2            4
##  3 nm11                          0           8           1            1
##  4 nm12                          5           2           1            1
##  5 nm13                          2           1           0            1
##  6 nm2                           1           0           0            0
##  7 nm5                           0           3           0            1
##  8 nm6                           0           2           0            0
##  9 nm7                           3           3           2            2
## 10 nm8                           3           4           9            3
## 11 nm9                           0           1           0            1
## # i 1 more variable: less_than_1_year <int>
```



### 1. Which critical competency gaps exist across directorates, divisions, and job grades, and which areas require immediate training intervention?

This question helps management identify

High-priority skill deficiencies
Departments with the greatest capacity gaps
Staff categories requiring urgent support

Likely outputs

Competency gap heatmaps
Directorate ranking tables
Priority training matrix
Skill deficiency frequencies

Likely recommendations

Targeted capacity-building programmes
Directorate-specific training plans
Prioritised budget allocation



## 2. To what extent are employees adequately prepared to meet emerging organisational, technological, and policy-related demands?


##  Key recommendations 

Priority actions   


## Conclusion   

Strategic implications 


## 7. Examplary visualisations


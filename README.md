# Personality Traits

<p align="center">
 <img width="500px" src="https://www.otempo.com.br/image/contentid/policy:1.2033605:1537401205/image.JPG?f=3x2&q=0.6&w=1200&$p$f$q$w=e65571f" align="center" alt="Identify Personality Traits" />
 <h2 align="center">Should this candidate be selected?</h2>
 <p align="center">The goal of this project is to use unsupervised learning algorithms to identify personality traits of candidates for a particular vacancy.</p>

# Index

-   [Introduction](#introduction)
-   [Dataset](#dataset)
    -   [Variables](#variables)
-   [PCA](#pca)
    -   [The 7 principal components](#the-7-principal-components)
-   [Clustering](#clustering)
-   [Conclusion](#conclusion)

# Introduction

In this project, data is used to filter candidates in the selection process recruitment for a particular vacancy in a company that is hiring employees.

The model developed aims to identify whether the profile of each candidate is suitable for the vacancy in question or direct candidates to the vacancies most suited to their profile, in situations of new projects or other activities.

# Dataset

The dataset “Personality Classification Type” was extracted from the following repository: [Open psychology data: Raw data from online personality tests (openpsychometrics.org)](http://openpsychometrics.org/_rawdata/).
>Section - Answers to the Machivallianism Test, a version of the MACH-IV from Christie and Geis (1970)

## Variables

### Input Variables

The variables Q1A to Q20A are used, the individual evaluates on a scale of 1 to 5, how much do you agree with the statement, where 1 is “Disagree” and 5 is “Agree”.

### Profile Variables

The profile variables correspond to characteristics sociodemographic characteristics of people, thus allowing to help draw segmented conclusions.

# PCA

Starting with 20 components (number of variables), 3 methods were used to find the number of principal components to maintain:

- Kaiser
- Screeplot
- Explained Variance

After using the methods, it was decided to keep 7 principal components. In loadings, the values in orange indicate in which variables each component
weighs more.

<p align="center">
  <img width="400px" src="https://user-images.githubusercontent.com/121397357/220458697-75b605aa-8b2d-4626-b85b-0e0c6d8c7c12.png" />
</p>

## The 7 principal components

Component   | Name
--------- | ------
RC1 | Calculism
RC2 | Honesty
RC3 | The Good of People
RC4 | Free Will
RC5 | Naivety of People
RC6 | People's Priorities
RC7 | Virtue

# Clustering

Several algorithms of clustering were used, like:
- Hierarchical Clustering;
- K-Means Clustering;
- PAM;
- Clustering of variables;
- etc.

## Best result was algorithm K-Means with 2 clusters

<p align="center">
  <img width="600px" src="https://user-images.githubusercontent.com/121397357/220468422-8ec68e41-93e7-4007-8dba-d82a13766d0c.png" />
</p>

<div align="center">
  <table>
    <tr> 
      <td> Cluster 1 </td>
      <td> Cluster 2 </td>
    </tr>
    <tr> 
      <td> Individual Worker </td>
      <td> Team Player </td>
    </tr>
  </table>
</div>

# Conclusion

It's possible to achieve the main objective of helping the Human Resources department in the selection of the candidate who's more suited to the vacancy.

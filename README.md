# Residential particulate matter air pollution and breast cancer risk: A prospective cohort analysis and meta-analysis
This repository accompanies the manuscript:

Richards Z, Wang S, Berrington de Gonzalez A, et al.
Residential particulate matter air pollution and breast cancer risk: a prospective cohort analysis and meta-analysis.

Submitted to Lancet Oncology (2026).

## Summary
This repository contains code used for analyses of residential PM2.5 and PM10
air pollution and breast cancer incidence in the UK Generations Study and for
the accompanying systematic review and meta-analysis.

The repository includes statistical analysis scripts, figure generation code,
and documentation required to reproduce published results.

## Data Availability 
The data underlying this study are from the Breast Cancer Now Generations Study and are not publicly available due to participant confidentiality and consent restrictions. 
Information on how to apply for data access is available at https://thegenerationsstudy.co.uk/. 
Air pollution data is available from Utrecht University, more application information at https://expanseproject.eu/. 

## Overall File Structure by Figure

| Output | Figure creation location |
|---------|---------|
| Figure 1 | GS_analysis.qmd |
| Figure 2 | Meta_analysis.qmd |
| Figure 3 | Meta_analysis.qmd |
| Figure 4 | Meta_analysis.qmd |
| Table 1 | GS_analysis.qmd |
| Table 3 | Meta_analysis.qmd |
| Supplementary Figure 1 | GS_analysis.qmd |
| Supplementary Figure 3 | Meta_analysis.qmd |
| Supplementary Table 5 | Meta_analysis.qmd |
| Supplementary Table 6 | Meta_analysis.qmd |

### UK Generations Study Cohort Analysis
Software Version: R version 4.4.1

Markdown for the UK Generations Study Analysis can be found here: [Generations Study Analysis](https://psychic-adventure-4q9wyj4.pages.github.io/GS_analysis.html)

### Meta-Analysis
Software Version: R version 4.6.2

Markdown for the Meta Analysis could be found here: http://localhost:4777/

Search date:
16 October 2026

Databases:
PubMed
Web of Science
EMBASE

Protocol registration:
CRD420251075459

Methods structure for meta-analysis: 
| Steps | Document location |
|---------|---------|
| Literature search strategy | Supplementary Table 2 |
| Study selection and eligibility criteria | Supplementary Table 3 & PRISMA Flow Chart.pptx (Supplementary Figure 2) |
| Data extraction | Data extraction table.xlsx (Supplementary Table 4) |
| Meta analysis & Subgroup analysis | Meta_analysis.qmd |
| Bias assessment | Meta_analysis.qmd |
| Population attributable fraction (PAF) estimation | Meta_analysis.qmd |

All manuscript figures can be recreated from source code using scripts in
the analysis/ directory.

Vector graphics were exported as SVG files for publication.

## License

This project is licensed under the MIT License. See the LICENSE file for details.

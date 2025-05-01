# MANTRIX
Predicting impacts of praying mantids using fuzzy interaction webs

# Description
Developing tools for rapidly predicting which introduced species will become invasive 
is essential for effective management. It’s also notoriously difficult. 
Quantitative models exist but often require extensive data, precluding timely application. 
Fuzzy Interaction Webs (FIW), a qualitative modeling approach, offer a practical alternative. 
FIWs hold the potential to predict impacts, guide early responses, and identify 
research needs using the limited data commonly available on an invasion front. 
As a case study, we applied FIW to a European praying mantis (*Mantis religiosa*) 
invasion in grassland communities of western Montana, USA. Modeling was done using 
the [MPG Matrix online tool](https://matrix.mpgranch.com/#/gallery)

# Directory of Reports
- **[Mantid observations](mantid_observations_mt.md).** See where mantids have been observed 
in western Montana and how many have been seen in recent years. These data are from 
[iNaturalist's](https://www.iNaturalist.org) citizen scientists.
- **[Mantid FIW parameters](mantid_abundance_pcis.md).** Read a summary of how we obtained 
mantid abundance and interaction strength estimates.
- **[Predicted impacts](sensitivity_analysis_results.md)** of mantids on native species vary, 
but are severe at high mantid abundances. Periodic cold snaps and parasitoids may 
dampen impacts.

# Fuzzy Interaction Webs
Two baseline FIWs were created to test nine scenarios. Scenarios 1-4 test the effects of increased
mantid abundance on the grassland community. Scenarios 5-9 test the effects of winter severity
and parasitoid wasps on mantid populations. Link to the scenarios on the MPG Matrix below. 
Reference the **How To Use** guide, accessible from the website header, and the 
[FAQ sheet](https://matrix.mpgranch.com/#/faq) to duplicate and modify the FIWs. 

## Scenarios and FIW links
**[Pre-invasion baseline FIW](https://matrix.mpgranch.com/#/matrices/bf3590):** Experiments
varying mantid abundance; impacts on native grassland community

1. [Observed-Low](https://matrix.mpgranch.com/#/matrices/2b8a41/experiments/066a05)
2. [Modeled Equilibrium](https://matrix.mpgranch.com/#/matrices/2b8a41/experiments/2610e8)
3. [Observed-Mid](https://matrix.mpgranch.com/#/matrices/2b8a41/experiments/1891c3)  
4. [Observed-High](https://matrix.mpgranch.com/#/matrices/2b8a41/experiments/7cc356)

**[Post-invasion baseline FIW](https://matrix.mpgranch.com/#/matrices/e096f7):** Experiments
varying winter severity and natural enemies of mantids; impacts on mantids and ramifying effects

5. [No Cold Snaps, No Wasps](https://matrix.mpgranch.com/#/matrices/e096f7/experiments/8c5042)
6. [Four Cold Snaps, No Wasps](https://matrix.mpgranch.com/#/matrices/e096f7/experiments/1b1e7c)
7. [No Cold Snaps, 6X Wasps](https://matrix.mpgranch.com/#/matrices/e096f7/experiments/36622d)
8. [Two Cold Snaps, 6X Wasps](https://matrix.mpgranch.com/#/matrices/e096f7/experiments/fdcd6a)
9. [Four Cold Snaps, 6X Wasps](https://matrix.mpgranch.com/#/matrices/e096f7/experiments/e44ac9)

# Appendix
- Source and analysis of [weather data](wmt_winter_severity.md) used to examine the potential 
for climate to affect mantid populations

![praying mantis BGL](https://www.inaturalist.org/observations/244570042)


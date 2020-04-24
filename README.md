# suicide-prevention-2019
Projects evaluates change in suicide morbidity and mortality rates among youth (10-24) in relationship to the GLS suicide prevention program in selected Florida counties. 

# Analytic Products

- [`./analysis/gls-activity/gls-activity-2-coverage.html`][gls_coverage] - Summary tables and graphs describing the delivery of GLS programming in the state of Florida for 2014-2018. 

- [`./analysis/counts-and-rates/counts-and-rates.html`][counts_and_rates] - shows how population size, death counts, and suicide rates vary across counties. Addresses the issues of measurement. 

- [`./analysis/trend-summary/trend-summary.html`][trend_summary] - displays observed counts and rates of suicide by sex, race, and ethniciy.

[gls_coverage]:https://raw.githack.com/dss-hmi/suicide-prevention-2019/master/analysis/gls-activity/gls-activity-2-coverage.html

[gls_pivot]:https://raw.githack.com/dss-hmi/suicide-prevention-2019/master/analysis/0-greeter/0-greeter-gls-pivot.html

[counts_and_rates]:https://raw.githack.com/dss-hmi/suicide-prevention-2019/master/analysis/counts-and-rates/counts-and-rates.html

[trend_summary]:https://raw.githack.com/dss-hmi/suicide-prevention-2019/master/analysis/trend-summary/trend-summary.html

# Data Genesis
- [`./manipulation/0-greeter-gls`][greeter_gls] - activity of GLS Suicide prevention program
- [`./manipulation/1-greeter-population`][greeter_population] - population estimates by FL Health
- [`./manipulation/2-greeter-suicide`][greeter_suicide] - deaths counts as reported by FL Health
- ...  
- [`./manipulation/9-combiner`][combiner] - combines sources and serves analysis-ready dataframe

[greeter_gls]:https://raw.githack.com/dss-hmi/suicide-prevention-2019/master/analysis/0-greeter/0-greeter-gls.html
[greeter_population]:https://raw.githack.com/dss-hmi/suicide-prevention-2019/master/analysis/1-greeter/1-greeter-population.html
[greeter_suicide]:https://raw.githack.com/dss-hmi/suicide-prevention-2019/master/analysis/2-greeter/suicide.html
[combiner]:https://raw.githack.com/dss-hmi/suicide-prevention-2019/master/analysis/9-combiner/9-combiner.html



# Links and Resources 
- [Research Notebook][notebook] (OneDrive sign-in required)
- [SP2019 shared folder][onedrive] on OneDrive (sign-in required)

[notebook]:https://ucf-my.sharepoint.com/:w:/g/personal/ki637574_ucf_edu/Ebcn_bcQRNpHtf97_nRvJYABQWOPzmpoC80ZPXct46gXVA?e=IYfaQh
[onedrive]:https://ucf-my.sharepoint.com/:f:/r/personal/ki637574_ucf_edu/Documents/SP2019?csf=1&e=OFZmDl

# Data 
 - [description of data sources](https://github.com/dss-hmi/suicide-prevention-2019/blob/master/data-public/raw/README.md)   

Using query builder of the Florida Health Charts (http://flhealthcharts.com)  we have obtained the following data tables:  
- Population estimates ( see [issue  4][issue4])
- Death Counts (see [issue 5][issue5])

[issue4]:https://github.com/dss-hmi/suicide-prevention-2019/issues/4
[issue5]:https://github.com/dss-hmi/suicide-prevention-2019/issues/5

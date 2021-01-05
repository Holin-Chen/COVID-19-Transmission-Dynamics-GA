# Transmission of COVID-19 in the state of Georgia, United States: Spatiotemporal variation and impact of social distancing

*For the full contents of our research, please visit [our preprint on medRxiv](https://www.medrxiv.org/content/10.1101/2020.10.22.20217661v1).*

This is an Emory COVID-19 response project collabrated with the Georgia Department of Public Health (GDPH) to help in guiding the state in prevention and control of COVID -19. We used the clinical data provided by GDPH to study the COVID-19 transmission in Georgia by identifying transmission patterns according to disease symptoms, demographics, and time periods. In addition, we examined the transmission dynamics by quantifying the time scale of spreading, summarized by serial interval, and estimating the magnitude of spreading, summarized by time-varied reproduction number R<sup>t</sup>, for COVID-19. 

## Methods

The data source are 118,491 confirmed COVID-19 cases with demographic and clinical information in all 159 counties during February 1–July 13, 2020. From the data, there are 4080 tracked pairs of primary case (infectors) and their secondary case (infectees).

The serial interval for symptom onset was defined as the number of days between symptom onset for a primary case and an associated secondary case. We modeled the serial interval as a gamma distribution and estimated the shape and scale parameters by using the data of tracked pairs of transmission cases and maximum likelihood method.

We estimated probabilities of transmission between any pair of cases in an outbreak, as proposed by [Teunis et al](https://royalsocietypublishing.org/doi/full/10.1098/rsif.2012.0955?cited-by=yes&legid=royinterface%3B10%2F81%2F20120955) and [Wang et al](https://www.frontiersin.org/articles/10.3389/fmed.2020.00329/full). For an outbreak with n observed cases, a transmission probability matrix can be defined with any element V<sup>i,j</sup> representing the probability that case i was infected by case j. When two cases are linked by their serial interval, the likelihood of transmission between these two cases can be calculated using the serial interval distribution as a kernel density. Additional information at an individual level (e.g., evidence of social contact between cases i and j) is accounted for by a n * n weighting matrix. The transmission probability matrix can be estimated in a Markov chain Monte Carlo procedure.



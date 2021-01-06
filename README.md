# Transmission of COVID-19 in the state of Georgia, United States: Spatiotemporal variation and impact of social distancing

*For the full contents of our research, please visit [our preprint on medRxiv](https://www.medrxiv.org/content/10.1101/2020.10.22.20217661v1).*

This is an Emory COVID-19 response project collabrated with the Georgia Department of Public Health (GDPH) to help in guiding the state in prevention and control of COVID-19. We used the clinical data provided by GDPH to study the COVID-19 transmission in Georgia by identifying transmission patterns according to disease symptoms, demographics, and time periods. In addition, we examined the transmission dynamics by quantifying the time scale of spreading, summarized by serial interval, and estimating the magnitude of spreading, summarized by time-varied reproduction number R<sub>t</sub>, for COVID-19. 

## Methods

The data source are 118,491 confirmed COVID-19 cases with demographic and clinical information in all 159 counties during February 1–July 13, 2020. From the data, there are 4080 tracked pairs of primary case (infectors) and their secondary case (infectees).

The serial interval for symptom onset was defined as the number of days between symptom onset for a primary case and an associated secondary case. We modeled the serial interval as a gamma distribution and estimated the shape and scale parameters by using the data of tracked pairs of transmission cases and maximum likelihood method.

We estimated probabilities of transmission between any pair of cases in an outbreak, as proposed by [Teunis et al](https://royalsocietypublishing.org/doi/full/10.1098/rsif.2012.0955?cited-by=yes&legid=royinterface%3B10%2F81%2F20120955) and [Wang et al](https://www.frontiersin.org/articles/10.3389/fmed.2020.00329/full). For an outbreak with n observed cases, a transmission probability matrix can be defined with any element V<sub>i,j</sub> representing the probability that case i was infected by case j. When two cases are linked by their serial interval, the likelihood of transmission between these two cases can be calculated using the serial interval distribution as a kernel density. Additional information at an individual level (e.g., evidence of social contact between cases i and j) is accounted for by a n * n weighting matrix. The transmission probability matrix can be estimated in a Markov chain Monte Carlo procedure.

When the transmission probability matrix is known, it can be used to calculate reproduction numbers. Elements of row i show the probabilities of case i having received their infection from any other case in the observed population. Rows of the matrix must therefore add to 1. Likewise, elements of column j show the probabilities that case j has transmitted their infection to any other cases in the observed population. Columns of the matirx therefore add to an estimate of the number of cases infected by case j: its reproduction number R<sub>t</sub>.

Using GDPH data on cases confirmed with COVID-19 during February 1– July 13, 2020, we estimated effective reproduction numbers (R<sub>t</sub>) by date, using dates of symptoms onset and social contact information (wherever available) in each county independently by estimating the transmission probability matrix. Among 118,491 confirmed cases, 48,887 (41.3%) had a missing date of symptom onset. These missing symptom onset dates were imputed based on dates of first specimen collection when available, or dates of laboratory report otherwise. The number of days between symptom onset and date of first specimen collected (or date of laboratory report) was modeled using negative binomial regression with the date of first specimen collected (or date of laboratory report) as the predictor.

## Main Results

1. **The average serial interval became shorter over time: from 5.97 days in February–April,186 to 5.03 days in May, and then to 4.40 days in June–July (Figure 1).**

<p align="center">
  <img src="https://github.com/Holin-Chen/COVID-19-Transmission-Dynamics-GA/blob/main/Paper%20Plots/Picture1.png" />
</p>

<div align="center">
  Figure 1. The estimated serial interval distribution for three time periods
</div>

2. **The major transmission shifted from elder generation to younger generation: from ages 40–70 years old in February–April to 20–50 years old in June–July (Figure 2).**

<p align="center">
  <img src="https://github.com/Holin-Chen/COVID-19-Transmission-Dynamics-GA/blob/main/Paper%20Plots/Picture2.png" />
</p>

<div align="center">
  Figure 2. Patterns of COVID-19 disease transmission by age group in three successive time periods
</div>

3. **By mid-July, two waves of COVID-19 transmission were apparent, separated by the shelter-in-place period in the state of Georgia.**

- The general pattern of transmission on R<sub>t</sub> was similar across all counties: a “first wave” started higher than 1 and then decreased until early May, followed by a “second wave” after the shelter-in-place order was lifted  (Figure 3).

![alt text](https://github.com/Holin-Chen/COVID-19-Transmission-Dynamics-GA/blob/main/Paper%20Plots/Picture3.png)

Figure 3. Distributions of estimated dates of first maximum, minimum, and second maximum in R<sub>t</sub> for 87 counties with cumulative 200 cases by July 13, 2020, and key events possibly driving COVID-19 transmission

- There is a strong heterogeneity of the dates of county-level R<sub>t</sub> reaching first peak, local minimum and second peak, and the magnitude of R<sub>t</sub> varied among counties. We summarized 5 transmission pattern categories based on epidemic curves and R<sub>t</sub> outputs in all the counties (Figure 4).

![alt text](https://github.com/Holin-Chen/COVID-19-Transmission-Dynamics-GA/blob/main/Paper%20Plots/Picture4.PNG)

<div align="center">
  Figure 4. Transmission pattern categories based on epidemic curves and estimated Rt curves 
</div>
**• Consistent spreading:** sustained transmission of COVID-19 (R<sub>t</sub> > 1) during the shelter-in-place period. Consequently, numbers of cases were high and increased rapidly upon reopening.

**• Two strong waves:** a first wave of early transmission followed by a slowdown (R<sub>t</sub> < 1) during the shelter-in-place period. After reopening, a new surge in cases (1 <= R<sub>t</sub> < 2) appeared.

**• Strong first wave:** a considerable number of cases during the initial period of the outbreak. During the shelter-in-place period spreading was controlled and after reopening no new surge in cases occurred (R<sub>t</sub> < 1).

**• Strong second wave**: there were few cases during the early transmission period, but a surge in new cases (R<sub>t</sub> >= 2) after reopening.

**• Small case number (n < 200)**: COVID-19 transmission was rare.

4. **Counties around major cities and along interstate highways had more intense transmission in Georgia (Figure 5).**

<p align="center">
  <img src="https://github.com/Holin-Chen/COVID-19-Transmission-Dynamics-GA/blob/main/Paper%20Plots/Picture5.jpg" />
</p>

<div align="center">
  Figure 5. spatial distribution of the five categories of transmission patterns of COVID-19 in Georgia by June 15, 2020
</div>



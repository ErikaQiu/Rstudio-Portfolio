### Customer Segmentation Classification
#### Overview
This project aims to classify customers of an automobile company into four segments (A, B, C, D) to better understand customer behaviors and preferences. By analyzing data from previous customers, we developed models using logistic regression, K-nearest neighbors (KNN), and Linear Discriminant Analysis (LDA) to predict potential customer segments in new markets.
#### Methods
##### Logistic Regression  
We applied logistic regression to binary groups created from the four segments and identified the most influential variables in customer segmentation.  

##### K-Nearest Neighbors (KNN)  
We utilized the KNN algorithm with Euclidean and Jaccard distances to classify customers based on similarity measures.  

##### Linear Discriminant Analysis (LDA)  
LDA was employed to reduce dimensionality and classify customer segments, providing insights into which variables significantly impact segmentation. 

#### Key Findings  
Variables Influencing Segmentation: The analysis identified key variables, such as marital status, profession, and spending score, as significant predictors of customer segmentation. 
Model Comparison: Logistic regression provided the highest accuracy among the models tested, indicating its suitability for binary classification tasks within this context.  

#### Segmentation Insights:
The segmentation process revealed distinct customer behaviors and preferences, aiding the company's market strategy development.

#### Getting Started
Clone the repository to your local machine.  
Ensure you have R and the necessary packages installed.  
Run the scripts in the /Scripts directory to perform the analysis.  

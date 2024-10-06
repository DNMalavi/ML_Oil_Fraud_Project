<center><b> Part 1: Detecting Olive Oil Fraud Using Unsupervised Learning</b></center>

This section of the project explores the use of unsupervised learning techniques to detect fraud in Extra-Virgin Olive Oil (EVOO) using **Near-Infrared Hyperspectral Imaging (NIR-HSI) data**.

**Overview**

- Objective: Apply **Principal Component Analysis (PCA)** for dimensionality reduction and K-Means clustering for unsupervised classification to identify patterns and potential adulteration in EVOO samples.
- **Data**: Hyperspectral data from pure and adulterated EVOO samples, capturing the spectral fingerprint of each sample.
- **Approach**: Use PCA to reduce data complexity and reveal key spectral features, followed by K-Means clustering to classify the samples based on spectral similarities.

**Key Files**


- `data`/: Contains the NIR-HSI,GC-MS, FTIR, Uv-Vis data for EVOO samples.
- `pca_analysis.R`: Script for performing PCA on the hyperspectral data.
- `kmeans_clustering.R`: Script for applying K-Means clustering to the PCA-transformed data. results/: Output files including plots and clustering results.

**How to Run**

- Ensure that the data is in the data/ folder.
- Run the pca_analysis.R script to perform PCA on the dataset.
- Run the kmeans_clustering.R script to cluster the samples using K-Means.

**Results**

- PCA reduces the dimensionality of the data while retaining the most significant spectral features.
- K-Means clustering does not fully classify the samples into clusters, which should correspond to pure and adulterated EVOO.

**Next Steps** 

The insights from PCA and K-Means clustering lay the foundation for `Part 2`, which will focus on `supervised machine learning models` for further `classification` and detection of EVOO adulteration.

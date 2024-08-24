import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.mixture import GaussianMixture
from scipy.stats import norm

# Load the data
data = pd.read_csv('data.csv')

# Combine all features into a single array
all_features = data.values.flatten()

# Remove duplicates for KDE calculation
unique_features = np.unique(all_features)

# KDE using seaborn
sns.kdeplot(all_features, bw_adjust=0.5, label='KDE', linestyle='--')

# Fit a Gaussian Mixture Model (GMM)
n_components = 3  # Number of Gaussians, you can adjust this
gmm = GaussianMixture(n_components=n_components, covariance_type='full')
gmm.fit(all_features.reshape(-1, 1))

# Create a range of values for plotting the fitted GMM
x = np.linspace(min(all_features), max(all_features), 1000).reshape(-1, 1)
logprob = gmm.score_samples(x)
pdf = np.exp(logprob)

# Plotting
plt.plot(x, pdf, color='red', label='Fitted GMM')

# Display the plot
plt.xlabel('Feature Value')
plt.ylabel('Density')
plt.title('KDE and Fitted Gaussian Mixture Model')
plt.legend()
plt.show()

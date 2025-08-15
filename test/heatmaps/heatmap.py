import pandas as pd

import matplotlib.pyplot as plt

df = pd.read_csv('test/heatmaps/voters.csv')

x = df.iloc[:, 0]
y = df.iloc[:, 1]

plt.figure(figsize=(10, 8))
plt.hexbin(x, y, gridsize=50, cmap='viridis')
plt.xlabel('X')
plt.ylabel('Y')
plt.title('Voters')
plt.colorbar(label='Counts')
plt.show()

#!/usr/bin/env python3

import pandas as pd

import matplotlib.pyplot as plt

# Read voters
df_voters = pd.read_csv('examples/heatmaps/voters.csv')
x_voters = df_voters.iloc[:, 0]
y_voters = df_voters.iloc[:, 1]

# Read electeds
df_electeds = pd.read_csv('examples/heatmaps/elected.csv')
elected_points = {}
systems = ['First-past-the-post', 'Two-round system', 'Instant runoff voting', 'Borda count', 'Copeland-Llull (Condorcet)', 'Approval voting']
for system,i in zip(systems, range(0, len(df_electeds.columns), 2)):
    ip2 = i + 2
    df = df_electeds.iloc[:, i:ip2]
    elected_points[system] = df.copy()

fig = plt.figure(figsize=(16, 7))
axes = fig.subplots(2, 4)

max_count = 0
def make_plot(x, y, title, axis):
    hb = axis.hexbin(x, y, gridsize=50, cmap='viridis', extent=(0,100,0,100))
    axis.set_xlabel('X')
    axis.set_ylabel('Y')
    axis.set_title(title)

    if any([s > max_count for s in hb.get_sizes()]):
        max_count = max(hb.sizes)
        fig.colorbar(hb, ax=axis, label='Counts')

make_plot(x_voters, y_voters, 'Voters', axes[0, 0])
for (system, df), axis in zip(elected_points.items(), axes.flat[1:]):
    make_plot(df.iloc[:, 0], df.iloc[:, 1], system, axis)

plt.tight_layout()
plt.show()
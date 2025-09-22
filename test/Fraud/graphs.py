#!/usr/bin/env python3

import pandas as pd
import numpy as np

from scipy.stats import gaussian_kde

import matplotlib.pyplot as plt

binwidth = 0.1
edges = np.arange(-binwidth/2, 100+binwidth/2, binwidth)
centers = np.arange(0,100,binwidth)

df_voters = pd.read_csv('test/Fraud/2024.csv')
putin_voters = df_voters["Путин Владимир Владимирович"]
valid_voters = df_voters["Число действительных избирательных бюллетеней"]
regis_voters = df_voters["Число избирательных бюллетеней, полученных участковой избирательной комиссией"]
putin_shares = 100 * putin_voters / valid_voters
turnout_rate = 100 * valid_voters / regis_voters

ph = np.histogram(putin_shares, bins=edges)[0]
th = np.histogram(turnout_rate, bins=edges)[0]

mag = int(100 / binwidth)
iindices = np.arange(0,mag,10)
nindices = [i for i in range(0, mag) if i % 10 > 0]

print('Precincts not integral:')
print(ph.take(nindices).sum())
print('Integral precincts:')
print(ph.take(iindices).sum())

def histogram(h: np.ndarray, title: str):
    plt.plot(centers, h, linewidth=1)
    plt.title(f'Number of precincts with x {title}')
    plt.xlabel(title)
    plt.ylabel('No. Precincts')
    plt.show()
    plt.close()

    plt.plot(centers.take(nindices), h.take(nindices), linewidth=1)
    plt.plot(centers.take(iindices), h.take(iindices), linewidth=1)
    plt.legend(['Non-integral precincts', 'Integral precincts'])
    plt.title(f'Number of precincts with x {title}')
    plt.xlabel(title)
    plt.ylabel('No. Precincts')
    plt.show()
    plt.close()

histogram(ph, 'Putin vote share')
histogram(th, 'Turnout')

plt.hexbin(turnout_rate, putin_shares, gridsize=100, cmap='viridis', extent=(0,100,0,100))
plt.xlabel('Turnout')
plt.ylabel('Putin %')
plt.show()
plt.close()

putin_c = [np.sum(putin_voters[turnout_rate<=(xi/10)], 0)/np.sum(valid_voters) for xi in range(mag)]

plt.plot(centers, putin_c)
plt.title('Cumulative vote share v. turnout')
plt.xlabel('Turnout')
plt.ylabel('Cumulative vote share for Putin')
plt.show()
plt.close()

# Benford's law

second_digits_putin = sorted([str(int(pv))[1] for pv in putin_voters if not np.isnan(pv) and pv > 10])

plt.hist(second_digits_putin)
plt.show()
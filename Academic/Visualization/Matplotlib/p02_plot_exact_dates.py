# Goal:
# Plot exact dates on X axis


#%%
import matplotlib.pyplot as plt
import numpy as np

#%%

y = np.array([1,2,3])
time = np.array(['2000-01-01','2005-05-01','2020-12-01'], dtype="datetime64[D]")

#%%

fig, ax = plt.subplots()

ax.plot(time,y)
ax.set_xticks(time)
fig.show()

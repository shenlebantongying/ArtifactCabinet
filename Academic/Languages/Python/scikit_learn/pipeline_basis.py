# %%
from sklearn.pipeline import make_pipeline
from sklearn.linear_model import LinearRegression
import numpy as np
import matplotlib.pyplot as plt

# %%
x = np.linspace(0, 10, 10)
y = x * 3 + 2
# %%

reg = LinearRegression()
linearReg_pipeline = make_pipeline(reg)
# %%
linearReg_pipeline.fit(x.reshape(-1, 1), y)

# %%
plt.plot(x, y)
plt.plot(x, linearReg_pipeline.predict(x.reshape(-1, 1)))

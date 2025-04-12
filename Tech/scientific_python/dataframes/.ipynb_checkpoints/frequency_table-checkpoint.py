# %%
# Goal:
# Given data from 3 groups
# Provides a frequency table
# %%
import pandas as pd
import numpy as np
# %%
d1= pd.DataFrame({"t":[1,2,3,4],"d1_v":['a','b','a','a']}).set_index("t")
d2= pd.DataFrame({"t":[1,2,3,4],"d2_v":['a','a','b','b']}).set_index("t")
d3= pd.DataFrame({"t":[1,2,3,4],"d3_v":['b','b','a','b']}).set_index("t")

d_all=pd.concat([d1,d2,d3],axis=1)
d_all
# %%
d_melt=d_all.melt()
d_melt
# %%
ct = d_melt.groupby(by="variable").value_counts()
ct
# %%
ct.to_numpy().reshape(2,3)
# %%

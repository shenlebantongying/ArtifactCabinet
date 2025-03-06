# %%
import pandas as pd

# %%
d = pd.DataFrame(
    {
        "d1": ["a", "b", "a", "a", "a"],
        "d2": ["a", "a", "b", "b", "b"],
        "d3": ["b", "b", "a", "b", "a"],
    }
)

# %%

d_melt = d.melt()
d_melt

# %%
d_melt.pivot_table(columns="variable", index="value", values="value", aggfunc=len)

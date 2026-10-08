# Goals:
# + create a frequency table for each columns' item

# %%
import ibis
import ibis.selectors as s

ibis.options.interactive = True

# %%
con = ibis.connect("duckdb://:memory:")
con.create_table(
    "freq",
    {
        "d1": ["a", "b", "a", "a", "a"],
        "d2": ["a", "a", "b", "b", "b"],
        "d3": ["b", "b", "a", "b", "a"],
    },
)
tb = con.table("freq")
tb

# %%
tb_longer = tb.pivot_longer(s.all())
tb_longer

# %%
tb_longer.pivot_wider(id_cols="value", values_agg="count").order_by("value")

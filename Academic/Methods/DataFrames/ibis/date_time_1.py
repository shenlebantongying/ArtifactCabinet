# Goals:
# + create timestamps for every hour in 2025
# + create new columns of year/month/day data

# %%
import ibis

ibis.options.interactive = True
ibis.options.repr.interactive.max_rows = 48

# %%
dts = []
for i in range(365 * 24):
    dts.append(
        (ibis.timestamp("2025-01-01 00:00:00") + ibis.interval(hours=i)).to_pyarrow()
    )

# %%
con = ibis.connect("duckdb://:memory:")
con.create_table("c", obj={"dt": dts}, schema=ibis.schema({"dt": "Timestamp"}))
t = con.table("c")
t
# %%

t.select("dt").mutate(year=t.dt.year(), month=t.dt.month(), day=t.dt.day())

# %%
t.count()

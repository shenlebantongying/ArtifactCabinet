# %%
import matplotlib.patches as mpatches
import matplotlib.pyplot as plt

# %%

fig, ax = plt.subplots()

ax.set_xlim(-11, 11)
ax.set_ylim(-11, 11)

arr = mpatches.FancyArrowPatch((0, 0), (5, 5), arrowstyle="->", mutation_scale=20)
ax.add_patch(arr)
ax.annotate(
    "Some Text",
    (0.5, 0.5),
    xycoords=arr,
    ha="center",
    va="bottom",
    rotation=45,
    rotation_mode="anchor",
)

circles = [mpatches.Circle((0, 0), x) for x in range(1, 10)]

for i, c in enumerate(circles):
    c.set_fill(False)

    def color_loop(x):
        match x % 3:
            case 0:
                return "r"
            case 1:
                return "g"
            case 2:
                return "b"
        return None

    c.set_edgecolor(color_loop(i))

ax.set_aspect(1)

for c in circles:
    ax.add_patch(c)


fig.show()

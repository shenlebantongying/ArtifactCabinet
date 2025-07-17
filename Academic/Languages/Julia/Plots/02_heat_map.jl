using CairoMakie

x = 1:10
y = 1:10
data = reshape(1:100, 10, 10)

fig,ax,hm = heatmap(x, t, data)

Colorbar(fig[:,end+1],hm)

save("a.pdf", fig)
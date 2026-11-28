fig = figure;
tiledlayout(2,1)

ax1=nexttile;
ax2=nexttile;

ax1.NextPlot="add";
ax2.NextPlot="add";
plot(ax1,rand(1,10))
plot(ax1,rand(1,10))
plot(ax2,rand(1,10))
plot(ax2,rand(1,10))

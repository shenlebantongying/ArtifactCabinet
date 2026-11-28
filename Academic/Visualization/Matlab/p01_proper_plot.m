% Properly do things.
fig = figure;
ax = axes("NextPlot","add");
for i=1:5
    plot(ax,1:10,rand(1,10));
end
exportgraphics(fig,"p01.pdf")

%% basic
x = [1 2 3; 4 5 6] %[output:69a044e1]
y = [10 20 30] %[output:09a5ac9c]
x+y %[output:4ee3bcae]
%%
%% 1*n
x2 = [1 2 3] %[output:86abc491]
x2 .* x2' %[output:2fb4d0a6]
bsxfun(@eq, x2, x2') %[output:3c737794]
eq(x2,x2') %[output:25157461]
%%
%% lower
repmat([1,2,3],2,1) %% expand only one dimension %[output:244c5476]

%[appendix]{"version":"1.0"}
%---
%[metadata:view]
%   data: {"layout":"onright"}
%---
%[output:69a044e1]
%   data: {"dataType":"matrix","outputData":{"columns":3,"name":"x","rows":2,"type":"double","value":[["1","2","3"],["4","5","6"]]}}
%---
%[output:09a5ac9c]
%   data: {"dataType":"matrix","outputData":{"columns":3,"name":"y","rows":1,"type":"double","value":[["10","20","30"]]}}
%---
%[output:4ee3bcae]
%   data: {"dataType":"matrix","outputData":{"columns":3,"name":"ans","rows":2,"type":"double","value":[["11","22","33"],["14","25","36"]]}}
%---
%[output:86abc491]
%   data: {"dataType":"matrix","outputData":{"columns":3,"name":"x2","rows":1,"type":"double","value":[["1","2","3"]]}}
%---
%[output:2fb4d0a6]
%   data: {"dataType":"matrix","outputData":{"columns":3,"name":"ans","rows":3,"type":"double","value":[["1","2","3"],["2","4","6"],["3","6","9"]]}}
%---
%[output:3c737794]
%   data: {"dataType":"matrix","outputData":{"columns":3,"header":"3×3 logical array","name":"ans","rows":3,"type":"logical","value":[["1","0","0"],["0","1","0"],["0","0","1"]]}}
%---
%[output:25157461]
%   data: {"dataType":"matrix","outputData":{"columns":3,"header":"3×3 logical array","name":"ans","rows":3,"type":"logical","value":[["1","0","0"],["0","1","0"],["0","0","1"]]}}
%---
%[output:244c5476]
%   data: {"dataType":"matrix","outputData":{"columns":3,"name":"ans","rows":2,"type":"double","value":[["1","2","3"],["1","2","3"]]}}
%---

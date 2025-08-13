ode := diff(y(t), t) = (4*t - 2*t*y(t))/(t^2 + 4);
sol := dsolve(ode);

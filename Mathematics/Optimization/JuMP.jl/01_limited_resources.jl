import JuMP, Ipopt, HiGHS

#=
The most basic OR problem.

[a,b,c] are 3 resources and [x,y,z] are 3 products.

Each product needs different amount of resources.

The profit of each products are different.

How to obtain the maxium possible profit?

# The data below has many solutions.
# Different solvers yields different results, but maxium profit is 100;
=#
begin
    a = 100
    b = 200
    c = 300

    # consume rate
    a_comsume = [1, 2, 3]
    b_comsume = [2, 1, 3]
    c_comsume = [3, 2, 1]

    model = JuMP.Model(Ipopt.Optimizer)
    # model = JuMP.Model(HiGHS.Optimizer)
    JuMP.@variable(model, x >= 0)
    JuMP.@variable(model, y >= 0)
    JuMP.@variable(model, z >= 0)

    JuMP.@constraint(model, sum([x, y, z] .* a_comsume) <= a)
    JuMP.@constraint(model, sum([x, y, z] .* b_comsume) <= b)
    JuMP.@constraint(model, sum([x, y, z] .* c_comsume) <= c)

    JuMP.@objective(model, Max, x + 2y + 3z)
    JuMP.optimize!(model)

    println(model)

    println(JuMP.value(x))
    println(JuMP.value(y))
    println(JuMP.value(z))
    println(JuMP.value(x) + 2 * JuMP.value(y) + 3 * JuMP.value(z))
end

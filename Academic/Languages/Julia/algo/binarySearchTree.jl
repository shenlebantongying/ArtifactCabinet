mutable struct Tree
    k
    l::Union{Tree,Nothing}
    r::Union{Tree,Nothing}
    Tree(k) = new(k, nothing, nothing)
end

function push!(t::Tree, k)
    if k < t.k
        if isnothing(t.l)
            t.l = Tree(k)
        else
            push!(t.l, k)
        end
    else
        if isnothing(t.r)
            t.r = Tree(k)
        else
            push!(t.r, k)
        end
    end
end

function treeWalk(t::Tree, level=0)
    if !isnothing(t.l)
        treeWalk(t.l, level + 1)
    end

    println(("-"^level) * "> " * string(t.k))

    if !isnothing(t.r)
        treeWalk(t.r, level + 1)
    end
end

iTree = Tree(0)

for i in [999, 1000, 1, 2, 4, 3, 998, 850]
    println("i -> " * string(i))
    push!(iTree, i)
end

treeWalk(iTree)

display(iTree)

%%
function acc = compute(k,N,P)
    acc = 0.0;
    t = 2^k;
    for i = N:t
        % FIXME: MATLAB's nchoosek is not accurate after 15 digits for double type...
        acc = acc + nchoosek(t, i)*P^i*(1-P)^(t-i);
    end
end

%%
data = readmatrix("./data/rosalind_lia.txt")
k = data(1)
N = data(2)
P = 1/4
compute(k,N,P)

%% Or use built-in CDF of binomial function
1-binocdf(N-1,2^k,P)

%%
x = linspace(0,2^k,200);
plot(x,1-binocdf(x,2^k,1/4))

function sol = newton_sym(f,i_sym,x0)
% Finding root f(x)=0 using the newton's method
%
%  f -> sym function
%  i_sym -> symbol used in f
%  x0 -> initial guess
%
%  x_(i+1) = x_(i) - f(x_i)/f'(xi)

arguments
    f sym
    i_sym sym
    x0 double
end

    f1=diff(f,i_sym);

    func_f = @(x) subs(f,i_sym,x);
    func_f1= @(x) subs(f1,i_sym,x);

    x_iter = x0;

    for i = 1:10000
        x_new = double(x_iter - func_f(x_iter)/func_f1(x_iter));

        if abs(x_new - x_iter)<0.001
            break
        end
        x_iter = x_new;
    end

    sol = x_iter;

end

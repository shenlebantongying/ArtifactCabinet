% https://www.mathworks.com/matlabcentral/cody/problems/672-longest-run-of-consecutive-numbers

function p672()
    function val=longrun(a)
        count = ones(size(a));
        for i=2:numel(a)
            if a(i)==a(i-1)
                count(i)=count(i-1)+1;
            end
        end
        val=a(count==max(count));
    end

a = [0 1 1 1 0 2 2 0 1 1 1 0];
longrun(a)
assert(isequal(longrun(a),[1 1]))
end

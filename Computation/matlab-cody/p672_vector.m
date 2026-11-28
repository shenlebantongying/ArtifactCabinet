% https://www.mathworks.com/matlabcentral/cody/problems/672-longest-run-of-consecutive-numbers
% Fully vectorized solution.

function p672_vector()
    function val=longrun(a)
        % find indexes where value is different from previous ones
        diff_idx=find(vertcat(1,diff(a(:))));
        distances=diff(diff_idx);
        val = a(diff_idx(distances==max(distances)));
    end

a = [0 1 1 1 0 2 2 0 1 1 1 0]';
longrun(a)
assert(isequal(longrun(a),[1 1]'))
end

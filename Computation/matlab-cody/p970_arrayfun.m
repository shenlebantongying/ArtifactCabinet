% https://www.mathworks.com/matlabcentral/cody/groups/12/problems/970

function p970_arrayfun

    function out=tracers(x,y)
        r1=x(:,1)';
        r2=x(:,2)';

        t1=arrayfun(@(x,y,n) linspace(x,y,n), r1(1:end-1), r1(2:end), y, "UniformOutput", false);
        t2=arrayfun(@(x,y,n) linspace(x,y,n), r2(1:end-1), r2(2:end), y, "UniformOutput", false);

        o=vertcat(horzcat(t1{:}),horzcat(t2{:}))';

        dups=diff(o)==0;
        o(dups(:,1),:)=[];
        out=o;
    end

x = [1 1 ; 3 0 ; 4 1];
y = [5 3];
tracers(x,y)
end

classdef mycat < handle
%                ^ this enable a class to modify roperties
    properties
        Name
    end
    methods
        
        % "static method"
        function toString(obj)
            disp(obj.Name);
        end
       
        function obj = setName(obj,name)
            obj.Name = name;
        end
        
    end
end


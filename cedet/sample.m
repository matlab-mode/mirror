classdef (abstract) sample ...
        < handle %Comment
    
    properties
        Prop1 (1,1) double
    end

    events
        EventName
    end

    methods
        function obj = sample(varargin)
            
            obj.Prop1 = 2; 
        end
        
        function [ b ] = fcn_ret ...
                (a)
            b=true;
            if a
            end
        end
        
        function fcn_no_ret(varargin)

        end
    end
    
    methods (Access='private')
        function fcn_in_another_block(a,b)
           disp(a,b) 
        end
    end
end


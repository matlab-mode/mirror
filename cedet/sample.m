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
                local_fcn(a);
            end
        end
        
        function c = fcn_no_ret(varargin)

            function b = nested_fcn(a)
                b = a;
            end
            
            c = nested_fcn(varargin{1});
        end
    end
    
    methods (Access='private')
        function fcn_in_another_block(a,b)
           disp(a,b)
        end
    end
end

function c = local_fcn(d)
   
    function b = nested_local_fcn(a)
        b = a;
    end
    
    c = nested_local_fcn(d);
end

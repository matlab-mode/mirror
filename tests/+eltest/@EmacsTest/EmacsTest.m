classdef EmacsTest
% Class EMACSTEST
%
% Class that produces errors and file names we can test.

    properties
        prop1;
    end

    methods
	function h = EmacsTest()
	% Constructor
	   
        end
        
        function throwerr(~)
            error('Error thrown from Emacs Test');
        end
        
        function throwprop(et)
            et.prop1 = 1;
            et.prop2 = 2; % should error
        end
    end
end
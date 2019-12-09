function emacsinit(clientcommand, startnetshell)
% EMACSINIT Initialize the current MATLAB session for matlab-shell-mode
%
% clientcommand is the emacsclient command used to open files in emacs. It
% is defined by `matlab-shell-emacsclient-command' in matlab.el. If empty,
% don't instruct MATLAB to use emacsclient to edit files.

    me = mfilename('fullpath');
    % This command can also update the path for the emacs Toolbox direcory.
    % This will make it possible to use this one command from a standalone
    % MATLAB and setup netshell.
    [ myDir ] = fileparts(me);
    
    if ~contains(path, myDir)
        
        disp(['Updating MATLAB Path to support Emacs toolbox + ' myDir]);

        addpath(myDir,'-begin');
        rehash;
    end
    
    if usejava('jvm')

        %{
        % Leaving in old hot-link code and description (see below)
        % in case someone with older MATLAB's need to use this.
        
        v = ver('MATLAB');
        if str2double(v.Version) < 8.5
            % In 8.5 (R2015b) the MATLAB removed the ability to display hot link's when
            % debugging in -nodesktop mode. In R2015a (8.4) MATLAB would display in -nodesktop
            % mode something like:
            %   >> dbstop in testit
            %   >> testit
            %   >> <a href="matlab: opentoline('/path/to/testit.m',3,1)">3   </a>    a = 10 * 10;
            %   K>>
            % and emacs then would use the hot link to drive debugging from within emacs.
            % Given that R2015b and later does not have hot links, we use the graphical debugging
            % by leaving EditorGraphicalDebugging set to true.

            % Use the desktop hotlinking system in MATLAB Shell.  matlab-shell
            % will interpret them, and provide clickable areas.
            % NOTE: This doesn't work in all cases where HotLinks are used.
            feature('HotLinks','on');

            % Disable built-in editor showing up for debugging
            com.mathworks.services.Prefs.setBooleanPref('EditorGraphicalDebugging', false);
        end
        %}

        % Starting in matlab-emacs v 4.0, we can simulate debugging
        % hot links, so we should always disable graphical
        % debugging.
        
        % Disable built-in editor showing up for debugging
        com.mathworks.services.Prefs.setBooleanPref('EditorGraphicalDebugging', false);
        
        
        if nargin == 1 && ~isempty(clientcommand)
            % Use clientcommand (e.g. emacsclient -n) for text editing
            com.mathworks.services.Prefs.setBooleanPref('EditorBuiltinEditor', false);
            com.mathworks.services.Prefs.setStringPref('EditorOtherEditor', clientcommand);
        end

        % Disable wrapping of text lines.  Emacs will wrap or not based on user preference.
        com.mathworks.services.Prefs.setBooleanPref('WrapLines',false)
    else
        % TODO - how to specify this preference w/out Java ?
        
    end

    % Check if we're running inside emacxs.  If we are NOT, then force the enablement of
    % the netshell interface to Emacs.
    emacs_env = getenv('INSIDE_EMACS');
    
    if isempty(emacs_env)
        startnetshell = true;
    elseif nargin < 2
        startnetshell = false;
    end

    % If requested, start the Emacs netshell interface.
    if startnetshell
        nso = emacsnetshell('init');
    else
        nso = [];
    end

    % Initialize Emacs breakboint handler.
    bp = emacs.Breakpoints(nso);
    setappdata(groot, 'EmacsBreakpoints', bp);

    % Initialize Emacs stack handler.
    bp = emacs.Stack(nso);
    setappdata(groot, 'EmacsStack', bp);

end
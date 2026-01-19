function [output] = string2boolean(string)
%STRING2BOOLEAN converts a string into a boolean. Returns true if, and only
%if, string is 'true'
%   Detailed explanation goes here
if strcmp(string,'true')
    output = true;
else
    output = false;
end

end

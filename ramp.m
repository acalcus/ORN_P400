function [output_pt] = ramp(pt,ramp_samp)
%PA_RAMP Create a sinusoidal ramp of ramp_samp samples for pt
%Author Celine Alameda, 2021

ramp = ones(1, length(pt));

%raising ramp at the beginning of a chord
for i = 1:ramp_samp
   ramp(i) = sin(pi * i / (2 * ramp_samp));
end

%falling ramp at the end of a chord
for i = 0:ramp_samp-1
    ramp(length(pt) - i) = sin(pi * i / (2 * ramp_samp));
end

output_pt = times(pt, ramp);

end

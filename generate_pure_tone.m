function [pt] = generate_pure_tone(freq, phase, dur, srate, PLOT)
% [pt] = generate_pure_tone(freq, phase, dur, srate, PLOT)
%       freq        Value specifying frequency (Hz).
%       phase       Value specifying phase (radians).
%       dur         Value specifying duration of tone (seconds).
%       srate       Value specifying sampling rate (Hz).
%       PLOT        Boolean (1 or 0) specifying whether to plot the
%                   waveform as a figure.
%
% Generates a pure tone with desired phase, frequency, and duration.
% 
% Emma Holmes
% Created on 04/01/2017


%% Generate tone
t 	= 0 : (1/srate) : (dur - 1/srate);  	% Time vector
pt	= sin(2 * pi * freq * t + phase);       % Waveform


%% Plot, if desired
if PLOT
    figure; 
    plot(t, pt, 'LineWidth', 2);
    
    % Tidy up figure
    set(gca, 'Box', 'off', 'XTick', [], 'XTickLabels', [], ...
        'YTick', [], 'YTickLabels', []);
    axis off;
    set(gcf, 'Position', [92, 270, 926, 269]);
end
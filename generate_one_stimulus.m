function [stimulus, stimulusParams] = generate_one_stimulus(chord_duration, stimulus_duration, figure_duration, figure_coherence, background_present, figure_present, sample_rate, all_freqs)
% GENERATE_ONE_STIMULUS generates one stimulus
%   Detailed explanation goes here

n_tonal_components = floor(5 + 11 * rand(1, stimulus_duration));
%extra components (figure or random extra components)
extra_tonal_components_onset = floor(15 + 6 * rand(1, 1));
extra_tonal_components = zeros(1, stimulus_duration);
extra_tonal_components(1, extra_tonal_components_onset:extra_tonal_components_onset+figure_duration - 1) = figure_coherence;

stimulusParams.chord_duration = chord_duration;
stimulusParams.stimulus_duration = stimulus_duration;
stimulusParams.figure_duration = figure_duration;
stimulusParams.figure_coherence = figure_coherence;
stimulusParams.extra_tonal_components_onset = extra_tonal_components_onset;
stimulusParams.background_present = background_present;
stimulusParams.figure_present = figure_present;
stimulusParams.sample_rate = sample_rate;
stimulusParams.all_freqs = all_freqs;

stimulus = [];
struct_all_tonal_components = [];
for chord_index = 1:stimulus_duration  
    tonal_components = n_tonal_components(chord_index);
    chord = zeros(1, chord_duration * sample_rate / 1000);    
    n_extra = extra_tonal_components(chord_index);
    % draw only if figure is not present
    if ~figure_present || exist("extra", "var") == 0 || numel(extra) == 0
        extra = draw_without_replacement(length(all_freqs), n_extra, []);
    end    
    % now draw the normal tonal components while avoiding the extra
    frequency_indexes = draw_without_replacement(length(all_freqs), tonal_components, extra);
    % even if we have the extra elements for the figure, do not include
    % them if figure is off
    n_extra_elements_to_include = min(numel(extra), n_extra);
    if background_present
        frequency_indexes = [frequency_indexes extra(1:n_extra_elements_to_include)];
    else
        frequency_indexes = extra(1:n_extra_elements_to_include);
    end
    frequencies = all_freqs(frequency_indexes);
    for frequency_index = 1:length(frequencies) 
        frequency = frequencies(frequency_index);
        pure_tone = generate_pure_tone(frequency, 0, chord_duration / 1000, sample_rate, false);
        chord = chord + ramp(pure_tone, sample_rate/100);
    end
    chord = chord / length(frequencies);
    stimulus = [stimulus chord];
    struct_tonal_components.frequency_indexes = frequency_indexes;
    struct_all_tonal_components = [struct_all_tonal_components struct_tonal_components];
end

stimulusParams.tonal_components = struct_all_tonal_components;

end

% subfunctions

function [draw] = draw_without_replacement(n_freqs, n_to_draw,  elements_to_avoid)
    freq_indices = 1:n_freqs;
    freq_indices(elements_to_avoid) = [];
    draw = randperm(numel(freq_indices), n_to_draw);
    draw = freq_indices(draw);
end


function run_stimulus_creation_Beno(dirSTIM)
% Stimulus Difficulty

% 260 (130 per condition), with 1/3 absent and 2/3 present (see Tóth)
% create arrays such as that, with 1 value per difficulty
ns_figure_absent_stimuli = [44;44];
ns_figure_present_stimuli = [88;88];
figure_coherences = [8;8]; % number of extra tonal components
figure_durations = [7;7]; % Teki 2013 duration of extra tonal components


% constant stimuli values
stimulus_duration = 40; % chords
chord_duration = 50; % ms
sample_rate = 48000.0; % Hz
n_freqs             = 129;
all_freqs           = 440 * 2 .^((-31 : 97) / 24);
sound_level         = 1.0;

% stimVar
stimVar.ns_figure_absent_stimuli = ns_figure_absent_stimuli;
stimVar.ns_figure_present_stimuli = ns_figure_present_stimuli;
stimVar.figure_coherences = figure_coherences;
stimVar.figure_durations = figure_durations;
stimVar.stimulus_duration = stimulus_duration;
stimVar.chord_duration = chord_duration;
stimVar.sample_rate = sample_rate;
stimVar.n_freqs             = n_freqs;
stimVar.all_freqs           = all_freqs;
stimVar.rng_seed            = 10061986;

% initialize rng with a seed for replicability
rng(stimVar.rng_seed);

% generate sound calibration stimuli
stimulus = generate_one_stimulus(chord_duration, 10*stimulus_duration, 10, 7, true, false, sample_rate, all_freqs, sound_level);
fileName = "sound_calibration.wav";
audiowrite(strcat(dirSTIM, fileName), stimulus, sample_rate);
stimVar.sound_calibration_name = fileName;
% generate example stimuli
coherence_example = figure_coherences(2); %easy
figure_duration_example = 7; % easy
% 3 background only
example_bg_names = [];
for i = 1:3
    stimulus = generate_one_stimulus(chord_duration, stimulus_duration, figure_duration_example, coherence_example, true, false, sample_rate, all_freqs, sound_level);
    fileName = "example_bg_"+i+".wav";
    example_bg_names = [example_bg_names fileName];
    audiowrite(strcat(dirSTIM, fileName), stimulus, sample_rate);
end
stimVar.example_bg_names = example_bg_names;
% 3 figure only
example_fig_names = [];
for i = 1:3
    stimulus = generate_one_stimulus(chord_duration, stimulus_duration, figure_duration_example, coherence_example, false, true, sample_rate, all_freqs, sound_level);
    fileName = "example_fig_"+i+".wav";
    example_fig_names = [example_fig_names fileName];
    audiowrite(strcat(dirSTIM, fileName), stimulus, sample_rate);
end
stimVar.example_fig_names = example_fig_names;

% 6 familiarization, easy
familiarization_names = [];
familiarization_figure_pst = [];

for i = 1:10
    if i < 6
        fig_pst = false;
    else
        fig_pst = true;
    end
   
    stimulus = generate_one_stimulus(chord_duration, stimulus_duration, figure_durations(1), figure_coherences(1), true, fig_pst, sample_rate, all_freqs, sound_level);
    fileName = "familiarization_"+i+".wav";
    familiarization_names = [familiarization_names fileName];
    familiarization_figure_pst = [familiarization_figure_pst fig_pst];
    audiowrite(strcat(dirSTIM, fileName), stimulus, sample_rate);
end
stimVar.familiarization_names = familiarization_names;
stimVar.familiarization_figure_pst = familiarization_figure_pst;


% generate the real stimuli
stimuli_structures = [];
for difficulty_level = 1:length(figure_durations)
    figure_duration = figure_durations(difficulty_level);
    figure_coherence = figure_coherences(difficulty_level);
    n_figure_present_stimuli = ns_figure_present_stimuli(difficulty_level);
    n_figure_absent_stimuli = ns_figure_absent_stimuli(difficulty_level);
    n_stimuli = n_figure_present_stimuli + n_figure_absent_stimuli;
    stimuli_structure = struct('figure_duration', figure_duration);
    stimuli_params = [];
    for i = 1:n_stimuli
        if i <= n_figure_present_stimuli
            i_file = i;
            presence = "present";
            presence_bool = true;
        else
            i_file = i - n_figure_present_stimuli;
            presence = "absent";
            presence_bool = false;
        end
        [stimulus, stimulusParams] = generate_one_stimulus(chord_duration, stimulus_duration, figure_duration, figure_coherence, true, presence_bool, sample_rate, all_freqs, sound_level);
        stimulus_name = "stim_"+presence+"_"+figure_duration+"_"+i_file+".wav";
        audiowrite(strcat(dirSTIM, stimulus_name), stimulus, sample_rate);
        stimulusParams.stimulus_name = stimulus_name;
        stimuli_params = [stimuli_params stimulusParams];
    end
    stimuli_structure.stimuli_params = stimuli_params;
    stimuli_structures = [stimuli_structures stimuli_structure];
end
stimVar.stimuli = stimuli_structures;

writestruct(stimVar, strcat(dirSTIM, "stimVar.xml"));

end


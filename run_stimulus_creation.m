function run_stimulus_creation(dirDATA, dirSTIM)
% Stimulus Difficulty

dirDATA = '/Users/axelle/Dropbox/Dossier de l''équipe InMignonetteWeTrust/MATLAB/stochastic-figure-ground-task/data/';    
dirSTIM = '/Users/axelle/Dropbox/Dossier de l''équipe InMignonetteWeTrust/MATLAB/stochastic-figure-ground-task/Stimuli/';

% 260 (130 per condition), with 1/3 absent and 2/3 present (see Tóth)
% create arrays such as that, with 1 value per difficulty
difficulties_names = {'easy'; 'hard'};
ns_figure_absent_stimuli = [87; 87];
ns_figure_present_stimuli = [173; 173];
figure_coherences = [8 ; 8]; % number of extra tonal components
figure_durations = [7 ; 4]; % Teki 2013 duration of extra tonal components


% constant stimuli values
stimulus_duration = 40; % chords
chord_duration = 50; % ms
sample_rate = 44100.0; % Hz
n_freqs             = 129;
all_freqs           = 440 * 2 .^((-31 : 97) / 24);


% stimVar
stimVar.difficulties_names = difficulties_names;
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

% % generate example stimuli
% coherence_example = figure_coherences(2); %easy
% figure_duration_example = 10; %extra easy for the example
% % 3 background only
% example_bg_names = [];
% for i = 1:3
%     stimulus = generate_one_stimulus(chord_duration, stimulus_duration, figure_duration_example, coherence_example, true, false, sample_rate, all_freqs);
%     fileName = "example_bg_"+i+".wav";
%     example_bg_names = [example_bg_names fileName];
% %    audiowrite(strcat(dirSTIM, fileName), stimulus, 44100);
% end
% stimVar.example_bg_names = example_bg_names;
% % 3 figure only
% example_fig_names = [];
% for i = 1:3
%     stimulus = generate_one_stimulus(chord_duration, stimulus_duration, figure_duration_example, coherence_example, false, true, sample_rate, all_freqs);
%     fileName = "example_fig_"+i+".wav";
%     example_fig_names = [example_fig_names fileName];
% %    audiowrite(strcat(dirSTIM, fileName), stimulus, 44100);
% end
% stimVar.example_fig_names = example_fig_names;
% % 6 familiarization, easy
% familiarization_names = [];
% for i = 1:6
%     if i < 4
%         fig_pst = true;
%     else
%         fig_pst = false;
%     end
%     stimulus = generate_one_stimulus(chord_duration, stimulus_duration, figure_durations(2), figure_coherences(2), true, fig_pst, sample_rate, all_freqs);
%     fileName = "familiarization_"+i+".wav";
%     familiarization_names = [familiarization_names fileName];
% %    audiowrite(strcat(dirSTIM, fileName), stimulus, 44100);
% end
% stimVar.familiarization_names = familiarization_names;
% stimVar.familiarization_figure_pst = [true true true false false false];


% generate the real stimuli
stimuli_structures = [];
for difficulty_level = 1:length(difficulties_names)
    figure_duration = figure_durations(difficulty_level);
    figure_coherence = figure_coherences(difficulty_level);
    n_figure_present_stimuli = ns_figure_present_stimuli(difficulty_level);
    n_figure_absent_stimuli = ns_figure_absent_stimuli(difficulty_level);
    n_stimuli = n_figure_present_stimuli + n_figure_absent_stimuli;
    stimuli_structure = struct('difficulty', difficulties_names(difficulty_level));
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
        [stimulus, stimulusParams] = generate_one_stimulus(chord_duration, stimulus_duration, figure_duration, figure_coherence, true, presence_bool, sample_rate, all_freqs);
        stimulus_name = "stim_"+presence+"_"+figure_duration+"_"+i_file+".wav";
        audiowrite(strcat(dirSTIM, stimulus_name), stimulus, 44100);
        stimulusParams.stimulus_name = stimulus_name;
        stimuli_params = [stimuli_params stimulusParams];
    end
    stimuli_structure.stimuli_params = stimuli_params;
    stimuli_structures = [stimuli_structures stimuli_structure];
end
stimVar.stimuli = stimuli_structures;

writestruct(stimVar, strcat(dirDATA, "stimVar.xml"));

end

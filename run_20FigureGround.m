% Clear the workspace
close all;
clear;
sca;
Screen('Preference', 'SkipSyncTests', 1);

addpath("1_Functions\");
%----------------------------------------------------------------------
%                       Directories
%----------------------------------------------------------------------

dirDATAa = '/Users/axelle/Dropbox/Dossier de l''équipe InMignonetteWeTrust/MATLAB/stochastic-figure-ground-task/data/';               
dirDATAb = 'C:/Users/Celine/Documents/GitHub/stochastic-figure-ground-task/Data/';
if exist(dirDATAa, 'dir')
    dirDATA = dirDATAa;
    dirSTIM = '/Users/axelle/Dropbox/Dossier de l''équipe InMignonetteWeTrust/MATLAB/stochastic-figure-ground-task/Stimuli/';
elseif exist(dirDATAb, 'dir')
    dirDATA = dirDATAb;
    dirSTIM = 'C:/Users/Celine/Documents/GitHub/stochastic-figure-ground-task/Stimuli/';
else
    disp("Error : no suitable directory structure found")
    return
end
clear dirDATAa dirDATAb


% First generate Stimuli if no stimuli are found
stimuli_wav_files = dir(strcat(dirSTIM, '*.wav'));
if(isempty(stimuli_wav_files))
    disp("Stimulus directory is empty. Creating stimuli.")
    run_stimulus_creation(dirDATA, dirSTIM);
    disp("Done. Run this script again to launch experiment.")
    sca;
    clear;
    return;
end




%----------------------------------------------------------------------
%                       Experiment setup
%----------------------------------------------------------------------

texts; % charger les textes
stimVar = readstruct(strcat(dirDATA, "stimVar.xml")); %stimuli information

% set or reset rng seed, so that possible rng use in stimulus generation
% does not affect the experiment
rng(stimVar.rng_seed);

expVar.expID = '20_FigureGround';
% Get user input data
promptstr = {'Subject ID', 'age', 'genre'};
initstr = {'abc', '00','M/F/X'};
titlestr = 'Experimental variables';
nlines = 1;
fileOK = false;
while ~fileOK
    % h = findobj ;
    dlgresult = inputdlg(promptstr,titlestr,nlines,initstr,'on');
    if ~isempty(dlgresult)
        expVar.SubjID = (dlgresult{1});
        expVar.age = str2num(dlgresult{2});
        expVar.genre = (dlgresult{3});
    else % cancel
        return
    end
    % si le fichier existe déjà, i.e. on a déjà un sujet numéroté comme ça
    if isfile(strcat(dirDATA, "expVar_",expVar.SubjID,".xml"))
        choice = questdlg('Le fichier existe déjà. Il sera écrasé. Continuer ?', 'EXPERIMENTER INPUT', ...
        'OK', 'Annuler', 'Annuler');
        switch choice
            case 'OK'
                fileOK = true;
            case 'Annuler'
                continue;
        end
    else
        fileOK = true;
    end
end

%----------------------------------------------------------------------
%                       Timing Information
%----------------------------------------------------------------------

% Interstimulus interval time in seconds
isiTimeSecs = 0.5;
minimumPause = 0.2; % pour s'assurer q'un appui long sur la touche ne compte pas comme deux appuis
% pour éviter une réponse trop précoce. 
% Le nombre d'accords minimal avant la figure est de 15, donc une réponse
% avant n'a pas de sens
minimumDelayBeforeAnswer = 0.75; 
soundDuration = 2;

%----------------------------------------------------------------------
%                       PsychToolBox Setup
%----------------------------------------------------------------------

% Setup PTB with some default values
PsychDefaultSetup(2);
% Set the screen number to the external secondary monitor if there is one
% connected
screenNumber = max(Screen('Screens'));
% Define black, white and grey
white = WhiteIndex(screenNumber);
grey = white / 2;
black = BlackIndex(screenNumber);
green = [0 1 0];
red = [1 0 0];

% Open the screen
[window, windowRect] = PsychImaging('OpenWindow', screenNumber, grey, [], 32, 2,...
    [], [],  kPsychNeed32BPCFloat);
% get size of the screen
[screenXpixels, screenYpixels] = Screen('WindowSize', window);
% Flip to clear
Screen('Flip', window);

% Query the frame duration
ifi = Screen('GetFlipInterval', window);

% Set the text size
Screen('TextSize', window, 40);

% Query the maximum priority level
topPriorityLevel = MaxPriority(window);

% Get the centre coordinate of the window
[xCenter, yCenter] = RectCenter(windowRect);


%----------------------------------------------------------------------
%                       Experiment
%----------------------------------------------------------------------

% appuyer sur espace pour commencer
DrawFormattedText(window, text_space_to_begin, 'center', 'center', black);
Screen('Flip', window);
wait_for_spacebar;
% présentation générale de l'expérience
DrawFormattedText(window, text_begin_experiment_1, 'center', screenYpixels /2 - 50, black);
DrawFormattedText(window, text_space_to_continue, 'center', screenYpixels - 100, black);
Screen('Flip', window);
pause(minimumPause);
wait_for_spacebar;
DrawFormattedText(window, text_begin_experiment_1, 'center', screenYpixels /2 - 50, black);
DrawFormattedText(window, text_begin_experiment_2, 'center', screenYpixels /2, black);
DrawFormattedText(window, text_space_to_continue, 'center', screenYpixels - 100, black);
Screen('Flip', window);
pause(minimumPause);
wait_for_spacebar;
DrawFormattedText(window, text_begin_experiment_1, 'center', screenYpixels /2 - 50, black);
DrawFormattedText(window, text_begin_experiment_2, 'center', screenYpixels /2, black);
DrawFormattedText(window, text_begin_experiment_3, 'center', screenYpixels /2 + 50, black);
DrawFormattedText(window, text_space_to_continue, 'center', screenYpixels - 100, black);
Screen('Flip', window);
pause(minimumPause);
wait_for_spacebar;
% présentation bg
for i = 1:3
    DrawFormattedText(window, convertStringsToChars(text_bg(i)), 'center', 'center', black);
    Screen('Flip', window);
    [stim, fs] = audioread(strcat(dirSTIM, stimVar.example_bg_names(i))); 
    pause(minimumPause)
    wait_for_n_or_x;
    player = audioplayer(stim,fs);
    playblocking(player);
end

% figures 
for i = 1:3
    DrawFormattedText(window, convertStringsToChars(text_fig(i)), 'center', 'center', black);
    Screen('Flip', window);
    [stim, fs] = audioread(strcat(dirSTIM, stimVar.example_fig_names(i))); 
    pause(minimumPause)
    wait_for_n_or_x;
    player = audioplayer(stim,fs);
    playblocking(player);
end

% Explication de la familiarisation
DrawFormattedText(window, text_familiarization_1, 'center', screenYpixels /2 - 50, black);
DrawFormattedText(window, text_space_to_continue, 'center', screenYpixels - 100, black);
Screen('Flip', window);
pause(minimumPause);
wait_for_spacebar;
DrawFormattedText(window, text_familiarization_1, 'center', screenYpixels /2 - 50, black);
DrawFormattedText(window, text_familiarization_2, 'center', screenYpixels /2, black);
DrawFormattedText(window, text_space_to_continue, 'center', screenYpixels - 100, black);
Screen('Flip', window);
pause(minimumPause);
wait_for_spacebar;
DrawFormattedText(window, text_familiarization_1, 'center', screenYpixels /2 - 50, black);
DrawFormattedText(window, text_familiarization_2, 'center', screenYpixels /2, black);
DrawFormattedText(window, text_familiarization_3, 'center', screenYpixels /2 + 50, black);
DrawFormattedText(window, text_space_to_continue, 'center', screenYpixels - 100, black);
Screen('Flip', window);
pause(minimumPause);
wait_for_spacebar;
DrawFormattedText(window, text_space_to_begin, 'center', 'center', black);
Screen('Flip', window);
pause(minimumPause);
wait_for_spacebar;
% familiarisation
familiarization_good_answers = stimVar.familiarization_figure_pst;
for i = 1:6
    DrawFormattedText(window, text_sfg, 'center', 'center', black);
    Screen('Flip', window);
    [stim, fs] = audioread(strcat(dirSTIM, stimVar.familiarization_names(i)));
    player = audioplayer(stim,fs);
    play(player);
    pause(minimumDelayBeforeAnswer);
    participant_answer = wait_for_n_or_x;
    stop(player)
    if participant_answer == string2boolean(familiarization_good_answers(i))
        DrawFormattedText(window, text_feedback_correct, 'center', 'center', green);
    else
        DrawFormattedText(window, text_feedback_incorrect, 'center', 'center', red);
    end
    Screen('Flip', window);
    pause(isiTimeSecs);
end

% get easy and hard stimuli
for i = 1:length(stimVar.stimuli)
    stimuli = stimVar.stimuli(i);
    if stimuli.difficulty == "easy"
        easy_stimuli = stimuli.stimuli_params;
    elseif stimuli.difficulty == "hard"
        hard_stimuli = stimuli.stimuli_params;
    end
end

% explication de la tâche
DrawFormattedText(window, text_task_1, 'center', screenYpixels /2 - 50, black);
DrawFormattedText(window, text_space_to_continue, 'center', screenYpixels - 100, black);
Screen('Flip', window);
pause(minimumPause);
wait_for_spacebar;
DrawFormattedText(window, text_task_1, 'center', screenYpixels /2 - 50, black);
DrawFormattedText(window, text_task_2, 'center', screenYpixels /2, black);
DrawFormattedText(window, text_space_to_continue, 'center', screenYpixels - 100, black);
Screen('Flip', window);
pause(minimumPause);
wait_for_spacebar;
DrawFormattedText(window, text_task_1, 'center', screenYpixels /2 - 50, black);
DrawFormattedText(window, text_task_2, 'center', screenYpixels /2, black);
DrawFormattedText(window, text_task_3, 'center', screenYpixels /2 + 50, black);
DrawFormattedText(window, text_space_to_continue, 'center', screenYpixels - 100, black);
Screen('Flip', window);
pause(minimumPause);
wait_for_spacebar;
DrawFormattedText(window, text_space_to_begin, 'center', 'center', black);
Screen('Flip', window);
pause(minimumPause);
wait_for_spacebar;
%croix de fixation
DrawFormattedText(window, '+', 'center', 'center', black);
Screen('Flip', window);
pause(isiTimeSecs);

% loop
all_responses = [];
for difficulty_level = 1:2
    if difficulty_level == 1
        stimuli = easy_stimuli;
        responses.difficulty = 'easy';
    else
        stimuli = hard_stimuli;
        responses.difficulty = 'hard';
        % annonce des stimuli difficiles
        DrawFormattedText(window, text_hard_stimuli, 'center', 'center', black);
        DrawFormattedText(window, text_space_to_continue, 'center', screenYpixels - 100, black);
        Screen('Flip', window);
        pause(minimumPause);
        wait_for_spacebar;
        DrawFormattedText(window, '+', 'center', 'center', black);
        Screen('Flip', window);
        pause(isiTimeSecs);
    end
    shuffled_order = randperm(length(stimuli), length(stimuli));
    participant_answers_structs = [];
    for i = 1:length(shuffled_order)
        picked_index = shuffled_order(i);
        stimulus = stimuli(picked_index);
        correct_answer = string2boolean(stimulus.figure_present);
        DrawFormattedText(window, text_sfg, 'center', 'center', black);
        Screen('Flip', window);
        [stim, fs] = audioread(strcat(dirSTIM, stimulus.stimulus_name)); 
        player = audioplayer(stim,fs);
        play(player);
        pause(minimumDelayBeforeAnswer);
        participant_answer = wait_for_n_or_x;
        stop(player)
        % fill the answer struct
        participant_answer_struct.stimulus_number = picked_index;
        participant_answer_struct.correct_answer = correct_answer;
        participant_answer_struct.participant_answer = participant_answer;
        participant_answers_structs = [participant_answers_structs participant_answer_struct];
        DrawFormattedText(window, '+', 'center', 'center', black);
        Screen('Flip', window);
        pause(isiTimeSecs);
        if mod(i, 130) == 0 && i ~= length(shuffled_order)
            DrawFormattedText(window, text_pause, 'center', 'center', black);
            Screen('Flip', window);
            pause(minimumPause);
            wait_for_spacebar;
        end
    end
    responses.participant_answers = participant_answers_structs;
    all_responses = [all_responses responses];
end
expVar.participant_answers = all_responses;

DrawFormattedText(window, text_end, 'center', 'center', black);
Screen('Flip', window);
pause(minimumPause);
wait_for_spacebar;

writestruct(expVar, strcat(dirDATA, "expVar_",expVar.SubjID,".xml"));

sca;
return;



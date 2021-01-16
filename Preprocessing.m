% This program was created based on an existing one provided by others.
% The original can be accessed here: https://github.com/uavster/Video2HeartRate
% The processed video file must be CONSTANT FRAME RATE (CFR).

video_file = 'XXX.mp4'; % Enter the name of the video file based on which you want to create a PPG signal for here.
start_point = 0; % Enter the number of the frame (counted from the biginning  of the abovevideo file) where you want to start processing for here.
end_point = 1000; % Enter the number of the frame (counted from the biginning  of the abovevideo file) where you want to end processing for here.
save_path = "/Users/XXX"; % Enter the name of the directory where you want to save the processed .mat file. 

if ischar(video_file)
    disp(['Loading file ' video_file]);
    v = VideoReader(video_file);
else
    v = video_file;
end

disp(v);

numFrames = v.NumberOfFrames;

disp(['Total frames: ' num2str(numFrames)]);

y = zeros(1, numFrames);
for i = start_point : end_point
    display(['Processing ' num2str(i) '/' num2str(numFrames)]);
    frame = read(v, i);
    redPlane = frame(:, :, 1);
    y(i) = sum(sum(redPlane)) / (size(frame, 1) * size(frame, 2));   
end

disp('Signal acquired.');

fps = v.FrameRate;

WINDOW_SECONDS = 6;             % [s] Sliding window length
BPM_SAMPLING_PERIOD = 0.5;      % [s] Time between heart rate estimations
BPM_L = 40; BPM_H = 230;        % [bpm] Valid heart rate range
FILTER_STABILIZATION_TIME = 0;  % [s] Filter startup transient
CUT_START_SECONDS = 0;          % [s] Initial signal period to cut off
FINE_TUNING_FREQ_INCREMENT = 1; % [bpm] Separation between test tones for smoothing
ANIMATION_SPEED_FACTOR = 2;     % [] This makes the animation run faster or slower than real time

% Build and apply input filter
[b, a] = butter(2, [(((BPM_L)/60)/fps*2) (((BPM_H)/60)/fps*2)]);
yf = filter(b, a, y);
processed = yf((fps * max(FILTER_STABILIZATION_TIME, CUT_START_SECONDS))+1:size(yf, 2));

disp('Signal filtered.');

currentSavedName = strcat(save_path, "/", "processed.mat");
save(currentSavedName,  'y', 'fps', 'res')

disp('The file has been saved successfully.');
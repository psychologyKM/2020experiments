dataset_directory = dir('/Users/XXX'); % Enter the name of the directory where PPG files from all participants are saved.
save_path = "/Users/XXX"; % Enter the name of the directory where you want to save csv and fig file. 
allValues = zeros(length(dataset_directory), 6); % The above directory must ONLY contain PPG files.

for l = 1:(length(dataset_directory))
    currentName = strcat(dataset_directory(l).folder, '/', dataset_directory(l).name);
    
    if ~contains(currentName, ".mat") 
        continue 
    end
        
    allValues(l, 1) = str2double(extractBefore(dataset_directory(l).name, '_'));
    allValues(l, 2) = str2double(extractBetween(dataset_directory(l).name, '_', '.'));
    
    dat = importdata(currentName);

    figure
    plot(dat.y-mean(dat.y));
    hold on
    plot(dat.res);
    plot(diff(dat.res, 2));

    [a,b] = findpeaks(diff(dat.res, 2));
    [c,d] = findpeaks(a); 

    plot(b(d),a(d),'or');
    legend('raw signal (deviation)', 'after filtering', 'after second differentiation', 'detected peak');
    lgd = legend;
    lgd.FontSize = 16;
    lgd.FontName = 'Times New Roman';
    ax = gca;
    ax.XAxis.Visible = 'off';
    ax.YAxis.Visible = 'off';
    
    figName = strcat(extractBefore(dataset_directory(l).name, '.'), '.fig');
    savefig(strcat(save_path, "/", figName))
    close(gcf)

    ts = b(d);
    ts = ts(ts>=find(dat.res ~= 0, 1) + 150);
    ts = ts(ts<=(find(dat.y ~= 0, 1, 'last') - 150));
    ts1 = ts(ts<(find(dat.y ~= 0, 1, 'last') - find(dat.res ~= 0, 1) + 1) / 2 + find(dat.res ~= 0 , 1));
    ts2 = ts(ts>=(find(dat.y ~= 0, 1, 'last') - find(dat.res ~= 0, 1) + 1) / 2 + find(dat.res ~= 0, 1));
    
    v = zeros(length(ts) - 1, 1);
    for i = 2:length(ts)
        v(i - 1) = ts(i) - ts(i - 1);
    end
    
    vF = zeros(length(ts1) - 1, 1);
    for i = 2:length(ts1)
        vF(i - 1) = ts1(i) - ts1(i - 1);
    end
    
    vL = zeros(length(ts2) - 1, 1);
    for i = 2:length(ts2)
        vL(i - 1) = ts2(i) - ts2(i - 1);
    end
    
    vSet = {v vF vL};
    
    if (allValues(l, 2) == 0)
        ii1 = 1;
        ii2 = 1;
        allValues(l, 4:5) = NaN;
    else
        ii1 = 2;
        ii2 = 3;
        allValues(l, 3) = NaN;
    end

    for ii = ii1:ii2
        v = vSet{ii};
        mat = zeros(length(v), 2501);
        fromWhereToWhere = zeros(length(v), 2);
        Mprime_range = [15, 40];
        everyStep = linspace(Mprime_range(1), Mprime_range(2), (Mprime_range(2) - Mprime_range(1)) * 100 + 1);
        for i = 1:length(v)
            v1 = min(round((v(i) * 2/3) * 100) -Mprime_range(1) * 100 + 1, (Mprime_range(2) - Mprime_range(1)) * 100 + 1);
            v2 = min(round((v(i) * 4/3) * 100) -Mprime_range(1) * 100 + 1, (Mprime_range(2) - Mprime_range(1)) * 100 + 1);

            k = 1;
            for j = k:v1
                mat(i, j) = power(everyStep(j) * 2 - v(i), 2);
            end

            k = max(1, v1 + 1);
            for j = k:v2
                mat(i, j) = power(everyStep(j) - v(i), 2);
            end

            k = max(1, v2 + 1);
            for j = k:length(everyStep)
                mat(i, j) = power(everyStep(j) / 2 - v(i), 2);
            end

        end

        [M, I] = min(sum(mat));
        Mprime = Mprime_range(1) + (I - 1) / 100;
        allValues(l, 2 + ii) = Mprime;
    end
    allValues(l,6) = std(vSet{1});
end

idx = allValues(:, 1) == 0;
T = array2table(allValues(~idx, :));
T.Properties.VariableNames = {'IDn', 'order', 'baseline', 'firstTime', 'secondTime', 'sd'};
currentSavedName = strcat(save_path, "/", "Mprime.csv");
writetable(T, currentSavedName)

disp(T)
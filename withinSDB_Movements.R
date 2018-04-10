#withinSDBmovements_2015

# converted from Matlab code (GPS_SDbay.m)

# Unifinished - probably not needed.


rm(list=ls())
source('Cm_SDB_movements.R')

data.df <- read.csv('data/GPS_04Sept2015.csv')
IDs <- data.df[,1]

data.df$LocalDate <- as.POSIXct(format(data.df$TransmissionDateTime,
                                       format = '%m/%d/%Y %H:%M:%S',
                                     tz = 'America/Los_Angeles'))

data.df$hr <- as.numeric(format(dat.df$LocalDate, '%H'))
data.df$DOY <- as.numeric(format(dat.df$LocalDate, '%j'))
hours <- numdates <- vector(mode = numeric, length = dim(data.df)[1])

for (k1 in 1:length(hours)){

}
       tmp = regexp(LocalTime{k1}, ' ', 'split');
       dates(k1) = datenum(tmp{1});
       tmp2 = cells2mat(regexp(tmp{2}, ':', 'split'));
       hours(k1) = tmp2(1) + tmp2(2)/(60) + tmp2(3)/(60*60);
       numdates(k1) = dates(k1) + (hours(k1)/24);
       end

       %% plot lat vs time
       uniqIDs = unique(IDs);
       uniqCl = jet(length(uniqIDs));
       figure(1); clf;
       hold on;
       for k2 = 1:length(uniqIDs),
       latID = lats(strcmp(uniqIDs(k2), IDs));
       lonID = lons(strcmp(uniqIDs(k2), IDs));
       hrsID = hours(strcmp(uniqIDs(k2), IDs));

       plot(hrsID, latID-32, 'o', 'MarkerFaceColor', uniqCl(k2, :));
       end
       axis([0, 24, 0.6, 0.70]);
       set(gca, 'box', 'off', 'fontsize', 12);
       xlabel('Time (hrs)');
       ylabel('Latitude -32^o');

       %% is there difference between pre- and post- PP closure?
       figure(2); clf;
       subplot(1, 2, 1);
       uniqIDs1 = unique(IDs(dates <= datenum(2010, 12, 31)));
       mm1 = cells2mat(char2cell(datestr(dates(dates <= datenum(2010, 12, 31)), 'mm')));
       yy1 = cells2mat(char2cell(datestr(dates(dates <= datenum(2010, 12, 31)), 'yyyy')));
       uniqCl = jet(length(uniqIDs1));
       hold on;
       for k2 = 1:length(uniqIDs1),
       latID = lats(strcmp(uniqIDs1(k2), IDs));
       lonID = lons(strcmp(uniqIDs1(k2), IDs));
       hrsID = hours(strcmp(uniqIDs1(k2), IDs));

       plot(hrsID, latID-32, 'o', 'MarkerFaceColor', uniqCl(k2, :));
       end
       axis([0, 24, 0.6, 0.70]);
       set(gca, 'box', 'off', 'fontsize', 12); %, 'XTickLabel', '');
       xlabel('Time (hrs)');
       ylabel('Latitude -32^o');
       title('Pre PP closure');
       hlineOps.LineWidth = 2;
       hlineOps.Color = 'k';
       hlineOps.linestyle = '-';
       horizline([0.62, 0.65, 0.68], hlineOps);

       subplot(1, 2, 2);
       uniqIDs2 = unique(IDs(dates > datenum(2010, 12, 31)));
       mm2 = cells2mat(char2cell(datestr(dates(dates > datenum(2010, 12, 31)), 'mm')));
       yy2 = cells2mat(char2cell(datestr(dates(dates > datenum(2010, 12, 31)), 'yyyy')));
       uniqCl = jet(length(uniqIDs2));
       hold on;
       for k2 = 1:length(uniqIDs2),
       latID = lats(strcmp(uniqIDs2(k2), IDs));
       lonID = lons(strcmp(uniqIDs2(k2), IDs));
       hrsID = hours(strcmp(uniqIDs2(k2), IDs));

       plot(hrsID, latID-32, 'o', 'MarkerFaceColor', uniqCl(k2, :));
       end
       axis([0, 24, 0.6, 0.70]);
       set(gca, 'box', 'off', 'fontsize', 12, 'YTickLabel', '');
       xlabel('Time (hrs)');
       %ylabel('Latitude -32^o');
       title('Post PP closure');
       horizline([0.62, 0.65, 0.68], hlineOps);

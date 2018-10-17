cd ~/Dropbox/Teaching/ENV' 115'/
%load ids16.mat
load ids360
classList=ids;

et=readtable('evalEmailsF16.csv');

toGrade=table2cell(et);

grades=zeros(length(classList),1);
for i=1:length(classList)
    grades(i)=sum(strcmp([classList{i} '@nau.edu'],toGrade));
end
noMatch=cell(1,1);
t=1;



%which don't match any?
for i =1:length(toGrade)
    if ~any(strcmp(toGrade{i},classList))
        noMatch{t}=toGrade{i};
        t=t+1;
    end
end

if any(isempty(noMatch))
   warning([num2str(length(noMatch)) ' 5-digit codes don''t correspond to the gradesheet']) 
end

out=[classList num2cell(grades.*5)];
cell2csv('evalPointsF16_360.csv',out);
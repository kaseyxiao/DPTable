function result=GetFinalResult(Z,O)
    [~,index]=max(Z,[],2);
    m=size(Z,2);
    result=cell(1,0);
    for i=1:m
        tmp_index=find(index==i);
        if ~isempty(tmp_index)
            tmp=sum(O(:,tmp_index),2);
            result{length(result)+1}=find(tmp>0)';
            disp(result{end});
        end
    end
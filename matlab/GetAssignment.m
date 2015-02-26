function assign=GetAssignment(Z)
    [~,index]=max(Z,[],2);
    assign=zeros(size(Z));
    for i=1:length(index)
        assign(i,index(i))=1;
    end
    clear index;
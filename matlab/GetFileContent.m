function [node_card,clique]=GetFileContent(filename)
    fid=fopen(filename);
    node_read_option=1;
    node_card=zeros(1,0);
    clique=cell(1,0);
    delimiter=blanks(1);
    while feof(fid)==0
        content=fgetl(fid);
        if isempty(content)
            continue;
        end
        if strcmp(content(1),'-')==1
            node_read_option=0;
        elseif node_read_option==1
            tmp=GetStrContent(content,delimiter);
            node_card(1,length(node_card)+1)=tmp(2);
        else
            clique{1,1+length(clique)}=GetStrContent(content,delimiter);
        end
    end
    fclose(fid);    

function result=GetStrContent(string,delimiter)
    remain = string;
    result=zeros(1,0);
    while true
        [s, remain] = strtok(remain, delimiter);
        if isempty(s)
            break;
        end
        result(1,length(result)+1)=str2double(s);
    end